-module(prometheus_rabbitmq_exchanges_collector).
-export([register/0,
         register/1,
         deregister_cleanup/1,
         collect_mf/2,
         collect_metrics/2]).

-import(prometheus_model_helpers, [create_mf/5,
                                   label_pairs/1,
                                   gauge_metrics/1,
                                   gauge_metric/1,
                                   gauge_metric/2,
                                   counter_metric/1,
                                   counter_metric/2]).

-include("prometheus_rabbitmq_exporter.hrl").

-behaviour(prometheus_collector).

-define(METRIC_NAME_PREFIX, "rabbitmq_exchange_").

-define(EXCHANGE_METRIC_NAME(S), ?METRIC_NAME_PREFIX ++ atom_to_list(S)).

-define(EXCHANGE_GAUGES, []).

-define(EXCHANGE_COUNTERS, []).

%%====================================================================
%% Collector API
%%====================================================================

register() ->
  register(default).

register(Registry) ->
  ok = prometheus_registry:register_collector(Registry, ?MODULE).

deregister_cleanup(_) -> ok.

collect_mf(_Registry, Callback) ->
  AllQueues = lists:merge([[Exchange || Exchange <- list_exchanges(VHost)] || [{name, VHost}] <- rabbit_vhost:info_all([name])]),
  [Callback(create_gauge(?EXCHANGE_METRIC_NAME(QueueKey), Help, {QueueKey, AllQueues})) || {QueueKey, Help} <- ?EXCHANGE_GAUGES],
  [Callback(create_counter(?EXCHANGE_METRIC_NAME(QueueKey), Help, {QueueKey, AllQueues})) || {QueueKey, Help} <- ?EXCHANGE_COUNTERS],

  case prometheus_rabbitmq_exporter_config:exchange_messages_stat() of
    [] ->
      ok;
    MessagesStat ->
      collect_messages_stat(Callback, AllQueues, MessagesStat)
  end,
  ok.

%% messages_stat
collect_metrics(_, {messages_stat, MSKey, AllQueues}) ->
  [counter_metric(labels(Exchange), prometheus_rabbitmq_message_stats:value(Exchange, MSKey))
   || Exchange <- AllQueues].

%%====================================================================
%% Private Parts
%%====================================================================

labels(Exchange) ->
  [{vhost, exchange_vhost(Exchange)},
   {exchange, exchange_name(Exchange)},
   {type, exchange_type(Exchange)}].

collect_messages_stat(Callback, AllQueues, MessagesStat) ->
  [Callback(create_counter(?EXCHANGE_METRIC_NAME(MetricName), Help, {messages_stat, MSKey, AllQueues}))
   || {MSKey, MetricName, Help} <- prometheus_rabbitmq_message_stats:metrics(), lists:member(MetricName, MessagesStat)].

%% emit_counter_metric_if_defined(Exchange, Value) ->
%%   case Value of
%%     undefined -> undefined;
%%     Value ->
%%       counter_metric(labels(Exchange), Value)
%%   end.

%% emit_gauge_metric_if_defined(Exchange, Value) ->
%%   case Value of
%%     undefined -> undefined;
%%     Value ->
%%       gauge_metric(labels(Exchange), Value)
%%   end.

exchange_vhost(Exchange) ->
  proplists:get_value(vhost, Exchange).

exchange_name(Exchange) ->
  proplists:get_value(name, Exchange).

exchange_type(Exchange) ->
  proplists:get_value(type, Exchange).

%% exchange_value(Exchange, Key) ->
%%   proplists:get_value(Key, Exchange, undefined).

list_exchanges(VHost) ->
  rabbit_mgmt_db:augment_exchanges(
    [rabbit_mgmt_format:exchange(X) || X <- rabbit_exchange:info_all(VHost)],
    ?NO_RANGE, basic).

create_counter(Name, Help, Data) ->
  create_mf(Name, Help, counter, ?MODULE, Data).

create_gauge(Name, Help, Data) ->
  create_mf(Name, Help, gauge, ?MODULE, Data).
