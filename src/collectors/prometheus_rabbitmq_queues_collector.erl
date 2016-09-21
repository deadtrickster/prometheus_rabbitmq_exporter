-module(prometheus_rabbitmq_queues_collector).
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

-include_lib("prometheus/include/prometheus.hrl").
-include_lib("rabbit_common/include/rabbit.hrl").
-behaviour(prometheus_collector).


-define(METRIC_NAME_PREFIX, "rabbitmq_queue_").

-define(QUEUE_METRIC_NAME(S), ?METRIC_NAME_PREFIX ++ atom_to_list(S)).

-define(QUEUE_GAUGES, [{messages_ready, "Number of messages ready to be delivered to clients."},
                       {messages_unacknowledged, "Number of messages delivered to clients but not yet acknowledged."},
                       {messages, "Sum of ready and unacknowledged messages (queue depth)."},
                       {messages_ready_ram, "Number of messages from messages_ready which are resident in ram."},
                       {messages_unacknowledged_ram, "Number of messages from messages_unacknowledged which are resident in ram."},
                       {messages_ram, "Total number of messages which are resident in ram."},
                       {messages_persistent, "Total number of persistent messages in the queue (will always be 0 for transient queues)."},
                       {message_bytes, "Sum of the size of all message bodies in the queue. This does not include the message properties (including headers) or any overhead."},
                       {message_bytes_ready, "Like message_bytes but counting only those messages ready to be delivered to clients."},
                       {message_bytes_unacknowledged, "Like message_bytes but counting only those messages delivered to clients but not yet acknowledged."},
                       {message_bytes_ram, "Like message_bytes but counting only those messages which are in RAM."},
                       {message_bytes_persistent, "Like message_bytes but counting only those messages which are persistent."},
                       {consumers, "Number of consumers."},
                       {consumer_utilisation, "Fraction of the time (between 0.0 and 1.0) that the queue is able to immediately deliver messages to consumers. This can be less than 1.0 if consumers are limited by network congestion or prefetch count."},
                       {memory, "Bytes of memory consumed by the Erlang process associated with the queue, including stack, heap and internal structures."}]).

-define(QUEUE_COUNTERS, [{disk_reads, "Total number of times messages have been read from disk by this queue since it started."},
                         {disk_writes, "Total number of times messages have been written to disk by this queue since it started."}]).

%%====================================================================
%% Collector API
%%====================================================================

register() ->
  register(default).

register(Registry) ->
  ok = prometheus_registry:register_collector(Registry, ?MODULE).

deregister_cleanup(_) -> ok.

collect_mf(_Registry, Callback) ->
  AllQueues = lists:merge([[Queue || Queue <- list_queues(VHost)] || [{name, VHost}] <- rabbit_vhost:info_all([name])]),
  [Callback(create_gauge(?QUEUE_METRIC_NAME(QueueKey), Help, {QueueKey, AllQueues})) || {QueueKey, Help} <- ?QUEUE_GAUGES],
  [Callback(create_counter(?QUEUE_METRIC_NAME(QueueKey), Help, {QueueKey, AllQueues})) || {QueueKey, Help} <- ?QUEUE_COUNTERS],

  case prometheus_rabbitmq_exporter_config:queue_messages_stat() of
    [] ->
      ok;
    MessagesStat ->
      collect_messages_stat(Callback, AllQueues, MessagesStat)
  end,
  ok.

%% [vhost, queue]
collect_metrics("rabbitmq_queue_disk_reads", {QueueKey, AllQueues}) ->
  [emit_counter_metric_if_defined(Queue, queue_value(Queue, QueueKey)) || Queue <- AllQueues];
collect_metrics("rabbitmq_queue_disk_writes", {QueueKey, AllQueues}) ->
  [emit_counter_metric_if_defined(Queue, queue_value(Queue, QueueKey)) || Queue <- AllQueues];
%% [vhost, queue]
collect_metrics(_MetricName, {QueueKey, AllQueues}) ->
  [emit_gauge_metric_if_defined(Queue, queue_value(Queue, QueueKey)) || Queue <- AllQueues];
%% messages_stat
collect_metrics(_, {messages_stat, MSKey, AllQueues}) ->
  [counter_metric(labels(Queue), prometheus_rabbitmq_message_stats:value(Queue, MSKey))
   || Queue <- AllQueues].

%%====================================================================
%% Private Parts
%%====================================================================

labels(Queue) ->
  [{vhost, queue_vhost(Queue)},
   {queue, queue_name(Queue)}].

collect_messages_stat(Callback, AllQueues, MessagesStat) ->
  [Callback(create_counter(?QUEUE_METRIC_NAME(MetricName), Help, {messages_stat, MSKey, AllQueues}))
   || {MSKey, MetricName, Help} <- prometheus_rabbitmq_message_stats:metrics(), lists:member(MetricName, MessagesStat)].

emit_counter_metric_if_defined(Queue, Value) ->
  case Value of
    undefined -> undefined;
    Value ->
      counter_metric(labels(Queue), Value)
  end.

emit_gauge_metric_if_defined(Queue, Value) ->
  case Value of
    undefined -> undefined;
    Value ->
      gauge_metric(labels(Queue), Value)
  end.

queue_vhost(Queue) ->
  proplists:get_value(vhost, Queue).

queue_name(Queue) ->
  proplists:get_value(name, Queue).

queue_value(Queue, Key) ->
  proplists:get_value(Key, Queue, undefined).

list_queues(VHost) ->
  Queues = rabbit_mgmt_db:augment_queues(
             [rabbit_mgmt_format:queue(Queue) || Queue <- rabbit_amqqueue:list(VHost) ++ rabbit_amqqueue:list_down(VHost)],
             {no_range, no_range, no_range, no_range},
             basic),
  Queues.

create_counter(Name, Help, Data) ->
  create_mf(Name, Help, counter, ?MODULE, Data).

create_gauge(Name, Help, Data) ->
  create_mf(Name, Help, gauge, ?MODULE, Data).
