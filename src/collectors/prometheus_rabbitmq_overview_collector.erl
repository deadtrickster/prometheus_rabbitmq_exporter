-module(prometheus_rabbitmq_overview_collector).
-export([register/0,
         register/1,
         deregister_cleanup/1,
         collect_mf/2,
         collect_metrics/2]).

-import(prometheus_model_helpers, [create_mf/4,
                                   create_mf/5,
                                   label_pairs/1,
                                   gauge_metrics/1,
                                   gauge_metric/1,
                                   gauge_metric/2,
                                   counter_metric/1,
                                   counter_metric/2,
                                   untyped_metric/1,
                                   untyped_metric/2]).

-include("prometheus_rabbitmq_exporter.hrl").
-behaviour(prometheus_collector).

-define(METRIC_NAME_PREFIX, "rabbitmq_").

-define(MESSAGE_STAT, [{queues_disk_reads, counter, "Total number of times messages have been read from disk by all queues.",
                        fun (Stat) ->
                            proplists:get_value(disk_reads, Stat)
                        end},
                       {queues_disk_writes, counter, "Total number of times messages have been written to disk by all queues.",
                        fun (Stat) ->
                            proplists:get_value(disk_writes, Stat)
                        end}]).
-define(OBJECT_TOTALS, [{consumers, gauge, "RabbitMQ Consumers count"},
                        {queues, gauge, "RabbitMQ Proplist count"},
                        {exchanges, gauge, "RabbitMQ Exchanges count"},
                        {connections, gauge, "RabbitMQ Connections count"},
                        {channels, gauge, "RabbitMQ Channels count"}]).
-define(QUEUE_TOTALS, [{messages_ready, gauge, "Messages ready for delivery"},
                       {messages_unacknowledged, gauge,
                        "Delivered but unacknowledged messages"}]).

-define(MEMORY_METRICS, [
                         %% Connections
                         {connection_readers,
                          "Processes responsible for connection parser "
                          "and most of connection state. Most of their "
                          "memory attributes to TCP buffers."},
                         {connection_writers,
                          "Processes responsible for serialization of "
                          "outgoing protocol frames and writing to "
                          "client connections sockets."},
                         {connection_channels,
                          "The more channels client connections use, "
                          "the more memory with be used by this category."},
                         {connection_other,
                          "Other memory related to client connections."},

                         %% Queues
                         {queue_procs,
                          "Queue masters, indices and messages kept in memory."},
                         {queue_slave_procs,
                          "Queue mirrors, indices and messages kept in memory."},

                         %% Processes
                         {plugins,
                          "Plugins such as Shovel, Federation, or protocol "
                          "implementations such as STOMP can accumulate "
                          "messages in memory."},
                         {other_proc,
                          "Queue masters, indices and messages kept in memory."},

                         %% Metrics
                         {metrics,
                          "Node-local metrics."},
                         {mgmt_db,
                          "Management DB ETS tables + processes."},

                         %% ETS
                         {mnesia,
                          "Virtual hosts, users, permissions, queue metadata "
                          "and state, exchanges, bindings, runtime parameters "
                          "and so on."},
                         {other_ets,
                          "Some plugins can  use ETS tables to store their state."},

                         %% Messages (mostly, some binaries are not messages)
                         {binary,
                          "Runtime binary heap. Most of this section is usually "
                          "message bodies and properties (metadata)."},
                         {msg_index,
                          "Message index ETS + processes."},

                         %% Totals
                         {erlang,
                          "Runtime Used"},
                         {allocated,
                          "Runtime Allocated"},
                         {rss,
                          "Resident Set Size (RSS) reported by the OS"}
                        ]).

%%====================================================================
%% Collector API
%%====================================================================

register() ->
  register(default).

register(Registry) ->
  ok = prometheus_registry:register_collector(Registry, ?MODULE).

deregister_cleanup(_) -> ok.

collect_mf(_Registry, Callback) ->
  Overview = rabbit_mgmt_db:get_overview(all, ?NO_RANGE),
  MessageStat = proplists:get_value(message_stats, Overview),
  ObjectTotals = proplists:get_value(object_totals, Overview),
  QueueTotals = proplists:get_value(queue_totals, Overview),
  collect_messages_stat(Callback, MessageStat),
  [mf(Callback, Metric, MessageStat) || Metric <- ?MESSAGE_STAT],
  [mf(Callback, Metric, ObjectTotals) || Metric <- ?OBJECT_TOTALS],
  [mf(Callback, Metric, QueueTotals) || Metric <- ?QUEUE_TOTALS],
  case prometheus_rabbitmq_exporter_config:memory_stat_enabled() of
    true ->
      collect_rabbit_memory(Callback);
    _ ->
      ok
  end,
  case prometheus_rabbitmq_exporter_config:connections_total_enabled() of
    true ->
      collect_rabbit_connections(Callback);
    _ ->
      ok
  end,
  ok.

collect_metrics(_, {messages_stat, MSKey, Stats}) ->
  counter_metric([], proplists:get_value(MSKey, Stats));
collect_metrics(_, {Type, Fun, Stats}) ->
  metric(Type, [], Fun(Stats)).

collect_rabbit_memory(Callback) ->
  %% We flatten in order to have the totals under the same list.
  Memory = lists:flatten(rabbit_vm:memory()),
  [begin
     FullName = ?METRIC_NAME(["memory_", Name, "_bytes"]),
     Value = proplists:get_value(Name, Memory, undefined),
     Callback(create_mf(FullName, Help, gauge, Value))
   end || {Name, Help} <- ?MEMORY_METRICS].

collect_rabbit_connections(Callback) ->
  Table = ets:new('$$connectinos_stat$$', [set]),
  try
    Connections = ets:select(connection_stats, [{{'$1', '$2'}, [], ['$2']}], 10),
    connections_loop(Connections, Table),
    Callback(create_mf(?METRIC_NAME(connections_total), "RabbitMQ connections count grouped by connection state.",
                       gauge, ets:tab2list(Table)))
  after
    ets:delete(Table)
  end,
  ok.

connections_loop('$end_of_table', _Table) ->
  ok;
connections_loop({Connections, Continuation}, Table) ->
  [begin
     Labels = [{state, proplists:get_value(state, Connection)}],
     try
       ets:update_counter(Table, Labels, 1)
     catch error:badarg ->
         ets:insert(Table, {Labels, 1})
     end
   end
   || Connection <- Connections],
  connections_loop(ets:select(Continuation), Table).

mf(Callback, Metric, Proplist) ->
  {Name, Type, Help, Fun} = case Metric of
                              {Key, Type1, Help1} ->
                                {Key, Type1, Help1, fun (Proplist1) ->
                                                        proplists:get_value(Key, Proplist1)
                                                    end};
                              {Key, Type1, Help1, Fun1} ->
                                {Key, Type1, Help1, Fun1}
                            end,
  Callback(create_mf(?METRIC_NAME(Name), Help, catch_boolean(Type), ?MODULE, {Type, Fun, Proplist})).

%% collect_metrics(rabbitmq_connections, _MFData) ->
%%   AllConnections = created_events(connection_stats),
%%   AllVHosts = rabbit_vhost:info_all([name]),
%%   [gauge_metric([{vhost, VHost}], length(filter_by_vhost(VHost, AllConnections))) || [{name, VHost}] <- AllVHosts];
%% collect_metrics(rabbitmq_channels, _MFData) ->
%%   AllChannels = created_events(channel_stats),
%%   AllVHosts = rabbit_vhost:info_all([name]),
%%   [gauge_metric([{vhost, VHost}], length(filter_by_vhost(VHost, AllChannels))) || [{name, VHost}] <- AllVHosts];
%% collect_metrics(rabbitmq_queues, _MFData) ->
%%   AllVHosts = rabbit_vhost:info_all([name]),
%%   [gauge_metric([{vhost, VHost}], length(rabbit_amqqueue:list(VHost))) || [{name, VHost}] <- AllVHosts];
%% collect_metrics(rabbitmq_exchanges, _MFData) ->
%%   AllVHosts = rabbit_vhost:info_all([name]),
%%   [gauge_metric([{vhost, VHost}], length(rabbit_exchange:list(VHost))) || [{name, VHost}] <- AllVHosts];
%% collect_metrics(rabbitmq_consumers, _MFData) ->
%%   gauge_metric([], ets:info(consumers_by_queue, size)).

%%====================================================================
%% Private Parts
%%====================================================================

collect_messages_stat(Callback, Stats) ->
  [Callback(create_counter(?METRIC_NAME(MetricName), Help, {messages_stat, MSKey, Stats}))
   || {MSKey, MetricName, Help} <- prometheus_rabbitmq_message_stats:metrics()].


%% filter_by_vhost(VHost, Channels) ->
%%   [I || I <- Channels, rabbit_misc:pget(vhost, I) =:= VHost].

%% %% created_events(Type) ->
%% %%   ets:select(Type, [{{{'_', '$1'}, '$2', '_'}, [{'==', 'create', '$1'}],
%% %%                      ['$2']}]).

%% created_events(connection_stats) ->
%%   rabbit_mgmt_db:get_all_connections(?NO_RANGE);
%% created_events(channel_stats) ->
%%   rabbit_mgmt_db:get_all_channels(?NO_RANGE).

%% create_gauge(Name, Help, Data) ->
%%   create_mf(Name, Help, gauge, ?MODULE, Data).

create_counter(Name, Help, Data) ->
  create_mf(Name, Help, counter, ?MODULE, Data).

metric(counter, Labels, Value) ->
  emit_counter_metric_if_defined(Labels, Value);
metric(gauge, Labels, Value) ->
  emit_gauge_metric_if_defined(Labels, Value);
metric(untyped, Labels, Value) ->
  untyped_metric(Labels, Value);
metric(boolean, Labels, Value0) ->
  Value = case Value0 of
            true -> 1;
            false -> 0;
            undefined -> undefined
          end,
  untyped_metric(Labels, Value).

catch_boolean(boolean) ->
    untyped;
     catch_boolean(T) ->
         T.

emit_counter_metric_if_defined(Labels, Value) ->
  case Value of
    undefined -> undefined;
    '' ->
      counter_metric(Labels, undefined);
    Value ->
      counter_metric(Labels, Value)
  end.

emit_gauge_metric_if_defined(Labels, Value) ->
  case Value of
    undefined -> undefined;
    '' ->
      gauge_metric(Labels, undefined);
    Value ->
      gauge_metric(Labels, Value)
  end.
