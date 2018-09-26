-module(prometheus_rabbitmq_nodes_collector).
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
                                   counter_metric/2,
                                   untyped_metric/1,
                                   untyped_metric/2,
                                   boolean_metric/2]).

-include("prometheus_rabbitmq_exporter.hrl").

-define(METRIC_NAME_PREFIX, "rabbitmq_node_").

-define(METRICS, [ {partitions, gauge, "Partitions detected in the cluster.",
                    fun(Node) ->
                            length(proplists:get_value(partitions, Node, []))
                    end},
                   {fd_total, gauge, "File descriptors available."},
                   {sockets_total, gauge, "Sockets available."},
                   {mem_limit, gauge, "Memory usage high watermark."},
                   {mem_alarm, boolean, "Set to 1 if a memory alarm is in effect in the node."},
                   {disk_free_limit, gauge, "Free disk space low watermark."},
                   {disk_free_alarm, boolean, "Set to 1 if a memory alarm is in effect in the node."},
                   {proc_total, gauge, "Erlang processes limit."},
                   {uptime, counter, "Time in milliseconds since node start."},
                   {run_queue, gauge, "Runtime run queue."},
                   {processors, gauge, "Logical processors."},
                   {net_ticktime, gauge, "Network tick time between pairs of Erlang nodes."},
                   {mem_used, gauge, "Memory used in bytes"},
                   {fd_used, gauge, "File descriptors used."},
                   {sockets_used, gauge, "Sockets used."},
                   {proc_used, gauge, "Erlang processes used."},
                   {disk_free, gauge, "Disk free in bytes"},
                   {gc_num, counter, "GC runs."},
                   {gc_bytes_reclaimed, counter, "Bytes reclaimed by GC."},
                   {context_switches, counter, "Context switches since node start."},
                   {io_read_count, counter, "Read operations since node start."},
                   {io_read_bytes, counter, "Bytes read since node start."},
                   {io_read_avg_time, gauge, "Average time of read operations."},
                   {io_write_count, counter, "Write operations since node start."},
                   {io_write_bytes, counter, "Bytes written since node start."},
                   {io_write_avg_time, gauge, "Average time of write operations."},
                   {io_sync_count, counter, "Sync operations sync node start."},
                   {io_sync_avg_time, gauge, "Average time of sync operations."},
                   {io_seek_count, counter, "Seek operations since node start."},
                   {io_seek_avg_time, gauge, "Average time of seek operations."},
                   {io_reopen_count, counter, "Times files have been reopened by the file handle cache."},
                   {mnesia_ram_tx_count, counter, "Mnesia transactions in RAM since node start."},
                   {mnesia_disk_tx_count, counter, "Mnesia transactions in disk since node start."},
                   {msg_store_read_count, counter, "Read operations in the message store since node start."},
                   {msg_store_write_count, counter, "Write operations in the message store since node start."},
                   {queue_index_journal_write_count, counter, "Write operations in the queue index journal since node start."},
                   {queue_index_write_count, counter, "Queue index write operations since node start."},
                   {queue_index_read_count, counter, "Queue index read operations since node start."},
                   {io_file_handle_open_attempt_count, counter, "File descriptor open attempts."},
                   {io_file_handle_open_attempt_avg_time, gauge, "Average time of file descriptor open attempts."},
                   {metrics_gc_queue_length_channel_closed, gauge, "Message queue length of GC process for channel metrics", get_metrics_gc_queue_length(channel_closed)},
                   {metrics_gc_queue_length_connection_closed, gauge, "Message queue length of GC process for connection metrics", get_metrics_gc_queue_length(connection_closed)},
                   {metrics_gc_queue_length_consumer_deleted, gauge, "Message queue length of GC process for consumer metrics", get_metrics_gc_queue_length(consumer_deleted)},
                   {metrics_gc_queue_length_exchange_deleted, gauge, "Message queue length of GC process for exchange metrics", get_metrics_gc_queue_length(exchange_deleted)},
                   {metrics_gc_queue_length_node_node_deleted, gauge, "Message queue length of GC process for node-node metrics", get_metrics_gc_queue_length(node_deleted)},
                   {metrics_gc_queue_length_queue_deleted, gauge, "Message queue length of GC process for queue metrics", get_metrics_gc_queue_length(queue_deleted)},
                   {metrics_gc_queue_length_vhost_deleted, gauge, "Message queue length of GC process for vhost metrics", get_metrics_gc_queue_length(vhost_deleted)},
                   {metrics_gc_queue_length_channel_consumer_deleted, gauge, "Message queue length of GC process for consumer metrics", get_metrics_gc_queue_length(channel_consumer_deleted)}
                 ]).

-behaviour(prometheus_collector).

%%====================================================================
%% Collector API
%%====================================================================

register() ->
  register(default).

register(Registry) ->
  ok = prometheus_registry:register_collector(Registry, ?MODULE).

deregister_cleanup(_) -> ok.

collect_mf(_Registry, Callback) ->
  Nodes = all_nodes_raw(),
  Callback(create_untyped(rabbitmq_node_up, "Node runnning status", Nodes)),
  case prometheus_rabbitmq_exporter_config:detailed_node_stat_enabled() of
      all ->
          collect_detailed_stats(Callback, Nodes);
      local ->
          [Node] = lists:filter(fun(N) ->
                                        node() == proplists:get_value(name, N)
                                end, Nodes),
          collect_detailed_stats(Callback, [Node]);
      _ ->
          ok
  end.

collect_metrics(rabbitmq_node_up, Nodes) ->
    [untyped_metric(labels(Node), node_running(Node)) || Node <- Nodes];
collect_metrics(_, {Type, Fun, Nodes}) ->
    [metric(Type, labels(Node), Fun(Node)) || Node <- Nodes].

metric(_, _, undefined) ->
    undefined;
metric(counter, Labels, Value) ->
    counter_metric(Labels, Value);
metric(gauge, Labels, Value) ->
    gauge_metric(Labels, Value);
metric(untyped, Labels, Value) ->
    untyped_metric(Labels, Value);
metric(boolean, Labels, Value) ->
    boolean_metric(Labels, Value).

%%====================================================================
%% Private Parts
%%====================================================================

%% just copied from rabbit_mgmt_wm_nodes
%% [[{name,hare@home},{type,disc},{running,true}],
%%  [{name,rabbit@home},{type,disc},{running,false}]]`
all_nodes_raw() ->
  S = rabbit_mnesia:status(),
  Nodes = proplists:get_value(nodes, S),
  Types = proplists:get_keys(Nodes),
  Running = proplists:get_value(running_nodes, S),
  [[{name, Node}, {type, Type}, {running, lists:member(Node, Running)}] ||
    Type <- Types, Node <- proplists:get_value(Type, Nodes)].

labels(Node) ->
  [{name, node_name(Node)},
   {type, node_type(Node)}].

node_name(Node) ->
  proplists:get_value(name, Node).

node_type(Node) ->
  proplists:get_value(type, Node).

node_running(Node) ->
  case proplists:get_value(running, Node) of
    true -> 1;
    _ -> 0
  end.

create_untyped(Name, Help, Data) ->
  create_mf(Name, Help, untyped, ?MODULE, Data).

collect_detailed_stats(Callback, Nodes) ->
    Augmented = rabbit_mgmt_db:augment_nodes(Nodes, ?NO_RANGE),
    [collect_detailed_stats(Callback, Augmented, Metric) || Metric <- ?METRICS],
    ok.

collect_detailed_stats(Callback, Augmented, {Key, Type, Help}) ->
    Callback(create_mf(?METRIC_NAME(Key), Help, Type, ?MODULE,
                       {Type, fun(Node) ->
                                      proplists:get_value(Key, Node, undefined)
                              end,
                        Augmented}));
collect_detailed_stats(Callback, Augmented, {Key, Type, Help, Fun}) ->
    Callback(create_mf(?METRIC_NAME(Key), Help, Type, ?MODULE,
                       {Type, Fun, Augmented})).

get_metrics_gc_queue_length(Tag) ->
    fun(Node) ->
            proplists:get_value(Tag,
                                proplists:get_value(metrics_gc_queue_length, Node, []),
                                undefined)
    end.
