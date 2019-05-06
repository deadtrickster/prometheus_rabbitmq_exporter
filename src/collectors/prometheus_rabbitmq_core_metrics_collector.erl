-module(prometheus_rabbitmq_core_metrics_collector).
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

-define(METRIC_NAME_PREFIX, "rabbitmq_core_").

% These metrics are mapped to ETS tables
-define(METRICS,
    [
        {channel_metrics, [
            {2, channel_consumer_count, gauge, "Consumers count.", consumer_count},
            {2, channel_messages_unacknowledged, gauge, "Count of messages unacknowledged.", messages_unacknowledged},
            {2, channel_messages_unconfirmed, gauge, "Count of messages unconfirmed.", messages_unconfirmed},
            {2, channel_messages_uncommited, gauge, "Count of messages uncommited.", messages_uncommited},
            {2, channel_messages_prefetch_count, gauge, "Limit to the number of unacknowledged messages on every connection on a channel.", prefetch_count},
            {2, channel_messages_global_prefetch_count, gauge, "Global limit to the number of unacknowledged messages shared between all connections on a channel.", global_prefetch_count}
        ]},

        {channel_exchange_metrics, [
            {2, channel_exchange_publish, gauge, "Count of messages published."},
            {3, channel_exchange_confirm, gauge, "Count of messages confirmed."},
            {4, channel_exchange_return_unroutable, gauge, "Count of messages returned to publisher as unroutable."}
        ]},

        {channel_process_metrics, [
            {2, channel_process_reductions, counter, "Count of reductions that take place on the channel process."}
        ]},

        {channel_queue_metrics, [
            {2, channel_queue_get, counter, "Count of messages delivered in acknowledgement mode in response to basic.get."},
            {3, channel_queue_get_no_ack, counter, "Count of messages delivered in no-acknowledgement mode in response to basic.get."},
            {4, channel_queue_deliver, counter, "Count of messages delivered in acknowledgement mode to consumers."},
            {5, channel_queue_deliver_no_ack, counter, "Count of messages delivered in no-acknowledgement mode to consumers."},
            {6, channel_queue_redeliver, counter, "Count of subset of delivered messages which had the redelivered flag set."},
            {7, channel_queue_ack, counter, "Count of messages acknowledged."}
        ]},

        {connection_churn_metrics, [
            {2, connection_created, counter, "Connections created."},
            {3, connection_closed, counter, "Connections closed."},
            {4, channel_created, counter, "Channels created."},
            {5, channel_closed, counter, "Channels closed."},
            {6, queue_declared, counter, "Queues declared."},
            {7, queue_created, counter, "Queues created."},
            {8, queue_deleted, counter, "Queues deleted."}
        ]},

        {connection_coarse_metrics, [
            {2, connection_recv_oct, counter, "Count of octects received on the connection."},
            {3, connection_send_oct, counter, "Count of octects sent on the connection."},
            {4, connection_reductions, counter, "Count of reductions that take place on the queue process."}
        ]},

        {connection_metrics, [
            {2, connection_recv_count, counter, "Count of bytes received on the connection.", recv_cnt},
            {2, connection_send_count, counter, "Count of bytes send on the connection.", send_cnt}
        ]},

        {channel_queue_exchange_metrics, [
            {2, channel_queue_exchange_publish, counter, "Count of messages published."}
        ]},

        {node_coarse_metrics, [
            {2, node_fd_used, gauge, "File descriptors used.", fd_used},
            {2, node_sockets_used, gauge, "Sockets used.", sockets_used},
            {2, node_mem_used, gauge, "Memory used in bytes.", mem_used},
            {2, node_disk_free, gauge, "Disk free in bytes.", disk_free},
            {2, node_proc_used, gauge, "Erlang processes used.", proc_used},
            {2, node_gc_num, counter, "GC runs.", gc_num},
            {2, node_gc_bytes_reclaimed, counter, "Bytes reclaimed by GC.", gc_bytes_reclaimed},
            {2, node_context_switches, counter, "Context switches since node start.", context_switches}
        ]},

        {node_metrics, [
            {2, node_fd_total, gauge, "File descriptors available.", fd_total},
            {2, node_sockets_total, gauge, "Sockets available.", sockets_total},
            {2, node_mem_limit, gauge, "Memory usage high watermark.", mem_limit},
            {2, node_disk_free_limit, gauge, "Free disk space low watermark.", disk_free_limit},
            {2, node_proc_total, gauge, "Erlang processes limit.", proc_total},
            {2, node_uptime, gauge, "Time in milliseconds since node start.", uptime},
            {2, node_run_queue, gauge, "Runtime run queue.", run_queue},
            {2, node_processors, gauge, "Logical processors.", processors},
            {2, node_net_ticktime, gauge, "Periodic tick interval between all pairs of nodes to maintain the connections and to detect disconnections.", net_ticktime}
        ]},

        {node_node_metrics, [
            {2, node_node_send_bytes, counter, "Count of bytes sent to node.", send_bytes},
            {2, node_node_recv_bytes, counter, "Count of bytes received from node.", recv_bytes}
        ]},

        {node_persister_metrics, [
            {2, node_io_read_count, counter, "Read operations since node start.", io_read_count},
            {2, node_io_read_bytes, gauge, "Bytes read since node start.", io_read_bytes},
            {2, node_io_read_time, gauge, "Total time of read operations.", io_read_time},
            {2, node_io_write_count, counter, "Write operations since node start.", io_write_count},
            {2, node_io_write_bytes, gauge, "Bytes written since node start.", io_write_bytes},
            {2, node_io_write_time, gauge, "Total time of write operations.", io_write_time},
            {2, node_io_sync_count, counter, "Sync operations since node start.", io_sync_count},
            {2, node_io_sync_time, gauge, "Total time of sync operations.", io_sync_time},
            {2, node_io_seek_count, counter, "Seek operations since node start.", io_seek_count},
            {2, node_io_seek_time, gauge, "Total time of seek operations.", io_seek_time},
            {2, node_io_reopen_count, counter, "Times files have been reopened by the file handle cache.", io_reopen_count},
            {2, node_mnesia_ram_tx_count, counter, "Mnesia transactions in RAM since node start.", mnesia_ram_tx_count},
            {2, node_mnesia_disk_tx_count, counter, "Mnesia transactions in disk since node start.", mnesia_disk_tx_count},
            {2, node_msg_store_read_count, counter, "Read operations in the message store since node start.", msg_store_read_count},
            {2, node_msg_store_write_count, counter, "Write operations in the message store since node start.", msg_store_write_count},
            {2, queue_index_journal_write_count, counter, "Write operations in the queue index journal since node start.", queue_index_journal_write_count},
            {2, queue_index_write_count, counter, "Queue index write operations since node start.", queue_index_write_count},
            {2, queue_index_read_count, counter, "Queue index read operations since node start.", queue_index_read_count},
            {2, queue_io_file_handle_open_attempt_count, counter, "File descriptor open attempts.", io_file_handle_open_attempt_count},
            {2, queue_io_file_handle_open_attempt_time, gauge, "Total time of file descriptor open attempts.", io_file_handle_open_attempt_time}
        ]},

        {queue_coarse_metrics, [
            {2, queue_messages_ready, gauge, "Number of messages ready to be delivered to clients."},
            {3, queue_messages_unacknowledge, gauge, "Number of messages delivered to clients but not yet acknowledged."},
            {4, queue_messages, gauge, "Sum of ready and unacknowledged messages (queue depth)."},
            {5, queue_reductions, counter, "Count of reductions that take place on the queue process."}
        ]},

        {queue_metrics, [
            {2, queue_disk_reads, gauge, "Total number of times messages have been read from disk by this queue.", disk_reads},
            {2, queue_disk_writes, gauge, "Total number of times messages have been written to disk by this queue.", disk_writes}
        ]}
    ]).

-define(TOTALS, [
    {connection_created, connections, gauge, "RabbitMQ Connections count."},
    {channel_created, channels, gauge, "RabbitMQ Channels count."},
    {consumer_created, consumers, gauge, "RabbitMQ Consumers count."},
    {queue_metrics, queues, gauge, "RabbitMQ Queues count."}
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
    [begin
         Data = ets:tab2list(Table),
         mf(Callback, Contents, Data)
     end || {Table, Contents} <- ?METRICS],
    [begin
         Size = ets:info(Table, size),
         mf_totals(Callback, Name, Type, Help, Size)
     end || {Table, Name, Type, Help} <- ?TOTALS],
    ok.

mf(Callback, Contents, Data) ->
    [begin
         Fun = fun(D) -> element(Index, D) end,
         Callback(create_mf(?METRIC_NAME(Name), Help, catch_boolean(Type), ?MODULE,
                            {Type, Fun, Data}))
     end || {Index, Name, Type, Help} <- Contents],
    [begin
         Fun = fun(D) -> proplists:get_value(Key, element(Index, D)) end,
         Callback(create_mf(?METRIC_NAME(Name), Help, catch_boolean(Type), ?MODULE,
                            {Type, Fun, Data}))
     end || {Index, Name, Type, Help, Key} <- Contents].

mf_totals(Callback, Name, Type, Help, Size) ->
    Callback(create_mf(?METRIC_NAME(Name), Help, catch_boolean(Type), Size)).

collect_metrics(_, {Type, Fun, Items}) ->
    [metric(Type, labels(Item), Fun(Item)) || Item <- Items].

labels(Item) ->
    label(element(1, Item)).

label(#resource{virtual_host = VHost, kind = exchange, name = Name}) ->
    [{vhost, VHost}, {exchange, Name}];
label(#resource{virtual_host = VHost, kind = queue, name = Name}) ->
    [{vhost, VHost}, {queue, Name}];
label({P, {#resource{virtual_host = QVHost, kind = queue, name = QName},
            #resource{virtual_host = EVHost, kind = exchange, name = EName}}}) when is_pid(P) ->
    %% channel_queue_exchange_metrics {channel_id, {queue_id, exchange_id}}
    [{channel, P}, {queue_vhost, QVHost}, {queue_name, QName},
     {exchange_vhost, EVHost}, {exchange_name, EName}];
label({I1, I2}) ->
    label(I1) ++ label(I2);
label(P) when is_pid(P) ->
    [{channel, P}];
label(A) when is_atom(A) ->
    [{node, A}].

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


%%====================================================================
%% Private Parts
%%====================================================================
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
