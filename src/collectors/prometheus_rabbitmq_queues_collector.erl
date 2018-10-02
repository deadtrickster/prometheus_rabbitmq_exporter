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
                                   counter_metric/2,
                                   untyped_metric/1,
                                   untyped_metric/2]).

-include("prometheus_rabbitmq_exporter.hrl").
-behaviour(prometheus_collector).


-define(METRIC_NAME_PREFIX, "rabbitmq_queue_").

-define(METRICS, [{durable, boolean, "Whether or not the queue survives server restarts."},
                  {auto_delete, boolean, "Whether the queue will be deleted automatically when no longer used."},
                  {exclusive, boolean, "True if queue is exclusive (i.e. has owner_pid), false otherwise."},
                  {messages_ready, gauge, "Number of messages ready to be delivered to clients."},
                  {messages_unacknowledged, gauge, "Number of messages delivered to clients but not yet acknowledged."},
                  {messages, gauge, "Sum of ready and unacknowledged messages (queue depth)."},
                  {messages_ready_ram, gauge, "Number of messages from messages_ready which are resident in ram."},
                  {messages_unacknowledged_ram, gauge, "Number of messages from messages_unacknowledged which are resident in ram."},
                  {messages_ram, gauge, "Total number of messages which are resident in ram."},
                  {messages_persistent, gauge, "Total number of persistent messages in the queue (will always be 0 for transient queues)."},
                  {message_bytes, gauge, "Sum of the size of all message bodies in the queue. This does not include the message properties (including headers) or any overhead."},
                  {message_bytes_ready, gauge, "Like message_bytes but counting only those messages ready to be delivered to clients."},
                  {message_bytes_unacknowledged, gauge, "Like message_bytes but counting only those messages delivered to clients but not yet acknowledged."},
                  {message_bytes_ram, gauge, "Like message_bytes but counting only those messages which are in RAM."},
                  {message_bytes_persistent, gauge, "Like message_bytes but counting only those messages which are persistent."},
                  {head_message_timestamp, gauge, "The timestamp property of the first message in the queue, if present. Timestamps of messages only appear when they are in the paged-in state."},
                  {disk_reads, counter, "Total number of times messages have been read from disk by this queue since it started."},
                  {disk_writes, counter, "Total number of times messages have been written to disk by this queue since it started."},
                  {disk_size_bytes, gauge, "Disk space occupied by the queue.",
                   fun (Queue) ->
                       queue_dir_size(Queue)
                   end},
                  {consumers, gauge, "Number of consumers."},
                  {consumer_utilisation, gauge, "Fraction of the time (between 0.0 and 1.0) that the queue is able to immediately deliver messages to consumers. This can be less than 1.0 if consumers are limited by network congestion or prefetch count."},
                  {memory, gauge, "Bytes of memory consumed by the Erlang process associated with the queue, including stack, heap and internal structures."},
                  {state, gauge, "The state of the queue. NaN if queue is located on cluster nodes that are currently down. "
                   "0 if queue is running normally. MsgCount if queue is synchronizing.",
                   fun(Queue) ->
                       case queue_value(Queue, state) of
                         running -> 0;
                         undefined -> undefined;
                         down -> undefined;
                         {syncing, MsgCount} -> MsgCount
                       end
                   end}
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
  AllQueues = lists:merge([[Queue || Queue <- list_queues(VHost)] || [{name, VHost}] <- rabbit_vhost:info_all([name])]),
  [mf(Callback, Metric, AllQueues) || Metric <- ?METRICS],

  case prometheus_rabbitmq_exporter_config:queue_messages_stat() of
    [] ->
      ok;
    MessagesStat ->
      collect_messages_stat(Callback, AllQueues, MessagesStat)
  end,
  ok.

mf(Callback, Metric, Queues) ->
  {Name, Type, Help, Fun} = case Metric of
                              {Key, Type1, Help1} ->
                                {Key, Type1, Help1, fun (Queue) ->
                                                        list_to_count(queue_value(Queue, Key))
                                                    end};
                              {Key, Type1, Help1, Fun1} ->
                                {Key, Type1, Help1, Fun1}
                            end,
  Callback(create_mf(?METRIC_NAME(Name), Help, catch_boolean(Type), ?MODULE, {Type, Fun, Queues})).


%% messages_stat
collect_metrics(_, {messages_stat, MSKey, AllQueues}) ->
  [counter_metric(labels(Queue), prometheus_rabbitmq_message_stats:value(Queue, MSKey))
   || Queue <- AllQueues];
collect_metrics(_, {Type, Fun, Queues}) ->
  [metric(Type, labels(Queue), Fun(Queue)) || Queue <- Queues].


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

labels(Queue) ->
    %% exclusive_consumer_tag should not be used as a label. Prometheus documentation
    %% states that labels should not be used to store dimensions with high cardinality,
    %% as every unique combination of key-value label pairs represents a new time
    %% series, which can dramatically increase the amount of data stored.
    %% As such, from the arguments only x-overflow, x-queue-mode and x-queue-type
    %% should be represented as arguments.
    add_if_not_empty(
      {queue_mode, queue_argument(<<"x-queue-mode">>, Queue)},
      add_if_not_empty(
        {type, queue_argument(<<"x-queue-type">>, Queue, <<"classic">>)},
        add_if_not_empty(
          {overflow, queue_argument(<<"x-overflow">>, Queue)},
          add_if_not_empty({policy, queue_policy(Queue)},
                           [{vhost, queue_vhost(Queue)},
                            {queue, queue_name(Queue)}])))).

add_if_not_empty({_, ''}, Acc) ->
    Acc;
add_if_not_empty(Tuple, Acc) ->
    [Tuple | Acc].

catch_boolean(boolean) ->
    untyped;
     catch_boolean(T) ->
         T.

collect_messages_stat(Callback, AllQueues, MessagesStat) ->
  [Callback(create_counter(?METRIC_NAME(MetricName), Help, {messages_stat, MSKey, AllQueues}))
   || {MSKey, MetricName, Help} <- prometheus_rabbitmq_message_stats:metrics(), lists:member(MetricName, MessagesStat)].

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

queue_vhost(Queue) ->
  proplists:get_value(vhost, Queue).

queue_name(Queue) ->
  proplists:get_value(name, Queue).

queue_policy(Queue) ->
    proplists:get_value(policy, Queue).

queue_argument(Arg, Queue) ->
    queue_argument(Arg, Queue, '').

queue_argument(Arg, Queue, Default) ->
    maps:get(Arg, proplists:get_value(arguments, Queue), Default).

queue_dir_size(Queue) ->
  QueueDirName = queue_dir_name(Queue),
  FullPath = [mnesia:system_info(directory), "/queues/", QueueDirName],
  dir_size(FullPath).

queue_dir_name(Queue) ->
  VHost = queue_vhost(Queue),
  Name = queue_name(Queue),
  %% http://hustoknow.blogspot.ru/2014/03/how-rabbitmq-computes-name-of-its.html
  <<Num:128>> = erlang:md5(term_to_binary(rabbit_misc:r(VHost, queue, Name))),
  rabbit_misc:format("~.36B", [Num]).

queue_value(Queue, Key) ->
  proplists:get_value(Key, Queue, undefined).

vhost_queues(VHost) ->
  [rabbit_mgmt_format:queue(Q) || Q <- rabbit_amqqueue:list(VHost)].

list_queues(VHost) ->
  Queues = rabbit_mgmt_db:augment_queues(vhost_queues(VHost),
                                         ?NO_RANGE, basic),
  Queues.

list_to_count(Value) when is_list(Value) ->
  length(Value);
list_to_count(Value) ->
  Value.

dir_size(Dir) ->
  filelib:fold_files(Dir, "", true,
                     fun (Name, Acc) ->
                         Acc + filelib:file_size(Name)
                     end, 0).

create_counter(Name, Help, Data) ->
  create_mf(Name, Help, counter, ?MODULE, Data).
