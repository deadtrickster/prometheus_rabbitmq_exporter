-module(prometheus_rabbitmq_queues).

-export([collect_mf/1,
         collect_metrics/3,
         register/1]).

-include("prometheus.hrl").
-include_lib("rabbit_common/include/rabbit.hrl").
-import(rabbit_misc, [pget/2]).

-define(QUEUE_GAUGES, [{"messages_ready", "Number of messages ready to be delivered to clients."},
                       {"messages_unacknowledged", "Number of messages delivered to clients but not yet acknowledged."},
                       {"messages", "Sum of ready and unacknowledged messages (queue depth)."},
                       {"messages_ready_ram", "Number of messages from messages_ready which are resident in ram."},
                       {"messages_unacknowledged_ram", "Number of messages from messages_unacknowledged which are resident in ram."},
                       {"messages_ram", "Total number of messages which are resident in ram."},
                       {"messages_persistent", "Total number of persistent messages in the queue (will always be 0 for transient queues)."},
                       {"message_bytes", "Sum of the size of all message bodies in the queue. This does not include the message properties (including headers) or any overhead."},
                       {"message_bytes_ready", "Like message_bytes but counting only those messages ready to be delivered to clients."},
                       {"message_bytes_unacknowledged", "Like message_bytes but counting only those messages delivered to clients but not yet acknowledged."},
                       {"message_bytes_ram", "Like message_bytes but counting only those messages which are in RAM."},
                       {"message_bytes_persistent", "Like message_bytes but counting only those messages which are persistent."},
                       {"consumers", "Number of consumers."},
                       {"consumer_utilisation", "Fraction of the time (between 0.0 and 1.0) that the queue is able to immediately deliver messages to consumers. This can be less than 1.0 if consumers are limited by network congestion or prefetch count."},
                       {"memory", "Bytes of memory consumed by the Erlang process associated with the queue, including stack, heap and internal structures."}]).

-define(QUEUE_COUNTERS, [{"disk_reads", "Total number of times messages have been read from disk by this queue since it started."},
                         {"disk_writes", "Total number of times messages have been written to disk by this queue since it started."},
                         {"messages_published_total", "Count of messages published."},
                         {"messages_confirmed_total", "Count of messages confirmed."},
                         {"messages_redelivered_total", "Count of subset of messages in rabbitmq_queue_messages_delivered_total which had the redelivered flag set."}]).


-define(METRIC_NAME_PREFIX, "rabbitmq_queue_").

collect_mf(Callback) ->
  AllQueues = lists:merge([[Queue || Queue <- list_queues(VHost)] || [{name, VHost}] <- rabbit_vhost:info_all([name])]),
  [Callback(gauge, ?METRIC_NAME_PREFIX ++ QueueKey, [vhost, queue], Help, AllQueues) || {QueueKey, Help} <- ?QUEUE_GAUGES],
  [Callback(counter, ?METRIC_NAME_PREFIX ++ QueueKey, [vhost, queue], Help, AllQueues) || {QueueKey, Help} <- ?QUEUE_COUNTERS],
  Callback(counter, ?METRIC_NAME_PREFIX ++ "messages_delivered_total", [vhost, queue, mode], "Count of messages delivered to consumers.", AllQueues).

collect_metrics("rabbitmq_queue_messages_published_total", Callback, AllQueues) ->
  [set_message_stats_metric_value(publish, Callback, Queue) || Queue <- AllQueues];
collect_metrics("rabbitmq_queue_messages_confirmed_total", Callback, AllQueues) ->
  [set_message_stats_metric_value(confirm, Callback, Queue) || Queue <- AllQueues];
collect_metrics("rabbitmq_queue_messages_redelivered_total", Callback, AllQueues) ->
  [set_message_stats_metric_value(redeliver, Callback, Queue) || Queue <- AllQueues];
collect_metrics("rabbitmq_queue_messages_delivered_total", Callback, AllQueues) ->
  [set_message_stats_deliver_value(Callback, Queue) || Queue <- AllQueues];
collect_metrics(MetricName, Callback, AllQueues) ->
  QueueKey = list_to_atom(string:sub_string(MetricName, 1 + length(?METRIC_NAME_PREFIX))),
  [maybe_call_metric_callback(Callback, Queue, queue_value(Queue, QueueKey)) || Queue <- AllQueues].

set_message_stats_metric_value(MSMName, Callback, Queue) ->
  case queue_value(Queue, message_stats) of
    '' -> ok;
    MessageStats ->
      maybe_call_metric_callback(Callback, Queue, queue_value(MessageStats, MSMName))
  end.

set_message_stats_deliver_value(Callback, Queue) ->
  case queue_value(Queue, message_stats) of
    '' -> ok;
    MessageStats ->
      Callback([proplists:get_value(vhost, Queue), proplists:get_value(name, Queue), deliver], queue_value(MessageStats, deliver)),
      Callback([proplists:get_value(vhost, Queue), proplists:get_value(name, Queue), deliver_noack], queue_value(MessageStats, deliver_no_ack)),
      Callback([proplists:get_value(vhost, Queue), proplists:get_value(name, Queue), get], queue_value(MessageStats, get)),
      Callback([proplists:get_value(vhost, Queue), proplists:get_value(name, Queue), get_noack], queue_value(MessageStats, get_no_ack))
  end.

maybe_call_metric_callback(Callback, Queue, Value) ->
  case Value of
    '' ->
      ok;
    Value ->
      Callback([proplists:get_value(vhost, Queue), proplists:get_value(name, Queue)], Value)
  end.

queue_value(Queue, Key) ->
  proplists:get_value(Key, Queue, '').

register(Registry) ->
  prometheus_registry:register_collector(Registry,  ?MODULE).

list_queues(VHost) ->
  Queues = rabbit_mgmt_db:augment_queues(
             [rabbit_mgmt_format:queue(Queue) || Queue <- rabbit_amqqueue:list(VHost) ++ rabbit_amqqueue:list_down(VHost)],
             {no_range, no_range, no_range, no_range},
             basic),
  Queues.
