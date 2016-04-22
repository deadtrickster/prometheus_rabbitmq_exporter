-module(prometheus_rabbitmq_queues).

-export([collect_mf/1,
         collect_metrics/3,
         register/1]).

-include("prometheus.hrl").
-include_lib("rabbit_common/include/rabbit.hrl").
-import(rabbit_misc, [pget/2]).

collect_mf(Callback) ->
  AllQueues = lists:merge([[Queue || Queue <- list_queues(VHost)] || [{name, VHost}] <- rabbit_vhost:info_all([name])]),
  Callback(gauge, rabbitmq_queue_messages_ready, [vhost, queue], "Number of messages ready to be delivered to clients.", AllQueues),
  Callback(gauge, rabbitmq_queue_messages_unacknowledged, [vhost, queue], "Number of messages delivered to clients but not yet acknowledged.", AllQueues).

collect_metrics(rabbitmq_queue_messages_ready, Callback, AllQueues) ->
  [Callback([proplists:get_value(vhost, Queue), proplists:get_value(name, Queue)], proplists:get_value(messages_ready, Queue)) || Queue <- AllQueues];
collect_metrics(rabbitmq_queue_messages_unacknowledged, Callback, AllQueues) ->
  [Callback([proplists:get_value(vhost, Queue), proplists:get_value(name, Queue)], proplists:get_value(messages_unacknowledged, Queue)) || Queue <- AllQueues].

register(Registry) ->
  ets:insert_new(?PROMETHEUS_TABLE, {{Registry, collector, prometheus_rabbitmq_queues, 0}, [], ""}).

list_queues(VHost) ->
  Queues = rabbit_mgmt_db:augment_queues(
             [rabbit_mgmt_format:queue(Queue) || Queue <- rabbit_amqqueue:list(VHost) ++ rabbit_amqqueue:list_down(VHost)],
             {no_range, no_range, no_range, no_range},
             basic),
  Queues.
