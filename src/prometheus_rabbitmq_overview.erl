-module(prometheus_rabbitmq_overview).

-export([collect_mf/1,
         collect_metrics/2,
         register/1]).

-include("prometheus.hrl").
-import(rabbit_misc, [pget/2]).

collect_mf(Callback) ->
    Callback(gauge, rabbitmq_connections, [vhost], "RabbitMQ Connections count"),
    Callback(gauge, rabbitmq_channels, [vhost], "RabbitMQ Channels count"),
    Callback(gauge, rabbitmq_queues, [vhost], "RabbitMQ Queues count"),
    Callback(gauge, rabbitmq_exchanges, [vhost], "RabbitMQ Exchanges count").

filter_by_vhost(VHost, Channels) ->
    [I || I <- Channels, pget(vhost, I) =:= VHost].

collect_metrics(rabbitmq_connections, Callback) ->
    AllConnections = rabbit_mgmt_db:get_all_connections([]),
    AllVHosts = rabbit_vhost:info_all([name]),
    [Callback([VHost], length(filter_by_vhost(VHost, AllConnections))) || [{name, VHost}] <- AllVHosts];
collect_metrics(rabbitmq_channels, Callback) ->
    AllChannels = rabbit_mgmt_db:get_all_channels([]),
    AllVHosts = rabbit_vhost:info_all([name]),
    [Callback([VHost], length(filter_by_vhost(VHost, AllChannels))) || [{name, VHost}] <- AllVHosts];
collect_metrics(rabbitmq_queues, Callback) ->
    AllVHosts = rabbit_vhost:info_all([name]),
    [Callback([VHost], length(rabbit_amqqueue:list(VHost))) || [{name, VHost}] <- AllVHosts];
collect_metrics(rabbitmq_exchanges, Callback) ->
    AllVHosts = rabbit_vhost:info_all([name]),
    [Callback([VHost], length(rabbit_exchange:list(VHost))) || [{name, VHost}] <- AllVHosts].

register(Registry) ->
    ets:insert_new(?PROMETHEUS_TABLE, {{Registry, collector, prometheus_rabbitmq_overview, 0}, [], ""}).
