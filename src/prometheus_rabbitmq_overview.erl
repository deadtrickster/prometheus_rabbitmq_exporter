-module(prometheus_rabbitmq_overview).

-export([collect_mf/1,
         collect_metrics/3,
         register/1]).

-include("prometheus.hrl").
-import(rabbit_misc, [pget/2]).

collect_mf(Callback) ->
  Callback(gauge, rabbitmq_connections, [vhost], "RabbitMQ Connections count", []),
  Callback(gauge, rabbitmq_channels, [vhost], "RabbitMQ Channels count", []),
  Callback(gauge, rabbitmq_queues, [vhost], "RabbitMQ Queues count", []),
  Callback(gauge, rabbitmq_exchanges, [vhost], "RabbitMQ Exchanges count", []),
  Callback(gauge, rabbitmq_consumers, [], "RabbitMQ Consumers count", []).

filter_by_vhost(VHost, Channels) ->
    [I || I <- Channels, pget(vhost, I) =:= VHost].

collect_metrics(rabbitmq_connections, Callback, _MFData) ->
    AllConnections = created_events(connection_stats),
    AllVHosts = rabbit_vhost:info_all([name]),
    [Callback([VHost], length(filter_by_vhost(VHost, AllConnections))) || [{name, VHost}] <- AllVHosts];
collect_metrics(rabbitmq_channels, Callback, _MFData) ->
    AllChannels = created_events(channel_stats),
    AllVHosts = rabbit_vhost:info_all([name]),
    [Callback([VHost], length(filter_by_vhost(VHost, AllChannels))) || [{name, VHost}] <- AllVHosts];
collect_metrics(rabbitmq_queues, Callback, _MFData) ->
    AllVHosts = rabbit_vhost:info_all([name]),
    [Callback([VHost], length(rabbit_amqqueue:list(VHost))) || [{name, VHost}] <- AllVHosts];
collect_metrics(rabbitmq_exchanges, Callback, _MFData) ->
    AllVHosts = rabbit_vhost:info_all([name]),
    [Callback([VHost], length(rabbit_exchange:list(VHost))) || [{name, VHost}] <- AllVHosts];
collect_metrics(rabbitmq_consumers, Callback, _MFData) ->
    Callback([], ets:info(consumers_by_queue, size)).


register(Registry) ->
    ets:insert_new(?PROMETHEUS_TABLE, {{Registry, collector, prometheus_rabbitmq_overview, 0}, [], ""}).



created_events(Type) ->
    ets:select(Type, [{{{'_', '$1'}, '$2', '_'}, [{'==', 'create', '$1'}],
                       ['$2']}]).
