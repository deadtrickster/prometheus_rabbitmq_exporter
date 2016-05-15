-module(prometheus_rabbitmq_overview_collector).
-export([register/0,
         register/1,
         deregister/1,
         collect_mf/2,
         collect_metrics/3]).

-include_lib("prometheus/include/prometheus.hrl").
-behaviour(prometheus_collector).

%%====================================================================
%% Collector API
%%====================================================================

register() ->
  register(default).

register(Registry) ->
  ok = prometheus_registry:register_collector(Registry, ?MODULE).

deregister(_) -> ok.

collect_mf(Callback, _Registry) ->
  Callback(gauge, rabbitmq_connections, [vhost], "RabbitMQ Connections count", []),
  Callback(gauge, rabbitmq_channels, [vhost], "RabbitMQ Channels count", []),
  Callback(gauge, rabbitmq_queues, [vhost], "RabbitMQ Queues count", []),
  Callback(gauge, rabbitmq_exchanges, [vhost], "RabbitMQ Exchanges count", []),
  Callback(gauge, rabbitmq_consumers, [], "RabbitMQ Consumers count", []).

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

%%====================================================================
%% Private Parts
%%====================================================================

filter_by_vhost(VHost, Channels) ->
  [I || I <- Channels, rabbit_misc:pget(vhost, I) =:= VHost].

created_events(Type) ->
  ets:select(Type, [{{{'_', '$1'}, '$2', '_'}, [{'==', 'create', '$1'}],
                     ['$2']}]).
