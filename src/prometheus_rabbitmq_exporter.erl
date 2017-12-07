-module(prometheus_rabbitmq_exporter).

-behaviour(rabbit_mgmt_extension).

-export([dispatcher/0, web_ui/0]).

dispatcher() ->
  {ok, _} = application:ensure_all_started(prometheus),
  prometheus_registry:register_collector(prometheus_rabbitmq_exchanges_collector),
  prometheus_registry:register_collector(prometheus_rabbitmq_mnesia_tables_collector),
  prometheus_registry:register_collector(prometheus_rabbitmq_nodes_collector),
  prometheus_registry:register_collector(prometheus_rabbitmq_overview_collector),
  prometheus_registry:register_collector(prometheus_rabbitmq_queues_collector),
  Path = prometheus_rabbitmq_exporter_config:path(),

  prometheus_http_impl:setup(),

  [{Path ++ "/[:registry]", prometheus_cowboy2_handler, []}].

web_ui()     -> [].
