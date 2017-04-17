-module(prometheus_rabbitmq_exporter).

-behaviour(rabbit_mgmt_extension).

-export([dispatcher/0, web_ui/0]).

dispatcher() ->
  application:ensure_all_started(prometheus),
  Path = prometheus_rabbitmq_exporter_config:path(),
  Registry = default,

  prometheus_http:setup(),

  [{Path, prometheus_rabbitmq_exporter_handler, {Registry}}].
web_ui()     -> [].
