-module(prometheus_rabbitmq_exporter).

-behaviour(rabbit_mgmt_extension).

-export([dispatcher/0, web_ui/0]).

dispatcher() ->
  Path = prometheus_rabbitmq_exporter_config:path(),
  Format = prometheus_rabbitmq_exporter_config:format(),
  [{Path, prometheus_rabbitmq_exporter_handler, {binary_to_list(Format:content_type()), Format}}].
web_ui()     -> [].
