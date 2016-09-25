-module(prometheus_rabbitmq_exporter).

-behaviour(rabbit_mgmt_extension).

-export([dispatcher/0, web_ui/0]).

dispatcher() ->
  Path = prometheus_rabbitmq_exporter_config:path(),
  Format = prometheus_rabbitmq_exporter_config:format(),
  Registry = default,

  prometheus_summary:declare([{name, telemetry_scrape_duration_seconds},
                              {help, "Scrape duration"},
                              {labels, ["registry", "content_type"]},
                              {registry, Registry}]),
  prometheus_summary:declare([{name, telemetry_scrape_size_bytes},
                              {help, "Scrape size, uncompressed"},
                              {labels, ["registry", "content_type"]},
                              {registry, Registry}]),

  [{Path, prometheus_rabbitmq_exporter_handler, {Registry, binary_to_list(Format:content_type()), Format}}].
web_ui()     -> [].
