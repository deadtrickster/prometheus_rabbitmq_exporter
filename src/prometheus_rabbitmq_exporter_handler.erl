-module(prometheus_rabbitmq_exporter_handler).

-export([init/1,
         content_types_provided/2,
         render_metrics/2]).

-include_lib("webmachine/include/webmachine.hrl").

%%--------------------------------------------------------------------
init(Config) -> {ok, Config}.

content_types_provided(ReqData, {_, ContentType, _} = Config) ->
  {[{ContentType, render_metrics}], ReqData, Config}.

render_metrics(ReqData, {Registry, ContentType, Format} = Config) ->
  Scrape = prometheus_summary:observe_duration(Registry,
                                               telemetry_scrape_duration_seconds,
                                               [Registry, ContentType],
                                               fun () -> Format:format(Registry) end),
  prometheus_summary:observe(Registry,
                             telemetry_scrape_size_bytes,
                             [Registry, ContentType],
                             iolist_size(Scrape)),
  {Scrape, ReqData, Config}.
