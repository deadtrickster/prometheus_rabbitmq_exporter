-module(prometheus_rabbitmq_exporter_handler).

-export([init/1,
         content_types_provided/2,
         render_metrics/2]).

-include_lib("webmachine/include/webmachine.hrl").

%%--------------------------------------------------------------------
init(Config) -> {ok, Config}.

content_types_provided(ReqData, {ContentType, _} = Config) ->
  {[{ContentType, render_metrics}], ReqData, Config}.

render_metrics(ReqData, {_, Format} = Config) ->
  {Format:format(), ReqData, Config}.
