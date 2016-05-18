-module(rabbit_mgmt_metrics_handler).

-export([init/1,
         content_types_provided/2,
         render_metrics/2]).

-include_lib("rabbitmq_management/include/rabbit_mgmt.hrl").
-include_lib("webmachine/include/webmachine.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

%%--------------------------------------------------------------------
init(_Config) -> {ok, #context{}}.

content_types_provided(ReqData, Context) ->
  {[{"text/plain; version=0.0.4", render_metrics}], ReqData, Context}.

render_metrics(ReqData, Context) ->
  {prometheus_text_format:format(), ReqData, Context}.
