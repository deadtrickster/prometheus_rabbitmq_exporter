-module(rabbit_mgmt_metrics_handler).

-export([init/3]).
-export([format_metrics/2, 
         content_types_provided/2]).

-include_lib("rabbitmq_management/include/rabbit_mgmt.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

%%--------------------------------------------------------------------
init(_, _, _) -> {upgrade, protocol, cowboy_rest}.

content_types_provided(ReqData, Context) ->
   {[{{<<"text">>, <<"html">>, '*'}, format_metrics}], ReqData, Context}.

format_metrics(ReqData, Context) ->
    {prometheus_text:format(), ReqData, Context}.
