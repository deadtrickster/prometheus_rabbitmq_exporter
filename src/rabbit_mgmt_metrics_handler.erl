-module(rabbit_mgmt_metrics_handler).

-export([init/3,
         handle/2,
         terminate/3]).

-include_lib("rabbitmq_management/include/rabbit_mgmt.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

%%--------------------------------------------------------------------
init(_Type, Req, Opts) ->
  {ok, Req, Opts}.

handle(Req, State) ->
  {ok, Req2} = cowboy_req:reply(200, [
                                {<<"content-type">>, <<"text/plain; version=0.0.4">>}
                               ], prometheus_text_format:format(), Req),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.
