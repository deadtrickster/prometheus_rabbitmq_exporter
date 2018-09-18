-module(prometheus_rabbitmq_exporter_handler).

-export([init/2]).
-export([generate_response/2, content_types_provided/2, is_authorized/2]).

-include_lib("rabbitmq_management_agent/include/rabbit_mgmt_records.hrl").

%% ===================================================================
%% Cowboy Handler Callbacks
%% ===================================================================

init(Req, _State) ->
  {cowboy_rest, Req, #context{}}.

content_types_provided(ReqData, Context) ->
  {[
    {<<"*/*">>, generate_response}
   ], ReqData, Context}.

is_authorized(ReqData, Context) ->
  case prometheus_rabbitmq_exporter_config:use_mgmt_auth() of
    false ->
      {true, ReqData, Context};
    true ->
      rabbit_mgmt_util:is_authorized(ReqData, Context)
  end.

%% ===================================================================
%% Private functions
%% ===================================================================

generate_response(ReqData, Context) ->
  {ok, Response, undefined} = prometheus_cowboy2_handler:init(ReqData, Context),
  {stop, Response, Context}.
