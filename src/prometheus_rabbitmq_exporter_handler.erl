-module(prometheus_rabbitmq_exporter_handler).

-include_lib("prometheus_httpd/include/prometheus_http.hrl").

-export([init/3,
         handle/2,
         terminate/3]).

%%--------------------------------------------------------------------
init(_Type, Req, Opts) ->
  {ok, Req, Opts}.

handle(Req, {Registry}) ->
  URI = true,
  GetHeader = fun(Name, Default) ->
                  {Value, _} = cowboy_req:header(iolist_to_binary(Name), Req, Default),
                  Value
              end,

  %% TODO: check method, response only to GET
  {Code, RespHeaders0, Body} = prometheus_http_impl:reply(#request{path = URI,
                                                              headers = GetHeader,
                                                              registry = Registry,
                                                              standalone = false}),

  ContentLength = integer_to_list(iolist_size(Body)),
  RespHeaders = lists:map(fun to_cowboy_headers/1,
                          RespHeaders0 ++ [{content_length, ContentLength}]),

  {ok, Req2} = cowboy_req:reply(Code, RespHeaders, Body, Req),
  {ok, Req2, undefined}.

terminate(_Reason, _Req, _State) ->
  ok.

to_cowboy_headers({Name, Value}) ->
  {to_cowboy_name(Name), Value}.

to_cowboy_name(Name) ->
  binary:replace(atom_to_binary(Name, utf8), <<"_">>, <<"-">>).
