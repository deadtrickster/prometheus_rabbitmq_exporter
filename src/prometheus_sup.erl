%%%-------------------------------------------------------------------
%% @doc prometheus top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(prometheus_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).
-include("prometheus.hrl").
-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  create_tables(),
  {ok, { {one_for_all, 0, 1}, []} }.

%%====================================================================
%% Internal functions
%%====================================================================

create_tables() ->
  Tables = [
            {?PROMETHEUS_TABLE, [set, named_table, public, {read_concurrency, true}]},
            {?PROMETHEUS_COUNTER_TABLE, [set, named_table, public, {write_concurrency, true}]}
           ],
  [maybe_create_table(ets:info(Name), Name, Opts) || {Name, Opts} <- Tables],
  prometheus_vm_memory:register(default),
  prometheus_vm_statistics:register(default),
  prometheus_rabbitmq_overview:register(default),
  prometheus_rabbitmq_queues:register(default),
  ok.

maybe_create_table(undefined, Name, Opts) ->
  ets:new(Name, Opts);
maybe_create_table(_, _, _) ->
  ok.
