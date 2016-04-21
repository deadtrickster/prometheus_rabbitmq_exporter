-module(prometheus_registry).
-export([collect/2]).

-include("prometheus.hrl").

collect(Name, Callback) ->
    [apply(Callback, MF) || MF <-  ets:match(?PROMETHEUS_TABLE, {{Name, '$1', '$2', '_'}, '$3', '$4'})].
    
