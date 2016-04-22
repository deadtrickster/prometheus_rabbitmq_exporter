-module(prometheus_vm).
-export([collect_mf/1,
         collect_metrics/3,
         register/1]).

-include("prometheus.hrl").

collect_mf(Callback) ->
  Callback(gauge, erlang_vm_memory_bytes, [kind], "Erlang VM Memory Metrics", []),
  Callback(gauge, erlang_vm_ets_tables, [], "Erlang VM ETS Tables count", []).

collect_metrics(erlang_vm_memory_bytes, Callback, _MFData) ->
  [Callback([Name], Value) || {Name, Value} <- erlang:memory()];
collect_metrics(erlang_vm_ets_tables, Callback, _MFData) ->
  Callback([], length(ets:all())).

register(Registry) ->
  ets:insert_new(?PROMETHEUS_TABLE, {{Registry, collector, prometheus_vm, 0}, [], ""}).
