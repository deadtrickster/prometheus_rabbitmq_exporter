-module(prometheus_rabbitmq_nodes_collector).
-export([register/0,
         register/1,
         deregister_cleanup/1,
         collect_mf/2,
         collect_metrics/2]).

-import(prometheus_model_helpers, [create_mf/5,
                                   label_pairs/1,
                                   gauge_metrics/1,
                                   gauge_metric/1,
                                   gauge_metric/2,
                                   counter_metric/1,
                                   counter_metric/2,
                                   untyped_metric/1,
                                   untyped_metric/2]).

-include("prometheus_rabbitmq_exporter.hrl").
-behaviour(prometheus_collector).

%%====================================================================
%% Collector API
%%====================================================================

register() ->
  register(default).

register(Registry) ->
  ok = prometheus_registry:register_collector(Registry, ?MODULE).

deregister_cleanup(_) -> ok.

collect_mf(_Registry, Callback) ->
  Nodes = all_nodes_raw(),
  Callback(create_untyped(rabbitmq_node_up, "Node runnning status", Nodes)),
  ok.

collect_metrics(rabbitmq_node_up, Nodes) ->
  [untyped_metric(labels(Node), node_running(Node)) || Node <- Nodes].

%%====================================================================
%% Private Parts
%%====================================================================

%% just copied from rabbit_mgmt_wm_nodes
%% [[{name,hare@home},{type,disc},{running,true}],
%%  [{name,rabbit@home},{type,disc},{running,false}]]`
all_nodes_raw() ->
  S = rabbit_mnesia:status(),
  Nodes = proplists:get_value(nodes, S),
  Types = proplists:get_keys(Nodes),
  Running = proplists:get_value(running_nodes, S),
  [[{name, Node}, {type, Type}, {running, lists:member(Node, Running)}] ||
    Type <- Types, Node <- proplists:get_value(Type, Nodes)].

labels(Node) ->
  [{name, node_name(Node)},
   {type, node_type(Node)}].

node_name(Node) ->
  proplists:get_value(name, Node).

node_type(Node) ->
  proplists:get_value(type, Node).

node_running(Node) ->
  case proplists:get_value(running, Node) of
    true -> 1;
    _ -> 0
  end.

create_untyped(Name, Help, Data) ->
  create_mf(Name, Help, untyped, ?MODULE, Data).
