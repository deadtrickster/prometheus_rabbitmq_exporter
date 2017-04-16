-module(prometheus_rabbitmq_mnesia_tables_collector).

-export([register/0,
         register/1,
         deregister_cleanup/1,
         collect_mf/2,
         collect_metrics/2]).

-include("prometheus_rabbitmq_exporter.hrl").

-import(prometheus_model_helpers, [create_mf/5,
                                   label_pairs/1,
                                   gauge_metrics/1,
                                   gauge_metric/1,
                                   gauge_metric/2,
                                   counter_metric/1,
                                   counter_metric/2,
                                   untyped_metric/1,
                                   untyped_metric/2]).

-behaviour(prometheus_collector).

-define(RABBIT_TABLES, [rabbit_durable_exchange,
                        rabbit_durable_queue,
                        rabbit_durable_route,
                        rabbit_exchange,
                        rabbit_exchange_serial,
                        rabbit_listener,
                        rabbit_queue,
                        rabbit_reverse_route,
                        rabbit_route,
                        rabbit_runtime_parameters,
                        rabbit_semi_durable_route,
                        rabbit_topic_trie_binding,
                        rabbit_topic_trie_edge,
                        rabbit_topic_trie_node,
                        rabbit_user,
                        rabbit_user_permission,
                        rabbit_vhost]).

-define(METRIC_NAME_PREFIX, "rabbitmq_mnesia_table_").

-define(METRIC_NAME(S), ?METRIC_NAME_PREFIX ++ atom_to_list(S)).

%% metric {Key, Type, Help, &optional Fun}
-define(METRICS, [{read_only, untyped, "Access mode of the table, 1 if table is read_only or 0 otherwise.",
                   fun(_T, Info) ->
                       case proplists:get_value(access_mode, Info) of
                         read_only -> 1;
                         _  -> 0
                       end
                   end},
                  {disc_copies, gauge, "Number of the nodes where a disc_copy of the table resides according to the schema."},
                  {disc_only_copies, gauge, "Number of the nodes where a disc_only_copy of the table resides according to the schema."},
                  {local_content, untyped, "If the table is configured to have locally unique content on each node, value is 1 or 0 otherwise.",
                   fun(_T, Info) ->
                       case proplists:get_value(local_content, Info) of
                         true -> 1;
                         _  -> 0
                       end
                   end},
                  {majority_required, untyped, "If 1, a majority of the table replicas must be available for an update to succeed.",
                   fun(_T, Info) ->
                       case proplists:get_value(majority, Info) of
                         true -> 1;
                         _  -> 0
                       end
                   end},
                  {master_nodes, gauge, "Number of the master nodes of a table."},
                  {memory_bytes, gauge, "The number of bytes allocated to the table on this node.",
                   fun (_T, Info) ->
                       proplists:get_value(memory, Info) *  erlang:system_info(wordsize)
                   end},
                  {ram_copies, gauge, "Number of the nodes where a ram_copy of the table resides according to the schema."},
                  {records_count, gauge, "Number of records inserted in the table.",
                   fun (_T, Info) ->
                       proplists:get_value(size, Info)
                   end},
                  {disk_size_bytes, gauge, "Disk space occupied by the table (DCL + DCD).",
                   fun (T, _) ->
                       filelib:fold_files(mnesia:system_info(directory), atom_to_list(T), false,
                                          fun (Name, Acc) ->
                                              Acc + filelib:file_size(Name)
                                          end, 0)
                   end}]).

%%====================================================================
%% Collector API
%%====================================================================

register() ->
  register(default).

register(Registry) ->
  ok = prometheus_registry:register_collector(Registry, ?MODULE).

deregister_cleanup(_) -> ok.

collect_mf(_Registry, Callback) ->
  Tables = [{Table, mnesia:table_info(Table, all)} || Table <- ?RABBIT_TABLES],
  [mf(Callback, Metric, Tables) || Metric <- ?METRICS],
  ok.

mf(Callback, Metric, Tables) ->
  {Name, Type, Help, Fun} = case Metric of
                                {Key, Type1, Help1} ->
                                {Key, Type1, Help1, fun (_Table, Info) ->
                                                        list_to_count(proplists:get_value(Key, Info))
                                                    end};
                                {Key, Type1, Help1, Fun1} ->
                                  {Key, Type1, Help1, Fun1}
                            end,
  Callback(create_mf(?METRIC_NAME(Name), Help, Type, ?MODULE, {Type, Fun, Tables})).


collect_metrics(_, {Type, Fun, Tables}) ->
  [metric(Type, [{table, Table}], Fun(Table, Info)) || {Table, Info} <- Tables].

metric(gauge, Labels, Value) ->
  gauge_metric(Labels, Value);
metric(untyped, Labels, Value) ->
  untyped_metric(Labels, Value).

%%====================================================================
%% Private Parts
%%====================================================================

list_to_count(Value) when is_list(Value) ->
  length(Value);
list_to_count(Value) ->
  Value.
