-module(rabbit_metrics_mgmt).

-behaviour(rabbit_mgmt_extension).

-export([dispatcher/0, web_ui/0]).

dispatcher() -> [{"/metrics",        rabbit_mgmt_metrics_handler, []}].
web_ui()     -> [].
