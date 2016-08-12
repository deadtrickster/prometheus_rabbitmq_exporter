-module(prometheus_rabbitmq_exporter_config).

-export([path/0,
         format/0]).

-define(DEFAULT_PATH, "metrics").
-define(DEFAULT_FORMAT, prometheus_text_format).
-define(DEFAULT_CONFIG, [{path, ?DEFAULT_PATH},
                         {format, ?DEFAULT_FORMAT}]).

config() ->
  application:get_env(prometheus, rabbitmq_exporter, ?DEFAULT_CONFIG).

path() ->
  Config = config(),
  string:tokens(proplists:get_value(path, Config, ?DEFAULT_PATH), "/").

format() ->
  Config = config(),
  proplists:get_value(format, Config, ?DEFAULT_FORMAT).
