-module(prometheus_rabbitmq_message_stats).

-export([metrics/0,
         value/2]).

%%====================================================================
%% Public API
%%====================================================================

metrics() ->
  [{publish, messages_published_total, "Count of messages published."},
   {publish_in, messages_published_in_total, "Count of messages published \"in\" to an exchange, i.e. not taking account of routing."},
   {publish_out, messages_published_out_total, "Count of messages published \"out\" of an exchange, i.e. taking account of routing."},
   {confirm, messages_confirmed_total, "Count of messages confirmed."},
   {deliver, messages_delivered_total, "Count of messages delivered in acknowledgement mode to consumers."},
   {deliver_no_ack, messages_delivered_no_ack_total, "Count of messages delivered in no-acknowledgement mode to consumers."},
   {get, messages_get_total, "Count of messages delivered in acknowledgement mode in response to basic.get."},
   {get_no_ack, messages_get_no_ack_total, "Count of messages delivered in no-acknowledgement mode in response to basic.get."},
   {deliver_get, messages_deliver_get_total, "Sum of *messages_delivered_total, *messages_delivered_no_ack_total, *messages_get_total and *messages_get_no_ack_total."},
   {redeliver, messages_redelivered_total, "Count of subset of delivered messages which had the redelivered flag set."},
   {return_unroutable, messages_returned_total, "Count of messages returned to publisher as unroutable."}].

value(Entity, Name) ->
  case proplists:get_value(message_stats, Entity) of
    undefined -> undefined;
    MessageStats ->
      proplists:get_value(Name, MessageStats)
  end.
