-module(usagi).

-include("logging.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([start/0, stop/0]).

-export([new_channel/1]).
-export([new_channel/2]).
-export([start_exchange/3]).
-export([start_queue/2]).
-export([bind_exchange/4]).
-export([bind_queue/4]).
-export([consume_queue/3]).
-export([consume_queue/4]).
-export([delete_exchange/2]).
-export([delete_queue/2]).
-export([consumer_count/2]).
-export([msg_count/2]).
-export([publish/4]).
-export([cancel_consume/2]).
-export([ack/2]).
-export([ack/3]).
-export([reject/2]).
-export([discard/2]).
-export([get_msg/2]).
-export([get_msg/3]).
-export([close_channel/1]).
-export([unpack/1]).
-export([get_msg/0]).

%% ===================================================================
%% Public
%% ===================================================================

start() ->
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

new_channel(Name) ->
    usagi_agent:get_channel(Name).

new_channel(Rabbit, Name) ->
    usagi_agent:get_channel(Rabbit, Name).

start_exchange(Channel, Name, Type) ->
    safe_call_channel(start_exchange, [Channel, Name, Type]).

start_queue(Channel, Queue) ->
    safe_call_channel(start_queue, [Channel, Queue]).

delete_exchange(Channel, Name) ->
    safe_call_channel(delete_exchange, [Channel, Name]).

delete_queue(Channel, Queue) ->
    safe_call_channel(delete_queue, [Channel, Queue]).

consumer_count(Channel, Queue) ->
    safe_call_channel(consumer_count, [Channel, Queue]).

msg_count(Channel, Queue) ->
    safe_call_channel(msg_count, [Channel, Queue]).

publish(Channel, Exchange, Key, Event) ->
    safe_call_channel(publish, [Channel, Exchange, Key, Event]).

bind_exchange(Channel, Source, Destination, Key) ->
    safe_call_channel(bind_exchange, [Channel, Source, Destination, Key]).

bind_queue(Channel, Exchange, Queue, Key) ->
    safe_call_channel(bind_queue, [Channel, Exchange, Queue, Key]).

consume_queue(Channel, Queue, Receiver) ->
    safe_call_channel(consume_queue, [Channel, Queue, Receiver]).

consume_queue(Channel, Queue, Receiver, NoAck) ->
    safe_call_channel(consume_queue, [Channel, Queue, Receiver, NoAck]).

cancel_consume(Channel, Tag) ->
    safe_call_channel(cancel_consume, [Channel, Tag]).

ack(Channel, Tag) ->
    ack(Channel, Tag, false).

ack(Channel, Tag, Multi) ->
    safe_call_channel(ack, [Channel, Tag, Multi]).

reject(Channel, Tag) ->
    safe_call_channel(reject, [Channel, Tag]).

discard(Channel, Tag) ->
    safe_call_channel(discard, [Channel, Tag]).

get_msg(Channel, Queue) ->
    safe_call_channel(get_msg, [Channel, Queue]).

get_msg(Channel, Queue, NoAck) ->
    safe_call_channel(get_msg, [Channel, Queue, NoAck]).

close_channel(Channel) ->
    safe_call_channel(close, [Channel]).

unpack(#'basic.consume_ok'{}) ->
    ignore;
unpack(#'basic.cancel_ok'{}) ->
    ignore;
unpack({#'basic.deliver'{}, #amqp_msg{payload=Payload}}) ->
    {rabbit_event, Payload};
unpack(Other) ->
    Other.

get_msg() ->
    receive
        #'basic.consume_ok'{} ->
            get_msg();
        #'basic.cancel_ok'{} ->
            get_msg();
        {#'basic.deliver'{}, #amqp_msg{payload=Payload}} ->
            {usagi, Payload};
        Other ->
            Other
    end.

%% ===================================================================
%% Utils
%% ===================================================================

safe_call_channel(Fun, Args) ->
    case catch apply(usagi_channel, Fun, Args) of
        {'EXIT', Reason} ->
            {error, Reason};
        Result ->
            Result
    end.
