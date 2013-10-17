-module(usagi).

-include("logging.hrl").
-include("usagi.hrl").
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

%% @doc start usagi application.
-spec start() -> ok.
start() ->
    application:start(?MODULE).

%% @doc stop usagi application.
-spec stop() -> ok.
stop() ->
    application:stop(?MODULE).

%% @doc start a new channel on default rabbit connection
-spec new_channel(channel()) -> {ok, pid()} | {error, _}.
new_channel(Name) ->
    usagi_agent:get_channel(Name).

%% @doc start a new channel on a rabbit connection
-spec new_channel(rabbit(), channel()) -> {ok, pid()} | {error, _}.
new_channel(Rabbit, Name) ->
    usagi_agent:get_channel(Rabbit, Name).

%% @doc start an exchange
-spec start_exchange(channel(), exchange(), binary()) -> whynot().
start_exchange(Channel, Name, Type) ->
    safe_call_channel(start_exchange, [Channel, Name, Type]).

%% @doc start a queue
-spec start_queue(channel(), r_queue()) -> whynot().
start_queue(Channel, Queue) ->
    safe_call_channel(start_queue, [Channel, Queue]).

%% @doc delete an queue
-spec delete_exchange(channel(), exchange()) -> whynot().
delete_exchange(Channel, Name) ->
    safe_call_channel(delete_exchange, [Channel, Name]).

%% @doc delete a queue
-spec delete_queue(channel(), r_queue()) -> whynot().
delete_queue(Channel, Queue) ->
    safe_call_channel(delete_queue, [Channel, Queue]).

%% @doc publish a message
-spec publish(channel(), exchange(), routing_key(), event()) -> whynot().
publish(Channel, Exchange, Key, Event) ->
    safe_call_channel(publish, [Channel, Exchange, Key, Event]).

%% @doc bind `Source' exchange to `Destination' exchange
-spec bind_exchange(channel(), exchange(), exchange(), routing_key()) -> whynot().
bind_exchange(Channel, Source, Destination, Key) ->
    safe_call_channel(bind_exchange, [Channel, Source, Destination, Key]).

%% @doc bind `Queue' to `Exchange' with `Key' as routing key
-spec bind_queue(channel(), exchange(), exchange(), routing_key()) -> whynot().
bind_queue(Channel, Exchange, Queue, Key) ->
    safe_call_channel(bind_queue, [Channel, Exchange, Queue, Key]).

%% @doc Set `Receiver' as a consumer to `Queue'
-spec consume_queue(channel(), r_queue(), pid()) -> whynot().
consume_queue(Channel, Queue, Receiver) ->
    safe_call_channel(consume_queue, [Channel, Queue, Receiver]).

%% @doc Set `Receiver' as a consumer to `Queue', also set `noack' option
-spec consume_queue(channel(), r_queue(), pid(), boolean()) -> whynot().
consume_queue(Channel, Queue, Receiver, NoAck) ->
    safe_call_channel(consume_queue, [Channel, Queue, Receiver, NoAck]).

%% @doc Cancel consuming with `Tag'
-spec cancel_consume(channel(), binary()) -> whynot().
cancel_consume(Channel, Tag) ->
    safe_call_channel(cancel_consume, [Channel, Tag]).

%% @see ack/3
-spec ack(channel(), binary()) -> whynot().
ack(Channel, Tag) ->
    ack(Channel, Tag, false).

%% @doc Ack messages by delivery tag
-spec ack(channel(), binary(), boolean()) -> whynot().
ack(Channel, Tag, Multi) ->
    safe_call_channel(ack, [Channel, Tag, Multi]).

%% @doc Reject message by delivery tag
-spec reject(channel(), binary()) -> whynot().
reject(Channel, Tag) ->
    safe_call_channel(reject, [Channel, Tag]).

%% @doc Discard message by delivery tag
-spec discard(channel(), binary()) -> whynot().
discard(Channel, Tag) ->
    safe_call_channel(discard, [Channel, Tag]).

%% @doc Get messges from queue
-spec get_msg(channel(), binary()) -> whynot().
get_msg(Channel, Queue) ->
    safe_call_channel(get_msg, [Channel, Queue]).

%% @doc Get messges from queue, optionally setting noack option
-spec get_msg(channel(), binary(), boolean()) -> whynot().
get_msg(Channel, Queue, NoAck) ->
    safe_call_channel(get_msg, [Channel, Queue, NoAck]).

%% @doc Close a channel
-spec close_channel(channel()) -> whynot().
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
