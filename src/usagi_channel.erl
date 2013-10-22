-module(usagi_channel).

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
-export([close/1]).
-export([qos/2]).

-include("usagi.hrl").
-include("logging.hrl").

-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("eunit/include/eunit.hrl").

%% ============================================================================
%% API
%% ============================================================================

-spec start_exchange(channel(), exchange(), binary()) -> ok.
start_exchange(Channel, Name, Type) ->
    Method = #'exchange.declare'{exchange=Name,type = Type},
    #'exchange.declare_ok'{} = call_channel(Channel, Method),
    ok.

-spec start_queue(channel(), r_queue()) -> {ok, {integer(),integer(),integer()}}.
start_queue(Channel, Queue) ->
    Method = declare_queue(Queue),
    #'queue.declare_ok'{queue=Q,message_count=MC,
        consumer_count=CC} = call_channel(Channel, Method),
    {ok, {Q, MC, CC}}.

-spec delete_exchange(channel(), exchange()) -> ok.
delete_exchange(Channel, Name) ->
    Method = #'exchange.delete'{exchange = Name},
    #'exchange.delete_ok'{} = call_channel(Channel, Method),
    ok.

-spec delete_queue(channel(), r_queue()) -> ok.
delete_queue(Channel, Queue) ->
    Method = #'queue.delete'{queue = queue(Queue)},
    #'queue.delete_ok'{} = call_channel(Channel, Method),
    ok.

-spec publish(channel(), exchange(), binary(), binary()) -> ok.
publish(Channel, Exchange, Key, Event) ->
    Publish = #'basic.publish'{exchange=Exchange, routing_key=Key},
    amqp_channel:cast(channel(Channel), Publish, #amqp_msg{payload = Event}).

-spec bind_exchange(channel(), exchange(), exchange(), binary()) -> ok.
bind_exchange(Channel, Src, Dest, Key) ->
    Method = #'exchange.bind'{destination=Dest,source=Src,routing_key=Key},
    #'exchange.bind_ok'{} = call_channel(Channel, Method),
    ok.

-spec bind_queue(channel(), exchange(), r_queue(), binary()) -> ok.
bind_queue(Channel, Exchange, Queue, Key) ->
    Method = #'queue.bind'{queue=queue(Queue),exchange=Exchange,routing_key=Key},
    #'queue.bind_ok'{} = call_channel(Channel, Method),
    ok.

-spec consume_queue(channel(), r_queue(), pid()) -> whynot(binary()).
consume_queue(Channel, Queue, Receiver) ->
    consume_queue(Channel, Queue, Receiver, false).

-spec consume_queue(channel(), r_queue(), pid(), boolean()) -> whynot(binary()).
consume_queue(Channel, Queue, Receiver, NoAck) ->
    Sub = #'basic.consume'{queue = queue(Queue), no_ack = NoAck},
    case catch amqp_channel:subscribe(channel(Channel), Sub, Receiver) of
        #'basic.consume_ok'{consumer_tag=Tag} ->
            {ok, Tag};
        {'EXIT', {{shutdown,{server_initiated_close,404,_}},_}} -> 
            ?error("queue ~p not found", [Queue]),
            {error, queue_not_found};
        {'EXIT', Reason} -> 
            {error, Reason}
    end.

-spec cancel_consume(channel(), binary()) -> ok.
cancel_consume(Channel, Tag) ->
    Method = #'basic.cancel'{consumer_tag = Tag,nowait = true},
    ok = call_channel(Channel, Method).

-spec ack(channel(), binary()) -> ok.
ack(Channel, Tag) ->
    ack(Channel, Tag, false).

-spec ack(channel(), binary(), boolean()) -> ok.
ack(Channel, Tag, Multi) ->
    Ack = #'basic.ack'{delivery_tag = Tag, multiple = Multi},
    amqp_channel:cast(channel(Channel), Ack).

-spec reject(channel(), binary()) -> ok.
reject(Channel, Tag) ->
    Reject = #'basic.reject'{delivery_tag = Tag, requeue = true},
    amqp_channel:cast(channel(Channel), Reject).

-spec discard(channel(), binary()) -> ok.
discard(Channel, Tag) ->
    Reject = #'basic.reject'{delivery_tag = Tag, requeue = false},
    amqp_channel:cast(channel(Channel), Reject).

-spec get_msg(channel(), binary()) -> {ok, {binary(), binary()}} | empty.
get_msg(Channel, Queue) ->
    get_msg(Channel, Queue, true).

-spec get_msg(channel(), binary(), boolean()) -> {ok, {binary(), binary()}} | empty.
get_msg(Channel, Queue, NoAck) ->
    Get = #'basic.get'{queue = queue(Queue), no_ack = NoAck},
    case call_channel(Channel, Get) of
        {#'basic.get_ok'{delivery_tag=Dtag}, #amqp_msg{payload = Content}} ->
            {ok, {Dtag, Content}};
        #'basic.get_empty'{} ->
            empty
    end.

-spec close(channel()) -> ok.
close(Channel) ->
    amqp_channel:close(channel(Channel)).

-spec qos(channel(), integer()) -> ok.
qos(Channel, Prefetch) ->
    Qos = #'basic.qos'{prefetch_count = Prefetch},
    #'basic.qos_ok'{} = call_channel(Channel, Qos),
    ok.

%% ============================================================================
%% Internal functions
%% ============================================================================

declare_queue(Queue) when is_binary(Queue) ->
    #'queue.declare'{queue = Queue};
declare_queue({Queue, Expires}) ->
    Args = [{<<"x-expires">>, long, Expires}],
    #'queue.declare'{queue = Queue, arguments = Args}.

queue({Queue,_}) -> Queue;
queue(Queue) -> Queue.

call_channel(Channel, Method) ->
    case catch amqp_channel:call(channel(Channel), Method) of
        {'EXIT', {{shutdown,Reason},_Stack}} ->
            ?error("call method ~p rabbit channel failed ~p:~n ~p",
                [Method, Channel,Reason]),
            throw({error, Reason});
        {'EXIT', Reason} ->
            ?error("call method ~p rabbit channel failed ~p:~n ~p",
                [Method, Channel,Reason]),
            throw({error, Reason});
        Result -> 
            Result
    end.

channel(Channel) when is_pid(Channel) ->
    Channel;
channel(Channel) ->
    usagi_agent:get_channel(Channel).
