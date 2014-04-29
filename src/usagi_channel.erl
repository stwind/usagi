-module(usagi_channel).

-export([start_exchange/3, start_exchange/4]).
-export([start_queue/2, start_queue/3]).
-export([bind_exchange/4, bind_exchange/5]).
-export([bind_queue/4, bind_queue/5]).
-export([consume_queue/3]).
-export([consume_queue/4]).
-export([delete_exchange/2, delete_exchange/3]).
-export([delete_queue/2, delete_queue/3]).
-export([publish/4, publish/6]).
-export([cancel_consume/2, cancel_consume/3]).
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

-type exchange() :: usagi_types:exchange().
-type channel() :: usagi_types:channel().
-type r_queue() :: usagi_types:r_queue().
-type whynot(R) :: usagi_types:whynot(R).

%% ============================================================================
%% API
%% ============================================================================

-spec start_exchange(channel(), exchange(), binary()) -> ok.
start_exchange(Channel, Name, Type) when is_binary(Type) ->
    start_exchange(Channel, Name, Type, []).

start_exchange(Channel, Name, Type, Opts) ->
    Method = usagi_recs:'#fromlist-exchange.declare'(
               Opts, #'exchange.declare'{exchange=Name, type=Type}),
    #'exchange.declare_ok'{} = call_channel(Channel, Method),
    ok.

%-spec start_queue(channel(), r_queue()) -> {ok, {integer(),integer(),integer()}}.
start_queue(Channel, Queue) ->
    start_queue(Channel, Queue, []).

start_queue(Channel, Queue, Opts) ->
    Method = usagi_recs:'#fromlist-queue.declare'(Opts, declare_queue(Queue)),
    #'queue.declare_ok'{queue=Q,message_count=MC,consumer_count=CC} =
        call_channel(Channel, Method),
    {ok, {Q, MC, CC}}.

-spec delete_exchange(channel(), exchange()) -> ok.
delete_exchange(Channel, Name) ->
    delete_exchange(Channel, Name, []).

delete_exchange(Channel, Name, Opts) ->
    Method = usagi_recs:'#fromlist-exchange.delete'(Opts, #'exchange.delete'{exchange = Name}),
    #'exchange.delete_ok'{} = call_channel(Channel, Method),
    ok.

-spec delete_queue(channel(), r_queue()) -> ok.
delete_queue(Channel, Queue) ->
    delete_queue(Channel, Queue, []).
delete_queue(Channel, Queue, Opts) ->
    Method = usagi_recs:'#fromlist-queue.delete'(Opts, #'queue.delete'{queue = queue(Queue)}),
    #'queue.delete_ok'{} = call_channel(Channel, Method),
    ok.

-spec publish(channel(), exchange(), binary(), binary()) -> ok.
publish(Channel, Exchange, Key, Event) ->
    publish(Channel, Exchange, Key, Event, [], []).

%% @doc `props` option is delivered to #amqp_msg{}
%% @end
publish(Channel, Exchange, Key, Event, Opts, Props) ->
    Props1 = usagi_recs:'#fromlist-P_basic'(Props),
    Publish = usagi_recs:'#fromlist-basic.publish'(
                Opts, #'basic.publish'{exchange=Exchange, routing_key=Key}),
    case proplists:get_value(mandatory, Opts, false)
        orelse proplists:get_value(immediate, Opts, false) of
        true ->
            amqp_channel:call(channel(Channel), Publish, #amqp_msg{props=Props1, payload = Event});
        false ->
            amqp_channel:cast(channel(Channel), Publish, #amqp_msg{props=Props1, payload = Event})
    end.

-spec bind_exchange(channel(), exchange(), exchange(), binary()) -> ok.
bind_exchange(Channel, Src, Dest, Key) ->
    bind_exchange(Channel, Src, Dest, Key, []).

bind_exchange(Channel, Src, Dest, Key, Opts) ->
    Method = usagi_recs:'#fromlist-exchange.bind'(
               Opts, #'exchange.bind'{destination=Dest,source=Src,routing_key=Key}),
    #'exchange.bind_ok'{} = call_channel(Channel, Method),
    ok.

-spec bind_queue(channel(), exchange(), r_queue(), binary()) -> ok.
bind_queue(Channel, Exchange, Queue, Key) ->
    bind_queue(Channel, Exchange, Queue, Key, []).

bind_queue(Channel, Exchange, Queue, Key, Opts) ->
    Method = usagi_recs:'#fromlist-queue.bind'(
               Opts, #'queue.bind'{queue=queue(Queue),exchange=Exchange,routing_key=Key}),
    #'queue.bind_ok'{} = call_channel(Channel, Method),
    ok.

-spec consume_queue(channel(), r_queue(), pid()) -> whynot(binary()).
consume_queue(Channel, Queue, Receiver) ->
    consume_queue(Channel, Queue, Receiver, false).

-spec consume_queue(channel(), r_queue(), pid(), boolean()) -> whynot(binary()).
consume_queue(Channel, Queue, Receiver, NoAck) when is_boolean(NoAck) ->
    consume_queue(Channel, Queue, Receiver, [{no_ack, NoAck}]);
consume_queue(Channel, Queue, Receiver, Opts) ->
    Sub = usagi_recs:'#fromlist-basic.consume'(Opts, #'basic.consume'{queue = queue(Queue)}),
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
    cancel_consume(Channel, Tag, true).

cancel_consume(Channel, Tag, Nowait) ->
    Method = #'basic.cancel'{consumer_tag = Tag,nowait = Nowait},
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
        {#'basic.get_ok'{delivery_tag=Dtag}, #amqp_msg{payload = Content, props=Props}} ->
            Props1 =
                case usagi_recs:'#is_record-'('P_basic', Props) of
                    true ->
                        lists:zip(usagi_recs:'#info-'('P_basic', fields), tl(tuple_to_list(Props)));
                    false ->
                        []
                end,
            {ok, {Dtag, Content, Props1}};
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
    {ok, Pid} = usagi_agent:get_channel(Channel),
    Pid.
