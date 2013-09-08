-module(usagi_channel).

-export([start_link/3]).
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
-export([close/1]).
-export([qos/2]).

-include("logging.hrl").

-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("eunit/include/eunit.hrl").

%% ============================================================================
%% API
%% ============================================================================

start_link(Name, Rabbit, Prefetch) ->
    ?info("opening rabbit channel to ~p: ~p",[Rabbit, Name]),
    gen_server:start_link({local, Name}, ?MODULE, [Rabbit, Prefetch], []).

start_exchange(Channel, Name, Type) ->
    Method = #'exchange.declare'{exchange=Name,type = Type},
    #'exchange.declare_ok'{} = call_channel(Channel, Method),
    ok.

start_queue(Channel, Queue) ->
    Method = declare_queue(Queue),
    #'queue.declare_ok'{queue=Q,message_count=MC,
        consumer_count=CC} = call_channel(Channel, Method),
    {Q, MC, CC}.

delete_exchange(Channel, Name) ->
    Method = #'exchange.delete'{exchange = Name},
    #'exchange.delete_ok'{} = call_channel(Channel, Method),
    ok.

delete_queue(Channel, Queue) ->
    Method = #'queue.delete'{queue = queue(Queue)},
    #'queue.delete_ok'{} = call_channel(Channel, Method),
    ok.

consumer_count(Channel, Queue) ->
    {_, _, CC} = start_queue(Channel, Queue),
    CC.

msg_count(Channel, Queue) ->
    {_, MC, _} = start_queue(Channel, Queue),
    MC.

publish(Channel, Exchange, Key, Event) ->
    Publish = #'basic.publish'{exchange=Exchange, routing_key=Key},
    amqp_channel:cast(channel(Channel), Publish, #amqp_msg{payload = Event}),
    ok.

bind_exchange(Channel, Src, Dest, Key) ->
    Method = #'exchange.bind'{destination=Dest,source=Src,routing_key=Key},
    #'exchange.bind_ok'{} = call_channel(Channel, Method),
    ok.

bind_queue(Channel, Exchange, Queue, Key) ->
    Method = #'queue.bind'{queue=queue(Queue),exchange=Exchange,routing_key=Key},
    #'queue.bind_ok'{} = call_channel(Channel, Method),
    ok.

consume_queue(Channel, Queue, Receiver) ->
    consume_queue(Channel, Queue, Receiver, false).

consume_queue(Channel, Queue, Receiver, NoAck) ->
    Sub = #'basic.consume'{queue = queue(Queue), no_ack = NoAck},
    case catch amqp_channel:subscribe(channel(Channel), Sub, Receiver) of
        #'basic.consume_ok'{consumer_tag=Tag} ->
            Tag;
        {'EXIT', {{shutdown,{server_initiated_close,404,_}},_}} -> 
            ?error("queue ~p not found", [Queue]),
            {error, queue_not_found};
        {'EXIT', Reason} -> 
            {error, Reason}
    end.

cancel_consume(Channel, Tag) ->
    Method = #'basic.cancel'{consumer_tag = Tag,nowait = true},
    ok = call_channel(Channel, Method),
    ok.

ack(Channel, Tag) ->
    ack(Channel, Tag, false).

ack(Channel, Tag, Multi) ->
    Ack = #'basic.ack'{delivery_tag = Tag, multiple = Multi},
    amqp_channel:cast(channel(Channel), Ack),
    ok.

reject(Channel, Tag) ->
    Reject = #'basic.reject'{delivery_tag = Tag, requeue = true},
    amqp_channel:cast(channel(Channel), Reject),
    ok.

discard(Channel, Tag) ->
    Reject = #'basic.reject'{delivery_tag = Tag, requeue = false},
    amqp_channel:cast(channel(Channel), Reject),
    ok.

get_msg(Channel, Queue) ->
    get_msg(Channel, Queue, true).

-spec get_msg(atom(), binary(), boolean()) -> empty | binary().
get_msg(Channel, Queue, NoAck) ->
    Get = #'basic.get'{queue = queue(Queue), no_ack = NoAck},
    case call_channel(Channel, Get) of
        {#'basic.get_ok'{delivery_tag=Dtag}, #amqp_msg{payload = Content}} ->
            {Dtag, Content};
        #'basic.get_empty'{} ->
            empty
    end.

close(Channel) ->
    amqp_channel:close(channel(Channel)).

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
    Args = [{<<"x-expires">>, short, Expires}],
    #'queue.declare'{queue = Queue, arguments = Args}.

queue({Queue,_}) -> Queue;
queue(Queue) -> Queue.

call_channel(Channel, Method) ->
    case catch amqp_channel:call(channel(Channel), Method) of
        {'EXIT', {{shutdown,Reason},_Stack}} ->
            ?error("call method ~p rabbit channel failed ~p: ~p",
                [Method, Channel,Reason]),
            {error, Reason};
        {'EXIT', Reason} ->
            ?error("call method ~p rabbit channel failed ~p: ~p",
                [Method, Channel,Reason]),
            {error, Reason};
        Result -> 
            Result
    end.

channel(Channel) when is_pid(Channel) ->
    Channel;
channel(Channel) ->
    usagi_agent:get_channel(Channel).
