-module(usagi_agent).

-behaviour(gen_server).

%% callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
        code_change/3]).

-export([
        start_link/1,
        add_rabbit/1,
        get_channel/1,
        get_channel/2
    ]).

-include("logging.hrl").

-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(SERVER, ?MODULE).
-define(RETRY_WAIT, 10000).
-define(CHANNELS, rabbit_channels).
-define(RABBITS, rabbits).

-record(state, {
        pending = [],
        retry_wait = ?RETRY_WAIT :: integer()
    }).

%% ===================================================================
%% API
%% ===================================================================

start_link(Rabbits) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Rabbits, []).

add_rabbit(Rabbit) ->
    gen_server:call(?SERVER, {add_rabbit, Rabbit}).

get_channel(Key) ->
    get_channel(default_rabbit(), Key).

get_channel(Name, Key) ->
    case maybe_valid_channel(Name, Key) of
        false ->
            case gen_server:call(?SERVER, {get_rabbit, Name}) of
                {error, _} = Err ->
                    Err;
                Rabbit ->
                    {ok, Channel} = open_channel(Rabbit, Key),
                    Channel
            end;
        {ok, Channel} ->
            Channel
    end.

%% ===================================================================
%% gen_server
%% ===================================================================

init(Rabbits) ->
    _ = ets:new(?CHANNELS, [named_table,public]),
    _ = ets:new(?RABBITS, [named_table,public,{read_concurrency,true}]),
    {ok, connect_rabbits(Rabbits, #state{})}.

handle_call({add_rabbit, Rabbit}, _From, State) ->
    {reply, ok, connect_rabbits([Rabbit], State)};

handle_call({get_rabbit, Name}, _From, State) ->
    {Rabbit, State1} = get_rabbit(Name, State),
    {reply, Rabbit, State1};

handle_call(_Call, _From, State) ->
    {reply, ok, State}.

handle_cast({monitor_channel, Channel}, State) ->
    erlang:monitor(process, Channel),
    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({retry_rabbit, Rabbit}, State) ->
    {noreply, connect_rabbits([Rabbit], State)};

handle_info({retry_channel, Rabbit, Key}, State) ->
    _ = open_channel(Rabbit, Key),
    {noreply, State};

handle_info({'DOWN', _, _, Pid, Reason}, State) ->
    {noreply, maybe_connection_down({Pid, Reason}, State)};

handle_info({nodedown, Node}, State) ->
    ?error("rabbit node down: ~p", [Node]),
    {noreply, node_down(Node, State)};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Internal functions
%% ===================================================================

reconnect_rabbits(#state{pending = Pending} = State) ->
    connect_rabbits(Pending, State#state{pending = []}).

connect_rabbits(Rabbits, State) ->
    lists:foldl(fun do_connect/2, State, Rabbits).

do_connect({Name, Props}, State) ->
    AmqpParam = amqp_param(Props),
    case amqp_connection:start(AmqpParam) of
        {ok, Conn} ->
            _ = monitor_rabbit(Conn, AmqpParam),
            rabbit_connected({Name, Props}, Conn, State);
        {error, Reason} ->
            ?error("failed connecting to ~p : ~p", [Name, Reason]),
            retry_rabbit({Name, Props}, State)
    end.

retry_rabbit(Rabbit, #state{pending=Pending} = State) ->
    ?info("retrying rabbit ~p in ~b ms",[rabbit_name(Rabbit), ?RETRY_WAIT]),
    {ok, _TRef} = timer:send_after(?RETRY_WAIT, {retry_rabbit, Rabbit}),
    State#state{pending=[Rabbit | Pending]}.

retry_channel({Rabbit, Conn}, Key, State) ->
    ?info("retrying channel ~p in ~b ms",[rabbit_name(Rabbit), ?RETRY_WAIT]),
    {ok, _TRef} = timer:send_after(?RETRY_WAIT, {retry_channel, {Rabbit, Conn}, Key}),
    State.

monitor_rabbit(Conn, #amqp_params_direct{node=Node}) ->
    erlang:monitor(process, Conn),
    erlang:monitor_node(Node, true);
monitor_rabbit(Conn, _) ->
    erlang:monitor(process, Conn).

monitor_channel(Channel) ->
    gen_server:cast(?MODULE, {monitor_channel, Channel}).

maybe_connection_down({Pid, Reason}, State) ->
    case ets:match(?RABBITS, {'$1', '$2', Pid}) of
        [[Name, Rabbit]] ->
            ?error("connection to rabbit ~p was down: ~p", 
                [rabbit_name(Rabbit), Reason]),
            ets:delete(?RABBITS, Name),
            ets:match_delete(?CHANNELS, {'_',{'_',Pid},'_'}),
            retry_rabbit(Rabbit, State);
        [] ->
            channel_down({Pid, Reason}, State)
    end.

channel_down({_, normal}, State) ->
    State;
channel_down({Pid, Reason}, State) ->
    case ets:match(?CHANNELS, {'$1', '$2', Pid}) of
        [[Key, Rabbit]] ->
            ?error("channel ~p (~p) was down: ~p", [raw_key(Key), Pid, Reason]),
            ets:delete(?CHANNELS, raw_key(Key)),
            retry_channel(Rabbit, Key, State);
        [] ->
            State
    end.

node_down(Node, State) ->
    ets:foldl(
      fun({Name, Props, Conn}, S) -> 
              case proplists:get_value(node, Props) of
                  Node ->
                      ?error("connection to rabbit ~p was down: nodedown", 
                             [Name]),
                      amqp_connection:close(Conn),
                      ets:delete(?RABBITS, Name),
                      retry_rabbit({Name, Props}, S);
                  _ ->
                      S
              end
      end, State, ?RABBITS).

get_rabbit(Name, State) ->
    case do_get_rabbit(Name) of
        {error, _} ->
            State1 = reconnect_rabbits(State),
            {do_get_rabbit(Name), State1};
        Rabbit ->
            {Rabbit, State}
    end.

do_get_rabbit(Name) ->
    case ets:lookup(?RABBITS, Name) of
        [{Name, Rabbit, Conn}] ->
            {Rabbit, Conn};
        [] ->
            {error, {no_rabbit, Name}}
    end.

default_rabbit() ->
    case ets:first(?RABBITS) of
        '$end_of_table' ->
            throw({error, no_available_rabbit});
        Name ->
            Name
    end.

open_channel({Rabbit, Conn}, Key0) ->
    Name = rabbit_name(Rabbit),
    Key = raw_key(Key0),
    ?info("opening channel ~p to ~p", [Key, Name]),
    {ok, Channel} = amqp_connection:open_channel(Conn),
    case ets:insert_new(?CHANNELS, {Key, {Rabbit, Conn}, Channel}) of
        true ->
            ?info("new channel ~p to ~p : ~p", [Key, Name, Channel]),
            ok = maybe_qos(Key0, Channel),
            monitor_channel(Channel),
            {ok, Channel};
        false ->
            usagi_channel:close(Channel),
            maybe_valid_channel(Name, Key0)
    end.

raw_key({Key, _}) -> Key;
raw_key(Key) -> Key.

rabbit_name({Name, _}) ->
    Name.

maybe_qos({_, Prefetch}, Channel) ->
    usagi_channel:qos(Channel, Prefetch);
maybe_qos(_, _) ->
    ok.

maybe_valid_channel(Rabbit, Key) ->
    case ets:lookup(?CHANNELS, raw_key(Key)) of
        [{_, {{Rabbit, _}, _}, Channel}] ->
            case is_process_alive(Channel) of
                true ->
                    {ok, Channel};
                false ->
                    ets:delete(?CHANNELS, raw_key(Key)),
                    false
            end;
        [] -> 
            false
    end.

amqp_param(Props) ->
    Type = proplists:get_value(type, Props, direct),
    amqp_param(Type, Props).

amqp_param(direct, Props) ->
    Node = proplists:get_value(node, Props, undefined),
    ?info("connecting to ~p", [Node]),
    #amqp_params_direct{
        node = Node,
        username = proplists:get_value(username, Props, <<"guest">>),
        virtual_host = proplists:get_value(vhost, Props, <<"/">>)
    };
amqp_param(network, Props) ->
    Host = proplists:get_value(host, Props, "localhost"),
    Port = proplists:get_value(port, Props, 5672),
    ?info("connecting to rabbit ~s:~p", [Host, Port]),
    #amqp_params_network{
        username = proplists:get_value(username, Props, <<"guest">>),
        password = proplists:get_value(password, Props, <<"guest">>),
        host = Host,
        port = Port,
        virtual_host = proplists:get_value(vhost, Props, <<"/">>)
    }.

rabbit_connected({Name, Rabbit}, Conn, State) ->
    ets:insert(?RABBITS, {Name, {Name, Rabbit}, Conn}),
    ?info("connected to ~p : ~p", [Name, Conn]),
    State.
