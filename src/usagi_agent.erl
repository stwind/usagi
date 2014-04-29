-module(usagi_agent).

-behaviour(gen_server).

%% callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-export([
         start_link/1,
         add_rabbit/1,
         get_channel/1,
         get_channel/2,
         wait_rabbit/2
        ]).

-include("../include/logging.hrl").

-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(SERVER, ?MODULE).
-define(CHANNELS, rabbit_channels).
-define(RABBITS, rabbits).

-type rabbit_spec() :: usagi_types:rabbit_spec().
-type channel_spec() :: usagi_types:channel_spec().

-record(rabbit, {name :: usagi_types:rabbit(),
                 connection :: pid(),
                 pid :: pid()
                }).

-record(channel, {name :: usagi_types:channel(),
                  rabbit :: usagi_types:rabbit(),
                  pid :: pid() | undefined,
                  spec :: channel_spec()
                 }).

-record(state, {}).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link(Rabbits::[rabbit_spec()]) -> {ok, pid()} | no_return.
start_link(Rabbits) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Rabbits, []).

wait_rabbit(Name, Timeout) ->
    case gen_server:call(?SERVER, {get_rabbit, Name}) of
        {ok, #rabbit{connection=ConnPid}} ->
            usagi_connection:wait_connection(ConnPid, Timeout);
        Error={error, _} ->
            Error
    end.

-spec add_rabbit(RabbitSpec::rabbit_spec()) -> ok.
add_rabbit(Rabbit) ->
    gen_server:call(?SERVER, {add_rabbit, Rabbit}).

-spec get_channel(ChannelSpec::channel_spec()) -> ok.
get_channel(ChannelSpec) ->
    get_channel(default_rabbit(), ChannelSpec).

get_channel(Name, ChannelSpec) ->
    case gen_server:call(?SERVER, {get_rabbit, Name}) of
        {error, _} = Err ->
            Err;
        {ok, Rabbit} ->
            open_channel(Rabbit, ChannelSpec)
    end.

%% ===================================================================
%% gen_server
%% ===================================================================

init(Rabbits) ->
    _ = ets:new(?CHANNELS, [named_table, public, {keypos, 2}, {read_concurrency,true}]),
    _ = ets:new(?RABBITS, [named_table, public, {keypos, 2}, {read_concurrency,true}]),
    {ok, connect_rabbits(Rabbits, #state{})}.

handle_call({add_rabbit, Rabbit}, _From, State) ->
    {reply, ok, connect_rabbits([Rabbit], State)};

handle_call({get_rabbit, Name}, _From, State) ->
    {reply, rabbits_find(Name), State};
handle_call(_Call, _From, State) ->
    {reply, ok, State}.

handle_cast({rabbit_connected, Name, Conn, ConnPid}, State) ->
    {noreply, rabbit_connected(Name, Conn, ConnPid, State)};
handle_cast({rabbit_disconnected, Name, Conn, ConnPid}, State) ->
    {noreply, rabbit_disconnected(Name, Conn, ConnPid, State)};
handle_cast({monitor_channel, Channel}, State) ->
    erlang:monitor(process, Channel),
    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'DOWN', _, _, Pid, Reason}, State) ->
    case channels_by_pid(Pid) of
        [C=#channel{name=N, rabbit=R}] ->
            ?error("channel ~p on ~p terminated: ~p", [N, R, Reason]),
            channels_add(C#channel{pid=undefined});
        [] ->
            ok
    end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Internal functions
%% ===================================================================

connect_rabbits(Rabbits, State) ->
    lists:foldl(fun do_connect/2, State, Rabbits).

do_connect({Name, Spec}, State) ->
    [Primary|Secondary] =
        case lists:all(fun is_list/1, Spec) of
            true ->
                Spec;
            false ->
                [Spec]
        end,
    case ets:member(?RABBITS, Name) of
        false ->
            {ok, Pid} = usagi_connection:start_link(Name, Primary, Secondary, []),
            rabbits_add(#rabbit{name=Name, connection=Pid, pid=undefined});
        true ->
            ok
    end,
    State.

open_channel(#rabbit{name=RabbitName, pid=ConnPid}, ChannelSpec) ->
    ChannelName = raw_key(ChannelSpec),
    Channel =
        case channels_find(ChannelName) of
            {ok, Channel1} ->
                Channel1;
            {error, _} ->
                Channel1 = #channel{name=ChannelName, rabbit=RabbitName, spec=ChannelSpec},
                channels_add(Channel1),
                Channel1
        end,
    maybe_open_channel(ConnPid, Channel).

maybe_open_channel(undefined, _Channel) ->
    {error, not_connected};
maybe_open_channel(ConnPid, C=#channel{name=Name, rabbit=RabbitName, spec=Spec, pid=undefined}) ->
    case amqp_connection:open_channel(ConnPid) of
        {ok, ChannelPid} ->
            channels_add(C#channel{pid=ChannelPid}),
            erlang:monitor(process, ChannelPid),
            ?info("open channel ~p to ~p : ~p", [Name, RabbitName, ChannelPid]),
            ok = maybe_qos(Spec, ChannelPid),
            {ok, ChannelPid};
        Error ->
            ?error("failed to open channel ~p to ~p : ~p", [Name, RabbitName, Error]),
            Error
    end;
maybe_open_channel(ConnPid, C=#channel{pid=Pid}) ->
    case erlang:is_process_alive(Pid) of
        true ->
            {ok, Pid};
        false ->
            maybe_open_channel(ConnPid, C#channel{pid=undefined})
    end.

default_rabbit() ->
    case ets:first(?RABBITS) of
        '$end_of_table' ->
            throw({error, no_available_rabbit});
        Name ->
            Name
    end.

rabbit_connected(Name, Conn, Pid, State) ->
    R = rabbits_get(Name),
    rabbits_add(R#rabbit{connection=Conn, pid=Pid}),
    channels_connect_rabbit(Name, Pid),
    ?info("connected to ~p : ~p ~p", [Name, Conn, Pid]),
    State.

rabbit_disconnected(Name, Conn, Pid, State) ->
    R = rabbits_get(Name),
    ?error("disconnected from ~p : ~p ~p", [Name, Conn, Pid]),
    rabbits_add(R#rabbit{pid=undefined}),
    channels_disconnect_rabbit(Name),
    State.

rabbits_get(Name) when is_atom(Name) ->
    [Ret] = ets:lookup(?RABBITS, Name),
    Ret.

rabbits_find(Name) when is_atom(Name) ->
    case ets:lookup(?RABBITS, Name) of
        [Rec] ->
            {ok, Rec};
        [] ->
            {error, {no_rabbit, Name}}
    end.

rabbits_add(R=#rabbit{}) -> ets:insert(?RABBITS, R).

%% channels
channels_disconnect_rabbit(RabbitName) ->
    [channel_disconnect(C) || C <- channels_by_rabbit(RabbitName)].

channels_connect_rabbit(RabbitName, ConnPid) ->
    [maybe_open_channel(ConnPid, C) || C <- channels_by_rabbit(RabbitName)].

channel_disconnect(#channel{pid=undefined}) ->
    ok;
channel_disconnect(C=#channel{pid=Pid}) ->
    case erlang:is_process_alive(Pid) of
        true ->
            catch amqp_channel:close(Pid);
        false ->
            ok
    end,
    channels_add(C#channel{pid=undefined}).

channels_by_rabbit(RabbitName) ->
    ets:match_object(?CHANNELS, #channel{_ = '_', rabbit=RabbitName}).

channels_by_pid(Pid) ->
    ets:match_object(?CHANNELS, #channel{_ = '_', pid=Pid}).

channels_add(C=#channel{}) ->
    ets:insert(?CHANNELS, C).

channels_find(Name) when is_atom(Name) ->
    case ets:lookup(?CHANNELS, Name) of
        [Rec] ->
            {ok, Rec};
        [] ->
            {error, {no_channel, Name}}
    end.

raw_key({Key, _}) -> Key;
raw_key(Key) -> Key.

maybe_qos({_, Prefetch}, Channel) ->
    usagi_channel:qos(Channel, Prefetch);
maybe_qos(_, _) ->
    ok.
