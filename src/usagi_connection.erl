%%% @author Vladimir G. Sekissov <eryx67@gmail.com>
%%% @copyright (C) 2014, Vladimir G. Sekissov
%%% @doc
%%%
%%% @end
%%% Created : 28 Apr 2014 by Vladimir G. Sekissov <eryx67@gmail.com>

-module(usagi_connection).

-behavior(gen_fallback_client).

-export([start_link/4, wait_connection/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([handle_connected/3, handle_disconnected/1]).

%% internal exports
-export([amqp_connect/2]).

-include("../include/logging.hrl").

-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("eunit/include/eunit.hrl").

-type option() :: gen_fallback_client:option().
-type rabbit_opt() :: usagi_types:rabbit_opt().

-define(SRV, usagi_agent).

-record(state, {name :: atom(),
                connection :: pid(),
                channels = []
               }).
%% @doc
%% @see //emb/gen_fallback_client:start_link/5
%% @end
-spec start_link(Name::atom(),
                 PrimarySrv::[rabbit_opt()],
                 SecondarySrvs::[rabbit_opt()],
                 Opts::[option()]) -> {ok, pid()} | no_return().

start_link(Name, PrimarySrv, SecondarySrvs, Opts) ->
    gen_fallback_client:start_link(?MODULE, {Name},
                                   {?MODULE, amqp_connect, [amqp_param(PrimarySrv)]},
                                   [{?MODULE, amqp_connect, [amqp_param(SS)]}
                                    || SS <- SecondarySrvs],
                                   Opts).

wait_connection(Pid, Timeout) ->
    gen_fallback_client:wait_connection(Pid, Timeout).

init({Name}) -> {ok, #state{name=Name}}.

handle_call(Msg, _From, State) ->
    ?debug("unwanted message ~w", [Msg]),
    {reply, ok, State}.

handle_cast({connection, Conn}, State) ->
    {noreply, State#state{connection=Conn}};
handle_cast(Msg, State) ->
    ?debug("unwanted message ~w", [Msg]),
    {noreply, State}.

handle_info(Msg, State) ->
    ?debug("unwanted message ~w", [Msg]),
    {noreply, State}.

handle_connected(_Pid, ?MODULE, S=#state{name=Name, connection=Conn}) ->
    gen_server:cast(?SRV, {rabbit_connected, Name, self(), Conn}),
    {ok, S}.

handle_disconnected(S=#state{name=Name, connection=Conn}) ->
    gen_server:cast(?SRV, {rabbit_disconnected, Name, self(), Conn}),
    {ok, S}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

amqp_connect(Parent, AmqpParam) ->
    StartF =
        fun () ->
                case amqp_connection:start(AmqpParam) of
                    {ok, Conn} ->
                        _ = monitor_rabbit(Conn, AmqpParam),
                        gen_server:cast(Parent, {connection, Conn}),
                        gen_fallback_client:connection_connected(Parent),
                        amqp_connect_loop(Conn);
                    {error, Reason} ->
                        exit(Reason)
                end
        end,
    {ok, erlang:spawn_link(StartF)}.

amqp_connect_loop(Conn) ->
    receive
        {'DOWN', _, _, Conn, Reason} ->
            exit(Reason);
        Err={nodedown, _} ->
            amqp_connection:close(Conn),
            exit(Err);
        {connection, From} ->
            From ! {connection, Conn},
            amqp_connect_loop(Conn)
    end.

monitor_rabbit(Conn, #amqp_params_direct{node=Node}) ->
    erlang:monitor(process, Conn),
    erlang:monitor_node(Node, true);
monitor_rabbit(Conn, _) ->
    erlang:monitor(process, Conn).

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
       virtual_host = proplists:get_value(vhost, Props, <<"/">>),
       ssl_options = proplists:get_value(ssl_options, Props, none)
      }.
