-module(usagi_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%-include_lib("yunio_logger/include/yunio_logger.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    case usagi_util:get_rabbits() of
        [] ->
            {error, no_rabbit};
        Rabbits ->
            RabbitAgent = {
              usagi_agent, {usagi_agent,start_link,[Rabbits]},
              permanent, 5000, worker, [usagi_agent]
             },
            {ok, {{one_for_one, 5, 10}, [RabbitAgent]}}
    end.
