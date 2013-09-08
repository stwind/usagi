-module(usagi_util).

%% exports
-export([
        get_env/1,
        get_env/2,
        get_rabbits/0
    ]).

get_env(Key) ->
    get_env(Key, undefined).

get_env(Key, Default) ->
    case application:get_env(usagi, Key) of
        {ok, Value} -> Value;
        undefined -> Default
    end.

get_rabbits() ->
    get_env(rabbits, []).
