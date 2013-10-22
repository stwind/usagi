-type whynot(R) :: {ok, R} | {error, _}.
-type whynot() :: ok | {error, _}.

-type channel() :: atom() | pid().
-type exchange() :: binary().
-type routing_key() :: binary().
-type rabbit() :: atom().
-type r_queue() :: binary() | {binary(), integer()}.
-type event() :: binary().
