%%% @author Vladimir G. Sekissov <eryx67@gmail.com>
%%% @copyright (C) 2014, Vladimir G. Sekissov
%%% @doc Common types
%%%
%%% @end
%%% Created : 28 Apr 2014 by Vladimir G. Sekissov <eryx67@gmail.com>

-module(usagi_types).

-export_type([channel/0, exchange/0, routing_key/0, rabbit/0, r_queue/0, event/0, rabbit_spec/0]).
-export_type([channel_spec/0, whynot/0, whynot/1]).

-type channel() :: atom() | pid().
-type exchange() :: binary().
-type routing_key() :: binary().
-type rabbit() :: atom().
-type r_queue() :: binary() | {binary(), integer()}.
-type event() :: binary().
-type qos_prefetch() :: integer().

-type rabbit_opt() :: {type, direct | network}
                      | {host, inet:hostname() | inet:ip_address()}
                      | {port, integer()}
                      | {username, string()}
                      | {password, string()}
                      | {virtual_host, string()}
                      | {ssl_options, [ssl:ssloption()]}.

-type rabbit_spec() :: {rabbit(), [[rabbit_opt()]] | [rabbit_opt()]}.

-type channel_spec() :: {atom(), qos_prefetch()} | atom().

-type whynot(R) :: {ok, R} | {error, _}.
-type whynot() :: ok | {error, _}.
