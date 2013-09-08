-define(info(Fmt), error_logger:info_msg(Fmt ++ "~n")).
-define(info(Fmt, Args), error_logger:info_msg(Fmt ++ "~n", Args)).

-define(error(Fmt), error_logger:error_msg(Fmt ++ "~n")).
-define(error(Fmt, Args), error_logger:error_msg(Fmt ++ "~n", Args)).
