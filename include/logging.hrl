-define(info(Fmt), error_logger:info_msg(Fmt)).
-define(info(Fmt, Args), error_logger:info_msg(Fmt, Args)).

-define(error(Fmt), error_logger:error_msg(Fmt)).
-define(error(Fmt, Args), error_logger:error_msg(Fmt, Args)).
