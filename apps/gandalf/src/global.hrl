%%%------------------------------------------------------------ 
%%% @author Jiangwen Su <uukuguy@gmail.com>
%%% @copyright (C) 2013, lastz.org
%%% @doc
%%%
%%% @end
%%% Created : 2014-01-31 23:40:12
%%%------------------------------------------------------------ 

-define (APPNAME, "gandalf").
-define (IF (Bool, A, B), if Bool -> A; true -> B end).

-define(MSGHEAD(Color), " \e[0;32m<<-- " ++ ?APPNAME ++ " log -->> " ++ Color).
-define(MSGTAIL(), "\e[0;38m").
-define(DEBUG(Msg, Args), lager:debug([{module, ?MODULE}], ?MSGHEAD("\e[0;38m") ++ Msg ++ ?MSGTAIL(), Args)).
-define(INFO(Msg, Args), lager:info([{module, ?MODULE}],   ?MSGHEAD("\e[1;37m") ++ Msg ++ ?MSGTAIL(), Args)).
-define(NOTICE(Msg, Args), lager:notice([{module, ?MODULE}], ?MSGHEAD("\e[1;36m") ++ "-------------------- " ++ Msg ++ " --------------------" ++ ?MSGTAIL(), Args)).
-define(WARNING(Msg, Args), lager:warning([{module, ?MODULE}], ?MSGHEAD("\e[1;33m") ++ Msg ++ ?MSGTAIL(), Args)).
-define(ERROR(Msg, Args), lager:error([{module, ?MODULE}], ?MSGHEAD("\e[1;31m") ++ Msg ++ ?MSGTAIL(), Args)).
-define(CRITICAL(Msg, Args), lager:critical([{module, ?MODULE}], ?MSGHEAD("\e[1;35m") ++ Msg ++ ?MSGTAIL(), Args)).

