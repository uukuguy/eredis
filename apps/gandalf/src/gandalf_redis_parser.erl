%%%------------------------------------------------------------ 
%%% @author Jiangwen Su <uukuguy@gmail.com>
%%% @copyright (C) 2014, lastz.org
%%% @doc
%%%     Redis解析程序。
%%%
%%% @end
%%% Created : 2014-02-06 00:42:35
%%%------------------------------------------------------------ 

-module(gandalf_redis_parser).
-include("global.hrl").

%% ------------------------------ APIs ------------------------------ 
-export([
        start/3
    ]).

%% ------------------------------ record ------------------------------ 
-record(state, 
    {
        redis_args = [],
        socket,
        transport
    }).

-define(SERVER, ?MODULE).
-define(DEFAULT_SERVER_PORT, 18060).

%% ============================== APIs ==============================
%%

start(Socket, Transport, Arguments) ->
    ?DEBUG("Start Redis Parser Arguments: ~p", [Arguments]),
    State = #state{socket = Socket, transport = Transport},
    {ok, State1} = do_append_arguments(Arguments, State),
    do_execute(State1),
    {ok, State1}.

do_append_arguments(Arguments, #state{redis_args = RedisArgs} = State) ->
    ?DEBUG("append_arguments RedisArgs: ~p Arguments: ~p", [RedisArgs, Arguments]),
    NewRedisArgs = lists:append([RedisArgs, Arguments]),
    ?DEBUG("append_arguments NewRedisArgs: ~p", [NewRedisArgs]),
    {ok, State#state{redis_args = NewRedisArgs}}.

do_execute(#state{
        redis_args = RedisArgs
    } = State) ->
    ?DEBUG("do_execute/1 RedisArgs = ~p", [RedisArgs]),
    %A = [ [binary_to_list(I),","] || I <- RedisArgs],
    %B = list_to_binary(A),
    %Transport:send(Socket, <<"+OK ", B/binary >>),
    execute_loop(RedisArgs, State).

execute_loop([], _State) ->
    ok;
execute_loop(RedisArgs, State) ->
    {ok, State1} = execute(RedisArgs, State),
    execute_loop(State1#state.redis_args, State1).

%% ------------------------------ [] ------------------------------ 
execute([], State) ->
    {ok, State};

%% ------------------------------ SET ------------------------------ 
execute([<<"SET">>, Key, Value | Rest], #state{
        socket = Socket,
        transport = Transport } = State) ->

    EKey = encode_kv_key(Key),
    case gandalf:put_data(EKey, Value) of
        ok ->
            Transport:send(Socket, <<"+OK\r\n">>);
        {error, Reason} ->
            ?ERROR("SET failed. Key: ~p Value: ~p Reason: ~p", [Key, Value, Reason]),
            Transport:send(Socket, <<"-ERR timeout\r\n">>)
    end,
    {ok, State#state{redis_args=Rest}};

%% ------------------------------ GET ------------------------------ 
execute([<<"GET">>, Key | Rest], #state{
        socket = Socket,
        transport = Transport} = State) ->

    EKey = encode_kv_key(Key),
    case gandalf:get_data(EKey) of
        {ok, Data} ->
            L = list_to_binary(common_utils:integer_to_list(size(Data), 10)),
            Transport:send(Socket, <<"$", L/binary, "\r\n", Data/binary, "\r\n">>);
        {error, notfound} ->
            ?ERROR("GET failed. Key: ~p Reason: notfound", [Key]),
            Transport:send(Socket, <<"-ERR notfound\r\n">>);
        {error, timeout} ->
            ?ERROR("GET failed. Key: ~p Reason: timeout", [Key]),
            Transport:send(Socket, <<"-ERR timeout\r\n">>);
        {error, Reason} ->
            ?ERROR("GET failed. Key: ~p Reason: ~p", [Key, Reason]),
            Transport:send(Socket, <<"-ERR server error.\r\n">>)
    end,
    {ok, State#state{redis_args=Rest}};

%% ------------------------------ Unknown Commands ------------------------------ 
execute(RedisArgs, #state{
        socket = Socket,
        transport = Transport} = State) ->

    ?WARNING("Unknown command. Arguments: ~p", [RedisArgs]),
    [H | _T] = RedisArgs,
    Transport:send(Socket, <<"-ERR unknown command '", H/binary,"'\r\n">>),
    {ok, State#state{redis_args = []} }.


encode_kv_key(Key) ->
    sext:encode({keyvalue, Key}).

%decode_kv_key(B) ->
    %sext:decode(B).

