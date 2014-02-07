%%%------------------------------------------------------------ 
%%% @author Jiangwen Su <uukuguy@gmail.com>
%%% @copyright (C) 2014, lastz.org
%%% @doc
%%%     redis protocol parser.
%%%
%%% @end
%%% Created : 2014-02-07 19:13:34
%%%------------------------------------------------------------ 

-module(gandalf_protocol_parser).
-include("global.hrl").

%% ------------------------------ APIs ------------------------------ 
-export([
        init/0,
        start/1,
        execute/2,
        stop/1,

        parse/3
    ]).

%% ------------------------------ record ------------------------------ 
-record(state, 
    {
        redis_args = [],
        socket,
        transport
    }).

-define(TABLE_ID, ?MODULE).

%% ============================== APIs ==============================
%%

%% ------------------------------ init ------------------------------ 
init() ->
    ?DEBUG("gandalf_protocol_parser:init/0", []),
    ets:new(?TABLE_ID, [public, named_table]),
    ok.

%% ------------------------------ start ------------------------------ 
start({ReqId, Socket, Transport}) ->
    {ok, ProtocolServerPid} = gandalf_protocol_server:start({Socket, Transport}),
    ?DEBUG("gandalf_protcol_parser/start1 ReqId: ~p gandalf_protocol_server pid: ~p", [ReqId, ProtocolServerPid]),
    ets:insert(?TABLE_ID, {ReqId, ProtocolServerPid}),
    ok.

%% ------------------------------ execute ------------------------------ 
%% async
execute(ReqId, Arguments) ->
    case ets:lookup(?TABLE_ID, ReqId) of
        [{ReqId, ProtocolServerPid}] -> 
            gandalf_protocol_server:execute(ProtocolServerPid, Arguments)
    end.

%% ------------------------------ stop ------------------------------ 
stop(ReqId) ->
    case ets:lookup(?TABLE_ID, ReqId) of
        [{ReqId, ProtocolServerPid}] -> 
            ets:match_delete(?TABLE_ID, {'_', ProtocolServerPid}),
            gandalf_protocol_server:stop(ProtocolServerPid)
    end.


%% ============================== parse redis ==============================
%%

%% ------------------------------ parse ------------------------------ 
parse(Socket, Transport, Arguments) ->
    ?DEBUG("Start Redis Parser Arguments: ~p", [Arguments]),
    State = #state{socket = Socket, transport = Transport},
    {ok, State1} = do_append_arguments(Arguments, State),
    parse(State1),
    {ok, State1}.

parse(#state{
        redis_args = RedisArgs
    } = State) ->
    ?DEBUG("parse/1 RedisArgs = ~p", [RedisArgs]),
    %A = [ [binary_to_list(I),","] || I <- RedisArgs],
    %B = list_to_binary(A),
    %Transport:send(Socket, <<"+OK ", B/binary >>),
    parse_loop(RedisArgs, State).

%% ------------------------------ do_append_arguments ------------------------------ 
do_append_arguments(Arguments, #state{redis_args = RedisArgs} = State) ->
    ?DEBUG("append_arguments RedisArgs: ~p Arguments: ~p", [RedisArgs, Arguments]),
    NewRedisArgs = lists:append([RedisArgs, Arguments]),
    ?DEBUG("append_arguments NewRedisArgs: ~p", [NewRedisArgs]),
    {ok, State#state{redis_args = NewRedisArgs}}.


%% ------------------------------ parse_loop ------------------------------ 
parse_loop([], _State) ->
    ok;
parse_loop(RedisArgs, State) ->
    {ok, State1} = do_parse(RedisArgs, State),
    parse_loop(State1#state.redis_args, State1).

%% ------------------------------ [] ------------------------------ 
do_parse([], State) ->
    {ok, State};

%% ------------------------------ SET ------------------------------ 
do_parse([<<"SET">>, Key, Value | Rest], #state{
        socket = Socket,
        transport = Transport } = State) ->

    EKey = gandalf:encode_kv_key(Key),
    case gandalf:put_data(EKey, Value) of
        ok ->
            Transport:send(Socket, <<"+OK\r\n">>);
        {error, Reason} ->
            ?ERROR("SET failed. Key: ~p Value: ~p Reason: ~p", [Key, Value, Reason]),
            Transport:send(Socket, <<"-ERR timeout\r\n">>)
    end,
    {ok, State#state{redis_args=Rest}};

%% ------------------------------ GET ------------------------------ 
do_parse([<<"GET">>, Key | Rest], #state{
        socket = Socket,
        transport = Transport} = State) ->

    EKey = gandalf:encode_kv_key(Key),
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
do_parse(RedisArgs, #state{
        socket = Socket,
        transport = Transport} = State) ->

    ?WARNING("Unknown command. Arguments: ~p", [RedisArgs]),
    [H | _T] = RedisArgs,
    Transport:send(Socket, <<"-ERR unknown command '", H/binary,"'\r\n">>),
    {ok, State#state{redis_args = []} }.


