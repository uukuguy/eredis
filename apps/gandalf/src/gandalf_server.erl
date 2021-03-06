%%%------------------------------------------------------------ 
%%% @author Jiangwen Su <uukuguy@gmail.com>
%%% @copyright (C) 2014, lastz.org
%%% @doc
%%%     主服务程序。
%%%
%%% @end
%%% Created : 2014-02-03 21:11:40
%%%------------------------------------------------------------ 

-module(gandalf_server).
-behaviour(gen_server).
-include("global.hrl").

%% ------------------------------ APIs ------------------------------ 
-export([
        start_link/0,
        start_link/1,
        get_count/0,
        stop/0
    ]).

%% ------------------------------ Callbacks ------------------------------ 
-export([
        init/1, 
        handle_call/3, 
        handle_cast/2, 
        handle_info/2,
        terminate/2, 
        code_change/3
    ]).

%% ------------------------------ record ------------------------------ 
-record(state, 
    {
        port :: integer(),
        lsock,
        request_count = 0 :: integer() 
    }).

-define(SERVER, ?MODULE).
-define(DEFAULT_SERVER_PORT, 18060).

%% ============================== APIs ==============================
%%

start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

start_link() ->
    ServerPort = common_utils:get_env(gandalf, server_port, ?DEFAULT_SERVER_PORT),
    start_link(ServerPort).

stop() ->
    gen_server:cast(?SERVER, stop).

get_count() ->
    gen_server:call(?SERVER, get_count).


%% ============================== Callbacks ==============================
%%

init([Port]) ->
    ?DEBUG("Start to listen at ~p", [Port]),
    {ok, LSock} = gen_tcp:listen(Port, [{active, true}]),
    {ok, #state{port = Port, lsock = LSock}, 0}.

handle_call(get_count, _From, State) ->
    {reply, {ok, State#state.request_count}, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({tcp, Socket, RawData}, State) ->
    ?DEBUG("Receive: ~p", [RawData]),
    gen_tcp:send(Socket, io_lib:fwrite("~p~n", [RawData])),
    %do_rpc(Socket, RawData),
    RequestCount = State#state.request_count,
    {noreply, State#state{request_count = RequestCount + 1}};
handle_info({tcp_closed, _Socket}, State) ->
    {noreply, State};
handle_info(timeout, #state{lsock = LSock} = State) ->
    {ok, _Sock} = gen_tcp:accept(LSock),
    {noreply, State}.

terminate(Reason, _State) ->
    ?NOTICE("gandalf_server terminated!!! Reason: ~p", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ============================== Internal functions ==============================
%%

%do_rpc(Socket, RawData) ->
    %?DEBUG("Enter do_rpc", []),
    %try
        %{M, F, A} = split_out_mfa(RawData),
        %Result = apply(M, F, A),
        %gen_tcp:send(Socket, io_lib:fwrite("~p~n", [Result]))
    %catch
        %_Class:Err ->
            %?ERROR("~p", [Err]),
            %gen_tcp:send(Socket, io_lib:fwrite("~p~n", [Err]))
    %end.

%split_out_mfa(RawData) ->
    %MFA = re:replace(RawData, "\r\n$", "", [{return, list}]),
    %{match, [M, F, A]} = 
    %re:run(MFA,
        %"(.*):(.*)\s*\\((.*)\s*\\)\s*.\s*$",
        %[{capture, [1,2,3], list}, ungreedy]),
    %{list_to_atom(M), list_to_atom(F), args_to_terms(A)}.

%args_to_terms(RawArgs) ->
    %{ok, Tokens, _Line} = erl_scan:string("[" ++ RawArgs ++ "]. ", 1),
    %{ok, Args} = erl_parse:parse_term(Tokens),
    %Args.

