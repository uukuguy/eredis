%%%------------------------------------------------------------ 
%%% @author Jiangwen Su <uukuguy@gmail.com>
%%% @copyright (C) 2014, lastz.org
%%% @doc
%%%     redis服务程序。
%%%
%%% @end
%%% Created : 2014-02-06 09:42:07
%%%------------------------------------------------------------ 

-module(gandalf_protocol_server).
-behaviour(gen_server).
-include("global.hrl").

%% ------------------------------ APIs ------------------------------ 
-export([
        start_link/1,
        stop/1,
        start/1,
        execute/2
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
        socket,
        transport,
        arguments = []
    }).

-define(TABLE_ID, ?MODULE).

%% ============================== APIs ==============================
%%

start_link({Socket, Transport}) ->
    gen_server:start_link(?MODULE, [{Socket, Transport}], []).

start({Socket, Transport})->
    gandalf_protocol_server_sup:start_child({Socket, Transport}).

stop(Pid) ->
    gen_server:cast(Pid, stop).

execute(Pid, Arguments) ->
    gen_server:cast(Pid, {execute, Arguments}).

%% ============================== Callbacks ==============================
%%

init([{Socket, Transport}]) ->
    ?DEBUG("Start gandalf protocol server.", []),
    
    {ok, #state{socket = Socket, transport = Transport}, 0}.

handle_call(_, _From, State) ->
    {noreply, State}.

handle_cast({execute, Arguments}, #state{
        socket = Socket,
        transport = Transport} = State) ->

    ?DEBUG("handle_cast/2 execute Arguments: ~p", [Arguments]),
    gandalf_protocol_parser:parse(Socket, Transport, Arguments),
    {noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(timeout, #state{} = State) ->
    {noreply, State}.

terminate(Reason, _State) ->
    ?NOTICE("redis_server terminated!!! Reason: ~p", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


