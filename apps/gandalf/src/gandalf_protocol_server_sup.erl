%%%------------------------------------------------------------ 
%%% @author Jiangwen Su <uukuguy@gmail.com>
%%% @copyright (C) 2013, lastz.org
%%% @doc
%%%
%%% @end
%%% Created : 2014-02-07 17:39:55
%%%------------------------------------------------------------ 

-module(gandalf_protocol_server_sup).
-behaviour(supervisor).
-include("global.hrl").

%% ------------------------------ APIs ------------------------------ 
-export([
        start_link/0,
        start_child/1
    ]).

%% ------------------------------ Callbacks ------------------------------ 
-export([init/1]).

%% ============================== APIs ==============================
%%

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child({Socket, Transport}) ->
    supervisor:start_child(?SERVER, [{Socket, Transport}]).

%% ============================== Callbacks ==============================
%%

init(_Args) ->

    ProtocolServer = { 
        %% -------- ID --------
        gandalf_protocol_server,                   
        %% -------- Start -------- 
        %%   {Module, Function, Arguments}
        {gandalf_protocol_server, start_link, []}, 
        %% -------- Restart -------- 
        %% 子进程故障时是否重启
        %%   permanent : 始终重启
        %%   temporary : 永不重启
        %%   transient : 仅在进程意外终止时重启
        temporary,                      
        %% -------- Shutdown -------- 
        %% 如何终止进程
        %%   整数 : 留给进程自我了断的时间（毫秒）
        %%   brutal_kill : 立即中止子进程
        %%   infinity : 子进程也是监督者进程，
        %%              保证有充分时间退出
        brutal_kill,                           
        %% -------- Type -------- 
        %% supervisor or worker 
        worker,                         
        %% -------- Dependences -------- 
        %% 依赖模块, 用于代码热升级。
        [gandalf_protocol_server]                  
    },

    Children = [ProtocolServer],
    RestartStrategy = {simple_one_for_one, 0, 1},

    { ok, {RestartStrategy , Children}}.

