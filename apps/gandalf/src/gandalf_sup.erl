%%%------------------------------------------------------------ 
%%% @author Jiangwen Su <uukuguy@gmail.com>
%%% @copyright (C) 2013, lastz.org
%%% @doc
%%%
%%% @end
%%% Created : 2014-01-31 23:39:53
%%%------------------------------------------------------------ 

-module(gandalf_sup).
-behaviour(supervisor).
-include("global.hrl").

%% ------------------------------ APIs ------------------------------ 
-export([start_link/0]).

%% ------------------------------ Callbacks ------------------------------ 
-export([init/1]).

%% ============================== APIs ==============================
%%

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% ============================== Callbacks ==============================
%%

init(_Args) ->

    _GandalfServer = { 
        %% -------- ID --------
        gandalf_server,                   
        %% -------- Start -------- 
        %%   {Module, Function, Arguments}
        {gandalf_server, start_link, []}, 
        %% -------- Restart -------- 
        %% 子进程故障时是否重启
        %%   permanent : 始终重启
        %%   temporary : 永不重启
        %%   transient : 仅在进程意外终止时重启
        permanent,                      
        %% -------- Shutdown -------- 
        %% 如何终止进程
        %%   整数 : 留给进程自我了断的时间（毫秒）
        %%   brutal_kill : 立即中止子进程
        %%   infinity : 子进程也是监督者进程，
        %%              保证有充分时间退出
        5000,                           
        %% -------- Type -------- 
        %% supervisor or worker 
        worker,                         
        %% -------- Dependences -------- 
        %% 依赖模块, 用于代码热升级。
        [gandalf_server]                  
    },

    CmdVNodeMaster = {
        gandalf_command_vnode_master,
        {riak_core_vnode_master, start_link, [gandalf_command_vnode]},
        permanent, 5000, worker, [riak_core_vnode_master]},

    % Figure out which processes we should run...
    %HasStorageBackend = (common_utils:get_env(gandalf, storage_backend) /= undefined),

    Children = lists:flatten([
                %?IF(HasStorageBackend, Storage, []),
                %GandalfServer
                CmdVNodeMaster
               ]),

    %% 重启策略
    %% 生产系统通常每小时(3600秒)4次，调试阶段设为不重启0/1。
    RestartStrategy = {
        one_for_one, %% How 仅重启退出的子进程 
        0,           %% Max 在指定时间段内最大重启次数
        1            %% Within 指定时间段。
    },

    { ok, {RestartStrategy , Children}}.

