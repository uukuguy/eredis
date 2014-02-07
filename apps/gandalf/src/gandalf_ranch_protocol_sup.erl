%%%------------------------------------------------------------ 
%%% @author Jiangwen Su <uukuguy@gmail.com>
%%% @copyright (C) 2013, lastz.org
%%% @doc
%%%
%%% @end
%%% Created : 2014-02-05 00:45:29
%%%------------------------------------------------------------ 
-module(gandalf_ranch_protocol_sup).
-behaviour(supervisor).
-include("global.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(DEFAULT_SERVER_PORT, 18060).
-define(DEFAULT_SERVER_LISTENERS, 100).

%%%------------------------------------------------------------ 
%%% API functions
%%%------------------------------------------------------------ 

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%------------------------------------------------------------ 
%%% Supervisor callacks
%%%------------------------------------------------------------ 

init([]) ->

    ServerPort = common_utils:get_env(gandalf, server_port, ?DEFAULT_SERVER_PORT),
    Listeners = common_utils:get_env(gandalf, server_listeners, ?DEFAULT_SERVER_LISTENERS),

    Children = [
        ranch:child_spec( 
            gandalf_ranch_protocol, Listeners, ranch_tcp,
            [
                {port, ServerPort}
            ],
            gandalf_ranch_protocol,
            [
            ])
    ],

    %% 重启策略
    %% 生产系统通常每小时(3600秒)4次，调试阶段设为不重启0/1。
    RestartStrategy = {
        one_for_one, %% How 仅重启退出的子进程 
        0,           %% Max 在指定时间段内最大重启次数
        1            %% Within 指定时间段。
    },

    { ok, {RestartStrategy , Children}}.

