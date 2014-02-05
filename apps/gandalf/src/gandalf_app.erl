%%%------------------------------------------------------------ 
%%% @author Jiangwen Su <uukuguy@gmail.com>
%%% @copyright (C) 2014, lastz.org
%%% @doc
%%%     应用程序启动入口。
%%%
%%% @end
%%% Created : 2014-01-31 23:40:27
%%%------------------------------------------------------------ 

-module(gandalf_app).
-behaviour(application).
-include("global.hrl").

%% ------------------------------ APIs ------------------------------ 
-export([start/2, stop/1]).

%% ============================== APIs ==============================
%%

start(StartType, StartArgs) ->

    ok = common_utils:start_app_deps(gandalf),

    %% Append user-provided code paths
    %% in gandalf.app.src.
    case common_utils:get_env(gandalf, add_paths) of
        List when is_list(List) ->
            ok = code:add_paths(List);
        _ ->
            ok
    end,

    %% -------------------- lager --------------------
    %application:start(lager),

    % 将核心模块相关的日志记录到log/gandalf.log中，方便调试。
    lager:trace_file("log/gandalf_debug.log", [{module, gandalf}], debug),
    lager:trace_file("log/gandalf_debug.log", [{module, gandalf_app}], debug),
    lager:trace_file("log/gandalf_debug.log", [{module, gandalf_sup}], debug),
    lager:trace_file("log/gandalf_debug.log", [{module, gandalf_command_vnode}], debug),
    lager:trace_file("log/gandalf_debug.log", [{module, gandalf_cowboy_handler}], debug),
    lager:trace_file("log/gandalf_debug.log", [{module, gandalf_server}], debug),
    lager:trace_file("log/gandalf_debug.log", [{module, gandalf_server_protocol}], debug),
    lager:trace_file("log/gandalf_debug.log", [{module, gandalf_server_sup}], debug),
    lager:trace_file("log/gandalf_debug.log", [{module, gandalf_redis_parser}], debug),

    lager:trace_file("log/gandalf_debug.log", [{module, gandalf_redis_server}], debug),
    lager:trace_file("log/gandalf_debug.log", [{module, gandalf_redis_server_sup}], debug),

    %% -------------------- gandalf_cowboy --------------------
    gandalf_cowboy_app:start(StartType, StartArgs),

    %% -------------------- gandalf_sup --------------------
    case gandalf_sup:start_link() of
        {ok, Pid} ->
            ?NOTICE("=== gandalf Start === Pid : ~p", [Pid]),
            ok = riak_core:register(gandalf, [{vnode_module, gandalf_command_vnode}]),

            ok = riak_core_ring_events:add_guarded_handler(gandalf_ring_event_handler, []),
            ok = riak_core_node_watcher_events:add_guarded_handler(gandalf_node_event_handler, []),
            ok = riak_core_node_watcher:service_up(gandalf_command, self()),

            Result = {ok, Pid};
        {error, Reason} ->
            ?ERROR("gandalf_sup:start_link() failure!!! Reason: ~p", [Reason]),
            Result = {error, Reason}
    end,


    Result.

stop(_State) ->
    ?NOTICE("=== gandalf Stop ===", []),
    ok.



