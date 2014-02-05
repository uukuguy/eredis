%%%------------------------------------------------------------ 
%%% @author Jiangwen Su <uukuguy@gmail.com>
%%% @copyright (C) 2013, lastz.org
%%% @doc
%%%     调用接口。
%%%     
%%%     操作由路径Path唯一指向的数据块Data。支持数据块级别的写入、读取、删除。
%%%
%%%     put_data - 写入数据块。
%%%     get_data - 读出数据块。
%%%     delete_data - 删除数据块。
%%%
%%% @end
%%% Created : 2014-01-14 02:13:49
%%%------------------------------------------------------------ 

-module(gandalf).
-include("global.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").
-include_lib("riak_core/include/riak_core_ring.hrl").

-type status() :: non_neg_integer() | binary().
-export_type([status/0]).

-define(TIMEOUT, 5000).

%% ------------------------------ Public APIs ------------------------------
-export([
         put_data/2, %% (Path:string(), Data:string()) -> ok | {error, term()}
         put_data/3, %% (Path:string(), Data:string(), Options:list() | W:integer()) -> ok | {error, term()}
         get_data/1, %% (Path:string()) -> {ok, Data} | {error, term()}
         get_data/2, %% (Path:string(), Options:list() | R:integer()) -> {ok, Data} | {error, term()}
         delete_data/1, %% (Path:string()) -> ok | {error, term()}
         delete_data/2,  %% (Path:string(), Options:list() | DW:integer()) -> ok | {error, term()}
         ping/0
        ]).

%% ------------------------------ Helper APIs ------------------------------
%-export([
         %to_object_key/2, %% (Bucket:binary(), Key:binary()) -> binary()
         %from_object_key/1, %% (LKey:binary()) -> {Bucket:binary(), Key:binary()} | undefined
         %to_index_key/4,
         %from_index_key/1,
         %to_first_key/1,
         %get_chash_args/0,
         %get_storage_preflist/2, %% (Path::string(), N:nag_integer()) -> riak_core_apl:preflist2()
         %get_resource_hashkey/1,
         %get_resource_primary_apl/1,
         %get_resource_primary_apl/2,
         %get_resource_apl/1,
         %get_resource_apl/2,
         %get_resource_vnode/1,
         %preflist_siblings/1,
         %responsible_preflists/1,
         %get_index_n/1,
         %%% 以下废弃
         %path_to_filename/1
        %]).

%% ============================== Public APIs ==============================

%% ------------------------------ ping ------------------------------
%% @doc Pings a random vnode to make sure communication is functional
%%
ping() ->
    command(ping).

%% 随机取得一个虚拟节点执行命令。
command(Cmd) ->
    CmdBin = list_to_binary(atom_to_list(Cmd)),
    DocIdx = riak_core_util:chash_key({CmdBin, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, gandalf_command),
    [{IdxNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IdxNode, Cmd, gandalf_command_vnode_master).


%% ------------------------------ put_data ------------------------------
%% @doc 写入由路径Path唯一指定的数据块Data。
%%      可以指定写参数，通常{w, 2}，表示完成2次成功写入才算成功。

-spec put_data(binary(), binary()) -> ok | {error, atom()}.
put_data(Path, Data) ->
    put_data(Path, Data, []).

-spec put_data(binary(), binary(), list() | integer()) -> ok | {error, term()}.
put_data(Path, Data, Options) when is_binary(Path), is_binary(Data), is_list(Options) ->
    ?NOTICE("call put_data/2", []),
    ?DEBUG("Enter put_data/2 Path = ~p ", [Path]),
    Me = self(),
    Bucket = <<>>,
    Key = Path,
    ReqId = common_utils:random_id(),
    RObj = riak_object:new(Bucket, Key, Data),
    riak_kv_put_fsm:start_link({raw, ReqId, Me}, RObj, Options),
    Timeout = spawn_timeout(Options),
    wait_for_reqid(ReqId, Timeout);
put_data(Path, Data, W) ->
    put_data(Path, Data, [{w, W}]).

%% ------------------------------ get_data ------------------------------
%% @doc 读出由路径Path唯一指定的数据块Data。
%%      可以指定读参数，通常{r, 2}，表示完成2次成功读出才算成功。

-spec get_data(binary()) -> {ok, binary()} | {error, term()}.
get_data(Path) ->
    get_data(Path, []).

get_data(Path, Options) when is_binary(Path), is_list(Options) ->
    ?NOTICE("call get_data/1", []),
    ?DEBUG("Enter get_data/1 Path = ~p", [Path]),
    Me = self(),
    Bucket = <<>>,
    Key = Path,
    ReqId = common_utils:random_id(),
    riak_kv_get_fsm:start_link({raw, ReqId, Me}, Bucket, Key, Options),
    Timeout = spawn_timeout(Options),
    wait_for_reqid(ReqId, Timeout);
get_data(Path, R) ->
    get_data(Path, [{r, R}]).

spawn_timeout(Options) ->
    case proplists:get_value(spawn_timeout, Options) of
        undefined ->
            proplists:get_value(timeout, Options, ?TIMEOUT) + 99;
        Timeout ->
            %% Otherwise use the directly supplied timeout.
            Timeout
    end.

%% ------------------------------ delete_data ------------------------------
%% @doc 删除由路径Path唯一指定的数据块Data。
%%      可以指定删除参数，通常{dw, 2}，表示完成2次成功删除才算成功。

delete_data(Path) ->
    delete_data(Path, []).

delete_data(Path, Options) when is_list(Options) ->
    ?NOTICE("call delete_data/1", []),
    ?DEBUG("Enter delete_data/1 Path = ~p", [Path]),
    Me = self(),
    Bucket = <<>>,
    Key = Path,
    ClientId = undefined,
    Node = node(),
    ReqId = common_utils:random_id(),
    Timeout = spawn_timeout(Options),
    riak_kv_delete_sup:start_delete(Node, [ReqId, Bucket, Key, Options, Timeout,
                                           Me, ClientId]),
    RTimeout = spawn_timeout(Options),
    wait_for_reqid(ReqId, erlang:min(Timeout, RTimeout));
delete_data(Path, DW) ->
    delete_data(Path, [{dw, DW}]).


%% Private
%% 支持异步读写，等待所需成功完成的操作次数或超时返回。
wait_for_reqid(ReqId, Timeout) ->
    receive
        {ReqId, {error, overload}=Response} ->
            case common_utils:get_env(riak_kv, overload_backoff, undefined) of
                Msecs when is_number(Msecs) ->
                    timer:sleep(Msecs);
                undefined ->
                    ok
            end,
            Response;
        {ReqId, Response} -> Response
    after Timeout ->
            {error, timeout}
    end.

