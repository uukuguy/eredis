%%%------------------------------------------------------------ 
%%% @author Jiangwen Su <uukuguy@gmail.com>
%%% @copyright (C) 2014, lastz.org
%%% @doc
%%%     
%%%
%%% @end
%%% Created : 2014-02-03 21:10:43
%%%------------------------------------------------------------ 

-module(gandalf_node_event_handler).
-include("global.hrl").
-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).
-record(state, {}).

init([]) ->
    {ok, #state{}}.

handle_event({service_update, Services}, State) ->
    ?DEBUG("service_update! Services = ~p State = ~p", [Services, State]),
    {ok, State}.

handle_call(_Event, State) ->
    {ok, ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

