%%%------------------------------------------------------------ 
%%% @author Jiangwen Su <uukuguy@gmail.com>
%%% @copyright (C) 2013, lastz.org
%%% @doc
%%%
%%% @end
%%% Created : 2014-02-05 00:41:55
%%%------------------------------------------------------------ 
-module(gandalf_server_protocol).
-behaviour(ranch_protocol).
-include("global.hrl").

%% ------------------------------ APIs ------------------------------ 
-export([start_link/4]).

%% ------------------------------ Callbacks ------------------------------ 
-export([init/4]).

%% ------------------------------ record ------------------------------ 
-type env() :: [{atom(), any()}].

-type opts() :: [
    {compress, boolean()}
	| {env, env()}
	| {max_empty_lines, non_neg_integer()}
	| {max_request_line_length, non_neg_integer()}
	| {timeout, timeout()}
].
-export_type([opts/0]).

-type redis_cmd() :: {
    binary(),
    [term()]
}.
-export_type([redis_cmd/0]).

-record(state, {
	socket :: inet:socket(),
	transport :: module(),
	compress :: boolean(),
    env :: env(),
	max_empty_lines :: non_neg_integer(),
	max_request_line_length :: non_neg_integer(),
	timeout :: timeout(),
	until :: non_neg_integer() | infinity
}).

%% ============================== APIs ==============================
%%
start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

%% ============================== Callbacks ==============================
%%
-spec init(ranch:ref(), inet:socket(), module(), opts()) -> ok.
init(Ref, Socket, Transport, Opts) ->

	Compress = common_utils:get_config_value(compress, Opts, false),
	MaxEmptyLines = common_utils:get_config_value(max_empty_lines, Opts, 5),
	MaxRequestLineLength = common_utils:get_config_value(max_request_line_length, Opts, 4096),
	Env = [{listener, Ref}|common_utils:get_config_value(env, Opts, [])],
	Timeout = common_utils:get_config_value(timeout, Opts, 5000), %%infinity),

    ok = ranch:accept_ack(Ref),
    loop(<<>>, #state{
            socket=Socket, 
            transport=Transport,
            compress=Compress, 
            env=Env,
            max_empty_lines=MaxEmptyLines, 
            max_request_line_length=MaxRequestLineLength,
            timeout=Timeout, 
            until=until(Timeout)
        }).
        
%% ------------------------------ loop ------------------------------ 
loop(Buffer, State) ->
    {RestBuffer} = parse_command(Buffer, State),
    loop(RestBuffer, State).

%% ============================== Internal Functions ==============================
%%

%% ------------------------------ do_command/2 ------------------------------ 
%% @private
do_command([], _State) ->
    ok;
do_command([Command | Arguments], _State) ->
    ?DEBUG("do_command/2 Command: ~p Arguments: ~p", [Command, Arguments]),
    %_RedisCmd = {Command, Arguments},
    ok.

%% ------------------------------ parse_command ------------------------------ 
%% @private
parse_command(Buffer, State) ->
    ParseNumOfArgumentsFunc = parse_num_of_arguments_fun(),
    case parse_line(Buffer, State, 0, ParseNumOfArgumentsFunc) of
        {error, Reason, RestBuffer} ->
            ?WARNING("parse_num_of_arguments/2 failed. Reason: ~p", [Reason]),
            {RestBuffer};
        {Args, RestBuffer} ->
            ?DEBUG("num of arguments: ~p", [Args]),
            parse_command(Args, RestBuffer, State, [])
    end.

parse_command(0, Buffer, State, Arguments) ->
    ?DEBUG("Arguments: ~p Buffer: ~p", [Arguments, Buffer]),
    do_command(Arguments, State),
    {Buffer};
parse_command(Args, Buffer, State, Arguments) ->
    ?DEBUG("parse_command/4 Args: ~p Buffer: ~p Arguments: ~p", [Args, Buffer, Arguments]),

    ParseArgumentBytesFunc = parse_argument_bytes_fun(),
    case parse_line(Buffer, State, 0, ParseArgumentBytesFunc) of
        {error, Reason, RestBuffer} ->
            ?WARNING("parse_argument_bytes/2 failed. Reason: ~p", [Reason]),
            {RestBuffer}; 
        {_ArgBytes, RestBuffer1} ->
            ?DEBUG("parse_command ArgBytes: ~p", [_ArgBytes]),

            ParseArgumentFunc = parse_argument_fun(),
            {Arg, RestBuffer2} = parse_line(RestBuffer1, State, 0, ParseArgumentFunc),
            ?DEBUG("parse_command Arg: ~p", [Arg]),

            parse_command(Args - 1, RestBuffer2, State, lists:append([Arguments, [Arg]]))
    end.


%% ------------------------------ parse_num_of_arguments_fun/0 ------------------------------ 
%% @private
parse_num_of_arguments_fun() ->
    fun(Buffer, #state{socket=Socket, transport=Transport} = _State) ->
            %?DEBUG("parse_num_of_arguments_fun Buffer: ~p",  [Buffer]),
            case Buffer of
                <<"*", Rest/binary>> ->
                    {N, Rest1} = split_by_lf(Rest),
                    %?DEBUG("parse_method return ~p Rest1: ~p", [N, Rest1]),
                    Args = list_to_integer(binary_to_list(N)),
                    %?DEBUG("num of arguments: ~p", [Args]),
                    {Args, Rest1};
                _ ->
                    {RestBuffer} = skip_line(Buffer),
                    ?DEBUG("parse_num_of_arguments not match. Buffer: ~p RestBuffer: ~p", [Buffer, RestBuffer]),
                    {C, _} = split_by_lf(Buffer),
                    Transport:send(Socket, <<"-ERR unknown command '", C/binary,"'\r\n">>),
                    {error, unknown_command, RestBuffer}
            end
    end.

%% ------------------------------ parse_argument_bytes_fun/0 ------------------------------ 
%% @private
parse_argument_bytes_fun() ->
    fun(Buffer, #state{socket=Socket, transport=Transport} = _State) ->
            %?DEBUG("parse_argument_bytes_fun Buffer: ~p",  [Buffer]),
            case Buffer of
                <<"$", Rest/binary>> ->
                    {N, Rest1} = split_by_lf(Rest),
                    %?DEBUG("parse_argument_bytes return ~p Rest1: ~p", [N, Rest1]),
                    ArgBytes = list_to_integer(binary_to_list(N)),
                    %?DEBUG("argument bytes: ~p", [ArgBytes]),
                    {ArgBytes, Rest1};
                _ ->
                    {RestBuffer} = skip_line(Buffer),
                    ?DEBUG("parse_argument_bytes not match. Buffer: ~p RestBuffer", [Buffer, RestBuffer]),
                    {C, _} = split_by_lf(Buffer),
                    Transport:send(Socket, <<"-ERR unknown command '", C/binary,"'\r\n">>),
                    {error, unknown_command, RestBuffer}
            end
    end.

%% ------------------------------ parse_argument_fun/0 ------------------------------ 
parse_argument_fun() ->
    fun(Buffer, _State) ->
            %?DEBUG("parse_argument__fun Buffer: ~p",  [Buffer]),
            case Buffer of
                _ ->
                    {S, Rest1} = split_by_lf(Buffer),
                    ?DEBUG("parse_argument return ~p Rest1: ~p", [S, Rest1]),
                    {S, Rest1}
            end
    end.

%% ------------------------------ parse_line/4 ------------------------------ 
%% @private
%-spec parse_line(binary(), #state{}, non_neg_integer()) -> ok.
parse_line(Buffer, State=#state{max_request_line_length=MaxLength,
        max_empty_lines=MaxEmpty}, ReqEmpty, ParseFunc) ->
    case Buffer of
        <<>> ->
            wait_line(<<>>, State, ReqEmpty, ParseFunc);
        << $\n, _/binary >> ->
            error_terminate(400, State);
        _ ->
            ?DEBUG("parse_line Buffer: ~p", [Buffer]),
            case match_eol(Buffer, 0) of
                nomatch when byte_size(Buffer) > MaxLength ->
                    error_terminate(414, State);
                nomatch ->
                    wait_line(Buffer, State, ReqEmpty, ParseFunc);
                1 when ReqEmpty =:= MaxEmpty ->
                    error_terminate(400, State);
                1 ->
                    << _:16, Rest/binary >> = Buffer,
                    parse_line(Rest, State, ReqEmpty + 1, ParseFunc);
                _ ->
                    ParseFunc(Buffer, State)
            end
    end.

%% ------------------------------ wait_line/4 ------------------------------ 
%% @private
%-spec wait_line(binary(), #state{}, non_neg_integer()) -> ok.
wait_line(Buffer, State=#state{socket=Socket, transport=Transport,
		until=Until}, ReqEmpty, ParseFunc) ->
	case recv(Socket, Transport, Until) of
		{ok, Data} ->
			parse_line(<< Buffer/binary, Data/binary >>, State, ReqEmpty, ParseFunc);
		{error, _} ->
			terminate(State)
	end.

%% ------------------------------ recv/3 ------------------------------ 
%% @private
-spec recv(inet:socket(), module(), non_neg_integer() | infinity)
	-> {ok, binary()} | {error, closed | timeout | atom()}.
recv(Socket, Transport, infinity) ->
	Transport:recv(Socket, 0, infinity);
recv(Socket, Transport, Until) ->
	{Me, S, Mi} = os:timestamp(),
	Now = Me * 1000000000 + S * 1000 + Mi div 1000,
	Timeout = Until - Now,
	if	Timeout < 0 ->
			{error, timeout};
		true ->
			Transport:recv(Socket, 0, Timeout)
	end.

%% ------------------------------ until/1 ------------------------------ 
%% @private
-spec until(timeout()) -> non_neg_integer() | infinity.
until(infinity) ->
	infinity;
until(Timeout) ->
	{Me, S, Mi} = os:timestamp(),
	Me * 1000000000 + S * 1000 + Mi div 1000 + Timeout.


%% ------------------------------ match_eol/2 ------------------------------ 
%% @private
match_eol(<< $\n, _/bits >>, N) ->
    %?DEBUG("match_eol $\\n: ~p", [N]),
    N;
match_eol(<< _Buffer, Rest/bits >>, N) ->
    %?DEBUG("match_eol N: ~p, Buffer: ~p", [N, Buffer]),
    match_eol(Rest, N + 1);
match_eol(_Buffer, _) ->
    %?DEBUG("nomatch Buffer: ~p", [Buffer]),
    nomatch.

%% ------------------------------ split_by_lf ------------------------------ 
%% @private
split_by_lf(Buffer) ->
    split_by_lf(Buffer, <<>>).

split_by_lf(<< C, Rest/bits >>, SoFar) ->
    %?DEBUG("split_by_lf C: ~p Rest: ~p SoFar: ~p", [C, Rest, SoFar]),
	case C of
		$\r -> 
            <<_, Rest1/binary>> = Rest,
            {SoFar, Rest1};
		_ -> split_by_lf(Rest, << SoFar/binary, C >>)
	end.

%% ------------------------------ skip_line/1 ------------------------------ 
%% @private
skip_line(<< C, Rest/bits >>) ->
	case C of
		$\r -> 
            <<_, Rest1/binary>> = Rest,
            {Rest1};
		_ -> skip_line(Rest)
	end.


%% ------------------------------ error_teminate/2 ------------------------------ 
%% @private
-spec error_terminate(gandalf:status(), #state{}) -> ok.
error_terminate(_Status, 
        State=#state{
            socket=_Socket, 
            transport=_Transport,
            compress=_Compress
        }) ->
    terminate(State).

%% ------------------------------ teminate/1 ------------------------------ 
%% @private
-spec terminate(#state{}) -> ok.
terminate(#state{socket=Socket, transport=Transport}) ->
	Transport:close(Socket),
	ok.

