%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_client).
-typing([eqwalizer]).

-export([voice_state_update/4]).
-export([do_call/3]).
-export([request_trace/1]).

-export_type([
    voice_state_update_success/0,
    voice_state_update_error/0,
    voice_state_update_result/0
]).

-type voice_state_update_success() :: #{
    success := true,
    token => binary(),
    endpoint => binary(),
    connection_id => binary(),
    voice_state => map(),
    needs_token => boolean()
}.

-type voice_state_update_rejection() :: #{
    success := false,
    ack := map()
}.

-type voice_state_update_error() :: {error, atom(), atom()}.

-type voice_state_update_result() ::
    {ok, voice_state_update_success()}
    | {ok, voice_state_update_rejection()}
    | {error, timeout}
    | {error, noproc}
    | {error, circuit_breaker_open}
    | {error, too_many_requests}
    | {error, atom(), atom()}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec execute_with_slot(pid(), map(), timeout()) -> voice_state_update_result().
execute_with_slot(TargetPid, Request, Timeout) ->
    try
        circuit_breaker_execute(TargetPid, Request, Timeout)
    after
        guild_client_circuit_breaker:release_slot(TargetPid)
    end.

-spec voice_state_update(pid(), integer(), map(), timeout()) -> voice_state_update_result().
voice_state_update(GuildPid, GuildId, Request, Timeout) ->
    guild_client_circuit_breaker:ensure_table(),
    maybe
        {ok, TargetPid} ?= resolve_voice_pid(GuildId, GuildPid),
        ok ?= guild_client_circuit_breaker:acquire_slot(TargetPid),
        execute_with_slot(TargetPid, Request, Timeout)
    else
        {error, not_found} ->
            logger:warning(
                "guild_client_no_voice_server:"
                " guild_id=~p guild_pid=~p request=~p",
                [GuildId, GuildPid, request_trace(Request)]
            ),
            {error, noproc};
        {error, Reason} ->
            {error, Reason}
    end.

-spec resolve_voice_pid(integer(), pid()) -> {ok, pid()} | {error, not_found}.
resolve_voice_pid(GuildId, FallbackGuildPid) ->
    case guild_voice_server:resolve_result(GuildId, FallbackGuildPid) of
        {ok, FallbackGuildPid} ->
            validate_voice_server_pid(FallbackGuildPid);
        Result ->
            Result
    end.

-spec validate_voice_server_pid(pid()) -> {ok, pid()} | {error, not_found}.
validate_voice_server_pid(Pid) ->
    case guild_voice_server:is_voice_server_pid(Pid) of
        true -> {ok, Pid};
        false -> {error, not_found}
    end.

-spec circuit_breaker_execute(pid(), map(), timeout()) ->
    voice_state_update_result().
circuit_breaker_execute(Pid, Request, Timeout) ->
    guild_client_circuit_breaker:execute_with_circuit_breaker(
        Pid, Request, Timeout
    ).

-spec do_call(pid(), map(), timeout()) -> voice_state_update_result().
do_call(GuildPid, Request, Timeout) ->
    try
        Reply = gen_server:call(GuildPid, {voice_state_update, Request}, Timeout),
        Result = normalize_voice_state_update_reply(Reply),
        maybe_log_unexpected_reply(GuildPid, Reply, Result),
        Result
    catch
        exit:ExitReason ->
            handle_call_exit(GuildPid, ExitReason);
        error:Reason ->
            log_call_error(GuildPid, {error, Reason}),
            {error, unknown, internal_error}
    end.

-spec handle_call_exit(pid(), term()) -> voice_state_update_result().
handle_call_exit(GuildPid, {timeout, _}) ->
    log_call_error(GuildPid, timeout),
    {error, timeout};
handle_call_exit(GuildPid, {noproc, _}) ->
    log_call_error(GuildPid, noproc),
    {error, noproc};
handle_call_exit(GuildPid, {normal, _}) ->
    log_call_error(GuildPid, normal_exit),
    {error, noproc};
handle_call_exit(GuildPid, {{nodedown, Node}, _}) ->
    log_call_error(GuildPid, {nodedown, Node}),
    {error, noproc};
handle_call_exit(GuildPid, {shutdown, R}) ->
    log_call_error(GuildPid, {shutdown, R}),
    {error, noproc};
handle_call_exit(GuildPid, {killed, R}) ->
    log_call_error(GuildPid, {killed, R}),
    {error, noproc};
handle_call_exit(GuildPid, Reason) ->
    log_call_error(GuildPid, {exit, Reason}),
    {error, unknown, internal_error}.

-spec log_call_error(pid(), term()) -> ok.
log_call_error(GuildPid, Reason) ->
    logger:warning(
        "guild_client_do_call_error:"
        " guild_pid=~p reason=~p",
        [GuildPid, Reason]
    ),
    ok.

-spec maybe_log_unexpected_reply(pid(), term(), voice_state_update_result()) -> ok.
maybe_log_unexpected_reply(GuildPid, Reply, {error, unknown, internal_error}) ->
    logger:warning(
        "guild_client_do_call_unexpected_reply: guild_pid=~p pid_info=~p raw_reply=~p",
        [GuildPid, pid_trace(GuildPid), Reply]
    ),
    ok;
maybe_log_unexpected_reply(_GuildPid, _Reply, _Result) ->
    ok.

-spec pid_trace(pid()) -> map().
pid_trace(GuildPid) ->
    #{
        alive => process_liveness:is_alive(GuildPid),
        current_function => normalize_process_info(process_info(GuildPid, current_function)),
        registered_name => normalize_process_info(process_info(GuildPid, registered_name)),
        initial_call => normalize_process_info(process_info(GuildPid, initial_call))
    }.

-spec normalize_process_info(term()) -> term().
normalize_process_info({_, Value}) -> Value;
normalize_process_info(undefined) -> undefined;
normalize_process_info(Value) -> Value.

-spec request_trace(map()) -> map().
request_trace(Request) ->
    #{
        user_id => maps:get(user_id, Request, undefined),
        channel_id => maps:get(channel_id, Request, undefined),
        session_id => maps:get(session_id, Request, undefined),
        connection_id => maps:get(connection_id, Request, undefined),
        self_mute => maps:get(self_mute, Request, undefined),
        self_deaf => maps:get(self_deaf, Request, undefined),
        self_video => maps:get(self_video, Request, undefined),
        self_stream => maps:get(self_stream, Request, undefined)
    }.

-spec normalize_voice_state_update_reply(term()) -> voice_state_update_result().
normalize_voice_state_update_reply(#{success := false, ack := Ack} = Response) when
    is_map(Ack)
->
    {ok, require_rejection_response(Response)};
normalize_voice_state_update_reply(Response) when is_map(Response) ->
    case maps:get(success, Response, false) of
        true -> {ok, require_success_response(Response)};
        false -> {error, unknown, internal_error}
    end;
normalize_voice_state_update_reply({ok, Response}) when is_map(Response) ->
    normalize_voice_state_update_reply(Response);
normalize_voice_state_update_reply({error, Category, ErrorAtom}) when
    is_atom(Category), is_atom(ErrorAtom)
->
    {error, Category, ErrorAtom};
normalize_voice_state_update_reply({error, {Category, ErrorAtom}}) when
    is_atom(Category), is_atom(ErrorAtom)
->
    {error, Category, ErrorAtom};
normalize_voice_state_update_reply({error, timeout}) ->
    {error, timeout};
normalize_voice_state_update_reply({error, noproc}) ->
    {error, noproc};
normalize_voice_state_update_reply({error, ErrorAtom}) when is_atom(ErrorAtom) ->
    {error, unknown, ErrorAtom};
normalize_voice_state_update_reply(ok) ->
    {error, unknown, internal_error};
normalize_voice_state_update_reply(_) ->
    {error, unknown, internal_error}.

-spec require_success_response(map()) -> voice_state_update_success().
require_success_response(#{success := true} = Response) ->
    Response.

-spec require_rejection_response(map()) -> voice_state_update_rejection().
require_rejection_response(#{success := false, ack := Ack} = Response) when is_map(Ack) ->
    Response.

-ifdef(TEST).

do_call_ok_reply_returns_error_test() ->
    Pid = start_test_call_server(ok),
    ?assertEqual({error, unknown, internal_error}, do_call(Pid, #{}, 1000)),
    stop_test_call_server(Pid).

do_call_success_map_reply_returns_ok_test() ->
    Reply = #{success => true, token => <<"abc">>},
    Pid = start_test_call_server(Reply),
    ?assertEqual({ok, Reply}, do_call(Pid, #{}, 1000)),
    stop_test_call_server(Pid).

do_call_rejected_ack_map_reply_returns_ok_test() ->
    Reply = #{success => false, ack => #{<<"status">> => <<"rejected">>}},
    Pid = start_test_call_server(Reply),
    ?assertEqual({ok, Reply}, do_call(Pid, #{}, 1000)),
    stop_test_call_server(Pid).

do_call_error_tuple_reply_passthrough_test() ->
    Reply = {error, validation_error, voice_member_not_found},
    Pid = start_test_call_server(Reply),
    ?assertEqual(Reply, do_call(Pid, #{}, 1000)),
    stop_test_call_server(Pid).

do_call_nested_error_tuple_reply_passthrough_test() ->
    Reply = {error, {validation_error, voice_member_not_found}},
    Pid = start_test_call_server(Reply),
    ?assertEqual({error, validation_error, voice_member_not_found}, do_call(Pid, #{}, 1000)),
    stop_test_call_server(Pid).

do_call_not_owner_error_tuple_reply_normalized_test() ->
    Reply = {error, {not_owner, 'fluxer_gateway@127.0.0.1'}},
    Pid = start_test_call_server(Reply),
    ?assertEqual({error, not_owner, 'fluxer_gateway@127.0.0.1'}, do_call(Pid, #{}, 1000)),
    stop_test_call_server(Pid).

-spec start_test_call_server(term()) -> pid().
start_test_call_server(Reply) ->
    spawn(fun() -> test_call_server_loop(Reply) end).

-spec stop_test_call_server(pid()) -> ok.
stop_test_call_server(Pid) ->
    Pid ! stop,
    ok.

-spec test_call_server_loop(term()) -> ok.
test_call_server_loop(Reply) ->
    receive
        {'$gen_call', From, {voice_state_update, _Request}} ->
            gen_server:reply(From, Reply),
            test_call_server_loop(Reply);
        stop ->
            ok
    after 30000 ->
        ok
    end.

-endif.
