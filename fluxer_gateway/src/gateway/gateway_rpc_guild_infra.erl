%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_rpc_guild_infra).

-typing([eqwalizer]).

-export([
    with_guild/2, with_guild/3,
    with_guild_unchecked/2,
    with_voice_server/2,
    ensure_guild_pid/1,
    get_guild_pid/1,
    get_or_start_guild_pid/1,
    get_guild_pid_with_retry/1,
    safe_guild_call/4,
    safe_gen_server_call/3,
    resolve_voice_pid/2,
    cache_guild_pid/2,
    delete_cached_guild_pid/2,
    batch_lookup_guild_pids/1,
    is_cached_guild_pid_alive/1
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(GUILD_LOOKUP_TIMEOUT, 2500).
-define(GUILD_START_LOOKUP_TIMEOUT, 8000).
-define(GUILD_CALL_TIMEOUT, 4000).
-define(GUILD_START_MAX_ATTEMPTS, 3).
-define(GUILD_START_BASE_MS, 250).
-define(GUILD_START_MAX_MS, 2000).
-define(GUILD_START_JITTER_MS, 100).

-spec with_guild(integer(), fun((pid()) -> T)) -> T when T :: term().
with_guild(GuildId, Fun) -> with_guild(GuildId, Fun, <<"guild_not_found">>).

-spec with_guild(integer(), fun((pid()) -> T), binary()) -> T when T :: term().
with_guild(GuildId, Fun, NotFoundError) ->
    case ensure_guild_pid(GuildId) of
        {ok, Pid} ->
            run_with_guild_pid(GuildId, Pid, Fun, NotFoundError);
        _ ->
            gateway_rpc_error:raise(NotFoundError)
    end.

-spec run_with_guild_pid(integer(), pid(), fun((pid()) -> T), binary()) -> T when
    T :: term().
run_with_guild_pid(GuildId, Pid, Fun, NotFoundError) ->
    Checked = fun(CurrentPid) ->
        ensure_responsive(CurrentPid),
        Fun(CurrentPid)
    end,
    run_with_guild_pid_guard(GuildId, Pid, fun() -> Checked(Pid) end, Checked, NotFoundError).

-spec with_guild_unchecked(integer(), fun((pid()) -> T)) -> T when T :: term().
with_guild_unchecked(GuildId, Fun) ->
    case ensure_guild_pid(GuildId) of
        {ok, Pid} ->
            run_with_guild_pid_guard(
                GuildId, Pid, fun() -> Fun(Pid) end, Fun, <<"guild_not_found">>
            );
        error ->
            gateway_rpc_error:raise(<<"guild_not_found">>)
    end.

-spec ensure_responsive(pid()) -> ok.
ensure_responsive(Pid) ->
    case guild_health:is_overloaded(Pid) of
        true -> gateway_rpc_error:raise(<<"guild_overloaded">>);
        false -> ok
    end.

-spec with_voice_server(integer(), fun((pid(), pid()) -> T)) -> T when T :: term().
with_voice_server(GuildId, Fun) ->
    case ensure_guild_pid(GuildId) of
        {ok, GuildPid} ->
            with_voice_guild_pid(GuildId, GuildPid, Fun);
        _ ->
            gateway_rpc_error:raise(<<"guild_not_found">>)
    end.

-spec with_voice_guild_pid(integer(), pid(), fun((pid(), pid()) -> T)) -> T when T :: term().
with_voice_guild_pid(GuildId, GuildPid, Fun) ->
    case safe_resolve_voice_pid(GuildId, GuildPid) of
        {ok, VoicePid} ->
            run_with_voice_pid(GuildId, GuildPid, VoicePid, Fun);
        error ->
            gateway_rpc_error:raise(<<"guild_not_found">>)
    end.

-spec run_with_voice_pid(integer(), pid(), pid(), fun((pid(), pid()) -> T)) -> T when
    T :: term().
run_with_voice_pid(GuildId, GuildPid, VoicePid, Fun) ->
    run_with_guild_pid_guard(
        GuildId,
        GuildPid,
        fun() -> Fun(VoicePid, GuildPid) end,
        fun(NewGuildPid) -> Fun(resolve_voice_pid(GuildId, NewGuildPid), NewGuildPid) end,
        <<"guild_not_found">>
    ).

-spec resolve_voice_pid(integer(), pid()) -> pid().
resolve_voice_pid(GuildId, FallbackGuildPid) ->
    case guild_voice_server:resolve_result(GuildId, FallbackGuildPid) of
        {ok, FallbackGuildPid} ->
            valid_fallback_voice_pid(FallbackGuildPid);
        {ok, VoicePid} ->
            VoicePid;
        {error, not_found} ->
            gateway_rpc_error:raise(<<"guild_not_found">>)
    end.

-spec valid_fallback_voice_pid(pid()) -> pid().
valid_fallback_voice_pid(FallbackGuildPid) ->
    case guild_voice_server:is_voice_server_pid(FallbackGuildPid) of
        true -> FallbackGuildPid;
        false -> gateway_rpc_error:raise(<<"guild_not_found">>)
    end.

-spec safe_resolve_voice_pid(integer(), pid()) -> {ok, pid()} | error.
safe_resolve_voice_pid(GuildId, GuildPid) ->
    try resolve_voice_pid(GuildId, GuildPid) of
        VoicePid -> decode_voice_pid(VoicePid)
    catch
        throw:_Reason -> error;
        error:_Reason -> error;
        exit:_Reason -> error
    end.

-spec decode_voice_pid(pid()) -> {ok, pid()}.
decode_voice_pid(VoicePid) ->
    {ok, VoicePid}.

-spec run_with_guild_pid_guard(integer(), pid(), fun(() -> T), fun((pid()) -> T), binary()) ->
    T
when
    T :: term().
run_with_guild_pid_guard(GuildId, Pid, Fun, RetryFun, ErrorCode) ->
    try
        Fun()
    catch
        exit:{timeout, _} ->
            delete_cached_guild_pid(GuildId, Pid),
            gateway_rpc_error:raise(<<"timeout">>);
        exit:{nodedown, _} ->
            retry_after_cached_pid_failure(GuildId, Pid, RetryFun, ErrorCode);
        exit:{noproc, _} ->
            retry_after_cached_pid_failure(GuildId, Pid, RetryFun, ErrorCode);
        error:{nodedown, _} ->
            retry_after_cached_pid_failure(GuildId, Pid, RetryFun, ErrorCode);
        error:{noproc, _} ->
            retry_after_cached_pid_failure(GuildId, Pid, RetryFun, ErrorCode);
        error:{gateway_rpc_error, Message} ->
            gateway_rpc_error:raise(Message);
        error:_Reason ->
            delete_cached_guild_pid(GuildId, Pid),
            gateway_rpc_error:raise(ErrorCode);
        exit:_Reason ->
            delete_cached_guild_pid(GuildId, Pid),
            gateway_rpc_error:raise(ErrorCode)
    end.

-spec retry_after_cached_pid_failure(integer(), pid(), fun((pid()) -> T), binary()) -> T when
    T :: term().
retry_after_cached_pid_failure(GuildId, Pid, RetryFun, ErrorCode) ->
    delete_cached_guild_pid(GuildId, Pid),
    retry_with_fresh_pid(GuildId, RetryFun, ErrorCode).

-spec retry_with_fresh_pid(integer(), fun((pid()) -> T), binary()) -> T when T :: term().
retry_with_fresh_pid(GuildId, RetryFun, ErrorCode) ->
    case ensure_guild_pid(GuildId) of
        {ok, NewPid} ->
            retry_with_pid(GuildId, NewPid, RetryFun, ErrorCode);
        error ->
            gateway_rpc_error:raise(ErrorCode)
    end.

-spec retry_with_pid(integer(), pid(), fun((pid()) -> T), binary()) -> T when T :: term().
retry_with_pid(GuildId, NewPid, RetryFun, ErrorCode) ->
    try
        RetryFun(NewPid)
    catch
        exit:{timeout, _} ->
            delete_cached_guild_pid(GuildId, NewPid),
            gateway_rpc_error:raise(<<"timeout">>);
        throw:_Reason ->
            delete_cached_guild_pid(GuildId, NewPid),
            gateway_rpc_error:raise(ErrorCode);
        error:{gateway_rpc_error, Message} ->
            gateway_rpc_error:raise(Message);
        error:_Reason ->
            delete_cached_guild_pid(GuildId, NewPid),
            gateway_rpc_error:raise(ErrorCode);
        exit:_Reason ->
            delete_cached_guild_pid(GuildId, NewPid),
            gateway_rpc_error:raise(ErrorCode)
    end.

-spec get_guild_pid_with_retry(integer()) -> {ok, pid()} | error.
get_guild_pid_with_retry(GuildId) -> get_guild_pid_with_retry(GuildId, 1).

-spec get_guild_pid_with_retry(integer(), pos_integer()) -> {ok, pid()} | error.
get_guild_pid_with_retry(GuildId, Attempt) ->
    case get_or_start_guild_pid(GuildId) of
        {ok, Pid} ->
            log_retry_success(GuildId, Attempt),
            {ok, Pid};
        error when Attempt < ?GUILD_START_MAX_ATTEMPTS ->
            Delay = guild_start_backoff_delay(Attempt),
            logger:warning(
                "Guild start retrying: guild_id=~p attempt=~p/~p delay_ms=~p",
                [GuildId, Attempt, ?GUILD_START_MAX_ATTEMPTS, Delay]
            ),
            ok = gateway_retry_timer:wait(Delay),
            get_guild_pid_with_retry(GuildId, Attempt + 1);
        error ->
            logger:error(
                "Guild start retries exhausted: guild_id=~p attempts=~p",
                [GuildId, ?GUILD_START_MAX_ATTEMPTS]
            ),
            error
    end.

-spec log_retry_success(integer(), pos_integer()) -> ok.
log_retry_success(_GuildId, 1) ->
    ok;
log_retry_success(GuildId, Attempt) ->
    logger:info("Guild start succeeded after retry", #{guild_id => GuildId, attempt => Attempt}).

-spec guild_start_backoff_delay(pos_integer()) -> pos_integer().
guild_start_backoff_delay(Attempt) ->
    Backoff = min(?GUILD_START_BASE_MS * (1 bsl (Attempt - 1)), ?GUILD_START_MAX_MS),
    Backoff + rand:uniform(?GUILD_START_JITTER_MS).

-spec get_guild_pid(integer()) -> {ok, pid()} | error.
get_guild_pid(GuildId) ->
    case lookup_guild_pid_from_cache(GuildId) of
        {ok, Pid} -> {ok, Pid};
        not_found -> lookup_guild_pid_from_manager(GuildId)
    end.

-spec ensure_guild_pid(integer()) -> {ok, pid()} | error.
ensure_guild_pid(GuildId) ->
    case lookup_guild_pid_from_cache(GuildId) of
        {ok, Pid} -> {ok, Pid};
        not_found -> start_or_lookup_guild_pid_from_manager(GuildId)
    end.

-spec get_or_start_guild_pid(integer()) -> {ok, pid()} | error.
get_or_start_guild_pid(GuildId) -> ensure_guild_pid(GuildId).

-spec lookup_guild_pid_from_cache(integer()) -> {ok, pid()} | not_found.
lookup_guild_pid_from_cache(GuildId) ->
    try ets:lookup(guild_pid_cache, GuildId) of
        [{GuildId, Pid}] when is_pid(Pid) ->
            cached_guild_pid_result(GuildId, Pid);
        _ ->
            not_found
    catch
        error:badarg -> not_found
    end.

-spec cached_guild_pid_result(integer(), pid()) -> {ok, pid()} | not_found.
cached_guild_pid_result(GuildId, Pid) ->
    case is_cached_guild_pid_alive(Pid) of
        true ->
            {ok, Pid};
        false ->
            safe_delete_guild_pid_cache(GuildId),
            not_found
    end.

-spec lookup_guild_pid_from_manager(integer()) -> {ok, pid()} | error.
lookup_guild_pid_from_manager(GuildId) ->
    case
        gateway_rpc_guild_routing:call_owner_guild_manager(
            GuildId, {lookup, GuildId}, ?GUILD_LOOKUP_TIMEOUT
        )
    of
        {ok, Pid} when is_pid(Pid) -> cache_and_return_guild_pid(GuildId, Pid);
        _ -> error
    end.

-spec start_or_lookup_guild_pid_from_manager(integer()) -> {ok, pid()} | error.
start_or_lookup_guild_pid_from_manager(GuildId) ->
    Request = {start_or_lookup, GuildId},
    case
        gateway_rpc_guild_routing:call_owner_guild_manager(
            GuildId, Request, ?GUILD_START_LOOKUP_TIMEOUT
        )
    of
        {ok, Pid} when is_pid(Pid) -> cache_and_return_guild_pid(GuildId, Pid);
        _ -> error
    end.

-spec cache_and_return_guild_pid(integer(), pid()) -> {ok, pid()}.
cache_and_return_guild_pid(GuildId, Pid) ->
    cache_guild_pid(GuildId, Pid),
    {ok, Pid}.

-spec cache_guild_pid(integer(), pid()) -> ok.
cache_guild_pid(GuildId, Pid) ->
    try ets:insert(guild_pid_cache, {GuildId, Pid}) of
        _ -> ok
    catch
        throw:_Reason -> ok;
        error:_Reason -> ok;
        exit:_Reason -> ok
    end.

-spec delete_cached_guild_pid(integer(), pid()) -> ok.
delete_cached_guild_pid(GuildId, Pid) ->
    try ets:lookup(guild_pid_cache, GuildId) of
        [{GuildId, Pid}] -> safe_delete_guild_pid_cache(GuildId);
        _ -> ok
    catch
        error:badarg -> ok
    end.

-spec safe_delete_guild_pid_cache(integer()) -> ok.
safe_delete_guild_pid_cache(GuildId) ->
    try ets:delete(guild_pid_cache, GuildId) of
        _ -> ok
    catch
        throw:_Reason -> ok;
        error:_Reason -> ok;
        exit:_Reason -> ok
    end.

-spec batch_lookup_guild_pids([integer()]) -> ok.
batch_lookup_guild_pids(GuildIds) ->
    lists:foreach(fun batch_lookup_guild_pid/1, GuildIds),
    ok.

-spec batch_lookup_guild_pid(integer()) -> ok.
batch_lookup_guild_pid(GuildId) ->
    case lookup_guild_pid_from_cache(GuildId) of
        {ok, _} -> ok;
        not_found -> start_or_lookup_for_batch(GuildId)
    end.

-spec start_or_lookup_for_batch(integer()) -> ok.
start_or_lookup_for_batch(GuildId) ->
    try guild_manager:start_or_lookup(GuildId, 2000) of
        {ok, Pid} when is_pid(Pid) -> cache_guild_pid(GuildId, Pid);
        _ -> ok
    catch
        throw:_Reason -> ok;
        error:_Reason -> ok;
        exit:_Reason -> ok
    end.

-spec is_cached_guild_pid_alive(pid()) -> boolean().
is_cached_guild_pid_alive(Pid) ->
    case node(Pid) of
        LocalNode when LocalNode =:= node() -> process_liveness:is_alive(Pid);
        RemoteNode -> lists:member(RemoteNode, nodes())
    end.

-spec safe_gen_server_call(pid(), term(), pos_integer()) -> {ok, term()} | error.
safe_gen_server_call(Pid, Request, Timeout) ->
    try gen_server:call(Pid, Request, Timeout) of
        Reply -> {ok, Reply}
    catch
        exit:{timeout, _} -> error;
        exit:{nodedown, _} -> error;
        exit:{noproc, _} -> error;
        throw:_Reason -> error;
        error:_Reason -> error;
        exit:_ -> error
    end.

-spec safe_guild_call(integer(), pid(), {atom(), map()}, pos_integer()) ->
    {ok, term()} | error.
safe_guild_call(GuildId, Pid, Request, Timeout) ->
    ensure_responsive(Pid),
    try guild_query_handler:call(Pid, Request, Timeout) of
        Reply -> {ok, Reply}
    catch
        exit:{timeout, _} ->
            delete_cached_guild_pid(GuildId, Pid),
            error;
        exit:{nodedown, _} ->
            retry_after_guild_call_failure(GuildId, Pid, Request, Timeout);
        exit:{noproc, _} ->
            retry_after_guild_call_failure(GuildId, Pid, Request, Timeout);
        exit:_ ->
            delete_cached_guild_pid(GuildId, Pid),
            error
    end.

-spec retry_after_guild_call_failure(integer(), pid(), {atom(), map()}, pos_integer()) ->
    {ok, term()} | error.
retry_after_guild_call_failure(GuildId, Pid, Request, Timeout) ->
    delete_cached_guild_pid(GuildId, Pid),
    retry_guild_call(GuildId, Request, Timeout).

-spec retry_guild_call(integer(), {atom(), map()}, pos_integer()) -> {ok, term()} | error.
retry_guild_call(GuildId, Request, Timeout) ->
    case get_guild_pid(GuildId) of
        {ok, NewPid} ->
            retry_guild_call_pid(GuildId, NewPid, Request, Timeout);
        error ->
            error
    end.

-spec retry_guild_call_pid(integer(), pid(), {atom(), map()}, pos_integer()) ->
    {ok, term()} | error.
retry_guild_call_pid(GuildId, NewPid, Request, Timeout) ->
    ensure_responsive(NewPid),
    try guild_query_handler:call(NewPid, Request, Timeout) of
        Reply -> {ok, Reply}
    catch
        throw:_Reason ->
            delete_cached_guild_pid(GuildId, NewPid),
            error;
        error:_Reason ->
            delete_cached_guild_pid(GuildId, NewPid),
            error;
        exit:_Reason ->
            delete_cached_guild_pid(GuildId, NewPid),
            error
    end.

-ifdef(TEST).

overloaded_guild_is_rejected_before_enqueuing_work_test() ->
    ok = guild_ets_owner:ensure_table(guild_health_status, [named_table, public, set]),
    Pid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    Pending = {make_ref(), erlang:monotonic_time(millisecond), 2500},
    true = ets:insert(guild_health_status, {Pid, 42, true, undefined, Pending}),
    try
        ?assertError(
            {gateway_rpc_error, <<"guild_overloaded">>},
            safe_guild_call(42, Pid, {get_viewer_counts, #{user_id => 200}}, 4000)
        ),
        ?assertError(
            {gateway_rpc_error, <<"guild_overloaded">>},
            retry_guild_call_pid(42, Pid, {get_viewer_counts, #{user_id => 200}}, 4000)
        ),
        ?assertEqual({message_queue_len, 0}, process_info(Pid, message_queue_len))
    after
        ets:delete(guild_health_status, Pid),
        Pid ! stop
    end.

guild_start_backoff_delay_exponential_test() ->
    Delay1 = guild_start_backoff_delay(1),
    ?assert(Delay1 >= ?GUILD_START_BASE_MS),
    ?assert(Delay1 =< ?GUILD_START_BASE_MS + ?GUILD_START_JITTER_MS),
    Delay2 = guild_start_backoff_delay(2),
    ?assert(Delay2 >= ?GUILD_START_BASE_MS * 2),
    ?assert(Delay2 =< ?GUILD_START_BASE_MS * 2 + ?GUILD_START_JITTER_MS).

guild_start_backoff_delay_caps_at_max_test() ->
    ?assert(guild_start_backoff_delay(10) =< ?GUILD_START_MAX_MS + ?GUILD_START_JITTER_MS).

run_with_guild_pid_guard_generic_exit_returns_domain_error_test() ->
    ?assertError(
        {gateway_rpc_error, <<"guild_not_found">>},
        run_with_guild_pid_guard(
            123,
            self(),
            fun() -> exit(kaboom) end,
            fun(_Pid) -> ok end,
            <<"guild_not_found">>
        )
    ).

safe_gen_server_call_dead_pid_returns_error_test() ->
    DeadPid = spawn(fun() -> ok end),
    Ref = monitor(process, DeadPid),
    receive
        {'DOWN', Ref, process, DeadPid, _Reason} -> ok
    after 1000 ->
        ?assert(false, guild_rpc_pid_did_not_exit)
    end,
    ?assertEqual(error, safe_gen_server_call(DeadPid, ping, 250)).

-endif.
