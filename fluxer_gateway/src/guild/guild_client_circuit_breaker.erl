%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_client_circuit_breaker).

-typing([eqwalizer]).

-export([
    ensure_table/0,
    acquire_slot/1,
    release_slot/1,
    get_circuit_state/1,
    execute_with_circuit_breaker/3
]).

-type circuit_state() :: closed | open | half_open.

-export_type([circuit_state/0]).

-define(CIRCUIT_BREAKER_TABLE, guild_circuit_breaker).
-define(FAILURE_THRESHOLD, 5).
-define(RECOVERY_TIMEOUT_MS, 30000).
-define(MAX_CONCURRENT, 50).

-spec ensure_table() -> ok.
ensure_table() ->
    guild_ets_utils:ensure_table(?CIRCUIT_BREAKER_TABLE, [
        named_table,
        public,
        set,
        {read_concurrency, true},
        {write_concurrency, true}
    ]).

-spec acquire_slot(pid()) -> ok | {error, too_many_requests}.
acquire_slot(GuildPid) ->
    case safe_lookup(GuildPid) of
        [] ->
            safe_insert(GuildPid, #{
                state => closed,
                failures => 0,
                concurrent => 1
            }),
            ok;
        [{_, #{concurrent := C}}] when C >= ?MAX_CONCURRENT ->
            {error, too_many_requests};
        [{_, #{concurrent := C} = State}] ->
            safe_insert(GuildPid, State#{concurrent => C + 1}),
            ok
    end.

-spec release_slot(pid()) -> ok.
release_slot(GuildPid) ->
    case safe_lookup(GuildPid) of
        [{_, #{concurrent := C} = State}] when C > 0 ->
            safe_insert(GuildPid, State#{concurrent => C - 1}),
            ok;
        _ ->
            ok
    end.

-spec get_circuit_state(pid()) -> circuit_state().
get_circuit_state(GuildPid) ->
    case safe_lookup(GuildPid) of
        [] ->
            closed;
        [{_, #{state := open, opened_at := OpenedAt}}] ->
            check_recovery_timeout(OpenedAt);
        [{_, #{state := State}}] ->
            State
    end.

-spec check_recovery_timeout(integer()) -> circuit_state().
check_recovery_timeout(OpenedAt) ->
    Now = erlang:system_time(millisecond),
    case Now - OpenedAt > ?RECOVERY_TIMEOUT_MS of
        true -> half_open;
        false -> open
    end.

-spec execute_with_circuit_breaker(pid(), map(), timeout()) ->
    guild_client:voice_state_update_result().
execute_with_circuit_breaker(GuildPid, Request, Timeout) ->
    case get_circuit_state(GuildPid) of
        open ->
            log_circuit_open(GuildPid, Request),
            {error, circuit_breaker_open};
        State when State =:= closed; State =:= half_open ->
            do_execute(GuildPid, Request, Timeout, State)
    end.

-spec do_execute(pid(), map(), timeout(), circuit_state()) ->
    guild_client:voice_state_update_result().
do_execute(GuildPid, Request, Timeout, State) ->
    try
        Result = guild_client:do_call(GuildPid, Request, Timeout),
        update_circuit_state(GuildPid, Result, State),
        Result
    catch
        Class:Reason ->
            log_exception(GuildPid, State, Class, Reason),
            update_circuit_state(GuildPid, {error, unknown, internal_error}, State),
            {error, unknown, internal_error}
    end.

-spec log_circuit_open(pid(), map()) -> ok.
log_circuit_open(GuildPid, Request) ->
    logger:warning(
        "guild_client_voice_state_update_circuit_open: guild_pid=~p request=~p",
        [GuildPid, guild_client:request_trace(Request)]
    ),
    ok.

-spec log_exception(pid(), circuit_state(), atom(), term()) -> ok.
log_exception(GuildPid, State, Class, Reason) ->
    logger:warning(
        "guild_client_voice_state_update_exception: guild_pid=~p state=~p class=~p reason=~p",
        [GuildPid, State, Class, Reason]
    ),
    ok.

-spec update_circuit_state(pid(), guild_client:voice_state_update_result(), circuit_state()) ->
    ok.
update_circuit_state(GuildPid, Result, PrevState) ->
    IsSuccess = is_success_result(Result),
    case {IsSuccess, PrevState} of
        {true, half_open} ->
            safe_delete(GuildPid),
            ok;
        {true, closed} ->
            reset_failures(GuildPid);
        {false, _} ->
            record_failure(GuildPid)
    end.

-spec is_success_result(guild_client:voice_state_update_result()) -> boolean().
is_success_result({ok, _}) ->
    true;
is_success_result({error, Category, _}) when
    Category =:= validation_error;
    Category =:= not_found;
    Category =:= permission_denied;
    Category =:= voice_error;
    Category =:= rate_limited;
    Category =:= auth_failed
->
    true;
is_success_result(_) ->
    false.

-spec reset_failures(pid()) -> ok.
reset_failures(GuildPid) ->
    case safe_lookup(GuildPid) of
        [{_, State}] ->
            safe_insert(GuildPid, State#{failures => 0}),
            ok;
        [] ->
            ok
    end.

-spec record_failure(pid()) -> ok.
record_failure(GuildPid) ->
    Now = erlang:system_time(millisecond),
    case safe_lookup(GuildPid) of
        [] ->
            safe_insert(GuildPid, #{
                state => closed,
                failures => 1,
                concurrent => 0
            }),
            ok;
        [{_, #{failures := F} = State}] when F + 1 >= ?FAILURE_THRESHOLD ->
            safe_insert(GuildPid, State#{
                state => open,
                failures => F + 1,
                opened_at => Now
            }),
            ok;
        [{_, #{failures := F} = State}] ->
            safe_insert(GuildPid, State#{failures => F + 1}),
            ok
    end.

-spec safe_insert(pid(), map()) -> ok.
safe_insert(GuildPid, State) ->
    ensure_table(),
    case try_insert(GuildPid, State) of
        ok ->
            ok;
        retry ->
            ensure_table(),
            _ = try_insert(GuildPid, State),
            ok
    end.

-spec try_insert(pid(), map()) -> ok | retry.
try_insert(GuildPid, State) ->
    try ets:insert(?CIRCUIT_BREAKER_TABLE, {GuildPid, State}) of
        true -> ok
    catch
        error:badarg -> retry
    end.

-spec safe_delete(pid()) -> ok.
safe_delete(GuildPid) ->
    ensure_table(),
    try ets:delete(?CIRCUIT_BREAKER_TABLE, GuildPid) of
        true -> ok
    catch
        error:badarg -> ok
    end.

-spec safe_lookup(pid()) -> list().
safe_lookup(GuildPid) ->
    try ets:lookup(?CIRCUIT_BREAKER_TABLE, GuildPid) of
        Result -> Result
    catch
        error:badarg -> []
    end.
