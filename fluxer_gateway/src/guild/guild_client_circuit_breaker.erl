%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_client_circuit_breaker).

-typing([eqwalizer]).

-export([
    ensure_table/0,
    acquire_slot/1,
    release_slot/2,
    prune_dead/0,
    get_circuit_state/1,
    execute_with_circuit_breaker/3
]).

-type circuit_state() :: closed | open | half_open.

-type row() :: {pid(), closed | open, integer(), integer() | undefined}.

-export_type([circuit_state/0]).

-define(CIRCUIT_BREAKER_TABLE, guild_circuit_breaker).
-define(SLOT_TABLE, guild_circuit_breaker_slots).
-define(FAILURE_THRESHOLD, 5).
-define(RECOVERY_TIMEOUT_MS, 30000).
-define(MAX_CONCURRENT, 50).
-define(STATE_POS, 2).
-define(FAILURES_POS, 3).
-define(OPENED_AT_POS, 4).

-spec ensure_table() -> ok.
ensure_table() ->
    Options = [public, named_table, {read_concurrency, true}, {write_concurrency, true}],
    ok = guild_ets_utils:ensure_table(?CIRCUIT_BREAKER_TABLE, [set | Options]),
    guild_ets_utils:ensure_table(?SLOT_TABLE, [ordered_set | Options]).

-spec acquire_slot(pid()) -> ok | {error, too_many_requests}.
acquire_slot(GuildPid) ->
    try
        true = ets:insert_new(?SLOT_TABLE, {{GuildPid, self()}}),
        ets:select_count(?SLOT_TABLE, [{{{GuildPid, '_'}}, [], [true]}])
    of
        Count when Count =< ?MAX_CONCURRENT ->
            ok;
        _Count ->
            release_slot(GuildPid, self()),
            {error, too_many_requests}
    catch
        error:_ -> ok
    end.

-spec release_slot(pid(), pid()) -> ok.
release_slot(GuildPid, Worker) ->
    try ets:delete(?SLOT_TABLE, {GuildPid, Worker}) of
        _ -> ok
    catch
        error:badarg -> ok
    end.

-spec prune_dead() -> ok.
prune_dead() ->
    drop_unknown_rows(),
    Slots = slot_keys(),
    GuildPids = lists:usort([GuildPid || {GuildPid, _Worker} <- Slots] ++ circuit_pids()),
    Alive = process_liveness:are_alive(GuildPids),
    IsAlive = fun(Pid) -> maps:get(Pid, Alive, true) end,
    lists:foreach(
        fun({GuildPid, Worker}) -> release_slot(GuildPid, Worker) end,
        [
            Slot
         || {GuildPid, Worker} = Slot <- Slots,
            not (IsAlive(GuildPid) andalso is_process_alive(Worker))
        ]
    ),
    lists:foreach(fun safe_delete/1, [Pid || Pid <- GuildPids, not IsAlive(Pid)]).

-spec drop_unknown_rows() -> ok.
drop_unknown_rows() ->
    try
        ets:select_delete(?CIRCUIT_BREAKER_TABLE, [
            {{'_', '_'}, [], [true]}, {{'_', '_', '_', '_', '_'}, [], [true]}
        ])
    of
        _ -> ok
    catch
        error:badarg -> ok
    end.

-spec slot_keys() -> [{pid(), pid()}].
slot_keys() ->
    try ets:select(?SLOT_TABLE, [{{'$1'}, [], ['$1']}]) of
        Keys ->
            [{GuildPid, Worker} || {GuildPid, Worker} <- Keys, is_pid(GuildPid), is_pid(Worker)]
    catch
        error:badarg -> []
    end.

-spec circuit_pids() -> [pid()].
circuit_pids() ->
    try ets:select(?CIRCUIT_BREAKER_TABLE, [{{'$1', '_', '_', '_'}, [], ['$1']}]) of
        Pids -> [Pid || Pid <- Pids, is_pid(Pid)]
    catch
        error:badarg -> []
    end.

-spec safe_delete(pid()) -> ok.
safe_delete(GuildPid) ->
    try ets:delete(?CIRCUIT_BREAKER_TABLE, GuildPid) of
        _ -> ok
    catch
        error:badarg -> ok
    end.

-spec get_circuit_state(pid()) -> circuit_state().
get_circuit_state(GuildPid) ->
    case safe_lookup(GuildPid) of
        [{_, open, _, OpenedAt}] when is_integer(OpenedAt) ->
            check_recovery_timeout(OpenedAt);
        _ ->
            closed
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
            safe_update_element(GuildPid, [
                {?STATE_POS, closed}, {?FAILURES_POS, 0}, {?OPENED_AT_POS, undefined}
            ]);
        {true, closed} ->
            safe_update_element(GuildPid, [{?FAILURES_POS, 0}]);
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

-spec record_failure(pid()) -> ok.
record_failure(GuildPid) ->
    case safe_update_counter(GuildPid, {?FAILURES_POS, 1}) of
        {ok, F} when F >= ?FAILURE_THRESHOLD ->
            Now = erlang:system_time(millisecond),
            safe_update_element(GuildPid, [{?STATE_POS, open}, {?OPENED_AT_POS, Now}]);
        _ ->
            ok
    end.

-spec safe_update_counter(pid(), {pos_integer(), integer()}) -> {ok, integer()} | error.
safe_update_counter(GuildPid, Op) ->
    case try_update_counter(GuildPid, Op) of
        {ok, _} = Ok ->
            Ok;
        error ->
            ensure_table(),
            drop_unknown_row(GuildPid),
            try_update_counter(GuildPid, Op)
    end.

-spec drop_unknown_row(pid()) -> ok.
drop_unknown_row(GuildPid) ->
    case safe_lookup(GuildPid) of
        [{_, _, _, _}] -> ok;
        [] -> ok;
        _ -> safe_delete(GuildPid)
    end.

-spec try_update_counter(pid(), {pos_integer(), integer()}) -> {ok, integer()} | error.
try_update_counter(GuildPid, Op) ->
    try ets:update_counter(?CIRCUIT_BREAKER_TABLE, GuildPid, Op, default_row(GuildPid)) of
        Value when is_integer(Value) -> {ok, Value}
    catch
        error:badarg -> error
    end.

-spec default_row(pid()) -> row().
default_row(GuildPid) ->
    {GuildPid, closed, 0, undefined}.

-spec safe_update_element(pid(), [{pos_integer(), term()}]) -> ok.
safe_update_element(GuildPid, Changes) ->
    try ets:update_element(?CIRCUIT_BREAKER_TABLE, GuildPid, Changes) of
        _ -> ok
    catch
        error:badarg -> ok
    end.

-spec safe_lookup(pid()) -> [row()].
safe_lookup(GuildPid) ->
    try ets:lookup(?CIRCUIT_BREAKER_TABLE, GuildPid) of
        Result -> Result
    catch
        error:badarg -> []
    end.
