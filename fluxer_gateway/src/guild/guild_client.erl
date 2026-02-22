%% Copyright (C) 2026 Fluxer Contributors
%%
%% This file is part of Fluxer.
%%
%% Fluxer is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Affero General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% Fluxer is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%% GNU Affero General Public License for more details.
%%
%% You should have received a copy of the GNU Affero General Public License
%% along with Fluxer. If not, see <https://www.gnu.org/licenses/>.

-module(guild_client).

-export([voice_state_update/3]).
-export([voice_state_update/4]).

-export_type([
    voice_state_update_success/0,
    voice_state_update_error/0,
    voice_state_update_result/0
]).

-define(CIRCUIT_BREAKER_TABLE, guild_circuit_breaker).
-define(FAILURE_THRESHOLD, 5).
-define(RECOVERY_TIMEOUT_MS, 30000).
-define(MAX_CONCURRENT, 50).

-type voice_state_update_success() :: #{
    success := true,
    token => binary(),
    endpoint => binary(),
    connection_id => binary(),
    voice_state => map(),
    needs_token => boolean()
}.

-type voice_state_update_error() :: {error, atom(), atom()}.

-type voice_state_update_result() ::
    {ok, voice_state_update_success()}
    | {error, timeout}
    | {error, noproc}
    | {error, circuit_breaker_open}
    | {error, too_many_requests}
    | {error, atom(), atom()}.

-type circuit_state() :: closed | open | half_open.

-spec voice_state_update(pid(), map(), timeout()) -> voice_state_update_result().
voice_state_update(GuildPid, Request, Timeout) ->
    ensure_table(),
    TargetPid = GuildPid,
    case acquire_slot(TargetPid) of
        ok ->
            try
                execute_with_circuit_breaker(TargetPid, Request, Timeout)
            after
                release_slot(TargetPid)
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec voice_state_update(pid(), integer(), map(), timeout()) -> voice_state_update_result().
voice_state_update(GuildPid, GuildId, Request, Timeout) ->
    ensure_table(),
    TargetPid = resolve_voice_pid(GuildId, GuildPid),
    case acquire_slot(TargetPid) of
        ok ->
            try
                execute_with_circuit_breaker(TargetPid, Request, Timeout)
            after
                release_slot(TargetPid)
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec resolve_voice_pid(integer(), pid()) -> pid().
resolve_voice_pid(GuildId, FallbackGuildPid) ->
    case guild_voice_server:lookup(GuildId) of
        {ok, VoicePid} -> VoicePid;
        {error, not_found} -> FallbackGuildPid
    end.

-spec execute_with_circuit_breaker(pid(), map(), timeout()) -> voice_state_update_result().
execute_with_circuit_breaker(GuildPid, Request, Timeout) ->
    case get_circuit_state(GuildPid) of
        open ->
            {error, circuit_breaker_open};
        State when State =:= closed; State =:= half_open ->
            Result = do_call(GuildPid, Request, Timeout),
            update_circuit_state(GuildPid, Result, State),
            Result
    end.

-spec do_call(pid(), map(), timeout()) -> voice_state_update_result().
do_call(GuildPid, Request, Timeout) ->
    try gen_server:call(GuildPid, {voice_state_update, Request}, Timeout) of
        Response when is_map(Response) ->
            case maps:get(success, Response, false) of
                true -> {ok, Response};
                false -> {error, unknown, internal_error}
            end;
        {error, Category, ErrorAtom} when is_atom(Category), is_atom(ErrorAtom) ->
            {error, Category, ErrorAtom}
    catch
        exit:{timeout, _} -> {error, timeout};
        exit:{noproc, _} -> {error, noproc};
        exit:{normal, _} -> {error, noproc}
    end.

-spec get_circuit_state(pid()) -> circuit_state().
get_circuit_state(GuildPid) ->
    case safe_lookup(GuildPid) of
        [] ->
            closed;
        [{_, #{state := open, opened_at := OpenedAt}}] ->
            Now = erlang:system_time(millisecond),
            case Now - OpenedAt > ?RECOVERY_TIMEOUT_MS of
                true -> half_open;
                false -> open
            end;
        [{_, #{state := State}}] ->
            State
    end.

-spec update_circuit_state(pid(), voice_state_update_result(), circuit_state()) -> ok.
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

-spec is_success_result(voice_state_update_result()) -> boolean().
is_success_result({ok, _}) -> true;
is_success_result(_) -> false.

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

-spec safe_insert(pid(), map()) -> ok.
safe_insert(GuildPid, State) ->
    ensure_table(),
    try ets:insert(?CIRCUIT_BREAKER_TABLE, {GuildPid, State}) of
        true -> ok
    catch
        error:badarg ->
            ensure_table(),
            try ets:insert(?CIRCUIT_BREAKER_TABLE, {GuildPid, State}) of
                true -> ok
            catch
                error:badarg -> ok
            end
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

-spec ensure_table() -> ok.
ensure_table() ->
    guild_ets_utils:ensure_table(?CIRCUIT_BREAKER_TABLE, [
        named_table,
        public,
        set,
        {read_concurrency, true},
        {write_concurrency, true}
    ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

module_exports_test() ->
    Exports = guild_client:module_info(exports),
    ?assert(lists:member({voice_state_update, 3}, Exports)).

ensure_table_creates_table_test() ->
    catch ets:delete(?CIRCUIT_BREAKER_TABLE),
    ?assertEqual(undefined, ets:whereis(?CIRCUIT_BREAKER_TABLE)),
    ensure_table(),
    ?assertNotEqual(undefined, ets:whereis(?CIRCUIT_BREAKER_TABLE)).

ensure_table_idempotent_test() ->
    ensure_table(),
    ensure_table(),
    ?assertNotEqual(undefined, ets:whereis(?CIRCUIT_BREAKER_TABLE)).

acquire_slot_creates_entry_test() ->
    ensure_table(),
    Pid = spawn(fun() ->
        receive
            done -> ok
        end
    end),
    ets:delete_all_objects(?CIRCUIT_BREAKER_TABLE),
    ?assertEqual(ok, acquire_slot(Pid)),
    [{Pid, State}] = ets:lookup(?CIRCUIT_BREAKER_TABLE, Pid),
    ?assertEqual(1, maps:get(concurrent, State)),
    Pid ! done.

acquire_slot_increments_test() ->
    ensure_table(),
    Pid = spawn(fun() ->
        receive
            done -> ok
        end
    end),
    ets:delete_all_objects(?CIRCUIT_BREAKER_TABLE),
    acquire_slot(Pid),
    acquire_slot(Pid),
    [{Pid, State}] = ets:lookup(?CIRCUIT_BREAKER_TABLE, Pid),
    ?assertEqual(2, maps:get(concurrent, State)),
    Pid ! done.

release_slot_decrements_test() ->
    ensure_table(),
    Pid = spawn(fun() ->
        receive
            done -> ok
        end
    end),
    ets:delete_all_objects(?CIRCUIT_BREAKER_TABLE),
    acquire_slot(Pid),
    acquire_slot(Pid),
    release_slot(Pid),
    [{Pid, State}] = ets:lookup(?CIRCUIT_BREAKER_TABLE, Pid),
    ?assertEqual(1, maps:get(concurrent, State)),
    Pid ! done.

get_circuit_state_closed_test() ->
    ensure_table(),
    Pid = spawn(fun() ->
        receive
            done -> ok
        end
    end),
    ets:delete_all_objects(?CIRCUIT_BREAKER_TABLE),
    ?assertEqual(closed, get_circuit_state(Pid)),
    Pid ! done.

get_circuit_state_open_test() ->
    ensure_table(),
    Pid = spawn(fun() ->
        receive
            done -> ok
        end
    end),
    ets:delete_all_objects(?CIRCUIT_BREAKER_TABLE),
    Now = erlang:system_time(millisecond),
    ets:insert(
        ?CIRCUIT_BREAKER_TABLE,
        {Pid, #{
            state => open,
            failures => 5,
            concurrent => 0,
            opened_at => Now
        }}
    ),
    ?assertEqual(open, get_circuit_state(Pid)),
    Pid ! done.

get_circuit_state_half_open_test() ->
    ensure_table(),
    Pid = spawn(fun() ->
        receive
            done -> ok
        end
    end),
    ets:delete_all_objects(?CIRCUIT_BREAKER_TABLE),
    OldTime = erlang:system_time(millisecond) - ?RECOVERY_TIMEOUT_MS - 1000,
    ets:insert(
        ?CIRCUIT_BREAKER_TABLE,
        {Pid, #{
            state => open,
            failures => 5,
            concurrent => 0,
            opened_at => OldTime
        }}
    ),
    ?assertEqual(half_open, get_circuit_state(Pid)),
    Pid ! done.

record_failure_opens_circuit_test() ->
    ensure_table(),
    Pid = spawn(fun() ->
        receive
            done -> ok
        end
    end),
    ets:delete_all_objects(?CIRCUIT_BREAKER_TABLE),
    ets:insert(
        ?CIRCUIT_BREAKER_TABLE,
        {Pid, #{
            state => closed,
            failures => ?FAILURE_THRESHOLD - 1,
            concurrent => 0
        }}
    ),
    record_failure(Pid),
    [{Pid, State}] = ets:lookup(?CIRCUIT_BREAKER_TABLE, Pid),
    ?assertEqual(open, maps:get(state, State)),
    Pid ! done.

record_failure_recreates_missing_table_test() ->
    catch ets:delete(?CIRCUIT_BREAKER_TABLE),
    Pid = spawn(fun() ->
        receive
            done -> ok
        end
    end),
    ?assertEqual(ok, record_failure(Pid)),
    [{Pid, State}] = ets:lookup(?CIRCUIT_BREAKER_TABLE, Pid),
    ?assertEqual(1, maps:get(failures, State)),
    Pid ! done.

is_success_result_test() ->
    ?assertEqual(true, is_success_result({ok, #{}})),
    ?assertEqual(false, is_success_result({error, timeout})),
    ?assertEqual(false, is_success_result({error, noproc})).

-endif.
