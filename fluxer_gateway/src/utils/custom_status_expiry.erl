%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(custom_status_expiry).
-typing([eqwalizer]).

-export([
    enabled/0,
    clear_if_expired/1,
    clear_if_expired/2,
    remaining_ms/2,
    next_wakeup_ms/1,
    reconcile_message/0,
    repair/1
]).

-export_type([custom_status/0]).

-type custom_status() :: map() | null.
-type user_id() :: integer().
-type wakeup() :: {ok, pos_integer()} | none.
-type offset() :: {ok, integer()} | none.

-define(ENABLED_KEY, custom_status_expiry_enabled).
-define(MAX_REPAIR_WINDOW_SECONDS, 86400).
-define(MAX_TIMER_MS, 86400000).
-define(WAKEUP_JITTER_MS, 5000).

-spec enabled() -> boolean().
enabled() ->
    application:get_env(fluxer_gateway, ?ENABLED_KEY, false) =:= true.

-spec clear_if_expired(term()) -> custom_status().
clear_if_expired(CustomStatus) ->
    clear_when_enabled(enabled(), CustomStatus).

-spec clear_when_enabled(boolean(), term()) -> custom_status().
clear_when_enabled(true, CustomStatus) ->
    clear_if_expired(CustomStatus, erlang:system_time(millisecond));
clear_when_enabled(false, CustomStatus) when is_map(CustomStatus) ->
    CustomStatus;
clear_when_enabled(false, _CustomStatus) ->
    null.

-spec clear_if_expired(term(), term()) -> custom_status().
clear_if_expired(CustomStatus, NowMs) when is_map(CustomStatus), is_integer(NowMs) ->
    keep_or_clear(expired(CustomStatus, NowMs), CustomStatus);
clear_if_expired(CustomStatus, _NowMs) when is_map(CustomStatus) ->
    CustomStatus;
clear_if_expired(_CustomStatus, _NowMs) ->
    null.

-spec keep_or_clear(boolean(), map()) -> custom_status().
keep_or_clear(true, _CustomStatus) -> null;
keep_or_clear(false, CustomStatus) -> CustomStatus.

-spec expired(map(), integer()) -> boolean().
expired(CustomStatus, NowMs) ->
    case remaining_ms(CustomStatus, NowMs) of
        {ok, RemainingMs} -> RemainingMs =< 0;
        none -> false
    end.

-spec remaining_ms(term(), term()) -> offset().
remaining_ms(CustomStatus, NowMs) when is_map(CustomStatus), is_integer(NowMs) ->
    offset_from(expires_at_ms(maps:get(<<"expires_at">>, CustomStatus, undefined)), NowMs);
remaining_ms(_CustomStatus, _NowMs) ->
    none.

-spec offset_from(offset(), integer()) -> offset().
offset_from({ok, ExpiryMs}, NowMs) -> {ok, ExpiryMs - NowMs};
offset_from(none, _NowMs) -> none.

-spec expires_at_ms(term()) -> offset().
expires_at_ms(Value) when is_binary(Value) ->
    parse_rfc3339(binary_to_list(Value));
expires_at_ms(Value) when is_list(Value) ->
    parse_rfc3339(Value);
expires_at_ms(_Value) ->
    none.

-spec parse_rfc3339(term()) -> offset().
parse_rfc3339(Chars) ->
    try calendar:rfc3339_to_system_time(Chars, [{unit, millisecond}]) of
        Ms -> {ok, Ms}
    catch
        _Class:_Reason -> none
    end.

-spec next_wakeup_ms(term()) -> wakeup().
next_wakeup_ms(CustomStatus) ->
    next_wakeup_ms(enabled(), CustomStatus).

-spec next_wakeup_ms(boolean(), term()) -> wakeup().
next_wakeup_ms(false, _CustomStatus) ->
    none;
next_wakeup_ms(true, CustomStatus) ->
    wakeup_delay(remaining_ms(CustomStatus, erlang:system_time(millisecond))).

-spec wakeup_delay(offset()) -> wakeup().
wakeup_delay(none) ->
    none;
wakeup_delay({ok, RemainingMs}) when RemainingMs =< 0 ->
    none;
wakeup_delay({ok, RemainingMs}) when RemainingMs > ?MAX_TIMER_MS ->
    {ok, jittered(?MAX_TIMER_MS)};
wakeup_delay({ok, RemainingMs}) ->
    {ok, jittered(RemainingMs)}.

-spec jittered(pos_integer()) -> pos_integer().
jittered(DelayMs) ->
    DelayMs + rand:uniform(?WAKEUP_JITTER_MS).

-spec reconcile_message() -> {'$gen_cast', reconcile_flattened_presence}.
reconcile_message() ->
    {'$gen_cast', reconcile_flattened_presence}.

-spec repair(term()) -> map().
repair(WindowSeconds) when
    is_integer(WindowSeconds), WindowSeconds >= 0, WindowSeconds =< ?MAX_REPAIR_WINDOW_SECONDS
->
    Considered = local_presence_user_ids(),
    Pids = expiring_presence_pids(Considered),
    Total = length(Pids),
    ok = schedule_reconciles(Pids, Total, WindowSeconds * 1000),
    #{
        scheduled => Total,
        considered => length(Considered),
        window_seconds => WindowSeconds,
        enabled => enabled()
    };
repair(_WindowSeconds) ->
    #{error => invalid_window_seconds}.

%% Only presences whose cached payload actually carries an expires_at are worth
%% nudging. Nudging every local presence would cost one payload rebuild and one
%% replicated cache write each, thousands per node, to correct a few dozen.
%% Filtering on "has an expiry" rather than "is expired" is deliberate: it also
%% arms the ongoing wakeup for statuses that have not expired yet.
-spec expiring_presence_pids([integer()]) -> [pid()].
expiring_presence_pids(UserIds) ->
    [
        Pid
     || UserId <- UserIds,
        has_expires_at(UserId),
        Pid <- [presence_pid(UserId)],
        is_pid(Pid)
    ].

-spec has_expires_at(integer()) -> boolean().
has_expires_at(UserId) ->
    case catch presence_cache:get(UserId) of
        {ok, Presence} when is_map(Presence) ->
            presence_carries_expiry(Presence);
        _ ->
            false
    end.

%% presence_cache:get/1 returns {ok, map()} | not_found, never a bare map, and
%% expires_at_ms/1 takes the timestamp value rather than the custom_status map.
-spec presence_carries_expiry(map()) -> boolean().
presence_carries_expiry(Presence) ->
    case maps:get(<<"custom_status">>, Presence, null) of
        CustomStatus when is_map(CustomStatus) ->
            expires_at_ms(maps:get(<<"expires_at">>, CustomStatus, null)) =/= none;
        _ ->
            false
    end.

-spec local_presence_user_ids() -> [user_id()].
local_presence_user_ids() ->
    try presence_manager_shards:determine_count() of
        {ShardCount, _Source} -> shard_user_ids(ShardCount)
    catch
        _Class:_Reason -> []
    end.

-spec shard_user_ids(pos_integer()) -> [user_id()].
shard_user_ids(ShardCount) ->
    lists:append([
        presence_manager:get_shard_user_ids(Index)
     || Index <- lists:seq(0, ShardCount - 1)
    ]).

-spec presence_pid(user_id()) -> pid() | undefined.
presence_pid(UserId) ->
    process_registry:registry_whereis(process_registry:build_process_key(presence, UserId)).

-spec schedule_reconciles([pid()], non_neg_integer(), non_neg_integer()) -> ok.
schedule_reconciles(Pids, Total, WindowMs) ->
    Message = reconcile_message(),
    _ = lists:foldl(
        fun(Pid, Index) ->
            _ = schedule_one(Pid, spread_ms(Index, Total, WindowMs), Message),
            Index + 1
        end,
        0,
        Pids
    ),
    ok.

%% A single unreachable destination must not abort the sweep and leave the
%% operator unable to tell how many nudges were actually scheduled.
-spec schedule_one(pid(), non_neg_integer(), term()) -> ok.
schedule_one(Pid, Delay, Message) ->
    try erlang:send_after(Delay, Pid, Message) of
        _Ref -> ok
    catch
        _Class:_Reason -> ok
    end.

-spec spread_ms(non_neg_integer(), non_neg_integer(), non_neg_integer()) -> non_neg_integer().
spread_ms(_Index, 0, _WindowMs) -> 0;
spread_ms(_Index, _Total, 0) -> 0;
spread_ms(Index, Total, WindowMs) -> (Index * WindowMs) div Total.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

presence_carries_expiry_detects_an_expiry_test() ->
    ?assert(
        presence_carries_expiry(#{
            <<"custom_status">> => #{<<"expires_at">> => <<"2026-05-13T13:02:27.497Z">>}
        })
    ),
    ?assertNot(presence_carries_expiry(#{<<"custom_status">> => #{<<"text">> => <<"hi">>}})),
    ?assertNot(presence_carries_expiry(#{<<"custom_status">> => null})),
    ?assertNot(presence_carries_expiry(#{})).

-define(LIVE_EXPIRES_AT, <<"2026-05-13T13:02:27.497Z">>).

epoch_ms(Iso) ->
    calendar:rfc3339_to_system_time(binary_to_list(Iso), [{unit, millisecond}]).

rfc3339(EpochMs) ->
    list_to_binary(
        calendar:system_time_to_rfc3339(EpochMs, [{unit, millisecond}, {offset, "Z"}])
    ).

expiry_ms() ->
    epoch_ms(?LIVE_EXPIRES_AT).

status(ExpiresAt) ->
    #{
        <<"emoji_animated">> => false,
        <<"emoji_name">> => null,
        <<"expires_at">> => ExpiresAt,
        <<"text">> => <<"brb">>
    }.

status_without_expiry() ->
    #{<<"emoji_animated">> => false, <<"emoji_name">> => null, <<"text">> => <<"brb">>}.

parser_anchors_on_the_unix_epoch_test() ->
    ?assertEqual(0, epoch_ms(<<"1970-01-01T00:00:00.000Z">>)),
    ?assertEqual(1500, epoch_ms(<<"1970-01-01T00:00:01.500Z">>)).

parser_keeps_milliseconds_test() ->
    ?assertEqual(497, expiry_ms() rem 1000).

not_expired_test() ->
    Status = status(?LIVE_EXPIRES_AT),
    ?assertEqual(Status, clear_if_expired(Status, expiry_ms() - 60000)).

expires_one_second_in_the_future_test() ->
    Status = status(?LIVE_EXPIRES_AT),
    ?assertEqual({ok, 1000}, remaining_ms(Status, expiry_ms() - 1000)),
    ?assertEqual(Status, clear_if_expired(Status, expiry_ms() - 1000)).

boundary_is_expired_test() ->
    Status = status(?LIVE_EXPIRES_AT),
    ?assertEqual({ok, 0}, remaining_ms(Status, expiry_ms())),
    ?assertEqual(null, clear_if_expired(Status, expiry_ms())).

one_millisecond_before_boundary_is_kept_test() ->
    Status = status(?LIVE_EXPIRES_AT),
    ?assertEqual(Status, clear_if_expired(Status, expiry_ms() - 1)).

expired_test() ->
    Status = status(?LIVE_EXPIRES_AT),
    ?assertEqual(null, clear_if_expired(Status, expiry_ms() + 1)),
    ?assertEqual(null, clear_if_expired(Status, expiry_ms() + 194 * 86400000)).

absent_expires_at_is_kept_test() ->
    Status = status_without_expiry(),
    ?assertEqual(none, remaining_ms(Status, expiry_ms())),
    ?assertEqual(Status, clear_if_expired(Status, expiry_ms())).

null_expires_at_is_kept_test() ->
    Status = status(null),
    ?assertEqual(none, remaining_ms(Status, expiry_ms())),
    ?assertEqual(Status, clear_if_expired(Status, expiry_ms())).

malformed_expires_at_is_kept_test() ->
    lists:foreach(
        fun(Value) ->
            Status = status(Value),
            ?assertEqual(none, remaining_ms(Status, expiry_ms())),
            ?assertEqual(Status, clear_if_expired(Status, expiry_ms()))
        end,
        [<<"">>, <<"not-a-date">>, <<"2026-13-45T99:99:99Z">>, 1747141347, [], #{}, true]
    ).

custom_status_null_test() ->
    ?assertEqual(none, remaining_ms(null, expiry_ms())),
    ?assertEqual(null, clear_if_expired(null, expiry_ms())).

custom_status_absent_test() ->
    ?assertEqual(none, remaining_ms(undefined, expiry_ms())),
    ?assertEqual(null, clear_if_expired(undefined, expiry_ms())).

non_map_custom_status_test() ->
    ?assertEqual(null, clear_if_expired(<<"garbage">>, expiry_ms())),
    ?assertEqual(null, clear_if_expired(123, expiry_ms())).

non_integer_reference_time_keeps_status_test() ->
    Status = status(?LIVE_EXPIRES_AT),
    ?assertEqual(Status, clear_if_expired(Status, not_a_time)).

wakeup_none_without_expiry_test() ->
    ?assertEqual(none, wakeup_delay(remaining_ms(status_without_expiry(), expiry_ms()))),
    ?assertEqual(none, wakeup_delay(remaining_ms(null, expiry_ms()))).

wakeup_none_when_already_expired_test() ->
    ?assertEqual(none, wakeup_delay({ok, 0})),
    ?assertEqual(none, wakeup_delay({ok, -1000})).

wakeup_is_never_early_test() ->
    {ok, DelayMs} = wakeup_delay({ok, 1000}),
    ?assert(DelayMs > 1000),
    ?assert(DelayMs =< 1000 + ?WAKEUP_JITTER_MS).

wakeup_clamps_to_the_timer_limit_test() ->
    {ok, DelayMs} = wakeup_delay({ok, 400 * ?MAX_TIMER_MS}),
    ?assert(DelayMs > ?MAX_TIMER_MS),
    ?assert(DelayMs =< ?MAX_TIMER_MS + ?WAKEUP_JITTER_MS).

disabled_by_default_test() ->
    application:unset_env(fluxer_gateway, ?ENABLED_KEY),
    Status = status(?LIVE_EXPIRES_AT),
    ?assertEqual(false, enabled()),
    ?assertEqual(Status, clear_if_expired(Status)),
    ?assertEqual(none, next_wakeup_ms(future_status())).

enabled_clears_expired_and_arms_future_test() ->
    Live = future_status(),
    with_expiry_enabled(fun() ->
        ?assertEqual(true, enabled()),
        ?assertEqual(null, clear_if_expired(status(?LIVE_EXPIRES_AT))),
        ?assertEqual(Live, clear_if_expired(Live)),
        ?assertMatch({ok, _}, next_wakeup_ms(Live))
    end).

future_status() ->
    status(rfc3339(erlang:system_time(millisecond) + 3600000)).

reconcile_message_is_a_gen_server_cast_test() ->
    ?assertEqual({'$gen_cast', reconcile_flattened_presence}, reconcile_message()).

spread_ms_test() ->
    ?assertEqual(0, spread_ms(0, 0, 60000)),
    ?assertEqual(0, spread_ms(5, 10, 0)),
    ?assertEqual(0, spread_ms(0, 10, 60000)),
    ?assertEqual(30000, spread_ms(5, 10, 60000)),
    ?assertEqual(54000, spread_ms(9, 10, 60000)).

repair_rejects_a_bad_window_test() ->
    ?assertEqual(#{error => invalid_window_seconds}, repair(-1)),
    ?assertEqual(#{error => invalid_window_seconds}, repair(<<"900">>)).

repair_reports_what_it_scheduled_test() ->
    Result = repair(0),
    ?assert(is_integer(maps:get(scheduled, Result))),
    ?assert(maps:get(scheduled, Result) >= 0),
    ?assertEqual(0, maps:get(window_seconds, Result)),
    ?assertEqual(enabled(), maps:get(enabled, Result)).

schedule_reconciles_delivers_the_cast_test() ->
    flush_mailbox(),
    ok = schedule_reconciles([self()], 1, 0),
    receive
        Message -> ?assertEqual(reconcile_message(), Message)
    after 1000 -> ?assert(false)
    end.

flush_mailbox() ->
    receive
        _Any -> flush_mailbox()
    after 0 -> ok
    end.

with_expiry_enabled(Fun) ->
    Previous = application:get_env(fluxer_gateway, ?ENABLED_KEY),
    application:set_env(fluxer_gateway, ?ENABLED_KEY, true),
    try
        Fun()
    after
        restore_expiry_env(Previous)
    end.

restore_expiry_env(undefined) ->
    application:unset_env(fluxer_gateway, ?ENABLED_KEY);
restore_expiry_env({ok, Value}) ->
    application:set_env(fluxer_gateway, ?ENABLED_KEY, Value).

-endif.
