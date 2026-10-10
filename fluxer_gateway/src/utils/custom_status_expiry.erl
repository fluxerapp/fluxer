%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(custom_status_expiry).
-typing([eqwalizer]).

-export([
    clear_if_expired/1,
    clear_if_expired/2,
    remaining_ms/2,
    next_wakeup_ms/1,
    reconcile_message/0
]).

-export_type([custom_status/0]).

-type custom_status() :: map() | null.
-type wakeup() :: {ok, pos_integer()} | none.
-type offset() :: {ok, integer()} | none.

-define(MAX_TIMER_MS, 86400000).
-define(WAKEUP_JITTER_MS, 5000).

-spec clear_if_expired(term()) -> custom_status().
clear_if_expired(CustomStatus) ->
    clear_if_expired(CustomStatus, erlang:system_time(millisecond)).

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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

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

clears_expired_and_arms_future_test() ->
    Live = future_status(),
    ?assertEqual(null, clear_if_expired(status(?LIVE_EXPIRES_AT))),
    ?assertEqual(Live, clear_if_expired(Live)),
    ?assertMatch({ok, _}, next_wakeup_ms(Live)).

future_status() ->
    status(rfc3339(erlang:system_time(millisecond) + 3600000)).

reconcile_message_is_a_gen_server_cast_test() ->
    ?assertEqual({'$gen_cast', reconcile_flattened_presence}, reconcile_message()).

-endif.
