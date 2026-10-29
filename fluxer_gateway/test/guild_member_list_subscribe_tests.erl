%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_member_list_subscribe_tests).
-typing([eqwalizer]).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(SESSION_ID, <<"session">>).
-define(LIST_ID, <<"0">>).

first_subscribe_syncs_requested_ranges_test() ->
    State = test_state(),
    ?assertMatch({_, true, [{0, 99}]}, subscribe([{0, 99}], State)).

repeated_subscribe_resyncs_requested_ranges_test() ->
    State = test_state(),
    {State1, true, [{0, 99}]} = subscribe([{0, 99}], State),
    ?assertMatch({_, true, [{0, 99}]}, subscribe([{0, 99}], State1)).

narrower_subscribe_does_not_sync_test() ->
    State = test_state(),
    {State1, true, [{0, 99}, {100, 199}]} = subscribe([{0, 99}, {100, 199}], State),
    ?assertMatch({_, false, []}, subscribe([{0, 99}], State1)).

wider_subscribe_syncs_only_new_ranges_test() ->
    State = test_state(),
    {State1, true, [{0, 99}]} = subscribe([{0, 99}], State),
    ?assertMatch({_, true, [{100, 199}]}, subscribe([{0, 99}, {100, 199}], State1)).

empty_subscribe_does_not_sync_test() ->
    State = test_state(),
    {State1, true, [{0, 99}]} = subscribe([{0, 99}], State),
    ?assertMatch({_, false, []}, subscribe([], State1)).

subscribe(Ranges, State) ->
    guild_member_list_subscribe:subscribe_ranges(?SESSION_ID, ?LIST_ID, Ranges, State).

test_state() ->
    #{member_list_subscriptions => guild_member_list_subs:new()}.

-endif.
