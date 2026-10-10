%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_dispatch_config).
-typing([eqwalizer]).

-export([
    is_member_list_updates_enabled/1,
    is_guild_operation_disabled/2,
    should_send_push_notifications/1
]).

-type guild_state() :: map().
-export_type([guild_state/0]).

-define(GUILD_DISABLED_OP_PUSH_NOTIFICATIONS, 1 bsl 0).
-define(GUILD_DISABLED_OP_MEMBER_LIST_UPDATES, 1 bsl 6).

-spec should_send_push_notifications(guild_state()) -> boolean().
should_send_push_notifications(State) ->
    not is_guild_operation_disabled(State, ?GUILD_DISABLED_OP_PUSH_NOTIFICATIONS).

-spec is_member_list_updates_enabled(guild_state()) -> boolean().
is_member_list_updates_enabled(State) ->
    case maps:get(disable_member_list_updates, State, false) of
        true -> false;
        false -> not is_guild_operation_disabled(State, ?GUILD_DISABLED_OP_MEMBER_LIST_UPDATES)
    end.

-spec is_guild_operation_disabled(guild_state(), integer()) -> boolean().
is_guild_operation_disabled(State, OperationMask) ->
    Data = maps:get(data, State, #{}),
    Guild = maps:get(<<"guild">>, Data, #{}),
    DisabledOperations = bitset:parse(maps:get(<<"disabled_operations">>, Guild, 0)),
    bitset:has(DisabledOperations, OperationMask).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec disabled_operations_state(integer() | binary()) -> guild_state().
disabled_operations_state(Value) ->
    #{data => #{<<"guild">> => #{<<"disabled_operations">> => Value}}}.

should_send_push_notifications_respects_flag_test() ->
    ?assertEqual(true, should_send_push_notifications(disabled_operations_state(0))),
    PushFlag = ?GUILD_DISABLED_OP_PUSH_NOTIFICATIONS,
    ?assertEqual(
        false,
        should_send_push_notifications(disabled_operations_state(PushFlag))
    ).

member_list_updates_enabled_respects_flag_test() ->
    ?assertEqual(true, is_member_list_updates_enabled(disabled_operations_state(<<"0">>))),
    DisabledState = #{
        disable_member_list_updates => true,
        data => #{<<"guild">> => #{}}
    },
    ?assertEqual(false, is_member_list_updates_enabled(DisabledState)),
    ?assertEqual(
        false,
        is_member_list_updates_enabled(
            disabled_operations_state(integer_to_binary(?GUILD_DISABLED_OP_MEMBER_LIST_UPDATES))
        )
    ).

-endif.
