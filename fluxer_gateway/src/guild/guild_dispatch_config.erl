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
