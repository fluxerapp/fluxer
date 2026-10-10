%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_visibility).
-typing([eqwalizer]).

-export([
    get_user_viewable_channels/2,
    compute_and_dispatch_visibility_changes/2,
    compute_and_dispatch_visibility_changes_for_users/3,
    compute_and_dispatch_visibility_changes_for_channels/3,
    viewable_channel_set/2
]).

-export_type([guild_state/0, user_id/0, channel_id/0]).

-type guild_state() :: map().
-type user_id() :: integer().
-type channel_id() :: integer().

-spec get_user_viewable_channels(user_id(), guild_state()) -> [channel_id()].
get_user_viewable_channels(UserId, State) ->
    guild_visibility_channels:get_user_viewable_channels(UserId, State).

-spec viewable_channel_set(user_id(), guild_state()) -> sets:set(channel_id()).
viewable_channel_set(UserId, State) ->
    guild_visibility_channels:viewable_channel_set(UserId, State).

-spec compute_and_dispatch_visibility_changes(guild_state(), guild_state()) -> guild_state().
compute_and_dispatch_visibility_changes(OldState, NewState) ->
    guild_visibility_overwrites:compute_and_dispatch_visibility_changes(OldState, NewState).

-spec compute_and_dispatch_visibility_changes_for_users(
    [user_id()], guild_state(), guild_state()
) -> guild_state().
compute_and_dispatch_visibility_changes_for_users(UserIds, OldState, NewState) ->
    guild_visibility_overwrites:compute_and_dispatch_visibility_changes_for_users(
        UserIds, OldState, NewState
    ).

-spec compute_and_dispatch_visibility_changes_for_channels(
    [channel_id()], guild_state(), guild_state()
) -> guild_state().
compute_and_dispatch_visibility_changes_for_channels(ChannelIds, OldState, NewState) ->
    guild_visibility_overwrites:compute_and_dispatch_visibility_changes_for_channels(
        ChannelIds, OldState, NewState
    ).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_user_viewable_channels_returns_empty_for_non_member_test() ->
    State = #{
        data => #{
            <<"channels">> => [#{<<"id">> => <<"100">>, <<"type">> => 0}],
            <<"members">> => []
        }
    },
    ?assertEqual([], get_user_viewable_channels(999, State)).

viewable_channel_set_returns_empty_for_invalid_user_test() ->
    State = #{data => #{}},
    ?assertEqual(sets:new(), viewable_channel_set(0, State)).

-endif.
