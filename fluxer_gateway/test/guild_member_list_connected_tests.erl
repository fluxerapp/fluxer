%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_member_list_connected_tests).
-typing([eqwalizer]).
-include_lib("eunit/include/eunit.hrl").

session_can_view_channel_uses_cached_visibility_test() ->
    SessionData = #{user_id => 12, viewable_channels => #{500 => true}},
    State = #{data => #{<<"members">> => #{}}},
    ?assertEqual(
        true, guild_member_list_connected:session_can_view_channel(SessionData, 500, State)
    ).

session_can_view_channel_rejects_when_cache_misses_and_user_missing_test() ->
    SessionData = #{user_id => 99, viewable_channels => #{}},
    State = #{data => #{<<"members">> => #{}}},
    ?assertEqual(
        false, guild_member_list_connected:session_can_view_channel(SessionData, 500, State)
    ).

session_can_view_channel_non_integer_channel_test() ->
    SessionData = #{user_id => 1, viewable_channels => #{}},
    State = #{data => #{<<"members">> => #{}}},
    ?assertEqual(
        false,
        guild_member_list_connected:session_can_view_channel(
            SessionData, invalid_channel_id(), State
        )
    ).

session_can_view_channel_zero_channel_test() ->
    SessionData = #{user_id => 1, viewable_channels => #{}},
    State = #{data => #{<<"members">> => #{}}},
    ?assertEqual(
        false, guild_member_list_connected:session_can_view_channel(SessionData, 0, State)
    ).

session_can_view_channel_negative_channel_test() ->
    SessionData = #{user_id => 1, viewable_channels => #{}},
    State = #{data => #{<<"members">> => #{}}},
    ?assertEqual(
        false, guild_member_list_connected:session_can_view_channel(SessionData, -5, State)
    ).

session_can_view_channel_no_user_id_test() ->
    SessionData = #{},
    State = #{data => #{<<"members">> => #{}}},
    ?assertEqual(
        false, guild_member_list_connected:session_can_view_channel(SessionData, 500, State)
    ).

session_can_view_channel_no_viewable_channels_map_test() ->
    SessionData = #{user_id => 1},
    State = #{data => #{<<"members">> => #{}}},
    ?assertEqual(
        false, guild_member_list_connected:session_can_view_channel(SessionData, 500, State)
    ).

invalid_channel_id() ->
    eqwalizer:dynamic_cast(not_an_integer).
