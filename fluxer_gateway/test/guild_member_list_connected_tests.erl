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

default_presence_returns_offline_test() ->
    P = guild_member_list_connected:default_presence(),
    ?assertEqual(<<"offline">>, maps:get(<<"status">>, P)),
    ?assertEqual(false, maps:get(<<"mobile">>, P)),
    ?assertEqual(false, maps:get(<<"afk">>, P)).

invalid_channel_id() ->
    eqwalizer:dynamic_cast(not_an_integer).

resolve_presence_missing_user_returns_default_test() ->
    State = #{member_presence => #{1 => #{<<"status">> => <<"online">>}}},
    P = guild_member_list_connected:resolve_presence_for_user(State, 999),
    ?assertEqual(<<"offline">>, maps:get(<<"status">>, P)).

resolve_presence_empty_presence_map_test() ->
    State = #{member_presence => #{}},
    P = guild_member_list_connected:resolve_presence_for_user(State, 1),
    ?assertEqual(<<"offline">>, maps:get(<<"status">>, P)).

resolve_presence_no_presence_key_test() ->
    State = #{},
    P = guild_member_list_connected:resolve_presence_for_user(State, 1),
    ?assertEqual(<<"offline">>, maps:get(<<"status">>, P)).

connected_session_user_ids_ignores_invalid_test() ->
    State = #{
        sessions => #{
            <<"s1">> => #{user_id => 10},
            <<"s2">> => #{user_id => 0},
            <<"s3">> => #{user_id => -1},
            <<"s4">> => #{},
            <<"s5">> => #{user_id => undefined}
        }
    },
    Ids = guild_member_list_connected:connected_session_user_ids(State),
    ?assertEqual(true, sets:is_element(10, Ids)),
    ?assertEqual(false, sets:is_element(0, Ids)),
    ?assertEqual(false, sets:is_element(-1, Ids)),
    ?assertEqual(1, sets:size(Ids)).
