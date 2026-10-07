%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_virtual_channel_access_tests).
-typing([eqwalizer]).
-include_lib("eunit/include/eunit.hrl").

add_virtual_access_test() ->
    State = #{},
    State1 = guild_virtual_channel_access:add_virtual_access(100, 500, State),
    ?assertEqual(true, guild_virtual_channel_access:has_virtual_access(100, 500, State1)),
    ?assertEqual(true, guild_virtual_channel_access:is_pending_join(100, 500, State1)).

add_virtual_access_updates_session_cache_test() ->
    State = #{
        sessions => #{
            <<"s1">> => #{user_id => 100, viewable_channels => #{}}
        }
    },
    State1 = guild_virtual_channel_access:add_virtual_access(100, 500, State),
    Session = maps:get(<<"s1">>, maps:get(sessions, State1)),
    ViewableChannels = maps:get(viewable_channels, Session, #{}),
    ?assertEqual(true, maps:is_key(500, ViewableChannels)).

remove_virtual_access_test() ->
    State = guild_virtual_channel_access:add_virtual_access(100, 500, #{}),
    State1 = guild_virtual_channel_access:remove_virtual_access(100, 500, State),
    ?assertEqual(false, guild_virtual_channel_access:has_virtual_access(100, 500, State1)).

remove_virtual_access_updates_session_cache_test() ->
    State = #{
        sessions => #{
            <<"s1">> => #{user_id => 100, viewable_channels => #{500 => true}}
        },
        virtual_channel_access => #{100 => sets:from_list([500])},
        virtual_channel_access_pending => #{100 => sets:from_list([500])},
        virtual_channel_access_preserve => #{100 => sets:new()},
        virtual_channel_access_move_pending => #{100 => sets:new()}
    },
    State1 = guild_virtual_channel_access:remove_virtual_access(100, 500, State),
    Session = maps:get(<<"s1">>, maps:get(sessions, State1)),
    ViewableChannels = maps:get(viewable_channels, Session, #{}),
    ?assertEqual(false, maps:is_key(500, ViewableChannels)).

get_virtual_channels_for_user_test() ->
    State = guild_virtual_channel_access:add_virtual_access(100, 500, #{}),
    State1 = guild_virtual_channel_access:add_virtual_access(100, 501, State),
    Channels = lists:sort(
        guild_virtual_channel_access:get_virtual_channels_for_user(100, State1)
    ),
    ?assertEqual([500, 501], Channels).

mark_and_clear_preserve_test() ->
    State = guild_virtual_channel_access:add_virtual_access(100, 500, #{}),
    State1 = guild_virtual_channel_access:mark_preserve(100, 500, State),
    ?assertEqual(true, guild_virtual_channel_access:has_preserve(100, 500, State1)),
    State2 = guild_virtual_channel_access:clear_preserve(100, 500, State1),
    ?assertEqual(false, guild_virtual_channel_access:has_preserve(100, 500, State2)).

mark_and_clear_move_pending_test() ->
    State = guild_virtual_channel_access:add_virtual_access(100, 500, #{}),
    State1 = guild_virtual_channel_access:mark_move_pending(100, 500, State),
    ?assertEqual(true, guild_virtual_channel_access:is_move_pending(100, 500, State1)),
    State2 = guild_virtual_channel_access:clear_move_pending(100, 500, State1),
    ?assertEqual(false, guild_virtual_channel_access:is_move_pending(100, 500, State2)).
