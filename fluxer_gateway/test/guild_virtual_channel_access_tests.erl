%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_virtual_channel_access_tests).
-typing([eqwalizer]).
-include_lib("eunit/include/eunit.hrl").

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
