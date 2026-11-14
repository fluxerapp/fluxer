%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(dm_voice_disconnect_tests).

-include_lib("eunit/include/eunit.hrl").

-define(CHANNEL_ID, 1539791936896897200).
-define(USER_ID, 1539791936896897201).
-define(PHONE_CONNECTION, <<"phone-connection">>).

disconnecting_another_device_kicks_it_from_the_media_room_test() ->
    Parent = self(),
    PhoneVoiceState = #{
        <<"user_id">> => integer_to_binary(?USER_ID),
        <<"channel_id">> => integer_to_binary(?CHANNEL_ID),
        <<"connection_id">> => ?PHONE_CONNECTION,
        <<"session_id">> => <<"phone-session">>
    },
    VoiceStates = #{?PHONE_CONNECTION => PhoneVoiceState},
    DesktopState = #{
        id => <<"desktop-session">>,
        user_id => ?USER_ID,
        session_pid => self(),
        channels => #{},
        dm_voice_states => VoiceStates,
        test_force_disconnect_fun => fun(GuildId, ChannelId, UserId, ConnectionId) ->
            Parent ! {force_disconnect, GuildId, ChannelId, UserId, ConnectionId},
            {ok, #{success => true}}
        end
    },
    {reply, Reply, NewState} = dm_voice_state:handle_dm_disconnect(
        ?PHONE_CONNECTION, ?USER_ID, VoiceStates, DesktopState
    ),
    ?assertEqual(#{success => true}, Reply),
    ?assertEqual(#{}, maps:get(dm_voice_states, NewState)),
    receive
        {force_disconnect, GuildId, ChannelId, UserId, ConnectionId} ->
            ?assertEqual(
                {null, ?CHANNEL_ID, ?USER_ID, ?PHONE_CONNECTION},
                {GuildId, ChannelId, UserId, ConnectionId}
            )
    after 1000 ->
        ?assert(false)
    end.
