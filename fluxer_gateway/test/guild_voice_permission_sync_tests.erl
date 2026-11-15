%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_permission_sync_tests).

-include_lib("eunit/include/eunit.hrl").

-define(GUILD_ID, 100).
-define(OWNER_ID, 200).
-define(USER_ID, 300).
-define(CHANNEL_ID, 400).
-define(CONNECTION_ID, <<"conn-a">>).

timed_out_member_is_disconnected_test() ->
    Until = iso8601_from_now_ms(60000),
    Result = sync_member_with_timeout(Until),
    ?assertEqual(#{disconnected => true}, Result).

expired_timeout_keeps_member_connected_test() ->
    Until = iso8601_from_now_ms(-60000),
    Result = sync_member_with_timeout(Until),
    ?assertNot(maps:is_key(disconnected, Result)),
    ?assertEqual(true, maps:get(can_speak, Result)).

sync_member_with_timeout(Until) ->
    Parent = self(),
    Member = #{
        <<"user">> => #{<<"id">> => integer_to_binary(?USER_ID)},
        <<"roles">> => [],
        <<"communication_disabled_until">> => Until
    },
    State = guild_state(Member, fun(_GuildId, _ChannelId, _UserId, _ConnId, Payload) ->
        Parent ! {synced, Payload}
    end),
    ok = guild_voice_permission_sync:maybe_sync_permissions_on_member_update(Member, State),
    receive
        {synced, Payload} -> Payload
    after 1000 -> error(no_sync)
    end.

guild_state(Member, SyncFun) ->
    Perms =
        constants:view_channel_permission() bor constants:connect_permission() bor
            constants:speak_permission() bor constants:stream_permission(),
    #{
        id => ?GUILD_ID,
        test_permission_sync_fun => SyncFun,
        data => #{
            <<"guild">> => #{
                <<"id">> => integer_to_binary(?GUILD_ID),
                <<"owner_id">> => integer_to_binary(?OWNER_ID)
            },
            <<"roles">> => [
                #{
                    <<"id">> => integer_to_binary(?GUILD_ID),
                    <<"permissions">> => integer_to_binary(Perms),
                    <<"position">> => 0
                }
            ],
            <<"channels">> => [
                #{
                    <<"id">> => integer_to_binary(?CHANNEL_ID),
                    <<"type">> => 2,
                    <<"permission_overwrites">> => []
                }
            ],
            <<"members">> => [Member]
        },
        voice_states => #{
            ?CONNECTION_ID => #{
                <<"user_id">> => integer_to_binary(?USER_ID),
                <<"channel_id">> => integer_to_binary(?CHANNEL_ID),
                <<"connection_id">> => ?CONNECTION_ID
            }
        }
    }.

iso8601_from_now_ms(OffsetMs) ->
    Ms = erlang:system_time(millisecond) + OffsetMs,
    list_to_binary(calendar:system_time_to_rfc3339(Ms, [{unit, millisecond}, {offset, "Z"}])).
