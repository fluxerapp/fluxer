%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_sessions_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

filter_sessions_for_channel_uses_cached_viewable_channels_test() ->
    SessionId = <<"s1">>,
    SessionData = #{
        session_id => SessionId,
        user_id => 10,
        pid => self(),
        viewable_channels => #{200 => true}
    },
    Sessions = #{SessionId => SessionData},
    State = #{sessions => Sessions, data => #{<<"members">> => #{}}},
    Result = guild_sessions:filter_sessions_for_channel(Sessions, 200, undefined, State),
    ?assertEqual([{SessionId, SessionData}], Result).

filter_sessions_exclude_session_specific_test() ->
    Sessions = #{<<"s1">> => #{}, <<"s2">> => #{}},
    Result = guild_sessions:filter_sessions_exclude_session(Sessions, <<"s1">>),
    ?assertEqual(1, length(Result)),
    ?assertEqual([{<<"s2">>, #{}}], Result).

pending_connect_filtered_from_channel_sessions_test() ->
    NormalSession = #{
        session_id => <<"s1">>,
        user_id => 10,
        pid => self(),
        viewable_channels => #{200 => true}
    },
    PendingSession = #{
        session_id => <<"s2">>,
        user_id => 11,
        pid => self(),
        pending_connect => true,
        viewable_channels => #{200 => true}
    },
    Sessions = #{<<"s1">> => NormalSession, <<"s2">> => PendingSession},
    State = #{sessions => Sessions, data => #{<<"members">> => #{}}},
    Result = guild_sessions:filter_sessions_for_channel(Sessions, 200, undefined, State),
    ?assertEqual(1, length(Result)),
    [{ResultSid, _}] = Result,
    ?assertEqual(<<"s1">>, ResultSid).

refresh_user_session_cache_updates_roles_and_viewable_channels_test() ->
    UserId = 10,
    GuildId = 42,
    RoleId = 99,
    ViewPerm = constants:view_channel_permission(),
    State = #{
        id => GuildId,
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"999">>},
            <<"roles">> => [
                #{
                    <<"id">> => integer_to_binary(GuildId),
                    <<"permissions">> => integer_to_binary(ViewPerm)
                },
                #{
                    <<"id">> => integer_to_binary(RoleId),
                    <<"permissions">> => integer_to_binary(ViewPerm)
                }
            ],
            <<"channels">> => [
                #{<<"id">> => <<"100">>, <<"permission_overwrites">> => []}
            ],
            <<"members">> => #{
                UserId => #{
                    <<"user">> => #{<<"id">> => integer_to_binary(UserId)},
                    <<"roles">> => [integer_to_binary(RoleId)]
                }
            }
        },
        sessions => #{
            <<"s1">> => #{user_id => UserId, user_roles => [], viewable_channels => #{}},
            <<"s2">> => #{user_id => 20, user_roles => [1], viewable_channels => #{200 => true}}
        }
    },
    Result = guild_sessions:refresh_user_session_cache(UserId, State),
    S1 = maps:get(<<"s1">>, maps:get(sessions, Result)),
    S2 = maps:get(<<"s2">>, maps:get(sessions, Result)),
    ?assertEqual([RoleId], maps:get(user_roles, S1)),
    ?assert(maps:is_key(100, maps:get(viewable_channels, S1))),
    ?assertEqual([1], maps:get(user_roles, S2)),
    ?assertEqual(#{200 => true}, maps:get(viewable_channels, S2)).

filter_sessions_exclude_session_filters_pending_test() ->
    Sessions = #{
        <<"s1">> => #{pending_connect => true},
        <<"s2">> => #{},
        <<"s3">> => #{pending_connect => false}
    },
    Result = guild_sessions:filter_sessions_exclude_session(Sessions, undefined),
    ResultIds = lists:sort([Sid || {Sid, _} <- Result]),
    ?assertEqual([<<"s2">>, <<"s3">>], ResultIds).

filter_sessions_for_channel_excludes_specified_session_test() ->
    S1 = #{
        session_id => <<"s1">>,
        user_id => 10,
        pid => self(),
        viewable_channels => #{200 => true}
    },
    S2 = #{
        session_id => <<"s2">>,
        user_id => 11,
        pid => self(),
        viewable_channels => #{200 => true}
    },
    Sessions = #{<<"s1">> => S1, <<"s2">> => S2},
    State = #{sessions => Sessions, data => #{<<"members">> => #{}}},
    Result = guild_sessions:filter_sessions_for_channel(Sessions, 200, <<"s1">>, State),
    ?assertEqual(1, length(Result)),
    [{ResultSid, _}] = Result,
    ?assertEqual(<<"s2">>, ResultSid).

filter_sessions_for_channel_falls_back_to_permission_check_test() ->
    GuildId = 42,
    UserId = 10,
    ChannelId = 200,
    ViewPerm = constants:view_channel_permission(),
    SessionData = #{session_id => <<"s1">>, user_id => UserId, pid => self()},
    Sessions = #{<<"s1">> => SessionData},
    State = #{
        id => GuildId,
        sessions => Sessions,
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"999">>},
            <<"roles">> => [
                #{
                    <<"id">> => integer_to_binary(GuildId),
                    <<"permissions">> => integer_to_binary(ViewPerm)
                }
            ],
            <<"members">> => #{
                UserId => #{
                    <<"user">> => #{<<"id">> => integer_to_binary(UserId)},
                    <<"roles">> => []
                }
            },
            <<"channels">> => [
                #{
                    <<"id">> => integer_to_binary(ChannelId),
                    <<"permission_overwrites">> => []
                }
            ]
        }
    },
    Result = guild_sessions:filter_sessions_for_channel(Sessions, ChannelId, undefined, State),
    ?assertEqual(1, length(Result)).

filter_sessions_for_channel_no_member_returns_empty_test() ->
    SessionData = #{session_id => <<"s1">>, user_id => 999, pid => self()},
    Sessions = #{<<"s1">> => SessionData},
    State = #{
        sessions => Sessions,
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"888">>},
            <<"members">> => [],
            <<"roles">> => [],
            <<"channels">> => []
        }
    },
    Result = guild_sessions:filter_sessions_for_channel(Sessions, 200, undefined, State),
    ?assertEqual([], Result).

refresh_all_viewable_channels_populates_cache_test() ->
    SessionId = <<"s1">>,
    UserId = 10,
    GuildId = 42,
    ViewPerm = constants:view_channel_permission(),
    State = #{
        id => GuildId,
        sessions => #{
            SessionId => #{session_id => SessionId, user_id => UserId, pid => self()}
        },
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"999">>},
            <<"roles">> => [
                #{
                    <<"id">> => integer_to_binary(GuildId),
                    <<"permissions">> => integer_to_binary(ViewPerm)
                }
            ],
            <<"members">> => #{
                UserId => #{
                    <<"user">> => #{<<"id">> => integer_to_binary(UserId)},
                    <<"roles">> => []
                }
            },
            <<"channels">> => [
                #{<<"id">> => <<"200">>, <<"permission_overwrites">> => []}
            ]
        }
    },
    UpdatedState = guild_sessions:refresh_all_viewable_channels(State),
    UpdatedSessions = maps:get(sessions, UpdatedState, #{}),
    UpdatedSession = maps:get(SessionId, UpdatedSessions),
    ViewableChannels = maps:get(viewable_channels, UpdatedSession, #{}),
    ?assertEqual(true, maps:is_key(200, ViewableChannels)).
