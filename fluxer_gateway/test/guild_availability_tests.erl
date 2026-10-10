%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_availability_tests).
-typing([eqwalizer]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec cleanup_unavailability_cache(integer()) -> ok.
cleanup_unavailability_cache(GuildId) ->
    guild_availability_cache:set_cached_unavailability_mode(GuildId, available).

disconnect_ineligible_sessions_staff_only_test() ->
    Parent = self(),
    GuildId = 99001,
    NonStaffPid = start_session_capture(non_staff, Parent),
    StaffPid = start_session_capture(staff, Parent),
    try
        BaseState = state_for_unavailability_transition_test(GuildId, NonStaffPid, StaffPid),
        NewState = BaseState#{
            data => #{
                <<"guild">> => #{<<"features">> => [<<"UNAVAILABLE_FOR_EVERYONE_BUT_STAFF">>]},
                <<"members">> => [
                    #{<<"user">> => #{<<"id">> => <<"1001">>, <<"flags">> => <<"0">>}},
                    #{<<"user">> => #{<<"id">> => <<"1002">>, <<"flags">> => <<"1">>}}
                ]
            }
        },
        UpdatedState = guild_availability:handle_unavailability_transition(BaseState, NewState),
        Sessions = maps:get(sessions, UpdatedState, #{}),
        ?assertEqual(1, map_size(Sessions)),
        ?assert(maps:is_key(<<"staff">>, Sessions)),
        ?assertEqual(
            unavailable_for_everyone_but_staff,
            guild_availability:get_cached_unavailability_mode(GuildId)
        ),
        receive
            {non_staff, {'$gen_cast', {guild_leave, GuildId, forced_unavailable}}} -> ok
        after 1000 -> ?assert(false)
        end,
        receive
            {staff, {'$gen_cast', {guild_leave, GuildId, forced_unavailable}}} -> ?assert(false)
        after 200 -> ok
        end
    after
        cleanup_unavailability_cache(GuildId),
        NonStaffPid ! stop,
        StaffPid ! stop
    end.

disconnect_ineligible_sessions_everyone_test() ->
    Parent = self(),
    GuildId = 99002,
    UserOnePid = start_session_capture(user_one, Parent),
    UserTwoPid = start_session_capture(user_two, Parent),
    try
        BaseState = state_for_unavailability_transition_test(GuildId, UserOnePid, UserTwoPid),
        NewState = BaseState#{
            data => #{
                <<"guild">> => #{<<"features">> => [<<"UNAVAILABLE_FOR_EVERYONE">>]},
                <<"members">> => [
                    #{<<"user">> => #{<<"id">> => <<"1001">>, <<"flags">> => <<"0">>}},
                    #{<<"user">> => #{<<"id">> => <<"1002">>, <<"flags">> => <<"1">>}}
                ]
            }
        },
        UpdatedState = guild_availability:handle_unavailability_transition(BaseState, NewState),
        ?assertEqual(#{}, maps:get(sessions, UpdatedState, #{})),
        ?assertEqual(
            unavailable_for_everyone, guild_availability:get_cached_unavailability_mode(GuildId)
        ),
        receive
            {user_one, {'$gen_cast', {guild_leave, GuildId, forced_unavailable}}} -> ok
        after 1000 -> ?assert(false)
        end,
        receive
            {user_two, {'$gen_cast', {guild_leave, GuildId, forced_unavailable}}} -> ok
        after 1000 -> ?assert(false)
        end
    after
        cleanup_unavailability_cache(GuildId),
        UserOnePid ! stop,
        UserTwoPid ! stop
    end.

disconnect_ineligible_sessions_everyone_hidden_test() ->
    Parent = self(),
    GuildId = 99004,
    NonStaffPid = start_session_capture(non_staff_hidden, Parent),
    StaffPid = start_session_capture(staff_hidden, Parent),
    try
        BaseState = state_for_unavailability_transition_test(GuildId, NonStaffPid, StaffPid),
        NewState = BaseState#{
            data => #{
                <<"guild">> => #{
                    <<"features">> => [<<"UNAVAILABLE_FOR_EVERYONE">>, <<"UNAVAILABLE_HIDDEN">>]
                },
                <<"members">> => [
                    #{<<"user">> => #{<<"id">> => <<"1001">>, <<"flags">> => <<"0">>}},
                    #{<<"user">> => #{<<"id">> => <<"1002">>, <<"flags">> => <<"1">>}}
                ]
            }
        },
        UpdatedState = guild_availability:handle_unavailability_transition(BaseState, NewState),
        ?assertEqual(#{}, maps:get(sessions, UpdatedState, #{})),
        ?assertEqual(
            unavailable_for_everyone, guild_availability:get_cached_unavailability_mode(GuildId)
        ),
        ?assertEqual(
            true, guild_availability:is_unavailable_hidden_enabled_from_cache(GuildId)
        ),
        receive
            {non_staff_hidden, {'$gen_cast', {guild_leave, GuildId, forced_unavailable, true}}} ->
                ok
        after 1000 -> ?assert(false)
        end,
        receive
            {staff_hidden, {'$gen_cast', {guild_leave, GuildId, forced_unavailable, true}}} ->
                ok
        after 1000 -> ?assert(false)
        end
    after
        cleanup_unavailability_cache(GuildId),
        NonStaffPid ! stop,
        StaffPid ! stop
    end.

is_guild_unavailable_for_user_from_cache_is_staff_test() ->
    GuildId = 99003,
    try
        guild_availability_cache:set_cached_unavailability_mode(
            GuildId, unavailable_for_everyone_but_staff
        ),
        ?assertEqual(
            true,
            guild_availability:is_guild_unavailable_for_user_from_cache(GuildId, #{
                <<"is_staff">> => false
            })
        ),
        ?assertEqual(
            false,
            guild_availability:is_guild_unavailable_for_user_from_cache(GuildId, #{
                <<"is_staff">> => true
            })
        )
    after
        cleanup_unavailability_cache(GuildId)
    end.

is_unavailable_hidden_enabled_from_cache_test() ->
    GuildId = 99005,
    HiddenUnavailableState = #{
        id => GuildId,
        data => #{
            <<"guild">> => #{
                <<"features">> => [<<"UNAVAILABLE_FOR_EVERYONE">>, <<"UNAVAILABLE_HIDDEN">>]
            }
        }
    },
    HiddenOnlyState = #{
        id => GuildId,
        data => #{<<"guild">> => #{<<"features">> => [<<"UNAVAILABLE_HIDDEN">>]}}
    },
    try
        _ = guild_availability:update_unavailability_cache_for_state(HiddenUnavailableState),
        ?assertEqual(
            true, guild_availability:is_unavailable_hidden_enabled(HiddenUnavailableState)
        ),
        ?assertEqual(
            true, guild_availability:is_unavailable_hidden_enabled_from_cache(GuildId)
        ),
        _ = guild_availability:update_unavailability_cache_for_state(HiddenOnlyState),
        ?assertEqual(false, guild_availability:is_unavailable_hidden_enabled(HiddenOnlyState)),
        ?assertEqual(
            false, guild_availability:is_unavailable_hidden_enabled_from_cache(GuildId)
        )
    after
        cleanup_unavailability_cache(GuildId)
    end.

is_guild_unavailable_for_user_unavailable_for_everyone_test() ->
    State = #{
        data => #{
            <<"guild">> => #{<<"features">> => [<<"UNAVAILABLE_FOR_EVERYONE">>]},
            <<"members">> => []
        }
    },
    ?assertEqual(true, guild_availability:is_guild_unavailable_for_user(123, State)).

-spec start_session_capture(atom(), pid()) -> pid().
start_session_capture(Tag, Parent) ->
    spawn(fun() -> session_capture_loop(Tag, Parent) end).

-spec session_capture_loop(atom(), pid()) -> ok.
session_capture_loop(Tag, Parent) ->
    receive
        stop ->
            ok;
        {'$gen_cast', Msg} ->
            Parent ! {Tag, {'$gen_cast', Msg}},
            session_capture_loop(Tag, Parent);
        _Other ->
            session_capture_loop(Tag, Parent)
    after infinity ->
        ok
    end.

-spec state_for_unavailability_transition_test(integer(), pid(), pid()) -> map().
state_for_unavailability_transition_test(GuildId, NonStaffPid, StaffPid) ->
    #{
        id => GuildId,
        sessions => #{
            <<"non_staff">> => #{
                session_id => <<"non_staff">>,
                user_id => 1001,
                pid => NonStaffPid,
                mref => make_ref(),
                active_guilds => sets:new(),
                user_roles => [],
                bot => false,
                is_staff => false
            },
            <<"staff">> => #{
                session_id => <<"staff">>,
                user_id => 1002,
                pid => StaffPid,
                mref => make_ref(),
                active_guilds => sets:new(),
                user_roles => [],
                bot => false,
                is_staff => true
            }
        },
        presence_subscriptions => #{},
        member_list_subscriptions => guild_member_list_subs:new(),
        member_subscriptions => guild_subscriptions:init_state(),
        connected_user_ids => sets:new(),
        user_session_counts => #{},
        data => #{
            <<"guild">> => #{<<"features">> => []},
            <<"members">> => [
                #{<<"user">> => #{<<"id">> => <<"1001">>, <<"flags">> => <<"0">>}},
                #{<<"user">> => #{<<"id">> => <<"1002">>, <<"flags">> => <<"1">>}}
            ]
        },
        voice_states => #{},
        pending_voice_connections => #{}
    }.

-endif.

guild_create_resend_carries_threads_only_for_viewers_test() ->
    Tab = guild_thread_store:new(),
    try
        Data = guild_data_index:normalize_map(#{
            <<"guild">> => #{<<"id">> => <<"77">>, <<"owner_id">> => <<"1">>},
            <<"roles">> => [#{<<"id">> => <<"77">>, <<"permissions">> => <<"1024">>}],
            <<"channels">> => [#{<<"id">> => <<"5">>, <<"type">> => 0}],
            <<"members">> => [#{<<"user">> => #{<<"id">> => <<"9">>}, <<"roles">> => []}],
            <<"emojis">> => [],
            <<"stickers">> => []
        }),
        State = #{
            id => 77,
            data => Data#{thread_store => Tab, thread_gate => #{active => true, version => 1}}
        },
        Viewer = #{user_id => 9, pid => self(), thread_viewer => true},
        ok = guild_availability:send_guild_create_to_session(Viewer, 77, State, []),
        ?assert(maps:is_key(<<"threads">>, received_guild_create())),
        ok = guild_availability:send_guild_create_to_session(
            Viewer#{thread_viewer => false}, 77, State, []
        ),
        ?assertNot(maps:is_key(<<"threads">>, received_guild_create()))
    after
        guild_thread_store:destroy(Tab)
    end.

received_guild_create() ->
    receive
        {'$gen_cast', {dispatch, guild_create, GuildState}} when is_map(GuildState) ->
            GuildState
    after 1000 -> erlang:error(guild_create_not_sent)
    end.
