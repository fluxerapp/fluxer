%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_state).
-typing([eqwalizer]).

-export([update_state/3]).

-type guild_state() :: map().
-type guild_data() :: map().
-type event() :: atom().
-type event_data() :: map().
-type user_id() :: integer().

-export_type([guild_state/0, event/0, event_data/0]).

-spec update_state(event(), event_data(), guild_state()) -> guild_state().
update_state(Event, EventData, State) ->
    StateWithUpdatedUser0 = guild_user_data:maybe_update_cached_user_data(
        Event, EventData, State
    ),
    Data0 = maps:get(data, StateWithUpdatedUser0),
    Data = ensure_normalized(Data0),
    StateWithUpdatedUser = StateWithUpdatedUser0#{data => Data},
    UpdatedData = update_data_for_event(Event, EventData, Data, State),
    UpdatedState = StateWithUpdatedUser#{data => UpdatedData},
    handle_post_update(Event, EventData, StateWithUpdatedUser, UpdatedState).

-spec ensure_normalized(guild_data()) -> guild_data().
ensure_normalized(Data) ->
    case maps:is_key(members_normalized, Data) of
        true -> Data;
        false -> guild_data_index:normalize_map(Data)
    end.

-spec update_data_for_event(event(), event_data(), guild_data(), guild_state()) -> guild_data().
update_data_for_event(Event, EventData, Data, State) ->
    case update_member_event(Event, EventData, Data, State) of
        {handled, Result} -> Result;
        unhandled -> update_other_event(Event, EventData, Data)
    end.

-spec update_member_event(event(), event_data(), guild_data(), guild_state()) ->
    {handled, guild_data()} | unhandled.
update_member_event(guild_member_add, EventData, Data, _State) ->
    {handled, guild_state_member:handle_member_add(EventData, Data)};
update_member_event(guild_member_update, EventData, Data, _State) ->
    {handled, guild_state_member:handle_member_update(EventData, Data)};
update_member_event(guild_member_remove, EventData, Data, State) ->
    {handled, guild_state_member:handle_member_remove(EventData, Data, State)};
update_member_event(_Event, _EventData, _Data, _State) ->
    unhandled.

-spec update_other_event(event(), event_data(), guild_data()) -> guild_data().
update_other_event(guild_update, ED, D) ->
    guild_state_channels:handle_guild_update(ED, D);
update_other_event(guild_role_create, ED, D) ->
    guild_state_roles:handle_role_create(ED, D);
update_other_event(guild_role_update, ED, D) ->
    guild_state_roles:handle_role_update(ED, D);
update_other_event(guild_role_update_bulk, ED, D) ->
    guild_state_roles:handle_role_update_bulk(ED, D);
update_other_event(guild_role_delete, ED, D) ->
    guild_state_roles:handle_role_delete(ED, D);
update_other_event(Event, ED, D) ->
    update_channel_event(Event, ED, D).

-spec update_channel_event(event(), event_data(), guild_data()) -> guild_data().
update_channel_event(channel_create, ED, D) ->
    guild_state_channels:handle_channel_create(ED, D);
update_channel_event(channel_update, ED, D) ->
    guild_state_channels:handle_channel_update(ED, D);
update_channel_event(channel_update_bulk, ED, D) ->
    guild_state_channels:handle_channel_update_bulk(ED, D);
update_channel_event(channel_delete, ED, D) ->
    guild_state_channels:handle_channel_delete(ED, D);
update_channel_event(message_create, ED, D) ->
    guild_state_channels:handle_message_create(ED, D);
update_channel_event(channel_pins_update, ED, D) ->
    guild_state_channels:handle_channel_pins_update(ED, D);
update_channel_event(guild_emojis_update, ED, D) ->
    guild_state_channels:handle_emojis_update(ED, D);
update_channel_event(guild_stickers_update, ED, D) ->
    guild_state_channels:handle_stickers_update(ED, D);
update_channel_event(_Event, _EventData, Data) ->
    Data.

-spec handle_post_update(event(), event_data(), guild_state(), guild_state()) -> guild_state().
handle_post_update(guild_update, _EventData, OldState, NewState) ->
    maybe_sync_member_list_permission_state(NewState),
    guild_availability:handle_unavailability_transition(OldState, NewState);
handle_post_update(guild_member_add, EventData, _OldState, NewState) ->
    guild_state_member:sync_member(EventData, NewState),
    UserId = guild_state_member:extract_user_id(EventData),
    refresh_member_session_cache(UserId, guild:update_counts(NewState));
handle_post_update(guild_member_update, EventData, OldState, NewState) ->
    guild_state_member:sync_member(EventData, NewState),
    UserId = guild_state_member:extract_user_id(EventData),
    State1 = dispatch_member_update_visibility(UserId, OldState, NewState),
    ok = guild_voice_permission_sync:maybe_sync_permissions_on_member_update(EventData, State1),
    refresh_member_session_cache(UserId, State1);
handle_post_update(Event, EventData, OldState, NewState) when
    Event =:= guild_role_create;
    Event =:= guild_role_update;
    Event =:= guild_role_update_bulk;
    Event =:= guild_role_delete
->
    post_update_role(Event, EventData, OldState, NewState);
handle_post_update(Event, EventData, OldState, NewState) when
    Event =:= channel_create;
    Event =:= channel_update;
    Event =:= channel_update_bulk;
    Event =:= channel_delete
->
    post_update_channel(Event, EventData, OldState, NewState);
handle_post_update(guild_member_remove, EventData, _OldState, NewState) ->
    post_update_member_remove(EventData, NewState);
handle_post_update(Event, _EventData, OldState, NewState) ->
    post_update_fallback(Event, OldState, NewState).

-spec post_update_role(event(), event_data(), guild_state(), guild_state()) -> guild_state().
post_update_role(guild_role_create, _EventData, _OldState, NewState) ->
    guild_state_roles:sync_hoisted_roles(NewState);
post_update_role(guild_role_update, EventData, OldState, NewState) ->
    NewState1 = guild_state_roles:sync_hoisted_roles(NewState),
    RoleIds = guild_state_roles:extract_role_ids_from_role_update(EventData),
    resync_roles_after_permission_change(RoleIds, OldState, NewState1);
post_update_role(guild_role_update_bulk, EventData, OldState, NewState) ->
    NewState1 = guild_state_roles:sync_hoisted_roles(NewState),
    RoleIds = guild_state_roles:extract_role_ids_from_role_update_bulk(EventData),
    resync_roles_after_permission_change(RoleIds, OldState, NewState1);
post_update_role(guild_role_delete, EventData, OldState, NewState) ->
    NewState1 = guild_state_roles:sync_hoisted_roles(NewState),
    RoleIds = guild_state_roles:extract_role_ids_from_role_delete(EventData),
    resync_roles_after_permission_change(RoleIds, OldState, NewState1).

-spec resync_roles_after_permission_change([integer()], guild_state(), guild_state()) ->
    guild_state().
resync_roles_after_permission_change(RoleIds, OldState, NewState) ->
    Recomputed = guild_state_roles:recompute_visibility_for_roles(RoleIds, OldState, NewState),
    lists:foreach(
        fun(RoleId) -> guild_voice_permission_sync:sync_users_with_role(RoleId, Recomputed) end,
        RoleIds
    ),
    Recomputed.

-spec post_update_channel(event(), event_data(), guild_state(), guild_state()) -> guild_state().
post_update_channel(channel_create, _EventData, _OldState, NewState) ->
    maybe_sync_member_list_permission_state(NewState),
    NewState;
post_update_channel(channel_update, EventData, OldState, NewState) ->
    ChanIds = guild_state_channels:extract_channel_ids_from_channel_update(EventData),
    resync_channels_after_permission_change(ChanIds, OldState, NewState);
post_update_channel(channel_update_bulk, EventData, OldState, NewState) ->
    ChanIds = guild_state_channels:extract_channel_ids_from_channel_update_bulk(EventData),
    resync_channels_after_permission_change(ChanIds, OldState, NewState);
post_update_channel(channel_delete, _EventData, _OldState, NewState) ->
    maybe_sync_member_list_permission_state(NewState),
    NewState.

-spec resync_channels_after_permission_change([integer()], guild_state(), guild_state()) ->
    guild_state().
resync_channels_after_permission_change(ChanIds, OldState, NewState) ->
    Rebuilt = guild_member_list_write:rebuild_channels_for_permission_change(ChanIds, NewState),
    Dispatched = guild_visibility:compute_and_dispatch_visibility_changes_for_channels(
        ChanIds, OldState, Rebuilt
    ),
    lists:foreach(
        fun(ChanId) ->
            guild_voice_permission_sync:sync_all_voice_permissions_for_channel(
                ChanId, Dispatched
            )
        end,
        ChanIds
    ),
    Dispatched.

-spec post_update_member_remove(event_data(), guild_state()) -> guild_state().
post_update_member_remove(EventData, NewState) ->
    UserId = guild_state_member:extract_user_id(EventData),
    guild_state_member:sync_member_remove(UserId, NewState),
    State1 = guild_state_member:cleanup_removed_member_sessions(UserId, NewState),
    State2 = guild_state_member:maybe_disconnect_removed_member(UserId, State1),
    guild:update_counts(State2).

-spec post_update_fallback(event(), guild_state(), guild_state()) -> guild_state().
post_update_fallback(Event, OldState, NewState) ->
    case guild_state_utils:needs_visibility_check(Event) of
        true ->
            guild_visibility:compute_and_dispatch_visibility_changes(OldState, NewState);
        false ->
            NewState
    end.

-spec dispatch_member_update_visibility(user_id() | undefined, guild_state(), guild_state()) ->
    guild_state().
dispatch_member_update_visibility(UserId, StateWithUpdatedUser, UpdatedState) when
    is_integer(UserId), UserId > 0
->
    guild_visibility:compute_and_dispatch_visibility_changes_for_users(
        [UserId], StateWithUpdatedUser, UpdatedState
    );
dispatch_member_update_visibility(_UserId, StateWithUpdatedUser, UpdatedState) ->
    guild_visibility:compute_and_dispatch_visibility_changes(
        StateWithUpdatedUser, UpdatedState
    ).

-spec refresh_member_session_cache(user_id() | undefined, guild_state()) -> guild_state().
refresh_member_session_cache(UserId, State) when is_integer(UserId), UserId > 0 ->
    guild_sessions:refresh_user_session_cache(UserId, State);
refresh_member_session_cache(_UserId, State) ->
    State.

-spec maybe_sync_member_list_permission_state(guild_state()) -> ok.
maybe_sync_member_list_permission_state(_State) ->
    ok.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

update_data_for_event_unknown_returns_data_unchanged_test() ->
    Data = #{<<"test">> => true},
    ?assertEqual(Data, update_data_for_event(unknown_event, #{}, Data, #{})).

guild_member_remove_disconnects_voice_test() ->
    Self = self(),
    TestFun = fun(GId, ChId, UId, ConnId) ->
        Self ! {force_disconnect, GId, ChId, UId, ConnId},
        {ok, #{success => true}}
    end,
    State = build_voice_test_state(TestFun),
    EventData = #{<<"user">> => #{<<"id">> => <<"5">>}},
    UpdatedState = update_state(guild_member_remove, EventData, State),
    ?assertEqual(#{}, maps:get(voice_states, UpdatedState)),
    ?assertEqual(#{}, maps:get(sessions, UpdatedState, #{})),
    receive
        {force_disconnect, 42, 20, 5, <<"conn">>} -> ok
    after 200 ->
        ?assert(false)
    end.

build_voice_test_state(TestFun) ->
    #{
        id => 42,
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"999">>},
            <<"roles">> => [#{<<"id">> => <<"42">>, <<"permissions">> => <<"0">>}],
            <<"members">> => #{5 => #{<<"user">> => #{<<"id">> => <<"5">>}, <<"roles">> => []}},
            <<"channels">> => [#{<<"id">> => <<"20">>}]
        },
        voice_states => #{
            <<"conn">> => #{
                <<"user_id">> => <<"5">>,
                <<"guild_id">> => <<"42">>,
                <<"channel_id">> => <<"20">>,
                <<"connection_id">> => <<"conn">>
            }
        },
        sessions => #{<<"s1">> => #{user_id => 5, pid => self()}},
        test_force_disconnect_fun => TestFun
    }.

make_cache_test_state(GuildId) ->
    #{
        id => GuildId,
        data => #{<<"guild">> => #{<<"features">> => []}, <<"members">> => []},
        sessions => #{}
    }.

guild_update_syncs_unavailability_cache_test() ->
    GuildId = 420042,
    CleanupState = #{id => GuildId, data => #{<<"guild">> => #{<<"features">> => []}}},
    _ = guild_availability:update_unavailability_cache_for_state(CleanupState),
    try
        State0 = make_cache_test_state(GuildId),
        State1 = update_state(
            guild_update,
            #{<<"features">> => [<<"UNAVAILABLE_FOR_EVERYONE">>]},
            State0
        ),
        ?assertEqual(
            unavailable_for_everyone,
            guild_availability:get_cached_unavailability_mode(GuildId)
        ),
        _State2 = update_state(guild_update, #{<<"features">> => []}, State1),
        ?assertEqual(available, guild_availability:get_cached_unavailability_mode(GuildId))
    after
        _ = guild_availability:update_unavailability_cache_for_state(CleanupState)
    end.

-endif.
