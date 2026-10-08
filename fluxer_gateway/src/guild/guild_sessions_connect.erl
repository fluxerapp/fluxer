%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_sessions_connect).
-typing([eqwalizer]).

-export([
    handle_session_connect/3,
    resection_connected_user/3,
    resection_connected_user/4,
    build_initial_last_message_ids/1,
    build_initial_channel_versions/1,
    handle_session_down/3,
    find_session_by_ref/2,
    put_session_ref/3,
    remove_session_ref/2,
    build_session_ref_index/1,
    remove_session/2,
    counts_as_connected/1,
    invalidate_viewable_channels_cache/1
]).

-type guild_state() :: #{viewable_channels_cache => ets:tid(), term() => term()}.
-type session_id() :: binary().
-type user_id() :: integer().
-type guild_id() :: integer().
-type session_data() :: map().
-type sessions_map() :: #{session_id() => session_data()}.
-type connect_reply() ::
    {ok, map()}
    | {ok, unavailable, map()}
    | {error, too_many_sessions}
    | {error, not_member}.

-export_type([guild_state/0, session_id/0, user_id/0, connect_reply/0]).

-define(MAX_SESSIONS_PER_USER_PER_GUILD, 100).

-spec handle_session_connect(map(), pid(), guild_state()) ->
    {reply, connect_reply(), guild_state()}.
handle_session_connect(Request, Pid, State) ->
    #{session_id := SessionId, user_id := UserId} = Request,
    Sessions = maps:get(sessions, State, #{}),
    case maps:is_key(SessionId, Sessions) of
        true ->
            Opts = guild_thread_gate:state_opts(maps:get(SessionId, Sessions)),
            {reply, {ok, guild_data:get_guild_state(UserId, State, Opts)}, State};
        false ->
            register_new_session(Request, Pid, UserId, SessionId, State)
    end.

-spec register_new_session(
    map(), pid(), user_id(), session_id(), guild_state()
) ->
    {reply, connect_reply(), guild_state()}.
register_new_session(Request, Pid, UserId, SessionId, State) ->
    Sessions = maps:get(sessions, State, #{}),
    case user_session_count(UserId, State, Sessions) >= ?MAX_SESSIONS_PER_USER_PER_GUILD of
        true ->
            {reply, {error, too_many_sessions}, State};
        false ->
            do_register_new_session(Request, Pid, UserId, SessionId, Sessions, State)
    end.

-spec user_session_count(user_id(), guild_state(), sessions_map()) -> non_neg_integer().
user_session_count(UserId, State, Sessions) ->
    Counts = maps:get(user_session_counts, State, undefined),
    case is_map(Counts) of
        true -> require_non_neg(maps:get(UserId, Counts, 0));
        false -> count_user_sessions(UserId, Sessions)
    end.

-spec do_register_new_session(
    map(), pid(), user_id(), session_id(), sessions_map(), guild_state()
) ->
    {reply, connect_reply(), guild_state()}.
do_register_new_session(Request, Pid, UserId, SessionId, Sessions, State) ->
    GuildId = require_guild_id(maps:get(id, State)),
    case admit_session_connect(UserId, State) of
        reject_not_member ->
            {reply, {error, not_member}, State};
        admit ->
            ThreadFields = guild_thread_gate:session_fields(Request, UserId, State),
            GuildState = guild_data:get_guild_state(
                UserId, State, guild_thread_gate:state_opts(maps:merge(Request, ThreadFields))
            ),
            register_admitted_session(
                Request, Pid, UserId, SessionId, GuildId, GuildState, Sessions, State
            )
    end.

-spec admit_session_connect(user_id(), guild_state()) -> admit | reject_not_member.
admit_session_connect(UserId, State) ->
    case guild_availability:is_guild_unavailable_for_user(UserId, State) of
        true ->
            admit;
        false ->
            admit_existing_member_connect(UserId, State)
    end.

-spec admit_existing_member_connect(user_id(), guild_state()) -> admit | reject_not_member.
admit_existing_member_connect(UserId, State) ->
    case guild_data_members:find_member_by_user_id(UserId, State) of
        undefined -> reject_not_member;
        _Member -> admit
    end.

-spec register_admitted_session(
    map(), pid(), user_id(), session_id(), guild_id(), map(), sessions_map(), guild_state()
) ->
    {reply, connect_reply(), guild_state()}.
register_admitted_session(
    Request, Pid, UserId, SessionId, GuildId, GuildState, Sessions, State
) ->
    SessionData = build_session_data(Request, Pid, UserId, SessionId, State),
    store_initial_passive_state(SessionId, GuildId, GuildState),
    NewSessions = Sessions#{SessionId => SessionData},
    StateWithSession = put_session_ref(SessionId, maps:get(mref, SessionData), State#{
        sessions => NewSessions
    }),
    State0a = guild_sessions_connect_cleanup:clear_auto_stop_pending(
        StateWithSession
    ),
    State1 = track_connected_user(UserId, 1, State0a),
    PresenceBefore = guild_member_list_connected:resolve_presence_for_user(State1, UserId),
    State2 = guild_sessions_presence:subscribe_connected_user_presence(UserId, State1),
    State3 = resection_connected_user(UserId, PresenceBefore, State, State2),
    InitialGuildId = maps:get(initial_guild_id, Request, undefined),
    finalize_connect(
        SessionId,
        UserId,
        GuildId,
        InitialGuildId,
        State3,
        GuildState
    ).

-spec count_user_sessions(user_id(), sessions_map()) -> non_neg_integer().
count_user_sessions(UserId, Sessions) ->
    maps:fold(
        fun(_SId, SData, Acc) ->
            Acc + user_session_count_increment(UserId, SData)
        end,
        0,
        Sessions
    ).

-spec user_session_count_increment(user_id(), session_data()) -> 0 | 1.
user_session_count_increment(UserId, SData) ->
    case maps:get(user_id, SData, undefined) of
        UserId -> 1;
        _ -> 0
    end.

-spec build_session_data(
    map(), pid(), user_id(), session_id(), guild_state()
) -> session_data().
build_session_data(Request, Pid, UserId, SessionId, State) ->
    UserRoles = session_passive:get_user_roles_for_guild(UserId, State),
    #{thread_capable := Capable, thread_viewer := Viewer} = guild_thread_gate:session_fields(
        Request, UserId, State
    ),
    #{
        session_id => SessionId,
        user_id => UserId,
        pid => Pid,
        mref => monitor(process, Pid),
        active_guilds => maps:get(active_guilds, Request, sets:new()),
        user_roles => UserRoles,
        bot => maps:get(bot, Request, false),
        is_staff => maps:get(is_staff, Request, false),
        viewable_channels => get_viewable_channels_cached(UserId, UserRoles, State),
        thread_capable => Capable,
        thread_viewer => Viewer
    }.

-spec store_initial_passive_state(session_id(), guild_id(), map()) -> ok.
store_initial_passive_state(SessionId, GuildId, GuildState) ->
    passive_sync_registry:store(SessionId, GuildId, #{
        previous_passive_updates => build_initial_last_message_ids(GuildState),
        previous_passive_channel_versions => build_initial_channel_versions(GuildState),
        previous_passive_voice_states => #{}
    }).

-spec finalize_connect(
    session_id(),
    user_id(),
    guild_id(),
    guild_id() | undefined,
    guild_state(),
    map()
) ->
    {reply, {ok, map()} | {ok, unavailable, map()}, guild_state()}.
finalize_connect(SessionId, UserId, GuildId, InitialGuildId, State, CachedGuildState) ->
    case guild_availability:is_guild_unavailable_for_user(UserId, State) of
        true ->
            build_unavailable_reply(GuildId, State);
        false ->
            SyncedState = maybe_auto_sync_initial_guild(
                SessionId, GuildId, InitialGuildId, State
            ),
            {reply, {ok, CachedGuildState}, SyncedState}
    end.

-spec build_unavailable_reply(guild_id(), guild_state()) ->
    {reply, {ok, unavailable, map()}, guild_state()}.
build_unavailable_reply(GuildId, State) ->
    Base = #{<<"id">> => integer_to_binary(GuildId), <<"unavailable">> => true},
    Response =
        case guild_availability:is_unavailable_hidden_enabled(State) of
            true -> Base#{<<"unavailable_hidden">> => true};
            false -> Base
        end,
    {reply, {ok, unavailable, Response}, State}.

-spec build_initial_last_message_ids(map()) -> #{binary() => binary()}.
build_initial_last_message_ids(GuildState) ->
    Channels = maps:get(<<"channels">>, GuildState, []),
    lists:foldl(fun add_last_message_id/2, #{}, Channels).

-spec add_last_message_id(term(), #{binary() => binary()}) ->
    #{binary() => binary()}.
add_last_message_id(Channel, Acc) when is_map(Channel) ->
    RawChannelId = maps:get(<<"id">>, Channel, undefined),
    RawLastMessageId = maps:get(<<"last_message_id">>, Channel, null),
    case
        {
            snowflake_binary(<<"id">>, RawChannelId),
            snowflake_binary(<<"last_message_id">>, RawLastMessageId)
        }
    of
        {undefined, _} -> Acc;
        {_, undefined} -> Acc;
        {ChannelId, LastMessageId} -> Acc#{ChannelId => LastMessageId}
    end;
add_last_message_id(_, Acc) ->
    Acc.

-spec build_initial_channel_versions(map()) -> #{binary() => integer()}.
build_initial_channel_versions(GuildState) ->
    Channels = maps:get(<<"channels">>, GuildState, []),
    lists:foldl(fun add_channel_version/2, #{}, Channels).

-spec add_channel_version(term(), #{binary() => integer()}) ->
    #{binary() => integer()}.
add_channel_version(Channel, Acc) when is_map(Channel) ->
    case snowflake_binary(<<"id">>, maps:get(<<"id">>, Channel, undefined)) of
        Id when is_binary(Id) ->
            Version = map_utils:get_integer(Channel, <<"version">>, 0),
            Acc#{Id => require_integer(Version)};
        _ ->
            Acc
    end;
add_channel_version(_, Acc) ->
    Acc.

-spec handle_session_down(
    reference(), {session_id() | undefined, session_data() | undefined}, guild_state()
) ->
    {noreply, guild_state()}.
handle_session_down(Ref, {DisconnectingSessionId, DisconnectingSession}, State) ->
    Sessions = maps:get(sessions, State, #{}),
    DisconnectUserId = disconnect_user_id(DisconnectingSession),
    State1 = cleanup_disconnecting_session(DisconnectingSession, State),
    NewSessions = remove_session_id(DisconnectingSessionId, Sessions),
    NewState0 = remove_session_ref(Ref, State1#{sessions => NewSessions}),
    NewState = track_connected_user(DisconnectUserId, -1, NewState0),
    NewState1 = maybe_resection_on_disconnect(
        DisconnectingSession, State, NewState
    ),
    finish_session_down(NewSessions, NewState1).

-spec disconnect_user_id(session_data() | undefined) ->
    user_id() | undefined.
disconnect_user_id(#{user_id := UID}) -> UID;
disconnect_user_id(_) -> undefined.

-spec finish_session_down(sessions_map(), guild_state()) ->
    {noreply, guild_state()}.
finish_session_down(NewSessions, State) ->
    case map_size(NewSessions) of
        0 ->
            {noreply, guild_sessions_connect_cleanup:maybe_mark_auto_stop_pending(State)};
        _ ->
            {noreply, guild_sessions_connect_cleanup:clear_auto_stop_pending(State)}
    end.

-spec maybe_resection_on_disconnect(
    session_data() | undefined, guild_state(), guild_state()
) -> guild_state().
maybe_resection_on_disconnect(#{user_id := UserIdDown}, OldState, NewState) when
    is_integer(UserIdDown)
->
    maybe_resection_disconnected_user(UserIdDown, OldState, NewState);
maybe_resection_on_disconnect(_, _OldState, NewState) ->
    NewState.

-spec remove_session(session_id(), guild_state()) -> guild_state().
remove_session(SessionId, State) ->
    Sessions = maps:get(sessions, State, #{}),
    case maps:get(SessionId, Sessions, undefined) of
        undefined ->
            State;
        Session when is_map(Session) ->
            do_remove_session(SessionId, Session, State)
    end.

-spec do_remove_session(
    session_id(), session_data(), guild_state()
) -> guild_state().
do_remove_session(SessionId, Session, State) ->
    maybe_demonitor_session(Session),
    StateAfterCleanup = cleanup_removed_session(Session, State),
    SessionsAfterCleanup = maps:get(sessions, StateAfterCleanup, #{}),
    NewSessions = maps:remove(SessionId, SessionsAfterCleanup),
    remove_session_ref(
        maps:get(mref, Session, undefined), StateAfterCleanup#{sessions => NewSessions}
    ).

-spec cleanup_removed_session(session_data(), guild_state()) -> guild_state().
cleanup_removed_session(Session, State) ->
    case counts_as_connected(Session) of
        true ->
            UserId = maps:get(user_id, Session, undefined),
            track_connected_user(UserId, -1, cleanup_disconnecting_session(Session, State));
        false ->
            cleanup_session_subscriptions(Session, State)
    end.

-spec counts_as_connected(session_data()) -> boolean().
counts_as_connected(#{pending_connect := true} = Session) ->
    maps:get(owns_connected_tracking, Session, false) =:= true;
counts_as_connected(_Session) ->
    true.

-spec maybe_demonitor_session(session_data()) -> ok.
maybe_demonitor_session(Session) ->
    Ref = maps:get(mref, Session, undefined),
    case is_reference(Ref) of
        true ->
            demonitor(Ref, [flush]),
            ok;
        false ->
            ok
    end.

-spec find_session_by_ref(reference(), guild_state()) ->
    {session_id() | undefined, session_data() | undefined}.
find_session_by_ref(Ref, State) ->
    Sessions = maps:get(sessions, State, #{}),
    SessionId = maps:get(Ref, maps:get(guild_session_refs, State, #{}), undefined),
    case maps:get(SessionId, Sessions, undefined) of
        #{mref := Ref} = Session ->
            {SessionId, Session};
        _ ->
            find_session_by_ref_scan(Ref, Sessions)
    end.

-spec find_session_by_ref_scan(reference(), sessions_map()) ->
    {session_id() | undefined, session_data() | undefined}.
find_session_by_ref_scan(Ref, Sessions) ->
    maps:fold(
        fun
            (SessionId, #{mref := MRef} = S, _Acc) when MRef =:= Ref -> {SessionId, S};
            (_SessionId, _S, Acc) -> Acc
        end,
        {undefined, undefined},
        Sessions
    ).

-spec remove_session_id(session_id() | undefined, sessions_map()) -> sessions_map().
remove_session_id(SessionId, Sessions) when is_binary(SessionId) ->
    maps:remove(SessionId, Sessions);
remove_session_id(undefined, Sessions) ->
    Sessions.

-spec put_session_ref(session_id(), term(), guild_state()) -> guild_state().
put_session_ref(SessionId, Ref, State) when is_reference(Ref) ->
    ok = guild_health:put_session(SessionId, State),
    Refs = maps:get(guild_session_refs, State, #{}),
    State#{guild_session_refs => Refs#{Ref => SessionId}};
put_session_ref(_SessionId, _Ref, State) ->
    State.

-spec remove_session_ref(term(), guild_state()) -> guild_state().
remove_session_ref(Ref, State) when is_reference(Ref) ->
    Refs = maps:get(guild_session_refs, State, #{}),
    ok = guild_health:remove_session(maps:get(Ref, Refs, undefined), State),
    State#{guild_session_refs => maps:remove(Ref, Refs)};
remove_session_ref(_Ref, State) ->
    State.

-spec build_session_ref_index(sessions_map()) -> #{reference() => session_id()}.
build_session_ref_index(Sessions) ->
    maps:fold(
        fun
            (SessionId, Session, Acc) when is_binary(SessionId), is_map(Session) ->
                add_session_ref_to_index(SessionId, Session, Acc);
            (_SessionId, _Session, Acc) ->
                Acc
        end,
        #{},
        Sessions
    ).

-spec add_session_ref_to_index(session_id(), session_data(), #{reference() => session_id()}) ->
    #{reference() => session_id()}.
add_session_ref_to_index(SessionId, Session, Acc) ->
    case maps:get(mref, Session, undefined) of
        Ref when is_reference(Ref) -> Acc#{Ref => SessionId};
        _ -> Acc
    end.

-spec cleanup_disconnecting_session(
    session_data() | undefined, guild_state()
) -> guild_state().
cleanup_disconnecting_session(undefined, State) ->
    State;
cleanup_disconnecting_session(Session, State) ->
    UserId = maps:get(user_id, Session),
    State1 = guild_sessions_presence:unsubscribe_from_user_presence(UserId, State),
    cleanup_session_subscriptions(Session, State1).

-spec cleanup_session_subscriptions(session_data(), guild_state()) -> guild_state().
cleanup_session_subscriptions(Session, State) ->
    SessionId = maps:get(session_id, Session),
    GuildId = require_guild_id(maps:get(id, State)),
    passive_sync_registry:delete(SessionId, GuildId),
    State1 = guild_member_list:unsubscribe_session(SessionId, State),
    MemberSubs = maps:get(member_subscriptions, State1, guild_subscriptions:init_state()),
    NewMemberSubs = guild_subscriptions:unsubscribe_session(SessionId, MemberSubs),
    State2 = State1#{member_subscriptions => NewMemberSubs},
    guild_sessions_connect_cleanup:cleanup_connect_admission_for_session(SessionId, State2).

-spec maybe_resection_disconnected_user(
    user_id(), guild_state(), guild_state()
) -> guild_state().
maybe_resection_disconnected_user(UserId, OldState, NewState) ->
    case became_disconnected(UserId, OldState, NewState) of
        true -> resection_user_after_connection_change(UserId, OldState, NewState);
        false -> NewState
    end.

-spec resection_connected_user(user_id() | undefined, guild_state(), guild_state()) ->
    guild_state().
resection_connected_user(UserId, OldState, NewState) ->
    resection_connected_user(UserId, undefined, OldState, NewState).

-spec resection_connected_user(
    user_id() | undefined, map() | undefined, guild_state(), guild_state()
) -> guild_state().
resection_connected_user(UserId, PresenceBefore, OldState, NewState) when
    is_integer(UserId), UserId > 0
->
    case became_connected(UserId, OldState, NewState) of
        true ->
            ResectionedState = resection_after_connect(
                UserId, PresenceBefore, OldState, NewState
            ),
            _ = guild_presence_reconcile:maybe_schedule_user_repair(UserId, ResectionedState),
            ResectionedState;
        false ->
            NewState
    end;
resection_connected_user(_UserId, _PresenceBefore, _OldState, NewState) ->
    NewState.

-spec resection_after_connect(
    user_id(), map() | undefined, guild_state(), guild_state()
) -> guild_state().
resection_after_connect(UserId, PresenceBefore, OldState, NewState) ->
    case presence_resynced_lists(UserId, PresenceBefore, NewState) of
        true -> resection_user_already_synced(UserId, NewState);
        false -> resection_user_after_connection_change(UserId, OldState, NewState)
    end.

-spec presence_resynced_lists(user_id(), map() | undefined, guild_state()) -> boolean().
presence_resynced_lists(_UserId, undefined, _State) ->
    false;
presence_resynced_lists(UserId, PresenceBefore, State) ->
    PresenceAfter = guild_member_list_connected:resolve_presence_for_user(State, UserId),
    guild_member_list_write:presence_change_resyncs_lists(PresenceBefore, PresenceAfter).

-spec resection_user_already_synced(user_id(), guild_state()) -> guild_state().
resection_user_already_synced(UserId, State) ->
    _ = guild_presence:sync_online_status(UserId, State),
    guild_member_list_write:queue_synced_connection_change(UserId, State).

-spec became_connected(user_id(), guild_state(), guild_state()) -> boolean().
became_connected(UserId, OldState, NewState) ->
    (not user_connected(UserId, OldState)) andalso user_connected(UserId, NewState).

-spec became_disconnected(user_id(), guild_state(), guild_state()) -> boolean().
became_disconnected(UserId, OldState, NewState) ->
    user_connected(UserId, OldState) andalso not user_connected(UserId, NewState).

-spec user_connected(user_id(), guild_state()) -> boolean().
user_connected(UserId, State) ->
    sets:is_element(UserId, guild_member_list_connected:connected_session_user_ids(State)).

-spec resection_user_after_connection_change(
    user_id(), guild_state(), guild_state()
) -> guild_state().
resection_user_after_connection_change(UserId, _OldState, NewState) ->
    _ = guild_presence:sync_online_status(UserId, NewState),
    guild_member_list_write:broadcast_channel_engine_connection_change(UserId, NewState).

-spec maybe_auto_sync_initial_guild(
    session_id(), guild_id(), guild_id() | undefined, guild_state()
) -> guild_state().
maybe_auto_sync_initial_guild(SessionId, GuildId, GuildId, State) ->
    Sessions = require_map(maps:get(sessions, State, #{})),
    case maps:get(SessionId, Sessions, undefined) of
        undefined ->
            State;
        SessionData when is_map(SessionData) ->
            Synced = session_passive:mark_guild_synced(
                GuildId, SessionData
            ),
            State#{sessions => Sessions#{SessionId => Synced}}
    end;
maybe_auto_sync_initial_guild(_, _, _, State) ->
    State.

-spec track_connected_user(
    user_id() | undefined, integer(), guild_state()
) -> guild_state().
track_connected_user(UserId, _Delta, State) when
    not is_integer(UserId); UserId =< 0
->
    State;
track_connected_user(UserId, Delta, State) ->
    Counts = require_map(maps:get(user_session_counts, State, #{})),
    Connected = maps:get(connected_user_ids, State, sets:new()),
    OldCount = require_non_neg(maps:get(UserId, Counts, 0)),
    NewCount = max(0, OldCount + Delta),
    {NC, NConn} = apply_count_change(
        UserId, NewCount, Counts, Connected
    ),
    State#{user_session_counts => NC, connected_user_ids => NConn}.

-spec apply_count_change(
    user_id(), non_neg_integer(), map(), sets:set(integer())
) -> {map(), sets:set(integer())}.
apply_count_change(UserId, 0, Counts, Connected) ->
    {maps:remove(UserId, Counts), sets:del_element(UserId, Connected)};
apply_count_change(UserId, NewCount, Counts, Connected) ->
    {Counts#{UserId => NewCount}, sets:add_element(UserId, Connected)}.

-spec get_viewable_channels_cached(
    user_id(), [integer()], guild_state()
) -> map().
get_viewable_channels_cached(UserId, UserRoles, State) ->
    RoleKey = list_to_tuple(lists:sort(UserRoles)),
    CacheTab = viewable_channels_cache_table(State),
    lookup_or_compute_viewable(UserId, RoleKey, CacheTab, State).

-spec lookup_or_compute_viewable(
    user_id(), tuple(), ets:tid() | undefined, guild_state()
) -> map().
lookup_or_compute_viewable(UserId, _RoleKey, undefined, State) ->
    compute_viewable_channel_map(UserId, State);
lookup_or_compute_viewable(UserId, RoleKey, CacheTab, State) ->
    case ets:lookup(CacheTab, RoleKey) of
        [{_, Map}] ->
            Map;
        [] ->
            Map = compute_viewable_channel_map(UserId, State),
            ets:insert(CacheTab, {RoleKey, Map}),
            Map
    end.

-spec compute_viewable_channel_map(user_id(), guild_state()) -> map().
compute_viewable_channel_map(UserId, State) ->
    guild_sessions:build_viewable_channel_map(
        guild_visibility:get_user_viewable_channels(UserId, State)
    ).

-spec invalidate_viewable_channels_cache(guild_state()) -> ok.
invalidate_viewable_channels_cache(State) ->
    case viewable_channels_cache_table(State) of
        CacheTab when is_reference(CacheTab) ->
            ets:delete_all_objects(CacheTab),
            ok;
        undefined ->
            ok
    end.

-spec viewable_channels_cache_table(guild_state()) -> ets:tid() | undefined.
viewable_channels_cache_table(#{viewable_channels_cache := CacheTab}) ->
    CacheTab;
viewable_channels_cache_table(_) ->
    undefined.

-spec require_map(term()) -> #{term() => term()}.
require_map(M) when is_map(M) -> M;
require_map(_) -> #{}.

-spec require_guild_id(term()) -> guild_id().
require_guild_id(Id) when is_integer(Id), Id > 0 -> Id;
require_guild_id(_) -> error(badarg).

-spec require_non_neg(term()) -> non_neg_integer().
require_non_neg(V) when is_integer(V), V >= 0 -> V;
require_non_neg(_) -> 0.

-spec require_integer(term()) -> integer().
require_integer(V) when is_integer(V) -> V;
require_integer(_) -> 0.

-spec snowflake_binary(binary(), term()) -> binary() | undefined.
snowflake_binary(FieldName, Value) ->
    case validation:validate_snowflake(FieldName, Value) of
        {ok, Id} -> integer_to_binary(Id);
        {error, _, _} -> undefined
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

count_user_sessions_empty_test() ->
    ?assertEqual(0, count_user_sessions(1, #{})).

count_user_sessions_counts_correctly_test() ->
    Sessions = #{
        <<"s1">> => #{user_id => 100, session_id => <<"s1">>},
        <<"s2">> => #{user_id => 100, session_id => <<"s2">>},
        <<"s3">> => #{user_id => 200, session_id => <<"s3">>},
        <<"s4">> => #{user_id => 100, session_id => <<"s4">>}
    },
    ?assertEqual(3, count_user_sessions(100, Sessions)),
    ?assertEqual(1, count_user_sessions(200, Sessions)),
    ?assertEqual(0, count_user_sessions(300, Sessions)).

count_user_sessions_at_limit_test() ->
    Sessions = sessions_for_user(42, ?MAX_SESSIONS_PER_USER_PER_GUILD),
    ?assertEqual(?MAX_SESSIONS_PER_USER_PER_GUILD, count_user_sessions(42, Sessions)).

count_user_sessions_under_limit_allows_test() ->
    Sessions = sessions_for_user(42, ?MAX_SESSIONS_PER_USER_PER_GUILD - 1),
    ?assert(count_user_sessions(42, Sessions) < ?MAX_SESSIONS_PER_USER_PER_GUILD).

count_user_sessions_at_limit_rejects_test() ->
    Sessions = sessions_for_user(42, ?MAX_SESSIONS_PER_USER_PER_GUILD),
    ?assert(count_user_sessions(42, Sessions) >= ?MAX_SESSIONS_PER_USER_PER_GUILD).

sessions_for_user(UserId, Count) ->
    Sessions = maps:from_list([
        {iolist_to_binary(["s", integer_to_list(I)]), #{
            user_id => UserId, session_id => iolist_to_binary(["s", integer_to_list(I)])
        }}
     || I <- lists:seq(1, Count)
    ]),
    Sessions.

resection_connected_user_skips_when_already_connected_test() ->
    Connected = sets:from_list([42]),
    Old = #{connected_user_ids => Connected},
    New = #{connected_user_ids => Connected, marker => updated},
    ?assertEqual(New, resection_connected_user(42, #{}, Old, New)).

resection_connected_user_skips_when_still_disconnected_test() ->
    Old = #{connected_user_ids => sets:new()},
    New = #{connected_user_ids => sets:new(), marker => updated},
    ?assertEqual(New, resection_connected_user(42, #{}, Old, New)).

resection_connected_user_ignores_undefined_user_test() ->
    New = #{connected_user_ids => sets:new()},
    ?assertEqual(New, resection_connected_user(undefined, #{}, New, New)).

resection_disconnected_user_skips_when_still_connected_test() ->
    Connected = sets:from_list([42]),
    Old = #{connected_user_ids => Connected},
    New = #{connected_user_ids => Connected, marker => updated},
    ?assertEqual(New, maybe_resection_disconnected_user(42, Old, New)).

resection_disconnected_user_skips_when_already_disconnected_test() ->
    Old = #{connected_user_ids => sets:new()},
    New = #{connected_user_ids => sets:new(), marker => updated},
    ?assertEqual(New, maybe_resection_disconnected_user(42, Old, New)).

became_disconnected_detects_transition_test() ->
    Old = #{connected_user_ids => sets:from_list([42])},
    New = #{connected_user_ids => sets:new()},
    ?assert(became_disconnected(42, Old, New)),
    ?assertNot(became_disconnected(42, Old, Old)),
    ?assertNot(became_disconnected(42, New, New)),
    ?assertNot(became_disconnected(42, New, Old)).

presence_resynced_lists_needs_a_list_visible_change_test() ->
    State = #{member_presence => #{42 => #{<<"status">> => <<"online">>}}},
    ?assertNot(presence_resynced_lists(42, undefined, State)),
    ?assert(presence_resynced_lists(42, #{}, State)),
    ?assertNot(presence_resynced_lists(42, #{<<"status">> => <<"online">>}, State)).

became_connected_detects_transition_test() ->
    Old = #{connected_user_ids => sets:new()},
    New = #{connected_user_ids => sets:from_list([42])},
    ?assert(became_connected(42, Old, New)),
    ?assertNot(became_connected(42, New, New)),
    ?assertNot(became_connected(42, Old, Old)).

-endif.
