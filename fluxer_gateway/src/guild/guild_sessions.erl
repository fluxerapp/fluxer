%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_sessions).
-typing([eqwalizer]).

-export([
    handle_session_connect/3,
    handle_session_down/2,
    remove_session/2,
    filter_sessions_for_channel/4,
    filter_sessions_for_message/5,
    filter_sessions_for_manage_channels/4,
    filter_sessions_exclude_session/2,
    handle_user_offline/2,
    set_session_active_guild/3,
    set_session_passive_guild/3,
    build_initial_last_message_ids/1,
    is_session_active/2,
    subscribe_connected_user_presence/2,
    subscribe_to_user_presence/2,
    unsubscribe_from_user_presence/2,
    set_session_viewable_channels/3,
    refresh_user_session_cache/2,
    refresh_all_viewable_channels/1,
    handle_set_typing_override/3,
    handle_send_guild_sync/2,
    handle_send_members_chunk/3,
    build_viewable_channel_map/1
]).

-define(MAX_PERM_MEMO_ENTRIES, 8192).

-type guild_state() :: map().
-type session_id() :: binary().
-type user_id() :: integer().
-type guild_id() :: integer().
-type channel_id() :: integer().
-type session_data() :: map().
-type sessions_map() :: #{session_id() => session_data()}.
-type session_pair() :: {session_id(), session_data()}.
-type perm_memo() :: #{user_id() => non_neg_integer()}.
-type message_ctx() :: {channel_id(), binary(), session_id() | undefined, guild_state()}.
-export_type([
    guild_state/0,
    session_id/0,
    user_id/0,
    guild_id/0,
    channel_id/0,
    sessions_map/0,
    session_pair/0
]).

-spec handle_session_connect(map(), pid(), guild_state()) ->
    {reply,
        {ok, map()}
        | {ok, unavailable, map()}
        | {error, too_many_sessions}
        | {error, not_member},
        guild_state()}.
handle_session_connect(Request, Pid, State) ->
    guild_sessions_connect:handle_session_connect(Request, Pid, State).

-spec handle_session_down(reference(), guild_state()) ->
    {noreply, guild_state()} | {stop, normal, guild_state()}.
handle_session_down(Ref, State) ->
    case pending_session_by_ref(Ref, State) of
        {ok, SessionId, Session, Sessions} ->
            handle_pending_ref_down(SessionId, Session, Ref, Sessions, State);
        not_found ->
            guild_sessions_connect:handle_session_down(Ref, State)
    end.

-spec handle_pending_ref_down(
    session_id(), session_data(), reference(), sessions_map(), guild_state()
) -> {noreply, guild_state()} | {stop, normal, guild_state()}.
handle_pending_ref_down(SessionId, Session, Ref, Sessions, State) ->
    case pending_session_owns_connected_tracking(Session, Sessions, State) of
        true -> guild_sessions_connect:handle_session_down(Ref, State);
        false -> handle_pending_session_down(SessionId, Ref, Sessions, State)
    end.

-spec pending_session_by_ref(reference(), guild_state()) ->
    {ok, session_id(), session_data(), sessions_map()} | not_found.
pending_session_by_ref(Ref, State) ->
    Sessions = maps:get(sessions, State, #{}),
    case session_id_by_ref(Ref, Sessions, State) of
        SessionId when is_binary(SessionId) -> pending_session_entry(SessionId, Sessions);
        _ -> not_found
    end.

-spec pending_session_entry(session_id(), sessions_map()) ->
    {ok, session_id(), session_data(), sessions_map()} | not_found.
pending_session_entry(SessionId, Sessions) ->
    case maps:get(SessionId, Sessions, undefined) of
        #{pending_connect := true} = Session -> {ok, SessionId, Session, Sessions};
        _ -> not_found
    end.

-spec session_id_by_ref(reference(), sessions_map(), guild_state()) -> session_id() | undefined.
session_id_by_ref(Ref, Sessions, State) ->
    Refs = maps:get(guild_session_refs, State, #{}),
    case maps:get(Ref, Refs, undefined) of
        SessionId when is_binary(SessionId) -> SessionId;
        _ -> session_id_by_ref_scan(Ref, Sessions)
    end.

-spec session_id_by_ref_scan(reference(), sessions_map()) -> session_id() | undefined.
session_id_by_ref_scan(Ref, Sessions) ->
    maps:fold(
        fun(SessionId, Session, Found) ->
            match_session_ref(Ref, SessionId, Session, Found)
        end,
        undefined,
        Sessions
    ).

-spec match_session_ref(reference(), session_id(), session_data(), session_id() | undefined) ->
    session_id() | undefined.
match_session_ref(Ref, SessionId, #{mref := Ref}, _Found) -> SessionId;
match_session_ref(_Ref, _SessionId, _Session, Found) -> Found.

-spec pending_session_owns_connected_tracking(session_data(), sessions_map(), guild_state()) ->
    boolean().
pending_session_owns_connected_tracking(Session, Sessions, State) ->
    UserId = maps:get(user_id, Session, undefined),
    Counts = maps:get(user_session_counts, State, #{}),
    TrackedCount = non_negative_count(maps:get(UserId, Counts, 0)),
    TrackedCount > active_session_count(UserId, Sessions).

-spec non_negative_count(term()) -> non_neg_integer().
non_negative_count(Count) when is_integer(Count), Count >= 0 -> Count;
non_negative_count(_) -> 0.

-spec active_session_count(user_id() | undefined, sessions_map()) -> non_neg_integer().
active_session_count(UserId, Sessions) ->
    maps:fold(
        fun(_SessionId, Session, Count) ->
            count_active_session(UserId, Session, Count)
        end,
        0,
        Sessions
    ).

-spec count_active_session(user_id() | undefined, session_data(), non_neg_integer()) ->
    non_neg_integer().
count_active_session(UserId, #{user_id := UserId} = Session, Count) ->
    case maps:get(pending_connect, Session, false) of
        true -> Count;
        false -> Count + 1
    end;
count_active_session(_UserId, _Session, Count) ->
    Count.

-spec handle_pending_session_down(session_id(), reference(), sessions_map(), guild_state()) ->
    {noreply, guild_state()}.
handle_pending_session_down(SessionId, Ref, Sessions, State) ->
    NewSessions = maps:remove(SessionId, Sessions),
    SessionRefs = maps:get(guild_session_refs, State, #{}),
    State1 = State#{
        sessions => NewSessions,
        guild_session_refs => maps:remove(Ref, SessionRefs)
    },
    State2 = guild_sessions_connect_cleanup:cleanup_connect_admission_for_session(
        SessionId, State1
    ),
    finish_pending_session_down(NewSessions, State2).

-spec finish_pending_session_down(sessions_map(), guild_state()) -> {noreply, guild_state()}.
finish_pending_session_down(NewSessions, State) ->
    case map_size(NewSessions) of
        0 ->
            {noreply, guild_sessions_connect_cleanup:maybe_mark_auto_stop_pending(State)};
        _ ->
            {noreply, guild_sessions_connect_cleanup:clear_auto_stop_pending(State)}
    end.

-spec remove_session(session_id(), guild_state()) -> guild_state().
remove_session(SessionId, State) ->
    guild_sessions_connect:remove_session(SessionId, State).

-spec build_initial_last_message_ids(map()) -> #{binary() => binary()}.
build_initial_last_message_ids(GuildState) ->
    guild_sessions_connect:build_initial_last_message_ids(GuildState).

-spec subscribe_connected_user_presence(user_id(), guild_state()) -> guild_state().
subscribe_connected_user_presence(UserId, State) ->
    guild_sessions_presence:subscribe_connected_user_presence(UserId, State).

-spec subscribe_to_user_presence(user_id(), guild_state()) -> guild_state().
subscribe_to_user_presence(UserId, State) ->
    guild_sessions_presence:subscribe_to_user_presence(UserId, State).

-spec unsubscribe_from_user_presence(user_id(), guild_state()) -> guild_state().
unsubscribe_from_user_presence(UserId, State) ->
    guild_sessions_presence:unsubscribe_from_user_presence(UserId, State).

-spec handle_user_offline(user_id(), guild_state()) -> guild_state().
handle_user_offline(UserId, State) ->
    guild_sessions_presence:handle_user_offline(UserId, State).

-spec set_session_active_guild(session_id(), guild_id(), guild_state()) -> guild_state().
set_session_active_guild(SessionId, GuildId, State) ->
    guild_sessions_passive:set_session_active_guild(SessionId, GuildId, State).

-spec set_session_passive_guild(session_id(), guild_id(), guild_state()) -> guild_state().
set_session_passive_guild(SessionId, GuildId, State) ->
    guild_sessions_passive:set_session_passive_guild(SessionId, GuildId, State).

-spec is_session_active(session_id(), guild_state()) -> boolean().
is_session_active(SessionId, State) ->
    guild_sessions_passive:is_session_active(SessionId, State).

-spec handle_set_typing_override(session_id(), boolean(), guild_state()) -> guild_state().
handle_set_typing_override(SessionId, TypingFlag, State) ->
    guild_sessions_passive:handle_set_typing_override(SessionId, TypingFlag, State).

-spec handle_send_guild_sync(session_id(), guild_state()) -> guild_state().
handle_send_guild_sync(SessionId, State) ->
    guild_sessions_passive:handle_send_guild_sync(SessionId, State).

-spec handle_send_members_chunk(session_id(), map(), guild_state()) -> ok.
handle_send_members_chunk(SessionId, ChunkData, State) ->
    guild_sessions_passive:handle_send_members_chunk(SessionId, ChunkData, State).

-spec filter_sessions_for_channel(
    sessions_map(), channel_id(), session_id() | undefined, guild_state()
) -> [session_pair()].
filter_sessions_for_channel(Sessions, ChannelId, SessionIdOpt, State) ->
    filter_active_sessions(Sessions, SessionIdOpt, fun(S, _Sid) ->
        session_can_view_channel(S, ChannelId, State)
    end).

-spec filter_sessions_for_message(
    sessions_map(), channel_id(), binary(), session_id() | undefined, guild_state()
) -> [session_pair()].
filter_sessions_for_message(Sessions, ChannelId, MessageId, SessionIdOpt, State) ->
    filter_message_memo(Sessions, ChannelId, MessageId, SessionIdOpt, State).

-spec filter_message_memo(
    sessions_map(), channel_id(), binary(), session_id() | undefined, guild_state()
) -> [session_pair()].
filter_message_memo(Sessions, ChannelId, MessageId, SessionIdOpt, State) ->
    Ctx = {ChannelId, MessageId, SessionIdOpt, State},
    {Acc, _Memo} = maps:fold(
        fun(Sid, S, In) -> collect_message_session(Sid, S, Ctx, In) end,
        {[], #{}},
        Sessions
    ),
    Acc.

-spec collect_message_session(
    session_id(), session_data(), message_ctx(), {[session_pair()], perm_memo()}
) -> {[session_pair()], perm_memo()}.
collect_message_session(Sid, S, Ctx, {Acc, Memo}) ->
    {ChannelId, _MessageId, SessionIdOpt, State} = Ctx,
    Visible =
        not is_pending_or_excluded(Sid, S, SessionIdOpt) andalso
            session_can_view_channel(S, ChannelId, State),
    case Visible of
        true -> memo_message_session(Sid, S, Ctx, Acc, Memo);
        false -> {Acc, Memo}
    end.

-spec memo_message_session(
    session_id(), session_data(), message_ctx(), [session_pair()], perm_memo()
) -> {[session_pair()], perm_memo()}.
memo_message_session(Sid, S, {ChannelId, MessageId, _SessionIdOpt, State}, Acc, Memo) ->
    case maps:get(user_id, S, undefined) of
        UserId when is_integer(UserId) ->
            {Perms, Memo1} = memo_member_permissions(UserId, ChannelId, State, Memo),
            Ok = guild_permissions:can_access_message_by_permissions(Perms, MessageId, State),
            {prepend_session(Ok, Sid, S, Acc), Memo1};
        _ ->
            {Acc, Memo}
    end.

-spec memo_member_permissions(user_id(), channel_id(), guild_state(), perm_memo()) ->
    {non_neg_integer(), perm_memo()}.
memo_member_permissions(UserId, ChannelId, State, Memo) ->
    case maps:get(UserId, Memo, undefined) of
        Perms when is_integer(Perms) ->
            {Perms, Memo};
        _ ->
            Computed = guild_permissions:get_member_permissions(UserId, ChannelId, State),
            {Computed, store_perm_memo(UserId, Computed, Memo)}
    end.

-spec store_perm_memo(user_id(), non_neg_integer(), perm_memo()) -> perm_memo().
store_perm_memo(UserId, Perms, Memo) when map_size(Memo) < ?MAX_PERM_MEMO_ENTRIES ->
    Memo#{UserId => Perms};
store_perm_memo(_UserId, _Perms, Memo) ->
    Memo.

-spec prepend_session(boolean(), session_id(), session_data(), [session_pair()]) ->
    [session_pair()].
prepend_session(true, Sid, S, Acc) -> [{Sid, S} | Acc];
prepend_session(false, _Sid, _S, Acc) -> Acc.

-spec filter_sessions_for_manage_channels(
    sessions_map(), channel_id(), session_id() | undefined, guild_state()
) -> [session_pair()].
filter_sessions_for_manage_channels(Sessions, ChannelId, SessionIdOpt, State) ->
    filter_active_sessions(Sessions, SessionIdOpt, fun(S, _Sid) ->
        UserId = maps:get(user_id, S),
        guild_permissions:can_manage_channel(UserId, ChannelId, State)
    end).

-spec filter_active_sessions(
    sessions_map(),
    session_id() | undefined,
    fun((session_data(), session_id()) -> boolean())
) -> [session_pair()].
filter_active_sessions(Sessions, SessionIdOpt, Pred) ->
    maps:fold(
        fun(Sid, S, Acc) ->
            collect_active_session(Sid, S, SessionIdOpt, Pred, Acc)
        end,
        [],
        Sessions
    ).

-spec collect_active_session(
    session_id(),
    session_data(),
    session_id() | undefined,
    fun((session_data(), session_id()) -> boolean()),
    [session_pair()]
) -> [session_pair()].
collect_active_session(Sid, S, SessionIdOpt, Pred, Acc) ->
    case not is_pending_or_excluded(Sid, S, SessionIdOpt) andalso Pred(S, Sid) of
        true -> [{Sid, S} | Acc];
        false -> Acc
    end.

-spec is_pending_or_excluded(session_id(), session_data(), session_id() | undefined) ->
    boolean().
is_pending_or_excluded(Sid, S, SessionIdOpt) ->
    maps:get(pending_connect, S, false) orelse
        should_exclude_session(Sid, SessionIdOpt).

-spec filter_sessions_exclude_session(sessions_map(), session_id() | undefined) ->
    [session_pair()].
filter_sessions_exclude_session(Sessions, SessionIdOpt) ->
    maps:fold(
        fun(Sid, S, Acc) ->
            collect_non_excluded(Sid, S, SessionIdOpt, Acc)
        end,
        [],
        Sessions
    ).

-spec collect_non_excluded(
    session_id(), session_data(), session_id() | undefined, [session_pair()]
) -> [session_pair()].
collect_non_excluded(Sid, S, SessionIdOpt, Acc) ->
    Excluded = is_pending_or_excluded(Sid, S, SessionIdOpt),
    case not Excluded of
        true -> [{Sid, S} | Acc];
        false -> Acc
    end.

-spec should_exclude_session(session_id(), session_id() | undefined) -> boolean().
should_exclude_session(_, undefined) -> false;
should_exclude_session(Sid, SessionId) -> Sid =:= SessionId.

-spec set_session_viewable_channels(session_id(), map(), guild_state()) -> guild_state().
set_session_viewable_channels(SessionId, ViewableChannels, State) ->
    Sessions = maps:get(sessions, State, #{}),
    case maps:get(SessionId, Sessions, undefined) of
        undefined ->
            State;
        SessionData ->
            NewSessionData = SessionData#{viewable_channels => ViewableChannels},
            NewSessions = Sessions#{SessionId => NewSessionData},
            State#{sessions => NewSessions}
    end.

-spec refresh_user_session_cache(user_id(), guild_state()) -> guild_state().
refresh_user_session_cache(UserId, State) when is_integer(UserId), UserId > 0 ->
    Sessions = maps:get(sessions, State, #{}),
    UserRoles = session_passive:get_user_roles_for_guild(UserId, State),
    ViewableChannels = build_viewable_channel_map(
        guild_visibility:get_user_viewable_channels(UserId, State)
    ),
    NewSessions = maps:map(
        fun(_SessionId, SessionData) ->
            maybe_refresh_user_session_cache(UserId, UserRoles, ViewableChannels, SessionData)
        end,
        Sessions
    ),
    State#{sessions => NewSessions};
refresh_user_session_cache(_UserId, State) ->
    State.

-spec maybe_refresh_user_session_cache(user_id(), [integer()], map(), session_data()) ->
    session_data().
maybe_refresh_user_session_cache(UserId, UserRoles, ViewableChannels, SessionData) ->
    case maps:get(user_id, SessionData, undefined) of
        UserId ->
            SessionData#{
                user_roles => UserRoles,
                viewable_channels => ViewableChannels
            };
        _ ->
            SessionData
    end.

-spec refresh_all_viewable_channels(guild_state()) -> guild_state().
refresh_all_viewable_channels(State) ->
    guild_sessions_connect:invalidate_viewable_channels_cache(State),
    Sessions = maps:get(sessions, State, #{}),
    maps:fold(
        fun refresh_session_viewable/3,
        State,
        Sessions
    ).

-spec refresh_session_viewable(session_id(), session_data(), guild_state()) -> guild_state().
refresh_session_viewable(SessionId, SessionData, AccState) ->
    UserId = maps:get(user_id, SessionData, undefined),
    case is_integer(UserId) of
        true ->
            ViewableChannels = build_viewable_channel_map(
                guild_visibility:get_user_viewable_channels(UserId, AccState)
            ),
            set_session_viewable_channels(SessionId, ViewableChannels, AccState);
        false ->
            AccState
    end.

-spec session_can_view_channel(session_data(), channel_id(), guild_state()) -> boolean().
session_can_view_channel(SessionData, ChannelId, State) ->
    UserId = maps:get(user_id, SessionData, undefined),
    case {UserId, maps:get(viewable_channels, SessionData, undefined)} of
        {Uid, ViewableChannels} when is_integer(Uid), is_map(ViewableChannels) ->
            maps:is_key(ChannelId, ViewableChannels) orelse
                check_member_channel_access(Uid, ChannelId, State);
        {Uid, _} when is_integer(Uid) ->
            check_member_channel_access(Uid, ChannelId, State);
        _ ->
            false
    end.

-spec check_member_channel_access(user_id(), channel_id(), guild_state()) -> boolean().
check_member_channel_access(UserId, ChannelId, State) ->
    Member = guild_permissions:find_member_by_user_id(UserId, State),
    case Member of
        undefined -> false;
        _ -> guild_permissions:can_view_channel(UserId, ChannelId, Member, State)
    end.

-spec build_viewable_channel_map([channel_id()]) -> #{channel_id() => true}.
build_viewable_channel_map(ChannelIds) ->
    lists:foldl(
        fun(ChannelId, Acc) -> Acc#{ChannelId => true} end,
        #{},
        ChannelIds
    ).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

should_exclude_session_test() ->
    ?assertEqual(false, should_exclude_session(<<"s1">>, undefined)),
    ?assertEqual(true, should_exclude_session(<<"s1">>, <<"s1">>)),
    ?assertEqual(false, should_exclude_session(<<"s1">>, <<"s2">>)).

is_pending_or_excluded_pending_test() ->
    S = #{pending_connect => true},
    ?assertEqual(true, is_pending_or_excluded(<<"s1">>, S, undefined)).

is_pending_or_excluded_excluded_test() ->
    S = #{},
    ?assertEqual(true, is_pending_or_excluded(<<"s1">>, S, <<"s1">>)).

is_pending_or_excluded_neither_test() ->
    S = #{},
    ?assertEqual(false, is_pending_or_excluded(<<"s1">>, S, <<"s2">>)),
    ?assertEqual(false, is_pending_or_excluded(<<"s1">>, S, undefined)).

is_pending_or_excluded_pending_false_test() ->
    S = #{pending_connect => false},
    ?assertEqual(false, is_pending_or_excluded(<<"s1">>, S, undefined)).

filter_active_sessions_test() ->
    S1 = #{user_id => 1, pending_connect => false},
    S2 = #{user_id => 2, pending_connect => true},
    S3 = #{user_id => 3},
    Sessions = #{<<"a">> => S1, <<"b">> => S2, <<"c">> => S3},
    Result = filter_active_sessions(Sessions, <<"a">>, fun(_S, _Sid) -> true end),
    ResultSids = lists:sort([Sid || {Sid, _} <- Result]),
    ?assertEqual([<<"c">>], ResultSids).

filter_active_sessions_with_predicate_test() ->
    S1 = #{user_id => 1},
    S2 = #{user_id => 2},
    Sessions = #{<<"a">> => S1, <<"b">> => S2},
    Result = filter_active_sessions(Sessions, undefined, fun(S, _Sid) ->
        maps:get(user_id, S) =:= 2
    end),
    ?assertEqual(1, length(Result)),
    [{<<"b">>, _}] = Result.

store_perm_memo_test() ->
    ?assertEqual(#{7 => 42}, store_perm_memo(7, 42, #{})),
    ?assertEqual(#{7 => 42}, store_perm_memo(7, 42, #{7 => 42})).

store_perm_memo_bound_test() ->
    Full = maps:from_list([{I, 0} || I <- lists:seq(1, ?MAX_PERM_MEMO_ENTRIES)]),
    ?assertEqual(Full, store_perm_memo(0, 1, Full)),
    ?assertEqual(?MAX_PERM_MEMO_ENTRIES, map_size(store_perm_memo(0, 1, Full))).

memo_member_permissions_hit_test() ->
    Memo = #{9 => 123},
    ?assertEqual({123, Memo}, memo_member_permissions(9, 5, #{}, Memo)).

memo_member_permissions_miss_test() ->
    ?assertEqual({0, #{9 => 0}}, memo_member_permissions(9, 5, #{}, #{})).

-spec reference_session_can_access_message(map(), channel_id(), binary(), guild_state()) ->
    boolean().
reference_session_can_access_message(SessionData, ChannelId, MessageId, State) ->
    case maps:get(user_id, SessionData, undefined) of
        UserId when is_integer(UserId) ->
            Perms = guild_permissions:get_member_permissions(UserId, ChannelId, State),
            guild_permissions:can_access_message_by_permissions(Perms, MessageId, State);
        _ ->
            false
    end.

-spec reference_filter_message_direct(
    sessions_map(), channel_id(), binary(), session_id() | undefined, guild_state()
) -> [session_pair()].
reference_filter_message_direct(Sessions, ChannelId, MessageId, SessionIdOpt, State) ->
    filter_active_sessions(Sessions, SessionIdOpt, fun(S, _Sid) ->
        session_can_view_channel(S, ChannelId, State) andalso
            reference_session_can_access_message(S, ChannelId, MessageId, State)
    end).

filter_message_memo_matches_direct_test() ->
    State = #{data => #{<<"guild">> => #{<<"owner_id">> => <<"1">>}}},
    Owner1 = #{user_id => 1, viewable_channels => #{5 => true}},
    Owner2 = #{user_id => 1, viewable_channels => #{5 => true}},
    Other = #{user_id => 2, viewable_channels => #{5 => true}},
    Sessions = #{<<"a">> => Owner1, <<"b">> => Owner2, <<"c">> => Other},
    Direct = reference_filter_message_direct(Sessions, 5, <<"1">>, undefined, State),
    Memoized = filter_message_memo(Sessions, 5, <<"1">>, undefined, State),
    ?assertEqual(Direct, Memoized),
    ?assertEqual([<<"a">>, <<"b">>], lists:sort([Sid || {Sid, _} <- Memoized])).

filter_message_memo_collects_visible_sessions_test() ->
    State = #{data => #{<<"guild">> => #{<<"owner_id">> => <<"1">>}}},
    Owner1 = #{user_id => 1, viewable_channels => #{5 => true}},
    Owner2 = #{user_id => 1, viewable_channels => #{5 => true}},
    Other = #{user_id => 2, viewable_channels => #{5 => true}},
    Sessions = #{<<"a">> => Owner1, <<"b">> => Owner2, <<"c">> => Other},
    Memoized = filter_message_memo(Sessions, 5, <<"1">>, undefined, State),
    ?assertEqual([<<"a">>, <<"b">>], lists:sort([Sid || {Sid, _} <- Memoized])).

filter_message_memo_skips_excluded_test() ->
    State = #{data => #{<<"guild">> => #{<<"owner_id">> => <<"1">>}}},
    S1 = #{user_id => 1, viewable_channels => #{5 => true}},
    S2 = #{user_id => 1, viewable_channels => #{5 => true}, pending_connect => true},
    Sessions = #{<<"a">> => S1, <<"b">> => S2},
    ?assertEqual([], filter_message_memo(Sessions, 5, <<"1">>, <<"a">>, State)).

filter_sessions_for_message_test() ->
    State = #{data => #{<<"guild">> => #{<<"owner_id">> => <<"1">>}}},
    Sessions = #{<<"a">> => #{user_id => 1, viewable_channels => #{5 => true}}},
    ?assertEqual(
        [{<<"a">>, maps:get(<<"a">>, Sessions)}],
        filter_sessions_for_message(Sessions, 5, <<"1">>, undefined, State)
    ).

non_negative_count_test() ->
    ?assertEqual(3, non_negative_count(3)),
    ?assertEqual(0, non_negative_count(0)),
    ?assertEqual(0, non_negative_count(-1)),
    ?assertEqual(0, non_negative_count(undefined)).

active_session_count_test() ->
    Sessions = #{
        <<"a">> => #{user_id => 1, pending_connect => true},
        <<"b">> => #{user_id => 1, pending_connect => false},
        <<"c">> => #{user_id => 1},
        <<"d">> => #{user_id => 2}
    },
    ?assertEqual(2, active_session_count(1, Sessions)),
    ?assertEqual(1, active_session_count(2, Sessions)),
    ?assertEqual(0, active_session_count(3, Sessions)).

session_id_by_ref_uses_index_test() ->
    Ref = make_ref(),
    Sessions = #{<<"a">> => #{mref => make_ref()}},
    State = #{guild_session_refs => #{Ref => <<"a">>}},
    ?assertEqual(<<"a">>, session_id_by_ref(Ref, Sessions, State)).

session_id_by_ref_scan_fallback_test() ->
    Ref = make_ref(),
    Sessions = #{<<"a">> => #{mref => make_ref()}, <<"b">> => #{mref => Ref}},
    ?assertEqual(<<"b">>, session_id_by_ref(Ref, Sessions, #{})),
    ?assertEqual(undefined, session_id_by_ref(make_ref(), Sessions, #{})).

pending_session_by_ref_found_test() ->
    Ref = make_ref(),
    Session = #{user_id => 1, mref => Ref, pending_connect => true},
    Sessions = #{<<"a">> => Session},
    State = #{sessions => Sessions, guild_session_refs => #{Ref => <<"a">>}},
    ?assertEqual({ok, <<"a">>, Session, Sessions}, pending_session_by_ref(Ref, State)),
    ?assertEqual(
        {ok, <<"a">>, Session, Sessions},
        pending_session_by_ref(Ref, #{sessions => Sessions})
    ).

pending_session_by_ref_not_pending_test() ->
    Ref = make_ref(),
    Session = #{user_id => 1, mref => Ref, pending_connect => false},
    State = #{sessions => #{<<"a">> => Session}, guild_session_refs => #{Ref => <<"a">>}},
    ?assertEqual(not_found, pending_session_by_ref(Ref, State)).

pending_session_by_ref_unknown_ref_test() ->
    Sessions = #{<<"a">> => #{user_id => 1, mref => make_ref(), pending_connect => true}},
    ?assertEqual(not_found, pending_session_by_ref(make_ref(), #{sessions => Sessions})).

pending_session_by_ref_stale_index_test() ->
    Ref = make_ref(),
    State = #{sessions => #{}, guild_session_refs => #{Ref => <<"gone">>}},
    ?assertEqual(not_found, pending_session_by_ref(Ref, State)).

pending_session_owns_connected_tracking_untracked_test() ->
    Session = #{user_id => 1, pending_connect => true},
    Sessions = #{<<"a">> => Session},
    ?assertEqual(false, pending_session_owns_connected_tracking(Session, Sessions, #{})).

pending_session_owns_connected_tracking_other_session_owns_test() ->
    Session = #{user_id => 1, pending_connect => true},
    Active = #{user_id => 1, pending_connect => false},
    Sessions = #{<<"a">> => Session, <<"b">> => Active},
    State = #{user_session_counts => #{1 => 1}},
    ?assertEqual(false, pending_session_owns_connected_tracking(Session, Sessions, State)).

pending_session_owns_connected_tracking_true_test() ->
    Session = #{user_id => 1, pending_connect => true},
    Sessions = #{<<"a">> => Session},
    State = #{user_session_counts => #{1 => 1}},
    ?assertEqual(true, pending_session_owns_connected_tracking(Session, Sessions, State)).

handle_pending_session_down_keeps_tracking_test() ->
    Ref = make_ref(),
    Pending = #{session_id => <<"a">>, user_id => 1, mref => Ref, pending_connect => true},
    Active = #{session_id => <<"b">>, user_id => 1, mref => make_ref()},
    Sessions = #{<<"a">> => Pending, <<"b">> => Active},
    State = #{
        sessions => Sessions,
        guild_session_refs => #{Ref => <<"a">>},
        session_connect_pending => #{<<"a">> => #{}},
        user_session_counts => #{1 => 1},
        connected_user_ids => sets:from_list([1])
    },
    {noreply, NewState} = handle_pending_session_down(<<"a">>, Ref, Sessions, State),
    ?assertEqual(#{<<"b">> => Active}, maps:get(sessions, NewState)),
    ?assertEqual(#{}, maps:get(guild_session_refs, NewState)),
    ?assertEqual(#{}, maps:get(session_connect_pending, NewState)),
    ?assertEqual(#{1 => 1}, maps:get(user_session_counts, NewState)),
    ?assertEqual([1], sets:to_list(maps:get(connected_user_ids, NewState))),
    ?assertEqual(false, maps:is_key(auto_stop_pending, NewState)).

handle_pending_session_down_last_session_test() ->
    Ref = make_ref(),
    Pending = #{session_id => <<"a">>, user_id => 1, mref => Ref, pending_connect => true},
    Sessions = #{<<"a">> => Pending},
    State = #{
        sessions => Sessions,
        guild_session_refs => #{Ref => <<"a">>},
        disable_auto_stop_on_empty => true
    },
    {noreply, NewState} = handle_pending_session_down(<<"a">>, Ref, Sessions, State),
    ?assertEqual(#{}, maps:get(sessions, NewState)),
    ?assertEqual(#{}, maps:get(guild_session_refs, NewState)),
    ?assertEqual(false, maps:is_key(auto_stop_pending, NewState)).

-endif.
