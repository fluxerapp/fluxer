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
    set_session_push_hold/3,
    released_push_holds/2,
    handle_send_guild_sync/2,
    handle_send_members_chunk/3,
    build_viewable_channel_map/1
]).

-define(MAX_MEMO_ENTRIES, 8192).

-type guild_state() :: map().
-type session_id() :: binary().
-type user_id() :: integer().
-type guild_id() :: integer().
-type channel_id() :: integer().
-type session_data() :: map().
-type sessions_map() :: #{session_id() => session_data()}.
-type session_pair() :: {session_id(), session_data()}.
-type perm_memo() :: #{user_id() => non_neg_integer()}.
-type view_memo() :: #{user_id() => boolean()}.
-type message_ctx() ::
    {channel_id(), binary(), session_id() | undefined, boolean(), guild_state()}.
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
    case guild_sessions_connect:find_session_by_ref(Ref, State) of
        {SessionId, #{pending_connect := true} = Session} = Found ->
            handle_pending_ref_down(SessionId, Session, Ref, Found, State);
        Found ->
            guild_sessions_connect:handle_session_down(Ref, Found, State)
    end.

-spec handle_pending_ref_down(
    session_id(), session_data(), reference(), {session_id(), session_data()}, guild_state()
) -> {noreply, guild_state()} | {stop, normal, guild_state()}.
handle_pending_ref_down(SessionId, Session, Ref, Found, State) ->
    case guild_sessions_connect:counts_as_connected(Session) of
        true ->
            guild_sessions_connect:handle_session_down(Ref, Found, State);
        false ->
            handle_pending_session_down(SessionId, Ref, maps:get(sessions, State, #{}), State)
    end.

-spec handle_pending_session_down(session_id(), reference(), sessions_map(), guild_state()) ->
    {noreply, guild_state()}.
handle_pending_session_down(SessionId, Ref, Sessions, State) ->
    NewSessions = maps:remove(SessionId, Sessions),
    State1 = guild_sessions_connect:remove_session_ref(Ref, State#{sessions => NewSessions}),
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

-spec set_session_push_hold(session_id(), boolean(), guild_state()) -> guild_state().
set_session_push_hold(SessionId, Hold, State) ->
    Sessions = maps:get(sessions, State, #{}),
    case maps:get(SessionId, Sessions, undefined) of
        SessionData when is_map(SessionData) ->
            State#{sessions => Sessions#{SessionId => SessionData#{push_hold => Hold}}};
        _ ->
            State
    end.

-spec released_push_holds([session_id()], guild_state()) -> [session_id()].
released_push_holds(SessionIds, State) ->
    Sessions = maps:get(sessions, State, #{}),
    [
        SessionId
     || SessionId <- SessionIds,
        maps:get(push_hold, maps:get(SessionId, Sessions, #{}), undefined) =:= false
    ].

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
    Gated = guild_thread_gate:needs_variant(State),
    {Acc, _Memo} = maps:fold(
        fun(Sid, S, In) ->
            collect_channel_session(Sid, S, ChannelId, SessionIdOpt, Gated, State, In)
        end,
        {[], #{}},
        Sessions
    ),
    Acc.

-spec collect_channel_session(
    session_id(),
    session_data(),
    channel_id(),
    session_id() | undefined,
    boolean(),
    guild_state(),
    {[session_pair()], view_memo()}
) -> {[session_pair()], view_memo()}.
collect_channel_session(Sid, S, ChannelId, SessionIdOpt, Gated, State, {Acc, Memo}) ->
    case is_pending_or_excluded(Sid, S, SessionIdOpt) of
        true ->
            {Acc, Memo};
        false ->
            {Visible, Memo1} = memo_session_can_view_channel(Gated, S, ChannelId, State, Memo),
            {prepend_session(Visible, Sid, S, Acc), Memo1}
    end.

-spec filter_sessions_for_message(
    sessions_map(), channel_id(), binary(), session_id() | undefined, guild_state()
) -> [session_pair()].
filter_sessions_for_message(Sessions, ChannelId, MessageId, SessionIdOpt, State) ->
    filter_message_memo(Sessions, ChannelId, MessageId, SessionIdOpt, State).

-spec filter_message_memo(
    sessions_map(), channel_id(), binary(), session_id() | undefined, guild_state()
) -> [session_pair()].
filter_message_memo(Sessions, ChannelId, MessageId, SessionIdOpt, State) ->
    Ctx = {ChannelId, MessageId, SessionIdOpt, guild_thread_gate:needs_variant(State), State},
    {Acc, _ViewMemo, _PermMemo} = maps:fold(
        fun(Sid, S, In) -> collect_message_session(Sid, S, Ctx, In) end,
        {[], #{}, #{}},
        Sessions
    ),
    Acc.

-spec collect_message_session(
    session_id(), session_data(), message_ctx(), {[session_pair()], view_memo(), perm_memo()}
) -> {[session_pair()], view_memo(), perm_memo()}.
collect_message_session(Sid, S, Ctx, {Acc, ViewMemo, PermMemo}) ->
    {ChannelId, _MessageId, SessionIdOpt, Gated, State} = Ctx,
    case is_pending_or_excluded(Sid, S, SessionIdOpt) of
        true ->
            {Acc, ViewMemo, PermMemo};
        false ->
            collect_visible_message_session(
                Sid,
                S,
                Ctx,
                Acc,
                memo_session_can_view_channel(Gated, S, ChannelId, State, ViewMemo),
                PermMemo
            )
    end.

-spec collect_visible_message_session(
    session_id(),
    session_data(),
    message_ctx(),
    [session_pair()],
    {boolean(), view_memo()},
    perm_memo()
) -> {[session_pair()], view_memo(), perm_memo()}.
collect_visible_message_session(Sid, S, Ctx, Acc, {true, ViewMemo}, PermMemo) ->
    {Acc1, PermMemo1} = memo_message_session(Sid, S, Ctx, Acc, PermMemo),
    {Acc1, ViewMemo, PermMemo1};
collect_visible_message_session(_Sid, _S, _Ctx, Acc, {false, ViewMemo}, PermMemo) ->
    {Acc, ViewMemo, PermMemo}.

-spec memo_message_session(
    session_id(), session_data(), message_ctx(), [session_pair()], perm_memo()
) -> {[session_pair()], perm_memo()}.
memo_message_session(Sid, S, {ChannelId, MessageId, _SessionIdOpt, _Gated, State}, Acc, Memo) ->
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
            {Computed, store_memo(UserId, Computed, Memo)}
    end.

-spec store_memo(user_id(), V, #{user_id() => V}) -> #{user_id() => V}.
store_memo(UserId, Value, Memo) when map_size(Memo) < ?MAX_MEMO_ENTRIES ->
    Memo#{UserId => Value};
store_memo(_UserId, _Value, Memo) ->
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

-spec memo_session_can_view_channel(
    boolean(), session_data(), channel_id(), guild_state(), view_memo()
) ->
    {boolean(), view_memo()}.
memo_session_can_view_channel(false, SessionData, ChannelId, State, Memo) ->
    memo_session_can_view_channel_by_permissions(SessionData, ChannelId, State, Memo);
memo_session_can_view_channel(true, SessionData, ChannelId, State, Memo) ->
    case guild_thread_gate:channel_visible(SessionData, ChannelId, State) of
        true ->
            memo_session_can_view_channel_by_permissions(SessionData, ChannelId, State, Memo);
        false ->
            {false, Memo}
    end.

-spec memo_session_can_view_channel_by_permissions(
    session_data(), channel_id(), guild_state(), view_memo()
) ->
    {boolean(), view_memo()}.
memo_session_can_view_channel_by_permissions(SessionData, ChannelId, State, Memo) ->
    UserId = maps:get(user_id, SessionData, undefined),
    case {UserId, maps:get(viewable_channels, SessionData, undefined)} of
        {Uid, ViewableChannels} when is_integer(Uid), is_map(ViewableChannels) ->
            case maps:is_key(ChannelId, ViewableChannels) of
                true -> {true, Memo};
                false -> memo_member_channel_access(Uid, ChannelId, State, Memo)
            end;
        {Uid, _} when is_integer(Uid) ->
            memo_member_channel_access(Uid, ChannelId, State, Memo);
        _ ->
            {false, Memo}
    end.

-spec memo_member_channel_access(user_id(), channel_id(), guild_state(), view_memo()) ->
    {boolean(), view_memo()}.
memo_member_channel_access(UserId, ChannelId, State, Memo) ->
    case maps:get(UserId, Memo, undefined) of
        Visible when is_boolean(Visible) ->
            {Visible, Memo};
        _ ->
            Computed = check_member_channel_access(UserId, ChannelId, State),
            {Computed, store_memo(UserId, Computed, Memo)}
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

store_memo_test() ->
    ?assertEqual(#{7 => 42}, store_memo(7, 42, #{})),
    ?assertEqual(#{7 => 42}, store_memo(7, 42, #{7 => 42})).

store_memo_bound_test() ->
    Full = maps:from_list([{I, 0} || I <- lists:seq(1, ?MAX_MEMO_ENTRIES)]),
    ?assertEqual(Full, store_memo(0, 1, Full)),
    ?assertEqual(?MAX_MEMO_ENTRIES, map_size(store_memo(0, 1, Full))).

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

-spec reference_session_can_view_channel(session_data(), channel_id(), guild_state()) ->
    boolean().
reference_session_can_view_channel(SessionData, ChannelId, State) ->
    guild_thread_gate:channel_visible(SessionData, ChannelId, State) andalso
        reference_session_can_view_channel_by_permissions(SessionData, ChannelId, State).

-spec reference_session_can_view_channel_by_permissions(
    session_data(), channel_id(), guild_state()
) ->
    boolean().
reference_session_can_view_channel_by_permissions(SessionData, ChannelId, State) ->
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

-spec reference_filter_channel_direct(
    sessions_map(), channel_id(), session_id() | undefined, guild_state()
) -> [session_pair()].
reference_filter_channel_direct(Sessions, ChannelId, SessionIdOpt, State) ->
    filter_active_sessions(Sessions, SessionIdOpt, fun(S, _Sid) ->
        reference_session_can_view_channel(S, ChannelId, State)
    end).

-spec reference_filter_message_direct(
    sessions_map(), channel_id(), binary(), session_id() | undefined, guild_state()
) -> [session_pair()].
reference_filter_message_direct(Sessions, ChannelId, MessageId, SessionIdOpt, State) ->
    filter_active_sessions(Sessions, SessionIdOpt, fun(S, _Sid) ->
        reference_session_can_view_channel(S, ChannelId, State) andalso
            reference_session_can_access_message(S, ChannelId, MessageId, State)
    end).

memo_member_channel_access_hit_test() ->
    Memo = #{1001 => true},
    ?assertEqual({true, Memo}, memo_member_channel_access(1001, 10, #{}, Memo)).

memo_member_channel_access_miss_test() ->
    ?assertEqual({false, #{1001 => false}}, memo_member_channel_access(1001, 10, #{}, #{})).

memo_session_can_view_channel_listed_skips_memo_test() ->
    Session = #{user_id => 1001, viewable_channels => #{10 => true}},
    ?assertEqual({true, #{}}, memo_session_can_view_channel(false, Session, 10, #{}, #{})).

memo_session_can_view_channel_without_user_test() ->
    Session = #{viewable_channels => #{10 => true}},
    ?assertEqual({false, #{}}, memo_session_can_view_channel(false, Session, 10, #{}, #{})).

channel_filters_match_per_session_reference_test() ->
    State = visibility_fixture_state(),
    Sessions = maps:get(sessions, State),
    Excludes = [undefined | lists:sublist(lists:sort(maps:keys(Sessions)), 3)],
    Results = [
        assert_filters_match_reference(Sessions, ChannelId, Exclude, State)
     || ChannelId <- visibility_fixture_channels(), Exclude <- Excludes
    ],
    Visible = lists:append([Sids || {Sids, _} <- Results]),
    ?assert(lists:member(<<"u1002-stale">>, Visible)),
    ?assert(lists:member(<<"u1005-nomap">>, Visible)),
    ?assert(lists:member(<<"u1007-empty">>, Visible)),
    ?assertNot(lists:member(<<"nouser">>, Visible)),
    ?assert(lists:any(fun({Sids, _}) -> Sids =/= [] end, Results)),
    ?assert(lists:any(fun({Sids, Msg}) -> length(Msg) < length(Sids) end, Results)).

assert_filters_match_reference(Sessions, ChannelId, Exclude, State) ->
    Channel = filter_sessions_for_channel(Sessions, ChannelId, Exclude, State),
    ?assertEqual(reference_filter_channel_direct(Sessions, ChannelId, Exclude, State), Channel),
    MessageId = <<"1430000000000000000">>,
    Message = filter_sessions_for_message(Sessions, ChannelId, MessageId, Exclude, State),
    ?assertEqual(
        reference_filter_message_direct(Sessions, ChannelId, MessageId, Exclude, State), Message
    ),
    {[Sid || {Sid, _} <- Channel], [Sid || {Sid, _} <- Message]}.

visibility_fixture_channels() ->
    [10, 20, 30, 40, 50, 60, 61, 70].

visibility_fixture_state() ->
    View = constants:view_channel_permission(),
    History = constants:read_message_history_permission(),
    Deny = fun(Id, Type, Bits) ->
        #{
            <<"id">> => Id,
            <<"type">> => Type,
            <<"allow">> => <<"0">>,
            <<"deny">> => integer_to_binary(Bits)
        }
    end,
    Allow = fun(Id, Type, Bits) ->
        #{
            <<"id">> => Id,
            <<"type">> => Type,
            <<"allow">> => integer_to_binary(Bits),
            <<"deny">> => <<"0">>
        }
    end,
    Channel = fun(Id, Type, Parent, Overwrites) ->
        #{
            <<"id">> => integer_to_binary(Id),
            <<"type">> => Type,
            <<"parent_id">> => Parent,
            <<"permission_overwrites">> => Overwrites
        }
    end,
    Member = fun(UserId, Roles) ->
        #{<<"user">> => #{<<"id">> => integer_to_binary(UserId)}, <<"roles">> => Roles}
    end,
    Data = #{
        <<"guild">> => #{
            <<"id">> => <<"1">>, <<"owner_id">> => <<"999">>, <<"features">> => []
        },
        <<"roles">> => [
            #{<<"id">> => <<"1">>, <<"permissions">> => integer_to_binary(View bor History)},
            #{<<"id">> => <<"200">>, <<"permissions">> => <<"0">>},
            #{<<"id">> => <<"300">>, <<"permissions">> => <<"0">>}
        ],
        <<"members">> => [
            Member(999, []),
            Member(1001, []),
            Member(1002, [<<"300">>]),
            Member(1003, [<<"200">>]),
            Member(1004, [<<"200">>, <<"300">>]),
            Member(1005, []),
            Member(1006, [<<"300">>]),
            Member(1007, [])
        ],
        <<"channels">> => [
            Channel(10, 0, null, []),
            Channel(20, 0, null, [Deny(<<"1">>, 0, View), Allow(<<"300">>, 0, View)]),
            Channel(30, 0, null, [Deny(<<"1">>, 0, View), Allow(<<"1005">>, 1, View)]),
            Channel(40, 0, null, [Deny(<<"200">>, 0, View)]),
            Channel(50, 0, null, [Deny(<<"1">>, 0, History), Allow(<<"300">>, 0, History)]),
            Channel(60, 4, null, [Deny(<<"1">>, 0, View)]),
            Channel(61, 0, <<"60">>, [Allow(<<"300">>, 0, View)])
        ]
    },
    Sessions = #{
        <<"owner">> => #{user_id => 999, viewable_channels => #{}},
        <<"u1001-a">> => #{user_id => 1001, viewable_channels => #{10 => true, 40 => true}},
        <<"u1001-b">> => #{user_id => 1001},
        <<"u1001-pending">> => #{user_id => 1001, pending_connect => true},
        <<"u1002-stale">> => #{user_id => 1002, viewable_channels => #{}},
        <<"u1002-full">> => #{user_id => 1002, viewable_channels => #{10 => true, 20 => true}},
        <<"u1003-a">> => #{user_id => 1003, viewable_channels => #{10 => true, 40 => true}},
        <<"u1003-b">> => #{user_id => 1003, viewable_channels => #{}},
        <<"u1004">> => #{user_id => 1004, viewable_channels => #{20 => true}},
        <<"u1005-nomap">> => #{user_id => 1005},
        <<"u1005-map">> => #{user_id => 1005, viewable_channels => #{10 => true}},
        <<"u1006">> => #{user_id => 1006, viewable_channels => #{61 => true}},
        <<"u1007-empty">> => #{user_id => 1007, viewable_channels => #{}},
        <<"u1008-nonmember">> => #{user_id => 1008, viewable_channels => #{}},
        <<"u1008-listed">> => #{user_id => 1008, viewable_channels => #{50 => true}},
        <<"nouser">> => #{viewable_channels => #{10 => true}}
    },
    #{
        id => 1,
        sessions => Sessions,
        data => Data,
        virtual_channel_access => #{1007 => sets:from_list([20])}
    }.

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
