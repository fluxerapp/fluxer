%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_sessions).
-typing([eqwalizer]).

-export([
    handle_session_down/2,
    filter_sessions_for_channel/4,
    filter_sessions_for_message/5,
    filter_sessions_for_manage_channels/4,
    filter_sessions_exclude_session/2,
    set_session_viewable_channels/3,
    refresh_user_session_cache/2,
    refresh_all_viewable_channels/1,
    set_session_push_hold/3,
    released_push_holds/2,
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

-spec handle_session_down(reference(), guild_state()) ->
    {noreply, guild_state()} | {stop, normal, guild_state()}.
handle_session_down(Ref, State) ->
    case guild_sessions_connect:find_session_by_ref(Ref, State) of
        {SessionId, #{pending_connect := true} = Session} = Found when is_binary(SessionId) ->
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

-spec set_session_push_hold(session_id(), boolean(), guild_state()) -> guild_state().
set_session_push_hold(SessionId, Hold, State) ->
    Sessions = maps:get(sessions, State, #{}),
    case maps:get(SessionId, Sessions, undefined) of
        SessionData when is_map(SessionData) ->
            State#{sessions => Sessions#{SessionId => SessionData#{push_hold => Hold}}};
        _ ->
            State
    end.

-spec released_push_holds([Id], guild_state()) -> [Id] when Id :: term().
released_push_holds(SessionIds, State) ->
    Sessions = maps:get(sessions, State, #{}),
    [
        SessionId
     || SessionId <- SessionIds,
        push_hold_released(maps:get(SessionId, Sessions, #{}))
    ].

-spec push_hold_released(map()) -> boolean().
push_hold_released(SessionData) ->
    case maps:get(push_hold, SessionData, undefined) of
        false -> true;
        _ -> false
    end.

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
            Ok = guild_permissions_check:can_access_message_by_permissions(
                Perms, MessageId, State
            ),
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
        guild_permissions_check:can_manage_channel(UserId, ChannelId, State)
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
        guild_visibility_channels:get_user_viewable_channels(UserId, State)
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
    guild_viewable_channels_cache:invalidate(State),
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
                guild_visibility_channels:get_user_viewable_channels(UserId, AccState)
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
        {Uid, ViewableChannels} when
            is_integer(Uid), is_map(ViewableChannels), is_map_key(ChannelId, ViewableChannels)
        ->
            {true, Memo};
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
    Member = guild_permissions_check:find_member_by_user_id(UserId, State),
    case Member of
        undefined -> false;
        _ -> guild_permissions_check:can_view_channel(UserId, ChannelId, Member, State)
    end.

-spec build_viewable_channel_map([channel_id()]) -> #{channel_id() => true}.
build_viewable_channel_map(ChannelIds) ->
    lists:foldl(
        fun(ChannelId, Acc) -> Acc#{ChannelId => true} end,
        #{},
        ChannelIds
    ).
