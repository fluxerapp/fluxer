%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_subscription_mutual_channels).

-typing([eqwalizer]).

-export([filter_member_ids/4, filter_session_member_ids/2, filter_session_member_ids/3]).

-type guild_state() :: map().
-type user_id() :: integer().
-type memo() :: #{exceptions := sets:set(user_id()), cache := #{term() => boolean()}}.
-type view_cache() :: #{term() => [integer()]}.
-type view_memo() :: #{exceptions => sets:set(user_id()), views => view_cache()}.
-export_type([guild_state/0, user_id/0, view_memo/0]).

-spec filter_member_ids(map(), user_id(), [user_id()], guild_state()) -> [user_id()].
filter_member_ids(_Session, _SessionUserId, [], _State) ->
    [];
filter_member_ids(Session, SessionUserId, MemberIds, State) ->
    SessionMap = visible_session_map(
        Session#{user_id => SessionUserId}, session_channel_map(SessionUserId, State), State
    ),
    {Kept, _Memo} = lists:foldl(
        fun(MemberId, Acc) ->
            keep_member_with_mutual_channel(MemberId, SessionUserId, SessionMap, State, Acc)
        end,
        {[], new_memo(State)},
        MemberIds
    ),
    lists:reverse(Kept).

-spec filter_session_member_ids([{term(), user_id(), [term()]}], guild_state()) ->
    #{term() => [user_id()]}.
filter_session_member_ids(Requests, State) ->
    {Results, _Memo} = filter_session_member_ids(Requests, #{}, State),
    Results.

-spec filter_session_member_ids([{term(), user_id(), [term()]}], view_memo(), guild_state()) ->
    {#{term() => [user_id()]}, view_memo()}.
filter_session_member_ids([], Memo, _State) ->
    {#{}, Memo};
filter_session_member_ids(Requests, Memo, State) ->
    Exceptions =
        case Memo of
            #{exceptions := Cached} -> Cached;
            _ -> guild_viewable_memo:exceptions(State)
        end,
    Views = maps:get(views, Memo, #{}),
    Sessions = maps:get(sessions, State, #{}),
    {Results, Cache} = lists:foldl(
        fun({SessionId, SessionUserId, MemberIds}, {Acc, Cache}) ->
            SessionMap = request_session_map(SessionId, SessionUserId, Sessions, State),
            {Kept, Cache1} = keep_session_members(
                MemberIds, SessionUserId, SessionMap, Exceptions, State, Cache
            ),
            {Acc#{SessionId => Kept}, Cache1}
        end,
        {#{}, Views},
        Requests
    ),
    {Results, #{exceptions => Exceptions, views => role_views(Cache)}}.

-spec role_views(view_cache()) -> view_cache().
role_views(Cache) ->
    maps:filter(fun(Key, _Channels) -> is_role_key(Key) end, Cache).

-spec is_role_key(term()) -> boolean().
is_role_key({roles, _RawRoles}) -> true;
is_role_key(_Key) -> false.

-spec request_session_map(term(), user_id(), map(), guild_state()) -> map().
request_session_map(SessionId, SessionUserId, Sessions, State) ->
    Session =
        case maps:get(SessionId, Sessions, undefined) of
            Found when is_map(Found) -> Found;
            _ -> #{}
        end,
    visible_session_map(
        Session#{user_id => SessionUserId},
        raw_request_session_map(SessionId, SessionUserId, Sessions, State),
        State
    ).

-spec visible_session_map(map(), map(), guild_state()) -> map().
visible_session_map(Session, Map, State) ->
    case
        guild_thread_gate:needs_variant(State) andalso
            not guild_thread_gate:session_viewer(Session)
    of
        true -> visible_channels_only(Session, Map, State);
        false -> Map
    end.

-spec visible_channels_only(map(), map(), guild_state()) -> map().
visible_channels_only(Session, Map, State) ->
    maps:filter(
        fun(ChannelId, _) -> guild_thread_gate:channel_visible(Session, ChannelId, State) end,
        Map
    ).

-spec raw_request_session_map(term(), user_id(), map(), guild_state()) -> map().
raw_request_session_map(SessionId, SessionUserId, Sessions, State) ->
    case maps:get(SessionId, Sessions, undefined) of
        #{user_id := SessionUserId, viewable_channels := Map} = SessionData when is_map(Map) ->
            settled_session_map(
                maps:get(pending_connect, SessionData, false), SessionUserId, Map, State
            );
        _ ->
            session_channel_map(SessionUserId, State)
    end.

-spec settled_session_map(term(), user_id(), map(), guild_state()) -> map().
settled_session_map(true, SessionUserId, _Map, State) ->
    session_channel_map(SessionUserId, State);
settled_session_map(_Pending, _SessionUserId, Map, _State) ->
    Map.

-spec keep_session_members(
    [term()], user_id(), map(), sets:set(user_id()), guild_state(), #{term() => [integer()]}
) -> {[user_id()], #{term() => [integer()]}}.
keep_session_members(MemberIds, SessionUserId, SessionMap, Exceptions, State, Cache) ->
    {Kept, _Seen, Cache1} = lists:foldl(
        fun(MemberId, Acc) ->
            keep_session_member(MemberId, Acc, SessionUserId, SessionMap, Exceptions, State)
        end,
        {[], #{}, Cache},
        MemberIds
    ),
    {lists:reverse(Kept), Cache1}.

-spec keep_session_member(
    term(),
    {[user_id()], map(), #{term() => [integer()]}},
    user_id(),
    map(),
    sets:set(user_id()),
    guild_state()
) -> {[user_id()], map(), #{term() => [integer()]}}.
keep_session_member(MemberId, Acc, SessionUserId, _SessionMap, _Exceptions, _State) when
    MemberId =:= SessionUserId; not is_integer(MemberId)
->
    Acc;
keep_session_member(
    MemberId, {KeptAcc, Seen, CacheAcc}, _SessionUserId, SessionMap, Exceptions, State
) ->
    {Key, CacheAcc1} = member_channel_key(MemberId, Exceptions, State, CacheAcc),
    case maps:find(Key, Seen) of
        {ok, true} ->
            {[MemberId | KeptAcc], Seen, CacheAcc1};
        {ok, false} ->
            {KeptAcc, Seen, CacheAcc1};
        error ->
            Shared = has_shared_channel(maps:get(Key, CacheAcc1), SessionMap),
            {kept_if(Shared, MemberId, KeptAcc), Seen#{Key => Shared}, CacheAcc1}
    end.

-spec kept_if(boolean(), user_id(), [user_id()]) -> [user_id()].
kept_if(true, MemberId, Kept) -> [MemberId | Kept];
kept_if(false, _MemberId, Kept) -> Kept.

-spec member_channel_key(user_id(), sets:set(user_id()), guild_state(), #{term() => [integer()]}) ->
    {term(), #{term() => [integer()]}}.
member_channel_key(MemberId, Exceptions, State, Cache) ->
    Key =
        case guild_viewable_memo:memo_key(MemberId, Exceptions, State) of
            bypass -> {user, MemberId};
            {ok, RawRoles} -> {roles, RawRoles}
        end,
    case maps:is_key(Key, Cache) of
        true ->
            {Key, Cache};
        false ->
            {Key, Cache#{
                Key => guild_visibility_channels:get_user_viewable_channels(MemberId, State)
            }}
    end.

-spec session_channel_map(user_id(), guild_state()) -> map().
session_channel_map(SessionUserId, State) ->
    case guild_visibility_channels:get_cached_viewable_channel_map(SessionUserId, State) of
        undefined ->
            guild_sessions:build_viewable_channel_map(
                guild_visibility_channels:get_user_viewable_channels(SessionUserId, State)
            );
        Map ->
            Map
    end.

-spec keep_member_with_mutual_channel(
    term(), user_id(), map(), guild_state(), {[user_id()], memo()}
) -> {[user_id()], memo()}.
keep_member_with_mutual_channel(MemberId, SessionUserId, _SessionMap, _State, Acc) when
    MemberId =:= SessionUserId; not is_integer(MemberId)
->
    Acc;
keep_member_with_mutual_channel(MemberId, _SessionUserId, SessionMap, State, {Kept, Memo}) ->
    case memoised_has_mutual_channel(MemberId, SessionMap, State, Memo) of
        {true, Memo1} -> {[MemberId | Kept], Memo1};
        {false, Memo1} -> {Kept, Memo1}
    end.

%% get_user_viewable_channels/2 reads nothing from a member except its raw roles
%% term, so two members holding the same term see the same channels and give the same
%% answer against a fixed session map. The user-specific inputs are the guild owner,
%% virtual channel access and user-type permission overwrites, and every user named
%% by one of those bypasses the memo.
-spec new_memo(guild_state()) -> memo().
new_memo(State) ->
    #{exceptions => guild_viewable_memo:exceptions(State), cache => #{}}.

-spec memoised_has_mutual_channel(user_id(), map(), guild_state(), memo()) ->
    {boolean(), memo()}.
memoised_has_mutual_channel(MemberId, SessionMap, State, Memo) ->
    #{exceptions := Exceptions, cache := Cache} = Memo,
    case guild_viewable_memo:memo_key(MemberId, Exceptions, State) of
        bypass ->
            {has_mutual_channel(MemberId, SessionMap, State), Memo};
        {ok, RawRoles} ->
            cached_has_mutual_channel(MemberId, RawRoles, SessionMap, State, Cache, Memo)
    end.

-spec cached_has_mutual_channel(
    user_id(), term(), map(), guild_state(), #{term() => boolean()}, memo()
) -> {boolean(), memo()}.
cached_has_mutual_channel(MemberId, RawRoles, SessionMap, State, Cache, Memo) ->
    case maps:find(RawRoles, Cache) of
        {ok, HasMutual} ->
            {HasMutual, Memo};
        error ->
            HasMutual = has_mutual_channel(MemberId, SessionMap, State),
            {HasMutual, Memo#{cache := Cache#{RawRoles => HasMutual}}}
    end.

-spec has_mutual_channel(user_id(), map(), guild_state()) -> boolean().
has_mutual_channel(MemberId, SessionMap, State) ->
    guild_visibility_channels:shares_viewable_channel(MemberId, SessionMap, State).

-spec has_shared_channel([integer()], map()) -> boolean().
has_shared_channel(MemberChannels, SessionMap) ->
    lists:any(fun(Ch) -> maps:is_key(Ch, SessionMap) end, MemberChannels).
