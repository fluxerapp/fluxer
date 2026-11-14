%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_mutual_online).

-typing([eqwalizer]).

-export([compute_count/2]).

-type user_id() :: integer().
-type guild_state() :: map().
-type viewable_index() :: #{user_id() => map()}.
-type index_ctx() :: {map(), viewable_index(), guild_state()}.
-type memo() :: guild_viewable_memo:memo().

-spec compute_count(term(), map()) -> non_neg_integer().
compute_count(UserId, State) when is_integer(UserId), UserId > 0 ->
    case viewer_sees_everything(UserId, State) of
        true ->
            guild_member_list_read:get_online_count(State);
        false ->
            slow_count(UserId, State)
    end;
compute_count(_, _) ->
    0.

-spec viewer_sees_everything(user_id(), guild_state()) -> boolean().
viewer_sees_everything(UserId, State) ->
    Perms = guild_permissions:get_member_permissions(UserId, undefined, State),
    bitset:has(Perms, constants:administrator_permission()).

-spec slow_count(user_id(), guild_state()) -> non_neg_integer().
slow_count(UserId, State) ->
    ViewerSet = guild_visibility_channels:viewable_channel_set(UserId, State),
    case sets:is_empty(ViewerSet) of
        true ->
            self_online_count(UserId, State);
        false ->
            count_mutually_visible_indexed(UserId, ViewerSet, State)
    end.

-spec self_online_count(user_id(), guild_state()) -> non_neg_integer().
self_online_count(UserId, State) ->
    case is_self_online(UserId, State) of
        true -> 1;
        false -> 0
    end.

-spec count_mutually_visible_indexed(user_id(), sets:set(), guild_state()) ->
    non_neg_integer().
count_mutually_visible_indexed(UserId, ViewerSet, State) ->
    {Count, _Memo} = count_and_memo(UserId, ViewerSet, State),
    Count.

-spec count_and_memo(user_id(), sets:set(), guild_state()) ->
    {non_neg_integer(), memo() | undefined}.
count_and_memo(UserId, ViewerSet, State) ->
    Tab = maps:get(member_presence, State),
    Ctx = {viewer_channel_map(ViewerSet, State), build_viewable_index(State), State},
    sets:fold(
        fun(OtherUserId, Acc) ->
            Presence = guild_state_member:lookup_presence(Tab, OtherUserId),
            count_online_member_indexed(UserId, Ctx, OtherUserId, Presence, Acc)
        end,
        {0, undefined},
        guild_member_list_connected:connected_session_user_ids(State)
    ).

-spec count_online_member_indexed(
    user_id(), index_ctx(), term(), term(), {non_neg_integer(), memo() | undefined}
) -> {non_neg_integer(), memo() | undefined}.
count_online_member_indexed(UserId, Ctx, OtherUserId, Presence, {Count, Memo}) when
    is_integer(OtherUserId), is_map(Presence), OtherUserId > 0
->
    {Visible, Memo1} =
        case is_online(Presence) of
            false -> {false, Memo};
            true when OtherUserId =:= UserId -> {true, Memo};
            true -> shares_viewable_channel(OtherUserId, Ctx, Memo)
        end,
    case Visible of
        true -> {Count + 1, Memo1};
        false -> {Count, Memo1}
    end;
count_online_member_indexed(_UserId, _Ctx, _OtherUserId, _Presence, Acc) ->
    Acc.

-spec shares_viewable_channel(user_id(), index_ctx(), memo() | undefined) ->
    {boolean(), memo() | undefined}.
shares_viewable_channel(OtherUserId, {ViewerMap, Index, State}, Memo) ->
    case maps:find(OtherUserId, Index) of
        {ok, OtherMap} ->
            {map_utils:maps_share_any_key(OtherMap, ViewerMap), Memo};
        error ->
            {OtherMap, Memo1} = guild_viewable_memo:memoised_channel_map(
                OtherUserId, State, ensure_memo(Memo, State)
            ),
            {map_utils:maps_share_any_key(OtherMap, ViewerMap), Memo1}
    end.

-spec ensure_memo(memo() | undefined, guild_state()) -> memo().
ensure_memo(undefined, State) ->
    guild_viewable_memo:new(State);
ensure_memo(Memo, _State) ->
    Memo.

-spec viewer_channel_map(sets:set(), guild_state()) -> map().
viewer_channel_map(ViewerSet, State) ->
    Index = guild_data_index_channels:channel_index(maps:get(data, State)),
    SkipThreadOnly = guild_thread_gate:needs_variant(State),
    sets:fold(
        fun(ChannelId, Acc) -> put_shared(ChannelId, Index, SkipThreadOnly, Acc) end,
        #{},
        ViewerSet
    ).

-spec put_shared(term(), map(), boolean(), map()) -> map().
put_shared(ChannelId, Index, SkipThreadOnly, Acc) ->
    Type =
        case maps:get(ChannelId, Index, undefined) of
            #{<<"type">> := ChannelType} -> ChannelType;
            _ -> undefined
        end,
    Shared =
        not channel_types:is_category(Type) andalso
            not (SkipThreadOnly andalso channel_types:is_thread_only(Type)),
    case Shared of
        true -> Acc#{ChannelId => true};
        false -> Acc
    end.

-spec build_viewable_index(guild_state()) -> viewable_index().
build_viewable_index(State) ->
    build_index_iter(maps:iterator(maps:get(sessions, State)), #{}).

-spec build_index_iter(maps:iterator(), viewable_index()) -> viewable_index().
build_index_iter(Iterator, Acc) ->
    case maps:next(Iterator) of
        none ->
            Acc;
        {_, #{user_id := UserId, viewable_channels := ViewableChannels}, Next} when
            is_integer(UserId), is_map(ViewableChannels)
        ->
            build_index_iter(Next, Acc#{UserId => maps:get(UserId, Acc, ViewableChannels)});
        {_, _, Next} ->
            build_index_iter(Next, Acc)
    end.

-spec is_self_online(user_id(), guild_state()) -> boolean().
is_self_online(UserId, State) ->
    Connected = guild_member_list_connected:connected_session_user_ids(State),
    sets:is_element(UserId, Connected) andalso
        is_online(guild_state_member:lookup_presence(maps:get(member_presence, State), UserId)).

-spec is_online(map()) -> boolean().
is_online(Presence) ->
    Status = maps:get(<<"status">>, Presence, <<"offline">>),
    Status =/= <<"offline">> andalso Status =/= <<"invisible">>.
