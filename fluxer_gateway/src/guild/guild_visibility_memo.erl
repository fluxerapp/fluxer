%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_visibility_memo).
-typing([eqwalizer]).

-export([new/3, pairs/5, categories/4]).

-export_type([memo/0, pairs/0, category_visibility/0, guild_state/0, channel_id/0]).

-type guild_state() :: map().
-type user_id() :: integer().
-type channel_id() :: integer().
-type visibility() :: #{channel_id() => {boolean(), boolean()}}.
-type settled() :: {#{channel_id() => true}, [channel_id()]} | unsettled.
-type pairs() :: {visibility(), settled()}.
-type category_visibility() :: {channel_id(), boolean(), boolean(), boolean()}.
-type child_index() :: #{channel_id() => [term()]}.
-type memo() :: #{
    exceptions := sets:set(user_id()),
    unchanged := #{channel_id() => true},
    pairs := #{term() => pairs()},
    candidates := [channel_id()],
    children := {child_index(), child_index()},
    categories := #{term() => [category_visibility()]}
}.

-spec new([channel_id()], guild_state(), guild_state()) -> memo().
new(ChannelIds, OldState, NewState) ->
    Candidates = candidate_categories(ChannelIds, OldState, NewState),
    #{
        exceptions => sets:union(
            guild_viewable_memo:exceptions(OldState),
            guild_viewable_memo:exceptions(NewState)
        ),
        unchanged => maps:from_keys(
            [
                Id
             || Id <- ChannelIds,
                guild_permissions_check:view_inputs(Id, OldState) =:=
                    guild_permissions_check:view_inputs(Id, NewState)
            ],
            true
        ),
        pairs => #{},
        candidates => Candidates,
        children => {child_index(Candidates, OldState), child_index(Candidates, NewState)},
        categories => #{}
    }.

-spec pairs([channel_id()], guild_state(), guild_state(), map(), memo()) ->
    {pairs() | none, memo()}.
pairs(ChannelIds, OldState, NewState, ChangeContext, Memo) ->
    #{user_id := UserId, old_member := OldMember, new_member := NewMember} = ChangeContext,
    #{exceptions := Exceptions} = Memo,
    case {sets:is_element(UserId, Exceptions), OldMember, NewMember} of
        {false, #{}, #{}} ->
            Key = {maps:get(<<"roles">>, OldMember, []), maps:get(<<"roles">>, NewMember, [])},
            cached_pairs(Key, ChannelIds, {OldState, NewState}, ChangeContext, Memo);
        _ ->
            {none, Memo}
    end.

-spec cached_pairs(
    {term(), term()}, [channel_id()], {guild_state(), guild_state()}, map(), memo()
) ->
    {pairs(), memo()}.
cached_pairs(Key, ChannelIds, {OldState, NewState}, ChangeContext, Memo) ->
    #{unchanged := Unchanged, pairs := Cache} = Memo,
    case maps:find(Key, Cache) of
        {ok, Found} ->
            {Found, Memo};
        error ->
            Reusable = reusable_channels(Key, Unchanged),
            Computed = compute(ChannelIds, OldState, NewState, Reusable, ChangeContext),
            {Computed, Memo#{pairs := Cache#{Key => Computed}}}
    end.

-spec reusable_channels({term(), term()}, #{channel_id() => true}) -> #{channel_id() => true}.
reusable_channels({Same, Same}, Unchanged) -> Unchanged;
reusable_channels(_Key, _Unchanged) -> #{}.

-spec compute([channel_id()], guild_state(), guild_state(), #{channel_id() => true}, map()) ->
    pairs().
compute(ChannelIds, OldState, NewState, Reusable, ChangeContext) ->
    Visibility = maps:from_list([
        {ChannelId, visibility(ChannelId, OldState, NewState, Reusable, ChangeContext)}
     || ChannelId <- ChannelIds
    ]),
    {Visibility, settle(Visibility)}.

-spec visibility(channel_id(), guild_state(), guild_state(), #{channel_id() => true}, map()) ->
    {boolean(), boolean()}.
visibility(ChannelId, OldState, NewState, Reusable, ChangeContext) ->
    #{user_id := UserId, old_member := OldMember, new_member := NewMember} = ChangeContext,
    OldVisible = guild_permissions_check:can_view_channel(
        UserId, ChannelId, OldMember, OldState
    ),
    case maps:is_key(ChannelId, Reusable) of
        true ->
            {OldVisible, OldVisible};
        false ->
            {OldVisible,
                guild_permissions_check:can_view_channel(
                    UserId, ChannelId, NewMember, NewState
                )}
    end.

-spec settle(visibility()) -> settled().
settle(Visibility) ->
    case all_unchanged(Visibility) of
        true ->
            {
                maps:from_keys([Id || {Id, {_, true}} <- maps:to_list(Visibility)], true),
                [Id || {Id, {_, false}} <- maps:to_list(Visibility)]
            };
        false ->
            unsettled
    end.

-spec all_unchanged(visibility()) -> boolean().
all_unchanged(Visibility) ->
    lists:all(fun({Old, New}) -> Old =:= New end, maps:values(Visibility)).

-spec categories(guild_state(), guild_state(), map(), memo()) ->
    {[category_visibility()], memo()}.
categories(OldState, NewState, ChangeContext, Memo) ->
    #{
        user_id := UserId,
        old_member := OldMember,
        new_member := NewMember,
        connected_set := ConnectedSet
    } = ChangeContext,
    #{exceptions := Exceptions, categories := Cache} = Memo,
    Shared =
        not sets:is_element(UserId, Exceptions) andalso is_map(OldMember) andalso
            is_map(NewMember) andalso sets:is_empty(ConnectedSet),
    case Shared of
        true ->
            Key = {maps:get(<<"roles">>, OldMember, []), maps:get(<<"roles">>, NewMember, [])},
            cached_categories(Key, Cache, {OldState, NewState}, ChangeContext, Memo);
        false ->
            {compute_categories(OldState, NewState, ChangeContext, Memo), Memo}
    end.

-spec cached_categories(
    {term(), term()},
    #{term() => [category_visibility()]},
    {guild_state(), guild_state()},
    map(),
    memo()
) -> {[category_visibility()], memo()}.
cached_categories(Key, Cache, {OldState, NewState}, ChangeContext, Memo) ->
    case maps:find(Key, Cache) of
        {ok, Found} ->
            {Found, Memo};
        error ->
            Computed = compute_categories(OldState, NewState, ChangeContext, Memo),
            {Computed, Memo#{categories := Cache#{Key => Computed}}}
    end.

-spec compute_categories(guild_state(), guild_state(), map(), memo()) ->
    [category_visibility()].
compute_categories(OldState, NewState, ChangeContext, Memo) ->
    #{user_id := UserId, old_member := OldMember, new_member := NewMember} = ChangeContext,
    #{candidates := Candidates, children := {OldChildren, NewChildren}} = Memo,
    [
        category_visibility(
            CategoryId,
            {UserId, OldMember, NewMember},
            {OldState, NewState},
            {OldChildren, NewChildren}
        )
     || CategoryId <- Candidates
    ].

-spec category_visibility(
    channel_id(),
    {user_id(), term(), term()},
    {guild_state(), guild_state()},
    {child_index(), child_index()}
) -> category_visibility().
category_visibility(
    CategoryId, {UserId, OldMember, NewMember}, {OldState, NewState}, {OldChildren, NewChildren}
) ->
    OwnOld = guild_permissions_check:can_view_channel(UserId, CategoryId, OldMember, OldState),
    OwnNew = guild_permissions_check:can_view_channel(UserId, CategoryId, NewMember, NewState),
    ShownOld =
        OwnOld orelse
            any_child_visible(CategoryId, UserId, OldMember, OldState, OldChildren),
    ShownNew =
        OwnNew orelse
            any_child_visible(CategoryId, UserId, NewMember, NewState, NewChildren),
    {CategoryId, OwnOld, ShownOld, ShownNew}.

-spec any_child_visible(channel_id(), user_id(), term(), guild_state(), child_index()) ->
    boolean().
any_child_visible(CategoryId, UserId, Member, State, Children) ->
    lists:any(
        fun(ChildId) ->
            guild_permissions_check:can_view_channel(UserId, ChildId, Member, State)
        end,
        maps:get(CategoryId, Children, [])
    ).

-spec child_index([channel_id()], guild_state()) -> child_index().
child_index([], _State) ->
    #{};
child_index(Candidates, State) ->
    CandidateSet = maps:from_keys(Candidates, true),
    Data = map_utils:ensure_map(map_utils:get_safe(State, data, #{})),
    Channels = map_utils:ensure_list(maps:get(<<"channels">>, Data, [])),
    lists:foldr(
        fun(Channel, Acc) -> index_child(Channel, CandidateSet, Acc) end,
        #{},
        Channels
    ).

-spec index_child(term(), #{channel_id() => true}, child_index()) -> child_index().
index_child(#{<<"type">> := 4}, _CandidateSet, Acc) ->
    Acc;
index_child(#{} = Channel, CandidateSet, Acc) ->
    ParentId = snowflake_id:parse_maybe(maps:get(<<"parent_id">>, Channel, undefined)),
    add_child(ParentId, Channel, CandidateSet, Acc);
index_child(_Channel, _CandidateSet, Acc) ->
    Acc.

-spec add_child(channel_id() | undefined, map(), #{channel_id() => true}, child_index()) ->
    child_index().
add_child(ParentId, Channel, CandidateSet, Acc) when
    is_integer(ParentId), is_map_key(ParentId, CandidateSet)
->
    Acc#{ParentId => [maps:get(<<"id">>, Channel) | maps:get(ParentId, Acc, [])]};
add_child(_ParentId, _Channel, _CandidateSet, Acc) ->
    Acc.

-spec candidate_categories([channel_id()], guild_state(), guild_state()) -> [channel_id()].
candidate_categories(ChannelIds, OldState, NewState) ->
    lists:usort(
        lists:append([
            category_ids_for(ChannelId, OldState) ++ category_ids_for(ChannelId, NewState)
         || ChannelId <- ChannelIds
        ])
    ).

-spec category_ids_for(channel_id(), guild_state()) -> [channel_id()].
category_ids_for(ChannelId, State) ->
    case guild_permissions_check:find_channel_by_id(ChannelId, State) of
        undefined ->
            [];
        #{<<"type">> := 4} ->
            [ChannelId];
        Channel ->
            parent_category_ids(Channel)
    end.

-spec parent_category_ids(map()) -> [channel_id()].
parent_category_ids(Channel) ->
    case snowflake_id:parse_maybe(maps:get(<<"parent_id">>, Channel, undefined)) of
        ParentId when is_integer(ParentId) -> [ParentId];
        _ -> []
    end.
