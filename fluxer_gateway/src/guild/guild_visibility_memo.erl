%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_visibility_memo).
-typing([eqwalizer]).

-export([new/3, pairs/5]).

-export_type([memo/0, pairs/0, guild_state/0, channel_id/0]).

-type guild_state() :: map().
-type user_id() :: integer().
-type channel_id() :: integer().
-type visibility() :: #{channel_id() => {boolean(), boolean()}}.
-type settled() :: {#{channel_id() => true}, [channel_id()]} | unsettled.
-type pairs() :: {visibility(), settled()}.
-type active_memo() :: #{
    exceptions := sets:set(user_id()),
    unchanged := #{channel_id() => true},
    pairs := #{term() => pairs()}
}.
-type memo() :: disabled | active_memo().

-spec new([channel_id()], guild_state(), guild_state()) -> memo().
new(ChannelIds, OldState, NewState) ->
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
        pairs => #{}
    }.

-spec pairs([channel_id()], guild_state(), guild_state(), map(), memo()) ->
    {pairs() | none, memo()}.
pairs(_ChannelIds, _OldState, _NewState, _ChangeContext, disabled) ->
    {none, disabled};
pairs(ChannelIds, OldState, NewState, ChangeContext, #{} = Memo) ->
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
    {term(), term()}, [channel_id()], {guild_state(), guild_state()}, map(), active_memo()
) ->
    {pairs(), active_memo()}.
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
