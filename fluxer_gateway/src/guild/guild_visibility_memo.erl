%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_visibility_memo).
-typing([eqwalizer]).

-export([new/3, pairs/5]).

-export_type([memo/0, pairs/0]).

-type guild_state() :: map().
-type user_id() :: integer().
-type channel_id() :: integer().
-type visibility() :: #{channel_id() => {boolean(), boolean()}}.
-type settled() :: {#{channel_id() => true}, [channel_id()]} | unsettled.
-type pairs() :: {visibility(), settled()}.
-type memo() ::
    disabled
    | #{
        exceptions := sets:set(user_id()),
        unchanged := #{channel_id() => true},
        pairs := #{term() => pairs()}
    }.

-spec new([channel_id()], guild_state(), guild_state()) -> memo().
new(ChannelIds, OldState, NewState) ->
    #{
        exceptions => sets:union(
            guild_maintenance:viewable_exceptions(OldState),
            guild_maintenance:viewable_exceptions(NewState)
        ),
        unchanged => maps:from_keys(
            [
                Id
             || Id <- ChannelIds,
                guild_permissions:view_inputs(Id, OldState) =:=
                    guild_permissions:view_inputs(Id, NewState)
            ],
            true
        ),
        pairs => #{}
    }.

-spec pairs([channel_id()], guild_state(), guild_state(), map(), memo()) ->
    {pairs() | none, memo()}.
pairs(_ChannelIds, _OldState, _NewState, _ChangeContext, disabled) ->
    {none, disabled};
pairs(ChannelIds, OldState, NewState, ChangeContext, Memo) ->
    #{user_id := UserId, old_member := OldMember, new_member := NewMember} = ChangeContext,
    #{exceptions := Exceptions, unchanged := Unchanged, pairs := Cache} = Memo,
    case {sets:is_element(UserId, Exceptions), OldMember, NewMember} of
        {false, #{}, #{}} ->
            Key = {maps:get(<<"roles">>, OldMember, []), maps:get(<<"roles">>, NewMember, [])},
            case maps:find(Key, Cache) of
                {ok, Found} ->
                    {Found, Memo};
                error ->
                    Reusable =
                        case Key of
                            {Same, Same} -> Unchanged;
                            _ -> #{}
                        end,
                    Computed = compute(ChannelIds, OldState, NewState, Reusable, ChangeContext),
                    {Computed, Memo#{pairs := Cache#{Key => Computed}}}
            end;
        _ ->
            {none, Memo}
    end.

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
    OldVisible = guild_visibility_channels:channel_is_visible(
        UserId, ChannelId, OldMember, OldState
    ),
    case maps:is_key(ChannelId, Reusable) of
        true ->
            {OldVisible, OldVisible};
        false ->
            {OldVisible,
                guild_visibility_channels:channel_is_visible(
                    UserId, ChannelId, NewMember, NewState
                )}
    end.

-spec settle(visibility()) -> settled().
settle(Visibility) ->
    case lists:all(fun({Old, New}) -> Old =:= New end, maps:values(Visibility)) of
        true ->
            {
                maps:from_keys([Id || {Id, {_, true}} <- maps:to_list(Visibility)], true),
                [Id || {Id, {_, false}} <- maps:to_list(Visibility)]
            };
        false ->
            unsettled
    end.
