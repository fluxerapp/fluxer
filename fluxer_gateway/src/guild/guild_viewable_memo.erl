%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_viewable_memo).
-typing([eqwalizer]).

-export([new/1, memoised_channel_map/3, channel_map/2, exceptions/1, memo_key/3]).
-export_type([memo/0, user_id/0, guild_state/0]).

-type user_id() :: integer().
-type guild_state() :: map().
-type memo() :: #{exceptions := sets:set(user_id()), cache := #{term() => map()}}.

-spec channel_map(user_id(), guild_state()) -> map().
channel_map(MemberId, State) ->
    guild_sessions:build_viewable_channel_map(
        guild_visibility_channels:get_user_viewable_channels(MemberId, State)
    ).

%% One pass reads a fixed state, and get_user_viewable_channels/2 reads nothing from a
%% member except its raw roles term, so two members holding the same term see the same
%% channels. The user-specific inputs are the guild owner, virtual channel access and
%% user-type permission overwrites, and every user named by one of those bypasses the
%% memo. A term is still computed the first time the pass meets it, so a term whose
%% computation raises raises on the same member it does without the memo.
-spec new(guild_state()) -> memo().
new(State) ->
    #{exceptions => exceptions(State), cache => #{}}.

-spec memoised_channel_map(user_id(), guild_state(), memo()) ->
    {map(), memo()}.
memoised_channel_map(MemberId, State, Memo) ->
    #{exceptions := Exceptions, cache := Cache} = Memo,
    case memo_key(MemberId, Exceptions, State) of
        bypass ->
            {channel_map(MemberId, State), Memo};
        {ok, RawRoles} ->
            cached_channel_map(MemberId, RawRoles, State, Cache, Memo)
    end.

-spec cached_channel_map(
    user_id(), term(), guild_state(), #{term() => map()}, memo()
) -> {map(), memo()}.
cached_channel_map(MemberId, RawRoles, State, Cache, Memo) ->
    case maps:find(RawRoles, Cache) of
        {ok, Viewable} ->
            {Viewable, Memo};
        error ->
            Viewable = channel_map(MemberId, State),
            {Viewable, Memo#{cache := Cache#{RawRoles => Viewable}}}
    end.

-spec memo_key(user_id(), sets:set(user_id()), guild_state()) -> {ok, term()} | bypass.
memo_key(MemberId, Exceptions, State) ->
    case sets:is_element(MemberId, Exceptions) of
        true ->
            bypass;
        false ->
            member_roles_key(guild_permissions_check:find_member_by_user_id(MemberId, State))
    end.

-spec member_roles_key(map() | undefined) -> {ok, term()} | bypass.
member_roles_key(Member) when is_map(Member) ->
    {ok, maps:get(<<"roles">>, Member, [])};
member_roles_key(_Member) ->
    bypass.

-spec exceptions(guild_state()) -> sets:set(user_id()).
exceptions(State) ->
    Data = map_utils:ensure_map(map_utils:get_safe(State, data, #{})),
    Guild = map_utils:ensure_map(maps:get(<<"guild">>, Data, #{})),
    Owner = owner_ids(maps:get(<<"owner_id">>, Guild, undefined)),
    Virtual = maps:keys(map_utils:ensure_map(maps:get(virtual_channel_access, State, #{}))),
    sets:from_list(Owner ++ Virtual ++ overwrite_target_ids(Data)).

%% parse_maybe/1, not parse_optional/1: an id this rejects makes the permission check
%% itself raise before it can match a user, so leaving it out keeps the memo exact.
-spec owner_ids(term()) -> [user_id()].
owner_ids(OwnerIdRaw) ->
    case snowflake_id:parse_maybe(OwnerIdRaw) of
        OwnerId when is_integer(OwnerId) -> [OwnerId];
        _ -> []
    end.

%% Both places a permission check can take overwrites from, and role targets alongside
%% user targets. Role ids can never collide with user ids, so the superset only costs a
%% few members their memo entry.
-spec overwrite_target_ids(map()) -> [user_id()].
overwrite_target_ids(Data) ->
    Cached = maps:fold(
        fun(_ChannelId, Entries, Acc) -> cached_target_ids(Entries, Acc) end,
        [],
        map_utils:ensure_map(maps:get(overwrite_perms_cache, Data, #{}))
    ),
    maps:fold(
        fun(_ChannelId, Channel, Acc) -> channel_target_ids(Channel, Acc) end,
        Cached,
        guild_data_index_channels:channel_index(Data)
    ).

-spec cached_target_ids(term(), [user_id()]) -> [user_id()].
cached_target_ids(Entries, Acc) when is_list(Entries) ->
    [Id || {Id, _Type, _Allow, _Deny} <- Entries, is_integer(Id)] ++ Acc;
cached_target_ids(_Entries, Acc) ->
    Acc.

-spec channel_target_ids(term(), [user_id()]) -> [user_id()].
channel_target_ids(Channel, Acc) when is_map(Channel) ->
    lists:foldl(
        fun overwrite_target_id/2,
        Acc,
        map_utils:ensure_list(maps:get(<<"permission_overwrites">>, Channel, []))
    );
channel_target_ids(_Channel, Acc) ->
    Acc.

-spec overwrite_target_id(term(), [user_id()]) -> [user_id()].
overwrite_target_id(Overwrite, Acc) when is_map(Overwrite) ->
    case snowflake_id:parse_maybe(maps:get(<<"id">>, Overwrite, undefined)) of
        Id when is_integer(Id) -> [Id | Acc];
        _ -> Acc
    end;
overwrite_target_id(_Overwrite, Acc) ->
    Acc.
