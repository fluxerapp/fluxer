%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_data_channels).
-typing([eqwalizer]).

-export([
    channels_from_state/1,
    channels_from_data/1,
    find_everyone_viewable_text_channel/2,
    sort_channels_for_ordering/1,
    derive_member_view/4,
    member_view/4,
    sanitize_voice_state/1,
    merge_members/2
]).

-type guild_state() :: map().
-type guild_data_map() :: map().
-type channel_list() :: [map()].
-type guild_member() :: map().
-type user_id() :: integer().
-type guild_id() :: integer().

-type member_view_cache() :: #{
    inputs := tuple(),
    exceptions := sets:set(user_id()),
    views := #{term() => #{integer() => true}}
}.

-define(MEMBER_VIEW_CACHE_LIMIT, 512).

-export_type([guild_state/0, guild_data_map/0, channel_list/0, guild_member/0, user_id/0]).

-spec channels_from_state(guild_state()) -> channel_list().
channels_from_state(State) ->
    Data = guild_data_index:ensure_data_map(State),
    channels_from_data(Data).

-spec channels_from_data(guild_data_map()) -> channel_list().
channels_from_data(Data) ->
    guild_data_index:channel_list(Data).

-spec find_everyone_viewable_text_channel(channel_list(), guild_state()) -> integer() | null.
find_everyone_viewable_text_channel(Channels, State) ->
    Data = guild_data_index:ensure_data_map(State),
    case guild_id(State, Data) of
        undefined -> null;
        GuildId -> find_everyone_viewable_text_channel_for_guild(Channels, Data, GuildId)
    end.

-spec find_everyone_viewable_text_channel_for_guild(
    channel_list(), guild_data_map(), guild_id()
) ->
    integer() | null.
find_everyone_viewable_text_channel_for_guild(Channels, Data, GuildId) ->
    Roles = map_utils:ensure_list(maps:get(<<"roles">>, Data, [])),
    EveryonePerms = role_permissions_for_id(Roles, GuildId),
    OrderedChannels = sort_channels_for_ordering(map_utils:ensure_list(Channels)),
    lists:foldl(
        fun(Channel, Acc) -> first_viewable_fold(Channel, GuildId, EveryonePerms, Acc) end,
        null,
        OrderedChannels
    ).

-spec first_viewable_fold(map(), guild_id(), integer(), integer() | null) -> integer() | null.
first_viewable_fold(Channel, GuildId, EveryonePerms, null) ->
    select_first_viewable(Channel, GuildId, EveryonePerms);
first_viewable_fold(_Channel, _GuildId, _EveryonePerms, Acc) ->
    Acc.

-spec derive_member_view(user_id(), guild_member() | undefined, guild_state(), channel_list()) ->
    {channel_list(), term()}.
derive_member_view(_UserId, undefined, _State, _Channels) ->
    {[], null};
derive_member_view(UserId, Member, State, Channels) ->
    Filtered = filter_viewable_channels(UserId, Member, State, Channels),
    JoinedAt = maps:get(<<"joined_at">>, Member, null),
    {with_missing_parents(Filtered, Channels), JoinedAt}.

-spec member_view(user_id(), guild_member(), guild_state(), channel_list()) ->
    {channel_list(), guild_state()}.
member_view(UserId, Member, State, Channels) ->
    #{exceptions := Exceptions, views := Views} = Cache = member_view_cache(State),
    RolesKey = maps:get(<<"roles">>, Member, []),
    case {sets:is_element(UserId, Exceptions), maps:find(RolesKey, Views)} of
        {true, _} ->
            {Viewable, _JoinedAt} = derive_member_view(UserId, Member, State, Channels),
            {Viewable, State#{member_view_cache => Cache}};
        {false, {ok, ViewableIds}} ->
            Filtered = [C || C <- Channels, maps:is_key(channel_id(C), ViewableIds)],
            {with_missing_parents(Filtered, Channels), State#{member_view_cache => Cache}};
        {false, error} ->
            Filtered = filter_viewable_channels(UserId, Member, State, Channels),
            ViewableIds = maps:from_keys(channel_ids(Filtered), true),
            NewCache = Cache#{views := put_member_view(RolesKey, ViewableIds, Views)},
            {with_missing_parents(Filtered, Channels), State#{member_view_cache => NewCache}}
    end.

-spec member_view_cache(guild_state()) -> member_view_cache().
member_view_cache(State) ->
    Inputs = member_view_inputs(State),
    case maps:get(member_view_cache, State, undefined) of
        #{inputs := CachedInputs} = Cache when CachedInputs =:= Inputs ->
            Cache;
        _ ->
            #{
                inputs => Inputs,
                exceptions => guild_maintenance:viewable_exceptions(State),
                views => #{}
            }
    end.

-spec member_view_inputs(guild_state()) -> tuple().
member_view_inputs(State) ->
    Data = guild_data_index:ensure_data_map(State),
    {
        maps:get(id, State, undefined),
        maps:get(<<"id">>, State, undefined),
        maps:get(virtual_channel_access, State, undefined),
        maps:get(<<"guild">>, Data, undefined),
        maps:get(<<"roles">>, Data, undefined),
        maps:get(<<"role_index">>, Data, undefined),
        maps:get(role_perms_cache, Data, undefined),
        maps:get(overwrite_perms_cache, Data, undefined),
        maps:get(<<"channels">>, Data, undefined),
        maps:map(fun channel_view_inputs/2, guild_data_index:channel_index(Data))
    }.

-spec channel_view_inputs(integer(), term()) -> term().
channel_view_inputs(_ChannelId, Channel) when is_map(Channel) ->
    {
        maps:get(<<"id">>, Channel, undefined),
        maps:get(<<"type">>, Channel, undefined),
        maps:get(<<"parent_id">>, Channel, undefined),
        maps:get(<<"permission_overwrites">>, Channel, undefined)
    };
channel_view_inputs(_ChannelId, Channel) ->
    Channel.

-spec put_member_view(term(), #{integer() => true}, #{term() => #{integer() => true}}) ->
    #{term() => #{integer() => true}}.
put_member_view(RolesKey, ViewableIds, Views) when
    map_size(Views) >= ?MEMBER_VIEW_CACHE_LIMIT
->
    #{RolesKey => ViewableIds};
put_member_view(RolesKey, ViewableIds, Views) ->
    Views#{RolesKey => ViewableIds}.

-spec with_missing_parents(channel_list(), channel_list()) -> channel_list().
with_missing_parents(Filtered, Channels) ->
    FilteredIds = sets:from_list(channel_ids(Filtered)),
    MissingParentIds = find_missing_parent_ids(Filtered, FilteredIds),
    Filtered ++ collect_extra_categories(MissingParentIds, Channels).

-spec sanitize_voice_state(map()) -> map().
sanitize_voice_state(VS) ->
    voice_state_utils:sanitize_voice_state_for_broadcast(VS).

-spec merge_members([guild_member()], [guild_member()]) -> [guild_member()].
merge_members(Primary, Secondary) ->
    {Merged, _} = lists:foldl(
        fun merge_member/2,
        {[], sets:new()},
        Primary ++ Secondary
    ),
    lists:reverse(Merged).

-spec merge_member(guild_member(), {[guild_member()], sets:set(integer())}) ->
    {[guild_member()], sets:set(integer())}.
merge_member(Member, {Acc, Seen}) ->
    case member_user_id(Member) of
        undefined -> {Acc, Seen};
        UserId -> maybe_add_member(UserId, Member, Acc, Seen)
    end.

-spec sort_channels_for_ordering(channel_list()) -> channel_list().
sort_channels_for_ordering(Channels) ->
    guild_data_channels_order:sort_channels_for_ordering(Channels).

-spec filter_viewable_channels(user_id(), guild_member(), guild_state(), channel_list()) ->
    channel_list().
filter_viewable_channels(UserId, Member, State, Channels) ->
    lists:filter(
        fun(Channel) ->
            ChannelId = channel_id(Channel),
            ChannelId =/= undefined andalso
                guild_permissions:can_view_channel(UserId, ChannelId, Member, State)
        end,
        Channels
    ).

-spec find_missing_parent_ids(channel_list(), sets:set(integer())) -> sets:set(integer()).
find_missing_parent_ids(Filtered, FilteredIds) ->
    ParentIds = sets:from_list([
        ParentId
     || C <- Filtered,
        ParentId <- [channel_parent_id(C)],
        ParentId =/= undefined
    ]),
    sets:subtract(ParentIds, FilteredIds).

-spec collect_extra_categories(sets:set(integer()), channel_list()) -> channel_list().
collect_extra_categories(MissingParentIds, Channels) ->
    case sets:is_empty(MissingParentIds) of
        true -> [];
        false -> filter_channels_in_set(Channels, MissingParentIds)
    end.

-spec filter_channels_in_set(channel_list(), sets:set(integer())) -> channel_list().
filter_channels_in_set(Channels, ChannelIds) ->
    lists:filter(fun(Channel) -> channel_in_set(Channel, ChannelIds) end, Channels).

-spec channel_in_set(map(), sets:set(integer())) -> boolean().
channel_in_set(Channel, ChannelIds) ->
    case channel_id(Channel) of
        ChannelId when is_integer(ChannelId) -> sets:is_element(ChannelId, ChannelIds);
        undefined -> false
    end.

-spec select_first_viewable(map(), integer(), integer()) -> integer() | null.
select_first_viewable(Channel, GuildId, BasePerms) ->
    ChannelType = guild_data_normalize_schema:int(maps:get(<<"type">>, Channel, undefined)),
    ChannelId = channel_id(Channel),
    check_viewable(ChannelType, ChannelId, Channel, GuildId, BasePerms).

-spec check_viewable(integer() | undefined, integer() | undefined, map(), integer(), integer()) ->
    integer() | null.
check_viewable(ChannelType, ChannelId, Channel, GuildId, BasePerms) when
    is_integer(ChannelId), ChannelType =:= 0 orelse ChannelType =:= 2 orelse ChannelType =:= 5
->
    case permission_bits:has(BasePerms, constants:administrator_permission()) of
        true -> ChannelId;
        false -> check_view_permission(ChannelId, Channel, GuildId, BasePerms)
    end;
check_viewable(_, _, _, _, _) ->
    null.

-spec check_view_permission(integer(), map(), integer(), integer()) -> integer() | null.
check_view_permission(ChannelId, Channel, GuildId, BasePerms) ->
    FinalPerms = guild_permissions:apply_channel_overwrites(
        BasePerms, undefined, [], Channel, GuildId
    ),
    case permission_bits:has(FinalPerms, constants:view_channel_permission()) of
        true -> ChannelId;
        false -> null
    end.

-spec role_permissions_for_id([map()], integer()) -> integer().
role_permissions_for_id(Roles, GuildId) ->
    lists:foldl(
        fun(Role, Acc) -> role_permissions_for_id_fold(Role, GuildId, Acc) end,
        0,
        map_utils:ensure_list(Roles)
    ).

-spec role_permissions_for_id_fold(map(), guild_id(), integer()) -> integer().
role_permissions_for_id_fold(Role, GuildId, Acc) ->
    case safe_snowflake_id(maps:get(<<"id">>, Role, undefined)) of
        GuildId -> permission_bits:parse(maps:get(<<"permissions">>, Role, undefined));
        _ -> Acc
    end.

-spec member_user_id(guild_member()) -> integer() | undefined.
member_user_id(Member) ->
    MemberUser = map_utils:ensure_map(maps:get(<<"user">>, Member, #{})),
    safe_snowflake_id(maps:get(<<"id">>, MemberUser, undefined)).

-spec maybe_add_member(integer(), guild_member(), [guild_member()], sets:set(integer())) ->
    {[guild_member()], sets:set(integer())}.
maybe_add_member(UserId, Member, Acc, Seen) ->
    case sets:is_element(UserId, Seen) of
        true -> {Acc, Seen};
        false -> {[Member | Acc], sets:add_element(UserId, Seen)}
    end.

-spec guild_id(guild_state(), guild_data_map()) -> guild_id() | undefined.
guild_id(State, Data) ->
    Guild = map_utils:ensure_map(maps:get(<<"guild">>, Data, #{})),
    snowflake_id:first([
        maps:get(id, State, undefined),
        maps:get(<<"id">>, State, undefined),
        maps:get(<<"id">>, Guild, undefined)
    ]).

-spec channel_ids(channel_list()) -> [integer()].
channel_ids(Channels) ->
    lists:filtermap(fun channel_id_item/1, Channels).

-spec channel_id_item(map()) -> {true, integer()} | false.
channel_id_item(Channel) ->
    case channel_id(Channel) of
        ChannelId when is_integer(ChannelId) -> {true, ChannelId};
        undefined -> false
    end.

-spec channel_id(map()) -> integer() | undefined.
channel_id(Channel) ->
    safe_snowflake_id(maps:get(<<"id">>, Channel, undefined)).

-spec channel_parent_id(map()) -> integer() | undefined.
channel_parent_id(Channel) ->
    safe_snowflake_id(maps:get(<<"parent_id">>, Channel, undefined)).

-spec safe_snowflake_id(term()) -> integer() | undefined.
safe_snowflake_id(Value) ->
    try snowflake_id:parse_optional(Value) of
        Id -> Id
    catch
        error:{invalid_snowflake, _} -> undefined
    end.
