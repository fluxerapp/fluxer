%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_data_channels_order).
-typing([eqwalizer]).

-export([sort_channels_for_ordering/1]).

-type channel_list() :: [map()].

-export_type([channel_list/0]).

-spec sort_channels_for_ordering(channel_list()) -> channel_list().
sort_channels_for_ordering(Channels) ->
    ChannelList = map_utils:ensure_list(Channels),
    ChannelIds = sets:from_list(channel_ids(ChannelList)),
    {RootChannels, Categories, ChildrenByParent} = split_channels(ChannelList, ChannelIds),
    RootBlock = sort_channel_list_group(RootChannels),
    InitialSeen = mark_channels_seen(RootBlock, sets:new()),
    {OrderedChannels, SeenIds} = fold_categories(
        Categories, ChildrenByParent, RootBlock, InitialSeen
    ),
    Remaining = collect_remaining(ChannelList, SeenIds),
    OrderedChannels ++ Remaining.

-spec fold_categories(
    [map()], #{integer() => channel_list()}, channel_list(), sets:set(integer())
) ->
    {channel_list(), sets:set(integer())}.
fold_categories(Categories, ChildrenByParent, RootBlock, InitialSeen) ->
    lists:foldl(
        fun(Channel, {Acc, Seen}) ->
            Children = children_for_category(Channel, ChildrenByParent),
            Block = [Channel | Children],
            {Acc ++ Block, mark_channels_seen(Block, Seen)}
        end,
        {RootBlock, InitialSeen},
        sort_channels_by_position(Categories)
    ).

-spec children_for_category(map(), #{integer() => channel_list()}) -> channel_list().
children_for_category(Channel, ChildrenByParent) ->
    case channel_id(Channel) of
        Id when is_integer(Id) -> sort_channel_list_group(maps:get(Id, ChildrenByParent, []));
        _ -> []
    end.

-spec collect_remaining(channel_list(), sets:set(integer())) -> channel_list().
collect_remaining(ChannelList, SeenIds) ->
    [C || C <- sort_channels_by_position(ChannelList), not channel_seen(C, SeenIds)].

-spec channel_ids(channel_list()) -> [integer()].
channel_ids(Channels) ->
    lists:filtermap(fun channel_id_item/1, Channels).

-spec channel_id_item(map()) -> {true, integer()} | false.
channel_id_item(Channel) ->
    case channel_id(Channel) of
        Id when is_integer(Id) -> {true, Id};
        _ -> false
    end.

-spec split_channels(channel_list(), sets:set(integer())) ->
    {channel_list(), channel_list(), #{integer() => channel_list()}}.
split_channels(Channels, ChannelIds) ->
    lists:foldl(
        fun(Channel, {Roots, Cats, ByParent}) ->
            classify_channel(Channel, ChannelIds, Roots, Cats, ByParent)
        end,
        {[], [], #{}},
        Channels
    ).

-spec classify_channel(
    map(),
    sets:set(integer()),
    channel_list(),
    channel_list(),
    #{integer() => channel_list()}
) ->
    {channel_list(), channel_list(), #{integer() => channel_list()}}.
classify_channel(Channel, ChannelIds, Roots, Cats, ByParent) ->
    case channel_is_category(Channel) of
        true -> {Roots, [Channel | Cats], ByParent};
        false -> classify_non_category(Channel, ChannelIds, Roots, Cats, ByParent)
    end.

-spec classify_non_category(
    map(),
    sets:set(integer()),
    channel_list(),
    channel_list(),
    #{integer() => channel_list()}
) ->
    {channel_list(), channel_list(), #{integer() => channel_list()}}.
classify_non_category(Channel, ChannelIds, Roots, Cats, ByParent) ->
    case channel_parent_id(Channel) of
        ParentId when is_integer(ParentId) ->
            classify_child(Channel, ParentId, ChannelIds, Roots, Cats, ByParent);
        _ ->
            {[Channel | Roots], Cats, ByParent}
    end.

-spec classify_child(
    map(),
    integer(),
    sets:set(integer()),
    channel_list(),
    channel_list(),
    #{integer() => channel_list()}
) ->
    {channel_list(), channel_list(), #{integer() => channel_list()}}.
classify_child(Channel, ParentId, ChannelIds, Roots, Cats, ByParent) ->
    case sets:is_element(ParentId, ChannelIds) of
        true ->
            Children = maps:get(ParentId, ByParent, []),
            {Roots, Cats, ByParent#{ParentId => [Channel | Children]}};
        false ->
            {Roots, Cats, ByParent}
    end.

-spec sort_channel_list_group(channel_list()) -> channel_list().
sort_channel_list_group(Channels) ->
    Sorted = sort_channels_by_position(Channels),
    [C || C <- Sorted, channel_is_layout_text(C)] ++
        [C || C <- Sorted, channel_is_layout_voice(C)] ++
        [C || C <- Sorted, not channel_is_layout_text(C), not channel_is_layout_voice(C)].

-spec sort_channels_by_position(channel_list()) -> channel_list().
sort_channels_by_position(Channels) ->
    lists:sort(fun channel_precedes/2, map_utils:ensure_list(Channels)).

-spec channel_precedes(map(), map()) -> boolean().
channel_precedes(A, B) ->
    APosition = channel_position(A),
    BPosition = channel_position(B),
    case APosition =:= BPosition of
        false -> APosition < BPosition;
        true -> channel_sort_key(A) =< channel_sort_key(B)
    end.

-spec channel_sort_key(map()) -> {0, integer()} | {1, term()}.
channel_sort_key(Channel) ->
    case channel_id(Channel) of
        Id when is_integer(Id) -> {0, Id};
        undefined -> {1, maps:get(<<"id">>, Channel, undefined)}
    end.

-spec channel_position(map()) -> integer().
channel_position(Channel) ->
    case guild_data_normalize_schema:int(maps:get(<<"position">>, Channel, undefined)) of
        undefined -> 0;
        Position -> Position
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

-spec channel_is_category(map()) -> boolean().
channel_is_category(Channel) ->
    channel_type(Channel) =:= 4.

-spec channel_is_layout_text(map()) -> boolean().
channel_is_layout_text(Channel) ->
    ChannelType = channel_type(Channel),
    ChannelType =:= 0 orelse ChannelType =:= 5 orelse ChannelType =:= 998 orelse
        ChannelType =:= 15 orelse ChannelType =:= 16.

-spec channel_is_layout_voice(map()) -> boolean().
channel_is_layout_voice(Channel) ->
    channel_type(Channel) =:= 2.

-spec channel_type(map()) -> integer() | undefined.
channel_type(Channel) ->
    guild_data_normalize_schema:int(maps:get(<<"type">>, Channel, undefined)).

-spec mark_channels_seen(channel_list(), sets:set(integer())) -> sets:set(integer()).
mark_channels_seen(Channels, Seen) ->
    lists:foldl(fun mark_channel_seen/2, Seen, Channels).

-spec mark_channel_seen(map(), sets:set(integer())) -> sets:set(integer()).
mark_channel_seen(Channel, Seen) ->
    case channel_id(Channel) of
        Id when is_integer(Id) -> sets:add_element(Id, Seen);
        _ -> Seen
    end.

-spec channel_seen(map(), sets:set(integer())) -> boolean().
channel_seen(Channel, Seen) ->
    case channel_id(Channel) of
        Id when is_integer(Id) -> sets:is_element(Id, Seen);
        _ -> false
    end.
