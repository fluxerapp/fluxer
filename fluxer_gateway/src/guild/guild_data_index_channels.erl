%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_data_index_channels).
-typing([eqwalizer]).

-export([
    channel_list/1,
    channel_index/1,
    put_channels/2,
    put_forum_categories/2,
    build_overwrite_perms_cache/1
]).

-type guild_data() :: map().
-type channel() :: map().
-type snowflake_id() :: integer().

-export_type([guild_data/0, channel/0, snowflake_id/0]).

-spec channel_list(term()) -> [channel()].
channel_list(Data) when is_map(Data) ->
    case maps:get(channels_stale, Data, false) of
        true ->
            Index = channel_index(Data),
            maps:values(Index);
        false ->
            lists:filtermap(
                fun normalize_channel_item/1,
                guild_data_index:ensure_list(maps:get(<<"channels">>, Data, []))
            )
    end;
channel_list(_) ->
    [].

-spec channel_index(term()) -> #{snowflake_id() => channel()}.
channel_index(Data) when is_map(Data) ->
    case maps:get(<<"channel_index">>, Data, undefined) of
        Index when is_map(Index) -> existing_channel_index(Index);
        _ -> guild_data_index:build_id_index(channel_list(Data))
    end;
channel_index(_) ->
    #{}.

-spec put_channels(term(), term()) -> term().
put_channels(Channels, Data) when is_map(Data) ->
    ChannelList = lists:filtermap(
        fun normalize_channel_item/1,
        guild_data_index:ensure_list(Channels)
    ),
    put_forum_categories(ChannelList, Data#{
        <<"channels">> => ChannelList,
        <<"channel_index">> => guild_data_index:build_id_index(ChannelList),
        overwrite_perms_cache => build_overwrite_perms_cache(ChannelList)
    });
put_channels(_, Data) ->
    Data.

-spec put_forum_categories([channel()], map()) -> map().
put_forum_categories(Channels, Data) ->
    case forum_categories(Channels) of
        Categories when map_size(Categories) =:= 0 ->
            maps:remove(thread_forum_categories, Data);
        Categories ->
            Data#{thread_forum_categories => Categories}
    end.

-spec forum_categories([channel()]) -> #{snowflake_id() => [snowflake_id()]}.
forum_categories(Channels) ->
    case [parent_of(C) || #{<<"type">> := Type} = C <- Channels, is_forum_type(Type)] of
        [] ->
            #{};
        Parents ->
            Categories = maps:from_list([{P, []} || P <- Parents, is_integer(P)]),
            lists:foldl(fun add_plain_child/2, Categories, Channels)
    end.

-spec add_plain_child(channel(), #{snowflake_id() => [snowflake_id()]}) ->
    #{snowflake_id() => [snowflake_id()]}.
add_plain_child(Channel, Categories) ->
    case
        {
            parent_of(Channel),
            is_forum_type(maps:get(<<"type">>, Channel, undefined)),
            snowflake_id:parse_maybe(maps:get(<<"id">>, Channel, undefined))
        }
    of
        {Parent, false, ChildId} when is_map_key(Parent, Categories), is_integer(ChildId) ->
            Categories#{Parent := [ChildId | maps:get(Parent, Categories)]};
        _ ->
            Categories
    end.

-spec parent_of(channel()) -> snowflake_id() | undefined.
parent_of(Channel) ->
    snowflake_id:parse_maybe(maps:get(<<"parent_id">>, Channel, undefined)).

-spec is_forum_type(term()) -> boolean().
is_forum_type(15) -> true;
is_forum_type(16) -> true;
is_forum_type(_) -> false.

-spec build_overwrite_perms_cache([channel()]) ->
    #{integer() => [{integer(), integer(), integer()}]}.
build_overwrite_perms_cache(Channels) ->
    lists:foldl(
        fun
            (Channel, Acc) when is_map(Channel) ->
                cache_channel_overwrites(Channel, Acc);
            (_, Acc) ->
                Acc
        end,
        #{},
        Channels
    ).

-spec normalize_channel(channel()) -> channel().
normalize_channel(Channel) ->
    case guild_data_normalize:channel(Channel) of
        Normalized when is_map(Normalized) -> Normalized;
        _ -> Channel
    end.

-spec normalize_channel_item(term()) -> {true, channel()} | false.
normalize_channel_item(Channel) when is_map(Channel) ->
    try normalize_channel(Channel) of
        Normalized -> {true, Normalized}
    catch
        error:{invalid_snowflake, _} -> normalize_channel_with_filtered_overwrites(Channel);
        error:{invalid_bitset, _} -> normalize_channel_with_filtered_overwrites(Channel)
    end;
normalize_channel_item(_) ->
    false.

-spec normalize_channel_with_filtered_overwrites(channel()) -> {true, channel()} | false.
normalize_channel_with_filtered_overwrites(Channel) ->
    Filtered = Channel#{
        <<"permission_overwrites">> => lists:filtermap(
            fun valid_overwrite/1,
            channel_overwrites(Channel)
        )
    },
    try normalize_channel(Filtered) of
        Normalized -> {true, Normalized}
    catch
        error:{invalid_snowflake, _} -> false;
        error:{invalid_bitset, _} -> false
    end.

-spec valid_overwrite(term()) -> {true, map()} | false.
valid_overwrite(Overwrite) when is_map(Overwrite) ->
    case
        {
            snowflake_id:parse_maybe(maps:get(<<"id">>, Overwrite, undefined)),
            map_utils:get_integer(Overwrite, <<"type">>, undefined),
            permission_bits:parse_maybe(maps:get(<<"allow">>, Overwrite, undefined)),
            permission_bits:parse_maybe(maps:get(<<"deny">>, Overwrite, undefined))
        }
    of
        {Id, Type, Allow, Deny} when
            is_integer(Id), is_integer(Type), is_integer(Allow), is_integer(Deny)
        ->
            {true, Overwrite};
        _ ->
            false
    end;
valid_overwrite(_) ->
    false.

-spec existing_channel_index(map()) -> #{snowflake_id() => channel()}.
existing_channel_index(Index) ->
    Index.

-spec cache_channel_overwrites(channel(), map()) -> map().
cache_channel_overwrites(Channel, Acc) ->
    case snowflake_id:parse_maybe(maps:get(<<"id">>, Channel, undefined)) of
        ChannelId when is_integer(ChannelId) ->
            Overwrites = channel_overwrites(Channel),
            Cached = lists:filtermap(fun overwrite_tuple/1, Overwrites),
            Acc#{ChannelId => Cached};
        undefined ->
            Acc
    end.

-spec channel_overwrites(channel()) -> [term()].
channel_overwrites(Channel) ->
    case maps:get(<<"permission_overwrites">>, Channel, []) of
        Overwrites when is_list(Overwrites) -> Overwrites;
        _ -> []
    end.

-spec overwrite_tuple(term()) -> false | {true, {integer(), term(), integer(), integer()}}.
overwrite_tuple(OW) when is_map(OW) ->
    OWId = snowflake_id:parse_maybe(maps:get(<<"id">>, OW, undefined)),
    Allow = permission_bits:parse_optional(maps:get(<<"allow">>, OW, undefined)),
    Deny = permission_bits:parse_optional(maps:get(<<"deny">>, OW, undefined)),
    Type = map_utils:get_integer(OW, <<"type">>, undefined),
    case {OWId, Allow, Deny, Type} of
        {Id, AllowBits, DenyBits, OverwriteType} when
            is_integer(Id),
            is_integer(AllowBits),
            is_integer(DenyBits),
            is_integer(OverwriteType)
        ->
            {true, {Id, OverwriteType, AllowBits, DenyBits}};
        _ ->
            false
    end;
overwrite_tuple(_) ->
    false.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

forum_categories_list_plain_children_of_forum_parents_test() ->
    Channels = [
        #{<<"id">> => 1, <<"type">> => 4},
        #{<<"id">> => 2, <<"type">> => 4},
        #{<<"id">> => 10, <<"type">> => 15, <<"parent_id">> => 1},
        #{<<"id">> => 11, <<"type">> => 0, <<"parent_id">> => 1},
        #{<<"id">> => 12, <<"type">> => 0, <<"parent_id">> => 2}
    ],
    ?assertEqual(#{1 => [11]}, forum_categories(Channels)),
    ?assertEqual(#{}, forum_categories(tl(tl(tl(Channels))))),
    Data = put_channels(Channels, #{}),
    ?assertEqual(#{1 => [11]}, maps:get(thread_forum_categories, Data)),
    ?assertNot(maps:is_key(thread_forum_categories, put_channels([], Data))).

-endif.
