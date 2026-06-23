%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_data_index_channels).
-typing([eqwalizer]).

-export([
    channel_list/1,
    channel_index/1,
    put_channels/2,
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
    Data#{
        <<"channels">> => ChannelList,
        <<"channel_index">> => guild_data_index:build_id_index(ChannelList),
        overwrite_perms_cache => build_overwrite_perms_cache(ChannelList)
    };
put_channels(_, Data) ->
    Data.

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

channel_list_non_map_input_test() ->
    ?assertEqual([], channel_list(not_a_map)).

channel_list_non_list_channels_value_test() ->
    ?assertEqual([], channel_list(#{<<"channels">> => <<"invalid">>})).

channel_index_non_map_input_test() ->
    ?assertEqual(#{}, channel_index(not_a_map)).

channel_index_from_list_test() ->
    Data = #{
        <<"channels">> => [
            #{<<"id">> => <<"300">>, <<"name">> => <<"general">>},
            #{<<"id">> => <<"301">>, <<"name">> => <<"random">>}
        ]
    },
    Index = channel_index(Data),
    ?assertEqual(2, map_size(Index)),
    ?assertEqual(<<"general">>, maps:get(<<"name">>, maps:get(300, Index))).

put_channels_updates_list_and_index_test() ->
    Data = #{<<"channels">> => []},
    NewChannels = [
        #{<<"id">> => <<"50">>, <<"name">> => <<"ch1">>},
        #{<<"id">> => <<"51">>, <<"name">> => <<"ch2">>}
    ],
    Updated = put_channels(NewChannels, Data),
    ?assertEqual(
        [
            #{<<"id">> => 50, <<"name">> => <<"ch1">>},
            #{<<"id">> => 51, <<"name">> => <<"ch2">>}
        ],
        channel_list(Updated)
    ),
    ?assertEqual(2, map_size(channel_index(Updated))).

put_channels_non_map_data_returns_unchanged_test() ->
    ?assertEqual(not_a_map, put_channels([], not_a_map)).

-endif.
