%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_state_channels).
-typing([eqwalizer]).

-export([
    handle_channel_create/2,
    handle_channel_update/2,
    handle_channel_update_bulk/2,
    handle_channel_delete/2,
    handle_message_create/2,
    handle_channel_pins_update/2,
    handle_emojis_update/2,
    handle_stickers_update/2,
    handle_guild_update/2,
    extract_channel_ids_from_channel_update/1,
    extract_channel_ids_from_channel_update_bulk/1
]).

-type guild_data() :: map().
-type event_data() :: map().

-export_type([guild_data/0, event_data/0]).

-spec handle_guild_update(event_data(), guild_data()) -> guild_data().
handle_guild_update(EventData, Data) ->
    Guild = maps:get(<<"guild">>, Data),
    UpdatedGuild = maps:merge(Guild, EventData),
    Data#{<<"guild">> => UpdatedGuild}.

-spec handle_channel_create(event_data(), guild_data()) -> guild_data().
handle_channel_create(EventData, Data) ->
    Channels = guild_data_index:channel_list(Data),
    guild_data_index:put_channels([EventData | Channels], Data).

-spec handle_channel_update(event_data(), guild_data()) -> guild_data().
handle_channel_update(EventData, Data) ->
    Channels = guild_data_index:channel_list(Data),
    ChannelId = maps:get(<<"id">>, EventData),
    UpdatedChannels = guild_state_utils:replace_item_by_id(Channels, ChannelId, EventData),
    guild_data_index:put_channels(UpdatedChannels, Data).

-spec handle_channel_update_bulk(event_data(), guild_data()) -> guild_data().
handle_channel_update_bulk(EventData, Data) ->
    Channels = guild_data_index:channel_list(Data),
    BulkChannels = maps:get(<<"channels">>, EventData, []),
    UpdatedChannels = guild_state_utils:bulk_update_items(Channels, BulkChannels),
    guild_data_index:put_channels(UpdatedChannels, Data).

-spec handle_channel_delete(event_data(), guild_data()) -> guild_data().
handle_channel_delete(EventData, Data) ->
    Channels = guild_data_index:channel_list(Data),
    ChannelId = maps:get(<<"id">>, EventData),
    FilteredChannels = guild_state_utils:remove_item_by_id(Channels, ChannelId),
    guild_data_index:put_channels(FilteredChannels, Data).

-spec handle_message_create(event_data(), guild_data()) -> guild_data().
handle_message_create(EventData, Data) ->
    ChannelId = snowflake_id:parse_optional(
        maps:get(<<"channel_id">>, EventData, undefined)
    ),
    MessageId = snowflake_id:parse_optional(maps:get(<<"id">>, EventData, undefined)),
    advance_last_message_id(Data, ChannelId, MessageId).

-spec advance_last_message_id(guild_data(), integer() | undefined, integer() | undefined) ->
    guild_data().
advance_last_message_id(Data, _ChannelId, undefined) ->
    Data;
advance_last_message_id(Data, ChannelId, MessageId) ->
    Index = guild_data_index:channel_index(Data),
    case maps:find(ChannelId, Index) of
        {ok, Channel} ->
            Current = snowflake_id:parse_maybe(
                maps:get(<<"last_message_id">>, Channel, undefined)
            ),
            case is_integer(Current) andalso Current >= MessageId of
                true ->
                    Data;
                false ->
                    update_channel_field_fast(Data, ChannelId, <<"last_message_id">>, MessageId)
            end;
        error ->
            Data
    end.

-spec handle_channel_pins_update(event_data(), guild_data()) -> guild_data().
handle_channel_pins_update(EventData, Data) ->
    ChannelId = snowflake_id:parse_optional(
        maps:get(<<"channel_id">>, EventData, undefined)
    ),
    LastPin = maps:get(<<"last_pin_timestamp">>, EventData),
    update_channel_field_fast(Data, ChannelId, <<"last_pin_timestamp">>, LastPin).

-spec update_channel_field_fast(guild_data(), integer() | undefined, binary(), term()) ->
    guild_data().
update_channel_field_fast(Data, undefined, _, _) ->
    Data;
update_channel_field_fast(Data, ChannelId, Field, Value) ->
    Index = guild_data_index:channel_index(Data),
    case maps:find(ChannelId, Index) of
        {ok, Channel} ->
            Updated = Channel#{Field => Value},
            Data#{
                <<"channel_index">> => Index#{ChannelId => Updated},
                channels_stale => true
            };
        error ->
            Data
    end.

-spec handle_emojis_update(event_data(), guild_data()) -> guild_data().
handle_emojis_update(EventData, Data) ->
    Data#{<<"emojis">> => maps:get(<<"emojis">>, EventData, [])}.

-spec handle_stickers_update(event_data(), guild_data()) -> guild_data().
handle_stickers_update(EventData, Data) ->
    Data#{<<"stickers">> => maps:get(<<"stickers">>, EventData, [])}.

-spec extract_channel_ids_from_channel_update(event_data()) -> [integer()].
extract_channel_ids_from_channel_update(EventData) ->
    case snowflake_id:parse_optional(maps:get(<<"id">>, EventData, undefined)) of
        undefined -> [];
        ChannelId -> [ChannelId]
    end.

-spec extract_channel_ids_from_channel_update_bulk(event_data()) -> [integer()].
extract_channel_ids_from_channel_update_bulk(EventData) ->
    Channels = maps:get(<<"channels">>, EventData, []),
    lists:filtermap(fun extract_channel_id/1, Channels).

-spec extract_channel_id(map()) -> false | {true, integer()}.
extract_channel_id(ChannelData) ->
    case snowflake_id:parse_optional(maps:get(<<"id">>, ChannelData, undefined)) of
        undefined -> false;
        ChannelId -> {true, ChannelId}
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

handle_channel_create_test() ->
    Data = #{<<"channels">> => []},
    EventData = #{<<"id">> => <<"100">>, <<"name">> => <<"general">>},
    Result = handle_channel_create(EventData, Data),
    Channels = maps:get(<<"channels">>, Result),
    ?assertEqual(1, length(Channels)).

handle_channel_update_test() ->
    Data = #{
        <<"channels">> => [
            #{<<"id">> => <<"100">>, <<"name">> => <<"old">>},
            #{<<"id">> => <<"101">>, <<"name">> => <<"keep">>}
        ]
    },
    EventData = #{<<"id">> => <<"100">>, <<"name">> => <<"updated">>},
    Result = handle_channel_update(EventData, Data),
    Channels = guild_data_index:channel_list(Result),
    [C1, C2] = Channels,
    ?assertEqual(<<"updated">>, maps:get(<<"name">>, C1)),
    ?assertEqual(<<"keep">>, maps:get(<<"name">>, C2)).

handle_channel_update_bulk_test() ->
    Data = #{
        <<"channels">> => [
            #{<<"id">> => <<"1">>, <<"name">> => <<"A">>},
            #{<<"id">> => <<"2">>, <<"name">> => <<"B">>}
        ]
    },
    EventData = #{
        <<"channels">> => [
            #{<<"id">> => <<"2">>, <<"name">> => <<"B2">>}
        ]
    },
    Result = handle_channel_update_bulk(EventData, Data),
    Channels = guild_data_index:channel_list(Result),
    [C1, C2] = Channels,
    ?assertEqual(<<"A">>, maps:get(<<"name">>, C1)),
    ?assertEqual(<<"B2">>, maps:get(<<"name">>, C2)).

handle_channel_delete_test() ->
    Data = #{
        <<"channels">> => [
            #{<<"id">> => <<"100">>, <<"name">> => <<"general">>},
            #{<<"id">> => <<"101">>, <<"name">> => <<"random">>}
        ]
    },
    EventData = #{<<"id">> => <<"100">>},
    Result = handle_channel_delete(EventData, Data),
    Channels = guild_data_index:channel_list(Result),
    ?assertEqual(1, length(Channels)),
    ?assertEqual(<<"random">>, maps:get(<<"name">>, hd(Channels))).

handle_message_create_updates_last_message_id_test() ->
    Data = #{
        <<"channels">> => [
            #{<<"id">> => <<"100">>, <<"last_message_id">> => <<"500">>},
            #{<<"id">> => <<"101">>, <<"last_message_id">> => <<"600">>}
        ]
    },
    EventData = #{<<"channel_id">> => <<"100">>, <<"id">> => <<"700">>},
    Result = handle_message_create(EventData, Data),
    Channels = guild_data_index:channel_list(Result),
    [C1, C2] = Channels,
    ?assertEqual(700, maps:get(<<"last_message_id">>, C1)),
    ?assertEqual(600, maps:get(<<"last_message_id">>, C2)).

handle_message_create_keeps_newer_last_message_id_test() ->
    Data = #{
        <<"channels">> => [
            #{<<"id">> => <<"100">>, <<"last_message_id">> => <<"900">>}
        ]
    },
    EventData = #{<<"channel_id">> => <<"100">>, <<"id">> => <<"700">>},
    Result = handle_message_create(EventData, Data),
    [C1] = guild_data_index:channel_list(Result),
    ?assertEqual(900, maps:get(<<"last_message_id">>, C1)).

handle_message_create_sets_last_message_id_when_missing_test() ->
    Data = #{
        <<"channels">> => [
            #{<<"id">> => <<"100">>, <<"last_message_id">> => null}
        ]
    },
    EventData = #{<<"channel_id">> => <<"100">>, <<"id">> => <<"700">>},
    Result = handle_message_create(EventData, Data),
    [C1] = guild_data_index:channel_list(Result),
    ?assertEqual(700, maps:get(<<"last_message_id">>, C1)).

handle_channel_pins_update_test() ->
    Data = #{
        <<"channels">> => [
            #{<<"id">> => <<"100">>}
        ]
    },
    Ts = <<"2024-01-01T00:00:00Z">>,
    EventData = #{<<"channel_id">> => <<"100">>, <<"last_pin_timestamp">> => Ts},
    Result = handle_channel_pins_update(EventData, Data),
    [Ch] = guild_data_index:channel_list(Result),
    ?assertEqual(<<"2024-01-01T00:00:00Z">>, maps:get(<<"last_pin_timestamp">>, Ch)).

handle_emojis_update_test() ->
    Data = #{<<"emojis">> => []},
    EventData = #{<<"emojis">> => [#{<<"id">> => <<"1">>}]},
    Result = handle_emojis_update(EventData, Data),
    ?assertEqual([#{<<"id">> => <<"1">>}], maps:get(<<"emojis">>, Result)).

handle_stickers_update_test() ->
    Data = #{<<"stickers">> => []},
    EventData = #{<<"stickers">> => [#{<<"id">> => <<"1">>}]},
    Result = handle_stickers_update(EventData, Data),
    ?assertEqual([#{<<"id">> => <<"1">>}], maps:get(<<"stickers">>, Result)).

handle_guild_update_merges_fields_test() ->
    Data = #{
        <<"guild">> => #{<<"name">> => <<"Old">>, <<"icon">> => <<"abc">>},
        <<"roles">> => [],
        <<"members">> => [],
        <<"channels">> => []
    },
    EventData = #{<<"name">> => <<"New">>, <<"description">> => <<"desc">>},
    Result = handle_guild_update(EventData, Data),
    Guild = maps:get(<<"guild">>, Result),
    ?assertEqual(<<"New">>, maps:get(<<"name">>, Guild)),
    ?assertEqual(<<"abc">>, maps:get(<<"icon">>, Guild)),
    ?assertEqual(<<"desc">>, maps:get(<<"description">>, Guild)).

extract_channel_ids_from_channel_update_test() ->
    ?assertEqual([42], extract_channel_ids_from_channel_update(#{<<"id">> => <<"42">>})),
    ?assertEqual([], extract_channel_ids_from_channel_update(#{})).

extract_channel_ids_from_channel_update_bulk_test() ->
    EventData = #{
        <<"channels">> => [
            #{<<"id">> => <<"10">>},
            #{<<"id">> => <<"11">>},
            #{<<"name">> => <<"missing_id">>}
        ]
    },
    ?assertEqual([10, 11], extract_channel_ids_from_channel_update_bulk(EventData)).

-endif.
