%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(voice_state_utils).
-typing([eqwalizer]).

-include_lib("fluxer_gateway/include/voice_state.hrl").

-export([
    voice_states/1,
    ensure_voice_states/1,
    voice_state_user_id/1,
    voice_state_channel_id/1,
    voice_state_guild_id/1,
    voice_state_version/1,
    channel_voice_states/2,
    initial_voice_state_version/0,
    complete_voice_state/1,
    external_voice_state/1,
    sanitize_voice_state_for_broadcast/1,
    filter_voice_states/2,
    drop_voice_states/2,
    broadcast_disconnects/2,
    voice_flags_from_context/1,
    parse_stream_key/1,
    normalize_session_id/1
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export_type([
    voice_state/0,
    voice_state_map/0,
    guild_state/0,
    stream_key_result/0,
    voice_flags/0
]).

-define(INITIAL_VOICE_STATE_VERSION, 0).

-type voice_state() :: map().
-type voice_state_map() :: #{binary() => voice_state()}.
-type guild_state() :: map().
-type stream_key_result() :: #{
    scope := guild | dm,
    guild_id := integer() | undefined,
    channel_id := integer(),
    connection_id := binary()
}.

-spec voice_states(guild_state()) -> voice_state_map().
voice_states(State) when is_map(State) ->
    case maps:get(voice_states, State, undefined) of
        Map when is_map(Map) -> ensure_voice_states(Map);
        _ -> #{}
    end.

-spec ensure_voice_states(term()) -> voice_state_map().
ensure_voice_states(Map) when is_map(Map) ->
    maps:fold(
        fun
            (Key, Value, Acc) when is_binary(Key), is_map(Value) -> Acc#{Key => Value};
            (_Key, _Value, Acc) -> Acc
        end,
        #{},
        Map
    );
ensure_voice_states(_) ->
    #{}.

-spec voice_state_user_id(voice_state()) -> integer() | undefined.
voice_state_user_id(VoiceState) ->
    snowflake_field(VoiceState, <<"user_id">>).

-spec voice_state_channel_id(voice_state()) -> integer() | undefined.
voice_state_channel_id(VoiceState) ->
    snowflake_field(VoiceState, <<"channel_id">>).

-spec voice_state_guild_id(voice_state()) -> integer() | undefined.
voice_state_guild_id(VoiceState) ->
    snowflake_field(VoiceState, <<"guild_id">>).

-spec channel_voice_states(integer() | undefined, voice_state_map()) -> voice_state_map().
channel_voice_states(ChannelId, VoiceStates) ->
    maps:filter(
        fun(_ConnId, VoiceState) -> voice_state_channel_id(VoiceState) =:= ChannelId end,
        VoiceStates
    ).

-spec voice_state_version(voice_state()) -> non_neg_integer().
voice_state_version(VoiceState) ->
    case map_utils:get_integer(VoiceState, <<"version">>, undefined) of
        Version when is_integer(Version), Version >= ?INITIAL_VOICE_STATE_VERSION -> Version;
        _ -> ?INITIAL_VOICE_STATE_VERSION
    end.

-spec initial_voice_state_version() -> non_neg_integer().
initial_voice_state_version() ->
    ?INITIAL_VOICE_STATE_VERSION.

-spec complete_voice_state(voice_state()) -> voice_state().
complete_voice_state(VoiceState) when is_map(VoiceState) ->
    maps:merge(default_voice_state_fields(), VoiceState);
complete_voice_state(_) ->
    default_voice_state_fields().

-spec default_voice_state_fields() -> voice_state().
default_voice_state_fields() ->
    #{
        <<"guild_id">> => null,
        <<"channel_id">> => null,
        <<"user_id">> => null,
        <<"connection_id">> => null,
        <<"session_id">> => null,
        <<"member">> => null,
        <<"mute">> => false,
        <<"deaf">> => false,
        <<"self_mute">> => false,
        <<"self_deaf">> => false,
        <<"self_video">> => false,
        <<"self_stream">> => false,
        <<"is_mobile">> => false,
        <<"suppress">> => false,
        <<"viewer_stream_keys">> => [],
        <<"e2ee_capable">> => false,
        <<"p2p">> => false,
        <<"region_id">> => null,
        <<"server_id">> => null,
        <<"version">> => ?INITIAL_VOICE_STATE_VERSION
    }.

-spec external_voice_state(voice_state()) -> voice_state().
external_voice_state(VoiceState) ->
    maps:without(
        [<<"latitude">>, <<"longitude">>],
        complete_voice_state(VoiceState)
    ).

-spec sanitize_voice_state_for_broadcast(voice_state()) -> voice_state().
sanitize_voice_state_for_broadcast(VoiceState) ->
    maps:without(
        [<<"server_id">>, <<"region_id">>],
        external_voice_state(VoiceState)
    ).

-spec filter_voice_states(voice_state_map(), fun((binary(), voice_state()) -> boolean())) ->
    voice_state_map().
filter_voice_states(VoiceStates, Predicate) when is_map(VoiceStates) ->
    maps:filter(Predicate, VoiceStates);
filter_voice_states(_, _) ->
    #{}.

-spec drop_voice_states(voice_state_map(), voice_state_map()) -> voice_state_map().
drop_voice_states(ToDrop, VoiceStates) ->
    maps:fold(
        fun(ConnId, _VoiceState, Acc) -> maps:remove(ConnId, Acc) end, VoiceStates, ToDrop
    ).

-spec broadcast_disconnects(voice_state_map(), guild_state()) -> ok.
broadcast_disconnects(VoiceStates, State) ->
    spawn(fun() -> broadcast_disconnects_async(VoiceStates, State) end),
    ok.

-spec broadcast_disconnects_async(voice_state_map(), guild_state()) -> ok.
broadcast_disconnects_async(VoiceStates, State) ->
    maps:foreach(
        fun(ConnId, VoiceState) -> broadcast_disconnect(ConnId, VoiceState, State) end,
        VoiceStates
    ).

-spec broadcast_disconnect(binary(), voice_state(), guild_state()) -> ok.
broadcast_disconnect(ConnId, VoiceState, State) ->
    OldChannelIdBin = maps:get(<<"channel_id">>, VoiceState, null),
    DisconnectVoiceState = VoiceState#{
        <<"channel_id">> => null,
        <<"connection_id">> => ConnId
    },
    guild_voice_broadcast:broadcast_voice_state_update(
        DisconnectVoiceState, State, OldChannelIdBin
    ).

-spec voice_flags_from_context(map()) -> voice_flags().
voice_flags_from_context(Context) ->
    #{
        self_mute => maps:get(self_mute, Context, false),
        self_deaf => maps:get(self_deaf, Context, false),
        self_video => maps:get(self_video, Context, false),
        self_stream => maps:get(self_stream, Context, false),
        is_mobile => maps:get(is_mobile, Context, false),
        suppress => maps:get(suppress, Context, false)
    }.

-spec parse_stream_key(term()) -> {ok, stream_key_result()} | {error, invalid_stream_key}.
parse_stream_key(StreamKey) when is_binary(StreamKey) ->
    Parts = binary:split(StreamKey, <<":">>, [global]),
    case Parts of
        [ScopeBin, ChannelBin, ConnId] when byte_size(ChannelBin) > 0, byte_size(ConnId) > 0 ->
            parse_stream_key_parts(ScopeBin, ChannelBin, ConnId);
        _ ->
            {error, invalid_stream_key}
    end;
parse_stream_key(_) ->
    {error, invalid_stream_key}.

-spec parse_stream_key_parts(binary(), binary(), binary()) ->
    {ok, stream_key_result()} | {error, invalid_stream_key}.
parse_stream_key_parts(ScopeBin, ChannelBin, ConnId) ->
    try
        Scope = parse_scope_bin(ScopeBin),
        ChannelId = parse_channel_bin(ChannelBin),
        build_stream_key_result(Scope, ChannelId, ConnId)
    catch
        _:_ -> {error, invalid_stream_key}
    end.

-spec parse_scope_bin(binary()) -> {dm, undefined} | {guild, integer()}.
parse_scope_bin(<<"dm">>) ->
    {dm, undefined};
parse_scope_bin(ScopeBin) ->
    {guild, require_positive_snowflake(ScopeBin)}.

-spec parse_channel_bin(binary()) -> integer().
parse_channel_bin(ChannelBin) ->
    require_positive_snowflake(ChannelBin).

-spec build_stream_key_result({dm, undefined} | {guild, integer()}, integer(), binary()) ->
    {ok, stream_key_result()}.
build_stream_key_result({dm, _}, ChannelId, ConnId) ->
    {ok, #{
        scope => dm,
        guild_id => undefined,
        channel_id => ChannelId,
        connection_id => ConnId
    }};
build_stream_key_result({guild, GuildId}, ChannelId, ConnId) ->
    {ok, #{
        scope => guild,
        guild_id => GuildId,
        channel_id => ChannelId,
        connection_id => ConnId
    }}.

-spec positive_integer(term()) -> integer() | undefined.
positive_integer(Value) ->
    guild_voice_connection_normalize:normalize_positive_snowflake(Value).

-spec snowflake_field(term(), binary()) -> integer() | undefined.
snowflake_field(Map, Key) when is_map(Map) -> positive_integer(maps:get(Key, Map, undefined));
snowflake_field(_, _) -> undefined.

-spec require_positive_snowflake(term()) -> integer().
require_positive_snowflake(Value) ->
    Id = positive_integer(Value),
    true = is_integer(Id),
    Id.

-spec normalize_session_id(term()) -> binary() | undefined.
normalize_session_id(Value) ->
    guild_voice_connection_normalize:normalize_session_id(Value).

-ifdef(TEST).

parse_stream_key_dm_test() ->
    Result = parse_stream_key(<<"dm:123:conn-id">>),
    ?assertMatch(
        {ok, #{scope := dm, channel_id := 123, connection_id := <<"conn-id">>}}, Result
    ).

parse_stream_key_guild_test() ->
    Result = parse_stream_key(<<"999:123:conn-id">>),
    ?assertMatch(
        {ok, #{
            scope := guild, guild_id := 999, channel_id := 123, connection_id := <<"conn-id">>
        }},
        Result
    ).

parse_stream_key_invalid_test() ->
    ?assertEqual({error, invalid_stream_key}, parse_stream_key(<<"invalid">>)),
    ?assertEqual({error, invalid_stream_key}, parse_stream_key(<<"a:b">>)),
    ?assertEqual({error, invalid_stream_key}, parse_stream_key(<<"dm:001:conn">>)),
    ?assertEqual({error, invalid_stream_key}, parse_stream_key(<<"001:123:conn">>)),
    ?assertEqual({error, invalid_stream_key}, parse_stream_key(123)).

sanitize_voice_state_for_broadcast_test() ->
    VoiceState = #{
        <<"user_id">> => <<"1">>,
        <<"latitude">> => <<"1.0">>,
        <<"longitude">> => <<"2.0">>,
        <<"region_id">> => <<"us-east">>,
        <<"server_id">> => <<"voice-1">>
    },
    Sanitized = sanitize_voice_state_for_broadcast(VoiceState),
    ?assertNot(maps:is_key(<<"latitude">>, Sanitized)),
    ?assertNot(maps:is_key(<<"longitude">>, Sanitized)),
    ?assertNot(maps:is_key(<<"region_id">>, Sanitized)),
    ?assertNot(maps:is_key(<<"server_id">>, Sanitized)),
    ?assertEqual(<<"1">>, maps:get(<<"user_id">>, Sanitized)),
    ?assertEqual(null, maps:get(<<"channel_id">>, Sanitized)),
    ?assertEqual(false, maps:get(<<"suppress">>, Sanitized)),
    ?assertEqual(0, maps:get(<<"version">>, Sanitized)).

-endif.
