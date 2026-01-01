%% Copyright (C) 2026 Fluxer Contributors
%%
%% This file is part of Fluxer.
%%
%% Fluxer is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Affero General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% Fluxer is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%% GNU Affero General Public License for more details.
%%
%% You should have received a copy of the GNU Affero General Public License
%% along with Fluxer. If not, see <https://www.gnu.org/licenses/>.

-module(voice_state_utils).

-include_lib("fluxer_gateway/include/voice_state.hrl").

-export([
    voice_states/1,
    ensure_voice_states/1,
    voice_state_user_id/1,
    voice_state_channel_id/1,
    voice_state_guild_id/1,
    filter_voice_states/2,
    drop_voice_states/2,
    broadcast_disconnects/2,
    voice_flags_from_context/1,
    parse_stream_key/1,
    build_stream_key/3
]).

voice_states(State) when is_map(State) ->
    case maps:get(voice_states, State, undefined) of
        Map when is_map(Map) -> Map;
        _ -> #{}
    end.

ensure_voice_states(Map) when is_map(Map) ->
    Map;
ensure_voice_states(_) ->
    #{}.

voice_state_user_id(VoiceState) ->
    map_utils:get_integer(VoiceState, <<"user_id">>, undefined).

voice_state_channel_id(VoiceState) ->
    map_utils:get_integer(VoiceState, <<"channel_id">>, undefined).

voice_state_guild_id(VoiceState) ->
    map_utils:get_integer(VoiceState, <<"guild_id">>, undefined).

filter_voice_states(VoiceStates, Predicate) when is_map(VoiceStates) ->
    maps:filter(Predicate, VoiceStates);
filter_voice_states(_, _) ->
    #{}.

drop_voice_states(ToDrop, VoiceStates) ->
    maps:fold(fun(ConnId, _VoiceState, Acc) -> maps:remove(ConnId, Acc) end, VoiceStates, ToDrop).

broadcast_disconnects(VoiceStates, State) ->
    maps:foreach(
        fun(ConnId, VoiceState) ->
            OldChannelIdBin = maps:get(<<"channel_id">>, VoiceState, null),
            DisconnectVoiceState = VoiceState#{
                <<"channel_id">> => null,
                <<"connection_id">> => ConnId
            },
            guild_voice_broadcast:broadcast_voice_state_update(
                DisconnectVoiceState, State, OldChannelIdBin
            )
        end,
        VoiceStates
    ).

voice_flags_from_context(Context) ->
    #voice_flags{
        self_mute = maps:get(self_mute, Context, false),
        self_deaf = maps:get(self_deaf, Context, false),
        self_video = maps:get(self_video, Context, false),
        self_stream = maps:get(self_stream, Context, false),
        is_mobile = maps:get(is_mobile, Context, false)
    }.

-spec parse_stream_key(term()) ->
    {ok, #{
        scope := guild | dm,
        guild_id := integer() | undefined,
        channel_id := integer(),
        connection_id := binary()
    }}
    | {error, invalid_stream_key}.
parse_stream_key(StreamKey) when is_binary(StreamKey) ->
    Parts = binary:split(StreamKey, <<":">>, [global]),
    case Parts of
        [ScopeBin, ChannelBin, ConnId] when byte_size(ChannelBin) > 0, byte_size(ConnId) > 0 ->
            try
                Scope = parse_scope_bin(ScopeBin),
                ChannelId = parse_channel_bin(ChannelBin),
                build_stream_key_result(Scope, ChannelId, ConnId)
            catch
                _:_ ->
                    {error, invalid_stream_key}
            end;
        _ ->
            {error, invalid_stream_key}
    end;
parse_stream_key(_) ->
    {error, invalid_stream_key}.

-spec parse_scope_bin(binary()) -> {dm, undefined} | {guild, integer()}.
parse_scope_bin(<<"dm">>) ->
    {dm, undefined};
parse_scope_bin(ScopeBin) ->
    GuildId = type_conv:to_integer(ScopeBin),
    true = is_integer(GuildId),
    {guild, GuildId}.

-spec parse_channel_bin(binary()) -> integer().
parse_channel_bin(ChannelBin) ->
    Chan = type_conv:to_integer(ChannelBin),
    true = is_integer(Chan),
    Chan.

-spec build_stream_key_result({dm, undefined} | {guild, integer()}, integer(), binary()) ->
    {ok, #{
        scope := guild | dm,
        guild_id := integer() | undefined,
        channel_id := integer(),
        connection_id := binary()
    }}.
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

-spec build_stream_key(integer() | undefined, integer(), binary()) -> binary().
build_stream_key(undefined, ChannelId, ConnectionId) when
    is_integer(ChannelId), is_binary(ConnectionId)
->
    <<"dm:", (integer_to_binary(ChannelId))/binary, ":", ConnectionId/binary>>;
build_stream_key(GuildId, ChannelId, ConnectionId) when
    is_integer(GuildId), is_integer(ChannelId), is_binary(ConnectionId)
->
    <<
        (integer_to_binary(GuildId))/binary,
        ":",
        (integer_to_binary(ChannelId))/binary,
        ":",
        ConnectionId/binary
    >>.
