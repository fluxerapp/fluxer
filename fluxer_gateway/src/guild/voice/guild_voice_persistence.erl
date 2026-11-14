%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_persistence).
-typing([eqwalizer]).

-export([persist_voice_state_update/2]).

-export_type([
    guild_state/0,
    voice_state/0
]).

-type guild_state() :: map().
-type voice_state() :: map().

-spec persist_voice_state_update(voice_state(), guild_state()) -> ok.
persist_voice_state_update(VoiceState, State) ->
    case resolve_guild_id(VoiceState, State) of
        GuildId when is_integer(GuildId), GuildId > 0 ->
            spawn_persist_voice_state_update(GuildId, VoiceState),
            ok;
        _ ->
            ok
    end.

-spec spawn_persist_voice_state_update(integer(), voice_state()) -> pid().
spawn_persist_voice_state_update(GuildId, VoiceState) ->
    spawn(fun() -> persist_voice_state_update_async(GuildId, VoiceState) end).

-spec resolve_guild_id(voice_state(), guild_state()) -> integer() | undefined.
resolve_guild_id(VoiceState, State) ->
    case
        snowflake_id:parse_maybe(
            maps:get(<<"guild_id">>, VoiceState, undefined)
        )
    of
        undefined ->
            snowflake_id:parse_maybe(
                maps:get(id, State, undefined)
            );
        GuildId ->
            GuildId
    end.

-spec persist_voice_state_update_async(integer(), voice_state()) -> ok.
persist_voice_state_update_async(GuildId, VoiceState) ->
    case maps:get(<<"connection_id">>, VoiceState, undefined) of
        ConnectionId when is_binary(ConnectionId), byte_size(ConnectionId) > 0 ->
            persist_voice_state_snapshot(GuildId, ConnectionId, VoiceState);
        _ ->
            ok
    end.

-spec persist_voice_state_snapshot(integer(), binary(), voice_state()) -> ok.
persist_voice_state_snapshot(GuildId, ConnectionId, VoiceState) ->
    case maps:get(<<"channel_id">>, VoiceState, null) of
        null ->
            Request = #{
                <<"type">> => <<"voice_state_remove">>,
                <<"guild_id">> => integer_to_binary(GuildId),
                <<"connection_id">> => ConnectionId
            },
            handle_rpc_result(Request, rpc_client:call(Request));
        _ ->
            Request = #{
                <<"type">> => <<"voice_state_upsert">>,
                <<"guild_id">> => integer_to_binary(GuildId),
                <<"voice_state">> => VoiceState
            },
            handle_rpc_result(Request, rpc_client:call(Request))
    end.

-spec handle_rpc_result(map(), {ok, map()} | {error, term()}) -> ok.
handle_rpc_result(_Request, {ok, _Response}) ->
    ok;
handle_rpc_result(Request, {error, Reason}) ->
    logger:warning("Failed to persist guild voice state snapshot", #{
        request_type => maps:get(<<"type">>, Request, undefined),
        guild_id => maps:get(<<"guild_id">>, Request, undefined),
        reason => Reason
    }),
    ok.
