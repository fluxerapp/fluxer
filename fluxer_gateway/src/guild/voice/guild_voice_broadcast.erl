%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_broadcast).
-typing([eqwalizer]).

-export([broadcast_voice_state_update/3]).
-export([broadcast_voice_server_update_to_session/7]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export_type([
    guild_state/0,
    voice_state/0
]).

-type guild_state() :: map().
-type voice_state() :: map().

-spec broadcast_voice_state_update(voice_state(), guild_state(), binary() | null) -> ok.
broadcast_voice_state_update(VoiceState, State, OldChannelIdBin) ->
    case maps:get(<<"connection_id">>, VoiceState, undefined) of
        undefined ->
            ok;
        _ConnectionId ->
            do_broadcast_voice_state_update(VoiceState, State, OldChannelIdBin)
    end.

-spec do_broadcast_voice_state_update(voice_state(), guild_state(), binary() | null) -> ok.
do_broadcast_voice_state_update(VoiceState, State, OldChannelIdBin) ->
    Sessions = maps:get(sessions, State, #{}),
    FilterChannelIdBin = filter_channel_id_bin(VoiceState, OldChannelIdBin),
    FilterChannelId = utils:binary_to_integer_safe(FilterChannelIdBin),
    FilteredSessions = filter_sessions_for_voice_channel(Sessions, FilterChannelId, State),
    Pids = [maps:get(pid, S) || {_Sid, S} <- FilteredSessions],
    maybe_dispatch_voice_state_update(Pids, sanitize_voice_state(VoiceState), State),
    maybe_persist_voice_state_update(VoiceState, State),
    maybe_sync_guild_voice_state(VoiceState, OldChannelIdBin, State),
    ok.

-spec filter_channel_id_bin(voice_state(), binary() | null) -> binary() | null.
filter_channel_id_bin(VoiceState, OldChannelIdBin) ->
    case maps:get(<<"channel_id">>, VoiceState, null) of
        null -> OldChannelIdBin;
        ChannelIdBin -> ChannelIdBin
    end.

-spec sanitize_voice_state(voice_state()) -> voice_state().
sanitize_voice_state(VoiceState) ->
    voice_state_utils:sanitize_voice_state_for_broadcast(VoiceState).

-spec filter_sessions_for_voice_channel(map(), integer() | undefined, guild_state()) ->
    [{term(), map()}].
filter_sessions_for_voice_channel(Sessions, FilterChannelId, State) when
    is_integer(FilterChannelId)
->
    [
        {SessionId, Session}
     || {SessionId, Session} <- guild_sessions:filter_sessions_for_channel(
            Sessions, FilterChannelId, undefined, State
        ),
        is_map(Session)
    ];
filter_sessions_for_voice_channel(_Sessions, undefined, _State) ->
    [].

-spec broadcast_voice_server_update_to_session(
    integer(), integer(), binary(), binary(), binary(), binary(), guild_state()
) -> ok.
broadcast_voice_server_update_to_session(
    GuildId,
    ChannelId,
    SessionId,
    Token,
    Endpoint,
    ConnectionId,
    State
) ->
    VoiceServerUpdate = #{
        <<"token">> => Token,
        <<"endpoint">> => Endpoint,
        <<"guild_id">> => integer_to_binary(GuildId),
        <<"channel_id">> => integer_to_binary(ChannelId),
        <<"connection_id">> => ConnectionId
    },
    Sessions = maps:get(sessions, State, #{}),
    case maps:get(SessionId, Sessions, undefined) of
        undefined ->
            ok;
        SessionData ->
            maybe_dispatch_voice_server_update(SessionData, VoiceServerUpdate, GuildId)
    end.

-spec maybe_dispatch_voice_server_update(map(), map(), integer()) -> ok.
maybe_dispatch_voice_server_update(SessionData, VoiceServerUpdate, GuildId) ->
    case maps:get(pid, SessionData, null) of
        Pid when is_pid(Pid) ->
            gateway_dispatch_relay:dispatch(
                Pid, voice_server_update, VoiceServerUpdate, GuildId
            ),
            ok;
        _ ->
            ok
    end.

-spec maybe_dispatch_voice_state_update([term()], voice_state(), guild_state()) -> ok.
maybe_dispatch_voice_state_update(Pids, SanitizedVoiceState, State) ->
    case state_guild_id(State) of
        GuildId when is_integer(GuildId) ->
            gateway_dispatch_relay:dispatch_many(
                [Pid || Pid <- Pids, is_pid(Pid)],
                voice_state_update,
                dispatch_payload(SanitizedVoiceState),
                GuildId
            );
        undefined ->
            ok
    end.

-spec dispatch_payload(voice_state()) -> voice_state() | {pre_encoded, binary()}.
dispatch_payload(SanitizedVoiceState) ->
    case voice_state_utils:voice_state_guild_id(SanitizedVoiceState) of
        undefined ->
            SanitizedVoiceState;
        _GuildId ->
            {pre_encoded,
                iolist_to_binary(
                    json:encode(
                        guild_data_wire:payload(SanitizedVoiceState), fun json:encode_value/2
                    )
                )}
    end.

-spec state_guild_id(guild_state()) -> integer() | undefined.
state_guild_id(State) ->
    case map_utils:get_integer(State, id, undefined) of
        GuildId when is_integer(GuildId), GuildId > 0 -> GuildId;
        _ -> undefined
    end.

-spec maybe_persist_voice_state_update(map(), guild_state()) -> ok.
maybe_persist_voice_state_update(VoiceState, State) ->
    case maps:get(guild_pid, State, undefined) of
        GuildPid when is_pid(GuildPid) ->
            guild_voice_persistence:persist_voice_state_update(VoiceState, State);
        _ ->
            ok
    end.

-spec maybe_sync_guild_voice_state(map(), binary() | null, guild_state()) -> ok.
maybe_sync_guild_voice_state(VoiceState, OldChannelIdBin, State) ->
    case maps:get(guild_pid, State, undefined) of
        GuildPid when is_pid(GuildPid) ->
            gen_server:cast(GuildPid, {relay_voice_state_update, VoiceState, OldChannelIdBin}),
            ok;
        _ ->
            ok
    end.

-ifdef(TEST).

broadcast_voice_state_update_missing_connection_id_test() ->
    VoiceState = #{<<"user_id">> => <<"1">>},
    State = #{sessions => #{}},
    ?assertEqual(ok, broadcast_voice_state_update(VoiceState, State, null)).

-endif.
