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

-module(guild_voice_connection).

-include_lib("fluxer_gateway/include/voice_state.hrl").

-export([voice_state_update/2]).
-export([confirm_voice_connection_from_livekit/2]).
-export([request_voice_token/4]).

-type guild_state() :: map().
-type voice_state() :: map().
-type voice_state_map() :: #{binary() => voice_state()}.
-type pending_voice_connections() :: #{binary() => map()}.
-type context() :: #{
    user_id := integer() | undefined,
    channel_id := integer() | null | undefined,
    session_id := term(),
    connection_id := binary() | undefined,
    raw_connection_id := term(),
    self_mute := boolean(),
    self_deaf := boolean(),
    self_video := boolean(),
    self_stream := boolean(),
    is_mobile := boolean(),
    viewer_stream_key := term()
}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-ifdef(TEST).
-define(LOG_INVALID_GUILD_ID(_Value), ok).
-else.
-define(LOG_INVALID_GUILD_ID(Value),
    logger:warning(
        "[guild_voice_connection] Invalid guild_id value: ~p", [Value]
    )
).
-endif.

voice_state_update(Request, State) ->
    Context = build_context(Request),
    case maps:get(user_id, Context) of
        undefined ->
            gateway_errors:error(voice_invalid_user_id);
        UserId ->
            logger:debug(
                "[guild_voice_connection] Processing voice state update: UserId=~p, ChannelId=~p, "
                "ConnectionId=~p",
                [UserId, maps:get(channel_id, Context), maps:get(connection_id, Context)]
            ),
            VoiceStates = voice_state_utils:voice_states(State),
            case guild_voice_member:find_member_by_user_id(UserId, State) of
                undefined ->
                    logger:warning("[guild_voice_connection] Member not found for UserId: ~p", [
                        UserId
                    ]),
                    gateway_errors:error(voice_member_not_found);
                Member ->
                    handle_member_voice(Context, Member, VoiceStates, State)
            end
    end.

-spec handle_member_voice(context(), map(), voice_state_map(), guild_state()) ->
    {reply, map(), guild_state()} | {error, atom(), binary()}.
handle_member_voice(Context, Member, VoiceStates, State) ->
    case maps:get(channel_id, Context) of
        undefined ->
            gateway_errors:error(voice_invalid_channel_id);
        null ->
            handle_disconnect(Context, VoiceStates, State);
        ChannelIdValue ->
            handle_voice_connect_or_update(Context, ChannelIdValue, Member, VoiceStates, State)
    end.

handle_disconnect(Context, VoiceStates, State) ->
    guild_voice_disconnect:handle_voice_disconnect(
        maps:get(raw_connection_id, Context),
        maps:get(session_id, Context),
        maps:get(user_id, Context),
        VoiceStates,
        State
    ).

handle_voice_connect_or_update(Context, ChannelIdValue, Member, VoiceStates, State) ->
    ConnectionId = maps:get(connection_id, Context),
    Channel = guild_voice_member:find_channel_by_id(ChannelIdValue, State),

    case Channel of
        undefined ->
            logger:warning("[guild_voice_connection] Channel not found: ~p", [ChannelIdValue]),
            gateway_errors:error(voice_channel_not_found);
        _ ->
            case ConnectionId of
                undefined ->
                    handle_new_connection(Context, Member, Channel, VoiceStates, State);
                _ ->
                    handle_update_connection(
                        Context,
                        ChannelIdValue,
                        Member,
                        Channel,
                        VoiceStates,
                        State
                    )
            end
    end.

handle_update_connection(Context, ChannelIdValue, Member, Channel, VoiceStates, State) ->
    ConnectionId = maps:get(connection_id, Context),

    case maps:get(ConnectionId, VoiceStates, undefined) of
        undefined ->
            gateway_errors:error(voice_connection_not_found);
        ExistingVoiceState ->
            ExistingChannelIdBin = maps:get(<<"channel_id">>, ExistingVoiceState, null),
            NewChannelIdBin = integer_to_binary(ChannelIdValue),
            IsChannelChange = ExistingChannelIdBin =/= NewChannelIdBin,
            UserId = maps:get(user_id, Context),
            GuildId = map_utils:get_integer(State, id, undefined),
            ViewerKeyResult =
                resolve_viewer_stream_key(
                    Context, GuildId, ChannelIdValue, VoiceStates, ExistingVoiceState
                ),

            PermCheck =
                case IsChannelChange of
                    true ->
                        guild_voice_permissions:check_voice_permissions_and_limits(
                            UserId, ChannelIdValue, Channel, VoiceStates, State, false
                        );
                    false ->
                        {ok, allowed}
                end,

            case PermCheck of
                {error, _Category, ErrorAtom} ->
                    {reply, gateway_errors:error(ErrorAtom), State};
                {ok, allowed} ->
                    case ViewerKeyResult of
                        {error, ErrorAtom} ->
                            {reply, gateway_errors:error(ErrorAtom), State};
                        {ok, ParsedViewerKey} ->
                            Flags = voice_state_utils:voice_flags_from_context(Context),
                            guild_voice_state:update_voice_state_data(
                                ConnectionId,
                                NewChannelIdBin,
                                Flags,
                                Member,
                                ExistingVoiceState,
                                VoiceStates,
                                State,
                                false,
                                ParsedViewerKey
                            )
                    end
            end
    end.

handle_new_connection(Context, Member, Channel, VoiceStates, State) ->
    UserId = maps:get(user_id, Context),
    ChannelIdValue = maps:get(channel_id, Context),
    GuildId = map_utils:get_integer(State, id, undefined),
    ViewerKeyResult = resolve_viewer_stream_key(Context, GuildId, ChannelIdValue, VoiceStates, #{}),

    PermCheck = guild_voice_permissions:check_voice_permissions_and_limits(
        UserId, ChannelIdValue, Channel, VoiceStates, State, false
    ),

    case {PermCheck, ViewerKeyResult} of
        {{error, _Category, ErrorAtom}, _} ->
            {reply, gateway_errors:error(ErrorAtom), State};
        {{ok, allowed}, {error, ErrorAtom}} ->
            {reply, gateway_errors:error(ErrorAtom), State};
        {{ok, allowed}, {ok, ParsedViewerKey}} ->
            get_voice_token_and_create_state(Context, Member, ParsedViewerKey, State)
    end.

get_voice_token_and_create_state(Context, Member, ParsedViewerStreamKey, State) ->
    ChannelIdValue = maps:get(channel_id, Context),
    UserId = maps:get(user_id, Context),
    SessionId = maps:get(session_id, Context),

    case resolve_guild_identity(State) of
        {error, ErrorAtom} ->
            {reply, gateway_errors:error(ErrorAtom), State};
        {ok, GuildId, GuildIdBin} ->
            logger:info(
                "[guild_voice_connection] Requesting voice token for GuildId=~p, ChannelId=~p, UserId=~p",
                [GuildId, ChannelIdValue, UserId]
            ),
            VoicePermissions = voice_utils:compute_voice_permissions(UserId, ChannelIdValue, State),
            logger:debug("[guild_voice_connection] Computed voice permissions: ~p", [
                VoicePermissions
            ]),
            case request_voice_token(GuildId, ChannelIdValue, UserId, VoicePermissions) of
                {ok, TokenData} ->
                    logger:debug("[guild_voice_connection] Voice token request succeeded"),
                    Token = maps:get(token, TokenData),
                    Endpoint = maps:get(endpoint, TokenData),
                    ConnectionId = maps:get(connection_id, TokenData),

                    ChannelIdBin = integer_to_binary(ChannelIdValue),
                    UserIdBin = integer_to_binary(UserId),
                    ServerMute = maps:get(<<"mute">>, Member, false),
                    ServerDeaf = maps:get(<<"deaf">>, Member, false),

                    Flags = voice_state_utils:voice_flags_from_context(Context),
                    #voice_flags{
                        self_mute = SelfMuteFlag,
                        self_deaf = SelfDeafFlag,
                        self_video = SelfVideoFlag,
                        self_stream = SelfStreamFlag,
                        is_mobile = IsMobileFlag
                    } = Flags,
                    SessionIdValue = maps:get(session_id, Context, undefined),
                    SessionIdBin = normalize_session_id(SessionIdValue),

                    VoiceState0 = guild_voice_state:create_voice_state(
                        GuildIdBin,
                        ChannelIdBin,
                        UserIdBin,
                        ConnectionId,
                        ServerMute,
                        ServerDeaf,
                        Flags,
                        ParsedViewerStreamKey
                    ),
                    VoiceState1 = maybe_attach_session_id(VoiceState0, SessionIdBin),
                    VoiceState = maybe_attach_member(VoiceState1, Member),

                    VoiceStates = voice_state_utils:voice_states(State),
                    NewVoiceStates = maps:put(ConnectionId, VoiceState, VoiceStates),
                    StateWithVoiceStates = maps:put(voice_states, NewVoiceStates, State),

                    guild_voice_broadcast:broadcast_voice_state_update(
                        VoiceState, StateWithVoiceStates, ChannelIdBin
                    ),

                    PendingMetadata = #{
                        user_id => UserId,
                        guild_id => GuildId,
                        channel_id => ChannelIdValue,
                        session_id => SessionIdBin,
                        self_mute => SelfMuteFlag,
                        self_deaf => SelfDeafFlag,
                        self_video => SelfVideoFlag,
                        self_stream => SelfStreamFlag,
                        is_mobile => IsMobileFlag,
                        server_mute => ServerMute,
                        server_deaf => ServerDeaf,
                        member => Member,
                        viewer_stream_key => ParsedViewerStreamKey
                    },
                    PendingConnections = pending_voice_connections(StateWithVoiceStates),
                    NewPendingConnections = maps:put(
                        ConnectionId,
                        PendingMetadata,
                        PendingConnections
                    ),
                    NewState = maps:put(
                        pending_voice_connections, NewPendingConnections, StateWithVoiceStates
                    ),

                    maybe_broadcast_voice_server_update(
                        SessionId, GuildId, Token, Endpoint, ConnectionId, NewState
                    ),

                    {reply,
                        #{
                            success => true,
                            token => Token,
                            endpoint => Endpoint,
                            connection_id => ConnectionId,
                            voice_state => VoiceState
                        },
                        NewState};
                {error, Reason} ->
                    logger:error("[guild_voice_connection] Failed to request voice token: ~p", [
                        Reason
                    ]),
                    {reply, gateway_errors:error(voice_token_failed), State}
            end
    end.

-spec build_context(map()) -> context().
build_context(Request0) ->
    Request = map_utils:ensure_map(Request0),
    RawConnectionId = maps:get(connection_id, Request, undefined),
    #{
        user_id => normalize_user_id(maps:get(user_id, Request, undefined)),
        channel_id => normalize_channel_id_value(maps:get(channel_id, Request, null)),
        session_id => maps:get(session_id, Request, undefined),
        connection_id => normalize_connection_id(RawConnectionId),
        raw_connection_id => RawConnectionId,
        self_mute => normalize_boolean(maps:get(self_mute, Request, false)),
        self_deaf => normalize_boolean(maps:get(self_deaf, Request, false)),
        self_video => normalize_boolean(maps:get(self_video, Request, false)),
        self_stream => normalize_boolean(maps:get(self_stream, Request, false)),
        is_mobile => normalize_boolean(maps:get(is_mobile, Request, false)),
        viewer_stream_key => maps:get(viewer_stream_key, Request, undefined)
    }.

normalize_connection_id(undefined) ->
    undefined;
normalize_connection_id(null) ->
    undefined;
normalize_connection_id(ConnectionId) ->
    ConnectionId.

-spec normalize_user_id(term()) -> integer() | undefined.
normalize_user_id(Value) ->
    type_conv:to_integer(Value).

-spec normalize_channel_id_value(term()) -> integer() | null | undefined.
normalize_channel_id_value(null) ->
    null;
normalize_channel_id_value(Value) ->
    type_conv:to_integer(Value).

-spec normalize_boolean(term()) -> boolean().
normalize_boolean(true) -> true;
normalize_boolean(<<"true">>) -> true;
normalize_boolean(false) -> false;
normalize_boolean(<<"false">>) -> false;
normalize_boolean(_) -> false.

maybe_attach_session_id(VoiceState, undefined) ->
    VoiceState;
maybe_attach_session_id(VoiceState, SessionId) when is_binary(SessionId) ->
    maps:put(<<"session_id">>, SessionId, VoiceState).

maybe_attach_member(VoiceState, Member) when is_map(Member) ->
    case maps:size(Member) of
        0 -> VoiceState;
        _ -> maps:put(<<"member">>, Member, VoiceState)
    end.

normalize_session_id(undefined) ->
    undefined;
normalize_session_id(null) ->
    undefined;
normalize_session_id(SessionId) when is_binary(SessionId) ->
    SessionId;
normalize_session_id(SessionId) when is_integer(SessionId) ->
    integer_to_binary(SessionId);
normalize_session_id(SessionId) when is_list(SessionId) ->
    list_to_binary(SessionId);
normalize_session_id(SessionId) ->
    try
        erlang:iolist_to_binary(SessionId)
    catch
        _:_ -> undefined
    end.

resolve_viewer_stream_key(Context, GuildId, ChannelIdValue, VoiceStates, ExistingVoiceState) ->
    RawKey = maps:get(viewer_stream_key, Context, undefined),
    case RawKey of
        undefined ->
            {ok, maps:get(<<"viewer_stream_key">>, ExistingVoiceState, null)};
        null ->
            {ok, null};
        _ when not is_binary(RawKey) ->
            {error, voice_invalid_state};
        _ ->
            case voice_state_utils:parse_stream_key(RawKey) of
                {error, _} ->
                    {error, voice_invalid_state};
                {ok, #{
                    scope := guild,
                    guild_id := ParsedGuildId,
                    channel_id := ParsedChannelId,
                    connection_id := StreamConnId
                }} when
                    is_integer(ChannelIdValue), ParsedChannelId =:= ChannelIdValue
                ->
                    GuildScopeCheck =
                        case GuildId of
                            undefined -> ok;
                            ParsedGuildId -> ok;
                            _ -> error
                        end,
                    case GuildScopeCheck of
                        ok ->
                            case maps:get(StreamConnId, VoiceStates, undefined) of
                                undefined ->
                                    {error, voice_connection_not_found};
                                StreamVS ->
                                    case
                                        map_utils:get_integer(StreamVS, <<"channel_id">>, undefined)
                                    of
                                        ChannelIdValue -> {ok, RawKey};
                                        _ -> {error, voice_invalid_state}
                                    end
                            end;
                        error ->
                            {error, voice_invalid_state}
                    end;
                {ok, #{scope := dm, channel_id := ParsedChannelId}} when
                    is_integer(ChannelIdValue), ParsedChannelId =:= ChannelIdValue
                ->
                    {ok, RawKey};
                _ ->
                    {error, voice_invalid_state}
            end
    end.

resolve_voice_state_from_pending(ConnectionId, PendingData, State, VoiceStates) ->
    case maps:get(ConnectionId, VoiceStates, undefined) of
        VoiceState when is_map(VoiceState) ->
            VoiceState;
        _ ->
            case maps:get(voice_state, PendingData, undefined) of
                VoiceState when is_map(VoiceState) ->
                    VoiceState;
                _ ->
                    build_voice_state_from_pending(PendingData, ConnectionId, State)
            end
    end.

build_voice_state_from_pending(PendingData, ConnectionId, State) ->
    GuildIdState = map_utils:get_integer(State, id, undefined),
    GuildId0 = pending_get_integer(PendingData, guild_id),
    GuildId =
        case GuildId0 of
            undefined -> GuildIdState;
            _ -> GuildId0
        end,
    ChannelId = pending_get_integer(PendingData, channel_id),
    UserId = pending_get_integer(PendingData, user_id),
    case {GuildId, ChannelId, UserId} of
        {undefined, _, _} ->
            undefined;
        {_, undefined, _} ->
            undefined;
        {_, _, undefined} ->
            undefined;
        {GId, ChId, UId} ->
            GuildIdBin = integer_to_binary(GId),
            ChannelIdBin = integer_to_binary(ChId),
            UserIdBin = integer_to_binary(UId),
            Flags = #voice_flags{
                self_mute = pending_get_boolean(PendingData, self_mute),
                self_deaf = pending_get_boolean(PendingData, self_deaf),
                self_video = pending_get_boolean(PendingData, self_video),
                self_stream = pending_get_boolean(PendingData, self_stream),
                is_mobile = pending_get_boolean(PendingData, is_mobile)
            },
            ServerMute = pending_get_boolean(PendingData, server_mute),
            ServerDeaf = pending_get_boolean(PendingData, server_deaf),
            ViewerStreamKey = pending_get_value(PendingData, viewer_stream_key),
            VoiceState0 = guild_voice_state:create_voice_state(
                GuildIdBin,
                ChannelIdBin,
                UserIdBin,
                ConnectionId,
                ServerMute,
                ServerDeaf,
                Flags,
                ViewerStreamKey
            ),
            SessionId = pending_get_binary(PendingData, session_id),
            Member = pending_get_map(PendingData, member),
            VoiceState1 = maybe_attach_session_id(VoiceState0, SessionId),
            maybe_attach_member(VoiceState1, Member)
    end.

pending_get_value(PendingData, Key) ->
    case maps:get(Key, PendingData, undefined) of
        undefined ->
            BinKey = atom_to_binary(Key, utf8),
            maps:get(BinKey, PendingData, undefined);
        Value ->
            Value
    end.

pending_get_integer(PendingData, Key) ->
    case pending_get_value(PendingData, Key) of
        undefined -> undefined;
        Value -> type_conv:to_integer(Value)
    end.

pending_get_boolean(PendingData, Key) ->
    case pending_get_value(PendingData, Key) of
        true -> true;
        false -> false;
        _ -> false
    end.

pending_get_binary(PendingData, Key) ->
    case pending_get_value(PendingData, Key) of
        undefined -> undefined;
        Value -> type_conv:to_binary(Value)
    end.

pending_get_map(PendingData, Key) ->
    case pending_get_value(PendingData, Key) of
        Map when is_map(Map) -> Map;
        _ -> #{}
    end.

resolve_guild_identity(State) ->
    Data = guild_data(State),
    DataGuildIdBin = maps:get(<<"id">>, Data, undefined),
    StateGuildId = map_utils:get_integer(State, id, undefined),
    GuildMeta = map_utils:ensure_map(maps:get(<<"guild">>, Data, #{})),
    GuildMetaIdBin = maps:get(<<"id">>, GuildMeta, undefined),

    resolve_guild_id_priority([
        {DataGuildIdBin, fun normalize_guild_id/1},
        {StateGuildId, fun normalize_guild_id/1},
        {GuildMetaIdBin, fun normalize_guild_id/1}
    ]).

resolve_guild_id_priority([]) ->
    logger:error("[guild_voice_connection] Missing guild id in state"),
    {error, voice_guild_id_missing};
resolve_guild_id_priority([{undefined, _} | Rest]) ->
    resolve_guild_id_priority(Rest);
resolve_guild_id_priority([{Value, NormalizeFun} | _]) ->
    NormalizeFun(Value).

normalize_guild_id(Value) ->
    case type_conv:to_integer(Value) of
        undefined ->
            ?LOG_INVALID_GUILD_ID(Value),
            {error, voice_invalid_guild_id};
        Int ->
            {ok, Int, guild_id_binary(Value, Int)}
    end.

guild_id_binary(Value, Int) ->
    case type_conv:to_binary(Value) of
        undefined -> integer_to_binary(Int);
        Bin -> Bin
    end.

maybe_broadcast_voice_server_update(undefined, _GuildId, _Token, _Endpoint, _ConnectionId, _State) ->
    ok;
maybe_broadcast_voice_server_update(null, _GuildId, _Token, _Endpoint, _ConnectionId, _State) ->
    ok;
maybe_broadcast_voice_server_update(SessionId, GuildId, Token, Endpoint, ConnectionId, State) ->
    guild_voice_broadcast:broadcast_voice_server_update_to_session(
        GuildId, SessionId, Token, Endpoint, ConnectionId, State
    ).

guild_data(State) ->
    map_utils:ensure_map(maps:get(data, State, #{})).

confirm_voice_connection_from_livekit(Request, State) ->
    ConnectionId = maps:get(connection_id, Request, undefined),

    logger:info(
        "[guild_voice_connection] confirm_voice_connection_from_livekit connection_id=~p pending_count=~p",
        [ConnectionId, maps:size(pending_voice_connections(State))]
    ),

    case ConnectionId of
        undefined ->
            gateway_errors:error(voice_missing_connection_id);
        _ ->
            PendingConnections = pending_voice_connections(State),

            case maps:get(ConnectionId, PendingConnections, undefined) of
                undefined ->
                    logger:warning(
                        "[guild_voice_connection] confirm_voice_connection_from_livekit missing pending connection_id=~p",
                        [ConnectionId]
                    ),
                    gateway_errors:error(voice_connection_not_found);
                PendingData ->
                    logger:info(
                        "[guild_voice_connection] Found pending connection_id=~p for guild=~p",
                        [ConnectionId, map_utils:get_integer(State, id, 0)]
                    ),
                    VoiceStates = voice_state_utils:voice_states(State),
                    VoiceState = resolve_voice_state_from_pending(
                        ConnectionId, PendingData, State, VoiceStates
                    ),

                    NewPendingConnections = maps:remove(ConnectionId, PendingConnections),
                    StateWithoutPending = maps:put(
                        pending_voice_connections, NewPendingConnections, State
                    ),

                    case VoiceState of
                        undefined ->
                            logger:warning(
                                "[guild_voice_connection] Missing voice_state for confirmed connection ~p",
                                [ConnectionId]
                            ),
                            {reply, #{success => true}, StateWithoutPending};
                        _ ->
                            UpdatedVoiceStates = maps:put(ConnectionId, VoiceState, VoiceStates),
                            StateWithVoiceStates = maps:put(
                                voice_states, UpdatedVoiceStates, StateWithoutPending
                            ),

                            ChannelIdBin = maps:get(<<"channel_id">>, VoiceState, null),
                            UserId = maps:get(<<"user_id">>, VoiceState, <<"unknown">>),
                            GuildId = maps:get(id, StateWithVoiceStates, 0),
                            Sessions = maps:get(sessions, StateWithVoiceStates, #{}),
                            logger:info(
                                "[guild_voice_connection] confirm_voice_connection_from_livekit: "
                                "guild_id=~p user_id=~p channel_id=~p connection_id=~p sessions_count=~p",
                                [GuildId, UserId, ChannelIdBin, ConnectionId, maps:size(Sessions)]
                            ),
                            guild_voice_broadcast:broadcast_voice_state_update(
                                VoiceState, StateWithVoiceStates, ChannelIdBin
                            ),

                            {reply, #{success => true}, StateWithVoiceStates}
                    end
            end
    end.

-spec request_voice_token(integer(), integer(), integer(), map()) ->
    {ok, map()} | {error, term()}.
request_voice_token(GuildId, ChannelId, UserId, VoicePermissions) ->
    Req = voice_utils:build_voice_token_rpc_request(
        GuildId, ChannelId, UserId, null, null, null, VoicePermissions
    ),
    case rpc_client:call(Req) of
        {ok, Data} ->
            {ok, #{
                token => maps:get(<<"token">>, Data),
                endpoint => maps:get(<<"endpoint">>, Data),
                connection_id => maps:get(<<"connectionId">>, Data)
            }};
        {error, Reason} ->
            logger:error("[guild_voice_connection] RPC request failed: ~p", [Reason]),
            {error, Reason}
    end.

-spec pending_voice_connections(guild_state()) -> pending_voice_connections().
pending_voice_connections(State) ->
    case maps:get(pending_voice_connections, State, undefined) of
        Map when is_map(Map) -> Map;
        _ -> #{}
    end.

-ifdef(TEST).

build_context_normalizes_fields_test() ->
    Request = #{
        user_id => <<"42">>,
        channel_id => <<"99">>,
        connection_id => <<"conn">>,
        self_mute => true,
        self_deaf => <<"nope">>,
        self_video => true,
        self_stream => false,
        is_mobile => <<"yes">>
    },
    Context = build_context(Request),
    ?assertEqual(42, maps:get(user_id, Context)),
    ?assertEqual(99, maps:get(channel_id, Context)),
    ?assertEqual(<<"conn">>, maps:get(connection_id, Context)),
    ?assertEqual(true, maps:get(self_mute, Context)),
    ?assertEqual(false, maps:get(self_deaf, Context)),
    ?assertEqual(true, maps:get(self_video, Context)),
    ?assertEqual(false, maps:get(self_stream, Context)),
    ?assertEqual(false, maps:get(is_mobile, Context)).

resolve_guild_identity_prefers_data_test() ->
    State = #{
        id => 7,
        data => #{
            <<"id">> => <<"555">>,
            <<"guild">> => #{<<"id">> => <<"111">>}
        }
    },
    ?assertMatch({ok, 555, <<"555">>}, resolve_guild_identity(State)).

normalize_guild_id_invalid_test() ->
    ?assertMatch({error, voice_invalid_guild_id}, normalize_guild_id(foo)).

voice_state_update_invalid_user_id_test() ->
    {error, validation_error, voice_invalid_user_id} =
        voice_state_update(#{channel_id => null}, #{}).

-endif.
