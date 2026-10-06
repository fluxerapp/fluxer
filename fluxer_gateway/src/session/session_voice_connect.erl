%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_voice_connect).
-typing([eqwalizer]).

-export([
    handle_voice_state_update/2
]).

-export_type([session_state/0, voice_state_reply/0]).

-type session_state() :: session:session_state().
-type guild_id() :: session:guild_id().
-type channel_id() :: session:channel_id().
-type user_id() :: session:user_id().

-type voice_state_reply() ::
    {reply, ok, session_state()}
    | {reply, {error, term(), term()}, session_state()}.

-spec handle_voice_state_update(map(), session_state()) ->
    voice_state_reply().
handle_voice_state_update(Data, State) ->
    Params = extract_voice_params(Data),
    SessionId = maps:get(id, State),
    UserId = maps:get(user_id, State),
    E2EECapable = maps:get(e2ee_capable, State, false),
    Bot = maps:get(bot, State, false),
    Guilds = maps:get(guilds, State),
    GuildIdRaw = maps:get(guild_id_raw, Params),
    ChannelIdRaw = maps:get(channel_id_raw, Params),
    ConnId = maps:get(connection_id, Params),
    log_voice_received(
        UserId,
        SessionId,
        GuildIdRaw,
        ChannelIdRaw,
        ConnId,
        E2EECapable,
        Bot
    ),
    GuildIdResult = validation:validate_optional_snowflake(GuildIdRaw),
    ChannelIdResult = validation:validate_optional_snowflake(ChannelIdRaw),
    Ctx = #{
        params => Params,
        session_id => SessionId,
        user_id => UserId,
        e2ee_capable => E2EECapable,
        bot => Bot,
        guilds => Guilds
    },
    dispatch_validated(GuildIdResult, ChannelIdResult, Ctx, State).

-spec extract_voice_params(map()) -> map().
extract_voice_params(Data) ->
    #{
        guild_id_raw => maps:get(<<"guild_id">>, Data, null),
        channel_id_raw => maps:get(<<"channel_id">>, Data, null),
        connection_id => maps:get(<<"connection_id">>, Data, null),
        self_mute => maps:get(<<"self_mute">>, Data, false),
        self_deaf => maps:get(<<"self_deaf">>, Data, false),
        self_video => maps:get(<<"self_video">>, Data, false),
        self_stream => maps:get(<<"self_stream">>, Data, false),
        viewer_stream_keys => maps:get(<<"viewer_stream_keys">>, Data, undefined),
        is_mobile => maps:get(<<"is_mobile">>, Data, false),
        latitude => maps:get(<<"latitude">>, Data, null),
        longitude => maps:get(<<"longitude">>, Data, null)
    }.

-spec dispatch_validated(
    {ok, guild_id() | null} | {error, term(), term()},
    {ok, channel_id() | null} | {error, term(), term()},
    map(),
    session_state()
) -> voice_state_reply().
dispatch_validated({ok, GId}, {ok, ChId}, Ctx, State) ->
    handle_validated(GId, ChId, Ctx, State);
dispatch_validated({error, _, _} = Error, _, Ctx, State) ->
    #{user_id := UserId, params := Params} = Ctx,
    SId = maps:get(id, State),
    GuildIdRaw = maps:get(guild_id_raw, Params, null),
    logger:warning(
        "voice_state_update_invalid_guild_id:"
        " user_id=~p session_id=~p"
        " guild_id_raw=~p error=~p",
        [UserId, SId, GuildIdRaw, Error]
    ),
    {reply, Error, State};
dispatch_validated(_, {error, _, _} = Error, Ctx, State) ->
    #{user_id := UserId, params := Params} = Ctx,
    SId = maps:get(id, State),
    ChIdRaw = maps:get(channel_id_raw, Params, null),
    logger:warning(
        "voice_state_update_invalid_channel_id:"
        " user_id=~p session_id=~p"
        " channel_id_raw=~p error=~p",
        [UserId, SId, ChIdRaw, Error]
    ),
    {reply, Error, State}.

-spec handle_validated(
    guild_id() | null,
    channel_id() | null,
    map(),
    session_state()
) -> voice_state_reply().
handle_validated(null, null, #{params := #{connection_id := null}}, State) ->
    log_disconnect_all(State),
    session_voice:handle_voice_disconnect(State);
handle_validated(null, null, Ctx, State) ->
    handle_null_null(Ctx, State);
handle_validated(null, ChId, Ctx, State) when is_integer(ChId) ->
    handle_dm_channel(ChId, Ctx, State);
handle_validated(GId, ChId, Ctx, State) when is_integer(GId) ->
    handle_guild_voice(GId, ChId, Ctx, State);
handle_validated(_, _, Ctx, State) ->
    #{user_id := UserId} = Ctx,
    SId = maps:get(id, State),
    invalid_params_reply(UserId, SId, State).

-spec log_disconnect_all(session_state()) -> ok.
log_disconnect_all(State) ->
    UserId = maps:get(user_id, State),
    SId = maps:get(id, State),
    logger:info(
        "voice_state_update_disconnect_all:"
        " user_id=~p session_id=~p",
        [UserId, SId]
    ).

-spec handle_null_null(map(), session_state()) -> voice_state_reply().
handle_null_null(Ctx, State) ->
    #{session_id := SId, user_id := UserId, params := Params} = Ctx,
    ConnId = maps:get(connection_id, Params),
    case is_binary(ConnId) of
        true ->
            handle_dm_disconnect(ConnId, SId, UserId, State);
        false ->
            invalid_params_reply(UserId, SId, State)
    end.

-spec handle_dm_channel(
    channel_id(), map(), session_state()
) -> voice_state_reply().
handle_dm_channel(ChId, Ctx, State) ->
    #{
        session_id := SId,
        user_id := UserId,
        params := Params,
        e2ee_capable := E2EE,
        bot := Bot
    } = Ctx,
    ConnId = maps:get(connection_id, Params),
    case is_binary(ConnId) orelse ConnId =:= null of
        true ->
            handle_dm_connect(
                ChId, Params, SId, UserId, E2EE, Bot, State
            );
        false ->
            invalid_params_reply(UserId, SId, State)
    end.

-spec log_voice_received(
    user_id(),
    binary(),
    term(),
    term(),
    term(),
    boolean(),
    boolean()
) -> ok.
log_voice_received(
    UserId,
    SessionId,
    GuildIdRaw,
    ChannelIdRaw,
    ConnId,
    E2EECapable,
    Bot
) ->
    BaseFields = [UserId, SessionId, GuildIdRaw, ChannelIdRaw],
    logger:info(
        "voice_state_update_received:"
        " user_id=~p session_id=~p"
        " guild_id_raw=~p channel_id_raw=~p"
        " connection_id=~p e2ee_capable=~p bot=~p",
        BaseFields ++ [ConnId, E2EECapable, Bot]
    ).

-spec log_invalid_params(user_id(), binary()) -> ok.
log_invalid_params(UserId, SessionId) ->
    logger:warning(
        "voice_state_update_invalid_params:"
        " user_id=~p session_id=~p",
        [UserId, SessionId]
    ).

-spec invalid_params_reply(user_id(), binary(), session_state()) ->
    voice_state_reply().
invalid_params_reply(UserId, SId, State) ->
    log_invalid_params(UserId, SId),
    {reply, gateway_errors:error(validation_invalid_params), State}.

-spec handle_dm_disconnect(
    binary(), binary(), user_id(), session_state()
) -> voice_state_reply().
handle_dm_disconnect(ConnId, SId, UserId, State) ->
    Request = #{
        user_id => UserId,
        channel_id => null,
        session_id => SId,
        connection_id => ConnId,
        self_mute => false,
        self_deaf => false,
        self_video => false,
        self_stream => false,
        viewer_stream_keys => [],
        is_mobile => false,
        latitude => null,
        longitude => null
    },
    StWithPid = State#{session_pid => self()},
    case dm_voice:voice_state_update(Request, StWithPid) of
        {reply, #{success := true}, NewState} ->
            log_dm_disconnect_ok(UserId, SId, ConnId),
            {reply, ok, maps:remove(session_pid, NewState)};
        {reply, {error, Cat, Err}, _StWithPid} ->
            log_dm_disconnect_err(UserId, SId, ConnId, Cat, Err),
            {reply, {error, Cat, Err}, State}
    end.

-spec log_dm_disconnect_ok(user_id(), binary(), binary()) -> ok.
log_dm_disconnect_ok(UserId, SId, ConnId) ->
    logger:info(
        "voice_state_update_dm_disconnect_ok:"
        " user_id=~p session_id=~p"
        " connection_id=~p",
        [UserId, SId, ConnId]
    ).

-spec log_dm_disconnect_err(
    user_id(), binary(), binary(), term(), term()
) -> ok.
log_dm_disconnect_err(UserId, SId, ConnId, Cat, Err) ->
    BaseFields = [UserId, SId, ConnId],
    logger:warning(
        "voice_state_update_dm_disconnect_failed:"
        " user_id=~p session_id=~p"
        " connection_id=~p category=~p error=~p",
        BaseFields ++ [Cat, Err]
    ).

-spec handle_dm_connect(
    channel_id(),
    map(),
    binary(),
    user_id(),
    boolean(),
    boolean(),
    session_state()
) -> voice_state_reply().
handle_dm_connect(ChId, Params, SId, UserId, E2EE, Bot, State) ->
    ConnId = maps:get(connection_id, Params),
    Request = #{
        user_id => UserId,
        channel_id => ChId,
        session_id => SId,
        connection_id => ConnId,
        self_mute => maps:get(self_mute, Params),
        self_deaf => maps:get(self_deaf, Params),
        self_video => maps:get(self_video, Params),
        self_stream => maps:get(self_stream, Params),
        viewer_stream_keys => maps:get(viewer_stream_keys, Params),
        is_mobile => maps:get(is_mobile, Params),
        latitude => maps:get(latitude, Params),
        longitude => maps:get(longitude, Params),
        e2ee_capable => E2EE,
        bot => Bot
    },
    StWithPid = State#{session_pid => self()},
    Result = dm_voice:voice_state_update(Request, StWithPid),
    handle_dm_connect_result(
        Result, ChId, Params, SId, UserId, State
    ).

-spec handle_dm_connect_result(
    term(),
    channel_id(),
    map(),
    binary(),
    user_id(),
    session_state()
) -> voice_state_reply().
handle_dm_connect_result(
    {reply, #{success := true, needs_token := true}, NewState},
    ChId,
    Params,
    SId,
    UserId,
    State
) when is_map(NewState) ->
    log_dm_info("dm_needs_token", UserId, SId, ChId, Params),
    spawn_voice_token_fetch(ChId, UserId, SId, Params),
    {reply, ok, merge_dm_voice_state(NewState, State)};
handle_dm_connect_result(
    {reply, #{success := true}, NewState},
    ChId,
    Params,
    SId,
    UserId,
    State
) when is_map(NewState) ->
    log_dm_info("dm_ok", UserId, SId, ChId, Params),
    {reply, ok, merge_dm_voice_state(NewState, State)};
handle_dm_connect_result(
    {reply, {error, Cat, Err}, _StWithPid},
    ChId,
    Params,
    SId,
    UserId,
    State
) ->
    ConnId = maps:get(connection_id, Params),
    logger:warning(
        dm_ch_fmt("dm_failed") ++
            " category=~p error=~p",
        [UserId, SId, ChId, ConnId, Cat, Err]
    ),
    {reply, {error, Cat, Err}, State}.

-spec dm_ch_fmt(string()) -> string().
dm_ch_fmt(Tag) ->
    "voice_state_update_" ++ Tag ++
        ":"
        " user_id=~p session_id=~p"
        " channel_id=~p connection_id=~p".

-spec log_dm_info(
    string(), user_id(), binary(), channel_id(), map()
) -> ok.
log_dm_info(Tag, UserId, SId, ChId, Params) ->
    ConnId = maps:get(connection_id, Params),
    logger:info(dm_ch_fmt(Tag), [UserId, SId, ChId, ConnId]).

-spec spawn_voice_token_fetch(
    channel_id(), user_id(), binary(), map()
) -> pid().
spawn_voice_token_fetch(ChId, UserId, SId, Params) ->
    Lat = maps:get(latitude, Params),
    Lon = maps:get(longitude, Params),
    SessionPid = self(),
    spawn(fun() ->
        dm_voice:get_voice_token(
            ChId, UserId, SId, SessionPid, Lat, Lon
        )
    end).

-spec merge_dm_voice_state(#{term() => term()}, session_state()) -> session_state().
merge_dm_voice_state(DmState, OrigState) when is_map(DmState) ->
    maps:fold(
        fun
            (session_pid, _V, Acc) -> Acc;
            (K, V, Acc) when is_atom(K) -> Acc#{K => V};
            (_, _, Acc) -> Acc
        end,
        OrigState,
        DmState
    ).

-spec handle_guild_voice(
    guild_id(), channel_id() | null, map(), session_state()
) -> voice_state_reply().
handle_guild_voice(GId, ChId, Ctx, State) ->
    #{guilds := Guilds} = Ctx,
    case maps:get(GId, Guilds, undefined) of
        undefined ->
            guild_voice_missing(GId, ChId, Ctx, State);
        {GuildPid, _Ref} when is_pid(GuildPid) ->
            guild_voice_queue(GuildPid, GId, ChId, Ctx, State);
        _ ->
            guild_voice_invalid(GId, Ctx, State)
    end.

-spec guild_voice_missing(
    guild_id(), channel_id() | null, map(), session_state()
) -> voice_state_reply().
guild_voice_missing(GId, ChId, Ctx, State) ->
    #{session_id := SId, user_id := UserId, params := Params} = Ctx,
    ConnId = maps:get(connection_id, Params),
    log_guild_warning("guild_missing", UserId, SId, GId, ChId, ConnId),
    {reply, gateway_errors:error(voice_guild_not_found), State}.

-spec guild_voice_queue(
    pid(),
    guild_id(),
    channel_id() | null,
    map(),
    session_state()
) -> voice_state_reply().
guild_voice_queue(GuildPid, GId, ChId, Ctx, State) ->
    #{
        session_id := SId,
        user_id := UserId,
        params := Params,
        e2ee_capable := E2EE,
        bot := Bot
    } = Ctx,
    ConnId = maps:get(connection_id, Params),
    log_guild_info("guild_queue", UserId, SId, GId, ChId, ConnId),
    Req = build_guild_request(ChId, Params, UserId, SId, E2EE, Bot),
    VoiceCtx = #{
        guild_pid => GuildPid,
        guild_id => GId,
        channel_id => ChId,
        user_id => UserId,
        conn_id => ConnId,
        session_id => SId,
        request => Req,
        lat => maps:get(latitude, Params),
        lon => maps:get(longitude, Params)
    },
    session_voice_dispatch:queue_guild_voice_state_update(
        VoiceCtx, State
    ).

-spec guild_voice_invalid(
    guild_id(), map(), session_state()
) -> voice_state_reply().
guild_voice_invalid(GId, Ctx, State) ->
    #{session_id := SId, user_id := UserId} = Ctx,
    logger:warning(
        "voice_state_update_guild_invalid_pid:"
        " user_id=~p session_id=~p guild_id=~p",
        [UserId, SId, GId]
    ),
    {reply, gateway_errors:error(internal_error), State}.

-spec guild_voice_fmt(string()) -> string().
guild_voice_fmt(Tag) ->
    "voice_state_update_" ++ Tag ++
        ":"
        " user_id=~p session_id=~p"
        " guild_id=~p channel_id=~p"
        " connection_id=~p".

-spec guild_voice_fields(
    user_id(),
    binary(),
    guild_id(),
    channel_id() | null,
    term()
) -> [term()].
guild_voice_fields(UserId, SId, GId, ChId, ConnId) ->
    [UserId, SId, GId, ChId, ConnId].

-spec log_guild_info(
    string(),
    user_id(),
    binary(),
    guild_id(),
    channel_id() | null,
    term()
) -> ok.
log_guild_info(Tag, UserId, SId, GId, ChId, ConnId) ->
    Fmt = guild_voice_fmt(Tag),
    Fields = guild_voice_fields(UserId, SId, GId, ChId, ConnId),
    logger:info(Fmt, Fields).

-spec log_guild_warning(
    string(),
    user_id(),
    binary(),
    guild_id(),
    channel_id() | null,
    term()
) -> ok.
log_guild_warning(Tag, UserId, SId, GId, ChId, ConnId) ->
    Fmt = guild_voice_fmt(Tag),
    Fields = guild_voice_fields(UserId, SId, GId, ChId, ConnId),
    logger:warning(Fmt, Fields).

-spec build_guild_request(
    channel_id() | null,
    map(),
    user_id(),
    binary(),
    boolean(),
    boolean()
) -> map().
build_guild_request(ChId, Params, UserId, SId, E2EE, Bot) ->
    #{
        user_id => UserId,
        session_id => SId,
        channel_id => ChId,
        connection_id => maps:get(connection_id, Params),
        self_mute => maps:get(self_mute, Params),
        self_deaf => maps:get(self_deaf, Params),
        self_video => maps:get(self_video, Params),
        self_stream => maps:get(self_stream, Params),
        viewer_stream_keys => maps:get(viewer_stream_keys, Params),
        is_mobile => maps:get(is_mobile, Params),
        latitude => maps:get(latitude, Params),
        longitude => maps:get(longitude, Params),
        e2ee_capable => E2EE,
        bot => Bot
    }.
