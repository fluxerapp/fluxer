%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_voice_dispatch).
-typing([eqwalizer]).

-export([
    queue_guild_voice_state_update/2,
    dispatch_to_session/4,
    dispatch_guild_voice_disconnects/2
]).

-export_type([
    session_state/0,
    guild_id/0,
    channel_id/0,
    user_id/0,
    voice_state_reply/0,
    voice_ctx/0
]).

-type session_state() :: session:session_state().
-type guild_id() :: session:guild_id().
-type channel_id() :: session:channel_id().
-type user_id() :: session:user_id().

-type voice_ctx() :: #{
    guild_pid := pid(),
    guild_id := guild_id(),
    channel_id := channel_id() | null,
    user_id := user_id(),
    conn_id := binary() | null,
    session_id := binary(),
    request := map(),
    lat := number() | null,
    lon := number() | null
}.

-type voice_state_reply() ::
    {reply, ok, session_state()}
    | {reply, {error, term(), term()}, session_state()}.

-spec queue_guild_voice_state_update(voice_ctx(), session_state()) ->
    voice_state_reply().
queue_guild_voice_state_update(Ctx, State) ->
    log_voice_info("voice_state_update_guild_spawn", Ctx),
    SessionPid = self(),
    spawn(fun() -> run_guild_voice_state_update(Ctx, SessionPid) end),
    {reply, ok, State}.

-spec run_guild_voice_state_update(voice_ctx(), pid()) -> ok.
run_guild_voice_state_update(Ctx, SessionPid) ->
    try
        handle_guild_voice_state_update(Ctx, SessionPid)
    catch
        Class:Reason:Stacktrace ->
            log_voice_warning_extra(
                Ctx,
                " crash_class=~p crash_reason=~p stacktrace=~p",
                [Class, Reason, Stacktrace]
            )
    end.

-spec handle_guild_voice_state_update(voice_ctx(), pid()) -> ok.
handle_guild_voice_state_update(Ctx, SessionPid) ->
    #{guild_pid := GuildPid, guild_id := GuildId, request := Request} = Ctx,
    log_voice_info("voice_state_update_guild_call_start", Ctx),
    Result = guild_client:voice_state_update(
        GuildPid, GuildId, Request, 12000
    ),
    handle_guild_call_result(Result, Ctx, SessionPid).

-spec handle_guild_call_result(term(), voice_ctx(), pid()) -> ok.
handle_guild_call_result({ok, Reply}, Ctx, SessionPid) when is_map(Reply) ->
    handle_guild_reply_ok(Reply, Ctx, SessionPid);
handle_guild_call_result({error, timeout}, Ctx, _SessionPid) ->
    log_voice_warning("voice_state_update_guild_call_timeout", Ctx);
handle_guild_call_result({error, noproc}, Ctx, _SessionPid) ->
    log_voice_warning("voice_state_update_guild_call_noproc", Ctx);
handle_guild_call_result({error, Cat, Err}, Ctx, _SessionPid) ->
    log_voice_warning_extra(Ctx, " category=~p error=~p", [Cat, Err]);
handle_guild_call_result({error, Reason}, Ctx, _SessionPid) ->
    log_voice_warning_extra(Ctx, " reason=~p", [Reason]).

-spec handle_guild_reply_ok(map(), voice_ctx(), pid()) -> ok.
handle_guild_reply_ok(Reply, Ctx, SessionPid) ->
    #{guild_id := GId, channel_id := ChId, conn_id := ConnId} = Ctx,
    logger:info(
        "voice_state_update_guild_call_ok:"
        " user_id=~p session_id=~p guild_id=~p"
        " channel_id=~p connection_id=~p"
        " has_token=~p has_endpoint=~p",
        [
            maps:get(user_id, Ctx),
            maps:get(session_id, Ctx),
            GId,
            ChId,
            ConnId,
            maps:is_key(token, Reply),
            maps:is_key(endpoint, Reply)
        ]
    ),
    maybe_dispatch_voice_server_update(
        Reply, GId, ChId, SessionPid
    ),
    case maps:get(ack, Reply, undefined) of
        Ack when is_map(Ack) ->
            dispatch_to_session(
                SessionPid, voice_state_ack, Ack, GId
            );
        _ ->
            ok
    end,
    ok.

-spec maybe_dispatch_voice_server_update(
    map(), guild_id(), channel_id() | null, pid()
) -> ok.
maybe_dispatch_voice_server_update(Reply, GId, ChId, SessionPid) ->
    Token = maps:get(token, Reply, undefined),
    Endpoint = maps:get(endpoint, Reply, undefined),
    ConnId = maps:get(connection_id, Reply, undefined),
    case {Token, Endpoint} of
        {T, E} when is_binary(T), is_binary(E), is_binary(ConnId) ->
            dispatch_voice_server_with_channel(
                T, E, ConnId, GId, resolve_dispatch_channel_id(Reply, ChId), Reply, SessionPid
            );
        {undefined, undefined} ->
            ok;
        _ ->
            logger:warning(
                "voice_server_update_missing_fields:"
                " guild_id=~p channel_id=~p reply=~p",
                [GId, ChId, Reply]
            ),
            ok
    end.

-spec resolve_dispatch_channel_id(map(), channel_id() | null) -> integer() | undefined.
resolve_dispatch_channel_id(_Reply, ChId) when is_integer(ChId) ->
    ChId;
resolve_dispatch_channel_id(Reply, _ChId) ->
    case maps:get(voice_state, Reply, undefined) of
        VoiceState when is_map(VoiceState) ->
            voice_state_utils:voice_state_channel_id(VoiceState);
        _ ->
            undefined
    end.

-spec dispatch_voice_server_with_channel(
    binary(), binary(), binary(), guild_id(), integer() | undefined, map(), pid()
) -> ok.
dispatch_voice_server_with_channel(Token, Endpoint, ConnId, GId, ChId, Reply, SessionPid) when
    is_integer(ChId), is_pid(SessionPid)
->
    build_and_dispatch_voice_server(
        Token, Endpoint, ConnId, GId, ChId, Reply, SessionPid
    );
dispatch_voice_server_with_channel(_Token, _Endpoint, ConnId, GId, ChId, _Reply, _SessionPid) ->
    logger:warning(
        "voice_server_update_unresolved_channel:"
        " guild_id=~p channel_id=~p connection_id=~p",
        [GId, ChId, ConnId]
    ),
    ok.

-spec build_and_dispatch_voice_server(
    binary(),
    binary(),
    binary(),
    guild_id(),
    channel_id(),
    map(),
    pid()
) -> ok.
build_and_dispatch_voice_server(
    Token, Endpoint, ConnId, GId, ChId, Reply, SessionPid
) ->
    VoiceServerUpdate0 = #{
        <<"token">> => Token,
        <<"endpoint">> => Endpoint,
        <<"guild_id">> => integer_to_binary(GId),
        <<"channel_id">> => integer_to_binary(ChId),
        <<"connection_id">> => ConnId
    },
    VoiceServerUpdate =
        case maps:get(e2ee_key, Reply, undefined) of
            Key when is_binary(Key) ->
                VoiceServerUpdate0#{<<"e2ee_key">> => Key};
            _ ->
                VoiceServerUpdate0
        end,
    HasE2EE = maps:is_key(<<"e2ee_key">>, VoiceServerUpdate),
    logger:info(
        "voice_server_update_dispatch:"
        " guild_id=~p channel_id=~p"
        " connection_id=~p has_e2ee_key=~p",
        [GId, ChId, ConnId, HasE2EE]
    ),
    dispatch_to_session(
        SessionPid,
        voice_server_update,
        VoiceServerUpdate,
        GId
    ),
    ok.

-spec dispatch_to_session(pid(), atom(), term(), integer()) -> ok.
dispatch_to_session(SessionPid, Event, Payload, GuildId) ->
    WirePayload = guild_data_wire:payload(Payload),
    case code:ensure_loaded(gateway_dispatch_relay) of
        {module, gateway_dispatch_relay} ->
            try_relay_dispatch(
                SessionPid, Event, WirePayload, GuildId
            );
        _ ->
            gen_server:cast(
                SessionPid, {dispatch, Event, WirePayload}
            )
    end.

-spec try_relay_dispatch(pid(), atom(), term(), integer()) -> ok.
try_relay_dispatch(SessionPid, Event, WirePayload, GuildId) ->
    try
        gateway_dispatch_relay:dispatch(
            SessionPid, Event, WirePayload, GuildId
        )
    of
        ok -> ok
    catch
        error:undef ->
            gen_server:cast(
                SessionPid, {dispatch, Event, WirePayload}
            )
    end.

-spec dispatch_guild_voice_disconnects(map(), map()) -> ok.
dispatch_guild_voice_disconnects(Guilds, Request) ->
    GuildCount = maps:size(Guilds),
    logger:info(
        "voice_disconnect_guild_broadcast_start:"
        " guild_count=~p",
        [GuildCount]
    ),
    maps:foreach(
        fun
            (GuildId, {GuildPid, _Ref}) when is_pid(GuildPid) ->
                spawn_voice_disconnect(GuildPid, GuildId, Request),
                ok;
            (_, _) ->
                ok
        end,
        Guilds
    ).

-spec spawn_voice_disconnect(pid(), guild_id(), map()) -> pid().
spawn_voice_disconnect(GuildPid, GuildId, Request) ->
    spawn(fun() ->
        Result = guild_client:voice_state_update(
            GuildPid, GuildId, Request, 10000
        ),
        logger:info(
            "voice_disconnect_guild_broadcast_result:"
            " guild_id=~p result=~p",
            [GuildId, Result]
        )
    end).

-spec voice_ctx_fields(voice_ctx()) -> [term()].
voice_ctx_fields(Ctx) ->
    #{
        user_id := UserId,
        session_id := SId,
        guild_id := GId,
        channel_id := ChId,
        conn_id := ConnId
    } = Ctx,
    [UserId, SId, GId, ChId, ConnId].

-spec voice_log_fmt(string()) -> string().
voice_log_fmt(Tag) ->
    Tag ++
        ":"
        " user_id=~p session_id=~p guild_id=~p"
        " channel_id=~p connection_id=~p".

-spec log_voice_info(string(), voice_ctx()) -> ok.
log_voice_info(Tag, Ctx) ->
    logger:info(voice_log_fmt(Tag), voice_ctx_fields(Ctx)).

-spec log_voice_warning(string(), voice_ctx()) -> ok.
log_voice_warning(Tag, Ctx) ->
    logger:warning(voice_log_fmt(Tag), voice_ctx_fields(Ctx)).

-spec log_voice_warning_extra(voice_ctx(), string(), [term()]) -> ok.
log_voice_warning_extra(Ctx, ExtraFmt, ExtraArgs) ->
    Fmt = voice_log_fmt("voice_state_update_guild_call_error") ++ ExtraFmt,
    logger:warning(Fmt, voice_ctx_fields(Ctx) ++ ExtraArgs).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

dispatch_to_session_converts_wire_payload_test() ->
    Payload = #{
        <<"id">> => 123,
        <<"permissions">> => 8,
        <<"roles">> => [456]
    },
    ok = dispatch_to_session(self(), voice_state_ack, Payload, 123),
    receive
        {'$gen_cast', {dispatch, voice_state_ack, WirePayload}} ->
            ?assertEqual(
                #{
                    <<"id">> => <<"123">>,
                    <<"permissions">> => <<"8">>,
                    <<"roles">> => [<<"456">>]
                },
                WirePayload
            )
    after 1000 ->
        ?assert(false, dispatch_not_received)
    end.

test_voice_ctx(ChId) ->
    #{
        guild_pid => self(),
        guild_id => 42,
        channel_id => ChId,
        user_id => 7,
        conn_id => <<"old-conn">>,
        session_id => <<"sess">>,
        request => #{},
        lat => null,
        lon => null
    }.

assert_no_dispatch() ->
    receive
        {'$gen_cast', {dispatch, Event, Payload}} ->
            ?assert(false, {unexpected_dispatch, Event, Payload})
    after 50 ->
        ok
    end.

receive_dispatch(ExpectedEvent) ->
    receive
        {'$gen_cast', {dispatch, ExpectedEvent, Payload}} ->
            Payload
    after 1000 ->
        ?assert(false, {dispatch_not_received, ExpectedEvent}),
        #{}
    end.

fresh_session_pid() ->
    Parent = self(),
    spawn(fun() ->
        receive
            {'$gen_cast', {dispatch, Event, Payload}} ->
                Parent ! {unexpected_session_dispatch, Event, Payload}
        after 200 ->
            ok
        end
    end).

assert_session_no_dispatch() ->
    receive
        {unexpected_session_dispatch, E, P} ->
            ?assert(false, {unexpected_dispatch, E, P})
    after 300 ->
        ok
    end.

in_channel_update_reply_does_not_dispatch_test() ->
    SessionPid = fresh_session_pid(),
    Reply = #{
        success => true,
        voice_state => #{<<"channel_id">> => <<"456">>, <<"self_deaf">> => true}
    },
    ok = handle_guild_reply_ok(Reply, test_voice_ctx(456), SessionPid),
    assert_session_no_dispatch().

plain_success_reply_does_not_dispatch_test() ->
    SessionPid = fresh_session_pid(),
    ok = handle_guild_reply_ok(#{success => true}, test_voice_ctx(456), SessionPid),
    assert_session_no_dispatch().

rejected_mutation_reply_dispatches_ack_only_test() ->
    Ack = #{<<"status">> => <<"rejected">>, <<"mutation_id">> => <<"m1">>},
    Reply = #{success => false, ack => Ack},
    ok = handle_guild_reply_ok(Reply, test_voice_ctx(456), self()),
    Payload = receive_dispatch(voice_state_ack),
    ?assertEqual(<<"rejected">>, maps:get(<<"status">>, Payload)),
    assert_no_dispatch().

in_channel_update_reply_dispatches_ack_test() ->
    Ack = #{<<"status">> => <<"applied">>, <<"mutation_id">> => <<"m2">>},
    Reply = #{
        success => true,
        voice_state => #{<<"channel_id">> => <<"456">>},
        ack => Ack
    },
    ok = handle_guild_reply_ok(Reply, test_voice_ctx(456), self()),
    Payload = receive_dispatch(voice_state_ack),
    ?assertEqual(<<"applied">>, maps:get(<<"status">>, Payload)),
    assert_no_dispatch().

join_reply_dispatches_voice_server_update_test() ->
    Reply = #{
        success => true,
        token => <<"tok">>,
        endpoint => <<"wss://voice">>,
        connection_id => <<"conn-1">>,
        voice_state => #{<<"channel_id">> => <<"456">>}
    },
    ok = handle_guild_reply_ok(Reply, test_voice_ctx(456), self()),
    Payload = receive_dispatch(voice_server_update),
    ?assertEqual(<<"tok">>, maps:get(<<"token">>, Payload)),
    ?assertEqual(<<"wss://voice">>, maps:get(<<"endpoint">>, Payload)),
    ?assertEqual(<<"42">>, maps:get(<<"guild_id">>, Payload)),
    ?assertEqual(<<"456">>, maps:get(<<"channel_id">>, Payload)),
    ?assertEqual(<<"conn-1">>, maps:get(<<"connection_id">>, Payload)),
    ?assertNot(maps:is_key(<<"e2ee_key">>, Payload)).

join_reply_attaches_e2ee_key_test() ->
    Reply = #{
        success => true,
        token => <<"tok">>,
        endpoint => <<"wss://voice">>,
        connection_id => <<"conn-1">>,
        e2ee_key => <<"secret">>
    },
    ok = handle_guild_reply_ok(Reply, test_voice_ctx(456), self()),
    Payload = receive_dispatch(voice_server_update),
    ?assertEqual(<<"secret">>, maps:get(<<"e2ee_key">>, Payload)).

null_channel_falls_back_to_reply_voice_state_test() ->
    Reply = #{
        success => true,
        token => <<"tok">>,
        endpoint => <<"wss://voice">>,
        connection_id => <<"conn-2">>,
        voice_state => #{<<"channel_id">> => <<"789">>}
    },
    ok = handle_guild_reply_ok(Reply, test_voice_ctx(null), self()),
    Payload = receive_dispatch(voice_server_update),
    ?assertEqual(<<"789">>, maps:get(<<"channel_id">>, Payload)).

token_reply_without_resolvable_channel_does_not_dispatch_test() ->
    SessionPid = fresh_session_pid(),
    Reply = #{
        success => true,
        token => <<"tok">>,
        endpoint => <<"wss://voice">>,
        connection_id => <<"conn-3">>
    },
    ok = handle_guild_reply_ok(Reply, test_voice_ctx(null), SessionPid),
    assert_session_no_dispatch().

partial_token_reply_does_not_dispatch_test() ->
    SessionPid = fresh_session_pid(),
    Reply = #{success => true, token => <<"tok">>},
    ok = handle_guild_reply_ok(Reply, test_voice_ctx(456), SessionPid),
    assert_session_no_dispatch().

dm_connect_reply_does_not_dispatch_test() ->
    SessionPid = fresh_session_pid(),
    Reply = #{success => true, needs_token => false, connection_id => <<"conn-4">>},
    ok = handle_guild_reply_ok(Reply, test_voice_ctx(456), SessionPid),
    assert_session_no_dispatch().

-endif.
