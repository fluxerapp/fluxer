%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_voice).
-typing([eqwalizer]).

-export([
    init_voice_queue/0,
    process_voice_queue/1,
    handle_voice_state_update/2,
    report_rejection/2,
    handle_voice_signal/2,
    handle_voice_disconnect/1
]).

-export_type([session_state/0, voice_state_reply/0]).

-type session_state() :: session:session_state().

-type voice_state_reply() ::
    {reply, ok, session_state()}
    | {reply, {error, term(), term()}, session_state()}.

-spec init_voice_queue() -> #{voice_queue := queue:queue(), voice_queue_timer := undefined}.
init_voice_queue() ->
    #{voice_queue => queue:new(), voice_queue_timer => undefined}.

-spec process_voice_queue(session_state()) -> session_state().
process_voice_queue(State) ->
    VoiceQueue = maps:get(voice_queue, State, queue:new()),
    case queue:out(VoiceQueue) of
        {empty, _} ->
            State;
        {{value, Item}, NewQueue} ->
            process_voice_queue_item(Item, State#{voice_queue => NewQueue})
    end.

-spec process_voice_queue_item(map(), session_state()) -> session_state().
process_voice_queue_item(Item, State) ->
    case maps:get(type, Item, undefined) of
        voice_state_update ->
            Data = maps:get(data, Item),
            {reply, _, NewState} = handle_voice_state_update(Data, State),
            NewState;
        _ ->
            State
    end.

-spec handle_voice_state_update(map(), session_state()) -> voice_state_reply().
handle_voice_state_update(Data, State) ->
    Result = session_voice_connect:handle_voice_state_update(Data, State),
    ok = report_rejected_reply(Result, State),
    Result.

-spec report_rejected_reply(voice_state_reply(), session_state()) -> ok.
report_rejected_reply({reply, {error, _Category, ErrorAtom}, _NewState}, State) ->
    report_rejection(ErrorAtom, State);
report_rejected_reply(_Result, _State) ->
    ok.

-spec report_rejection(term(), session_state()) -> ok.
report_rejection(ErrorAtom, State) ->
    case {voice_p2p:reports_rejection(ErrorAtom), maps:get(socket_pid, State, undefined)} of
        {true, SocketPid} when is_pid(SocketPid) ->
            SocketPid ! {gateway_error, ErrorAtom},
            ok;
        _ ->
            ok
    end.

-spec handle_voice_signal(map(), session_state()) -> ok.
handle_voice_signal(Data, State) ->
    GuildIdResult = validation:validate_optional_snowflake(
        maps:get(<<"guild_id">>, Data, null)
    ),
    ChannelIdResult = validation:validate_snowflake(maps:get(<<"channel_id">>, Data, null)),
    To = maps:get(<<"to">>, Data, undefined),
    Signal = maps:get(<<"data">>, Data, undefined),
    case {GuildIdResult, ChannelIdResult} of
        {{ok, GuildId}, {ok, ChannelId}} when is_binary(To), is_map(Signal) ->
            route_voice_signal(GuildId, ChannelId, To, Signal, State);
        _ ->
            ok
    end.

-spec route_voice_signal(integer() | null, integer(), binary(), map(), session_state()) -> ok.
route_voice_signal(null, ChannelId, To, Signal, State) ->
    case maps:get(ChannelId, maps:get(calls, State, #{}), undefined) of
        {CallPid, _Ref} when is_pid(CallPid) ->
            gen_server:cast(CallPid, {voice_signal, maps:get(id, State), To, Signal});
        _ ->
            ok
    end;
route_voice_signal(GuildId, ChannelId, To, Signal, State) ->
    case maps:get(GuildId, maps:get(guilds, State, #{}), undefined) of
        {GuildPid, _Ref} when is_pid(GuildPid) ->
            gen_server:cast(
                GuildPid,
                {voice_signal, #{
                    session_id => maps:get(id, State),
                    channel_id => ChannelId,
                    to => To,
                    data => Signal
                }}
            );
        _ ->
            ok
    end.

-spec handle_voice_disconnect(session_state()) -> voice_state_reply().
handle_voice_disconnect(State) ->
    Guilds = maps:get(guilds, State),
    UserId = maps:get(user_id, State),
    SessionId = maps:get(id, State),
    ConnectionId = maps:get(connection_id, State, null),
    logger:info(
        "voice_disconnect_start: user_id=~p session_id=~p connection_id=~p guild_count=~p",
        [UserId, SessionId, ConnectionId, maps:size(Guilds)]
    ),
    Request = #{
        user_id => UserId,
        channel_id => null,
        session_id => SessionId,
        connection_id => ConnectionId,
        self_mute => false,
        self_deaf => false,
        self_video => false,
        self_stream => false,
        viewer_stream_keys => []
    },
    session_voice_dispatch:dispatch_guild_voice_disconnects(Guilds, Request),
    {reply, #{success := true}, NewState} =
        dm_voice:disconnect_voice_user(UserId, State),
    logger:info(
        "voice_disconnect_ok: user_id=~p session_id=~p",
        [UserId, SessionId]
    ),
    {reply, ok, NewState}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

init_voice_queue_test() ->
    Result = init_voice_queue(),
    ?assert(maps:is_key(voice_queue, Result)),
    ?assert(maps:is_key(voice_queue_timer, Result)),
    ?assertEqual(undefined, maps:get(voice_queue_timer, Result)),
    ?assert(queue:is_empty(maps:get(voice_queue, Result))),
    ok.

report_rejection_sends_p2p_rejections_to_the_socket_test() ->
    State = #{socket_pid => self()},
    ok = report_rejection(voice_p2p_consent_required, State),
    ok = report_rejection(voice_p2p_unavailable, State),
    ok = report_rejection(voice_channel_full, State),
    ok = report_rejection(voice_token_failed, State),
    ok = report_rejection(voice_channel_full, #{socket_pid => undefined}),
    ?assertEqual(
        [voice_p2p_consent_required, voice_p2p_unavailable, voice_channel_full],
        drain_gateway_errors()
    ).

drain_gateway_errors() ->
    receive
        {gateway_error, ErrorAtom} -> [ErrorAtom | drain_gateway_errors()]
    after 50 ->
        []
    end.

voice_signal_routes_to_the_guild_test() ->
    State = #{id => <<"sess">>, guilds => #{42 => {self(), make_ref()}}, calls => #{}},
    Data = #{
        <<"guild_id">> => <<"42">>,
        <<"channel_id">> => <<"100">>,
        <<"to">> => <<"conn-b">>,
        <<"data">> => #{<<"type">> => <<"offer">>}
    },
    ok = handle_voice_signal(Data, State),
    receive
        {'$gen_cast', {voice_signal, Request}} ->
            ?assertEqual(
                #{
                    session_id => <<"sess">>,
                    channel_id => 100,
                    to => <<"conn-b">>,
                    data => #{<<"type">> => <<"offer">>}
                },
                Request
            )
    after 1000 ->
        error(voice_signal_not_routed)
    end.

voice_signal_routes_to_the_call_test() ->
    State = #{id => <<"sess">>, guilds => #{}, calls => #{100 => {self(), make_ref()}}},
    Data = #{<<"channel_id">> => <<"100">>, <<"to">> => <<"conn-b">>, <<"data">> => #{}},
    ok = handle_voice_signal(Data, State),
    receive
        {'$gen_cast', {voice_signal, <<"sess">>, <<"conn-b">>, #{}}} -> ok
    after 1000 ->
        error(voice_signal_not_routed)
    end.

voice_signal_drops_unroutable_frames_test() ->
    State = #{id => <<"sess">>, guilds => #{42 => {self(), make_ref()}}, calls => #{}},
    Valid = #{
        <<"guild_id">> => <<"42">>,
        <<"channel_id">> => <<"100">>,
        <<"to">> => <<"conn-b">>,
        <<"data">> => #{}
    },
    Dropped = [
        Valid#{<<"guild_id">> => <<"43">>},
        Valid#{<<"guild_id">> => null},
        Valid#{<<"channel_id">> => null},
        Valid#{<<"to">> => 5},
        Valid#{<<"data">> => <<"sdp">>},
        #{}
    ],
    [?assertEqual(ok, handle_voice_signal(Data, State)) || Data <- Dropped],
    receive
        {'$gen_cast', Unexpected} -> error({unexpected_voice_signal, Unexpected})
    after 50 ->
        ok
    end.

process_voice_queue_empty_test() ->
    State = #{voice_queue => queue:new()},
    Result = process_voice_queue(State),
    ?assertEqual(State, Result),
    ok.

-endif.
