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

-module(gateway_handler).
-behaviour(cowboy_websocket).

-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(VOICE_UPDATE_RATE_LIMIT, 10).
-define(VOICE_RATE_LIMIT_WINDOW, 1000).
-define(VOICE_QUEUE_TABLE, voice_update_queue).
-define(VOICE_RATE_LIMIT_TABLE, voice_update_rate_limit).
-define(VOICE_QUEUE_PROCESS_INTERVAL, 100).
-define(MAX_VOICE_QUEUE_LENGTH, 64).
-define(RATE_LIMIT_WINDOW_MS, 60000).
-define(RATE_LIMIT_MAX_EVENTS, 120).
-define(REQUEST_GUILD_MEMBERS_RATE_LIMIT_WINDOW_MS, 10000).
-define(REQUEST_GUILD_MEMBERS_RATE_LIMIT_MAX_EVENTS, 3).

-type state() :: #{
    version := 1 | undefined,
    encoding := gateway_codec:encoding(),
    compress_ctx := gateway_compress:compress_ctx() | undefined,
    session_pid := pid() | undefined,
    heartbeat_state := map(),
    socket_pid := pid() | undefined,
    peer_ip := binary() | undefined,
    rate_limit_state := map(),
    request_guild_members_pid := pid() | undefined,
    otel_span_ctx := term(),
    voice_queue_timer := reference() | undefined
}.

-spec new_state() -> state().
new_state() ->
    #{
        version => undefined,
        encoding => json,
        compress_ctx => undefined,
        session_pid => undefined,
        heartbeat_state => #{},
        socket_pid => undefined,
        peer_ip => undefined,
        rate_limit_state => #{
            events => [],
            request_guild_members_events => [],
            window_start => undefined
        },
        request_guild_members_pid => undefined,
        otel_span_ctx => undefined,
        voice_queue_timer => undefined
    }.

-type ws_frame() :: {text, binary()} | {binary, binary()}.
-type ws_result() :: {ok, state()} | {[ws_frame() | {close, integer(), binary()}], state()}.

-spec init(cowboy_req:req(), term()) -> {cowboy_websocket, cowboy_req:req(), state()}.
init(Req, _Opts) ->
    QS = cowboy_req:parse_qs(Req),
    Version = parse_version(proplists:get_value(<<"v">>, QS)),
    Encoding = gateway_codec:parse_encoding(proplists:get_value(<<"encoding">>, QS)),
    Compression = gateway_compress:parse_compression(proplists:get_value(<<"compress">>, QS)),
    CompressCtx = gateway_compress:new_context(Compression),
    PeerIPBinary = extract_client_ip(Req),
    State = new_state(),
    {cowboy_websocket, Req, State#{
        version => Version,
        encoding => Encoding,
        compress_ctx => CompressCtx,
        socket_pid => self(),
        peer_ip => PeerIPBinary
    }}.

-spec parse_version(binary() | undefined) -> 1 | undefined.
parse_version(<<"1">>) -> 1;
parse_version(_) -> undefined.

-spec websocket_init(state()) -> ws_result().
websocket_init(State = #{version := 1}) ->
    gateway_metrics_collector:inc_connections(),
    ConnSpanCtx = start_websocket_connect_span(1, State),
    NewState = State#{otel_span_ctx => ConnSpanCtx},
    CompressionType = gateway_compress:get_type(maps:get(compress_ctx, NewState)),
    FreshCompressCtx = gateway_compress:new_context(CompressionType),
    FreshState0 = NewState#{compress_ctx => FreshCompressCtx},
    HeartbeatInterval = constants:heartbeat_interval(),
    HelloMessage = #{
        <<"op">> => constants:opcode_to_num(hello),
        <<"d">> => #{<<"heartbeat_interval">> => HeartbeatInterval}
    },
    schedule_heartbeat_check(),
    NewState1 = FreshState0#{
        heartbeat_state => #{
            last_ack => erlang:system_time(millisecond),
            waiting_for_ack => false
        }
    },
    case encode_and_compress(HelloMessage, NewState1) of
        {ok, Frame, NewState2} ->
            {[Frame], NewState2};
        {error, {compress_failed, CT, _Reason}} ->
            close_with_reason(decode_error, compression_error_reason(CT), FreshState0);
        {error, _Reason} ->
            close_with_reason(decode_error, <<"Encode failed">>, FreshState0)
    end;
websocket_init(State) ->
    gateway_metrics_collector:inc_connections(),
    ConnSpanCtx = start_websocket_connect_span(undefined, State),
    NewState = State#{otel_span_ctx => ConnSpanCtx},
    close_with_reason(invalid_api_version, <<"Invalid API version">>, NewState).

-spec websocket_handle({text, binary()} | {binary, binary()} | term(), state()) -> ws_result().
websocket_handle({text, Text}, State) ->
    handle_incoming_data(Text, State);
websocket_handle({binary, Binary}, State) ->
    handle_incoming_data(Binary, State);
websocket_handle(_, State) ->
    {ok, State}.

-spec handle_incoming_data(binary(), state()) -> ws_result().
handle_incoming_data(Data, State = #{encoding := Encoding, compress_ctx := CompressCtx}) ->
    MaxSize = constants:max_payload_size(),
    case byte_size(Data) =< MaxSize of
        true ->
            handle_decode(gateway_codec:decode(Data, Encoding), State#{
                compress_ctx => CompressCtx
            });
        false ->
            close_with_reason(decode_error, <<"Payload too large">>, State)
    end.

-spec handle_decode({ok, map()} | {error, term()}, state()) -> ws_result().
handle_decode({ok, #{<<"op">> := Op} = Payload}, State) ->
    OpAtom = constants:gateway_opcode(Op),
    case check_rate_limit(State, OpAtom) of
        {ok, RateLimitedState} ->
            handle_gateway_payload(OpAtom, Payload, RateLimitedState);
        rate_limited ->
            close_with_reason(rate_limited, <<"Rate limited">>, State)
    end;
handle_decode({ok, _}, State) ->
    close_with_reason(decode_error, <<"Invalid payload">>, State);
handle_decode({error, _Reason}, State) ->
    close_with_reason(decode_error, <<"Decode failed">>, State).

-spec websocket_info(term(), state()) -> ws_result().
websocket_info({heartbeat_check}, State = #{heartbeat_state := HeartbeatState}) ->
    handle_heartbeat_check(State, HeartbeatState);
websocket_info({dispatch, Event, Data, Seq}, State) ->
    handle_dispatch(Event, Data, Seq, State);
websocket_info({session_backpressure_error, Details}, State) ->
    handle_session_backpressure_error(Details, State);
websocket_info({'DOWN', _, process, Pid, _}, State = #{session_pid := Pid}) ->
    handle_session_down(State);
websocket_info(
    {'DOWN', _, process, Pid, _}, State = #{request_guild_members_pid := Pid}
) ->
    {ok, State#{request_guild_members_pid => undefined}};
websocket_info({process_voice_queue}, State) ->
    NewState = process_queued_voice_updates(State#{voice_queue_timer => undefined}),
    {ok, NewState};
websocket_info(_, State) ->
    {ok, State}.

-spec handle_heartbeat_check(state(), map()) -> ws_result().
handle_heartbeat_check(State = #{heartbeat_state := HeartbeatState}, _) ->
    Now = erlang:system_time(millisecond),
    LastAck = maps:get(last_ack, HeartbeatState, Now),
    WaitingForAck = maps:get(waiting_for_ack, HeartbeatState, false),
    HeartbeatTimeout = constants:heartbeat_timeout(),
    HeartbeatInterval = constants:heartbeat_interval(),
    handle_heartbeat_state(State, Now, LastAck, WaitingForAck, HeartbeatTimeout, HeartbeatInterval).

-spec handle_heartbeat_state(state(), integer(), integer(), boolean(), integer(), integer()) ->
    ws_result().
handle_heartbeat_state(State, Now, LastAck, true, HeartbeatTimeout, _) when
    (Now - LastAck) > HeartbeatTimeout
->
    gateway_metrics_collector:inc_heartbeat_failure(),
    close_with_reason(session_timeout, <<"Heartbeat timeout">>, State);
handle_heartbeat_state(
    State = #{heartbeat_state := HeartbeatState}, Now, LastAck, _, _, HeartbeatInterval
) when (Now - LastAck) >= (HeartbeatInterval * 0.9) ->
    Message = #{<<"op">> => constants:opcode_to_num(heartbeat), <<"d">> => null},
    schedule_heartbeat_check(),
    NewState = State#{heartbeat_state => HeartbeatState#{waiting_for_ack => true}},
    case encode_and_compress(Message, NewState) of
        {ok, Frame, NewState2} -> {[Frame], NewState2};
        {error, _} -> {ok, NewState}
    end;
handle_heartbeat_state(State, _, _, _, _, _) ->
    schedule_heartbeat_check(),
    {ok, State}.

-spec handle_dispatch(atom() | binary(), map() | null, integer(), state()) -> ws_result().
handle_dispatch(Event, Data, Seq, State) ->
    EventName = dispatch_event_name(Event),
    Message = #{
        <<"op">> => constants:opcode_to_num(dispatch),
        <<"t">> => EventName,
        <<"d">> => Data,
        <<"s">> => Seq
    },
    case encode_and_compress(Message, State) of
        {ok, Frame, NewState} -> {[Frame], NewState};
        {error, _Reason} -> {ok, State}
    end.

-spec dispatch_event_name(atom() | binary()) -> binary().
dispatch_event_name(Event) when is_binary(Event) -> Event;
dispatch_event_name(Event) when is_atom(Event) -> constants:dispatch_event_atom(Event);
dispatch_event_name(_) -> <<"UNKNOWN">>.

-spec handle_session_down(state()) -> ws_result().
handle_session_down(State) ->
    Message = #{<<"op">> => constants:opcode_to_num(invalid_session), <<"d">> => false},
    NewState = State#{session_pid => undefined},
    case encode_and_compress(Message, NewState) of
        {ok, Frame, NewState2} -> {[Frame], NewState2};
        {error, _} -> {ok, NewState}
    end.

-spec handle_session_backpressure_error(map(), state()) -> ws_result().
handle_session_backpressure_error(Details, State) ->
    Message = format_backpressure_close_message(Details),
    close_with_reason(ack_backpressure, Message, State).

-spec format_backpressure_close_message(map()) -> binary().
format_backpressure_close_message(Details) ->
    Kind = detail_value_to_binary(map_utils:get_safe(Details, kind, <<"unknown">>)),
    Unacked = map_utils:get_safe(Details, unacked_events, 0),
    CurrentSize = map_utils:get_safe(Details, current_size, 0),
    Limit = map_utils:get_safe(Details, limit, 0),
    Seq = map_utils:get_safe(Details, seq, 0),
    AckSeq = map_utils:get_safe(Details, ack_seq, 0),
    <<
        "Acknowledgement backlog exceeded: kind=",
        Kind/binary,
        " unacked=",
        (integer_to_binary(Unacked))/binary,
        " current=",
        (integer_to_binary(CurrentSize))/binary,
        " limit=",
        (integer_to_binary(Limit))/binary,
        " seq=",
        (integer_to_binary(Seq))/binary,
        " ack_seq=",
        (integer_to_binary(AckSeq))/binary
    >>.

-spec detail_value_to_binary(term()) -> binary().
detail_value_to_binary(Value) when is_binary(Value) ->
    Value;
detail_value_to_binary(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
detail_value_to_binary(_) ->
    <<"unknown">>.

-spec terminate(term(), cowboy_req:req(), state() | term()) -> ok.
terminate(Reason, _Req, #{compress_ctx := CompressCtx, otel_span_ctx := SpanCtx}) ->
    gateway_metrics_collector:inc_disconnections(),
    end_websocket_disconnect_span(Reason, SpanCtx),
    gateway_compress:close_context(CompressCtx),
    ok;
terminate(_Reason, _Req, _State) ->
    gateway_metrics_collector:inc_disconnections(),
    ok.

-spec validate_identify_data(map()) ->
    {ok, binary(), map(), term(), [binary()], non_neg_integer(), integer() | undefined}
    | {error, atom()}.
validate_identify_data(Data) ->
    try
        Token = maps:get(<<"token">>, Data),
        Properties = maps:get(<<"properties">>, Data),
        IgnoredEventsRaw = maps:get(<<"ignored_events">>, Data, []),
        InitialGuildIdRaw = maps:get(<<"initial_guild_id">>, Data, undefined),
        validate_properties(Token, Properties, IgnoredEventsRaw, InitialGuildIdRaw, Data)
    catch
        error:{badkey, _} -> {error, missing_required_field}
    end.

-spec validate_properties(binary(), term(), term(), term(), map()) ->
    {ok, binary(), map(), term(), [binary()], non_neg_integer(), integer() | undefined}
    | {error, atom()}.
validate_properties(Token, Properties, IgnoredEventsRaw, InitialGuildIdRaw, Data) when
    is_map(Properties)
->
    Os = maps:get(<<"os">>, Properties),
    Browser = maps:get(<<"browser">>, Properties),
    Device = maps:get(<<"device">>, Properties),
    case {is_binary(Os), is_binary(Browser), is_binary(Device)} of
        {true, true, true} ->
            Presence = maps:get(<<"presence">>, Data, null),
            validate_ignored_events(
                Token, Properties, Presence, IgnoredEventsRaw, InitialGuildIdRaw, Data
            );
        _ ->
            {error, invalid_properties}
    end;
validate_properties(_, _, _, _, _) ->
    {error, invalid_properties}.

-spec validate_ignored_events(binary(), map(), term(), term(), term(), map()) ->
    {ok, binary(), map(), term(), [binary()], non_neg_integer(), integer() | undefined}
    | {error, atom()}.
validate_ignored_events(Token, Properties, Presence, IgnoredEventsRaw, InitialGuildIdRaw, Data) ->
    case parse_ignored_events(IgnoredEventsRaw) of
        {ok, IgnoredEvents} ->
            FlagsRaw = maps:get(<<"flags">>, Data, 0),
            validate_flags(Token, Properties, Presence, IgnoredEvents, InitialGuildIdRaw, FlagsRaw);
        {error, Reason} ->
            {error, Reason}
    end.

-spec validate_flags(binary(), map(), term(), [binary()], term(), term()) ->
    {ok, binary(), map(), term(), [binary()], non_neg_integer(), integer() | undefined}
    | {error, atom()}.
validate_flags(Token, Properties, Presence, IgnoredEvents, InitialGuildIdRaw, Flags) when
    is_integer(Flags), Flags >= 0
->
    {ok, Token, Properties, Presence, IgnoredEvents, Flags,
        parse_initial_guild_id(InitialGuildIdRaw)};
validate_flags(_, _, _, _, _, _) ->
    {error, invalid_properties}.

-spec parse_ignored_events(term()) -> {ok, [binary()]} | {error, invalid_ignored_events}.
parse_ignored_events(undefined) ->
    {ok, []};
parse_ignored_events(null) ->
    {ok, []};
parse_ignored_events(Events) when is_list(Events) ->
    case lists:all(fun erlang:is_binary/1, Events) of
        true -> {ok, lists:usort([normalize_event_name(E) || E <- Events])};
        false -> {error, invalid_ignored_events}
    end;
parse_ignored_events(_) ->
    {error, invalid_ignored_events}.

-spec parse_initial_guild_id(term()) -> integer() | undefined.
parse_initial_guild_id(undefined) ->
    undefined;
parse_initial_guild_id(null) ->
    undefined;
parse_initial_guild_id(Value) when is_binary(Value) ->
    case validation:validate_snowflake(<<"initial_guild_id">>, Value) of
        {ok, GuildId} -> GuildId;
        {error, _, _} -> undefined
    end;
parse_initial_guild_id(_) ->
    undefined.

-spec normalize_event_name(binary()) -> binary().
normalize_event_name(Event) ->
    list_to_binary(string:uppercase(binary_to_list(Event))).

-spec handle_gateway_payload(atom(), map(), state()) -> ws_result().
handle_gateway_payload(
    heartbeat,
    #{<<"d">> := Seq},
    State = #{heartbeat_state := HeartbeatState, session_pid := SessionPid}
) ->
    handle_heartbeat(Seq, SessionPid, State, HeartbeatState);
handle_gateway_payload(
    identify, #{<<"d">> := Data}, State = #{session_pid := undefined, peer_ip := PeerIP}
) ->
    handle_identify(Data, PeerIP, State);
handle_gateway_payload(identify, _, State) ->
    close_with_reason(already_authenticated, <<"Already authenticated">>, State);
handle_gateway_payload(presence_update, #{<<"d">> := _}, State = #{session_pid := undefined}) ->
    close_with_reason(not_authenticated, <<"Not authenticated">>, State);
handle_gateway_payload(presence_update, #{<<"d">> := Data}, State = #{session_pid := Pid}) when
    is_pid(Pid)
->
    handle_presence_update(Data, Pid, State);
handle_gateway_payload(resume, #{<<"d">> := Data}, State) ->
    handle_resume(Data, State);
handle_gateway_payload(
    voice_state_update, #{<<"d">> := _}, State = #{session_pid := undefined}
) ->
    close_with_reason(not_authenticated, <<"Not authenticated">>, State);
handle_gateway_payload(
    voice_state_update, #{<<"d">> := Data}, State = #{session_pid := Pid}
) when is_pid(Pid) ->
    handle_voice_state_update(Pid, Data, State);
handle_gateway_payload(
    request_guild_members, #{<<"d">> := _}, State = #{session_pid := undefined}
) ->
    close_with_reason(not_authenticated, <<"Not authenticated">>, State);
handle_gateway_payload(
    request_guild_members, #{<<"d">> := Data}, State = #{session_pid := Pid}
) when is_pid(Pid) ->
    handle_request_guild_members(Data, Pid, State);
handle_gateway_payload(lazy_request, #{<<"d">> := _}, State = #{session_pid := undefined}) ->
    close_with_reason(not_authenticated, <<"Not authenticated">>, State);
handle_gateway_payload(lazy_request, #{<<"d">> := Data}, State = #{session_pid := Pid}) when
    is_pid(Pid)
->
    handle_lazy_request(Data, Pid, State);
handle_gateway_payload(_, _, State) ->
    close_with_reason(unknown_opcode, <<"Unknown opcode">>, State).

-spec handle_heartbeat(term(), pid() | undefined, state(), map()) -> ws_result().
handle_heartbeat(Seq, SessionPid, State, HeartbeatState) ->
    AckOk = verify_heartbeat_ack(Seq, SessionPid),
    case AckOk of
        true ->
            NewHeartbeatState = HeartbeatState#{
                last_ack => erlang:system_time(millisecond),
                waiting_for_ack => false
            },
            gateway_metrics_collector:inc_heartbeat_success(),
            AckMessage = #{<<"op">> => constants:opcode_to_num(heartbeat_ack)},
            NewState = State#{heartbeat_state => NewHeartbeatState},
            case encode_and_compress(AckMessage, NewState) of
                {ok, Frame, NewState2} -> {[Frame], NewState2};
                {error, _} -> {ok, NewState}
            end;
        false ->
            gateway_metrics_collector:inc_heartbeat_failure(),
            close_with_reason(invalid_seq, <<"Invalid sequence">>, State)
    end.

-spec verify_heartbeat_ack(term(), pid() | undefined) -> boolean().
verify_heartbeat_ack(_, undefined) ->
    true;
verify_heartbeat_ack(null, _) ->
    true;
verify_heartbeat_ack(SeqNum, Pid) when is_integer(SeqNum), is_pid(Pid) ->
    try
        case gen_server:call(Pid, {heartbeat_ack, SeqNum}, 5000) of
            true -> true;
            ok -> true;
            _ -> false
        end
    catch
        exit:_ -> false
    end;
verify_heartbeat_ack(_, _) ->
    false.

-spec handle_identify(map(), binary(), state()) -> ws_result().
handle_identify(Data, PeerIP, State) ->
    case validate_identify_data(Data) of
        {ok, Token, Properties, Presence, IgnoredEvents, Flags, InitialGuildId} ->
            SessionId = utils:generate_session_id(),
            SocketPid = self(),
            IdentifyData0 = #{
                token => Token,
                properties => Properties,
                presence => Presence,
                ignored_events => IgnoredEvents,
                flags => Flags
            },
            IdentifyData = add_initial_guild_id(IdentifyData0, InitialGuildId),
            Request = #{
                session_id => SessionId,
                peer_ip => PeerIP,
                identify_data => IdentifyData,
                version => maps:get(version, State)
            },
            start_session(Request, SocketPid, State);
        {error, _Reason} ->
            close_with_reason(decode_error, <<"Invalid identify payload">>, State)
    end.

-spec add_initial_guild_id(map(), integer() | undefined) -> map().
add_initial_guild_id(Data, undefined) -> Data;
add_initial_guild_id(Data, Id) -> maps:put(initial_guild_id, Id, Data).

-spec start_session(map(), pid(), state()) -> ws_result().
start_session(Request, SocketPid, State) ->
    case session_manager:start(Request, SocketPid) of
        {success, Pid} when is_pid(Pid) ->
            monitor(process, Pid),
            {ok, State#{session_pid => Pid}};
        {error, invalid_token} ->
            close_with_reason(authentication_failed, <<"Invalid token">>, State);
        {error, rate_limited} ->
            close_with_reason(rate_limited, <<"Rate limited">>, State);
        {error, identify_rate_limited} ->
            gateway_metrics_collector:inc_identify_rate_limited(),
            send_invalid_session(State);
        _ ->
            close_with_reason(unknown_error, <<"Failed to start session">>, State)
    end.

-spec send_invalid_session(state()) -> ws_result().
send_invalid_session(State) ->
    Message = #{<<"op">> => constants:opcode_to_num(invalid_session), <<"d">> => false},
    case encode_and_compress(Message, State) of
        {ok, Frame, NewState} -> {[Frame], NewState};
        {error, _} -> {ok, State}
    end.

-spec handle_presence_update(map(), pid(), state()) -> ws_result().
handle_presence_update(Data, Pid, State) ->
    Status = utils:parse_status(maps:get(<<"status">>, Data)),
    AdjustedStatus = adjust_status(Status),
    Afk = maps:get(<<"afk">>, Data, false),
    Mobile = maps:get(<<"mobile">>, Data, false),
    gen_server:cast(
        Pid, {presence_update, #{status => AdjustedStatus, afk => Afk, mobile => Mobile}}
    ),
    {ok, State}.

-spec adjust_status(atom()) -> atom().
adjust_status(offline) -> invisible;
adjust_status(Other) -> Other.

-spec handle_resume(map(), state()) -> ws_result().
handle_resume(Data, State) ->
    Token = maps:get(<<"token">>, Data),
    SessionId = maps:get(<<"session_id">>, Data),
    Seq = maps:get(<<"seq">>, Data),
    case is_binary(SessionId) of
        false ->
            handle_resume_session_not_found(State);
        true ->
            case session_manager:lookup(SessionId) of
                {ok, Pid} when is_pid(Pid) ->
                    handle_resume_with_session(Pid, Token, SessionId, Seq, State);
                {error, _} ->
                    handle_resume_session_not_found(State)
            end
    end.

-spec handle_voice_state_update(pid(), map(), state()) -> ws_result().
handle_voice_state_update(Pid, Data, State) ->
    case should_queue_voice_update(Pid) of
        false ->
            process_voice_update(Pid, Data, State);
        true ->
            queue_voice_update(Pid, Data),
            NewState = ensure_voice_queue_timer(State),
            {ok, NewState}
    end.

-spec handle_request_guild_members(map(), pid(), state()) -> ws_result().
handle_request_guild_members(
    _Data, _Pid, State = #{request_guild_members_pid := RequestPid}
) when is_pid(RequestPid) ->
    {ok, State};
handle_request_guild_members(Data, Pid, State) ->
    SocketPid = self(),
    {WorkerPid, _Ref} = spawn_monitor(fun() ->
        try
            case gen_server:call(Pid, {get_state}, 5000) of
                SessionState when is_map(SessionState) ->
                    guild_request_members:handle_request(Data, SocketPid, SessionState);
                _ ->
                    ok
            end
        catch
            _:_ -> ok
        end
    end),
    {ok, State#{request_guild_members_pid => WorkerPid}}.

-spec handle_lazy_request(map(), pid(), state()) -> ws_result().
handle_lazy_request(Data, Pid, State) ->
    SocketPid = self(),
    spawn(fun() ->
        try
            case gen_server:call(Pid, {get_state}, 5000) of
                SessionState when is_map(SessionState) ->
                    guild_unified_subscriptions:handle_subscriptions(Data, SocketPid, SessionState);
                _ ->
                    ok
            end
        catch
            _:_ -> ok
        end
    end),
    {ok, State}.

-spec schedule_heartbeat_check() -> reference().
schedule_heartbeat_check() ->
    erlang:send_after(constants:heartbeat_interval() div 3, self(), {heartbeat_check}).

-spec check_rate_limit(state(), atom()) -> {ok, state()} | rate_limited.
check_rate_limit(State = #{rate_limit_state := RateLimitState}, Op) ->
    Now = erlang:system_time(millisecond),
    Events = maps:get(events, RateLimitState, []),
    WindowStart = maps:get(window_start, RateLimitState, Now),
    EventsInWindow = [T || T <- Events, (Now - T) < ?RATE_LIMIT_WINDOW_MS],
    case length(EventsInWindow) >= ?RATE_LIMIT_MAX_EVENTS of
        true ->
            rate_limited;
        false ->
            case check_opcode_rate_limit(Op, RateLimitState, Now) of
                rate_limited ->
                    rate_limited;
                {ok, OpRateLimitState} ->
                    NewEvents = [Now | EventsInWindow],
                    NewRateLimitState =
                        OpRateLimitState#{events => NewEvents, window_start => WindowStart},
                    {ok, State#{rate_limit_state => NewRateLimitState}}
            end
    end.

-spec check_opcode_rate_limit(atom(), map(), integer()) -> {ok, map()} | rate_limited.
check_opcode_rate_limit(request_guild_members, RateLimitState, Now) ->
    RequestEvents = maps:get(request_guild_members_events, RateLimitState, []),
    RequestEventsInWindow =
        [T || T <- RequestEvents, (Now - T) < ?REQUEST_GUILD_MEMBERS_RATE_LIMIT_WINDOW_MS],
    case length(RequestEventsInWindow) >= ?REQUEST_GUILD_MEMBERS_RATE_LIMIT_MAX_EVENTS of
        true ->
            rate_limited;
        false ->
            {ok, RateLimitState#{request_guild_members_events => [Now | RequestEventsInWindow]}}
    end;
check_opcode_rate_limit(_, RateLimitState, _Now) ->
    {ok, RateLimitState}.

-spec extract_client_ip(cowboy_req:req()) -> binary().
extract_client_ip(Req) ->
    case cowboy_req:header(<<"x-forwarded-for">>, Req) of
        undefined ->
            peer_ip_to_binary(cowboy_req:peer(Req));
        ForwardedFor ->
            case parse_forwarded_for(ForwardedFor) of
                <<>> -> peer_ip_to_binary(cowboy_req:peer(Req));
                IP -> IP
            end
    end.

-spec peer_ip_to_binary({inet:ip_address(), inet:port_number()}) -> binary().
peer_ip_to_binary({PeerIP, _Port}) ->
    list_to_binary(inet:ntoa(PeerIP)).

-spec parse_forwarded_for(binary()) -> binary().
parse_forwarded_for(HeaderValue) ->
    case binary:split(HeaderValue, <<",">>) of
        [First | _] ->
            case normalize_forwarded_ip(First) of
                {ok, IP} -> IP;
                error -> <<>>
            end;
        [] ->
            <<>>
    end.

-spec normalize_forwarded_ip(binary()) -> {ok, binary()} | error.
normalize_forwarded_ip(Value) ->
    Trimmed = string:trim(Value),
    case Trimmed of
        <<>> ->
            error;
        <<"[", _/binary>> ->
            case strip_ipv6_brackets(Trimmed) of
                {ok, IPv6} -> validate_ip(IPv6);
                error -> error
            end;
        _ ->
            Cleaned = strip_ipv4_port(Trimmed),
            validate_ip(Cleaned)
    end.

-spec strip_ipv6_brackets(binary()) -> {ok, binary()} | error.
strip_ipv6_brackets(<<"[", Rest/binary>>) ->
    case binary:match(Rest, <<"]">>) of
        {Pos, _Len} when Pos > 0 -> {ok, binary:part(Rest, 0, Pos)};
        _ -> error
    end;
strip_ipv6_brackets(_) ->
    error.

-spec strip_ipv4_port(binary()) -> binary().
strip_ipv4_port(IP) ->
    case binary:match(IP, <<".">>) of
        nomatch ->
            IP;
        _ ->
            case binary:split(IP, <<":">>, [global]) of
                [Addr, _Port] -> Addr;
                _ -> IP
            end
    end.

-spec validate_ip(binary()) -> {ok, binary()} | error.
validate_ip(IP) ->
    case inet:parse_address(binary_to_list(IP)) of
        {ok, Parsed} -> {ok, list_to_binary(inet:ntoa(Parsed))};
        {error, _} -> error
    end.

-spec handle_resume_with_session(pid(), binary(), binary(), integer(), state()) -> ws_result().
handle_resume_with_session(Pid, Token, SessionId, Seq, State) ->
    case gen_server:call(Pid, {token_verify, Token}, 5000) of
        true -> handle_resume_with_verified_token(Pid, SessionId, Seq, State);
        false -> handle_resume_invalid_token(State)
    end.

-spec handle_resume_with_verified_token(pid(), binary(), integer(), state()) -> ws_result().
handle_resume_with_verified_token(Pid, SessionId, Seq, State) ->
    SocketPid = self(),
    case gen_server:call(Pid, {resume, Seq, SocketPid}, 5000) of
        {ok, MissedEvents} when is_list(MissedEvents) ->
            handle_resume_success(Pid, SessionId, Seq, MissedEvents, State);
        invalid_seq ->
            handle_resume_invalid_seq(State)
    end.

-spec handle_resume_success(pid(), binary(), integer(), [map()], state()) -> ws_result().
handle_resume_success(Pid, _SessionId, Seq, MissedEvents, State) ->
    gateway_metrics_collector:inc_resume_success(),
    SocketPid = self(),
    monitor(process, Pid),
    lists:foreach(
        fun(Event) when is_map(Event) ->
            SocketPid !
                {dispatch, maps:get(event, Event), maps:get(data, Event), maps:get(seq, Event)}
        end,
        MissedEvents
    ),
    SocketPid ! {dispatch, resumed, null, Seq},
    {ok, State#{
        session_pid => Pid,
        heartbeat_state => #{
            last_ack => erlang:system_time(millisecond),
            waiting_for_ack => false
        }
    }}.

-spec handle_resume_invalid_seq(state()) -> ws_result().
handle_resume_invalid_seq(State) ->
    gateway_metrics_collector:inc_resume_failure(),
    close_with_reason(invalid_seq, <<"Invalid sequence">>, State).

-spec handle_resume_invalid_token(state()) -> ws_result().
handle_resume_invalid_token(State) ->
    gateway_metrics_collector:inc_resume_failure(),
    close_with_reason(authentication_failed, <<"Invalid token">>, State).

-spec handle_resume_session_not_found(state()) -> ws_result().
handle_resume_session_not_found(State) ->
    gateway_metrics_collector:inc_resume_failure(),
    send_invalid_session(State).

-spec encode_and_compress(map(), state()) -> {ok, ws_frame(), state()} | {error, term()}.
encode_and_compress(Message, State = #{encoding := Encoding, compress_ctx := CompressCtx}) ->
    case gateway_codec:encode(Message, Encoding) of
        {ok, Encoded, FrameType} ->
            case gateway_compress:compress(Encoded, CompressCtx) of
                {ok, Compressed, NewCompressCtx} ->
                    Frame = make_frame(Compressed, FrameType, NewCompressCtx),
                    {ok, Frame, State#{compress_ctx => NewCompressCtx}};
                {error, Reason} ->
                    {error, {compress_failed, gateway_compress:get_type(CompressCtx), Reason}}
            end;
        {error, Reason} ->
            {error, {encode_failed, Reason}}
    end.

-spec compression_error_reason(atom()) -> binary().
compression_error_reason(zstd_stream) -> <<"Compression failed: zstd-stream">>;
compression_error_reason(_) -> <<"Encode failed">>.

-spec close_with_reason(atom(), binary(), state()) -> ws_result().
close_with_reason(Reason, Message, State) ->
    gateway_metrics_collector:inc_websocket_close(Reason),
    CloseCode = constants:close_code_to_num(Reason),
    {[{close, CloseCode, Message}], State}.

-spec make_frame(binary(), text | binary, gateway_compress:compress_ctx()) -> ws_frame().
make_frame(Data, FrameType, CompressCtx) ->
    case gateway_compress:get_type(CompressCtx) of
        none -> {FrameType, Data};
        _ -> {binary, Data}
    end.

-spec start_websocket_connect_span(1 | undefined, state()) -> term().
start_websocket_connect_span(Version, #{peer_ip := PeerIP}) ->
    gateway_tracing:start_connection_span(?MODULE, Version, PeerIP).

-spec end_websocket_disconnect_span(term(), term()) -> ok.
end_websocket_disconnect_span(_Reason, undefined) ->
    ok;
end_websocket_disconnect_span(Reason, Ctx) ->
    ReasonBin = reason_to_binary(Reason),
    gateway_tracing:end_connection_span(Ctx, ReasonBin).

-spec reason_to_binary(term()) -> binary().
reason_to_binary(normal) -> <<"normal">>;
reason_to_binary(remote) -> <<"remote">>;
reason_to_binary(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8);
reason_to_binary(_) -> <<"unknown">>.

-spec should_queue_voice_update(pid()) -> boolean().
should_queue_voice_update(SessionPid) ->
    ensure_voice_rate_limit_table(),
    Now = erlang:system_time(millisecond),
    case ets:lookup(?VOICE_RATE_LIMIT_TABLE, SessionPid) of
        [] ->
            ets:insert(?VOICE_RATE_LIMIT_TABLE, {SessionPid, [Now]}),
            false;
        [{SessionPid, Timestamps}] ->
            FilteredTimestamps = [T || T <- Timestamps, (Now - T) < ?VOICE_RATE_LIMIT_WINDOW],
            Count = length(FilteredTimestamps),
            case Count >= ?VOICE_UPDATE_RATE_LIMIT of
                true ->
                    true;
                false ->
                    NewTimestamps = [Now | FilteredTimestamps],
                    ets:insert(?VOICE_RATE_LIMIT_TABLE, {SessionPid, NewTimestamps}),
                    false
            end
    end.

-spec process_voice_update(pid(), map(), state()) -> ws_result().
process_voice_update(SessionPid, Data, State) ->
    try
        gen_server:call(
            SessionPid,
            {voice_state_update, Data},
            5000
        ),
        {ok, State}
    catch
        exit:{timeout, _} ->
            {ok, State};
        exit:{noproc, _} ->
            {ok, State}
    end.

-spec queue_voice_update(pid(), map()) -> ok.
queue_voice_update(SessionPid, Data) ->
    ensure_voice_queue_table(),
    case ets:lookup(?VOICE_QUEUE_TABLE, SessionPid) of
        [] ->
            Queue = queue:in(Data, queue:new()),
            ets:insert(?VOICE_QUEUE_TABLE, {SessionPid, Queue});
        [{SessionPid, Queue}] ->
            TrimmedQueue = trim_voice_queue(Queue),
            NewQueue = queue:in(Data, TrimmedQueue),
            ets:insert(?VOICE_QUEUE_TABLE, {SessionPid, NewQueue})
    end,
    ok.

-spec trim_voice_queue(queue:queue()) -> queue:queue().
trim_voice_queue(Queue) ->
    case queue:len(Queue) >= ?MAX_VOICE_QUEUE_LENGTH of
        false ->
            Queue;
        true ->
            case queue:out(Queue) of
                {empty, EmptyQueue} ->
                    EmptyQueue;
                {{value, _Dropped}, RemainingQueue} ->
                    RemainingQueue
            end
    end.

-spec ensure_voice_queue_timer(state()) -> state().
ensure_voice_queue_timer(State = #{voice_queue_timer := undefined}) ->
    Timer = erlang:send_after(?VOICE_QUEUE_PROCESS_INTERVAL, self(), {process_voice_queue}),
    State#{voice_queue_timer => Timer};
ensure_voice_queue_timer(State) ->
    State.

-spec process_queued_voice_updates(state()) -> state().
process_queued_voice_updates(State = #{session_pid := SessionPid}) when is_pid(SessionPid) ->
    ensure_voice_queue_table(),
    case ets:lookup(?VOICE_QUEUE_TABLE, SessionPid) of
        [] ->
            State;
        [{SessionPid, Queue}] ->
            process_queue_item(Queue, SessionPid, State)
    end;
process_queued_voice_updates(State) ->
    State.

-spec process_queue_item(queue:queue(), pid(), state()) -> state().
process_queue_item(Queue, SessionPid, State) ->
    case queue:out(Queue) of
        {empty, _} ->
            ets:delete(?VOICE_QUEUE_TABLE, SessionPid),
            State;
        {{value, Data}, NewQueue} ->
            case should_queue_voice_update(SessionPid) of
                false ->
                    process_voice_update(SessionPid, Data, State),
                    case queue:is_empty(NewQueue) of
                        true ->
                            ets:delete(?VOICE_QUEUE_TABLE, SessionPid),
                            State;
                        false ->
                            ets:insert(?VOICE_QUEUE_TABLE, {SessionPid, NewQueue}),
                            ensure_voice_queue_timer(State)
                    end;
                true ->
                    ensure_voice_queue_timer(State)
            end
    end.

-spec ensure_voice_queue_table() -> ok.
ensure_voice_queue_table() ->
    case ets:whereis(?VOICE_QUEUE_TABLE) of
        undefined ->
            try
                ets:new(?VOICE_QUEUE_TABLE, [named_table, public, set]),
                ok
            catch
                error:badarg -> ok
            end;
        _ ->
            ok
    end.

-spec ensure_voice_rate_limit_table() -> ok.
ensure_voice_rate_limit_table() ->
    case ets:whereis(?VOICE_RATE_LIMIT_TABLE) of
        undefined ->
            try
                ets:new(?VOICE_RATE_LIMIT_TABLE, [named_table, public, set]),
                ok
            catch
                error:badarg -> ok
            end;
        _ ->
            ok
    end.

-ifdef(TEST).

parse_forwarded_for_ipv4_test() ->
    ?assertEqual(<<"203.0.113.7">>, parse_forwarded_for(<<"203.0.113.7">>)).

parse_forwarded_for_ipv4_with_port_test() ->
    ?assertEqual(<<"203.0.113.7">>, parse_forwarded_for(<<"203.0.113.7:8080">>)).

parse_forwarded_for_ipv4_with_port_and_extra_entries_test() ->
    Header = <<" 203.0.113.7:8080 , 10.0.0.1">>,
    ?assertEqual(<<"203.0.113.7">>, parse_forwarded_for(Header)).

parse_forwarded_for_ipv6_test() ->
    ?assertEqual(<<"2001:db8::1">>, parse_forwarded_for(<<"2001:db8::1">>)).

parse_forwarded_for_ipv6_with_brackets_test() ->
    ?assertEqual(<<"2001:db8::1">>, parse_forwarded_for(<<"[2001:db8::1]">>)).

parse_forwarded_for_ipv6_with_brackets_and_port_test() ->
    ?assertEqual(<<"2001:db8::1">>, parse_forwarded_for(<<"[2001:db8::1]:443">>)).

parse_forwarded_for_ipv6_with_spaces_test() ->
    ?assertEqual(<<"2001:db8::1">>, parse_forwarded_for(<<"  [2001:db8::1]  ">>)).

parse_forwarded_for_invalid_ip_test() ->
    ?assertEqual(<<>>, parse_forwarded_for(<<"not_an_ip">>)).

parse_forwarded_for_invalid_ipv4_octet_test() ->
    ?assertEqual(<<>>, parse_forwarded_for(<<"203.0.113.300">>)).

parse_forwarded_for_unterminated_bracket_test() ->
    ?assertEqual(<<>>, parse_forwarded_for(<<"[2001:db8::1">>)).

parse_version_test() ->
    ?assertEqual(1, parse_version(<<"1">>)),
    ?assertEqual(undefined, parse_version(<<"2">>)),
    ?assertEqual(undefined, parse_version(undefined)).

parse_ignored_events_test() ->
    ?assertEqual({ok, []}, parse_ignored_events(undefined)),
    ?assertEqual({ok, []}, parse_ignored_events(null)),
    ?assertEqual({ok, [<<"TYPING_START">>]}, parse_ignored_events([<<"typing_start">>])),
    ?assertEqual({error, invalid_ignored_events}, parse_ignored_events([123])),
    ?assertEqual({error, invalid_ignored_events}, parse_ignored_events(<<"not_a_list">>)).

adjust_status_test() ->
    ?assertEqual(invisible, adjust_status(offline)),
    ?assertEqual(online, adjust_status(online)),
    ?assertEqual(idle, adjust_status(idle)).

check_rate_limit_blocks_general_flood_test() ->
    Now = erlang:system_time(millisecond),
    Events = lists:duplicate(?RATE_LIMIT_MAX_EVENTS, Now - 1000),
    State = (new_state())#{
        rate_limit_state => #{
            events => Events,
            request_guild_members_events => [],
            window_start => Now
        }
    },
    ?assertEqual(rate_limited, check_rate_limit(State, heartbeat)).

check_rate_limit_blocks_request_guild_members_burst_test() ->
    Now = erlang:system_time(millisecond),
    RequestEvents =
        lists:duplicate(?REQUEST_GUILD_MEMBERS_RATE_LIMIT_MAX_EVENTS, Now - 1000),
    State = (new_state())#{
        rate_limit_state => #{
            events => [],
            request_guild_members_events => RequestEvents,
            window_start => Now
        }
    },
    ?assertEqual(rate_limited, check_rate_limit(State, request_guild_members)).

check_rate_limit_allows_other_ops_when_request_guild_members_is_hot_test() ->
    Now = erlang:system_time(millisecond),
    RequestEvents =
        lists:duplicate(?REQUEST_GUILD_MEMBERS_RATE_LIMIT_MAX_EVENTS, Now - 1000),
    State = (new_state())#{
        rate_limit_state => #{
            events => [],
            request_guild_members_events => RequestEvents,
            window_start => Now
        }
    },
    ?assertMatch({ok, _}, check_rate_limit(State, heartbeat)).

-endif.
