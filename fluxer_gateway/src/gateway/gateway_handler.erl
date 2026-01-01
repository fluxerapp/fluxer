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

-record(state, {
    version,
    encoding = json :: gateway_codec:encoding(),
    compress_ctx :: gateway_compress:compress_ctx(),
    session_pid,
    heartbeat_state = #{},
    socket_pid,
    peer_ip,
    rate_limit_state = #{events => [], window_start => undefined}
}).

init(Req, _Opts) ->
    QS = cowboy_req:parse_qs(Req),
    Version =
        case proplists:get_value(<<"v">>, QS) of
            <<"1">> -> 1;
            _ -> undefined
        end,
    Encoding = gateway_codec:parse_encoding(proplists:get_value(<<"encoding">>, QS)),
    Compression = gateway_compress:parse_compression(proplists:get_value(<<"compress">>, QS)),
    CompressCtx = gateway_compress:new_context(Compression),

    PeerIPBinary = extract_client_ip(Req),

    {cowboy_websocket, Req, #state{
        version = Version,
        encoding = Encoding,
        compress_ctx = CompressCtx,
        socket_pid = self(),
        peer_ip = PeerIPBinary
    }}.

websocket_init(State = #state{version = Version}) ->
    gateway_metrics_collector:inc_connections(),
    case Version of
        1 ->
            CompressionType = gateway_compress:get_type(State#state.compress_ctx),
            FreshCompressCtx = gateway_compress:new_context(CompressionType),
            FreshState0 = State#state{compress_ctx = FreshCompressCtx},
            HeartbeatInterval = constants:heartbeat_interval(),
            HelloMessage = #{
                <<"op">> => constants:opcode_to_num(hello),
                <<"d">> => #{
                    <<"heartbeat_interval">> => HeartbeatInterval
                }
            },
            schedule_heartbeat_check(),
            NewState = FreshState0#state{
                heartbeat_state = #{
                    last_ack => erlang:system_time(millisecond),
                    waiting_for_ack => false
                }
            },
            case encode_and_compress(HelloMessage, NewState) of
                {ok, Frame, NewState2} ->
                    {[Frame], NewState2};
                {error, {compress_failed, CompressionType, Reason}} ->
                    logger:warning(
                        "[gateway_handler] Failed to compress HELLO frame, type=~p, reason=~p",
                        [CompressionType, Reason]
                    ),
                    close_with_reason(decode_error, compression_error_reason(CompressionType), NewState);
                {error, _Reason} ->
                    close_with_reason(decode_error, <<"Encode failed">>, NewState)
            end;
        _ ->
            close_with_reason(invalid_api_version, <<"Invalid API version">>, State)
    end.

websocket_handle({text, Text}, State) ->
    handle_incoming_data(Text, State);
websocket_handle({binary, Binary}, State) ->
    handle_incoming_data(Binary, State);
websocket_handle(_, State) ->
    {ok, State}.

handle_incoming_data(Data, State = #state{encoding = Encoding, compress_ctx = CompressCtx}) ->
    MaxSize = constants:max_payload_size(),
    case byte_size(Data) =< MaxSize of
        true ->
            case gateway_codec:decode(Data, Encoding) of
                {ok, #{<<"op">> := Op} = Payload} ->
                    logger:debug("handle_incoming_data: received op ~p", [Op]),
                    NewState = State#state{compress_ctx = CompressCtx},
                    OpAtom = constants:gateway_opcode(Op),
                    logger:debug("handle_incoming_data: op ~p converted to atom ~p", [Op, OpAtom]),
                    case check_rate_limit(NewState) of
                        {ok, RateLimitedState} ->
                            handle_gateway_payload(OpAtom, Payload, RateLimitedState);
                        rate_limited ->
                            close_with_reason(rate_limited, <<"Rate limited">>, NewState)
                    end;
                {ok, _} ->
                    close_with_reason(decode_error, <<"Invalid payload">>, State#state{
                        compress_ctx = CompressCtx
                    });
                {error, _Reason} ->
                    close_with_reason(decode_error, <<"Decode failed">>, State)
            end;
        false ->
            close_with_reason(decode_error, <<"Payload too large">>, State)
    end.

websocket_info({heartbeat_check}, State = #state{heartbeat_state = HeartbeatState}) ->
    Now = erlang:system_time(millisecond),
    LastAck = maps:get(last_ack, HeartbeatState, Now),
    WaitingForAck = maps:get(waiting_for_ack, HeartbeatState, false),

    HeartbeatTimeout = constants:heartbeat_timeout(),
    HeartbeatInterval = constants:heartbeat_interval(),

    if
        WaitingForAck andalso (Now - LastAck) > HeartbeatTimeout ->
            gateway_metrics_collector:inc_heartbeat_failure(),
            close_with_reason(session_timeout, <<"Heartbeat timeout">>, State);
        (Now - LastAck) >= (HeartbeatInterval * 0.9) ->
            Message = #{
                <<"op">> => constants:opcode_to_num(heartbeat),
                <<"d">> => null
            },
            schedule_heartbeat_check(),
            NewState = State#state{heartbeat_state = HeartbeatState#{waiting_for_ack => true}},
            case encode_and_compress(Message, NewState) of
                {ok, Frame, NewState2} ->
                    {[Frame], NewState2};
                {error, _} ->
                    {ok, NewState}
            end;
        true ->
            schedule_heartbeat_check(),
            {ok, State}
    end;
websocket_info({dispatch, Event, Data, Seq}, State) ->
    logger:debug("websocket_info: dispatch event ~p with seq ~p", [Event, Seq]),
    EventName =
        if
            is_binary(Event) -> Event;
            is_atom(Event) -> constants:dispatch_event_atom(Event);
            true -> <<"UNKNOWN">>
        end,

    DataPreview =
        case is_map(Data) of
            true -> maps:with([<<"guild_id">>, <<"chunk_index">>, <<"chunk_count">>, <<"nonce">>], Data);
            false -> Data
        end,

    logger:debug(
        "websocket_info: dispatch data preview: ~p",
        [DataPreview]
    ),

    Message = #{
        <<"op">> => constants:opcode_to_num(dispatch),
        <<"t">> => EventName,
        <<"d">> => Data,
        <<"s">> => Seq
    },
    case encode_and_compress(Message, State) of
        {ok, Frame, NewState} ->
            logger:debug(
                "websocket_info: dispatch ~p (seq ~p) encoded and sent successfully",
                [EventName, Seq]
            ),
            {[Frame], NewState};
        {error, Reason} ->
            logger:error("websocket_info: encode_and_compress failed for ~p: ~p", [EventName, Reason]),
            {ok, State}
    end;
websocket_info({'DOWN', _, process, Pid, _}, State = #state{session_pid = SessionPid}) when
    Pid =:= SessionPid
->
    Message = #{
        <<"op">> => constants:opcode_to_num(invalid_session),
        <<"d">> => false
    },
    NewState = State#state{session_pid = undefined},
    case encode_and_compress(Message, NewState) of
        {ok, Frame, NewState2} ->
            {[Frame], NewState2};
        {error, _} ->
            {ok, NewState}
    end;
websocket_info(_, State) ->
    {ok, State}.

terminate(_Reason, _Req, #state{compress_ctx = CompressCtx}) ->
    gateway_metrics_collector:inc_disconnections(),
    gateway_compress:close_context(CompressCtx),
    ok;
terminate(_Reason, _Req, _State) ->
    gateway_metrics_collector:inc_disconnections(),
    ok.

validate_identify_data(Data) ->
    try
        Token = maps:get(<<"token">>, Data),
        Properties = maps:get(<<"properties">>, Data),
        IgnoredEventsRaw = maps:get(<<"ignored_events">>, Data, []),
        InitialGuildIdRaw = maps:get(<<"initial_guild_id">>, Data, undefined),

        case is_map(Properties) of
            true ->
                Os = maps:get(<<"os">>, Properties),
                Browser = maps:get(<<"browser">>, Properties),
                Device = maps:get(<<"device">>, Properties),

                case is_binary(Os) andalso is_binary(Browser) andalso is_binary(Device) of
                    true ->
                        Presence = maps:get(<<"presence">>, Data, null),
                case parse_ignored_events(IgnoredEventsRaw) of
                    {ok, IgnoredEvents} ->
                        FlagsRaw = maps:get(<<"flags">>, Data, 0),
                        case FlagsRaw of
                            Flags when is_integer(Flags), Flags >= 0 ->
                                {ok, Token, Properties, Presence, IgnoredEvents, Flags, parse_initial_guild_id(InitialGuildIdRaw)};
                            _ ->
                                {error, invalid_properties}
                        end;
                    {error, Reason} ->
                        {error, Reason}
                end;
            false ->
                {error, invalid_properties}
        end;
    false ->
        {error, invalid_properties}
        end
    catch
        error:{badkey, _} ->
            {error, missing_required_field}
    end.

parse_ignored_events(undefined) ->
    {ok, []};
parse_ignored_events(null) ->
    {ok, []};
parse_ignored_events(Events) when is_list(Events) ->
    case lists:all(fun(E) -> is_binary(E) end, Events) of
        true ->
            Normalized = lists:usort([normalize_event_name(E) || E <- Events]),
            {ok, Normalized};
        false ->
            {error, invalid_ignored_events}
    end;
parse_ignored_events(_) ->
    {error, invalid_ignored_events}.

parse_initial_guild_id(undefined) ->
    undefined;
parse_initial_guild_id(null) ->
    undefined;
parse_initial_guild_id(Value) when is_binary(Value) ->
    case validation:validate_snowflake(<<"initial_guild_id">>, Value) of
        {ok, GuildId} ->
            GuildId;
        {error, _, Reason} ->
            logger:warning(
                "[gateway_handler] Invalid initial_guild_id ~p: ~p",
                [Value, Reason]
            ),
            undefined
    end;
parse_initial_guild_id(_) ->
    undefined.

normalize_event_name(Event) ->
    list_to_binary(string:uppercase(binary_to_list(Event))).

handle_gateway_payload(
    heartbeat,
    #{<<"d">> := Seq},
    State = #state{heartbeat_state = HeartbeatState, session_pid = SessionPid}
) ->
    AckOk =
        try
            case {SessionPid, Seq} of
                {undefined, _} -> true;
                {_Pid, null} -> true;
                {Pid, SeqNum} when is_integer(SeqNum) ->
                    case gen_server:call(Pid, {heartbeat_ack, SeqNum}, 5000) of
                        true -> true;
                        ok -> true;
                        _ -> false
                    end;
                _ ->
                    false
            end
        catch
            exit:_ -> false
        end,

    case AckOk of
        true ->
            NewHeartbeatState = HeartbeatState#{
                last_ack => erlang:system_time(millisecond),
                waiting_for_ack => false
            },
            gateway_metrics_collector:inc_heartbeat_success(),
            AckMessage = #{<<"op">> => constants:opcode_to_num(heartbeat_ack)},
            NewState = State#state{heartbeat_state = NewHeartbeatState},
            case encode_and_compress(AckMessage, NewState) of
                {ok, Frame, NewState2} ->
                    {[Frame], NewState2};
                {error, _} ->
                    {ok, NewState}
            end;
        false ->
            gateway_metrics_collector:inc_heartbeat_failure(),
            close_with_reason(invalid_seq, <<"Invalid sequence">>, State)
    end;
handle_gateway_payload(
    identify,
    #{<<"d">> := Data},
    State = #state{session_pid = undefined, peer_ip = PeerIP}
) ->
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
                    IdentifyData =
                        case InitialGuildId of
                            undefined -> IdentifyData0;
                            Id -> maps:put(initial_guild_id, Id, IdentifyData0)
                        end,
                    Request = #{
                        session_id => SessionId,
                        peer_ip => PeerIP,
                        identify_data => IdentifyData,
                        version => State#state.version
                    },

            case gen_server:call(session_manager, {start, Request, SocketPid}, 10000) of
                {success, Pid} when is_pid(Pid) ->
                    monitor(process, Pid),
                    {ok, State#state{session_pid = Pid}};
                {error, invalid_token} ->
                    close_with_reason(authentication_failed, <<"Invalid token">>, State);
                {error, rate_limited} ->
                    close_with_reason(rate_limited, <<"Rate limited">>, State);
                {error, identify_rate_limited} ->
                    gateway_metrics_collector:inc_identify_rate_limited(),
                    Message = #{
                        <<"op">> => constants:opcode_to_num(invalid_session),
                        <<"d">> => false
                    },
                    case encode_and_compress(Message, State) of
                        {ok, Frame, NewState} ->
                            {[Frame], NewState};
                        {error, _} ->
                            {ok, State}
                    end;
                _ ->
                    close_with_reason(unknown_error, <<"Failed to start session">>, State)
            end;
        {error, _Reason} ->
            close_with_reason(decode_error, <<"Invalid identify payload">>, State)
    end;
handle_gateway_payload(identify, _, State = #state{session_pid = _}) ->
    close_with_reason(already_authenticated, <<"Already authenticated">>, State);
handle_gateway_payload(
    presence_update, #{<<"d">> := _Data}, State = #state{session_pid = undefined}
) ->
    close_with_reason(not_authenticated, <<"Not authenticated">>, State);
handle_gateway_payload(presence_update, #{<<"d">> := Data}, State = #state{session_pid = Pid}) when
    is_pid(Pid)
->
    Status = utils:parse_status(maps:get(<<"status">>, Data)),
    AdjustedStatus =
        case Status of
            offline -> invisible;
            Other -> Other
        end,
    Afk = maps:get(<<"afk">>, Data, false),
    Mobile = maps:get(<<"mobile">>, Data, false),

    gen_server:cast(
        Pid, {presence_update, #{status => AdjustedStatus, afk => Afk, mobile => Mobile}}
    ),
    {ok, State};
handle_gateway_payload(resume, #{<<"d">> := Data}, State) ->
    Token = maps:get(<<"token">>, Data),
    SessionId = maps:get(<<"session_id">>, Data),
    Seq = maps:get(<<"seq">>, Data),

    case gen_server:call(session_manager, {lookup, SessionId}, 5000) of
        {ok, Pid} when is_pid(Pid) ->
            handle_resume_with_session(Pid, Token, SessionId, Seq, State);
        {error, not_found} ->
            handle_resume_session_not_found(SessionId, State)
    end;
handle_gateway_payload(
    voice_state_update, #{<<"d">> := _Data}, State = #state{session_pid = undefined}
) ->
    close_with_reason(not_authenticated, <<"Not authenticated">>, State);
handle_gateway_payload(
    voice_state_update, #{<<"d">> := Data}, State = #state{session_pid = Pid}
) when
    is_pid(Pid)
->
    logger:debug("[gateway_handler] Processing voice state update: ~p", [Data]),
    try gen_server:call(Pid, {voice_state_update, Data}, 15000) of
        ok ->
            logger:debug("[gateway_handler] Voice state update succeeded"),
            {ok, State};
        {error, Category, ErrorAtom} when is_atom(ErrorAtom) ->
            logger:warning("[gateway_handler] Voice state update failed: Category=~p, Error=~p", [
                Category, ErrorAtom
            ]),
            send_gateway_error(ErrorAtom, State);
        UnexpectedResponse ->
            logger:error(
                "[gateway_handler] Voice state update returned unexpected response: ~p, Data: ~p", [
                    UnexpectedResponse, Data
                ]
            ),
            send_gateway_error(internal_error, State)
    catch
        exit:{timeout, _} ->
            logger:error("[gateway_handler] Voice state update timed out (>15s) for Data: ~p", [
                Data
            ]),
            send_gateway_error(timeout, State);
        Class:ExReason:Stacktrace ->
            logger:error(
                "[gateway_handler] Voice state update crashed: ~p:~p~nStacktrace: ~p~nData: ~p", [
                    Class, ExReason, Stacktrace, Data
                ]
            ),
            send_gateway_error(internal_error, State)
    end;
handle_gateway_payload(call_connect, #{<<"d">> := _Data}, State = #state{session_pid = undefined}) ->
    close_with_reason(not_authenticated, <<"Not authenticated">>, State);
handle_gateway_payload(call_connect, #{<<"d">> := Data}, State = #state{session_pid = Pid}) when
    is_pid(Pid)
->
    ChannelId = maps:get(<<"channel_id">>, Data),

    gen_server:cast(Pid, {call_connect, ChannelId}),
    {ok, State};
handle_gateway_payload(
    request_guild_members, #{<<"d">> := _Data}, State = #state{session_pid = undefined}
) ->
    close_with_reason(not_authenticated, <<"Not authenticated">>, State);
handle_gateway_payload(
    request_guild_members, #{<<"d">> := Data}, State = #state{session_pid = Pid}
) when
    is_pid(Pid)
->
    SocketPid = self(),
    spawn(fun() ->
        try
            case gen_server:call(Pid, {get_state}, 5000) of
                SessionState when is_map(SessionState) ->
                    case guild_request_members:handle_request(Data, SocketPid, SessionState) of
                        ok ->
                            logger:debug("[gateway_handler] Guild members request completed successfully");
                        {error, ErrorReason} ->
                            logger:warning(
                                "[gateway_handler] Guild members request failed: ~p",
                                [ErrorReason]
                            )
                    end;
                Other ->
                    logger:warning(
                        "[gateway_handler] Failed to get session state for guild members request: ~p",
                        [Other]
                    )
            end
        catch
            Class:ExceptionReason:Stacktrace ->
                logger:error(
                    "[gateway_handler] Guild members request crashed: ~p:~p~nStacktrace: ~p",
                    [Class, ExceptionReason, Stacktrace]
                )
        end
    end),
    {ok, State};
handle_gateway_payload(
    lazy_request, #{<<"d">> := _Data}, State = #state{session_pid = undefined}
) ->
    close_with_reason(not_authenticated, <<"Not authenticated">>, State);
handle_gateway_payload(
    lazy_request, #{<<"d">> := Data}, State = #state{session_pid = Pid}
) when
    is_pid(Pid)
->
    logger:debug("lazy_request received with data: ~p", [Data]),
    SocketPid = self(),
    spawn(fun() ->
        try
            logger:debug("lazy_request: fetching session state"),
            case gen_server:call(Pid, {get_state}, 5000) of
                SessionState when is_map(SessionState) ->
                    logger:debug("lazy_request: session state retrieved, calling unified subscriptions handler"),
                    guild_unified_subscriptions:handle_subscriptions(Data, SocketPid, SessionState);
                Other ->
                    logger:warning("lazy_request: unexpected session state: ~p", [Other])
            end
        catch
            Class:Reason:StackTrace ->
                logger:error("lazy_request: exception ~p:~p", [Class, Reason], #{stacktrace => StackTrace})
        end
    end),
    {ok, State};
handle_gateway_payload(_, _, State) ->
    close_with_reason(unknown_opcode, <<"Unknown opcode">>, State).

schedule_heartbeat_check() ->
    erlang:send_after(constants:heartbeat_interval() div 3, self(), {heartbeat_check}).

check_rate_limit(State = #state{rate_limit_state = RateLimitState}) ->
    Now = erlang:system_time(millisecond),
    Events = maps:get(events, RateLimitState, []),
    WindowStart = maps:get(window_start, RateLimitState, Now),

    WindowDuration = 60000,
    MaxEvents = 120,

    EventsInWindow = [T || T <- Events, (Now - T) < WindowDuration],
    EventsCount = length(EventsInWindow),

    case EventsCount >= MaxEvents of
        true ->
            rate_limited;
        false ->
            NewEvents = [Now | EventsInWindow],
            NewRateLimitState = #{
                events => NewEvents,
                window_start => WindowStart
            },
            {ok, State#state{rate_limit_state = NewRateLimitState}}
    end.

extract_client_ip(Req) ->
    case cowboy_req:header(<<"x-forwarded-for">>, Req) of
        undefined ->
            {PeerIP, _Port} = cowboy_req:peer(Req),
            list_to_binary(inet:ntoa(PeerIP));
        ForwardedFor ->
            case parse_forwarded_for(ForwardedFor) of
                <<>> ->
                    {PeerIP, _Port} = cowboy_req:peer(Req),
                    list_to_binary(inet:ntoa(PeerIP));
                IP ->
                    IP
            end
    end.

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

normalize_forwarded_ip(Value) ->
    Trimmed = string:trim(Value),
    case Trimmed of
        <<>> ->
            error;
        _ ->
            case Trimmed of
                <<"[", _/binary>> ->
                    case strip_ipv6_brackets(Trimmed) of
                        {ok, IPv6} ->
                            validate_ip(IPv6);
                        error ->
                            error
                    end;
                _ ->
                    Cleaned = strip_ipv4_port(Trimmed),
                    validate_ip(Cleaned)
            end
    end.

strip_ipv6_brackets(<<"[", Rest/binary>>) ->
    case binary:match(Rest, <<"]">>) of
        {Pos, _Len} when Pos > 0 ->
            {ok, binary:part(Rest, 0, Pos)};
        _ ->
            error
    end;
strip_ipv6_brackets(_) ->
    error.

strip_ipv4_port(IP) ->
    case binary:match(IP, <<".">>) of
        nomatch ->
            IP;
        _ ->
            case binary:split(IP, <<":">>, [global]) of
                [Addr, _Port] ->
                    Addr;
                _ ->
                    IP
            end
    end.

validate_ip(IP) ->
    case inet:parse_address(binary_to_list(IP)) of
        {ok, Parsed} ->
            {ok, list_to_binary(inet:ntoa(Parsed))};
        {error, _Reason} ->
            error
    end.

handle_resume_with_session(Pid, Token, SessionId, Seq, State) ->
    case gen_server:call(Pid, {token_verify, Token}, 5000) of
        true ->
            handle_resume_with_verified_token(Pid, SessionId, Seq, State);
        false ->
            handle_resume_invalid_token(SessionId, State)
    end.

handle_resume_with_verified_token(Pid, SessionId, Seq, State) ->
    SocketPid = self(),
    case gen_server:call(Pid, {resume, Seq, SocketPid}, 5000) of
        {ok, MissedEvents} when is_list(MissedEvents) ->
            handle_resume_success(Pid, SessionId, Seq, MissedEvents, State);
        invalid_seq ->
            handle_resume_invalid_seq(Seq, State)
    end.

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

    {ok, State#state{
        session_pid = Pid,
        heartbeat_state = #{
            last_ack => erlang:system_time(millisecond),
            waiting_for_ack => false
        }
    }}.

handle_resume_invalid_seq(_Seq, State) ->
    gateway_metrics_collector:inc_resume_failure(),
    close_with_reason(invalid_seq, <<"Invalid sequence">>, State).

handle_resume_invalid_token(_SessionId, State) ->
    gateway_metrics_collector:inc_resume_failure(),
    close_with_reason(authentication_failed, <<"Invalid token">>, State).

handle_resume_session_not_found(_SessionId, State) ->
    gateway_metrics_collector:inc_resume_failure(),
    Message = #{
        <<"op">> => constants:opcode_to_num(invalid_session),
        <<"d">> => false
    },
    case encode_and_compress(Message, State) of
        {ok, Frame, NewState} ->
            {[Frame], NewState};
        {error, _} ->
            {ok, State}
    end.

send_gateway_error(ErrorAtom, State) when is_atom(ErrorAtom) ->
    ErrorCode = gateway_errors:error_code(ErrorAtom),
    ErrorMessage = gateway_errors:error_message(ErrorAtom),
    Message = #{
        <<"op">> => constants:opcode_to_num(gateway_error),
        <<"d">> => #{
            <<"code">> => ErrorCode,
            <<"message">> => ErrorMessage
        }
    },
    case encode_and_compress(Message, State) of
        {ok, Frame, NewState} ->
            {[Frame], NewState};
        {error, _} ->
            {ok, State}
    end.

encode_and_compress(Message, State = #state{encoding = Encoding, compress_ctx = CompressCtx}) ->
    case gateway_codec:encode(Message, Encoding) of
        {ok, Encoded, FrameType} ->
            case gateway_compress:compress(Encoded, CompressCtx) of
                {ok, Compressed, NewCompressCtx} ->
                    Frame = make_frame(Compressed, FrameType, NewCompressCtx),
                    {ok, Frame, State#state{compress_ctx = NewCompressCtx}};
                {error, Reason} ->
                    {error, {compress_failed, gateway_compress:get_type(CompressCtx), Reason}}
            end;
        {error, Reason} ->
            {error, {encode_failed, Reason}}
    end.

compression_error_reason(zstd_stream) ->
    <<"Compression failed: zstd-stream">>;
compression_error_reason(_) ->
    <<"Encode failed">>.

close_with_reason(Reason, Message, State) ->
    gateway_metrics_collector:inc_websocket_close(Reason),
    CloseCode = constants:close_code_to_num(Reason),
    {[{close, CloseCode, Message}], State}.

make_frame(Data, FrameType, CompressCtx) ->
    case gateway_compress:get_type(CompressCtx) of
        none -> {FrameType, Data};
        _ -> {binary, Data}
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

-endif.
