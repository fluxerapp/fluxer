%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_handler).
-typing([eqwalizer]).
-behaviour(cowboy_websocket).

-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).
-export([new_state/0]).

-ifdef(TEST).
-export([parse_forwarded_for/1, parse_version/1]).
-endif.

-type state() :: #{
    version := 1 | undefined,
    encoding := gateway_codec:encoding(),
    compress_ctx := gateway_compress:compress_ctx() | undefined,
    session_pid := pid() | undefined,
    heartbeat_state := map(),
    heartbeat_timer := {reference(), reference()} | undefined,
    socket_pid := pid() | undefined,
    peer_ip := binary() | undefined,
    rate_limit_state := map(),
    request_guild_members_pid := pid() | undefined,
    request_guild_members_pending := map() | undefined,
    request_workers := map(),
    voice_queue_timer := reference() | undefined,
    pending_identify := {map(), pid()} | undefined,
    pending_identify_retry_timer := {reference(), reference()} | reference() | undefined,
    connection_acquired => boolean()
}.

-type ws_frame() :: {text, binary()} | {binary, binary()}.
-type ws_result() :: {ok, state()} | {[ws_frame() | {close, integer(), binary()}], state()}.

-export_type([state/0, ws_frame/0, ws_result/0]).

-spec new_state() -> state().
new_state() ->
    #{
        version => undefined,
        encoding => json,
        compress_ctx => undefined,
        session_pid => undefined,
        heartbeat_state => #{},
        heartbeat_timer => undefined,
        socket_pid => undefined,
        peer_ip => undefined,
        rate_limit_state => #{
            events => [],
            op_events => #{}
        },
        request_guild_members_pid => undefined,
        request_guild_members_pending => undefined,
        request_workers => #{},
        voice_queue_timer => undefined,
        pending_identify => undefined,
        pending_identify_retry_timer => undefined
    }.

-spec init(cowboy_req:req(), term()) -> {cowboy_websocket, cowboy_req:req(), state()}.
init(Req, _Opts) ->
    QS = cowboy_req:parse_qs(Req),
    Version = parse_version(proplists:get_value(<<"v">>, QS)),
    Encoding = gateway_codec:parse_encoding(proplists:get_value(<<"encoding">>, QS)),
    Compression = gateway_compress:parse_compression(
        proplists:get_value(<<"compress">>, QS),
        proplists:get_value(<<"stream">>, QS)
    ),
    CompressCtx = gateway_compress:new_context(Compression),
    PeerIPBinary = extract_client_ip(Req),
    State = new_state(),
    ReqWithVersion = gateway_build_info:set_version_header(Req),
    {cowboy_websocket, ReqWithVersion, State#{
        version => Version,
        encoding => Encoding,
        compress_ctx => CompressCtx,
        socket_pid => self(),
        peer_ip => PeerIPBinary
    }}.

-spec websocket_init(state()) -> ws_result().
websocket_init(#{version := 1} = State) ->
    case gateway_ip_connections:acquire(maps:get(peer_ip, State, undefined)) of
        ok ->
            do_websocket_init(State#{connection_acquired => true});
        {error, too_many_connections} ->
            gateway_handler_encode:close_with_reason(
                rate_limited, <<"Too many connections">>, State
            )
    end;
websocket_init(State) ->
    gateway_handler_encode:close_with_reason(
        invalid_api_version, <<"Invalid API version">>, State
    ).

-spec websocket_handle(
    {text, binary()} | {binary, binary()} | term(), state()
) -> ws_result().
websocket_handle({text, Text}, State) when is_binary(Text) ->
    handle_incoming_data(Text, State);
websocket_handle({binary, Binary}, State) when is_binary(Binary) ->
    handle_incoming_data(Binary, State);
websocket_handle(_, State) ->
    {ok, State}.

-spec websocket_info(term(), state()) -> ws_result().
websocket_info({heartbeat_check, Token}, State) when is_reference(Token) ->
    gateway_handler_heartbeat:handle_heartbeat_check(Token, State);
websocket_info({heartbeat_check}, State) ->
    gateway_handler_heartbeat:handle_legacy_heartbeat_check(State);
websocket_info({dispatch, Event, Data, Seq}, State) when
    is_integer(Seq), is_atom(Event), is_map(Data);
    is_integer(Seq), is_binary(Event), is_map(Data);
    is_integer(Seq), is_atom(Event), is_list(Data);
    is_integer(Seq), is_binary(Event), is_list(Data)
->
    gateway_handler_dispatch:handle_dispatch(Event, Data, Seq, State);
websocket_info({dispatch, Event, null, Seq}, State) when
    is_integer(Seq), is_atom(Event); is_integer(Seq), is_binary(Event)
->
    gateway_handler_dispatch:handle_dispatch(Event, null, Seq, State);
websocket_info({dispatch, Event, {pre_encoded, Bin} = Data, Seq}, State) when
    is_integer(Seq), is_atom(Event), is_binary(Bin);
    is_integer(Seq), is_binary(Event), is_binary(Bin)
->
    gateway_handler_dispatch:handle_dispatch(Event, Data, Seq, State);
websocket_info(rollout_config_changed, State) ->
    gateway_handler_identify:handle_rollout_config_changed(State);
websocket_info({retry_pending_identify, Token}, State) when is_reference(Token) ->
    gateway_handler_identify:handle_pending_identify_retry(Token, State);
websocket_info(retry_pending_identify, State) ->
    gateway_handler_identify:handle_pending_identify_retry(State);
websocket_info(session_reconnect, State) ->
    gateway_handler_dispatch:handle_session_reconnect(State);
websocket_info({'DOWN', _, process, Pid, _}, #{session_pid := Pid} = State) ->
    gateway_handler_dispatch:handle_session_down(State);
websocket_info({'DOWN', Ref, process, Pid, Reason}, State) when
    is_reference(Ref), is_pid(Pid)
->
    gateway_handler_dispatch:handle_request_worker_down(Ref, Pid, Reason, State);
websocket_info({gateway_request_worker_timeout, Ref, Type}, State) when is_reference(Ref) ->
    gateway_handler_dispatch:handle_request_worker_timeout(Ref, Type, State);
websocket_info({process_voice_queue}, State) ->
    QueueState = State#{voice_queue_timer => undefined},
    NewState = gateway_handler_voice:process_queued_voice_updates(QueueState),
    {ok, NewState};
websocket_info(_, State) ->
    {ok, State}.

-spec terminate(term(), cowboy_req:req(), state() | term()) -> ok.
terminate(_Reason, _Req, State) when is_map(State) ->
    terminate_with_state(eqwalizer:dynamic_cast(State));
terminate(_Reason, _Req, _State) ->
    ok.

-spec terminate_with_state(state()) -> ok.
terminate_with_state(#{compress_ctx := CompressCtx, session_pid := SessionPid} = State) ->
    gateway_rollout_config:unsubscribe_changes(self()),
    maybe_close_context(CompressCtx),
    gateway_handler_heartbeat:cancel_heartbeat_timer(State),
    gateway_handler_dispatch:cleanup_request_workers(State),
    cleanup_session_value(SessionPid),
    maybe_release_connection(State),
    ok;
terminate_with_state(_) ->
    ok.

-spec maybe_release_connection(state()) -> ok.
maybe_release_connection(#{connection_acquired := true} = State) ->
    gateway_ip_connections:note_disconnect(State);
maybe_release_connection(_) ->
    ok.

-spec maybe_close_context(gateway_compress:compress_ctx() | undefined) -> ok.
maybe_close_context(undefined) ->
    ok;
maybe_close_context(CompressCtx) ->
    gateway_compress:close_context(CompressCtx).

-spec cleanup_session_value(term()) -> ok.
cleanup_session_value(SessionPid) when is_pid(SessionPid); SessionPid =:= undefined ->
    gateway_handler_voice:cleanup_session(SessionPid);
cleanup_session_value(_SessionPid) ->
    ok.

-spec do_websocket_init(state()) -> ws_result().
do_websocket_init(State) ->
    CompressType = compression_type(maps:get(compress_ctx, State)),
    FreshCtx = gateway_compress:new_context(CompressType),
    FreshState = State#{compress_ctx => FreshCtx},
    HeartbeatInterval = constants:heartbeat_interval(),
    HelloMessage = #{
        <<"op">> => constants:opcode_to_num(hello),
        <<"d">> => #{<<"heartbeat_interval">> => HeartbeatInterval}
    },
    ReadyState0 = FreshState#{
        heartbeat_state => #{
            last_ack => erlang:system_time(millisecond),
            waiting_for_ack => false
        }
    },
    ReadyState = gateway_handler_heartbeat:schedule_heartbeat_check(ReadyState0),
    send_hello_or_close(HelloMessage, ReadyState, FreshState).

-spec send_hello_or_close(map(), state(), state()) -> ws_result().
send_hello_or_close(HelloMessage, ReadyState, FallbackState) ->
    case gateway_handler_encode:encode_and_compress(HelloMessage, ReadyState) of
        {ok, Frame, NewState} ->
            {[Frame], NewState};
        {error, {compress_failed, CT, _Reason}} when is_atom(CT) ->
            gateway_handler_encode:close_with_reason(
                decode_error,
                gateway_handler_encode:compression_error_reason(CT),
                FallbackState
            );
        {error, _Reason} ->
            gateway_handler_encode:close_with_reason(
                decode_error,
                <<"Encode failed">>,
                FallbackState
            )
    end.

-spec compression_type(gateway_compress:compress_ctx() | undefined) ->
    gateway_compress:compression().
compression_type(undefined) ->
    none;
compression_type(CompressCtx) ->
    gateway_compress:get_type(CompressCtx).

-spec resolve_compress_ctx(gateway_compress:compress_ctx() | undefined) ->
    gateway_compress:compress_ctx().
resolve_compress_ctx(undefined) ->
    gateway_compress:new_context(none);
resolve_compress_ctx(CompressCtx) ->
    CompressCtx.

-spec handle_incoming_data(binary(), state()) -> ws_result().
handle_incoming_data(Data, #{encoding := Encoding, compress_ctx := CompressCtx0} = State) ->
    CompressCtx = resolve_compress_ctx(CompressCtx0),
    case byte_size(Data) =< constants:max_payload_size() of
        true ->
            handle_decompressed_incoming_data(Data, Encoding, CompressCtx, State);
        false ->
            gateway_handler_encode:close_with_reason(
                decode_error, <<"Payload too large">>, State
            )
    end.

-spec handle_decompressed_incoming_data(
    binary(), gateway_codec:encoding(), gateway_compress:compress_ctx(), state()
) ->
    ws_result().
handle_decompressed_incoming_data(Data, Encoding, CompressCtx, State) ->
    MaxPayloadSize = constants:max_payload_size(),
    case gateway_compress:decompress(Data, CompressCtx) of
        {ok, Decompressed, NewCompressCtx} when byte_size(Decompressed) =< MaxPayloadSize ->
            Decoded = gateway_codec:decode(Decompressed, Encoding),
            handle_decode(Decoded, State#{compress_ctx => NewCompressCtx});
        {ok, _Decompressed, _NewCompressCtx} ->
            gateway_handler_encode:close_with_reason(
                decode_error, <<"Payload too large">>, State
            );
        {error, _Reason} ->
            gateway_handler_encode:close_with_reason(
                decode_error, <<"Decompression failed">>, State
            )
    end.

-spec handle_decode({ok, map()} | {error, term()}, state()) -> ws_result().
handle_decode({ok, #{<<"op">> := Op} = Payload}, State) ->
    OpAtom = constants:gateway_opcode(Op),
    RateLimitResult = gateway_handler_rate_limit:check_rate_limit(State, OpAtom),
    dispatch_after_rate_limit(RateLimitResult, OpAtom, Payload);
handle_decode({ok, _}, State) ->
    gateway_handler_encode:close_with_reason(decode_error, <<"Invalid payload">>, State);
handle_decode({error, _Reason}, State) ->
    gateway_handler_encode:close_with_reason(decode_error, <<"Decode failed">>, State).

-spec dispatch_after_rate_limit(
    {ok, state()} | {rate_limited, state()} | {opcode_rate_limited, state()},
    atom(),
    map()
) -> ws_result().
dispatch_after_rate_limit({ok, RLState}, OpAtom, Payload) ->
    gateway_handler_dispatch:handle_opcode(OpAtom, Payload, RLState);
dispatch_after_rate_limit({opcode_rate_limited, RLState}, _OpAtom, _Payload) ->
    {ok, RLState};
dispatch_after_rate_limit({rate_limited, RLState}, _OpAtom, _Payload) ->
    gateway_handler_encode:close_with_reason(rate_limited, <<"Rate limited">>, RLState).

-spec extract_client_ip(cowboy_req:req()) -> binary().
extract_client_ip(Req) ->
    case fluxer_gateway_env:get(trust_client_ip_header) of
        true -> extract_trusted_client_ip(Req);
        _ -> peer_ip_to_binary(cowboy_req:peer(Req))
    end.

-spec extract_trusted_client_ip(cowboy_req:req()) -> binary().
extract_trusted_client_ip(Req) ->
    ClientIpHeader = client_ip_header(),
    case cowboy_req:header(ClientIpHeader, Req) of
        undefined -> peer_ip_to_binary(cowboy_req:peer(Req));
        ForwardedFor -> extract_from_forwarded_for(ForwardedFor, Req)
    end.

-spec extract_from_forwarded_for(binary(), cowboy_req:req()) -> binary().
extract_from_forwarded_for(ForwardedFor, Req) ->
    case parse_forwarded_for(ForwardedFor) of
        <<>> -> peer_ip_to_binary(cowboy_req:peer(Req));
        IP -> IP
    end.

-spec peer_ip_to_binary({inet:ip_address(), inet:port_number()}) -> binary().
peer_ip_to_binary({PeerIP, _Port}) ->
    ip_address_to_binary(PeerIP).

-spec parse_forwarded_for(binary()) -> binary().
parse_forwarded_for(HeaderValue) ->
    case binary:split(HeaderValue, <<",">>) of
        [First | _] -> normalize_first_forwarded_for(First);
        [] -> <<>>
    end.

-spec normalize_first_forwarded_for(binary()) -> binary().
normalize_first_forwarded_for(First) ->
    case normalize_forwarded_ip(First) of
        {ok, IP} -> IP;
        error -> <<>>
    end.

-spec normalize_forwarded_ip(binary()) -> {ok, binary()} | error.
normalize_forwarded_ip(Value) ->
    case string:trim(Value) of
        <<>> -> error;
        <<"[", _/binary>> = Trimmed -> normalize_bracketed_ipv6(Trimmed);
        Trimmed -> validate_ip(strip_ipv4_port(Trimmed))
    end.

-spec normalize_bracketed_ipv6(binary()) -> {ok, binary()} | error.
normalize_bracketed_ipv6(Trimmed) ->
    case strip_ipv6_brackets(Trimmed) of
        {ok, IPv6} -> validate_ip(IPv6);
        error -> error
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
        nomatch -> IP;
        _ -> strip_port_from_dotted(IP)
    end.

-spec strip_port_from_dotted(binary()) -> binary().
strip_port_from_dotted(IP) ->
    case binary:split(IP, <<":">>, [global]) of
        [Addr, _Port] -> Addr;
        _ -> IP
    end.

-spec validate_ip(binary()) -> {ok, binary()} | error.
validate_ip(IP) ->
    case inet:parse_address(binary_to_list(IP)) of
        {ok, Parsed} -> {ok, ip_address_to_binary(Parsed)};
        {error, _} -> error
    end.

-spec parse_version(binary() | undefined) -> 1 | undefined.
parse_version(<<"1">>) -> 1;
parse_version(_) -> undefined.

-spec client_ip_header() -> binary().
client_ip_header() ->
    case fluxer_gateway_env:get(client_ip_header) of
        Header when is_binary(Header) -> Header;
        Header when is_atom(Header) -> atom_to_binary(Header, utf8);
        Header when is_list(Header) -> list_to_binary_safe(Header);
        _ -> <<"x-forwarded-for">>
    end.

-spec list_to_binary_safe([term()]) -> binary().
list_to_binary_safe(List) ->
    try
        Chars = lists:map(fun ensure_byte/1, List),
        list_to_binary(Chars)
    catch
        _:_ -> <<"x-forwarded-for">>
    end.

-spec ensure_byte(term()) -> byte().
ensure_byte(C) when is_integer(C), C >= 0, C =< 255 -> C;
ensure_byte(_) -> error(badarg).

-spec ip_address_to_binary(inet:ip_address()) -> binary().
ip_address_to_binary(IP) ->
    case inet:ntoa(IP) of
        Address when is_list(Address) -> list_to_binary(Address);
        {error, _Reason} -> <<>>
    end.
