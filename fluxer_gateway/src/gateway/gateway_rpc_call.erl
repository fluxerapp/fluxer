%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_rpc_call).
-typing([eqwalizer]).

-export([execute_method/2]).

-define(CALL_LOOKUP_TIMEOUT, 2000).
-define(CALL_CREATE_TIMEOUT, 10000).

-spec execute_method(binary(), map()) -> term().
execute_method(<<"call.get">>, #{<<"channel_id">> := ChannelIdBin}) ->
    handle_call_get(ChannelIdBin);
execute_method(<<"call.get_pending_joins">>, #{<<"channel_id">> := ChannelIdBin}) ->
    handle_call_get_pending_joins(ChannelIdBin);
execute_method(<<"call.create">>, Params) ->
    handle_call_create(Params);
execute_method(<<"call.update_region">>, #{
    <<"channel_id">> := ChannelIdBin, <<"region">> := Region
}) ->
    handle_call_update_region(ChannelIdBin, Region);
execute_method(
    <<"call.ring">>,
    #{<<"channel_id">> := ChannelIdBin, <<"recipients">> := RecipientsBin} = Params
) ->
    handle_call_ring(ChannelIdBin, RecipientsBin, Params);
execute_method(<<"call.stop_ringing">>, #{
    <<"channel_id">> := ChannelIdBin, <<"recipients">> := RecipientsBin
}) ->
    handle_call_stop_ringing(ChannelIdBin, RecipientsBin);
execute_method(Method, Params) ->
    execute_session_method(Method, Params).

-spec execute_session_method(binary(), map()) -> term().
execute_session_method(<<"call.join">>, #{
    <<"channel_id">> := ChannelIdBin,
    <<"user_id">> := UserIdBin,
    <<"session_id">> := SessionIdBin,
    <<"voice_state">> := VoiceState
}) ->
    handle_call_join(ChannelIdBin, UserIdBin, SessionIdBin, VoiceState);
execute_session_method(<<"call.leave">>, #{
    <<"channel_id">> := ChannelIdBin, <<"session_id">> := SessionId
}) ->
    handle_call_leave(ChannelIdBin, SessionId);
execute_session_method(<<"call.delete">>, #{<<"channel_id">> := ChannelIdBin}) ->
    handle_call_delete(ChannelIdBin);
execute_session_method(<<"call.confirm_connection">>, #{
    <<"channel_id">> := ChannelIdBin, <<"connection_id">> := ConnectionId
}) ->
    handle_call_confirm_connection(ChannelIdBin, ConnectionId);
execute_session_method(
    <<"call.disconnect_user_if_in_channel">>,
    #{<<"channel_id">> := ChannelIdBin, <<"user_id">> := UserIdBin} = Params
) ->
    handle_call_disconnect_user(ChannelIdBin, UserIdBin, Params).

-spec handle_call_get(binary()) -> term().
handle_call_get(ChannelIdBin) ->
    ChannelId = validation:snowflake_or_throw(<<"channel_id">>, ChannelIdBin),
    case gateway_rpc_call_lookup:lookup_call(ChannelId) of
        {ok, Pid} ->
            get_call_state(Pid);
        not_found ->
            null;
        {error, _Reason} ->
            gateway_rpc_error:raise(<<"call_lookup_error">>)
    end.

-spec get_call_state(pid()) -> term().
get_call_state(Pid) ->
    case gateway_rpc_call_lookup:safe_gen_server_call(Pid, {get_state}, ?CALL_LOOKUP_TIMEOUT) of
        {ok, {ok, CallData}} ->
            CallData;
        {error, not_found} ->
            null;
        _ ->
            gateway_rpc_error:raise(<<"call_state_error">>)
    end.

-spec handle_call_get_pending_joins(binary()) -> term().
handle_call_get_pending_joins(ChannelIdBin) ->
    ChannelId = validation:snowflake_or_throw(<<"channel_id">>, ChannelIdBin),
    case gateway_rpc_call_lookup:lookup_call(ChannelId) of
        {ok, Pid} ->
            get_pending_connections(Pid);
        not_found ->
            #{<<"pending_joins">> => []};
        {error, _Reason} ->
            gateway_rpc_error:raise(<<"call_lookup_error">>)
    end.

-spec get_pending_connections(pid()) -> map().
get_pending_connections(Pid) ->
    case
        gateway_rpc_call_lookup:safe_gen_server_call(
            Pid, {get_pending_connections}, ?CALL_LOOKUP_TIMEOUT
        )
    of
        {ok, #{pending_joins := PendingJoins}} ->
            #{<<"pending_joins">> => PendingJoins};
        {error, not_found} ->
            #{<<"pending_joins">> => []};
        _ ->
            gateway_rpc_error:raise(<<"call_pending_joins_error">>)
    end.

-spec handle_call_create(map()) -> term().
handle_call_create(Params) ->
    #{
        <<"channel_id">> := ChannelIdBin,
        <<"message_id">> := MessageIdBin,
        <<"region">> := Region,
        <<"ringing">> := RingingBins,
        <<"recipients">> := RecipientsBins
    } = Params,
    ChannelId = validation:snowflake_or_throw(<<"channel_id">>, ChannelIdBin),
    MessageId = validation:snowflake_or_throw(<<"message_id">>, MessageIdBin),
    Ringing = validation:snowflake_list_or_throw(<<"ringing">>, RingingBins),
    Recipients = validation:snowflake_list_or_throw(<<"recipients">>, RecipientsBins),
    CallData = maps:merge(
        #{
            channel_id => ChannelId,
            message_id => MessageId,
            region => Region,
            ringing => Ringing,
            recipients => Recipients
        },
        caller_params(Params)
    ),
    do_call_create(ChannelId, CallData).

-spec caller_params(map()) -> map().
caller_params(Params) ->
    #{
        caller_id => snowflake_id:parse_maybe(maps:get(<<"caller_id">>, Params, undefined)),
        caller_name => optional_binary(maps:get(<<"caller_name">>, Params, undefined)),
        caller_avatar => optional_binary(maps:get(<<"caller_avatar">>, Params, undefined))
    }.

-spec optional_binary(term()) -> binary() | undefined.
optional_binary(Value) when is_binary(Value), byte_size(Value) > 0 ->
    Value;
optional_binary(_Value) ->
    undefined.

-spec handle_call_update_region(binary(), binary()) -> term().
handle_call_update_region(ChannelIdBin, Region) ->
    ChannelId = validation:snowflake_or_throw(<<"channel_id">>, ChannelIdBin),
    gateway_rpc_call_lookup:with_call(ChannelId, fun(Pid) -> update_region(Pid, Region) end).

-spec update_region(pid(), binary()) -> true.
update_region(Pid, Region) ->
    case
        gateway_rpc_call_lookup:safe_gen_server_call(
            Pid, {update_region, Region}, ?CALL_LOOKUP_TIMEOUT
        )
    of
        {ok, ok} -> true;
        _ -> gateway_rpc_error:raise(<<"update_region_error">>)
    end.

-spec handle_call_ring(binary(), list(), map()) -> term().
handle_call_ring(ChannelIdBin, RecipientsBin, Params) ->
    ChannelId = validation:snowflake_or_throw(<<"channel_id">>, ChannelIdBin),
    Recipients = validation:snowflake_list_or_throw(<<"recipients">>, RecipientsBin),
    Caller = caller_params(Params),
    gateway_rpc_call_lookup:with_call(ChannelId, fun(Pid) ->
        ring_recipients(Pid, Recipients, Caller)
    end).

-spec ring_recipients(pid(), [integer()], map()) -> true.
ring_recipients(Pid, Recipients, Caller) ->
    ok = set_call_caller(Pid, Caller),
    case
        gateway_rpc_call_lookup:safe_gen_server_call(
            Pid, {ring_recipients, Recipients}, ?CALL_LOOKUP_TIMEOUT
        )
    of
        {ok, ok} -> true;
        _ -> gateway_rpc_error:raise(<<"ring_recipients_error">>)
    end.

-spec set_call_caller(pid(), map()) -> ok.
set_call_caller(_Pid, #{caller_id := undefined}) ->
    ok;
set_call_caller(Pid, Caller) ->
    gen_server:cast(Pid, {set_caller, Caller}),
    ok.

-spec handle_call_stop_ringing(binary(), list()) -> term().
handle_call_stop_ringing(ChannelIdBin, RecipientsBin) ->
    ChannelId = validation:snowflake_or_throw(<<"channel_id">>, ChannelIdBin),
    Recipients = validation:snowflake_list_or_throw(<<"recipients">>, RecipientsBin),
    gateway_rpc_call_lookup:with_call(ChannelId, fun(Pid) -> stop_ringing(Pid, Recipients) end).

-spec stop_ringing(pid(), [integer()]) -> true.
stop_ringing(Pid, Recipients) ->
    case
        gateway_rpc_call_lookup:safe_gen_server_call(
            Pid, {stop_ringing, Recipients}, ?CALL_LOOKUP_TIMEOUT
        )
    of
        {ok, ok} -> true;
        _ -> gateway_rpc_error:raise(<<"stop_ringing_error">>)
    end.

-spec handle_call_join(binary(), binary(), binary(), map()) -> term().
handle_call_join(ChannelIdBin, UserIdBin, SessionIdBin, VoiceState) ->
    ChannelId = validation:snowflake_or_throw(<<"channel_id">>, ChannelIdBin),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    SessionId = SessionIdBin,
    case session_manager:lookup(SessionId) of
        {ok, SessionPid} ->
            join_call(ChannelId, UserId, VoiceState, SessionId, SessionPid);
        {error, not_found} ->
            gateway_rpc_error:raise(<<"session_not_found">>)
    end.

-spec join_call(integer(), integer(), map(), binary(), pid()) -> true.
join_call(ChannelId, UserId, VoiceState, SessionId, SessionPid) ->
    case gateway_rpc_call_lookup:lookup_call(ChannelId) of
        {ok, CallPid} ->
            join_call_pid(CallPid, UserId, VoiceState, SessionId, SessionPid);
        not_found ->
            gateway_rpc_error:raise(<<"call_not_found">>);
        {error, _Reason} ->
            gateway_rpc_error:raise(<<"call_lookup_error">>)
    end.

-spec join_call_pid(pid(), integer(), map(), binary(), pid()) -> true.
join_call_pid(CallPid, UserId, VoiceState, SessionId, SessionPid) ->
    gen_server:cast(CallPid, {join_async, UserId, VoiceState, SessionId, SessionPid}),
    true.

-spec handle_call_leave(binary(), binary()) -> term().
handle_call_leave(ChannelIdBin, SessionId) ->
    ChannelId = validation:snowflake_or_throw(<<"channel_id">>, ChannelIdBin),
    gateway_rpc_call_lookup:with_call(ChannelId, fun(Pid) -> leave_call(Pid, SessionId) end).

-spec leave_call(pid(), binary()) -> true.
leave_call(Pid, SessionId) ->
    case
        gateway_rpc_call_lookup:safe_gen_server_call(
            Pid, {leave, SessionId}, ?CALL_LOOKUP_TIMEOUT
        )
    of
        {ok, ok} -> true;
        _ -> gateway_rpc_error:raise(<<"leave_call_error">>)
    end.

-spec handle_call_delete(binary()) -> term().
handle_call_delete(ChannelIdBin) ->
    ChannelId = validation:snowflake_or_throw(<<"channel_id">>, ChannelIdBin),
    case gateway_rpc_call_lookup:terminate_call_any(ChannelId) of
        ok ->
            true;
        {error, not_found} ->
            gateway_rpc_error:raise(<<"call_not_found">>);
        _ ->
            gateway_rpc_error:raise(<<"delete_call_error">>)
    end.

-spec handle_call_confirm_connection(binary(), binary()) -> term().
handle_call_confirm_connection(ChannelIdBin, ConnectionId) ->
    ChannelId = validation:snowflake_or_throw(<<"channel_id">>, ChannelIdBin),
    case gateway_rpc_call_lookup:lookup_call(ChannelId) of
        {ok, Pid} ->
            confirm_connection(Pid, ConnectionId);
        not_found ->
            #{success => true, call_not_found => true};
        {error, _Reason} ->
            gateway_rpc_error:raise(<<"call_lookup_error">>)
    end.

-spec confirm_connection(pid(), binary()) -> term().
confirm_connection(Pid, ConnectionId) ->
    case
        gateway_rpc_call_lookup:safe_gen_server_call(
            Pid, {confirm_connection, ConnectionId}, ?CALL_LOOKUP_TIMEOUT
        )
    of
        {ok, Response} ->
            Response;
        {error, not_found} ->
            #{success => true, call_not_found => true};
        _ ->
            gateway_rpc_error:raise(<<"confirm_connection_error">>)
    end.

-spec handle_call_disconnect_user(binary(), binary(), map()) -> term().
handle_call_disconnect_user(ChannelIdBin, UserIdBin, Params) ->
    ChannelId = validation:snowflake_or_throw(<<"channel_id">>, ChannelIdBin),
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    ConnectionId = maps:get(<<"connection_id">>, Params, undefined),
    do_disconnect_user_if_in_channel(ChannelId, UserId, ConnectionId).

-spec do_call_create(integer(), map()) -> term().
do_call_create(ChannelId, CallData) ->
    case gateway_rpc_call_lookup:lookup_call(ChannelId) of
        {ok, Pid} ->
            fetch_call_state_or_throw(Pid);
        not_found ->
            do_call_create_new(ChannelId, CallData);
        {error, _Reason} ->
            gateway_rpc_error:raise(<<"call_lookup_error">>)
    end.

-spec do_call_create_new(integer(), map()) -> term().
do_call_create_new(ChannelId, CallData) ->
    case
        gateway_rpc_call_lookup:call_owner_call_manager(
            ChannelId, {create, ChannelId, CallData}, ?CALL_CREATE_TIMEOUT
        )
    of
        {ok, Pid} when is_pid(Pid) ->
            fetch_call_state_or_throw(Pid);
        {error, already_exists} ->
            fetch_existing_call_state_or_throw(ChannelId);
        {error, Reason} ->
            logger:warning("Gateway RPC call create failed", #{
                channel_id => ChannelId, reason => Reason
            }),
            gateway_rpc_error:raise(<<"create_call_error">>);
        _ ->
            gateway_rpc_error:raise(<<"create_call_error">>)
    end.

-spec fetch_existing_call_state_or_throw(integer()) -> term().
fetch_existing_call_state_or_throw(ChannelId) ->
    case gateway_rpc_call_lookup:lookup_call(ChannelId) of
        {ok, Pid} -> fetch_call_state_or_throw(Pid);
        _ -> gateway_rpc_error:raise(<<"create_call_error">>)
    end.

-spec fetch_call_state_or_throw(pid()) -> term().
fetch_call_state_or_throw(Pid) ->
    case gateway_rpc_call_lookup:safe_gen_server_call(Pid, {get_state}, ?CALL_LOOKUP_TIMEOUT) of
        {ok, {ok, CallState}} -> CallState;
        _ -> gateway_rpc_error:raise(<<"call_state_error">>)
    end.

-spec do_disconnect_user_if_in_channel(integer(), integer(), binary() | undefined) -> term().
do_disconnect_user_if_in_channel(ChannelId, UserId, ConnectionId) ->
    case gateway_rpc_call_lookup:lookup_call(ChannelId) of
        {ok, Pid} ->
            call_disconnect_user(Pid, UserId, ChannelId, ConnectionId);
        not_found ->
            #{success => true, call_not_found => true};
        {error, _Reason} ->
            gateway_rpc_error:raise(<<"call_lookup_error">>)
    end.

-spec call_disconnect_user(pid(), integer(), integer(), binary() | undefined) -> term().
call_disconnect_user(Pid, UserId, ChannelId, ConnectionId) ->
    case
        gateway_rpc_call_lookup:safe_gen_server_call(
            Pid,
            {disconnect_user_if_in_channel, UserId, ChannelId, ConnectionId},
            ?CALL_LOOKUP_TIMEOUT
        )
    of
        {ok, Response} -> Response;
        {error, not_found} -> #{success => true, call_not_found => true};
        _ -> gateway_rpc_error:raise(<<"disconnect_user_error">>)
    end.
