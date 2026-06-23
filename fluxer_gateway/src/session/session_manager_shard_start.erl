%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_manager_shard_start).
-typing([eqwalizer]).

-export([
    build_and_start_session/7,
    build_and_start_session/8
]).

-export_type([session_id/0, session_ref/0, state/0, start_reply/0]).

-type session_id() :: session_manager_shard_lifecycle:session_id().
-type session_ref() :: session_manager_shard_lifecycle:session_ref().
-type state() :: session_manager_shard_lifecycle:state().
-type start_reply() :: session_manager_shard_lifecycle:start_reply().

-spec build_and_start_session(
    map(),
    map(),
    non_neg_integer(),
    pid(),
    session_id(),
    #{session_id() => session_ref()},
    state()
) ->
    {reply, start_reply(), state()}.
build_and_start_session(Data, IdentifyData, Version, SocketPid, SessionId, Sessions, State) ->
    build_and_start_session(
        Data,
        IdentifyData,
        Version,
        SocketPid,
        SessionId,
        Sessions,
        State,
        gateway_timings:new()
    ).

-spec build_and_start_session(
    map(),
    map(),
    non_neg_integer(),
    pid(),
    session_id(),
    #{session_id() => session_ref()},
    state(),
    gateway_timings:recorder()
) ->
    {reply, start_reply(), state()}.
build_and_start_session(
    Data, IdentifyData, Version, SocketPid, SessionId, Sessions, State, GwTimings0
) ->
    ValidateStartedAt = gateway_timings:start(),
    ValidateResult = validate_identify_payload(Data),
    GwTimings = gateway_timings:record_function(
        validate_identify_payload,
        <<"session_manager_shard_start:validate_identify_payload/1">>,
        ValidateStartedAt,
        GwTimings0
    ),
    case ValidateResult of
        {ok, UserDataMap, UserId, UserIdBin} ->
            try_eligible_session(
                #{
                    identify_data => IdentifyData,
                    version => Version,
                    socket_pid => SocketPid,
                    session_id => SessionId,
                    sessions => Sessions,
                    state => State,
                    data => Data,
                    user_data_map => UserDataMap,
                    user_id => UserId,
                    user_id_bin => UserIdBin,
                    gw_timings => GwTimings
                }
            );
        {error, Reason} ->
            {reply, {error, {invalid_identify_payload, Reason}}, State}
    end.

-spec try_eligible_session(map()) -> {reply, start_reply(), state()}.
try_eligible_session(#{user_id_bin := UserIdBin, gw_timings := GwTimings0} = Ctx) ->
    EligibleStartedAt = gateway_timings:start(),
    Eligible = gateway_rollout_config:is_session_eligible(UserIdBin),
    GwTimings = gateway_timings:record_function(
        check_session_rollout_eligibility,
        <<"gateway_rollout_config:is_session_eligible/1">>,
        EligibleStartedAt,
        GwTimings0
    ),
    case Eligible of
        false ->
            {reply, {error, not_eligible}, maps:get(state, Ctx)};
        true ->
            try_user_session_limit(Ctx#{gw_timings := GwTimings})
    end.

-spec try_user_session_limit(map()) -> {reply, start_reply(), state()}.
try_user_session_limit(Ctx) ->
    LimitStartedAt = gateway_timings:start(),
    {CheckResult, LimitFunctionName} = check_user_session_limit(Ctx),
    GwTimings = gateway_timings:record_function(
        check_user_session_limit,
        LimitFunctionName,
        LimitStartedAt,
        maps:get(gw_timings, Ctx)
    ),
    handle_user_session_limit_result(CheckResult, Ctx#{gw_timings := GwTimings}).

-spec check_user_session_limit(map()) -> {ok | {error, term()}, binary()}.
check_user_session_limit(
    #{
        user_data_map := UserDataMap,
        data := Data,
        identify_data := IdentifyData,
        user_id := UserId
    }
) ->
    case map_utils:get_safe(UserDataMap, <<"bot">>, false) of
        true ->
            {
                session_manager_shard_drain:validate_identify_sharding(
                    Data, IdentifyData, true
                ),
                <<"session_manager_shard_drain:validate_identify_sharding/3">>
            };
        _ ->
            {
                session_abuse_protection:check_user_session_limit(UserId),
                <<"session_abuse_protection:check_user_session_limit/1">>
            }
    end.

-spec handle_user_session_limit_result(ok | {error, term()}, map()) ->
    {reply, start_reply(), state()}.
handle_user_session_limit_result({error, too_many_sessions}, Ctx) ->
    {reply, {error, too_many_sessions}, maps:get(state, Ctx)};
handle_user_session_limit_result(ok, Ctx) ->
    UserId = maps:get(user_id, Ctx),
    session_abuse_protection:increment_user_sessions(UserId),
    BuildStartedAt = gateway_timings:start(),
    SessionData0 = session_manager_shard_drain:build_session_data(
        maps:get(data, Ctx),
        maps:get(identify_data, Ctx),
        maps:get(version, Ctx),
        maps:get(socket_pid, Ctx),
        maps:get(session_id, Ctx),
        maps:get(user_data_map, Ctx),
        UserId
    ),
    GwTimings = gateway_timings:record_function(
        build_session_data,
        <<"session_manager_shard_drain:build_session_data/7">>,
        BuildStartedAt,
        maps:get(gw_timings, Ctx)
    ),
    SessionData = SessionData0#{gw_timings => GwTimings},
    StartResult = session_manager_shard_lifecycle:start_session_process(
        SessionData,
        maps:get(session_id, Ctx),
        maps:get(sessions, Ctx),
        maps:get(state, Ctx)
    ),
    rollback_on_start_failure(StartResult, UserId).

-spec rollback_on_start_failure({reply, start_reply(), state()}, integer()) ->
    {reply, start_reply(), state()}.
rollback_on_start_failure({reply, {success, Pid}, _State} = Result, _UserId) when is_pid(Pid) ->
    Result;
rollback_on_start_failure({reply, {error, _Reason}, _State} = Result, UserId) ->
    session_abuse_protection:decrement_user_sessions(UserId),
    Result.

-spec validate_identify_payload(term()) ->
    {ok, map(), integer(), binary()} | {error, term()}.
validate_identify_payload(Data) when is_map(Data) ->
    case maps:get(<<"user">>, Data, undefined) of
        UserDataMap when is_map(UserDataMap) ->
            validate_identify_user_id(UserDataMap);
        _Other ->
            {error, missing_user}
    end;
validate_identify_payload(_Data) ->
    {error, invalid_payload}.

-spec validate_identify_user_id(map()) -> {ok, map(), integer(), binary()} | {error, term()}.
validate_identify_user_id(UserDataMap) ->
    case maps:get(<<"id">>, UserDataMap, undefined) of
        UserIdBin when is_binary(UserIdBin) ->
            parse_user_id(UserDataMap, UserIdBin);
        _Other ->
            {error, invalid_user_id}
    end.

-spec parse_user_id(map(), binary()) -> {ok, map(), integer(), binary()} | {error, term()}.
parse_user_id(UserDataMap, UserIdBin) ->
    try type_conv:extract_id(UserDataMap, <<"id">>) of
        UserId when is_integer(UserId) -> {ok, UserDataMap, UserId, UserIdBin};
        _Other -> {error, invalid_user_id}
    catch
        _Class:_Reason -> {error, invalid_user_id}
    end.
