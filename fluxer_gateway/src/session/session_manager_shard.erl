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

-module(session_manager_shard).
-behaviour(gen_server).

-include_lib("fluxer_gateway/include/timeout_config.hrl").

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export_type([session_data/0, user_id/0]).

-define(IDENTIFY_FLAG_DEBOUNCE_MESSAGE_REACTIONS, 16#2).

-type session_id() :: binary().
-type user_id() :: integer().
-type session_ref() :: {pid(), reference()}.
-type status() :: online | offline | idle | dnd.
-type identify_timestamp() :: integer().

-type identify_request() :: #{
    session_id := session_id(),
    identify_data := map(),
    version := non_neg_integer(),
    peer_ip := term(),
    token := binary()
}.

-type session_data() :: #{
    id := session_id(),
    user_id := user_id(),
    user_data := map(),
    version := non_neg_integer(),
    token_hash := binary(),
    auth_session_id_hash := binary(),
    properties := map(),
    status := status(),
    afk := boolean(),
    mobile := boolean(),
    socket_pid := pid(),
    guilds := [integer()],
    ready := map(),
    ignored_events := [binary()]
}.

-type state() :: #{
    sessions := #{session_id() => session_ref()},
    identify_attempts := [identify_timestamp()],
    pending_identifies := #{session_id() => pending_identify()},
    identify_workers := #{reference() => session_id()},
    shard_index := non_neg_integer()
}.

-type pending_identify() :: #{
    request := identify_request(),
    socket_pid := pid(),
    froms := [gen_server:from()]
}.

-type start_reply() ::
    {success, pid()}
    | {error, invalid_token}
    | {error, rate_limited}
    | {error, identify_rate_limited}
    | {error, {server_error, non_neg_integer()}}
    | {error, {rpc_error, non_neg_integer(), binary()}}
    | {error, {network_error, term()}}
    | {error, registration_failed}
    | {error, term()}.

-type lookup_reply() :: {ok, pid()} | {error, not_found}.

-spec start_link(non_neg_integer()) -> {ok, pid()} | {error, term()}.
start_link(ShardIndex) ->
    gen_server:start_link(?MODULE, #{shard_index => ShardIndex}, []).

-spec init(map()) -> {ok, state()}.
init(Args) ->
    fluxer_gateway_env:load(),
    process_flag(trap_exit, true),
    ShardIndex = maps:get(shard_index, Args, 0),
    {ok, #{
        sessions => #{},
        identify_attempts => [],
        pending_identifies => #{},
        identify_workers => #{},
        shard_index => ShardIndex
    }}.

-spec handle_call(Request, From, State) -> Result when
    Request ::
        {start, identify_request(), pid()}
        | {lookup, session_id()}
        | get_local_count
        | get_global_count
        | term(),
    From :: gen_server:from(),
    State :: state(),
    Result :: {reply, Reply, state()} | {noreply, state()},
    Reply :: start_reply() | lookup_reply() | {ok, non_neg_integer()} | ok.
handle_call({start, Request, SocketPid}, From, State) ->
    Sessions = maps:get(sessions, State),
    Attempts = maps:get(identify_attempts, State),
    PendingIdentifies = maps:get(pending_identifies, State),
    SessionId = maps:get(session_id, Request),
    case maps:get(SessionId, Sessions, undefined) of
        {Pid, _Ref} ->
            {reply, {success, Pid}, State};
        undefined ->
            SessionName = process_registry:build_process_name(session, SessionId),
            case whereis(SessionName) of
                undefined ->
                    case maps:get(SessionId, PendingIdentifies, undefined) of
                        undefined ->
                            case check_identify_rate_limit(Attempts) of
                                {ok, NewAttempts} ->
                                    NewState = maps:put(identify_attempts, NewAttempts, State),
                                    start_identify_fetch(Request, SocketPid, SessionId, From, NewState);
                                {error, rate_limited} ->
                                    {reply, {error, identify_rate_limited}, State}
                            end;
                        PendingIdentify ->
                            UpdatedPending = PendingIdentify#{
                                froms => [From | maps:get(froms, PendingIdentify, [])]
                            },
                            NewPending = maps:put(SessionId, UpdatedPending, PendingIdentifies),
                            {noreply, maps:put(pending_identifies, NewPending, State)}
                    end;
                Pid ->
                    Ref = monitor(process, Pid),
                    NewSessions = maps:put(SessionId, {Pid, Ref}, Sessions),
                    {reply, {success, Pid}, maps:put(sessions, NewSessions, State)}
            end
    end;
handle_call({lookup, SessionId}, _From, State) when is_binary(SessionId) ->
    Sessions = maps:get(sessions, State),
    case maps:get(SessionId, Sessions, undefined) of
        {Pid, _Ref} ->
            {reply, {ok, Pid}, State};
        undefined ->
            SessionName = process_registry:build_process_name(session, SessionId),
            case whereis(SessionName) of
                undefined ->
                    {reply, {error, not_found}, State};
                Pid ->
                    Ref = monitor(process, Pid),
                    NewSessions = maps:put(SessionId, {Pid, Ref}, Sessions),
                    {reply, {ok, Pid}, maps:put(sessions, NewSessions, State)}
            end
    end;
handle_call({lookup, _InvalidSessionId}, _From, State) ->
    {reply, {error, not_found}, State};
handle_call(get_local_count, _From, State) ->
    Sessions = maps:get(sessions, State),
    {reply, {ok, maps:size(Sessions)}, State};
handle_call(get_global_count, _From, State) ->
    Sessions = maps:get(sessions, State),
    {reply, {ok, maps:size(Sessions)}, State};
handle_call(_, _From, State) ->
    {reply, ok, State}.

-spec build_and_start_session(
    map(), map(), non_neg_integer(), pid(), session_id(), #{session_id() => session_ref()}, state()
) ->
    {reply, start_reply(), state()}.
build_and_start_session(Data, IdentifyData, Version, SocketPid, SessionId, Sessions, State) ->
    UserDataMap = maps:get(<<"user">>, Data),
    UserId = type_conv:extract_id(UserDataMap, <<"id">>),
    AuthSessionIdHashEncoded = maps:get(<<"auth_session_id_hash">>, Data, undefined),
    AuthSessionIdHash =
        case AuthSessionIdHashEncoded of
            undefined -> <<>>;
            null -> <<>>;
            _ -> base64url:decode(AuthSessionIdHashEncoded)
        end,
    Status = parse_presence(Data, IdentifyData),
    GuildIds = parse_guild_ids(Data),
    Properties = maps:get(properties, IdentifyData),
    Presence = map_utils:get_safe(IdentifyData, presence, null),
    IgnoredEvents = map_utils:get_safe(IdentifyData, ignored_events, []),
    InitialGuildId = map_utils:get_safe(IdentifyData, initial_guild_id, undefined),
    Bot = map_utils:get_safe(UserDataMap, <<"bot">>, false),
    ReadyData =
        case Bot of
            true -> maps:merge(Data, #{<<"guilds">> => []});
            false -> Data
        end,
    UserSettingsMap = map_utils:get_safe(Data, <<"user_settings">>, #{}),
    CustomStatusFromSettings = map_utils:get_safe(UserSettingsMap, <<"custom_status">>, null),
    PresenceCustomStatus = get_presence_custom_status(Presence),
    CustomStatus =
        case CustomStatusFromSettings of
            null -> PresenceCustomStatus;
            _ -> CustomStatusFromSettings
        end,
    Mobile =
        case Presence of
            null -> map_utils:get_safe(Properties, <<"mobile">>, false);
            P when is_map(P) -> map_utils:get_safe(P, <<"mobile">>, false);
            _ -> false
        end,
    Afk =
        case Presence of
            null -> false;
            P2 when is_map(P2) -> map_utils:get_safe(P2, <<"afk">>, false);
            _ -> false
        end,
    UserData0 = #{
        <<"id">> => maps:get(<<"id">>, UserDataMap),
        <<"username">> => maps:get(<<"username">>, UserDataMap),
        <<"discriminator">> => maps:get(<<"discriminator">>, UserDataMap),
        <<"avatar">> => maps:get(<<"avatar">>, UserDataMap),
        <<"avatar_color">> => map_utils:get_safe(UserDataMap, <<"avatar_color">>, undefined),
        <<"bot">> => map_utils:get_safe(UserDataMap, <<"bot">>, undefined),
        <<"system">> => map_utils:get_safe(UserDataMap, <<"system">>, undefined),
        <<"flags">> => maps:get(<<"flags">>, UserDataMap)
    },
    NormalizedUserData = user_utils:normalize_user(UserData0),
    UserData = maps:put(
        <<"is_staff">>,
        maps:get(<<"is_staff">>, UserDataMap, false),
        NormalizedUserData
    ),
    DebounceReactions = should_debounce_reactions(IdentifyData),
    SessionData = #{
        id => SessionId,
        user_id => UserId,
        user_data => UserData,
        custom_status => CustomStatus,
        version => Version,
        token_hash => utils:hash_token(maps:get(token, IdentifyData)),
        auth_session_id_hash => AuthSessionIdHash,
        properties => Properties,
        status => Status,
        afk => Afk,
        mobile => Mobile,
        socket_pid => SocketPid,
        guilds => GuildIds,
        ready => ReadyData,
        bot => Bot,
        ignored_events => IgnoredEvents,
        initial_guild_id => InitialGuildId,
        debounce_reactions => DebounceReactions
    },
    start_session_process(SessionData, SessionId, Sessions, State).

-spec start_session_process(map(), session_id(), #{session_id() => session_ref()}, state()) ->
    {reply, start_reply(), state()}.
start_session_process(SessionData, SessionId, Sessions, State) ->
    SessionName = process_registry:build_process_name(session, SessionId),
    case whereis(SessionName) of
        undefined ->
            case session:start_link(SessionData) of
                {ok, Pid} ->
                    case process_registry:register_and_monitor(SessionName, Pid, Sessions) of
                        {ok, RegisteredPid, Ref, NewSessions0} ->
                            CleanSessions = maps:remove(SessionName, NewSessions0),
                            NewSessions = maps:put(SessionId, {RegisteredPid, Ref}, CleanSessions),
                            {reply, {success, RegisteredPid},
                                maps:put(sessions, NewSessions, State)};
                        {error, registration_race_condition} ->
                            {reply, {error, registration_failed}, State};
                        {error, _Reason} ->
                            {reply, {error, registration_failed}, State}
                    end;
                Error ->
                    {reply, Error, State}
            end;
        ExistingPid ->
            Ref = monitor(process, ExistingPid),
            CleanSessions = maps:remove(SessionName, Sessions),
            NewSessions = maps:put(SessionId, {ExistingPid, Ref}, CleanSessions),
            {reply, {success, ExistingPid}, maps:put(sessions, NewSessions, State)}
    end.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_, State) ->
    {noreply, State}.

-spec start_identify_fetch(identify_request(), pid(), session_id(), gen_server:from(), state()) ->
    {noreply, state()}.
start_identify_fetch(Request, SocketPid, SessionId, From, State) ->
    ManagerPid = self(),
    {_WorkerPid, WorkerRef} =
        spawn_monitor(fun() ->
            PeerIP = maps:get(peer_ip, Request),
            FetchResult = fetch_rpc_data(Request, PeerIP),
            ManagerPid ! {identify_fetch_result, SessionId, FetchResult}
        end),
    PendingIdentifies = maps:get(pending_identifies, State),
    NewPending = maps:put(SessionId, #{
        request => Request,
        socket_pid => SocketPid,
        froms => [From]
    }, PendingIdentifies),
    IdentifyWorkers = maps:get(identify_workers, State),
    NewWorkers = maps:put(WorkerRef, SessionId, IdentifyWorkers),
    {noreply, State#{
        pending_identifies := NewPending,
        identify_workers := NewWorkers
    }}.

-spec should_debounce_reactions(map()) -> boolean().
should_debounce_reactions(IdentifyData) ->
    case map_utils:get_safe(IdentifyData, flags, 0) of
        Flags when is_integer(Flags), Flags >= 0 ->
            (Flags band ?IDENTIFY_FLAG_DEBOUNCE_MESSAGE_REACTIONS) =/= 0;
        _ ->
            false
    end.

-spec handle_info(Info, State) -> {noreply, state()} when
    Info :: {'DOWN', reference(), process, pid(), term()} | term(),
    State :: state().
handle_info({identify_fetch_result, SessionId, FetchResult}, State) ->
    complete_identify_fetch(SessionId, FetchResult, State);
handle_info({'DOWN', Ref, process, Pid, Reason}, State) ->
    IdentifyWorkers = maps:get(identify_workers, State),
    case maps:take(Ref, IdentifyWorkers) of
        {SessionId, RemainingWorkers} ->
            StateWithoutWorker = maps:put(identify_workers, RemainingWorkers, State),
            maybe_fail_pending_identify(SessionId, Reason, StateWithoutWorker);
        error ->
            Sessions = maps:get(sessions, State),
            NewSessions = process_registry:cleanup_on_down(Pid, Sessions),
            {noreply, maps:put(sessions, NewSessions, State)}
    end;
handle_info(_, State) ->
    {noreply, State}.

-spec complete_identify_fetch(session_id(), term(), state()) -> {noreply, state()}.
complete_identify_fetch(SessionId, FetchResult, State) ->
    PendingIdentifies = maps:get(pending_identifies, State),
    case maps:take(SessionId, PendingIdentifies) of
        error ->
            {noreply, State};
        {PendingIdentify, RemainingPending} ->
            State1 = maps:put(pending_identifies, RemainingPending, State),
            State2 = cleanup_identify_worker(SessionId, State1),
            {Reply, NewState} = resolve_identify_result(FetchResult, PendingIdentify, SessionId, State2),
            reply_to_waiters(maps:get(froms, PendingIdentify, []), Reply),
            {noreply, NewState}
    end.

-spec maybe_fail_pending_identify(session_id(), term(), state()) -> {noreply, state()}.
maybe_fail_pending_identify(_SessionId, Reason, State) when
    Reason =:= normal; Reason =:= shutdown
->
    {noreply, State};
maybe_fail_pending_identify(SessionId, Reason, State) ->
    PendingIdentifies = maps:get(pending_identifies, State),
    case maps:take(SessionId, PendingIdentifies) of
        error ->
            {noreply, State};
        {PendingIdentify, RemainingPending} ->
            reply_to_waiters(
                maps:get(froms, PendingIdentify, []),
                {error, {network_error, Reason}}
            ),
            NewState = maps:put(pending_identifies, RemainingPending, State),
            {noreply, NewState}
    end.

-spec cleanup_identify_worker(session_id(), state()) -> state().
cleanup_identify_worker(SessionId, State) ->
    IdentifyWorkers = maps:get(identify_workers, State),
    RemainingWorkers = maps:filter(
        fun(_Ref, WorkerSessionId) -> WorkerSessionId =/= SessionId end,
        IdentifyWorkers
    ),
    maps:put(identify_workers, RemainingWorkers, State).

-spec resolve_identify_result(term(), pending_identify(), session_id(), state()) ->
    {start_reply(), state()}.
resolve_identify_result({ok, Data}, PendingIdentify, SessionId, State) ->
    Request = maps:get(request, PendingIdentify),
    IdentifyData = maps:get(identify_data, Request),
    Version = maps:get(version, Request),
    SocketPid = maps:get(socket_pid, PendingIdentify),
    Sessions = maps:get(sessions, State),
    {reply, Reply, NewState} = build_and_start_session(
        Data,
        IdentifyData,
        Version,
        SocketPid,
        SessionId,
        Sessions,
        State
    ),
    {Reply, NewState};
resolve_identify_result({error, invalid_token}, _PendingIdentify, _SessionId, State) ->
    {{error, invalid_token}, State};
resolve_identify_result({error, rate_limited}, _PendingIdentify, _SessionId, State) ->
    {{error, rate_limited}, State};
resolve_identify_result({error, Reason}, _PendingIdentify, _SessionId, State) ->
    {{error, Reason}, State}.

-spec reply_to_waiters([gen_server:from()], start_reply()) -> ok.
reply_to_waiters(Waiters, Reply) ->
    lists:foreach(fun(From) -> gen_server:reply(From, Reply) end, Waiters),
    ok.

-spec terminate(Reason, State) -> ok when
    Reason :: term(),
    State :: state().
terminate(_Reason, _State) ->
    ok.

-spec code_change(OldVsn, State, Extra) -> {ok, state()} when
    OldVsn :: term(),
    State :: state() | tuple(),
    Extra :: term().
code_change(_OldVsn, State, _Extra) when is_map(State) ->
    {ok, State#{
        pending_identifies => maps:get(pending_identifies, State, #{}),
        identify_workers => maps:get(identify_workers, State, #{}),
        shard_index => maps:get(shard_index, State, 0)
    }};
code_change(_OldVsn, State, _Extra) when is_tuple(State), element(1, State) =:= state ->
    Sessions = element(2, State),
    IdentifyAttempts = element(5, State),
    {ok, #{
        sessions => Sessions,
        identify_attempts => IdentifyAttempts,
        pending_identifies => #{},
        identify_workers => #{},
        shard_index => 0
    }};
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec fetch_rpc_data(map(), term()) ->
    {ok, map()}
    | {error, invalid_token}
    | {error, rate_limited}
    | {error, {server_error, non_neg_integer()}}
    | {error, {network_error, term()}}.
fetch_rpc_data(Request, PeerIP) ->
    StartTime = erlang:system_time(millisecond),
    Result = do_fetch_rpc_data(Request, PeerIP),
    EndTime = erlang:system_time(millisecond),
    LatencyMs = EndTime - StartTime,
    gateway_metrics_collector:record_rpc_latency(LatencyMs),
    Result.

-spec do_fetch_rpc_data(map(), term()) ->
    {ok, map()}
    | {error, invalid_token}
    | {error, rate_limited}
    | {error, {server_error, non_neg_integer()}}
    | {error, {network_error, term()}}.
do_fetch_rpc_data(Request, PeerIP) ->
    IdentifyData = maps:get(identify_data, Request),
    Properties = map_utils:get_safe(IdentifyData, properties, #{}),
    LatitudeRaw = map_utils:get_safe(Properties, <<"latitude">>, undefined),
    LongitudeRaw = map_utils:get_safe(Properties, <<"longitude">>, undefined),
    Latitude = normalize_coordinate(LatitudeRaw),
    Longitude = normalize_coordinate(LongitudeRaw),
    RpcRequest = #{
        <<"type">> => <<"session">>,
        <<"token">> => maps:get(token, IdentifyData),
        <<"version">> => maps:get(version, Request),
        <<"ip">> => PeerIP
    },
    RpcRequestWithCoords = add_coordinates(RpcRequest, Latitude, Longitude),
    case rpc_client:call(RpcRequestWithCoords) of
        {ok, Data} ->
            {ok, Data};
        {error, {rpc_error, 401, _}} ->
            {error, invalid_token};
        {error, {rpc_error, 429, _}} ->
            {error, rate_limited};
        {error, {rpc_error, StatusCode, _}} when StatusCode >= 500 ->
            {error, {server_error, StatusCode}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

-spec normalize_coordinate(term()) -> term() | undefined.
normalize_coordinate(undefined) -> undefined;
normalize_coordinate(null) -> undefined;
normalize_coordinate(Value) -> Value.

-spec add_coordinates(map(), term(), term()) -> map().
add_coordinates(Request, undefined, undefined) ->
    Request;
add_coordinates(Request, Lat, undefined) ->
    maps:put(<<"latitude">>, Lat, Request);
add_coordinates(Request, undefined, Lon) ->
    maps:put(<<"longitude">>, Lon, Request);
add_coordinates(Request, Lat, Lon) ->
    maps:merge(Request, #{<<"latitude">> => Lat, <<"longitude">> => Lon}).


-spec parse_presence(map(), map()) -> status().
parse_presence(Data, IdentifyData) ->
    StoredStatus = get_stored_status(Data),
    PresenceStatus =
        case map_utils:get_safe(IdentifyData, presence, null) of
            null ->
                undefined;
            Presence when is_map(Presence) ->
                map_utils:get_safe(Presence, status, <<"online">>);
            _ ->
                undefined
        end,
    SelectedStatus = select_initial_status(PresenceStatus, StoredStatus),
    utils:parse_status(SelectedStatus).

-spec parse_guild_ids(map()) -> [integer()].
parse_guild_ids(Data) ->
    GuildIds = map_utils:get_safe(Data, <<"guild_ids">>, []),
    [utils:binary_to_integer_safe(Id) || Id <- GuildIds, Id =/= undefined].

-spec check_identify_rate_limit(list()) -> {ok, list()} | {error, rate_limited}.
check_identify_rate_limit(Attempts) ->
    case fluxer_gateway_env:get(identify_rate_limit_enabled) of
        true ->
            Now = erlang:system_time(millisecond),
            WindowDuration = 5000,
            AttemptsInWindow = [T || T <- Attempts, (Now - T) < WindowDuration],
            AttemptsCount = length(AttemptsInWindow),
            MaxIdentifiesPerWindow = 1,
            case AttemptsCount >= MaxIdentifiesPerWindow of
                true ->
                    {error, rate_limited};
                false ->
                    NewAttempts = [Now | AttemptsInWindow],
                    {ok, NewAttempts}
            end;
        _ ->
            {ok, Attempts}
    end.

-spec get_presence_custom_status(term()) -> map() | null.
get_presence_custom_status(Presence) ->
    case Presence of
        null -> null;
        Map when is_map(Map) -> map_utils:get_safe(Map, <<"custom_status">>, null);
        _ -> null
    end.

-spec get_stored_status(map()) -> binary().
get_stored_status(Data) ->
    case map_utils:get_safe(Data, <<"user_settings">>, null) of
        null ->
            <<"online">>;
        UserSettings ->
            case normalize_status(map_utils:get_safe(UserSettings, <<"status">>, <<"online">>)) of
                undefined -> <<"online">>;
                Value -> Value
            end
    end.

-spec select_initial_status(binary() | undefined, binary()) -> binary().
select_initial_status(PresenceStatus, StoredStatus) ->
    NormalizedPresence = normalize_status(PresenceStatus),
    case {NormalizedPresence, StoredStatus} of
        {undefined, Stored} ->
            Stored;
        {<<"unknown">>, Stored} ->
            Stored;
        {<<"online">>, Stored} when Stored =/= <<"online">> ->
            Stored;
        {Presence, _} ->
            Presence
    end.

-spec normalize_status(term()) -> binary() | undefined.
normalize_status(undefined) ->
    undefined;
normalize_status(null) ->
    undefined;
normalize_status(Status) when is_binary(Status) ->
    Status;
normalize_status(Status) when is_atom(Status) ->
    try constants:status_type_atom(Status) of
        Value when is_binary(Value) -> Value
    catch
        _:_ -> undefined
    end;
normalize_status(_) ->
    undefined.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

normalize_status_test() ->
    ?assertEqual(undefined, normalize_status(undefined)),
    ?assertEqual(undefined, normalize_status(null)),
    ?assertEqual(<<"online">>, normalize_status(<<"online">>)),
    ?assertEqual(<<"idle">>, normalize_status(<<"idle">>)),
    ?assertEqual(undefined, normalize_status(123)),
    ok.

select_initial_status_test() ->
    ?assertEqual(<<"idle">>, select_initial_status(undefined, <<"idle">>)),
    ?assertEqual(<<"dnd">>, select_initial_status(<<"unknown">>, <<"dnd">>)),
    ?assertEqual(<<"idle">>, select_initial_status(<<"online">>, <<"idle">>)),
    ?assertEqual(<<"online">>, select_initial_status(<<"online">>, <<"online">>)),
    ?assertEqual(<<"dnd">>, select_initial_status(<<"dnd">>, <<"online">>)),
    ok.

normalize_coordinate_test() ->
    ?assertEqual(undefined, normalize_coordinate(undefined)),
    ?assertEqual(undefined, normalize_coordinate(null)),
    ?assertEqual(1.5, normalize_coordinate(1.5)),
    ?assertEqual(<<"test">>, normalize_coordinate(<<"test">>)),
    ok.

add_coordinates_test() ->
    Base = #{<<"type">> => <<"session">>},
    ?assertEqual(Base, add_coordinates(Base, undefined, undefined)),
    ?assertEqual(
        #{<<"type">> => <<"session">>, <<"latitude">> => 1.0}, add_coordinates(Base, 1.0, undefined)
    ),
    ?assertEqual(
        #{<<"type">> => <<"session">>, <<"longitude">> => 2.0},
        add_coordinates(Base, undefined, 2.0)
    ),
    ?assertEqual(
        #{<<"type">> => <<"session">>, <<"latitude">> => 1.0, <<"longitude">> => 2.0},
        add_coordinates(Base, 1.0, 2.0)
    ),
    ok.

should_debounce_reactions_test() ->
    ?assertEqual(false, should_debounce_reactions(#{})),
    ?assertEqual(false, should_debounce_reactions(#{flags => 0})),
    ?assertEqual(false, should_debounce_reactions(#{flags => 1})),
    ?assertEqual(true, should_debounce_reactions(#{flags => 2})),
    ?assertEqual(true, should_debounce_reactions(#{flags => 3})),
    ?assertEqual(true, should_debounce_reactions(#{flags => 18})),
    ?assertEqual(false, should_debounce_reactions(#{flags => -1})),
    ok.

get_presence_custom_status_test() ->
    ?assertEqual(null, get_presence_custom_status(null)),
    ?assertEqual(null, get_presence_custom_status(#{})),
    ?assertEqual(
        #{<<"text">> => <<"hello">>},
        get_presence_custom_status(#{<<"custom_status">> => #{<<"text">> => <<"hello">>}})
    ),
    ?assertEqual(null, get_presence_custom_status(not_a_map)),
    ok.

-endif.
