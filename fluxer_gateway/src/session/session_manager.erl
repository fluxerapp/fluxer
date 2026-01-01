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

-module(session_manager).
-behaviour(gen_server).

-include_lib("fluxer_gateway/include/timeout_config.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export_type([session_data/0, user_id/0]).

-type session_id() :: binary().
-type user_id() :: integer().
-type session_ref() :: {pid(), reference()}.
-type status() :: online | offline | idle | dnd.
-type identify_timestamp() :: integer().
-define(IDENTIFY_FLAG_USE_CANARY_API, 16#1).

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
    api_host := string(),
    api_canary_host := undefined | string(),
    identify_attempts := [identify_timestamp()]
}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init([]) -> {ok, state()}.
init([]) ->
    fluxer_gateway_env:load(),
    process_flag(trap_exit, true),
    ApiHost = fluxer_gateway_env:get(api_host),
    ApiCanaryHost = fluxer_gateway_env:get(api_canary_host),
    {ok, #{
        sessions => #{},
        api_host => ApiHost,
        api_canary_host => ApiCanaryHost,
        identify_attempts => []
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
    Result :: {reply, Reply, state()},
    Reply ::
        {success, pid()}
        | {ok, pid()}
        | {error, not_found}
        | {error, identify_rate_limited}
        | {error, invalid_token}
        | {error, rate_limited}
        | {error, {server_error, non_neg_integer()}}
        | {error, {http_error, non_neg_integer()}}
        | {error, {network_error, term()}}
        | {error, registration_failed}
        | {error, term()}
        | {ok, non_neg_integer()}
        | ok.
handle_call(
    {start, Request, SocketPid},
    _From,
    State
) ->
    Sessions = maps:get(sessions, State),
    Attempts = maps:get(identify_attempts, State),
    SessionId = maps:get(session_id, Request),
    case maps:get(SessionId, Sessions, undefined) of
        {Pid, _Ref} ->
            {reply, {success, Pid}, State};
        undefined ->
            SessionName = process_registry:build_process_name(session, SessionId),
            case whereis(SessionName) of
                undefined ->
                    case check_identify_rate_limit(Attempts) of
                        {ok, NewAttempts} ->
                            handle_identify_request(
                                Request,
                                SocketPid,
                                SessionId,
                                Sessions,
                                maps:put(identify_attempts, NewAttempts, State)
                            );
                        {error, rate_limited} ->
                            {reply, {error, identify_rate_limited}, State}
                    end;
                Pid ->
                    Ref = monitor(process, Pid),
                    NewSessions = maps:put(SessionId, {Pid, Ref}, Sessions),
                    {reply, {success, Pid}, maps:put(sessions, NewSessions, State)}
            end
    end;
handle_call({lookup, SessionId}, _From, State) ->
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
handle_call(get_local_count, _From, State) ->
    Sessions = maps:get(sessions, State),
    {reply, {ok, maps:size(Sessions)}, State};
handle_call(get_global_count, _From, State) ->
    Sessions = maps:get(sessions, State),
    {reply, {ok, maps:size(Sessions)}, State};
handle_call(_, _From, State) ->
    {reply, ok, State}.

-spec handle_identify_request(
    identify_request(),
    pid(),
    session_id(),
    #{session_id() => session_ref()},
    state()
) ->
    {reply,
        {success, pid()}
        | {error, invalid_token}
        | {error, rate_limited}
        | {error, {server_error, non_neg_integer()}}
        | {error, {http_error, non_neg_integer()}}
        | {error, {network_error, term()}}
        | {error, registration_failed}
        | {error, term()},
        state()}.
handle_identify_request(
    Request, SocketPid, SessionId, Sessions, State
) ->
    IdentifyData = maps:get(identify_data, Request),
    Version = maps:get(version, Request),
    PeerIP = maps:get(peer_ip, Request),
    UseCanary = should_use_canary_api(IdentifyData),
    {_UsedCanary, RpcClient} = select_rpc_client(State, UseCanary),
    case fetch_rpc_data(Request, PeerIP, RpcClient) of
        {ok, Data} ->
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
            CustomStatusFromSettings = map_utils:get_safe(
                UserSettingsMap, <<"custom_status">>, null
            ),
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
                <<"avatar_color">> => map_utils:get_safe(
                    UserDataMap, <<"avatar_color">>, undefined
                ),
                <<"bot">> => map_utils:get_safe(UserDataMap, <<"bot">>, undefined),
                <<"system">> => map_utils:get_safe(UserDataMap, <<"system">>, undefined),
                <<"flags">> => maps:get(<<"flags">>, UserDataMap)
            },
            UserData = user_utils:normalize_user(UserData0),
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
                initial_guild_id => InitialGuildId
            },
            SessionName = process_registry:build_process_name(session, SessionId),
            case whereis(SessionName) of
                undefined ->
                    case session:start_link(SessionData) of
                        {ok, Pid} ->
                            case
                                process_registry:register_and_monitor(SessionName, Pid, Sessions)
                            of
                                {ok, RegisteredPid, Ref, NewSessions0} ->
                                    CleanSessions = maps:remove(SessionName, NewSessions0),
                                    NewSessions = maps:put(
                                        SessionId, {RegisteredPid, Ref}, CleanSessions
                                    ),
                                    {reply, {success, RegisteredPid}, maps:put(
                                        sessions, NewSessions, State
                                    )};
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
            end;
        {error, invalid_token} ->
            {reply, {error, invalid_token}, State};
        {error, rate_limited} ->
            {reply, {error, rate_limited}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_, State) ->
    {noreply, State}.

select_rpc_client(State, true) ->
    case maps:get(api_canary_host, State) of
        undefined ->
            logger:warning(
                "[session_manager] Canary API requested but not configured, falling back to stable API"
            ),
            {false, maps:get(api_host, State)};
        CanaryHost ->
            {true, CanaryHost}
    end;
select_rpc_client(State, false) ->
    {false, maps:get(api_host, State)}.

should_use_canary_api(IdentifyData) ->
    case map_utils:get_safe(IdentifyData, flags, 0) of
        Flags when is_integer(Flags), Flags >= 0 ->
            (Flags band ?IDENTIFY_FLAG_USE_CANARY_API) =/= 0;
        _ ->
            false
    end.

-spec handle_info(Info, State) -> {noreply, state()} when
    Info :: {'DOWN', reference(), process, pid(), term()} | term(),
    State :: state().
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    Sessions = maps:get(sessions, State),
    NewSessions = process_registry:cleanup_on_down(Pid, Sessions),
    {noreply, maps:put(sessions, NewSessions, State)};
handle_info(_, State) ->
    {noreply, State}.

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
    {ok, State};
code_change(_OldVsn, State, _Extra) when is_tuple(State), element(1, State) =:= state ->
    Sessions = element(2, State),
    ApiHost = element(3, State),
    ApiCanaryHost = element(4, State),
    IdentifyAttempts = element(5, State),
    {ok, #{
        sessions => Sessions,
        api_host => ApiHost,
        api_canary_host => ApiCanaryHost,
        identify_attempts => IdentifyAttempts
    }};
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec fetch_rpc_data(map(), term(), string()) ->
    {ok, map()}
    | {error, invalid_token}
    | {error, rate_limited}
    | {error, {server_error, non_neg_integer()}}
    | {error, {http_error, non_neg_integer()}}
    | {error, {network_error, term()}}.
fetch_rpc_data(Request, PeerIP, ApiHost) ->
    StartTime = erlang:system_time(millisecond),
    Result = do_fetch_rpc_data(Request, PeerIP, ApiHost),
    EndTime = erlang:system_time(millisecond),
    LatencyMs = EndTime - StartTime,
    gateway_metrics_collector:record_rpc_latency(LatencyMs),
    Result.

-spec do_fetch_rpc_data(map(), term(), string()) ->
    {ok, map()}
    | {error, invalid_token}
    | {error, rate_limited}
    | {error, {server_error, non_neg_integer()}}
    | {error, {http_error, non_neg_integer()}}
    | {error, {network_error, term()}}.
do_fetch_rpc_data(Request, PeerIP, ApiHost) ->
    Url = rpc_client:get_rpc_url(ApiHost),
    Headers = rpc_client:get_rpc_headers() ++ [{<<"content-type">>, <<"application/json">>}],
    IdentifyData = maps:get(identify_data, Request),
    Properties = map_utils:get_safe(IdentifyData, properties, #{}),
    LatitudeRaw = map_utils:get_safe(Properties, <<"latitude">>, undefined),
    LongitudeRaw = map_utils:get_safe(Properties, <<"longitude">>, undefined),
    Latitude =
        case LatitudeRaw of
            undefined -> undefined;
            null -> undefined;
            SafeLatitude -> SafeLatitude
        end,
    Longitude =
        case LongitudeRaw of
            undefined -> undefined;
            null -> undefined;
            SafeLongitude -> SafeLongitude
        end,
    RpcRequest = #{
        <<"type">> => <<"session">>,
        <<"token">> => maps:get(token, IdentifyData),
        <<"version">> => maps:get(version, Request),
        <<"ip">> => PeerIP
    },
    RpcRequestWithLatitude =
        case Latitude of
            undefined -> RpcRequest;
            LatitudeValue -> maps:put(<<"latitude">>, LatitudeValue, RpcRequest)
        end,
    RpcRequestWithLongitude =
        case Longitude of
            undefined -> RpcRequestWithLatitude;
            LongitudeValue -> maps:put(<<"longitude">>, LongitudeValue, RpcRequestWithLatitude)
        end,
    Body = jsx:encode(RpcRequestWithLongitude),
    case hackney:request(post, Url, Headers, Body, []) of
        {ok, 200, _RespHeaders, ClientRef} ->
            case hackney:body(ClientRef) of
                {ok, ResponseBody} ->
                    hackney:close(ClientRef),
                    ResponseData = jsx:decode(ResponseBody, [{return_maps, true}]),
                    {ok, maps:get(<<"data">>, ResponseData)};
                {error, BodyError} ->
                    hackney:close(ClientRef),
                    logger:error("[session_manager] Failed to read response body: ~p", [BodyError]),
                    {error, {network_error, BodyError}}
            end;
        {ok, 401, _, ClientRef} ->
            hackney:close(ClientRef),
            logger:info("[session_manager] RPC authentication failed (401)"),
            {error, invalid_token};
        {ok, 429, _, ClientRef} ->
            hackney:close(ClientRef),
            logger:warning("[session_manager] RPC rate limited (429)"),
            {error, rate_limited};
        {ok, StatusCode, _, ClientRef} when StatusCode >= 500 ->
            ErrorBody =
                case hackney:body(ClientRef) of
                    {ok, Body2} -> Body2;
                    {error, _} -> <<"<unable to read error body>">>
                end,
            hackney:close(ClientRef),
            logger:error("[session_manager] RPC server error ~p: ~s", [StatusCode, ErrorBody]),
            {error, {server_error, StatusCode}};
        {ok, StatusCode, _, ClientRef} when StatusCode >= 400 ->
            ErrorBody =
                case hackney:body(ClientRef) of
                    {ok, Body2} -> Body2;
                    {error, _} -> <<"<unable to read error body>">>
                end,
            hackney:close(ClientRef),
            logger:warning("[session_manager] RPC client error ~p: ~s", [StatusCode, ErrorBody]),
            {error, {http_error, StatusCode}};
        {ok, StatusCode, _, ClientRef} ->
            hackney:close(ClientRef),
            logger:warning("[session_manager] RPC unexpected status: ~p", [StatusCode]),
            {error, {http_error, StatusCode}};
        {error, Reason} ->
            logger:error("[session_manager] RPC request failed: ~p", [Reason]),
            {error, {network_error, Reason}}
    end.

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
