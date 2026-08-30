%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_http_client).
-typing([eqwalizer]).
-behaviour(gen_server).

-export([start_link/0, request/5, request/6]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([pick_sharded_profile/1, ensure_started/0, cleanup_max_age_ms/0]).
-export([push_max_concurrency/0]).

-define(SERVER, ?MODULE).
-define(CIRCUIT_TABLE, gateway_http_circuit_breaker).
-define(INFLIGHT_TABLE, gateway_http_inflight).

-define(DEFAULT_RPC_CONNECT_TIMEOUT_MS, 5000).
-define(DEFAULT_RPC_RECV_TIMEOUT_MS, 30000).
-define(DEFAULT_PUSH_CONNECT_TIMEOUT_MS, 3000).
-define(DEFAULT_PUSH_RECV_TIMEOUT_MS, 5000).

-define(DEFAULT_RPC_MAX_CONCURRENCY, 512).
-define(DEFAULT_PUSH_MAX_CONCURRENCY, 256).

-define(DEFAULT_FAILURE_THRESHOLD, 500).
-define(DEFAULT_RECOVERY_TIMEOUT_MS, 5000).
-define(DEFAULT_CLEANUP_INTERVAL_MS, 30000).
-define(DEFAULT_CLEANUP_MAX_AGE_MS, 300000).

-define(RPC_PROFILE_SHARDS, 8).
-define(PUSH_PROFILE_SHARDS, 4).

-type workload() :: rpc | push.
-type method() :: get | post | put | patch | delete | head | options.
-type request_headers() :: [{binary() | string(), binary() | string()}].
-type request_options() :: #{
    connect_timeout => timeout(),
    recv_timeout => timeout(),
    max_concurrency => pos_integer(),
    failure_threshold => pos_integer(),
    recovery_timeout_ms => pos_integer(),
    content_type => binary() | string(),
    host_key => binary(),
    is_https => boolean()
}.
-type response() :: {ok, non_neg_integer(), [{binary(), binary()}], binary()} | {error, term()}.

-type state() :: #{}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    case whereis(?SERVER) of
        undefined ->
            start_server();
        Pid when is_pid(Pid) ->
            {ok, Pid}
    end.

-spec start_server() -> {ok, pid()} | {error, term()}.
start_server() ->
    case gen_server:start_link({local, ?SERVER}, ?MODULE, [], []) of
        {error, {already_started, Pid}} when is_pid(Pid) -> {ok, Pid};
        {ok, Pid} -> {ok, Pid};
        {error, E} -> {error, E}
    end.

-spec request(workload(), method(), iodata(), request_headers(), iodata() | undefined) ->
    response().
request(Workload, Method, Url, Headers, Body) ->
    request(Workload, Method, Url, Headers, Body, #{}).

-spec request(
    workload(), method(), iodata(), request_headers(), iodata() | undefined, request_options()
) ->
    response().
request(Workload, Method, Url, Headers, Body, Opts) when is_map(Opts) ->
    ensure_runtime(Workload),
    WorkloadOpts = merged_workload_options(Workload, Opts),
    MaxConcurrency = maps:get(max_concurrency, WorkloadOpts),
    FailureThreshold = maps:get(failure_threshold, WorkloadOpts),
    RecoveryTimeoutMs = maps:get(recovery_timeout_ms, WorkloadOpts),
    Host = resolve_host_key(Url, WorkloadOpts),
    CircuitKey = {Workload, Host},
    do_request_with_circuit(
        CircuitKey,
        Workload,
        Method,
        Url,
        Headers,
        Body,
        WorkloadOpts,
        MaxConcurrency,
        FailureThreshold,
        RecoveryTimeoutMs
    ).

-spec pick_sharded_profile(workload()) -> atom().
pick_sharded_profile(rpc) ->
    Idx = erlang:phash2(self(), ?RPC_PROFILE_SHARDS),
    sharded_profile_name(rpc, Idx);
pick_sharded_profile(push) ->
    Idx = erlang:phash2(self(), ?PUSH_PROFILE_SHARDS),
    sharded_profile_name(push, Idx).

-spec ensure_started() -> ok.
ensure_started() ->
    case start_link() of
        {ok, _Pid} -> ok;
        _ -> ok
    end.

-spec cleanup_max_age_ms() -> pos_integer().
cleanup_max_age_ms() ->
    get_int_or_default(gateway_http_cleanup_max_age_ms, ?DEFAULT_CLEANUP_MAX_AGE_MS).

-spec push_max_concurrency() -> pos_integer().
push_max_concurrency() ->
    get_int_or_default(gateway_http_push_max_concurrency, ?DEFAULT_PUSH_MAX_CONCURRENCY).

-spec init([]) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),
    erlang:process_flag(fullsweep_after, 50),
    ensure_table(?CIRCUIT_TABLE),
    ensure_table(?INFLIGHT_TABLE),
    ok = ensure_sharded_profiles(rpc, ?RPC_PROFILE_SHARDS),
    ok = ensure_sharded_profiles(push, ?PUSH_PROFILE_SHARDS),
    schedule_cleanup(),
    {ok, #{}}.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, ok, state()}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(cleanup_circuits, State) ->
    gateway_http_client_response:prune_circuit_table(),
    schedule_cleanup(),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    erlang:garbage_collect(),
    {ok, State}.

-spec do_request_with_circuit(
    {workload(), binary()},
    workload(),
    method(),
    iodata(),
    request_headers(),
    iodata() | undefined,
    request_options(),
    pos_integer(),
    pos_integer(),
    pos_integer()
) -> response().
do_request_with_circuit(
    CircuitKey,
    Workload,
    Method,
    Url,
    Headers,
    Body,
    WorkloadOpts,
    MaxConcurrency,
    FailureThreshold,
    RecoveryTimeoutMs
) ->
    case gateway_http_client_response:allow_circuit_request(CircuitKey, RecoveryTimeoutMs) of
        ok ->
            do_request_with_slot(
                CircuitKey,
                Workload,
                Method,
                Url,
                Headers,
                Body,
                WorkloadOpts,
                MaxConcurrency,
                FailureThreshold
            );
        {error, circuit_open} ->
            {error, circuit_open}
    end.

-spec do_request_with_slot(
    {workload(), binary()},
    workload(),
    method(),
    iodata(),
    request_headers(),
    iodata() | undefined,
    request_options(),
    pos_integer(),
    pos_integer()
) -> response().
do_request_with_slot(
    CircuitKey,
    Workload,
    Method,
    Url,
    Headers,
    Body,
    WorkloadOpts,
    MaxConcurrency,
    FailureThreshold
) ->
    case gateway_http_client_response:acquire_inflight_slot(Workload, MaxConcurrency) of
        ok ->
            request_with_acquired_slot(
                CircuitKey, Workload, Method, Url, Headers, Body, WorkloadOpts, FailureThreshold
            );
        {error, overloaded} ->
            {error, overloaded}
    end.

-spec request_with_acquired_slot(
    {workload(), binary()},
    workload(),
    method(),
    iodata(),
    request_headers(),
    iodata() | undefined,
    request_options(),
    pos_integer()
) -> response().
request_with_acquired_slot(
    CircuitKey,
    Workload,
    Method,
    Url,
    Headers,
    Body,
    WorkloadOpts,
    FailureThreshold
) ->
    try
        gateway_http_client_request:safe_do_request(
            Workload, Method, Url, Headers, Body, WorkloadOpts
        )
    of
        Result ->
            gateway_http_client_response:update_circuit_state_direct(
                CircuitKey, Result, FailureThreshold
            ),
            Result
    after
        gateway_http_client_response:release_inflight_slot(Workload)
    end.

-spec resolve_host_key(iodata(), request_options()) -> binary().
resolve_host_key(Url, Opts) ->
    case maps:get(host_key, Opts, undefined) of
        Host when is_binary(Host) -> Host;
        _ -> gateway_http_client_request:extract_host_key(Url)
    end.

-spec ensure_runtime(workload()) -> ok.
ensure_runtime(Workload) ->
    ok = ensure_started(),
    _ = Workload,
    ok.

-spec ensure_table(atom()) -> ok.
ensure_table(Name) ->
    case ets:whereis(Name) of
        undefined -> create_table(Name);
        _ -> ok
    end.

-spec create_table(atom()) -> ok.
create_table(Name) ->
    try
        _ = create_ets_table_raw(Name),
        ok
    catch
        error:badarg -> ok
    end.

-spec create_ets_table_raw(atom()) -> ets:table().
create_ets_table_raw(Name) ->
    ets:new(Name, [
        named_table,
        public,
        set,
        {read_concurrency, true},
        {write_concurrency, true}
    ]).

-spec schedule_cleanup() -> reference().
schedule_cleanup() ->
    erlang:send_after(cleanup_interval_ms(), self(), cleanup_circuits).

-spec ensure_sharded_profiles(workload(), pos_integer()) -> ok.
ensure_sharded_profiles(Workload, ShardCount) ->
    lists:foreach(
        fun(Idx) ->
            Profile = sharded_profile_name(Workload, Idx),
            ensure_httpc_profile(Profile, Workload)
        end,
        lists:seq(0, ShardCount - 1)
    ).

-spec sharded_profile_name(workload(), non_neg_integer()) -> atom().
sharded_profile_name(rpc, Idx) ->
    list_to_atom("gateway_http_rpc_profile_" ++ integer_to_list(Idx));
sharded_profile_name(push, Idx) ->
    list_to_atom("gateway_http_push_profile_" ++ integer_to_list(Idx)).

-spec ensure_httpc_profile(atom(), workload()) -> ok.
ensure_httpc_profile(Profile, Workload) ->
    _ =
        case inets:start(httpc, [{profile, Profile}]) of
            {ok, _Pid} -> ok;
            {error, {already_started, _Pid}} -> ok;
            {error, {already_started, _Pid, _}} -> ok;
            {error, _Reason} -> ok
        end,
    Options = workload_httpc_options(Workload),
    _ = httpc:set_options(Options, Profile),
    ok.

-spec workload_httpc_options(workload()) -> list().
workload_httpc_options(rpc) ->
    [
        {max_sessions, 256},
        {max_keep_alive_length, 128},
        {max_pipeline_length, 0},
        {keep_alive_timeout, 120000}
    ];
workload_httpc_options(push) ->
    [{max_sessions, 512}, {max_keep_alive_length, 128}].

-spec merged_workload_options(workload(), request_options()) -> request_options().
merged_workload_options(Workload, Opts) ->
    maps:merge(default_options(Workload), Opts).

-spec default_options(workload()) -> request_options().
default_options(rpc) ->
    default_options(
        gateway_http_rpc_connect_timeout_ms,
        ?DEFAULT_RPC_CONNECT_TIMEOUT_MS,
        gateway_http_rpc_recv_timeout_ms,
        ?DEFAULT_RPC_RECV_TIMEOUT_MS,
        gateway_http_rpc_max_concurrency,
        ?DEFAULT_RPC_MAX_CONCURRENCY,
        <<"application/json">>
    );
default_options(push) ->
    default_options(
        gateway_http_push_connect_timeout_ms,
        ?DEFAULT_PUSH_CONNECT_TIMEOUT_MS,
        gateway_http_push_recv_timeout_ms,
        ?DEFAULT_PUSH_RECV_TIMEOUT_MS,
        gateway_http_push_max_concurrency,
        ?DEFAULT_PUSH_MAX_CONCURRENCY,
        <<"application/octet-stream">>
    ).

-spec default_options(atom(), integer(), atom(), integer(), atom(), integer(), binary()) ->
    request_options().
default_options(ConnectKey, ConnectDefault, RecvKey, RecvDefault, MaxKey, MaxDefault, Type) ->
    #{
        connect_timeout =>
            get_int_or_default(ConnectKey, ConnectDefault),
        recv_timeout =>
            get_int_or_default(RecvKey, RecvDefault),
        max_concurrency =>
            get_int_or_default(MaxKey, MaxDefault),
        failure_threshold =>
            get_int_or_default(gateway_http_failure_threshold, ?DEFAULT_FAILURE_THRESHOLD),
        recovery_timeout_ms =>
            get_int_or_default(gateway_http_recovery_timeout_ms, ?DEFAULT_RECOVERY_TIMEOUT_MS),
        content_type => Type
    }.

-spec cleanup_interval_ms() -> pos_integer().
cleanup_interval_ms() ->
    get_int_or_default(gateway_http_cleanup_interval_ms, ?DEFAULT_CLEANUP_INTERVAL_MS).

-spec get_int_or_default(atom(), integer()) -> integer().
get_int_or_default(Key, Default) ->
    case fluxer_gateway_env:get_optional(Key) of
        Value when is_integer(Value), Value > 0 -> Value;
        _ -> Default
    end.
