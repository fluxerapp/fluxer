%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_guild_pin_keeper).
-typing([eqwalizer]).
-behaviour(gen_server).

-export([
    start_link/0,
    child_specs/0,
    apply_boot_pins/0,
    stage/0,
    prewarm/0,
    arm/0,
    disarm/0,
    status/0
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(PEER_NAME_PREFIX, "fluxer_gateway@").
-define(RPC_TIMEOUT_MS, 2000).
-define(PATCH_TIMEOUT_MS, 10000).
-define(ARM_PIN_TIMEOUT_MS, 3000).
-define(CALL_TIMEOUT_MS, 60000).
-define(SWEEP_INTERVAL_MS, 1000).
-define(REPATCH_DELAY_MS, 100).
-define(RECONNECT_INTERVAL_MS, 20).
-define(RECONNECT_WINDOW_MS, 600000).
-define(PREWARM_TIMEOUT_MS, 55000).
-define(RELEASE_TIMEOUT_MS, 150000).
-define(RELOAD_TIMEOUT_MS, 60000).
-define(MAX_SOURCE_MAILBOX, 50).

-type guild_id() :: pos_integer().
-type worker() :: {patch | unpin | reconnect, node()} | sweep.
-type peer() :: #{
    status := patched | down | {failed, term()}, role := atom(), at => integer()
}.
-type state() :: #{
    guild_ids := [guild_id()],
    beam := gateway_guild_pin_remote:beam() | {error, term()},
    base_md5s := [binary()],
    armed := boolean(),
    ever_armed := boolean(),
    peers := #{node() => peer()},
    blocked := #{node() => term()},
    workers := #{reference() => {worker(), pid()}},
    sweep_timer := reference() | undefined,
    strays_stopped := non_neg_integer(),
    last_stray := map() | undefined,
    drifted := [node()]
}.

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec child_specs() -> [supervisor:child_spec()].
child_specs() ->
    case pinned_guild_ids() of
        [] ->
            [];
        _ ->
            [
                #{
                    id => ?MODULE,
                    start => {?MODULE, start_link, []},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker
                }
            ]
    end.

-spec apply_boot_pins() -> ok.
apply_boot_pins() ->
    case pinned_guild_ids() of
        [] ->
            ok;
        _GuildIds ->
            application:set_env(fluxer_gateway, guild_pinned_only_nodes, [node()])
    end.

-spec stage() -> {ok, map()} | {error, term()}.
stage() ->
    gen_server:call(?MODULE, stage, ?CALL_TIMEOUT_MS).

-spec prewarm() -> {ok, map()} | {error, term()}.
prewarm() ->
    gen_server:call(?MODULE, prewarm, ?CALL_TIMEOUT_MS).

-spec arm() -> {ok, map()} | {error, term()}.
arm() ->
    gen_server:call(?MODULE, arm, ?RELEASE_TIMEOUT_MS).

-spec disarm() -> {ok, map()} | {error, map()}.
disarm() ->
    gen_server:call(?MODULE, disarm, ?RELEASE_TIMEOUT_MS).

-spec status() -> map().
status() ->
    gen_server:call(?MODULE, status, ?RPC_TIMEOUT_MS).

-spec init([]) -> {ok, state()}.
init([]) ->
    ok = net_kernel:monitor_nodes(true),
    Armed = env_flag(guild_pin_keeper_armed),
    EverArmed = env_flag(guild_pin_keeper_ever_armed),
    ok = maybe_resume_armed(Armed),
    {ok, #{
        guild_ids => pinned_guild_ids(),
        beam => gateway_guild_pin_remote:read_beam(
            fluxer_gateway_env:get(guild_pin_keeper_beam),
            fluxer_gateway_env:get(guild_pin_keeper_beam_md5)
        ),
        base_md5s => gateway_guild_pin_remote:parse_md5_list(
            fluxer_gateway_env:get(guild_pin_keeper_base_md5s)
        ),
        armed => Armed,
        ever_armed => Armed orelse EverArmed,
        peers => #{},
        blocked => #{},
        workers => #{},
        sweep_timer => undefined,
        strays_stopped => 0,
        last_stray => undefined,
        drifted => []
    }}.

-spec env_flag(atom()) -> boolean().
env_flag(Key) ->
    case application:get_env(fluxer_gateway, Key, false) of
        true -> true;
        _ -> false
    end.

-spec maybe_resume_armed(boolean()) -> ok.
maybe_resume_armed(false) ->
    ok;
maybe_resume_armed(true) ->
    lists:foreach(fun(Node) -> self() ! {repatch, Node} end, gateway_peers(nodes())),
    self() ! sweep,
    ok.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, term(), state()}.
handle_call(stage, _From, State) ->
    {reply, do_stage(State), State};
handle_call(prewarm, _From, State) ->
    {reply, do_prewarm(State), State};
handle_call(arm, _From, State) ->
    {Reply, State1} = do_arm(State),
    {reply, Reply, State1};
handle_call(disarm, _From, State) ->
    {Reply, State1} = do_disarm(State),
    {reply, Reply, State1};
handle_call(status, _From, State) ->
    {reply, status_map(State), State};
handle_call(_Request, _From, State) ->
    {reply, {error, unsupported}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({nodeup, Node}, State) when is_atom(Node) ->
    {noreply, handle_nodeup(Node, State)};
handle_info({nodedown, Node}, State) when is_atom(Node) ->
    {noreply, handle_nodedown(Node, State)};
handle_info({repatch, Node}, State) when is_atom(Node) ->
    {noreply, handle_nodeup(Node, State)};
handle_info(sweep, State) ->
    {noreply, start_sweep(State#{sweep_timer := undefined})};
handle_info({worker_result, Ref, Result}, #{workers := Workers} = State) when
    is_reference(Ref), is_map_key(Ref, Workers)
->
    erlang:demonitor(Ref, [flush]),
    {Worker, _WorkerPid} = maps:get(Ref, Workers),
    {noreply, handle_worker_result(Worker, Result, drop_worker(Ref, State))};
handle_info({'DOWN', Ref, process, _Pid, Reason}, #{workers := Workers} = State) when
    is_reference(Ref), is_map_key(Ref, Workers)
->
    {Worker, _WorkerPid} = maps:get(Ref, Workers),
    Result = {unknown, {error, {worker_down, Reason}}},
    {noreply, handle_worker_result(Worker, Result, drop_worker(Ref, State))};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    _ = cancel_sweep(State),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec do_stage(state()) -> {ok, map()} | {error, term()}.
do_stage(#{beam := {error, Reason}}) ->
    {error, {beam_unavailable, Reason}};
do_stage(#{beam := Beam, base_md5s := BaseMd5s}) ->
    Peers = gateway_peers(nodes()),
    Results = gateway_guild_pin_remote:run_parallel(
        fun(Node) -> gateway_guild_pin_remote:ensure_router(Node, Beam, BaseMd5s) end,
        Peers,
        ?PATCH_TIMEOUT_MS
    ),
    case [{Node, Result} || {Node, Result} <- Results, Result =/= ok] of
        [] -> {ok, #{staged => length(Results)}};
        Failed -> {error, #{failed => Failed, staged => length(Results) - length(Failed)}}
    end.

-spec do_prewarm(state()) -> {ok, map()} | {error, term()}.
do_prewarm(#{guild_ids := []}) ->
    {error, no_pinned_guilds};
do_prewarm(#{guild_ids := GuildIds}) ->
    ok = gateway_guild_pin_remote:pin_local(GuildIds),
    Started = maps:from_list([
        {G, guild_manager:start_or_lookup(G, ?PREWARM_TIMEOUT_MS)}
     || G <- GuildIds
    ]),
    case lists:all(fun is_started/1, maps:values(Started)) of
        true -> {ok, Started};
        false -> {error, Started}
    end.

-spec is_started(term()) -> boolean().
is_started({ok, Pid}) -> is_pid(Pid);
is_started(_Other) -> false.

-spec do_arm(state()) -> {{ok, map()} | {error, term()}, state()}.
do_arm(#{guild_ids := []} = State) ->
    {{error, no_pinned_guilds}, State};
do_arm(#{beam := {error, Reason}} = State) ->
    {{error, {beam_unavailable, Reason}}, State};
do_arm(#{guild_ids := GuildIds} = State) ->
    case source_busy(GuildIds) of
        ok -> reload_and_arm(State);
        Busy -> {Busy, State}
    end.

-spec reload_and_arm(state()) -> {{ok, map()} | {error, term()}, state()}.
reload_and_arm(#{guild_ids := GuildIds} = State) ->
    ok = gateway_guild_pin_remote:pin_local(GuildIds),
    Refreshes = [{GuildId, refresh_local_copy(GuildId)} || GuildId <- GuildIds],
    case [Refresh || {_GuildId, Result} = Refresh <- Refreshes, not refreshed(Result)] of
        [] -> arm_if_calm(State);
        Failed -> {{error, #{reload_failed => Failed}}, State}
    end.

-spec refreshed(term()) -> boolean().
refreshed(ok) -> true;
refreshed(adopted) -> true;
refreshed(_Result) -> false.

-spec refresh_local_copy(guild_id()) -> ok | adopted | {error, term()}.
refresh_local_copy(GuildId) ->
    case
        local_copy(GuildId) =/= undefined andalso
            gateway_guild_pin_remote:routed_here(GuildId, gateway_peers(nodes()))
    of
        true -> adopted;
        false -> reload_local_copy(GuildId)
    end.

-spec arm_if_calm(state()) -> {{ok, map()} | {error, term()}, state()}.
arm_if_calm(#{guild_ids := GuildIds} = State) ->
    case source_busy(GuildIds) of
        ok -> arm_peers(gateway_peers(nodes()), State);
        Busy -> {Busy, State}
    end.

-spec source_busy([guild_id()]) -> ok | {error, map()}.
source_busy(GuildIds) ->
    Max = application:get_env(
        fluxer_gateway, guild_pin_keeper_max_source_mailbox, ?MAX_SOURCE_MAILBOX
    ),
    case gateway_guild_pin_remote:busy_copies(GuildIds, gateway_peers(nodes()), Max) of
        [] -> ok;
        Busy -> {error, #{source_busy => Busy, max_mailbox => Max, next => retry_when_calm}}
    end.

-spec reload_local_copy(guild_id()) -> ok | {error, term()}.
reload_local_copy(GuildId) ->
    case {local_copy(GuildId), guild_handoff_freeze:shard_pid(GuildId, node())} of
        {undefined, _} ->
            {error, not_prewarmed};
        {_Pid, {ok, Shard}} ->
            reload_on_shard(Shard, GuildId);
        {_Pid, {error, _} = Error} ->
            Error
    end.

-spec reload_on_shard(pid(), guild_id()) -> ok | {error, term()}.
reload_on_shard(Shard, GuildId) ->
    Request = {reload_guild, GuildId},
    case shard_utils:safe_gen_call_remote(Shard, Request, ?RELOAD_TIMEOUT_MS) of
        ok -> ok;
        Other -> {error, Other}
    end.

-spec arm_peers([node()], state()) -> {{ok, map()} | {error, term()}, state()}.
arm_peers(Peers, #{beam := #{md5 := _} = Beam, base_md5s := BaseMd5s} = State) ->
    Ready = gateway_guild_pin_remote:run_parallel(
        fun(Node) -> gateway_guild_pin_remote:ensure_router(Node, Beam, BaseMd5s) end,
        Peers,
        ?PATCH_TIMEOUT_MS
    ),
    case [{Node, Result} || {Node, Result} <- Ready, Result =/= ok] of
        [] -> pin_peers(Peers, State);
        Unready -> {{error, #{unready => Unready, next => retry}}, State}
    end.

-spec pin_peers([node()], state()) -> {{ok, map()} | {error, term()}, state()}.
pin_peers(
    Peers, #{beam := #{md5 := _} = Beam, base_md5s := BaseMd5s, guild_ids := GuildIds} = State
) ->
    ok = application:set_env(fluxer_gateway, guild_pin_keeper_ever_armed, true),
    State0 = State#{ever_armed := true},
    Results = gateway_guild_pin_remote:run_parallel(
        fun(Node) -> gateway_guild_pin_remote:patch_peer(Node, Beam, BaseMd5s, GuildIds) end,
        Peers,
        ?ARM_PIN_TIMEOUT_MS
    ),
    case [{Node, Result} || {Node, Result} <- Results, not is_patched(Result)] of
        [] ->
            ok = application:set_env(fluxer_gateway, guild_pin_keeper_armed, true),
            State1 = record_patches(Results, State0#{armed := true}),
            {{ok, #{patched => length(Results)}}, schedule_sweep(0, State1)};
        Failed ->
            {{_Released, Release}, State1} = do_disarm(State0),
            {{error, Release#{failed => Failed}}, State1}
    end.

-spec do_disarm(state()) -> {{ok, map()} | {error, map()}, state()}.
do_disarm(#{guild_ids := GuildIds} = State) ->
    ok = application:unset_env(fluxer_gateway, guild_pin_keeper_armed),
    State1 = settle_workers(cancel_sweep(State#{armed := false})),
    Peers = gateway_peers(nodes()),
    case gateway_guild_pin_remote:release_peers(Peers, GuildIds) of
        [] ->
            ok = gateway_guild_pin_remote:unpin_local(GuildIds),
            Stopped = [{GuildId, stop_local_copy(GuildId)} || GuildId <- GuildIds],
            Reply = #{unpinned => length(Peers), pinned_after => [], stopped_local => Stopped},
            {{ok, Reply}, State1};
        PinnedAfter ->
            Reply = #{pinned_after => PinnedAfter, kept_local => GuildIds, next => disarm},
            {{error, Reply}, State1}
    end.

-spec settle_workers(state()) -> state().
settle_workers(#{workers := Workers} = State) ->
    Deadline = erlang:monotonic_time(millisecond) + ?PATCH_TIMEOUT_MS,
    lists:foldl(
        fun
            ({Ref, {{Kind, _Node}, _Pid}}, Acc) when Kind =:= patch; Kind =:= unpin ->
                await_worker(Ref, Deadline, Acc);
            ({Ref, _Worker}, Acc) ->
                kill_worker(Ref, Acc)
        end,
        State,
        maps:to_list(Workers)
    ).

-spec await_worker(reference(), integer(), state()) -> state().
await_worker(Ref, Deadline, State) ->
    Wait = max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {worker_result, Ref, _Result} ->
            erlang:demonitor(Ref, [flush]),
            drop_worker(Ref, State);
        {'DOWN', Ref, process, _Pid, _Reason} ->
            drop_worker(Ref, State)
    after Wait ->
        kill_worker(Ref, State)
    end.

-spec stop_local_copy(guild_id()) -> term().
stop_local_copy(GuildId) ->
    case local_copy(GuildId) of
        undefined -> not_running;
        _Pid -> gateway_guild_pin_remote:stop_stray(GuildId, node())
    end.

-spec handle_nodeup(node(), state()) -> state().
handle_nodeup(Node, #{armed := true} = State) ->
    case is_gateway_peer(Node) andalso lists:member(Node, nodes()) of
        true -> start_patch(Node, State);
        false -> State
    end;
handle_nodeup(Node, #{ever_armed := true} = State) ->
    case is_gateway_peer(Node) of
        true -> start_unpin(Node, State);
        false -> State
    end;
handle_nodeup(_Node, State) ->
    State.

-spec start_unpin(node(), state()) -> state().
start_unpin(Node, #{guild_ids := GuildIds} = State) ->
    start_worker(
        {unpin, Node}, fun() -> gateway_guild_pin_remote:unpin_peer(Node, GuildIds) end, State
    ).

-spec handle_nodedown(node(), state()) -> state().
handle_nodedown(Node, #{armed := true, peers := Peers} = State) ->
    case maps:get(Node, Peers, undefined) of
        #{role := Role} = Peer ->
            State1 = State#{peers := Peers#{Node := Peer#{status := down}}},
            maybe_reconnect(Node, Role, State1);
        undefined ->
            State
    end;
handle_nodedown(_Node, State) ->
    State.

-spec maybe_reconnect(node(), atom(), state()) -> state().
maybe_reconnect(Node, Role, State) when Role =:= guilds; Role =:= all; Role =:= unknown ->
    Deadline = erlang:monotonic_time(millisecond) + ?RECONNECT_WINDOW_MS,
    start_worker({reconnect, Node}, fun() -> reconnect_loop(Node, Deadline) end, State);
maybe_reconnect(_Node, _Role, State) ->
    State.

-spec start_patch(node(), state()) -> state().
start_patch(_Node, #{beam := {error, _Reason}} = State) ->
    State;
start_patch(Node, #{beam := Beam, base_md5s := BaseMd5s, guild_ids := GuildIds} = State) ->
    Patch = fun() -> gateway_guild_pin_remote:patch_peer(Node, Beam, BaseMd5s, GuildIds) end,
    start_worker({patch, Node}, Patch, State).

-spec handle_worker_result(worker(), term(), state()) -> state().
handle_worker_result({patch, Node}, {Role, ok}, State) when is_atom(Role) ->
    #{peers := Peers, blocked := Blocked} = State,
    self() ! sweep,
    State#{peers := Peers#{Node => patched_peer(Role)}, blocked := maps:remove(Node, Blocked)};
handle_worker_result({patch, Node}, {Role0, {error, Reason}}, #{peers := Peers} = State) ->
    Role = known_role(Role0, Node, Peers),
    State1 = State#{peers := Peers#{Node => #{status => {failed, Reason}, role => Role}}},
    patch_failed(Node, Reason, State1);
handle_worker_result(sweep, {ok, #{stopped := Stopped, unpinned := Unpinned}}, State) ->
    #{peers := Peers} = State,
    Drifted = [
        Node
     || Node <- eqwalizer:dynamic_cast(Unpinned),
        maps:get(status, maps:get(Node, Peers, #{}), none) =:= patched
    ],
    lists:foreach(fun(Node) -> self() ! {repatch, Node} end, Drifted),
    schedule_sweep(
        ?SWEEP_INTERVAL_MS,
        record_strays(eqwalizer:dynamic_cast(Stopped), State#{drifted := Drifted})
    );
handle_worker_result(sweep, _Result, State) ->
    schedule_sweep(?SWEEP_INTERVAL_MS, State);
handle_worker_result(_Worker, _Result, State) ->
    State.

-spec patch_failed(node(), term(), state()) -> state().
patch_failed(Node, Reason, #{armed := true, blocked := Blocked} = State) ->
    case permanent_patch_error(Reason) of
        true ->
            logger:error("guild pin keeper cannot patch ~p: ~p", [Node, Reason]),
            State#{blocked := Blocked#{Node => Reason}};
        false ->
            _ = erlang:send_after(?REPATCH_DELAY_MS, self(), {repatch, Node}),
            State
    end;
patch_failed(_Node, _Reason, State) ->
    State.

-spec permanent_patch_error(term()) -> boolean().
permanent_patch_error({unexpected_router_md5, _}) -> true;
permanent_patch_error({router_old_code, _}) -> true;
permanent_patch_error({router_md5_after_load, _}) -> true;
permanent_patch_error(_) -> false.

-spec known_role(term(), node(), #{node() => peer()}) -> atom().
known_role(Role, _Node, _Peers) when is_atom(Role), Role =/= unknown ->
    Role;
known_role(_Role, Node, Peers) ->
    case maps:get(Node, Peers, undefined) of
        #{role := Role} -> Role;
        undefined -> unknown
    end.

-spec start_sweep(state()) -> state().
start_sweep(#{armed := false} = State) ->
    State;
start_sweep(#{workers := Workers, blocked := Blocked} = State) ->
    case has_worker(sweep, Workers) of
        true -> State;
        false -> start_sweep_worker(maps:get(guild_ids, State), maps:keys(Blocked), State)
    end.

-spec start_sweep_worker([guild_id()], [node()], state()) -> state().
start_sweep_worker(GuildIds, Blocked, State) ->
    Nodes = gateway_peers(nodes()) -- Blocked,
    start_worker(
        sweep, fun() -> {ok, gateway_guild_pin_remote:sweep(GuildIds, Nodes)} end, State
    ).

-spec record_strays([map()], state()) -> state().
record_strays([], State) ->
    State;
record_strays(Stopped, #{strays_stopped := Count} = State) ->
    logger:warning("guild pin keeper stopped stray guild copies: ~p", [Stopped]),
    State#{
        strays_stopped := Count + length(Stopped),
        last_stray := #{at => erlang:system_time(millisecond), stopped => Stopped}
    }.

-spec reconnect_loop(node(), integer()) -> connected | gave_up.
reconnect_loop(Node, Deadline) ->
    case net_kernel:connect_node(Node) of
        true -> connected;
        _ -> retry_connect(erlang:monotonic_time(millisecond) >= Deadline, Node, Deadline)
    end.

-spec retry_connect(boolean(), node(), integer()) -> connected | gave_up.
retry_connect(true, _Node, _Deadline) ->
    gave_up;
retry_connect(false, Node, Deadline) ->
    timer:sleep(?RECONNECT_INTERVAL_MS),
    reconnect_loop(Node, Deadline).

-spec kill_worker(reference(), state()) -> state().
kill_worker(Ref, #{workers := Workers} = State) ->
    {_Worker, Pid} = maps:get(Ref, Workers),
    exit(Pid, kill),
    erlang:demonitor(Ref, [flush]),
    drop_worker(Ref, State).

-spec start_worker(worker(), fun(() -> term()), state()) -> state().
start_worker(Worker, Fun, #{workers := Workers} = State) ->
    case has_worker(Worker, Workers) of
        true ->
            State;
        false ->
            spawn_worker(Worker, Fun, State)
    end.

-spec spawn_worker(worker(), fun(() -> term()), state()) -> state().
spawn_worker(Worker, Fun, #{workers := Workers} = State) ->
    Parent = self(),
    {Pid, Ref} = spawn_monitor(fun() -> run_worker(Parent, Fun) end),
    Pid ! {run, Ref},
    State#{workers := Workers#{Ref => {Worker, Pid}}}.

-spec run_worker(pid(), fun(() -> term())) -> ok.
run_worker(Parent, Fun) ->
    receive
        {run, MonRef} ->
            Parent ! {worker_result, MonRef, Fun()},
            ok
    after infinity ->
        ok
    end.

-spec has_worker(worker(), #{reference() => {worker(), pid()}}) -> boolean().
has_worker(Worker, Workers) ->
    lists:keymember(Worker, 1, maps:values(Workers)).

-spec drop_worker(reference(), state()) -> state().
drop_worker(Ref, #{workers := Workers} = State) ->
    State#{workers := maps:remove(Ref, Workers)}.

-spec record_patches([{node(), term()}], state()) -> state().
record_patches(Results, #{peers := Peers} = State) ->
    NewPeers = lists:foldl(
        fun
            ({Node, {Role, ok}}, Acc) when is_atom(Role) ->
                Acc#{Node => patched_peer(Role)};
            ({_Node, _Other}, Acc) ->
                Acc
        end,
        Peers,
        Results
    ),
    State#{peers := NewPeers}.

-spec patched_peer(atom()) -> peer().
patched_peer(Role) ->
    #{status => patched, role => Role, at => erlang:system_time(millisecond)}.

-spec schedule_sweep(non_neg_integer(), state()) -> state().
schedule_sweep(_DelayMs, #{armed := false} = State) ->
    State;
schedule_sweep(DelayMs, State) ->
    State1 = cancel_sweep(State),
    State1#{sweep_timer := erlang:send_after(DelayMs, self(), sweep)}.

-spec cancel_sweep(state()) -> state().
cancel_sweep(#{sweep_timer := undefined} = State) ->
    State;
cancel_sweep(#{sweep_timer := Ref} = State) ->
    _ = erlang:cancel_timer(Ref),
    State#{sweep_timer := undefined}.

-spec status_map(state()) -> map().
status_map(State) ->
    #{
        node => node(),
        armed => maps:get(armed, State),
        ever_armed => maps:get(ever_armed, State),
        guild_ids => maps:get(guild_ids, State),
        beam_md5 => beam_status(maps:get(beam, State)),
        base_md5s => [binary:encode_hex(Md5) || Md5 <- maps:get(base_md5s, State)],
        peers => maps:get(peers, State),
        blocked => maps:get(blocked, State),
        workers => lists:sort([
            Worker
         || {Worker, _Pid} <- maps:values(maps:get(workers, State))
        ]),
        local_copies => maps:from_list([
            {GuildId, local_copy(GuildId)}
         || GuildId <- maps:get(guild_ids, State)
        ]),
        strays_stopped => maps:get(strays_stopped, State),
        last_stray => maps:get(last_stray, State),
        drifted => maps:get(drifted, State)
    }.

-spec local_copy(guild_id()) -> pid() | undefined.
local_copy(GuildId) ->
    process_registry:registry_whereis(process_registry:build_process_key(guild, GuildId)).

-spec beam_status(gateway_guild_pin_remote:beam() | {error, term()}) ->
    binary() | {error, term()}.
beam_status(#{md5 := Md5}) -> binary:encode_hex(Md5);
beam_status({error, _Reason} = Error) -> Error.

-spec pinned_guild_ids() -> [guild_id()].
pinned_guild_ids() ->
    case fluxer_gateway_env:get(pinned_guild_ids) of
        Ids when is_list(Ids) -> [Id || Id <- Ids, is_integer(Id), Id > 0];
        _ -> []
    end.

-spec gateway_peers([node()]) -> [node()].
gateway_peers(Nodes) ->
    [Node || Node <- Nodes, is_gateway_peer(Node)].

-spec is_gateway_peer(node()) -> boolean().
is_gateway_peer(Node) ->
    lists:prefix(?PEER_NAME_PREFIX, atom_to_list(Node)).

-spec is_patched(term()) -> boolean().
is_patched({_Role, ok}) -> true;
is_patched(_) -> false.
