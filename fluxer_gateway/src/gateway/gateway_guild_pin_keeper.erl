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

-define(ROUTER, gateway_node_router).
-define(PEER_NAME_PREFIX, "fluxer_gateway@").
-define(LOADED_BEAM_NAME, "/tmp/fluxer-guild-pin/gateway_node_router.beam").
-define(RPC_TIMEOUT_MS, 2000).
-define(PATCH_TIMEOUT_MS, 10000).
-define(ARM_PIN_TIMEOUT_MS, 3000).
-define(CALL_TIMEOUT_MS, 60000).
-define(SWEEP_INTERVAL_MS, 1000).
-define(REPATCH_DELAY_MS, 100).
-define(RECONNECT_INTERVAL_MS, 20).
-define(RECONNECT_WINDOW_MS, 600000).
-define(STOP_TIMEOUT_MS, 5000).
-define(PREWARM_TIMEOUT_MS, 55000).
-define(RELEASE_TIMEOUT_MS, 150000).
-define(RELOAD_TIMEOUT_MS, 60000).
-define(MAX_SOURCE_MAILBOX, 50).
-define(RELEASE_ROUNDS, 3).
-define(RELEASE_RETRY_MS, 200).

-type guild_id() :: pos_integer().
-type beam() :: #{binary := binary(), md5 := binary()}.
-type outcome() :: ok | {error, term()}.
-type worker() :: {patch | unpin | reconnect, node()} | sweep.
-type peer() :: #{
    status := patched | down | {failed, term()}, role := atom(), at => integer()
}.
-type state() :: #{
    guild_ids := [guild_id()],
    beam := beam() | {error, term()},
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

-spec pin_local([guild_id()]) -> ok.
pin_local(GuildIds) ->
    Pins = as_pins(application:get_env(fluxer_gateway, guild_owner_pins, #{})),
    application:set_env(fluxer_gateway, guild_owner_pins, pin_guilds(Pins, GuildIds, node())).

-spec unpin_local([guild_id()]) -> ok.
unpin_local(GuildIds) ->
    Pins = as_pins(application:get_env(fluxer_gateway, guild_owner_pins, #{})),
    application:set_env(fluxer_gateway, guild_owner_pins, unpin_guilds(Pins, GuildIds, node())).

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
    Armed = application:get_env(fluxer_gateway, guild_pin_keeper_armed, false) =:= true,
    EverArmed =
        application:get_env(fluxer_gateway, guild_pin_keeper_ever_armed, false) =:= true,
    ok = maybe_resume_armed(Armed),
    {ok, #{
        guild_ids => pinned_guild_ids(),
        beam => read_beam(
            fluxer_gateway_env:get(guild_pin_keeper_beam),
            fluxer_gateway_env:get(guild_pin_keeper_beam_md5)
        ),
        base_md5s => parse_md5_list(fluxer_gateway_env:get(guild_pin_keeper_base_md5s)),
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
    is_map_key(Ref, Workers)
->
    erlang:demonitor(Ref, [flush]),
    {Worker, _WorkerPid} = maps:get(Ref, Workers),
    {noreply, handle_worker_result(Worker, Result, drop_worker(Ref, State))};
handle_info({'DOWN', Ref, process, _Pid, Reason}, #{workers := Workers} = State) when
    is_map_key(Ref, Workers)
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
    Results = run_parallel(
        fun(Node) -> ensure_router(Node, Beam, BaseMd5s) end, Peers, ?PATCH_TIMEOUT_MS
    ),
    case [{Node, Result} || {Node, Result} <- Results, Result =/= ok] of
        [] -> {ok, #{staged => length(Results)}};
        Failed -> {error, #{failed => Failed, staged => length(Results) - length(Failed)}}
    end.

-spec do_prewarm(state()) -> {ok, map()} | {error, term()}.
do_prewarm(#{guild_ids := []}) ->
    {error, no_pinned_guilds};
do_prewarm(#{guild_ids := GuildIds}) ->
    ok = pin_local(GuildIds),
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
    ok = pin_local(GuildIds),
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
        local_copy(GuildId) =/= undefined andalso routed_here(GuildId, gateway_peers(nodes()))
    of
        true -> adopted;
        false -> reload_local_copy(GuildId)
    end.

-spec routed_here(guild_id(), [node()]) -> boolean().
routed_here(_GuildId, []) ->
    true;
routed_here(GuildId, Peers) ->
    Answers = erpc:multicall(
        Peers, ?ROUTER, owner_node_result, [GuildId, guilds], ?RPC_TIMEOUT_MS
    ),
    lists:all(fun(Answer) -> Answer =:= {ok, {ok, node()}} end, Answers).

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
    case busy_copies(GuildIds, gateway_peers(nodes()), Max) of
        [] -> ok;
        Busy -> {error, #{source_busy => Busy, max_mailbox => Max, next => retry_when_calm}}
    end.

-spec busy_copies([guild_id()], [node()], non_neg_integer()) -> [{node(), term()}].
busy_copies(GuildIds, Nodes, Max) ->
    [
        {Node, Len}
     || GuildId <- GuildIds,
        Key <- [process_registry:build_process_key(guild, GuildId)],
        Node <- stray_nodes(Key, Nodes),
        Len <- [copy_mailbox(Node, Key)],
        not (is_integer(Len) andalso Len =< Max)
    ].

-spec copy_mailbox(node(), process_registry:process_key()) -> term().
copy_mailbox(Node, Key) ->
    case rpc:call(Node, ets, lookup, [process_registry_table, Key], ?RPC_TIMEOUT_MS) of
        [{_, Pid}] when is_pid(Pid) ->
            case
                rpc:call(Node, erlang, process_info, [Pid, message_queue_len], ?RPC_TIMEOUT_MS)
            of
                {message_queue_len, Len} -> Len;
                undefined -> 0;
                Other -> {unreadable, Other}
            end;
        _ ->
            0
    end.

-spec reload_local_copy(guild_id()) -> ok | {error, term()}.
reload_local_copy(GuildId) ->
    case {local_copy(GuildId), guild_handoff_freeze:shard_pid(GuildId, node())} of
        {undefined, _} ->
            {error, not_prewarmed};
        {_Pid, {ok, Shard}} ->
            Request = {reload_guild, GuildId},
            case shard_utils:safe_gen_call_remote(Shard, Request, ?RELOAD_TIMEOUT_MS) of
                ok -> ok;
                Other -> {error, Other}
            end;
        {_Pid, {error, _} = Error} ->
            Error
    end.

-spec arm_peers([node()], state()) -> {{ok, map()} | {error, term()}, state()}.
arm_peers(Peers, #{beam := Beam, base_md5s := BaseMd5s} = State) ->
    Ready = run_parallel(
        fun(Node) -> ensure_router(Node, Beam, BaseMd5s) end, Peers, ?PATCH_TIMEOUT_MS
    ),
    case [{Node, Result} || {Node, Result} <- Ready, Result =/= ok] of
        [] -> pin_peers(Peers, State);
        Unready -> {{error, #{unready => Unready, next => retry}}, State}
    end.

-spec pin_peers([node()], state()) -> {{ok, map()} | {error, term()}, state()}.
pin_peers(Peers, #{beam := Beam, base_md5s := BaseMd5s, guild_ids := GuildIds} = State) ->
    ok = application:set_env(fluxer_gateway, guild_pin_keeper_ever_armed, true),
    State0 = State#{ever_armed := true},
    Results = run_parallel(
        fun(Node) -> patch_peer(Node, Beam, BaseMd5s, GuildIds) end, Peers, ?ARM_PIN_TIMEOUT_MS
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
    case release_peers(Peers, GuildIds) of
        [] ->
            ok = unpin_local(GuildIds),
            Stopped = [{GuildId, stop_local_copy(GuildId)} || GuildId <- GuildIds],
            Reply = #{unpinned => length(Peers), pinned_after => [], stopped_local => Stopped},
            {{ok, Reply}, State1};
        PinnedAfter ->
            Reply = #{pinned_after => PinnedAfter, kept_local => GuildIds, next => disarm},
            {{error, Reply}, State1}
    end.

-spec release_peers([node()], [guild_id()]) -> [{node(), term()}].
release_peers(Peers, GuildIds) ->
    release_peers(Peers, GuildIds, ?RELEASE_ROUNDS).

-spec release_peers([node()], [guild_id()], pos_integer()) -> [{node(), term()}].
release_peers([], _GuildIds, _Left) ->
    [];
release_peers(Peers, GuildIds, Left) ->
    _ = run_parallel(fun(Node) -> unpin_peer(Node, GuildIds) end, Peers, ?PATCH_TIMEOUT_MS),
    Checked = run_parallel(
        fun(Node) -> routes_here(Node, GuildIds) end, Peers, ?PATCH_TIMEOUT_MS
    ),
    case [{Node, Answer} || {Node, Answer} <- Checked, Answer =/= released] of
        [] ->
            [];
        Still when Left =:= 1 ->
            Still;
        Still ->
            timer:sleep(?RELEASE_RETRY_MS),
            release_peers([Node || {Node, _Answer} <- Still], GuildIds, Left - 1)
    end.

-spec routes_here(node(), [guild_id()]) -> released | {pinned | unverified, term()}.
routes_here(Node, GuildIds) ->
    Answers = [
        rpc:call(Node, ?ROUTER, owner_node_result, [GuildId, guilds], ?RPC_TIMEOUT_MS)
     || GuildId <- GuildIds
    ],
    case [Answer || Answer <- Answers, not released_answer(Answer)] of
        [] -> released;
        Wrong -> release_status(Wrong)
    end.

-spec released_answer(term()) -> boolean().
released_answer({ok, Owner}) -> Owner =/= node();
released_answer({error, {no_active_nodes, _}}) -> true;
released_answer({badrpc, nodedown}) -> true;
released_answer(_Answer) -> false.

-spec release_status([term()]) -> {pinned | unverified, term()}.
release_status(Wrong) ->
    case lists:member({ok, node()}, Wrong) of
        true -> {pinned, Wrong};
        false -> {unverified, Wrong}
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
        _Pid -> stop_stray(GuildId, node())
    end.

-spec handle_nodeup(node(), state()) -> state().
handle_nodeup(Node, #{armed := true} = State) ->
    case is_gateway_peer(Node) andalso lists:member(Node, nodes()) of
        true -> start_patch(Node, State);
        false -> State
    end;
handle_nodeup(Node, #{ever_armed := true, guild_ids := GuildIds} = State) ->
    case is_gateway_peer(Node) of
        true -> start_worker({unpin, Node}, fun() -> unpin_peer(Node, GuildIds) end, State);
        false -> State
    end;
handle_nodeup(_Node, State) ->
    State.

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
start_patch(Node, #{beam := Beam, base_md5s := BaseMd5s, guild_ids := GuildIds} = State) ->
    case Beam of
        #{} ->
            start_worker(
                {patch, Node}, fun() -> patch_peer(Node, Beam, BaseMd5s, GuildIds) end, State
            );
        {error, _Reason} ->
            State
    end.

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
     || Node <- Unpinned, maps:get(status, maps:get(Node, Peers, #{}), none) =:= patched
    ],
    lists:foreach(fun(Node) -> self() ! {repatch, Node} end, Drifted),
    schedule_sweep(?SWEEP_INTERVAL_MS, record_strays(Stopped, State#{drifted := Drifted}));
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
        true ->
            State;
        false ->
            GuildIds = maps:get(guild_ids, State),
            Nodes = gateway_peers(nodes()) -- maps:keys(Blocked),
            start_worker(sweep, fun() -> {ok, sweep(GuildIds, Nodes)} end, State)
    end.

-spec sweep([guild_id()], [node()]) -> #{stopped := [map()], unpinned := [node()]}.
sweep(GuildIds, Nodes) ->
    #{
        unpinned => lists:usort(
            lists:flatmap(fun(G) -> unpinned_nodes(G, Nodes) end, GuildIds)
        ),
        stopped => lists:flatmap(fun(GuildId) -> sweep_guild(GuildId, Nodes) end, GuildIds)
    }.

-spec unpinned_nodes(guild_id(), [node()]) -> [node()].
unpinned_nodes(_GuildId, []) ->
    [];
unpinned_nodes(GuildId, Nodes) ->
    Expected = {ok, node()},
    Answers = erpc:multicall(
        Nodes, ?ROUTER, owner_node_result, [GuildId, guilds], ?RPC_TIMEOUT_MS
    ),
    [Node || {Node, Answer} <- lists:zip(Nodes, Answers), Answer =/= {ok, Expected}].

-spec sweep_guild(guild_id(), [node()]) -> [map()].
sweep_guild(GuildId, Nodes) ->
    Key = process_registry:build_process_key(guild, GuildId),
    case process_registry:registry_whereis(Key) of
        undefined ->
            [];
        _LocalPid ->
            [
                #{guild_id => GuildId, node => Node, result => stop_stray(GuildId, Node)}
             || Node <- stray_nodes(Key, Nodes)
            ]
    end.

-spec stray_nodes(process_registry:process_key(), [node()]) -> [node()].
stray_nodes(_Key, []) ->
    [];
stray_nodes(Key, Nodes) ->
    Results = erpc:multicall(
        Nodes, ets, lookup, [process_registry_table, Key], ?RPC_TIMEOUT_MS
    ),
    [Node || {Node, {ok, [{_, Pid}]}} <- lists:zip(Nodes, Results), is_pid(Pid)].

-spec stop_stray(guild_id(), node()) -> term().
stop_stray(GuildId, Node) ->
    case guild_handoff_freeze:shard_pid(GuildId, Node) of
        {ok, ShardPid} ->
            Request = {stop_guild, GuildId, {shutdown, handoff}},
            shard_utils:safe_gen_call_remote(ShardPid, Request, ?STOP_TIMEOUT_MS);
        {error, _Reason} = Error ->
            Error
    end.

-spec record_strays([map()], state()) -> state().
record_strays([], State) ->
    State;
record_strays(Stopped, #{strays_stopped := Count} = State) ->
    logger:warning("guild pin keeper stopped stray guild copies: ~p", [Stopped]),
    State#{
        strays_stopped := Count + length(Stopped),
        last_stray := #{at => erlang:system_time(millisecond), stopped => Stopped}
    }.

-spec patch_peer(node(), beam(), [binary()], [guild_id()]) -> {atom(), outcome()}.
patch_peer(Node, Beam, BaseMd5s, GuildIds) ->
    Role = peer_role(Node),
    Outcome =
        maybe
            ok ?= ensure_router(Node, Beam, BaseMd5s),
            ok ?= pin_peer(Node, GuildIds),
            verify_pins(Node, GuildIds)
        end,
    {Role, Outcome}.

-spec ensure_router(node(), beam(), [binary()]) -> ok | {error, term()}.
ensure_router(Node, #{binary := Bin, md5 := PinnedMd5}, BaseMd5s) ->
    case remote_router_md5(Node) of
        {ok, PinnedMd5} ->
            ok;
        {ok, Md5} ->
            case lists:member(Md5, BaseMd5s) of
                true -> load_router(Node, Bin, PinnedMd5);
                false -> pin_aware_router(Node, Md5)
            end;
        {error, _Reason} = Error ->
            Error
    end.

-spec pin_aware_router(node(), binary()) -> ok | {error, term()}.
pin_aware_router(Node, Md5) ->
    Args = [?ROUTER, pinned_guild_owner, 1],
    case rpc:call(Node, erlang, function_exported, Args, ?RPC_TIMEOUT_MS) of
        true -> ok;
        _ -> {error, {unexpected_router_md5, binary:encode_hex(Md5)}}
    end.

-spec remote_router_md5(node()) -> {ok, binary()} | {error, term()}.
remote_router_md5(Node) ->
    case rpc:call(Node, code, ensure_loaded, [?ROUTER], ?RPC_TIMEOUT_MS) of
        {module, ?ROUTER} ->
            case rpc:call(Node, erlang, get_module_info, [?ROUTER, md5], ?RPC_TIMEOUT_MS) of
                Md5 when is_binary(Md5) -> {ok, Md5};
                Other -> {error, {router_md5_unavailable, Other}}
            end;
        Other ->
            {error, {router_unavailable, Other}}
    end.

-spec load_router(node(), binary(), binary()) -> ok | {error, term()}.
load_router(Node, Bin, PinnedMd5) ->
    case rpc:call(Node, erlang, check_old_code, [?ROUTER], ?RPC_TIMEOUT_MS) of
        false ->
            Args = [?ROUTER, ?LOADED_BEAM_NAME, Bin],
            case rpc:call(Node, code, load_binary, Args, ?RPC_TIMEOUT_MS) of
                {module, ?ROUTER} -> confirm_router(Node, PinnedMd5);
                Other -> {error, {router_load_failed, Other}}
            end;
        Other ->
            {error, {router_old_code, Other}}
    end.

-spec confirm_router(node(), binary()) -> ok | {error, term()}.
confirm_router(Node, PinnedMd5) ->
    case remote_router_md5(Node) of
        {ok, PinnedMd5} ->
            _ = rpc:call(Node, code, soft_purge, [?ROUTER], ?RPC_TIMEOUT_MS),
            ok;
        Other ->
            {error, {router_md5_after_load, Other}}
    end.

-spec pin_peer(node(), [guild_id()]) -> ok | {error, term()}.
pin_peer(Node, GuildIds) ->
    update_remote_pins(Node, fun(Pins) -> pin_guilds(Pins, GuildIds, node()) end).

-spec unpin_peer(node(), [guild_id()]) -> ok | {error, term()}.
unpin_peer(Node, GuildIds) ->
    update_remote_pins(Node, fun(Pins) -> unpin_guilds(Pins, GuildIds, node()) end).

-spec update_remote_pins(node(), fun((#{guild_id() => node()}) -> #{guild_id() => node()})) ->
    ok | {error, term()}.
update_remote_pins(Node, Update) ->
    case
        rpc:call(
            Node, application, get_env, [fluxer_gateway, guild_owner_pins, #{}], ?RPC_TIMEOUT_MS
        )
    of
        Pins when is_map(Pins) -> write_remote_pins(Node, as_pins(Pins), Update(as_pins(Pins)));
        Other -> {error, {pins_unreadable, Other}}
    end.

-spec write_remote_pins(node(), #{guild_id() => node()}, #{guild_id() => node()}) ->
    ok | {error, term()}.
write_remote_pins(_Node, Same, Same) ->
    ok;
write_remote_pins(Node, _Old, New) when map_size(New) =:= 0 ->
    remote_env(Node, unset_env, [fluxer_gateway, guild_owner_pins]);
write_remote_pins(Node, _Old, New) ->
    remote_env(Node, set_env, [fluxer_gateway, guild_owner_pins, New]).

-spec remote_env(node(), set_env | unset_env, [term()]) -> ok | {error, term()}.
remote_env(Node, Function, Args) ->
    case rpc:call(Node, application, Function, Args, ?RPC_TIMEOUT_MS) of
        ok -> ok;
        Other -> {error, {Function, Other}}
    end.

-spec verify_pins(node(), [guild_id()]) -> ok | {error, term()}.
verify_pins(Node, GuildIds) ->
    Expected = {ok, node()},
    Answers = [
        {GuildId,
            rpc:call(Node, ?ROUTER, owner_node_result, [GuildId, guilds], ?RPC_TIMEOUT_MS)}
     || GuildId <- GuildIds
    ],
    case [Answer || {_GuildId, Got} = Answer <- Answers, Got =/= Expected] of
        [] -> ok;
        Wrong -> {error, {pin_not_effective, Wrong}}
    end.

-spec peer_role(node()) -> atom().
peer_role(Node) ->
    case rpc:call(Node, fluxer_gateway_sup, current_role, [], ?RPC_TIMEOUT_MS) of
        Role when is_atom(Role) -> Role;
        _Other -> unknown
    end.

-spec reconnect_loop(node(), integer()) -> connected | gave_up.
reconnect_loop(Node, Deadline) ->
    case net_kernel:connect_node(Node) of
        true ->
            connected;
        _ ->
            case erlang:monotonic_time(millisecond) >= Deadline of
                true ->
                    gave_up;
                false ->
                    receive
                    after ?RECONNECT_INTERVAL_MS -> reconnect_loop(Node, Deadline)
                    end
            end
    end.

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
            Parent = self(),
            {Pid, Ref} = spawn_monitor(fun() ->
                receive
                    {run, MonRef} -> Parent ! {worker_result, MonRef, Fun()}
                end
            end),
            Pid ! {run, Ref},
            State#{workers := Workers#{Ref => {Worker, Pid}}}
    end.

-spec has_worker(worker(), #{reference() => {worker(), pid()}}) -> boolean().
has_worker(Worker, Workers) ->
    lists:keymember(Worker, 1, maps:values(Workers)).

-spec drop_worker(reference(), state()) -> state().
drop_worker(Ref, #{workers := Workers} = State) ->
    State#{workers := maps:remove(Ref, Workers)}.

-spec run_parallel(fun((node()) -> term()), [node()], pos_integer()) -> [{node(), term()}].
run_parallel(Fun, Nodes, TimeoutMs) ->
    Parent = self(),
    Ref = make_ref(),
    Pids = [{spawn(fun() -> Parent ! {Ref, Node, Fun(Node)} end), Node} || Node <- Nodes],
    Deadline = erlang:monotonic_time(millisecond) + TimeoutMs,
    Results = collect_parallel(Ref, Nodes, #{}, Deadline),
    [exit(Pid, kill) || {Pid, Node} <- Pids, not maps:is_key(Node, Results)],
    [{Node, maps:get(Node, Results, {error, timeout})} || Node <- Nodes].

-spec collect_parallel(reference(), [node()], #{node() => term()}, integer()) ->
    #{node() => term()}.
collect_parallel(_Ref, Nodes, Results, _Deadline) when map_size(Results) =:= length(Nodes) ->
    Results;
collect_parallel(Ref, Nodes, Results, Deadline) ->
    Wait = max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {Ref, Node, Result} -> collect_parallel(Ref, Nodes, Results#{Node => Result}, Deadline)
    after Wait ->
        Results
    end.

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

-spec beam_status(beam() | {error, term()}) -> binary() | {error, term()}.
beam_status(#{md5 := Md5}) -> binary:encode_hex(Md5);
beam_status({error, _Reason} = Error) -> Error.

-spec read_beam(term(), term()) -> beam() | {error, term()}.
read_beam(Path, ExpectedHex) when is_list(Path), is_binary(ExpectedHex) ->
    case file:read_file(Path) of
        {ok, Bin} -> check_beam(Bin, decode_md5(ExpectedHex));
        {error, Reason} -> {error, {read_failed, Path, Reason}}
    end;
read_beam(_Path, _ExpectedHex) ->
    {error, not_configured}.

-spec check_beam(binary(), {ok, binary()} | error) -> beam() | {error, term()}.
check_beam(_Bin, error) ->
    {error, invalid_expected_md5};
check_beam(Bin, {ok, Expected}) ->
    case beam_lib:md5(Bin) of
        {ok, {?ROUTER, Expected}} -> #{binary => Bin, md5 => Expected};
        {ok, {Module, Md5}} -> {error, {beam_mismatch, Module, binary:encode_hex(Md5)}};
        {error, beam_lib, Reason} -> {error, {invalid_beam, Reason}}
    end.

-spec parse_md5_list(term()) -> [binary()].
parse_md5_list(Value) when is_binary(Value) ->
    [
        Md5
     || Token <- string:lexemes(binary_to_list(Value), ", "), {ok, Md5} <- [decode_md5(Token)]
    ];
parse_md5_list(_Value) ->
    [].

-spec decode_md5(string() | binary()) -> {ok, binary()} | error.
decode_md5(Hex) when is_list(Hex) ->
    decode_md5(list_to_binary(Hex));
decode_md5(Hex) when byte_size(Hex) =:= 32 ->
    try binary:decode_hex(Hex) of
        Md5 -> {ok, Md5}
    catch
        error:badarg -> error
    end;
decode_md5(_Hex) ->
    error.

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

-spec as_pins(term()) -> #{guild_id() => node()}.
as_pins(Pins) when is_map(Pins) ->
    maps:filter(fun(Key, Value) -> is_integer(Key) andalso is_atom(Value) end, Pins);
as_pins(_Pins) ->
    #{}.

-spec pin_guilds(#{guild_id() => node()}, [guild_id()], node()) -> #{guild_id() => node()}.
pin_guilds(Pins, GuildIds, Owner) ->
    maps:merge(Pins, maps:from_list([{GuildId, Owner} || GuildId <- GuildIds])).

-spec unpin_guilds(#{guild_id() => node()}, [guild_id()], node()) -> #{guild_id() => node()}.
unpin_guilds(Pins, GuildIds, Owner) ->
    maps:filter(
        fun(GuildId, Node) -> not (Node =:= Owner andalso lists:member(GuildId, GuildIds)) end,
        Pins
    ).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(PG, 1100000000000000001).

unpin_only_removes_this_nodes_pins_test() ->
    Pins = #{?PG => node(), 42 => 'pin_other@h', 7 => node()},
    ?assertEqual(#{42 => 'pin_other@h', 7 => node()}, unpin_guilds(Pins, [?PG], node())),
    ?assertEqual(Pins, unpin_guilds(Pins, [42], node())).

pin_overrides_a_stale_owner_test() ->
    Pins = #{?PG => 'fluxer_gateway_dn_dead@h', 42 => 'g@h'},
    ?assertEqual(#{?PG => node(), 42 => 'g@h'}, pin_guilds(Pins, [?PG], node())).

only_release_named_nodes_are_peers_test() ->
    ?assertEqual(
        ['fluxer_gateway@10.0.0.1'],
        gateway_peers(['fluxer_gateway@10.0.0.1', 'fluxer_gateway_dn_ab12@10.0.0.2', 'probe@h'])
    ).

beam_must_match_the_expected_md5_test() ->
    {?ROUTER, Bin, _Path} = code:get_object_code(?ROUTER),
    {ok, {?ROUTER, Md5}} = beam_lib:md5(Bin),
    ?assertMatch(#{md5 := Md5}, check_beam(Bin, decode_md5(binary:encode_hex(Md5)))),
    ?assertMatch({error, {beam_mismatch, ?ROUTER, _}}, check_beam(Bin, {ok, <<0:128>>})),
    ?assertEqual({error, invalid_expected_md5}, check_beam(Bin, decode_md5(<<"D5E4">>))),
    ?assertEqual({error, not_configured}, read_beam(undefined, undefined)).

base_md5_list_parses_hex_and_skips_garbage_test() ->
    ?assertEqual(
        [binary:decode_hex(<<"5D7C0BB7B8DA42FC92C6B3BDC7584CD2">>)],
        parse_md5_list(<<"5D7C0BB7B8DA42FC92C6B3BDC7584CD2, nope">>)
    ).

boot_pins_are_off_without_pinned_guilds_test() ->
    persistent_term:put({fluxer_gateway, runtime_config}, #{}),
    try
        ok = apply_boot_pins(),
        ?assertEqual(undefined, application:get_env(fluxer_gateway, guild_owner_pins)),
        ?assertEqual(undefined, application:get_env(fluxer_gateway, guild_pinned_only_nodes)),
        ?assertEqual([], child_specs())
    after
        persistent_term:erase({fluxer_gateway, runtime_config})
    end.

local_pins_follow_arm_and_disarm_test() ->
    application:set_env(fluxer_gateway, guild_owner_pins, #{42 => 'pin_other@h'}),
    try
        ok = pin_local([?PG]),
        ?assertEqual(
            {ok, #{?PG => node(), 42 => 'pin_other@h'}},
            application:get_env(fluxer_gateway, guild_owner_pins)
        ),
        ok = unpin_local([?PG]),
        ?assertEqual(
            {ok, #{42 => 'pin_other@h'}}, application:get_env(fluxer_gateway, guild_owner_pins)
        )
    after
        application:unset_env(fluxer_gateway, guild_owner_pins)
    end.

boot_pins_leave_this_node_owning_nothing_until_prewarm_test() ->
    persistent_term:put({fluxer_gateway, runtime_config}, #{
        pinned_guild_ids => [?PG], gateway_role => guilds
    }),
    try
        ok = apply_boot_pins(),
        ?assertEqual(undefined, application:get_env(fluxer_gateway, guild_owner_pins)),
        ?assertEqual(
            {ok, [node()]}, application:get_env(fluxer_gateway, guild_pinned_only_nodes)
        ),
        ?assertEqual(
            {error, {no_active_nodes, guilds}}, ?ROUTER:owner_node_result(?PG, guilds)
        ),
        ok = pin_local([?PG]),
        ?assertEqual({ok, node()}, ?ROUTER:owner_node_result(?PG, guilds)),
        ?assertMatch([#{id := ?MODULE}], child_specs())
    after
        application:unset_env(fluxer_gateway, guild_owner_pins),
        application:unset_env(fluxer_gateway, guild_pinned_only_nodes),
        persistent_term:erase({fluxer_gateway, runtime_config})
    end.

sweep_leaves_remote_copies_alone_without_a_local_copy_test() ->
    ok = process_registry:init(),
    ?assertEqual([], sweep_guild(?PG, ['fluxer_gateway@10.0.0.1'])),
    ?assertEqual(#{stopped => [], unpinned => []}, sweep([?PG], [])).

release_unpins_and_verifies_the_route_test() ->
    persistent_term:put({gateway_cluster_membership, members}, [node(), 'g1@h', 'g2@h']),
    persistent_term:put({gateway_cluster_membership, members_by_role}, #{
        guilds => [node(), 'g1@h', 'g2@h']
    }),
    application:set_env(fluxer_gateway, guild_pinned_only_nodes, [node()]),
    application:set_env(fluxer_gateway, guild_owner_pins, #{?PG => node(), 42 => 'g1@h'}),
    try
        ?assertMatch({pinned, [{ok, _}]}, routes_here(node(), [?PG])),
        ?assertEqual([], release_peers([node(), 'fluxer_gateway@10.9.9.9'], [?PG])),
        ?assertEqual(
            {ok, #{42 => 'g1@h'}}, application:get_env(fluxer_gateway, guild_owner_pins)
        ),
        ?assertEqual(released, routes_here(node(), [?PG]))
    after
        application:unset_env(fluxer_gateway, guild_owner_pins),
        application:unset_env(fluxer_gateway, guild_pinned_only_nodes),
        persistent_term:erase({gateway_cluster_membership, members}),
        persistent_term:erase({gateway_cluster_membership, members_by_role})
    end.

arm_refuses_without_a_prewarmed_copy_test() ->
    ok = process_registry:init(),
    State = #{guild_ids => [?PG], beam => #{binary => <<>>, md5 => <<0:128>>}},
    try
        ?assertEqual(
            {{error, #{reload_failed => [{?PG, {error, not_prewarmed}}]}}, State}, do_arm(State)
        ),
        ?assertEqual(
            undefined, application:get_env(fluxer_gateway, guild_pin_keeper_ever_armed)
        )
    after
        application:unset_env(fluxer_gateway, guild_owner_pins)
    end.

arm_waits_for_a_calm_source_copy_test() ->
    ok = process_registry:init(),
    {ok, Pid} = gen_event:start(),
    Key = process_registry:build_process_key(guild, ?PG),
    true = ets:insert(process_registry_table, {Key, Pid}),
    try
        ?assertEqual([], busy_copies([?PG], [node()], 100)),
        true = erlang:suspend_process(Pid),
        [Pid ! {backlog, N} || N <- lists:seq(1, 101)],
        ?assertEqual([{node(), 101}], busy_copies([?PG], [node()], 100)),
        ?assertEqual([], busy_copies([?PG], [node()], 101))
    after
        ets:delete(process_registry_table, Key),
        exit(Pid, kill)
    end.

arm_pins_no_peer_until_every_peer_is_ready_test() ->
    Down = 'fluxer_gateway@10.255.0.9',
    State = #{guild_ids => [?PG], beam => #{binary => <<>>, md5 => <<0:128>>}, base_md5s => []},
    ?assertMatch(
        {{error, #{unready := [{Down, {error, _}}], next := retry}}, State},
        arm_peers([Down], State)
    ),
    ?assertEqual(undefined, application:get_env(fluxer_gateway, guild_pin_keeper_ever_armed)).

arm_adopts_a_copy_that_already_owns_the_guild_test() ->
    ok = process_registry:init(),
    Key = process_registry:build_process_key(guild, ?PG),
    true = ets:insert(process_registry_table, {Key, self()}),
    try
        ?assertEqual(adopted, refresh_local_copy(?PG))
    after
        ets:delete(process_registry_table, Key)
    end.

router_with_pins_needs_no_load_test() ->
    Beam = #{binary => <<>>, md5 => <<0:128>>},
    ?assertEqual(ok, ensure_router(node(), Beam, [])).

permanent_patch_failure_blocks_the_peer_until_patched_test() ->
    Node = 'fluxer_gateway@10.9.9.8',
    State = #{armed => true, peers => #{}, blocked => #{}},
    Failed = handle_worker_result(
        {patch, Node}, {guilds, {error, {unexpected_router_md5, <<"AB">>}}}, State
    ),
    ?assertEqual(#{Node => {unexpected_router_md5, <<"AB">>}}, maps:get(blocked, Failed)),
    Patched = handle_worker_result({patch, Node}, {guilds, ok}, Failed),
    ?assertEqual(#{}, maps:get(blocked, Patched)),
    receive
        sweep -> ok
    after 0 -> ok
    end.

permanent_patch_errors_are_not_retried_test() ->
    ?assert(permanent_patch_error({unexpected_router_md5, <<"AB">>})),
    ?assert(permanent_patch_error({router_old_code, true})),
    ?assertNot(permanent_patch_error({router_unavailable, {badrpc, nodedown}})).

-endif.
