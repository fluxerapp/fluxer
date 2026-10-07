%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_cluster_membership).
-typing([eqwalizer]).
-behaviour(gen_server).

-export([
    start_link/0, start_link/1,
    members/0,
    is_member/1,
    alive_count/0,
    members_by_role/0,
    subscribe/1,
    unsubscribe/1
]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-export([
    apply_discovered/2,
    maybe_remove_member/2,
    maybe_add_member/2,
    update_members/2,
    add_subscriber/2,
    reconcile_against_connected_nodes/1
]).

-define(MEMBERS_KEY, {gateway_cluster_membership, members}).
-define(ROLE_MEMBERS_KEY, {gateway_cluster_membership, members_by_role}).
-define(ROLE_LOOKUP_TIMEOUT_MS, 1000).
-define(ROLE_LOOKUP_COLLECT_TIMEOUT_MS, ?ROLE_LOOKUP_TIMEOUT_MS + 100).
-define(ROLE_REFRESH_MS, 10000).
-define(DISCOVERY_RESUBSCRIBE_MS, 10000).
-define(NODE_RECONCILE_MS, 30000).

-type options() :: #{
    connect_fun => fun((node()) -> boolean()),
    connected_nodes_fun => fun(() -> [node()]),
    role_fun => fun((node()) -> atom()),
    auto_subscribe => boolean()
}.

-type state() :: #{
    members := [node()],
    members_by_role := #{atom() => [node()]},
    discovered := [node()],
    connect_fun := fun((node()) -> boolean()),
    connected_nodes_fun := fun(() -> [node()]),
    role_fun := fun((node()) -> atom()),
    subscribers := [{pid(), reference()}],
    role_refresh_timer := reference() | undefined,
    discovery_resubscribe_timer := reference() | undefined,
    node_reconcile_timer := reference() | undefined
}.

-spec start_link() -> gen_server:start_ret().
start_link() ->
    start_link(#{}).

-spec start_link(options()) -> gen_server:start_ret().
start_link(Options) when is_map(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

-spec members() -> [node()].
members() ->
    persistent_term:get(?MEMBERS_KEY, [node()]).

-spec members_by_role() -> #{atom() => [node()]}.
members_by_role() ->
    persistent_term:get(?ROLE_MEMBERS_KEY, #{all => [node()]}).

-spec is_member(node()) -> boolean().
is_member(Node) ->
    lists:member(Node, members()).

-spec alive_count() -> non_neg_integer().
alive_count() ->
    length(members()).

-spec subscribe(pid()) -> ok.
subscribe(Pid) when is_pid(Pid) ->
    gen_server:cast(?MODULE, {subscribe, Pid}).

-spec unsubscribe(pid()) -> ok.
unsubscribe(Pid) when is_pid(Pid) ->
    gen_server:cast(?MODULE, {unsubscribe, Pid}).

-spec init(options()) -> {ok, state()}.
init(Options) ->
    process_flag(trap_exit, true),
    erlang:process_flag(fullsweep_after, 0),
    safe_monitor_nodes(true),
    RoleFun = maps:get(role_fun, Options, fun role_for_node/1),
    State0 = #{
        members => [node()],
        members_by_role => role_members([node()], RoleFun),
        discovered => [],
        connect_fun => maps:get(connect_fun, Options, fun default_connect/1),
        connected_nodes_fun =>
            maps:get(connected_nodes_fun, Options, fun default_connected_nodes/0),
        role_fun => RoleFun,
        subscribers => [],
        role_refresh_timer => schedule_role_refresh(),
        discovery_resubscribe_timer => undefined,
        node_reconcile_timer => schedule_node_reconcile()
    },
    persistent_term:put(?MEMBERS_KEY, [node()]),
    persistent_term:put(?ROLE_MEMBERS_KEY, maps:get(members_by_role, State0)),
    _ = gateway_node_metadata:refresh_node_pod_names([node()]),
    State =
        case maps:get(auto_subscribe, Options, true) of
            true ->
                safe_subscribe_discovery(),
                State0#{discovery_resubscribe_timer := schedule_discovery_resubscribe()};
            false ->
                State0
        end,
    {ok, State}.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, term(), state()}.
handle_call(_Request, _From, State) ->
    {reply, {error, unsupported}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({subscribe, Pid}, State) when is_pid(Pid) ->
    {noreply, add_subscriber(Pid, State)};
handle_cast({unsubscribe, Pid}, State) when is_pid(Pid) ->
    {noreply, remove_subscriber(Pid, State)};
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({cluster_peers_changed, Peers}, State) ->
    {noreply, apply_discovered(normalize_nodes(Peers), State)};
handle_info({nodeup, Node}, State) when is_atom(Node) ->
    ok = presence_cache:handle_nodeup(Node),
    {noreply, maybe_add_member(Node, State)};
handle_info({nodedown, Node}, State) when is_atom(Node) ->
    ok = maybe_start_presence_grace(Node, State),
    {noreply, maybe_remove_member(Node, State)};
handle_info(refresh_roles, State) ->
    State1 = reconcile_against_connected_nodes(State),
    _ = gateway_node_metadata:refresh_node_pod_names(maps:get(members, State1, [])),
    {noreply, State1#{role_refresh_timer := schedule_role_refresh()}};
handle_info(reconcile_nodes, State) ->
    State1 = reconcile_against_connected_nodes(State),
    {noreply, State1#{node_reconcile_timer := schedule_node_reconcile()}};
handle_info(resubscribe_discovery, State) ->
    safe_subscribe_discovery(),
    {noreply, State#{discovery_resubscribe_timer := schedule_discovery_resubscribe()}};
handle_info({'DOWN', _MonRef, process, Pid, _Reason}, State) when is_pid(Pid) ->
    {noreply, remove_subscriber(Pid, State)};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    safe_monitor_nodes(false),
    cancel_timer(maps:get(role_refresh_timer, State, undefined)),
    cancel_timer(maps:get(discovery_resubscribe_timer, State, undefined)),
    cancel_timer(maps:get(node_reconcile_timer, State, undefined)),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    erlang:process_flag(fullsweep_after, 0),
    erlang:garbage_collect(),
    {ok, State}.

-spec apply_discovered([node()], state()) -> state().
apply_discovered(Peers, #{connect_fun := Connect} = State) ->
    Self = node(),
    Filtered = [P || P <- Peers, P =/= Self],
    Connected = [P || P <- Filtered, Connect(P)],
    reconcile_against_connected_nodes(State#{discovered := Filtered}, Connected).

-spec normalize_nodes(term()) -> [node()].
normalize_nodes(Nodes) when is_list(Nodes) ->
    [Node || Node <- Nodes, is_atom(Node)];
normalize_nodes(_Nodes) ->
    [].

-spec maybe_add_member(node(), state()) -> state().
maybe_add_member(Node, #{members := Members, discovered := Discovered} = State) ->
    Known = lists:member(Node, Members) orelse lists:member(Node, Discovered),
    case Known of
        true ->
            update_members(lists:usort([Node | Members]), State);
        false ->
            request_discovery_refresh(),
            State
    end.

-spec request_discovery_refresh() -> ok.
request_discovery_refresh() ->
    _ = spawn(fun force_discovery_refresh_safely/0),
    ok.

-spec force_discovery_refresh_safely() -> ok.
force_discovery_refresh_safely() ->
    try gateway_cluster_discovery:force_refresh() of
        _ -> ok
    catch
        throw:_ -> ok;
        error:_ -> ok;
        exit:_ -> ok
    end.

-spec maybe_start_presence_grace(node(), state()) -> ok.
maybe_start_presence_grace(Node, #{members := Members}) ->
    case lists:member(Node, Members) andalso Node =/= node() of
        true -> presence_cache:handle_nodedown(Node);
        false -> ok
    end.

-spec maybe_remove_member(node(), state()) -> state().
maybe_remove_member(Node, #{members := Members} = State) ->
    case lists:member(Node, Members) andalso Node =/= node() of
        true ->
            update_members(lists:delete(Node, Members), State);
        false ->
            State
    end.

-spec update_members([node()], state()) -> state().
update_members(NewMembers, #{members := OldMembers, members_by_role := OldRoleMembers} = State) ->
    RoleFun = maps:get(role_fun, State, fun role_for_node/1),
    NewRoleMembers = role_members(NewMembers, RoleFun, OldRoleMembers),
    case NewMembers =:= OldMembers andalso NewRoleMembers =:= OldRoleMembers of
        true ->
            State;
        false ->
            do_update_members(NewMembers, NewRoleMembers, State)
    end.

-spec do_update_members([node()], #{atom() => [node()]}, state()) -> state().
do_update_members(NewMembers, NewRoleMembers, State) ->
    OldMembers = maps:get(members, State, []),
    record_membership_transitions(OldMembers, NewMembers),
    persistent_term:put(?MEMBERS_KEY, NewMembers),
    persistent_term:put(?ROLE_MEMBERS_KEY, NewRoleMembers),
    _ = gateway_node_metadata:refresh_node_pod_names(NewMembers),
    notify_subscribers(NewMembers, State),
    State#{members := NewMembers, members_by_role := NewRoleMembers}.

-spec role_members([node()], fun((node()) -> atom())) -> #{atom() => [node()]}.
role_members(Members, RoleFun) ->
    role_members(Members, RoleFun, #{}).

-spec role_members([node()], fun((node()) -> atom()), #{atom() => [node()]}) ->
    #{atom() => [node()]}.
role_members(Members, RoleFun, PreviousRoleMembers) ->
    RoleResults = lookup_roles(Members, RoleFun),
    lists:foldl(
        fun(Node, Acc) ->
            Role = resolved_role(Node, RoleResults, PreviousRoleMembers),
            add_role_member(Role, Node, Acc)
        end,
        #{},
        Members
    ).

-spec resolved_role(node(), #{node() => atom()}, #{atom() => [node()]}) -> atom().
resolved_role(Node, RoleResults, PreviousRoleMembers) ->
    Role = maps:get(Node, RoleResults, unknown),
    case valid_role(Role) of
        true -> Role;
        false -> previous_role_for_node(Node, PreviousRoleMembers)
    end.

-spec lookup_roles([node()], fun((node()) -> atom())) -> #{node() => atom()}.
lookup_roles(Members, RoleFun) ->
    Parent = self(),
    Ref = make_ref(),
    Workers = [spawn_role_lookup_worker(Node, Parent, Ref, RoleFun) || Node <- Members],
    Deadline = erlang:monotonic_time(millisecond) + ?ROLE_LOOKUP_COLLECT_TIMEOUT_MS,
    Results = collect_role_results(Ref, length(Workers), #{}, Deadline),
    kill_unfinished_role_workers(Workers, Results),
    Results.

-spec spawn_role_lookup_worker(node(), pid(), reference(), fun((node()) -> atom())) ->
    {pid(), node()}.
spawn_role_lookup_worker(Node, Parent, Ref, RoleFun) ->
    Pid = spawn(fun() ->
        Parent ! {Ref, self(), Node, safe_role_for_node(Node, RoleFun)}
    end),
    {Pid, Node}.

-spec collect_role_results(reference(), non_neg_integer(), #{node() => atom()}, integer()) ->
    #{node() => atom()}.
collect_role_results(_Ref, 0, Results, _Deadline) ->
    Results;
collect_role_results(Ref, Remaining, Results, Deadline) ->
    TimeoutMs = erlang:max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {Ref, _Pid, Node, Role} when is_atom(Node), is_atom(Role) ->
            collect_role_results(Ref, Remaining - 1, Results#{Node => Role}, Deadline);
        {Ref, _Pid, Node, _Role} when is_atom(Node) ->
            collect_role_results(Ref, Remaining - 1, Results#{Node => unknown}, Deadline)
    after TimeoutMs ->
        Results
    end.

-spec kill_unfinished_role_workers([{pid(), node()}], #{node() => atom()}) -> ok.
kill_unfinished_role_workers(Workers, Results) ->
    lists:foreach(
        fun(Worker) -> kill_unfinished_role_worker(Worker, Results) end,
        Workers
    ).

-spec kill_unfinished_role_worker({pid(), node()}, #{node() => atom()}) -> ok.
kill_unfinished_role_worker({Pid, Node}, Results) ->
    case maps:is_key(Node, Results) of
        true ->
            ok;
        false ->
            _ = exit(Pid, kill),
            ok
    end.

-spec add_role_member(atom(), node(), #{atom() => [node()]}) -> #{atom() => [node()]}.
add_role_member(Role, Node, Acc) ->
    case valid_role(Role) of
        true ->
            Nodes = maps:get(Role, Acc, []),
            Acc#{Role => lists:usort([Node | Nodes])};
        false ->
            Acc
    end.

-spec role_for_node(node()) -> atom().
role_for_node(Node) when Node =:= node() ->
    fluxer_gateway_sup:current_role();
role_for_node(Node) ->
    Role = safe_role_rpc(Node),
    case valid_role(Role) of
        true ->
            Role;
        false ->
            unknown
    end.

-spec safe_connected_nodes(state()) -> [node()].
safe_connected_nodes(#{connected_nodes_fun := ConnectedNodesFun}) ->
    try ConnectedNodesFun() of
        Nodes when is_list(Nodes) -> Nodes;
        _ -> []
    catch
        error:_Reason -> [];
        exit:_Reason -> [];
        throw:_Reason -> []
    end.

-spec valid_role(term()) -> boolean().
valid_role(websocket) -> true;
valid_role(sessions) -> true;
valid_role(presence) -> true;
valid_role(guilds) -> true;
valid_role(calls) -> true;
valid_role(push) -> true;
valid_role(all) -> true;
valid_role(_) -> false.

-spec previous_role_for_node(node(), #{atom() => [node()]}) -> atom().
previous_role_for_node(Node, RoleMembers) when is_map(RoleMembers) ->
    case [Role || {Role, Nodes} <- maps:to_list(RoleMembers), lists:member(Node, Nodes)] of
        [Role | _] when is_atom(Role) -> Role;
        [] -> unknown
    end;
previous_role_for_node(_Node, _RoleMembers) ->
    unknown.

-spec safe_role_for_node(node(), fun((node()) -> atom())) -> atom().
safe_role_for_node(Node, RoleFun) ->
    try RoleFun(Node) of
        Role when is_atom(Role) -> Role;
        _ -> unknown
    catch
        error:_Reason -> unknown;
        exit:_Reason -> unknown;
        throw:_Reason -> unknown
    end.

-spec record_membership_transitions([node()], [node()]) -> ok.
record_membership_transitions(OldMembers, NewMembers) ->
    Up = [Node || Node <- NewMembers, not lists:member(Node, OldMembers)],
    Down = [Node || Node <- OldMembers, not lists:member(Node, NewMembers)],
    lists:foreach(fun record_member_up/1, Up),
    lists:foreach(fun record_member_down/1, Down),
    ok.

-spec record_member_up(node()) -> ok.
record_member_up(_Node) ->
    gateway_cluster_metrics:record_membership_transition(up).

-spec record_member_down(node()) -> ok.
record_member_down(_Node) ->
    gateway_cluster_metrics:record_membership_transition(down).

-spec notify_subscribers([node()], state()) -> ok.
notify_subscribers(Members, #{subscribers := Subs}) ->
    Msg = {cluster_membership_changed, Members},
    lists:foreach(fun({Pid, _Ref}) -> Pid ! Msg end, Subs),
    ok.

-spec add_subscriber(pid(), state()) -> state().
add_subscriber(Pid, #{subscribers := Subs, members := Members} = State) ->
    case lists:keyfind(Pid, 1, Subs) of
        false ->
            Ref = erlang:monitor(process, Pid),
            Pid ! {cluster_membership_changed, Members},
            State#{subscribers := [{Pid, Ref} | Subs]};
        _ ->
            State
    end.

-spec remove_subscriber(pid(), state()) -> state().
remove_subscriber(Pid, #{subscribers := Subs} = State) ->
    case lists:keytake(Pid, 1, Subs) of
        {value, {Pid, Ref}, Rest} ->
            erlang:demonitor(Ref, [flush]),
            State#{subscribers := Rest};
        false ->
            State
    end.

-spec default_connect(node()) -> boolean().
default_connect(Node) ->
    case net_kernel:connect_node(Node) of
        true -> true;
        _ -> false
    end.

-spec default_connected_nodes() -> [node()].
default_connected_nodes() ->
    erlang:nodes().

-spec safe_monitor_nodes(boolean()) -> ok.
safe_monitor_nodes(Flag) ->
    try net_kernel:monitor_nodes(Flag) of
        _ -> ok
    catch
        throw:_Reason -> ok;
        error:_Reason -> ok;
        exit:_Reason -> ok
    end.

-spec safe_subscribe_discovery() -> ok.
safe_subscribe_discovery() ->
    try gateway_cluster_discovery:subscribe(self()) of
        _ -> ok
    catch
        throw:_Reason -> ok;
        error:_Reason -> ok;
        exit:_Reason -> ok
    end.

-spec schedule_role_refresh() -> reference().
schedule_role_refresh() ->
    erlang:send_after(?ROLE_REFRESH_MS, self(), refresh_roles).

-spec schedule_discovery_resubscribe() -> reference().
schedule_discovery_resubscribe() ->
    erlang:send_after(?DISCOVERY_RESUBSCRIBE_MS, self(), resubscribe_discovery).

-spec schedule_node_reconcile() -> reference().
schedule_node_reconcile() ->
    erlang:send_after(?NODE_RECONCILE_MS, self(), reconcile_nodes).

-spec reconcile_against_connected_nodes(state()) -> state().
reconcile_against_connected_nodes(State) ->
    reconcile_against_connected_nodes(State, []).

-spec reconcile_against_connected_nodes(state(), [node()]) -> state().
reconcile_against_connected_nodes(#{members := Members} = State, ExtraConnected) ->
    Self = node(),
    ExistingConnected = [
        N
     || N <- safe_connected_nodes(State),
        lists:member(N, Members)
    ],
    Connected = lists:usort([
        N
     || N <- ExtraConnected ++ ExistingConnected,
        N =/= Self,
        is_atom(N)
    ]),
    NewMembers = lists:usort([Self | Connected]),
    Stale = [N || N <- Members, N =/= Self, not lists:member(N, Connected)],
    Added = [N || N <- Connected, not lists:member(N, Members)],
    case Stale =/= [] orelse Added =/= [] of
        true ->
            logger:warning(
                "Gateway cluster membership reconcile: added nodes ~p removed stale nodes ~p",
                [Added, Stale]
            );
        false ->
            ok
    end,
    update_members(NewMembers, State).

-spec cancel_timer(reference() | undefined) -> ok.
cancel_timer(undefined) ->
    ok;
cancel_timer(Ref) when is_reference(Ref) ->
    _ = erlang:cancel_timer(Ref),
    ok.

-spec safe_role_rpc(node()) -> atom().
safe_role_rpc(Node) ->
    try rpc:call(Node, fluxer_gateway_sup, current_role, [], ?ROLE_LOOKUP_TIMEOUT_MS) of
        Result when is_atom(Result) -> Result;
        _ -> unknown
    catch
        throw:Reason when is_atom(Reason) -> Reason;
        throw:_Reason -> unknown;
        error:_Reason -> unknown;
        exit:_Reason -> unknown
    end.
