%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_node_metadata).
-typing([eqwalizer]).

-export([
    node_pod_names/0,
    pod_name_for_node/1,
    cache_node_pod_name/2,
    refresh_node_pod_names/1,
    local_pod_name/0
]).

-define(NODE_POD_NAMES_KEY, {gateway_cluster_membership, node_pod_names}).
-define(LOOKUP_TIMEOUT_MS, 1000).
-define(COLLECT_TIMEOUT_MS, ?LOOKUP_TIMEOUT_MS + 100).

-spec node_pod_names() -> #{node() => binary()}.
node_pod_names() ->
    persistent_term:get(?NODE_POD_NAMES_KEY, #{}).

-spec pod_name_for_node(node()) -> binary() | undefined.
pod_name_for_node(Node) when Node =:= node() ->
    local_pod_name();
pod_name_for_node(Node) ->
    maps:get(Node, node_pod_names(), undefined).

-spec cache_node_pod_name(node(), binary()) -> ok.
cache_node_pod_name(Node, PodName) when
    is_atom(Node), is_binary(PodName), byte_size(PodName) > 0
->
    NodePodNames = node_pod_names(),
    persistent_term:put(?NODE_POD_NAMES_KEY, NodePodNames#{Node => PodName}),
    ok.

-spec refresh_node_pod_names([node()]) -> #{node() => binary()}.
refresh_node_pod_names(Members) ->
    NodePodNames = lookup_pod_names(Members),
    persistent_term:put(?NODE_POD_NAMES_KEY, NodePodNames),
    NodePodNames.

-spec lookup_pod_names([node()]) -> #{node() => binary()}.
lookup_pod_names(Members) ->
    Parent = self(),
    Ref = make_ref(),
    Workers = [spawn_lookup_worker(Node, Parent, Ref) || Node <- Members],
    Deadline = erlang:monotonic_time(millisecond) + ?COLLECT_TIMEOUT_MS,
    Results = collect_results(Ref, length(Workers), #{}, Deadline),
    kill_unfinished_workers(Workers, Results),
    Results.

-spec spawn_lookup_worker(node(), pid(), reference()) -> {pid(), node()}.
spawn_lookup_worker(Node, Parent, Ref) ->
    Pid = spawn(fun() ->
        Parent ! {Ref, self(), Node, safe_pod_name_for_node(Node)}
    end),
    {Pid, Node}.

-spec collect_results(reference(), non_neg_integer(), #{node() => binary()}, integer()) ->
    #{node() => binary()}.
collect_results(_Ref, 0, Results, _Deadline) ->
    Results;
collect_results(Ref, Remaining, Results, Deadline) ->
    TimeoutMs = erlang:max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {Ref, _Pid, Node, PodName} when is_atom(Node) ->
            Results1 = maybe_add_pod_name(Node, PodName, Results),
            collect_results(Ref, Remaining - 1, Results1, Deadline)
    after TimeoutMs ->
        Results
    end.

-spec maybe_add_pod_name(node(), term(), #{node() => binary()}) -> #{node() => binary()}.
maybe_add_pod_name(Node, PodName, Results) when is_binary(PodName), byte_size(PodName) > 0 ->
    Results#{Node => PodName};
maybe_add_pod_name(_Node, _PodName, Results) ->
    Results.

-spec kill_unfinished_workers([{pid(), node()}], #{node() => binary()}) -> ok.
kill_unfinished_workers(Workers, Results) ->
    lists:foreach(fun(Worker) -> kill_unfinished_worker(Worker, Results) end, Workers).

-spec kill_unfinished_worker({pid(), node()}, #{node() => binary()}) -> ok.
kill_unfinished_worker({Pid, Node}, Results) ->
    case maps:is_key(Node, Results) of
        true ->
            ok;
        false ->
            _ = exit(Pid, kill),
            ok
    end.

-spec safe_pod_name_for_node(node()) -> binary() | undefined.
safe_pod_name_for_node(Node) when Node =:= node() ->
    local_pod_name();
safe_pod_name_for_node(Node) ->
    first_defined([
        fun() -> safe_env_rpc(Node, "POD_NAME") end,
        fun() -> safe_env_rpc(Node, "HOSTNAME") end,
        fun() -> safe_hostname_rpc(Node) end
    ]).

-spec local_pod_name() -> binary() | undefined.
local_pod_name() ->
    first_defined([
        fun() -> normalize_pod_name(os:getenv("POD_NAME")) end,
        fun() -> normalize_pod_name(os:getenv("HOSTNAME")) end,
        fun local_hostname/0
    ]).

-spec first_defined([fun(() -> binary() | undefined)]) -> binary() | undefined.
first_defined([]) ->
    undefined;
first_defined([Fun | Rest]) ->
    case Fun() of
        Value when is_binary(Value), byte_size(Value) > 0 -> Value;
        _ -> first_defined(Rest)
    end.

-spec safe_env_rpc(node(), string()) -> binary() | undefined.
safe_env_rpc(Node, Name) ->
    try rpc:call(Node, os, getenv, [Name], ?LOOKUP_TIMEOUT_MS) of
        Value -> normalize_pod_name(Value)
    catch
        throw:_Reason -> undefined;
        error:_Reason -> undefined;
        exit:_Reason -> undefined
    end.

-spec safe_hostname_rpc(node()) -> binary() | undefined.
safe_hostname_rpc(Node) ->
    try rpc:call(Node, inet, gethostname, [], ?LOOKUP_TIMEOUT_MS) of
        {ok, Hostname} -> normalize_pod_name(Hostname);
        _ -> undefined
    catch
        throw:_Reason -> undefined;
        error:_Reason -> undefined;
        exit:_Reason -> undefined
    end.

-spec local_hostname() -> binary() | undefined.
local_hostname() ->
    case hostname_result() of
        {ok, Hostname} -> normalize_pod_name(Hostname);
        _ -> undefined
    end.

-spec hostname_result() -> term().
hostname_result() ->
    eqwalizer:dynamic_cast(inet:gethostname()).

-spec normalize_pod_name(term()) -> binary() | undefined.
normalize_pod_name(false) ->
    undefined;
normalize_pod_name(undefined) ->
    undefined;
normalize_pod_name(Value) when is_binary(Value) ->
    normalize_pod_binary(string:trim(Value));
normalize_pod_name(Value) when is_list(Value) ->
    try unicode:characters_to_binary(eqwalizer:dynamic_cast(Value)) of
        Binary when is_binary(Binary) -> normalize_pod_binary(string:trim(Binary));
        _ -> undefined
    catch
        error:_Reason -> undefined;
        exit:_Reason -> undefined;
        throw:_Reason -> undefined
    end;
normalize_pod_name(_) ->
    undefined.

-spec normalize_pod_binary(binary()) -> binary() | undefined.
normalize_pod_binary(Binary) when byte_size(Binary) > 0 ->
    Binary;
normalize_pod_binary(_) ->
    undefined.
