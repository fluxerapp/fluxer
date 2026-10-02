%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_timings).
-typing([eqwalizer]).

-export([
    new/0,
    start/0,
    elapsed_us/1,
    record/3,
    record/4,
    record_function/4,
    record_function/5,
    span/2,
    span/3,
    merge/2,
    finalize/1,
    from_state/1,
    put_state/2,
    merge_state/2,
    record_node_hit/3,
    record_api_node_from_session_result/2,
    api_remote_from_session_result/1,
    remote_node/2,
    is_recorder/1
]).

-export_type([recorder/0]).

-define(REMOTE_POD_LOOKUP_TIMEOUT_MS, 100).

-type recorder() :: #{
    started_at_us := integer(),
    started_node := node() | undefined,
    steps := map(),
    nodes := [map()],
    trace := [map()],
    pod_name := binary(),
    _ => _
}.

-spec new() -> recorder().
new() ->
    PodName = pod_name(),
    #{
        started_at_us => start(),
        started_node => node(),
        steps => #{},
        nodes => [],
        trace => [],
        pod_name => PodName
    }.

-spec start() -> integer().
start() ->
    erlang:monotonic_time(microsecond).

-spec elapsed_us(integer()) -> non_neg_integer().
elapsed_us(StartedAtUs) when is_integer(StartedAtUs) ->
    max(start() - StartedAtUs, 0);
elapsed_us(_) ->
    0.

-spec record(term(), integer(), recorder()) -> recorder().
record(Name, StartedAtUs, Recorder) ->
    record_function(Name, Name, StartedAtUs, Recorder).

-spec record(term(), integer(), map(), recorder()) -> recorder().
record(Name, StartedAtUs, ChildSteps, Recorder0) ->
    record_function_with_child_steps(Name, Name, StartedAtUs, ChildSteps, #{}, Recorder0).

-spec record_function(term(), term(), integer(), recorder()) -> recorder().
record_function(StepName, FunctionName, StartedAtUs, Recorder) ->
    record_function(StepName, FunctionName, StartedAtUs, #{}, Recorder).

-spec record_function(term(), term(), integer(), map(), recorder()) -> recorder().
record_function(StepName, FunctionName, StartedAtUs, TraceMeta, Recorder0) ->
    record_function_with_child_steps(
        StepName, FunctionName, StartedAtUs, #{}, TraceMeta, Recorder0
    ).

-spec span(term(), integer()) -> map().
span(FunctionName, StartedAtUs) ->
    span(FunctionName, StartedAtUs, #{}).

-spec span(term(), integer(), map()) -> map().
span(FunctionName, StartedAtUs, TraceMeta) ->
    trace_span(FunctionName, elapsed_us(StartedAtUs), TraceMeta).

-spec record_function_with_child_steps(
    term(), term(), integer(), map(), map(), recorder()
) -> recorder().
record_function_with_child_steps(
    StepName, FunctionName, StartedAtUs, ChildSteps, TraceMeta, Recorder0
) ->
    Recorder = ensure_recorder(Recorder0),
    DurationUs = elapsed_us(StartedAtUs),
    Step0 = #{<<"duration_us">> => DurationUs},
    Step = maybe_put_steps(Step0, ChildSteps),
    Recorder1 = put_step(normalize_key(StepName), Step, Recorder),
    case trace_enabled(TraceMeta) of
        true -> append_trace(FunctionName, DurationUs, TraceMeta, Recorder1);
        false -> Recorder1
    end.

-spec merge(recorder(), term()) -> recorder().
merge(Recorder0, Recorder1) ->
    Recorder = ensure_recorder(Recorder0),
    case is_recorder(Recorder1) of
        true ->
            Other = ensure_recorder(Recorder1),
            Steps = merge_steps(
                maps:get(steps, Recorder, #{}),
                maps:get(steps, Other, #{})
            ),
            Nodes = merge_nodes(
                maps:get(nodes, Recorder, []),
                maps:get(nodes, Other, [])
            ),
            Trace = maps:get(trace, Other, []) ++ maps:get(trace, Recorder, []),
            Recorder#{steps := Steps, nodes := Nodes, trace => Trace};
        false ->
            Recorder
    end.

-spec finalize(term()) -> map().
finalize(Recorder0) ->
    Recorder = ensure_recorder(Recorder0),
    Trace = lists:reverse(maps:get(trace, Recorder, [])),
    #{
        <<"unit">> => <<"microseconds">>,
        <<"total_us">> => total_us(Recorder, Trace),
        <<"pod_name">> => maps:get(pod_name, Recorder, pod_name()),
        <<"trace">> => Trace
    }.

-spec from_state(map()) -> recorder().
from_state(State) when is_map(State) ->
    Candidate = maps:get(gw_timings, State, undefined),
    case is_recorder(Candidate) of
        true -> ensure_recorder(Candidate);
        false -> new()
    end;
from_state(_) ->
    new().

-spec put_state(recorder(), map()) -> map().
put_state(Recorder, State) when is_map(State) ->
    State#{gw_timings => ensure_recorder(Recorder)}.

-spec merge_state(term(), map()) -> map().
merge_state(Update, State) ->
    put_state(merge(from_state(State), Update), State).

-spec record_node_hit(term(), term(), recorder()) -> recorder().
record_node_hit(_Role, Node, Recorder0) when Node =:= node() ->
    ensure_recorder(Recorder0);
record_node_hit(Role, Node, Recorder0) ->
    Recorder = ensure_recorder(Recorder0),
    case remote_node(Role, Node) of
        NodeInfo when is_map(NodeInfo) ->
            Recorder#{nodes := merge_nodes(maps:get(nodes, Recorder, []), [NodeInfo])};
        _ ->
            Recorder
    end.

-spec record_api_node_from_session_result(term(), recorder()) -> recorder().
record_api_node_from_session_result({ok, Data}, Recorder) when is_map(Data) ->
    case maps:get(<<"_timings">>, Data, undefined) of
        Timings when is_map(Timings) ->
            record_external_node(<<"api">>, Timings, Recorder);
        _ ->
            Recorder
    end;
record_api_node_from_session_result(_, Recorder) ->
    Recorder.

-spec api_remote_from_session_result(term()) -> map() | undefined.
api_remote_from_session_result({ok, Data}) when is_map(Data) ->
    case maps:get(<<"_timings">>, Data, undefined) of
        Timings when is_map(Timings) ->
            external_node_info(<<"api">>, Timings);
        _ ->
            undefined
    end;
api_remote_from_session_result(_) ->
    undefined.

-spec remote_node(term(), term()) -> map() | undefined.
remote_node(_Operation, NodeName) when NodeName =:= node() ->
    undefined;
remote_node(Operation, NodeName) when is_atom(NodeName) ->
    maybe_put_pod_name(
        remote_pod_name(NodeName),
        #{<<"operation">> => normalize_key(Operation)}
    );
remote_node(Operation, _NodeName) ->
    #{<<"operation">> => normalize_key(Operation)}.

-spec is_recorder(term()) -> boolean().
is_recorder(#{started_at_us := StartedAtUs, steps := Steps, nodes := Nodes}) ->
    is_integer(StartedAtUs) andalso is_map(Steps) andalso is_list(Nodes);
is_recorder(_) ->
    false.

-spec ensure_recorder(term()) -> recorder().
ensure_recorder(
    #{
        started_at_us := StartedAtUs,
        steps := Steps,
        nodes := Nodes
    } = Recorder
) when is_integer(StartedAtUs), is_map(Steps), is_list(Nodes) ->
    StartedNode = ensure_started_node(Recorder),
    Recorder1 = Recorder#{
        started_node => StartedNode,
        nodes => ensure_nodes(Nodes),
        trace => ensure_trace(maps:get(trace, Recorder, [])),
        pod_name => runtime_binary(maps:get(pod_name, Recorder, pod_name()))
    },
    Recorder1;
ensure_recorder(_) ->
    new().

-spec total_us(recorder(), [map()]) -> non_neg_integer().
total_us(Recorder, Trace) ->
    case Trace of
        [] -> local_elapsed_us(Recorder);
        _ -> trace_total_us(Trace)
    end.

-spec local_elapsed_us(recorder()) -> non_neg_integer().
local_elapsed_us(Recorder) ->
    case maps:get(started_node, Recorder, undefined) of
        NodeName when NodeName =:= node() ->
            elapsed_us(maps:get(started_at_us, Recorder, start()));
        _ ->
            0
    end.

-spec trace_total_us([map()]) -> non_neg_integer().
trace_total_us(Trace) ->
    sum_non_neg_integers([span_duration_us(Span) || Span <- Trace]).

-spec sum_non_neg_integers([non_neg_integer()]) -> non_neg_integer().
sum_non_neg_integers([]) ->
    0;
sum_non_neg_integers([Value | Rest]) ->
    Value + sum_non_neg_integers(Rest).

-spec span_duration_us(map()) -> non_neg_integer().
span_duration_us(Span) ->
    case maps:get(<<"duration_us">>, Span, 0) of
        Value when is_integer(Value), Value >= 0 -> Value;
        _ -> 0
    end.

-spec ensure_started_node(map()) -> node() | undefined.
ensure_started_node(Recorder) ->
    case maps:get(started_node, Recorder, undefined) of
        NodeName when is_atom(NodeName) -> NodeName;
        _ -> recorder_node(Recorder)
    end.

-spec ensure_trace(term()) -> [map()].
ensure_trace(Trace) when is_list(Trace) ->
    [Span || Span <- Trace, is_map(Span)];
ensure_trace(_) ->
    [].

-spec ensure_nodes(term()) -> [map()].
ensure_nodes(Nodes) when is_list(Nodes) ->
    [NodeInfo || NodeInfo <- Nodes, is_map(NodeInfo)];
ensure_nodes(_) ->
    [].

-spec runtime_binary(term()) -> binary().
runtime_binary(Value) ->
    normalize_key(Value).

-spec put_step(binary(), map(), recorder()) -> recorder().
put_step(Key, Step, Recorder) ->
    Steps = maps:get(steps, Recorder, #{}),
    Existing = maps:get(Key, Steps, undefined),
    Recorder#{steps := Steps#{Key => merge_step(Existing, Step)}}.

-spec merge_step(term(), map()) -> map().
merge_step(undefined, Step) ->
    Step;
merge_step(Existing, Step) when is_map(Existing) ->
    Duration0 = step_duration(Existing),
    Duration1 = step_duration(Step),
    Count = step_count(Existing) + 1,
    Merged0 = Existing#{
        <<"duration_us">> => Duration0 + Duration1,
        <<"count">> => Count,
        <<"min_us">> => min(step_min(Existing, Duration0), Duration1),
        <<"max_us">> => max(step_max(Existing, Duration0), Duration1)
    },
    MergedSteps = merge_steps(step_child_steps(Existing), step_child_steps(Step)),
    maybe_put_steps(Merged0, MergedSteps);
merge_step(_, Step) ->
    Step.

-spec merge_steps(map(), map()) -> map().
merge_steps(Left, Right) when is_map(Left), is_map(Right) ->
    maps:fold(
        fun(Key, Step, Acc) ->
            Acc#{Key => merge_step(maps:get(Key, Acc, undefined), Step)}
        end,
        Left,
        Right
    ).

-spec step_duration(map()) -> non_neg_integer().
step_duration(Step) ->
    case maps:get(<<"duration_us">>, Step, 0) of
        Value when is_integer(Value), Value >= 0 -> Value;
        _ -> 0
    end.

-spec step_count(map()) -> pos_integer().
step_count(Step) ->
    case maps:get(<<"count">>, Step, 1) of
        Value when is_integer(Value), Value >= 1 -> Value;
        _ -> 1
    end.

-spec step_min(map(), non_neg_integer()) -> non_neg_integer().
step_min(Step, Default) ->
    case maps:get(<<"min_us">>, Step, Default) of
        Value when is_integer(Value), Value >= 0 -> Value;
        _ -> Default
    end.

-spec step_max(map(), non_neg_integer()) -> non_neg_integer().
step_max(Step, Default) ->
    case maps:get(<<"max_us">>, Step, Default) of
        Value when is_integer(Value), Value >= 0 -> Value;
        _ -> Default
    end.

-spec step_child_steps(map()) -> map().
step_child_steps(Step) ->
    case maps:get(<<"steps">>, Step, #{}) of
        Value when is_map(Value) -> Value;
        _ -> #{}
    end.

-spec maybe_put_steps(map(), term()) -> map().
maybe_put_steps(Step, ChildSteps) when is_map(ChildSteps), map_size(ChildSteps) > 0 ->
    Step#{<<"steps">> => ChildSteps};
maybe_put_steps(Step, _) ->
    maps:remove(<<"steps">>, Step).

-spec record_external_node(binary(), map(), recorder()) -> recorder().
record_external_node(Role, Timings, Recorder0) ->
    Recorder = ensure_recorder(Recorder0),
    NodeInfo = external_node_info(Role, Timings),
    Recorder#{nodes := merge_nodes(maps:get(nodes, Recorder, []), [NodeInfo])}.

-spec external_node_info(binary(), map()) -> map().
external_node_info(Role, Timings) ->
    NodeInfo0 = #{<<"operation">> => Role},
    lists:foldl(
        fun(Key, Acc) -> put_external_runtime_key(Key, Timings, Acc) end,
        NodeInfo0,
        runtime_metadata_keys()
    ).

-spec runtime_metadata_keys() -> [binary()].
runtime_metadata_keys() ->
    [<<"pod_name">>].

-spec put_external_runtime_key(binary(), map(), map()) -> map().
put_external_runtime_key(Key, Timings, Acc) ->
    case maps:get(Key, Timings, undefined) of
        Value when is_binary(Value), byte_size(Value) > 0 -> Acc#{Key => Value};
        Value when is_list(Value), Value =/= [] -> Acc#{Key => normalize_key(Value)};
        _ -> Acc
    end.

-spec append_trace(term(), non_neg_integer(), map(), recorder()) -> recorder().
append_trace(FunctionName, DurationUs, TraceMeta, Recorder) ->
    Span = trace_span(FunctionName, DurationUs, TraceMeta),
    Recorder#{trace => [Span | maps:get(trace, Recorder, [])]}.

-spec trace_span(term(), non_neg_integer(), map()) -> map().
trace_span(FunctionName, DurationUs, TraceMeta) ->
    Span0 = #{
        <<"name">> => normalize_key(FunctionName),
        <<"duration_us">> => DurationUs
    },
    Span1 = maybe_put_remote(trace_meta_value(remote, TraceMeta), Span0),
    maybe_put_children(trace_meta_value(children, TraceMeta), Span1).

-spec trace_meta_value(atom(), map()) -> term().
trace_meta_value(Key, Meta) when is_map(Meta), is_atom(Key) ->
    BinaryKey = atom_to_binary(Key, utf8),
    maps:get(Key, Meta, maps:get(BinaryKey, Meta, undefined));
trace_meta_value(_, _) ->
    undefined.

-spec trace_enabled(map()) -> boolean().
trace_enabled(TraceMeta) ->
    case trace_meta_value(trace, TraceMeta) of
        false -> false;
        _ -> true
    end.

-spec maybe_put_remote(term(), map()) -> map().
maybe_put_remote(Remote, Span) when is_map(Remote), map_size(Remote) > 0 ->
    Span#{<<"remote">> => Remote};
maybe_put_remote(_, Span) ->
    Span.

-spec maybe_put_pod_name(term(), map()) -> map().
maybe_put_pod_name(PodName, NodeInfo) when is_binary(PodName), byte_size(PodName) > 0 ->
    NodeInfo#{<<"pod_name">> => PodName};
maybe_put_pod_name(_PodName, NodeInfo) ->
    NodeInfo.

-spec maybe_put_children(term(), map()) -> map().
maybe_put_children(Children0, Span) when is_list(Children0) ->
    Children = valid_trace_children(Children0),
    case Children of
        [] -> Span;
        _ -> Span#{<<"children">> => Children}
    end;
maybe_put_children(_, Span) ->
    Span.

-spec valid_trace_children([term()]) -> [map()].
valid_trace_children(Children) ->
    [
        Child
     || Child <- Children,
        is_map(Child),
        maps:is_key(<<"name">>, Child),
        maps:is_key(<<"duration_us">>, Child)
    ].

-spec merge_nodes([map()], [map()]) -> [map()].
merge_nodes(Existing, NewNodes) ->
    lists:foldl(fun add_node_info/2, Existing, NewNodes).

-spec add_node_info(map(), [map()]) -> [map()].
add_node_info(NodeInfo, Nodes) ->
    Key = node_info_key(NodeInfo),
    case node_info_seen(Key, Nodes) of
        true -> Nodes;
        false -> [NodeInfo | Nodes]
    end.

-spec node_info_seen({term(), term()}, [map()]) -> boolean().
node_info_seen(Key, Nodes) ->
    lists:any(fun(Existing) -> node_info_key(Existing) =:= Key end, Nodes).

-spec node_info_key(map()) -> {term(), term()}.
node_info_key(NodeInfo) ->
    {
        maps:get(<<"operation">>, NodeInfo, undefined),
        maps:get(<<"pod_name">>, NodeInfo, undefined)
    }.

-spec recorder_node(map()) -> node() | undefined.
recorder_node(Recorder) ->
    case maps:get(erlang_node_name, Recorder, undefined) of
        NodeName when is_atom(NodeName) ->
            NodeName;
        NodeName when is_binary(NodeName), byte_size(NodeName) > 0 ->
            existing_node_atom(NodeName);
        NodeName when is_list(NodeName), NodeName =/= [] ->
            existing_node_atom(normalize_key(NodeName));
        _ ->
            undefined
    end.

-spec existing_node_atom(binary()) -> node() | undefined.
existing_node_atom(NodeName) ->
    try binary_to_existing_atom(NodeName, utf8) of
        NodeAtom when is_atom(NodeAtom) -> NodeAtom
    catch
        error:_Reason -> undefined
    end.

-spec remote_pod_name(node()) -> binary() | undefined.
remote_pod_name(NodeName) ->
    case gateway_node_metadata:pod_name_for_node(NodeName) of
        PodName when is_binary(PodName), byte_size(PodName) > 0 ->
            PodName;
        _ ->
            lookup_remote_pod_name(NodeName)
    end.

-spec lookup_remote_pod_name(node()) -> binary() | undefined.
lookup_remote_pod_name(NodeName) ->
    try
        rpc:call(
            NodeName,
            gateway_node_metadata,
            local_pod_name,
            [],
            ?REMOTE_POD_LOOKUP_TIMEOUT_MS
        )
    of
        PodName when is_binary(PodName), byte_size(PodName) > 0 ->
            ok = gateway_node_metadata:cache_node_pod_name(NodeName, PodName),
            PodName;
        _ ->
            undefined
    catch
        throw:_Reason -> undefined;
        error:_Reason -> undefined;
        exit:_Reason -> undefined
    end.

-spec pod_name() -> binary().
pod_name() ->
    first_runtime_name(["POD_NAME", "HOSTNAME"], hostname()).

-spec first_runtime_name([string()], binary()) -> binary().
first_runtime_name([], Fallback) ->
    Fallback;
first_runtime_name([Name | Rest], Fallback) ->
    case fluxer_gateway_config:env_value(Name) of
        undefined -> first_runtime_name(Rest, Fallback);
        Value -> normalize_key(Value)
    end.

-spec hostname() -> binary().
hostname() ->
    case hostname_result() of
        {ok, Hostname} -> normalize_key(Hostname);
        _ -> <<"unknown">>
    end.

-spec hostname_result() -> term().
hostname_result() ->
    eqwalizer:dynamic_cast(inet:gethostname()).

-spec normalize_key(term()) -> binary().
normalize_key(Value) when is_binary(Value) ->
    Value;
normalize_key(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
normalize_key(Value) when is_integer(Value) ->
    integer_to_binary(Value);
normalize_key(Value) when is_list(Value) ->
    case unicode:characters_to_binary(eqwalizer:dynamic_cast(Value)) of
        Binary when is_binary(Binary) -> Binary;
        _ -> fallback_binary(Value)
    end;
normalize_key(Value) ->
    fallback_binary(Value).

-spec fallback_binary(term()) -> binary().
fallback_binary(Value) ->
    iolist_to_binary(io_lib:format("~p", [Value])).
