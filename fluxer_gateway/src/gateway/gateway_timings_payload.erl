%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_timings_payload).
-typing([eqwalizer]).

-export([finalize/1, sanitize/1, sanitize_message/1]).

-spec finalize(term()) -> map().
finalize(Recorder0) ->
    Recorder = map_utils:ensure_map(Recorder0),
    Trace = sanitize_trace(
        lists:reverse(gateway_timings:ensure_trace(maps:get(trace, Recorder, [])))
    ),
    build_payload(Recorder, Trace, total_us(Recorder, Trace)).

-spec sanitize(term()) -> map().
sanitize(Timings0) ->
    Timings = map_utils:ensure_map(Timings0),
    Trace = sanitize_trace(
        gateway_timings:ensure_trace(first_present([<<"trace">>, trace], Timings, []))
    ),
    build_payload(Timings, Trace, sanitized_total_us(Timings, Trace)).

-spec sanitize_message(map()) -> map().
sanitize_message(
    #{
        <<"op">> := 0,
        <<"t">> := Event,
        <<"d">> := #{<<"_timings_gw">> := Timings} = Data
    } = Message
) when Event =:= <<"READY">>; Event =:= <<"RESUMED">> ->
    Message#{<<"d">> := Data#{<<"_timings_gw">> := sanitize(Timings)}};
sanitize_message(Message) when is_map(Message) ->
    Message.

-spec build_payload(map(), [map()], non_neg_integer()) -> map().
build_payload(Source, Trace, TotalUs) ->
    #{
        <<"unit">> => <<"microseconds">>,
        <<"total_us">> => TotalUs,
        <<"pod_name">> => recorder_pod_name(Source),
        <<"trace">> => Trace
    }.

-spec sanitize_trace([map()]) -> [map()].
sanitize_trace(Trace) ->
    [sanitize_span(Span) || Span <- Trace].

-spec sanitize_span(map()) -> map().
sanitize_span(Span) ->
    Span0 = #{
        <<"name">> => span_name(Span),
        <<"duration_us">> => span_duration_us(Span)
    },
    Span1 = gateway_timings:maybe_put_remote(remote_info(Span), Span0),
    maybe_put_children(span_children(Span), Span1).

-spec span_name(map()) -> binary().
span_name(Span) ->
    gateway_timings:normalize_key(first_present([<<"name">>, name], Span, <<"unknown">>)).

-spec span_duration_us(map()) -> non_neg_integer().
span_duration_us(Span) ->
    non_negative_integer(first_present([<<"duration_us">>, duration_us], Span, 0)).

-spec remote_info(map()) -> map() | undefined.
remote_info(Span) ->
    case first_present([<<"remote">>, remote], Span, undefined) of
        Remote when is_map(Remote) ->
            sanitize_remote(Remote);
        _ ->
            undefined
    end.

-spec sanitize_remote(map()) -> map() | undefined.
sanitize_remote(Remote) ->
    Operation = optional_binary(first_present([<<"operation">>, operation], Remote, undefined)),
    PodName = remote_pod_name(Remote),
    Remote0 = maybe_put_optional(<<"operation">>, Operation, #{}),
    Remote1 = maybe_put_optional(<<"pod_name">>, PodName, Remote0),
    case map_size(Remote1) of
        0 -> undefined;
        _ -> Remote1
    end.

-spec span_children(map()) -> [map()].
span_children(Span) ->
    case first_present([<<"children">>, children], Span, []) of
        Children when is_list(Children) ->
            [sanitize_span(Child) || Child <- Children, is_map(Child)];
        _ ->
            []
    end.

-spec maybe_put_children([map()], map()) -> map().
maybe_put_children([], Span) ->
    Span;
maybe_put_children(Children, Span) ->
    Span#{<<"children">> => Children}.

-spec total_us(map(), [map()]) -> non_neg_integer().
total_us(Recorder, Trace) ->
    case Trace of
        [] -> local_elapsed_us(Recorder);
        _ -> sum_non_neg_integers([span_duration_us(Span) || Span <- Trace])
    end.

-spec sanitized_total_us(map(), [map()]) -> non_neg_integer().
sanitized_total_us(Timings, Trace) ->
    case Trace of
        [] -> non_negative_integer(first_present([<<"total_us">>, total_us], Timings, 0));
        _ -> sum_non_neg_integers([span_duration_us(Span) || Span <- Trace])
    end.

-spec sum_non_neg_integers([non_neg_integer()]) -> non_neg_integer().
sum_non_neg_integers([]) ->
    0;
sum_non_neg_integers([Value | Rest]) ->
    Value + sum_non_neg_integers(Rest).

-spec local_elapsed_us(map()) -> non_neg_integer().
local_elapsed_us(Recorder) ->
    case recorder_started_here(Recorder) of
        true -> elapsed_us(first_present([started_at_us, <<"started_at_us">>], Recorder, 0));
        false -> 0
    end.

-spec elapsed_us(term()) -> non_neg_integer().
elapsed_us(StartedAtUs) when is_integer(StartedAtUs) ->
    max(erlang:monotonic_time(microsecond) - StartedAtUs, 0);
elapsed_us(_) ->
    0.

-spec recorder_started_here(map()) -> boolean().
recorder_started_here(Recorder) ->
    case first_present([started_node, <<"started_node">>], Recorder, undefined) of
        NodeName when NodeName =:= node() ->
            true;
        NodeName when is_atom(NodeName) ->
            false;
        _ ->
            recorder_node_started_here(Recorder)
    end.

-spec recorder_node_started_here(map()) -> boolean().
recorder_node_started_here(Recorder) ->
    case node_from_map(Recorder) of
        NodeName when NodeName =:= node() -> true;
        undefined -> true;
        NodeName when is_atom(NodeName) -> false
    end.

-spec recorder_pod_name(map()) -> binary().
recorder_pod_name(Recorder) ->
    case optional_binary(first_present([pod_name, <<"pod_name">>], Recorder, undefined)) of
        PodName when is_binary(PodName), byte_size(PodName) > 0 ->
            PodName;
        _ ->
            local_pod_name_or_unknown()
    end.

-spec local_pod_name_or_unknown() -> binary().
local_pod_name_or_unknown() ->
    case gateway_node_metadata:local_pod_name() of
        LocalPodName when is_binary(LocalPodName), byte_size(LocalPodName) > 0 ->
            LocalPodName;
        _ ->
            <<"unknown">>
    end.

-spec remote_pod_name(map()) -> binary() | undefined.
remote_pod_name(Remote) ->
    case optional_binary(first_present([<<"pod_name">>, pod_name], Remote, undefined)) of
        PodName when is_binary(PodName), byte_size(PodName) > 0 ->
            PodName;
        _ ->
            remote_pod_name_from_node(node_from_map(Remote))
    end.

-spec remote_pod_name_from_node(node() | undefined) -> binary() | undefined.
remote_pod_name_from_node(undefined) ->
    undefined;
remote_pod_name_from_node(NodeName) ->
    case gateway_node_metadata:pod_name_for_node(NodeName) of
        PodName when is_binary(PodName), byte_size(PodName) > 0 ->
            PodName;
        _ ->
            gateway_timings:lookup_remote_pod_name(NodeName)
    end.

-spec node_from_map(map()) -> node() | undefined.
node_from_map(Map) ->
    existing_node_atom(
        first_present(
            [<<"erlang_node_name">>, erlang_node_name, <<"node">>, node],
            Map,
            undefined
        )
    ).

-spec existing_node_atom(term()) -> node() | undefined.
existing_node_atom(NodeName) when is_atom(NodeName) ->
    NodeName;
existing_node_atom(NodeName) when is_binary(NodeName), byte_size(NodeName) > 0 ->
    try binary_to_existing_atom(NodeName, utf8) of
        NodeAtom when is_atom(NodeAtom) -> NodeAtom
    catch
        error:_Reason -> undefined
    end;
existing_node_atom(NodeName) when is_list(NodeName), NodeName =/= [] ->
    existing_node_atom(gateway_timings:normalize_key(NodeName));
existing_node_atom(_) ->
    undefined.

-spec first_present([term()], map(), term()) -> term().
first_present([], _Map, Default) ->
    Default;
first_present([Key | Rest], Map, Default) ->
    case maps:find(Key, Map) of
        {ok, Value} -> Value;
        error -> first_present(Rest, Map, Default)
    end.

-spec maybe_put_optional(binary(), binary() | undefined, map()) -> map().
maybe_put_optional(Key, Value, Map) when is_binary(Value), byte_size(Value) > 0 ->
    Map#{Key => Value};
maybe_put_optional(_Key, _Value, Map) ->
    Map.

-spec optional_binary(term()) -> binary() | undefined.
optional_binary(undefined) ->
    undefined;
optional_binary(Value) ->
    case gateway_timings:normalize_key(Value) of
        Binary when byte_size(Binary) > 0 -> Binary;
        _ -> undefined
    end.

-spec non_negative_integer(term()) -> non_neg_integer().
non_negative_integer(Value) when is_integer(Value), Value >= 0 ->
    Value;
non_negative_integer(_) ->
    0.
