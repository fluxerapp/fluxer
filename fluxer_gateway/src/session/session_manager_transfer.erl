%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_manager_transfer).
-typing([eqwalizer]).

-include_lib("fluxer_gateway/include/timeout_config.hrl").

-export([aggregate_counts/2, aggregate_handoff_to_topology/2]).

-export_type([handoff_result/0, shard/0, state/0]).

-type shard() :: #{pid := pid(), ref := reference()}.
-type state() :: #{shards := #{non_neg_integer() => shard()}, shard_count := pos_integer()}.
-type handoff_result() :: #{
    attempted := non_neg_integer(),
    handed_off := non_neg_integer()
}.

-spec aggregate_counts(term(), state()) -> {non_neg_integer(), state()}.
aggregate_counts(Request, State) ->
    Shards = maps:get(shards, State),
    Total = maps:fold(
        fun(_Index, #{pid := Pid}, Acc) ->
            Acc + extract_count(safe_shard_call(Pid, Request))
        end,
        0,
        Shards
    ),
    {Total, State}.

-spec aggregate_handoff_to_topology([node()], state()) -> {handoff_result(), state()}.
aggregate_handoff_to_topology(TargetNodes, State) ->
    Shards = maps:get(shards, State),
    Result = maps:fold(
        fun(_Index, #{pid := Pid}, Acc) ->
            add_handoff_result(
                extract_handoff_result(
                    safe_shard_call(Pid, {handoff_to_topology, TargetNodes})
                ),
                Acc
            )
        end,
        #{attempted => 0, handed_off => 0},
        Shards
    ),
    {Result, State}.

-spec extract_count(term()) -> non_neg_integer().
extract_count({ok, Count}) when is_integer(Count) -> Count;
extract_count(Count) when is_integer(Count) -> Count;
extract_count(_) -> 0.

-spec extract_handoff_result(term()) -> handoff_result().
extract_handoff_result({ok, Result}) ->
    normalize_handoff_result(Result);
extract_handoff_result(Result) ->
    normalize_handoff_result(Result).

-spec normalize_handoff_result(term()) -> handoff_result().
normalize_handoff_result(#{attempted := Attempted, handed_off := HandedOff}) when
    is_integer(Attempted),
    Attempted >= 0,
    is_integer(HandedOff),
    HandedOff >= 0
->
    #{attempted => Attempted, handed_off => HandedOff};
normalize_handoff_result(Count) when is_integer(Count), Count >= 0 ->
    #{attempted => Count, handed_off => Count};
normalize_handoff_result(_InvalidOrUnavailable) ->
    #{attempted => 1, handed_off => 0}.

-spec add_handoff_result(handoff_result(), handoff_result()) -> handoff_result().
add_handoff_result(#{attempted := Attempted, handed_off := HandedOff}, Acc) ->
    Acc#{
        attempted := maps:get(attempted, Acc) + Attempted,
        handed_off := maps:get(handed_off, Acc) + HandedOff
    }.

-spec safe_shard_call(pid(), term()) -> term().
safe_shard_call(Pid, Request) ->
    try
        gen_server:call(Pid, Request, ?DEFAULT_GEN_SERVER_TIMEOUT)
    catch
        error:_Reason -> unavailable;
        exit:_Reason -> unavailable
    end.
