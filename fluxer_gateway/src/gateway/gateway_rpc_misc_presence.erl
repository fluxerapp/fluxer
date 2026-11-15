%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_rpc_misc_presence).
-typing([eqwalizer]).

-export([
    get_local_voice_state_counts/0,
    collect_and_aggregate_voice_state_counts/1
]).

-define(NODE_RPC_TIMEOUT, 10000).

-spec get_local_voice_state_counts() -> map().
get_local_voice_state_counts() ->
    voice_state_counts_cache:get_local_counts().

-spec collect_and_aggregate_voice_state_counts([node()]) -> map().
collect_and_aggregate_voice_state_counts(Nodes) ->
    TargetNodes = normalize_target_nodes(Nodes),
    NodeCounts = [fetch_node_voice_state_counts(N) || N <- TargetNodes],
    aggregate_voice_state_counts(NodeCounts).

-spec normalize_target_nodes([term()]) -> [node()].
normalize_target_nodes(Nodes) ->
    ValidNodes = valid_nodes(Nodes, []),
    lists:sort(lists:usort(ValidNodes)).

-spec valid_nodes([term()], [node()]) -> [node()].
valid_nodes([], Acc) ->
    Acc;
valid_nodes([NodeValue | Rest], Acc) when is_atom(NodeValue) ->
    case lists:member($@, atom_to_list(NodeValue)) of
        true -> valid_nodes(Rest, [NodeValue | Acc]);
        false -> valid_nodes(Rest, Acc)
    end;
valid_nodes([_NodeValue | Rest], Acc) ->
    valid_nodes(Rest, Acc).

-spec fetch_node_voice_state_counts(node()) -> map().
fetch_node_voice_state_counts(TargetNode) ->
    case
        gateway_rpc_misc_session:safe_node_call(
            TargetNode, get_local_voice_state_counts, [], ?NODE_RPC_TIMEOUT
        )
    of
        VoiceStateCounts when is_map(VoiceStateCounts) ->
            VoiceStateCounts;
        _ ->
            #{
                <<"total_voice_states">> => 0,
                <<"regions">> => [],
                <<"servers">> => []
            }
    end.

-spec aggregate_voice_state_counts([map()]) -> map().
aggregate_voice_state_counts(NodeCounts) ->
    {TotalVoiceStates, RegionCounts, ServerCounts} = lists:foldl(
        fun merge_node_voice_state_counts/2,
        {0, #{}, #{}},
        NodeCounts
    ),
    #{
        <<"total_voice_states">> => TotalVoiceStates,
        <<"regions">> => build_count_entries(RegionCounts, <<"region_id">>),
        <<"servers">> => build_count_entries(ServerCounts, <<"server_id">>)
    }.

-spec merge_node_voice_state_counts(
    map(),
    {non_neg_integer(), #{binary() => non_neg_integer()}, #{binary() => non_neg_integer()}}
) -> {non_neg_integer(), #{binary() => non_neg_integer()}, #{binary() => non_neg_integer()}}.
merge_node_voice_state_counts(NodeCounts, {TotalAcc, RegionAcc, ServerAcc}) ->
    TotalVS = gateway_rpc_misc_session:decode_integer(
        maps:get(<<"total_voice_states">>, NodeCounts, 0)
    ),
    Regions = maps:get(<<"regions">>, NodeCounts, []),
    Servers = maps:get(<<"servers">>, NodeCounts, []),
    {
        TotalAcc + TotalVS,
        merge_count_entries(Regions, <<"region_id">>, RegionAcc),
        merge_count_entries(Servers, <<"server_id">>, ServerAcc)
    }.

-spec merge_count_entries(list(), binary(), #{binary() => non_neg_integer()}) ->
    #{binary() => non_neg_integer()}.
merge_count_entries(Entries, IdKey, Acc) when is_list(Entries) ->
    lists:foldl(
        fun(Entry, InnerAcc) ->
            merge_single_count_entry(Entry, IdKey, InnerAcc)
        end,
        Acc,
        Entries
    );
merge_count_entries(_, _IdKey, Acc) ->
    Acc.

-spec merge_single_count_entry(map(), binary(), #{binary() => non_neg_integer()}) ->
    #{binary() => non_neg_integer()}.
merge_single_count_entry(Entry, IdKey, Acc) ->
    EntryId = maps:get(IdKey, Entry, undefined),
    VoiceStateCount = gateway_rpc_misc_session:decode_integer(
        maps:get(<<"voice_state_count">>, Entry, 0)
    ),
    increment_count(EntryId, VoiceStateCount, Acc).

-spec increment_count(binary() | term(), non_neg_integer(), map()) -> map().
increment_count(EntryId, VoiceStateCount, Acc) when
    is_binary(EntryId), VoiceStateCount > 0
->
    Updater = fun(Count) -> Count + VoiceStateCount end,
    maps:update_with(EntryId, Updater, VoiceStateCount, Acc);
increment_count(_, _, Acc) ->
    Acc.

-spec build_count_entries(#{binary() => non_neg_integer()}, binary()) -> [map()].
build_count_entries(CountsMap, IdKey) ->
    SortedCounts = lists:sort(fun compare_count_pairs/2, maps:to_list(CountsMap)),
    [
        #{IdKey => Id, <<"voice_state_count">> => Count}
     || {Id, Count} <- SortedCounts,
        is_binary(Id),
        is_integer(Count),
        Count > 0
    ].

-spec compare_count_pairs({binary(), integer()}, {binary(), integer()}) -> boolean().
compare_count_pairs({LeftId, LeftCount}, {RightId, RightCount}) ->
    case LeftCount =:= RightCount of
        true -> LeftId =< RightId;
        false -> LeftCount > RightCount
    end.
