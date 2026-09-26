%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(presence_cache_anti_entropy).
-typing([eqwalizer]).

-export([
    schedule_anti_entropy/0,
    cancel_anti_entropy_timer/1,
    perform_anti_entropy/1,
    handle_anti_entropy_request/3,
    handle_anti_entropy_digest_request/3,
    merge_anti_entropy_entries/2,
    record_delete/2,
    record_put/3
]).

-export_type([state/0]).

-define(ANTI_ENTROPY_INTERVAL_MS, 30000).
-define(ANTI_ENTROPY_MSG, anti_entropy_tick).
-define(SNAPSHOT_CHUNK_SIZE, 500).
-define(TOMBSTONE_TTL_MS, 90000).
-define(TOMBSTONE_LIMIT, 50000).

-type state() :: map().

-spec schedule_anti_entropy() -> reference().
schedule_anti_entropy() ->
    erlang:send_after(?ANTI_ENTROPY_INTERVAL_MS, self(), ?ANTI_ENTROPY_MSG).

-spec cancel_anti_entropy_timer(state()) -> ok.
cancel_anti_entropy_timer(State) ->
    case maps:get(anti_entropy_timer, State, undefined) of
        TimerRef when is_reference(TimerRef) ->
            _ = erlang:cancel_timer(TimerRef),
            ok;
        _ ->
            ok
    end.

-spec perform_anti_entropy(state()) -> state().
perform_anti_entropy(State) ->
    case persistent_term:get(presence_noop, false) of
        true -> State;
        false -> broadcast_anti_entropy_requests(prune_tombstones(State))
    end.

-spec handle_anti_entropy_request(node(), non_neg_integer(), state()) -> {noreply, state()}.
handle_anti_entropy_request(FromNode, RemoteGeneration, State) ->
    LocalGeneration = maps:get(generation, State, 0),
    case LocalGeneration =:= RemoteGeneration of
        true -> {noreply, State};
        false -> send_anti_entropy_response(FromNode, State)
    end.

-spec handle_anti_entropy_digest_request(node(), binary(), state()) -> {noreply, state()}.
handle_anti_entropy_digest_request(FromNode, RemoteDigest, State) ->
    LocalDigest = presence_cache_shards:content_digest(State),
    case LocalDigest =:= RemoteDigest of
        true -> {noreply, State};
        false -> send_anti_entropy_response(FromNode, State)
    end.

-spec merge_anti_entropy_entries(#{integer() => map()}, state()) -> state().
merge_anti_entropy_entries(Entries, State) when is_map(Entries) ->
    NewState = maps:fold(
        fun merge_single_entry/3,
        State,
        Entries
    ),
    erlang:garbage_collect(),
    NewState;
merge_anti_entropy_entries(_, State) ->
    State.

-spec record_delete(integer(), state()) -> state().
record_delete(UserId, State) ->
    ExpiresAt = now_ms() + ?TOMBSTONE_TTL_MS,
    Tombstones = (tombstones(State))#{UserId => ExpiresAt},
    Order = queue:in({ExpiresAt, UserId}, tombstone_order(State)),
    evict_over_limit(State#{delete_tombstones => Tombstones, delete_tombstone_order => Order}).

-spec record_put(integer(), map(), state()) -> state().
record_put(UserId, Presence, State) ->
    case is_visible_presence(Presence) of
        true -> clear_tombstone(UserId, State);
        false -> record_delete(UserId, State)
    end.

-spec clear_tombstone(integer(), state()) -> state().
clear_tombstone(UserId, State) ->
    Tombstones = tombstones(State),
    case maps:is_key(UserId, Tombstones) of
        true -> State#{delete_tombstones => maps:remove(UserId, Tombstones)};
        false -> State
    end.

-spec tombstones(state()) -> #{integer() => integer()}.
tombstones(State) ->
    case maps:get(delete_tombstones, State, #{}) of
        Tombstones when is_map(Tombstones) -> Tombstones;
        _ -> #{}
    end.

-spec tombstone_order(state()) -> queue:queue({integer(), integer()}).
tombstone_order(State) ->
    Order = maps:get(delete_tombstone_order, State, undefined),
    case queue:is_queue(Order) of
        true -> Order;
        false -> queue:new()
    end.

-spec prune_tombstones(state()) -> state().
prune_tombstones(State) ->
    Now = now_ms(),
    State#{
        delete_tombstones => drop_expired(tombstones(State), Now),
        delete_tombstone_order => drop_expired_order(tombstone_order(State), Now)
    }.

-spec drop_expired(#{integer() => integer()}, integer()) -> #{integer() => integer()}.
drop_expired(Tombstones, Now) ->
    maps:filter(fun(_UserId, ExpiresAt) -> ExpiresAt > Now end, Tombstones).

-spec drop_expired_order(queue:queue({integer(), integer()}), integer()) ->
    queue:queue({integer(), integer()}).
drop_expired_order(Order, Now) ->
    case queue:peek(Order) of
        {value, {ExpiresAt, _UserId}} when ExpiresAt =< Now ->
            drop_expired_order(queue:drop(Order), Now);
        _ ->
            Order
    end.

-spec evict_over_limit(state()) -> state().
evict_over_limit(State) ->
    Tombstones = tombstones(State),
    case maps:size(Tombstones) > ?TOMBSTONE_LIMIT of
        true -> evict_oldest_tombstone(Tombstones, tombstone_order(State), State);
        false -> State
    end.

-spec evict_oldest_tombstone(
    #{integer() => integer()}, queue:queue({integer(), integer()}), state()
) -> state().
evict_oldest_tombstone(Tombstones, Order, State) ->
    case queue:out(Order) of
        {{value, {ExpiresAt, UserId}}, Rest} ->
            Kept = drop_if_current(UserId, ExpiresAt, Tombstones),
            evict_over_limit(State#{delete_tombstones => Kept, delete_tombstone_order => Rest});
        {empty, _Order} ->
            State
    end.

-spec drop_if_current(integer(), integer(), #{integer() => integer()}) ->
    #{integer() => integer()}.
drop_if_current(UserId, ExpiresAt, Tombstones) ->
    case maps:get(UserId, Tombstones, undefined) of
        ExpiresAt -> maps:remove(UserId, Tombstones);
        _ -> Tombstones
    end.

-spec now_ms() -> integer().
now_ms() ->
    erlang:monotonic_time(millisecond).

-spec broadcast_anti_entropy_requests(state()) -> state().
broadcast_anti_entropy_requests(State) ->
    Digest = presence_cache_shards:content_digest(State),
    PeerNodes = [N || N <- gateway_node_router:active_nodes(presence), N =/= node()],
    lists:foreach(
        fun(PeerNode) ->
            safe_cast_anti_entropy(PeerNode, Digest)
        end,
        PeerNodes
    ),
    State.

-spec safe_cast_anti_entropy(node(), binary()) -> ok.
safe_cast_anti_entropy(PeerNode, Digest) ->
    try
        gen_server:cast(
            {presence_cache, PeerNode}, {anti_entropy_digest_request, node(), Digest}
        )
    catch
        error:_ -> ok;
        exit:_ -> ok
    end.

-spec send_anti_entropy_response(node(), state()) -> {noreply, state()}.
send_anti_entropy_response(FromNode, State) ->
    Chunks = presence_cache_shards:local_snapshot_chunks(State, ?SNAPSHOT_CHUNK_SIZE),
    lists:foreach(fun(Chunk) -> safe_cast_response(FromNode, Chunk) end, Chunks),
    {noreply, State}.

-spec safe_cast_response(node(), #{integer() => map()}) -> ok.
safe_cast_response(FromNode, Chunk) ->
    try
        gen_server:cast({presence_cache, FromNode}, {anti_entropy_response, Chunk})
    catch
        error:_ -> ok;
        exit:_ -> ok
    end,
    ok.

-spec merge_single_entry(term(), term(), state()) -> state().
merge_single_entry(UserId, Presence, AccState) when
    is_integer(UserId), UserId > 0, is_map(Presence)
->
    OwnerNodes = presence_cache_bulk:resolve_owner_nodes(UserId),
    case lists:member(node(), OwnerNodes) of
        true -> merge_if_missing(UserId, Presence, AccState);
        false -> AccState
    end;
merge_single_entry(_UserId, _Presence, AccState) ->
    AccState.

-spec merge_if_missing(integer(), map(), state()) -> state().
merge_if_missing(UserId, Presence, State) ->
    case presence_cache_bulk:get_local_fast(UserId) of
        not_found ->
            merge_unless_deleted(UserId, Presence, State);
        {ok, _} ->
            State
    end.

-spec merge_unless_deleted(integer(), map(), state()) -> state().
merge_unless_deleted(UserId, Presence, State) ->
    Tombstones = tombstones(State),
    Now = now_ms(),
    case maps:get(UserId, Tombstones, undefined) of
        ExpiresAt when is_integer(ExpiresAt), ExpiresAt > Now ->
            State;
        undefined ->
            merge_if_visible(UserId, Presence, State);
        _ ->
            merge_if_visible(
                UserId, Presence, State#{delete_tombstones => maps:remove(UserId, Tombstones)}
            )
    end.

-spec merge_if_visible(integer(), map(), state()) -> state().
merge_if_visible(UserId, Presence, State) ->
    case is_visible_presence(Presence) of
        true ->
            {_Reply, NewState} = presence_cache:put_local(UserId, Presence, State),
            presence_cache_rebalance:increment_generation(NewState);
        false ->
            State
    end.

-spec is_visible_presence(map()) -> boolean().
is_visible_presence(Presence) ->
    case maps:get(<<"status">>, Presence, <<"offline">>) of
        <<"online">> -> true;
        <<"idle">> -> true;
        <<"dnd">> -> true;
        _ -> false
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

schedule_anti_entropy_returns_ref_test() ->
    Ref = schedule_anti_entropy(),
    ?assert(is_reference(Ref)),
    _ = erlang:cancel_timer(Ref).

cancel_anti_entropy_timer_noop_undefined_test() ->
    ?assertEqual(ok, cancel_anti_entropy_timer(#{})),
    ?assertEqual(ok, cancel_anti_entropy_timer(#{anti_entropy_timer => undefined})).

cancel_anti_entropy_timer_cancels_ref_test() ->
    Ref = erlang:send_after(60000, self(), test),
    ?assertEqual(ok, cancel_anti_entropy_timer(#{anti_entropy_timer => Ref})).

is_visible_presence_online_test() ->
    ?assertEqual(true, is_visible_presence(#{<<"status">> => <<"online">>})),
    ?assertEqual(true, is_visible_presence(#{<<"status">> => <<"idle">>})),
    ?assertEqual(true, is_visible_presence(#{<<"status">> => <<"dnd">>})).

is_visible_presence_offline_test() ->
    ?assertEqual(false, is_visible_presence(#{<<"status">> => <<"offline">>})),
    ?assertEqual(false, is_visible_presence(#{<<"status">> => <<"invisible">>})),
    ?assertEqual(false, is_visible_presence(#{})).

merge_single_entry_filters_invalid_test() ->
    State = #{generation => 0},
    ?assertEqual(State, merge_single_entry(<<"bad">>, #{}, State)),
    ?assertEqual(State, merge_single_entry(-1, #{}, State)),
    ?assertEqual(State, merge_single_entry(1, not_a_map, State)).

prune_tombstones_drops_expired_order_test() ->
    Now = now_ms(),
    State = #{
        delete_tombstones => #{1 => Now - 1, 2 => Now + 60000},
        delete_tombstone_order => queue:from_list([{Now - 1, 1}, {Now + 60000, 2}])
    },
    Pruned = prune_tombstones(State),
    ?assertEqual(#{2 => Now + 60000}, maps:get(delete_tombstones, Pruned)),
    ?assertEqual([{Now + 60000, 2}], queue:to_list(maps:get(delete_tombstone_order, Pruned))).

record_delete_above_limit_costs_bounded_work_test_() ->
    {timeout, 120, fun assert_record_delete_above_limit_is_amortised/0}.

assert_record_delete_above_limit_is_amortised() ->
    Batch = 1000,
    Empty = #{delete_tombstones => #{}, delete_tombstone_order => queue:new()},
    Filled = record_delete_range(Empty, 1, ?TOMBSTONE_LIMIT - Batch),
    {BelowCost, AtLimit} = measure_record_deletes(
        Filled, ?TOMBSTONE_LIMIT - Batch + 1, ?TOMBSTONE_LIMIT
    ),
    ?assertEqual(?TOMBSTONE_LIMIT, maps:size(tombstones(AtLimit))),
    {AboveCost, Above} = measure_record_deletes(
        AtLimit, ?TOMBSTONE_LIMIT + 1, ?TOMBSTONE_LIMIT + Batch
    ),
    ?assert(AboveCost < BelowCost * 20),
    ?assertEqual(?TOMBSTONE_LIMIT, maps:size(tombstones(Above))),
    ?assert(maps:is_key(?TOMBSTONE_LIMIT + Batch, tombstones(Above))),
    ?assertNot(maps:is_key(1, tombstones(Above))).

measure_record_deletes(State, From, To) ->
    {reductions, Before} = erlang:process_info(self(), reductions),
    NewState = record_delete_range(State, From, To),
    {reductions, After} = erlang:process_info(self(), reductions),
    {After - Before, NewState}.

record_delete_range(State, From, To) ->
    lists:foldl(fun record_delete/2, State, lists:seq(From, To)).

-endif.
