%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_manager_transfer).
-typing([eqwalizer]).

-include_lib("fluxer_gateway/include/timeout_config.hrl").

-export([
    aggregate_counts/2,
    aggregate_handoff_to_topology/2,
    find_shard_by_ref/2,
    find_shard_by_pid/2
]).

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

-spec find_shard_by_ref(reference(), #{non_neg_integer() => shard()}) ->
    {ok, non_neg_integer()} | not_found.
find_shard_by_ref(Ref, Shards) ->
    shard_utils:find_shard_by_ref(Ref, Shards).

-spec find_shard_by_pid(pid(), #{non_neg_integer() => shard()}) ->
    {ok, non_neg_integer()} | not_found.
find_shard_by_pid(Pid, Shards) ->
    shard_utils:find_shard_by_pid(Pid, Shards).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

reconnect_drain_aggregates_shard_counts_test() ->
    ShardPidA = spawn(fun() -> shard_reconnect_test_loop(2) end),
    ShardPidB = spawn(fun() -> shard_reconnect_test_loop(3) end),
    State = #{
        shards => #{
            0 => #{pid => ShardPidA, ref => make_ref()},
            1 => #{pid => ShardPidB, ref => make_ref()}
        },
        shard_count => 2
    },
    {5, State} = aggregate_counts(reconnect_drain, State),
    ShardPidA ! stop,
    ShardPidB ! stop.

aggregate_counts_ignores_invalid_replies_test() ->
    InvalidShardPid = spawn(fun shard_invalid_reply_test_loop/0),
    ValidShardPid = spawn(fun() -> shard_reconnect_test_loop(3) end),
    State = #{
        shards => #{
            0 => #{pid => InvalidShardPid, ref => make_ref()},
            1 => #{pid => ValidShardPid, ref => make_ref()}
        },
        shard_count => 2
    },
    {3, _State} = aggregate_counts(reconnect_drain, State),
    InvalidShardPid ! stop,
    ValidShardPid ! stop.

aggregate_counts_ignores_unavailable_shards_test() ->
    ExitedShardPid = spawn(fun() -> ok end),
    ok = gateway_retry_timer:wait(5),
    ValidShardPid = spawn(fun() -> shard_reconnect_test_loop(2) end),
    State = #{
        shards => #{
            0 => #{pid => ExitedShardPid, ref => make_ref()},
            1 => #{pid => ValidShardPid, ref => make_ref()}
        },
        shard_count => 2
    },
    {2, _State} = aggregate_counts(reconnect_drain, State),
    ValidShardPid ! stop.

aggregate_handoff_to_topology_sums_attempts_and_successes_test() ->
    ShardPidA = spawn(fun() -> shard_handoff_test_loop(#{attempted => 2, handed_off => 1}) end),
    ShardPidB = spawn(fun() -> shard_handoff_test_loop(#{attempted => 3, handed_off => 3}) end),
    State = #{
        shards => #{
            0 => #{pid => ShardPidA, ref => make_ref()},
            1 => #{pid => ShardPidB, ref => make_ref()}
        },
        shard_count => 2
    },
    {#{attempted := 5, handed_off := 4}, State} =
        aggregate_handoff_to_topology(['peer@host'], State),
    ShardPidA ! stop,
    ShardPidB ! stop.

aggregate_handoff_to_topology_marks_invalid_reply_as_failed_test() ->
    InvalidShardPid = spawn(fun shard_invalid_reply_test_loop/0),
    State = #{
        shards => #{
            0 => #{pid => InvalidShardPid, ref => make_ref()}
        },
        shard_count => 1
    },
    {#{attempted := 1, handed_off := 0}, _State} =
        aggregate_handoff_to_topology(['peer@host'], State),
    InvalidShardPid ! stop.

aggregate_handoff_to_topology_counts_unavailable_and_bare_replies_test() ->
    ExitedShardPid = spawn(fun() -> ok end),
    ok = gateway_retry_timer:wait(5),
    BareShardPid = spawn(fun() -> shard_bare_reply_test_loop(2) end),
    State = #{
        shards => #{
            0 => #{pid => ExitedShardPid, ref => make_ref()},
            1 => #{pid => BareShardPid, ref => make_ref()}
        },
        shard_count => 2
    },
    {#{attempted := 3, handed_off := 2}, _State} =
        aggregate_handoff_to_topology(['peer@host'], State),
    BareShardPid ! stop.

shard_reconnect_test_loop(DrainCount) ->
    receive
        {'$gen_call', From, reconnect_drain} ->
            gen_server:reply(From, {ok, DrainCount}),
            shard_reconnect_test_loop(DrainCount);
        {'$gen_call', From, _Request} ->
            gen_server:reply(From, {ok, 0}),
            shard_reconnect_test_loop(DrainCount);
        stop ->
            ok
    after 5000 ->
        ok
    end.

shard_handoff_test_loop(Result) ->
    receive
        {'$gen_call', From, {handoff_to_topology, _TargetNodes}} ->
            gen_server:reply(From, {ok, Result}),
            shard_handoff_test_loop(Result);
        {'$gen_call', From, _Request} ->
            gen_server:reply(From, {ok, 0}),
            shard_handoff_test_loop(Result);
        stop ->
            ok
    after 5000 ->
        ok
    end.

shard_invalid_reply_test_loop() ->
    shard_bare_reply_test_loop(invalid_reply).

shard_bare_reply_test_loop(Reply) ->
    receive
        {'$gen_call', From, _Request} ->
            gen_server:reply(From, Reply),
            shard_bare_reply_test_loop(Reply);
        stop ->
            ok
    after 5000 ->
        ok
    end.

-endif.
