%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(shard_utils).
-typing([eqwalizer]).

-export([
    max_positive/1,
    determine_shard_count/1,
    find_shard_by_ref/2,
    find_shard_by_pid/2,
    safe_gen_call/3,
    safe_gen_call_detailed/3,
    safe_gen_call_remote/3,
    safe_gen_call_wrapped/3,
    safe_apply/2,
    safe_cast/2,
    check_mailbox_pressure/1
]).

-export_type([shard/0]).

-type shard() :: #{pid := pid(), ref := reference()}.

-spec max_positive(list()) -> pos_integer().
max_positive(Candidates) ->
    lists:max([C || C <- Candidates, is_integer(C), C > 0] ++ [1]).

-spec determine_shard_count(atom()) -> {pos_integer(), configured | auto}.
determine_shard_count(ConfigKey) ->
    case fluxer_gateway_env:get(ConfigKey) of
        Value when is_integer(Value), Value > 0 ->
            {Value, configured};
        _ ->
            {clustered_ets_cache:default_shard_count(), auto}
    end.

-spec find_shard_by_ref(reference(), #{non_neg_integer() => shard()}) ->
    {ok, non_neg_integer()} | not_found.
find_shard_by_ref(Ref, Shards) ->
    maps:fold(
        fun
            (Index, #{ref := R}, _) when R =:= Ref -> {ok, Index};
            (_, _, Acc) -> Acc
        end,
        not_found,
        Shards
    ).

-spec find_shard_by_pid(pid(), #{non_neg_integer() => shard()}) ->
    {ok, non_neg_integer()} | not_found.
find_shard_by_pid(Pid, Shards) ->
    maps:fold(
        fun
            (Index, #{pid := P}, _) when P =:= Pid -> {ok, Index};
            (_, _, Acc) -> Acc
        end,
        not_found,
        Shards
    ).

-spec safe_gen_call(pid() | atom(), term(), pos_integer()) -> term().
safe_gen_call(Pid, Request, Timeout) ->
    try
        gen_server:call(Pid, Request, Timeout)
    catch
        throw:_ -> {error, unavailable};
        error:_ -> {error, unavailable};
        exit:_ -> {error, unavailable}
    end.

-spec safe_gen_call_detailed(pid(), term(), pos_integer()) -> term().
safe_gen_call_detailed(Pid, Request, Timeout) ->
    try gen_server:call(Pid, Request, Timeout) of
        Reply -> Reply
    catch
        exit:{timeout, _Call} -> {error, timeout};
        exit:{noproc, _Call} -> {error, unavailable};
        exit:Reason -> {error, {exit, Reason}}
    end.

-spec safe_gen_call_remote(pid() | atom(), term(), pos_integer()) -> term().
safe_gen_call_remote(Server, Request, Timeout) ->
    try gen_server:call(Server, Request, Timeout) of
        Reply -> Reply
    catch
        exit:{timeout, _Call} -> {error, timeout};
        exit:{noproc, _Call} -> {error, unavailable};
        exit:{nodedown, _Node} -> {error, unavailable};
        exit:Reason -> {error, {exit, Reason}}
    end.

-spec safe_gen_call_wrapped(gen_server:server_ref(), term(), timeout()) ->
    {ok, term()} | {exit, term()}.
safe_gen_call_wrapped(ServerRef, Request, Timeout) ->
    try gen_server:call(ServerRef, Request, Timeout) of
        Reply -> {ok, Reply}
    catch
        error:Reason -> {exit, {error, Reason}};
        exit:Reason -> {exit, Reason}
    end.

-spec safe_apply(fun(() -> T), T) -> T when T :: term().
safe_apply(Fun, Fallback) ->
    try
        Fun()
    catch
        throw:_ -> Fallback;
        error:_ -> Fallback;
        exit:_ -> Fallback
    end.

-define(MAILBOX_PRESSURE_THRESHOLD, 5000).

-spec safe_cast(pid() | atom() | {atom(), node()}, term()) -> ok | {error, overloaded}.
safe_cast(Dest, Msg) ->
    case check_mailbox_pressure(Dest) of
        ok ->
            gen_server:cast(Dest, Msg),
            ok;
        overloaded ->
            {error, overloaded}
    end.

-spec check_mailbox_pressure(pid() | atom() | {atom(), node()}) -> ok | overloaded.
check_mailbox_pressure(Dest) when is_pid(Dest) ->
    check_pid_mailbox_pressure(Dest);
check_mailbox_pressure(Name) when is_atom(Name) ->
    case whereis(Name) of
        undefined -> ok;
        Pid when is_pid(Pid) -> check_mailbox_pressure(Pid)
    end;
check_mailbox_pressure({Name, Node}) when is_atom(Name), is_atom(Node), Node =:= node() ->
    check_mailbox_pressure(Name);
check_mailbox_pressure(_) ->
    ok.

-spec check_pid_mailbox_pressure(pid()) -> ok | overloaded.
check_pid_mailbox_pressure(Pid) when node(Pid) =/= node() ->
    ok;
check_pid_mailbox_pressure(Pid) ->
    case erlang:process_info(Pid, message_queue_len) of
        {message_queue_len, Len} when Len > ?MAILBOX_PRESSURE_THRESHOLD -> overloaded;
        _ -> ok
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

max_positive_normal_test() ->
    ?assertEqual(8, max_positive([4, 8, 2])).

max_positive_fallback_test() ->
    ?assertEqual(1, max_positive([])).

max_positive_non_integers_test() ->
    ?assertEqual(1, max_positive([undefined, foo, -1, 0])).

max_positive_mixed_test() ->
    ?assertEqual(4, max_positive([undefined, 4, -1, 0])).

safe_gen_call_noproc_test() ->
    Pid = spawn(fun() -> ok end),
    ok = gateway_retry_timer:wait(50),
    ?assertEqual({error, unavailable}, safe_gen_call(Pid, ping, 100)).

safe_apply_success_test() ->
    ?assertEqual(42, safe_apply(fun() -> 42 end, 0)).

safe_apply_throw_test() ->
    ?assertEqual(fallback, safe_apply(fun() -> exit(boom) end, fallback)).

safe_apply_error_test() ->
    ?assertEqual(fallback, safe_apply(fun() -> error(boom) end, fallback)).

safe_apply_exit_test() ->
    ?assertEqual(fallback, safe_apply(fun() -> exit(boom) end, fallback)).

safe_gen_call_detailed_noproc_test() ->
    Pid = spawn(fun() -> ok end),
    ok = gateway_retry_timer:wait(50),
    ?assertEqual({error, unavailable}, safe_gen_call_detailed(Pid, ping, 100)).

safe_gen_call_remote_noproc_test() ->
    Pid = spawn(fun() -> ok end),
    ok = gateway_retry_timer:wait(50),
    ?assertEqual({error, unavailable}, safe_gen_call_remote(Pid, ping, 100)).

safe_gen_call_wrapped_noproc_test() ->
    Pid = spawn(fun() -> ok end),
    ok = gateway_retry_timer:wait(50),
    ?assertMatch({exit, _}, safe_gen_call_wrapped(Pid, ping, 100)).

check_mailbox_pressure_ok_test() ->
    ?assertEqual(ok, check_mailbox_pressure(self())).

check_mailbox_pressure_dead_pid_test() ->
    Pid = spawn(fun() -> ok end),
    ok = gateway_retry_timer:wait(50),
    ?assertEqual(ok, check_mailbox_pressure(Pid)).

check_mailbox_pressure_unregistered_name_test() ->
    ?assertEqual(ok, check_mailbox_pressure(nonexistent_name_xyz_test)).

check_mailbox_pressure_remote_tuple_test() ->
    ?assertEqual(ok, check_mailbox_pressure({some_name, 'nonode@nohost'})).

safe_cast_ok_test() ->
    Pid = spawn(fun safe_cast_test_loop/0),
    ?assertEqual(ok, safe_cast(Pid, hello)),
    Pid ! stop.

safe_cast_test_loop() ->
    receive
        stop -> ok
    after 5000 -> ok
    end.

safe_cast_dead_pid_test() ->
    Pid = spawn(fun() -> ok end),
    ok = gateway_retry_timer:wait(50),
    ?assertEqual(ok, safe_cast(Pid, hello)).

-endif.
