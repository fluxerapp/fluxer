%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_ets_cache_stress_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

-define(USER_COUNT, 10000).
-define(CONCURRENT_WRITERS, 8).
-define(WRITES_PER_WORKER, 2000).

get_subscriptions_many_large_mixed_set_test_() ->
    {timeout, 15, fun get_subscriptions_many_large_mixed_set/0}.

concurrent_subscription_reads_and_writes_keep_cache_consistent_test_() ->
    {timeout, 15, fun concurrent_subscription_reads_and_writes_keep_cache_consistent/0}.

get_subscriptions_many_large_mixed_set() ->
    cleanup_tables(),
    ok = push_ets_cache:init(),
    try
        lists:foreach(fun put_subscription_for_even_user/1, lists:seq(1, ?USER_COUNT)),
        {Cached, Missing} = push_ets_cache:get_subscriptions_many(lists:seq(1, ?USER_COUNT)),
        ?assertEqual(?USER_COUNT div 2, map_size(Cached)),
        ?assertEqual(?USER_COUNT div 2, length(Missing)),
        ?assertEqual([#{endpoint => <<"endpoint-2">>}], maps:get(2, Cached)),
        ?assertEqual(true, lists:member(1, Missing)),
        ?assertEqual(false, lists:member(2, Missing))
    after
        cleanup_tables()
    end.

concurrent_subscription_reads_and_writes_keep_cache_consistent() ->
    cleanup_tables(),
    ok = push_ets_cache:init(),
    Ref = make_ref(),
    Parent = self(),
    try
        _Writers = spawn_writers(Ref, Parent),
        _Readers = spawn_readers(Ref, Parent),
        collect_done(Ref, writer_done, ?CONCURRENT_WRITERS),
        collect_done(Ref, reader_done, ?CONCURRENT_WRITERS),
        ExpectedSize = ?CONCURRENT_WRITERS * ?WRITES_PER_WORKER,
        ?assertEqual(ExpectedSize, push_ets_cache:table_size(push_subscriptions)),
        {Cached, Missing} = push_ets_cache:get_subscriptions_many(lists:seq(1, ExpectedSize)),
        ?assertEqual(ExpectedSize, map_size(Cached)),
        ?assertEqual([], Missing)
    after
        cleanup_tables()
    end.

put_subscription_for_even_user(UserId) when UserId rem 2 =:= 0 ->
    ok = put_subscriptions(UserId, [#{endpoint => endpoint(UserId)}]);
put_subscription_for_even_user(_UserId) ->
    ok.

spawn_writers(Ref, Parent) ->
    [
        spawn(fun() ->
            writer_loop(WriterIndex, ?WRITES_PER_WORKER),
            Parent ! {Ref, writer_done}
        end)
     || WriterIndex <- lists:seq(0, ?CONCURRENT_WRITERS - 1)
    ].

writer_loop(WriterIndex, Count) ->
    Start = WriterIndex * Count + 1,
    End = Start + Count - 1,
    lists:foreach(
        fun(UserId) ->
            ok = put_subscriptions(UserId, [#{endpoint => endpoint(UserId)}])
        end,
        lists:seq(Start, End)
    ).

spawn_readers(Ref, Parent) ->
    [
        spawn(fun() ->
            reader_loop(ReaderIndex),
            Parent ! {Ref, reader_done}
        end)
     || ReaderIndex <- lists:seq(0, ?CONCURRENT_WRITERS - 1)
    ].

reader_loop(ReaderIndex) ->
    Offset = ReaderIndex * 100,
    lists:foreach(
        fun(Iteration) ->
            Start = 1 + ((Offset + Iteration * 97) rem ?USER_COUNT),
            _ = push_ets_cache:get_subscriptions_many(lists:seq(Start, Start + 99)),
            ok
        end,
        lists:seq(1, 100)
    ).

collect_done(_Ref, _Message, 0) ->
    ok;
collect_done(Ref, Message, Remaining) ->
    receive
        {Ref, Message} ->
            collect_done(Ref, Message, Remaining - 1)
    after 5000 ->
        ?assert(false)
    end.

put_subscriptions(UserId, Subscriptions) ->
    push_ets_cache:put_subscriptions(
        UserId, Subscriptions, push_ets_cache:reserve_subscriptions([UserId])
    ).

endpoint(UserId) ->
    UserIdBin = integer_to_binary(UserId),
    <<"endpoint-", UserIdBin/binary>>.

cleanup_tables() ->
    delete_table(push_user_guild_settings),
    delete_table(push_subscriptions),
    delete_table(push_blocked_ids),
    delete_table(push_badge_counts),
    ok.

delete_table(Table) ->
    try ets:delete(Table) of
        _ -> ok
    catch
        throw:_ -> ok;
        error:_ -> ok;
        exit:_ -> ok
    end.
