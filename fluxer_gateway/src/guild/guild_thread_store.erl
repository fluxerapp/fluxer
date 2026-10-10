%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_thread_store).
-typing([eqwalizer]).

-export([
    init/0,
    ensure_counter/2,
    add_counter/3,
    new/0,
    destroy/1,
    retire/1,
    load/3,
    put_thread/2,
    get_thread/2,
    remove_thread/2,
    put_member/4,
    get_member/3,
    remove_member/3,
    is_member/3,
    member_ids/2,
    members/2,
    user_thread_ids/2,
    parent_thread_ids/2,
    threads/1,
    thread_count/1,
    total_threads/0,
    row_count/1,
    remove_user/2,
    export/1,
    import/1
]).

-export_type([tab/0, thread_id/0, user_id/0]).

-define(REGISTRY, guild_thread_store_registry).
-define(RETIRE_DELAY_MS, 60000).
-define(MAX_THREADS, 5000).

-type tab() :: ets:table().
-type thread_id() :: integer().
-type user_id() :: integer().
-type row() ::
    {{t, thread_id()}, map()}
    | {{m, thread_id(), user_id()}, map()}
    | {{u, user_id(), thread_id()}, true}
    | {{p, integer(), thread_id()}, true}.

-spec init() -> ok.
init() ->
    _ =
        case ets:info(?REGISTRY) of
            undefined ->
                ets:new(?REGISTRY, [named_table, public, set, {write_concurrency, true}]);
            _ ->
                ok
        end,
    ok = guild_thread_permissions:init_counters(),
    ok = guild_thread_dispatch:init_counters(),
    ok = guild_thread_load:init_counters(),
    ok = guild_thread_flip:init_counters(),
    ok = guild_thread_subscriptions:init_counters().

-spec ensure_counter(term(), pos_integer()) -> ok.
ensure_counter(Key, Size) ->
    case persistent_term:get(Key, undefined) of
        undefined -> persistent_term:put(Key, counters:new(Size, [atomics]));
        _Counters -> ok
    end.

-spec add_counter(term(), pos_integer(), integer()) -> ok.
add_counter(Key, Index, N) ->
    case persistent_term:get(Key, undefined) of
        undefined -> ok;
        Counters -> counters:add(Counters, Index, N)
    end.

-spec new() -> tab().
new() ->
    Tab = ets:new(guild_thread_store, [ordered_set, protected, {read_concurrency, true}]),
    ok = registry(fun() -> ets:insert(?REGISTRY, {Tab, true}) end),
    Tab.

-spec destroy(tab() | undefined) -> ok.
destroy(undefined) ->
    ok;
destroy(Tab) ->
    ok = registry(fun() -> ets:delete(?REGISTRY, Tab) end),
    try ets:delete(Tab) of
        _ -> ok
    catch
        error:badarg -> ok
    end.

-spec retire(tab() | undefined) -> ok.
retire(undefined) ->
    ok;
retire(Tab) ->
    ok = registry(fun() -> ets:delete(?REGISTRY, Tab) end),
    _ = erlang:send_after(?RETIRE_DELAY_MS, self(), {thread_store_retire, Tab}),
    ok.

-spec registry(fun(() -> true)) -> ok.
registry(Fun) ->
    try Fun() of
        true -> ok
    catch
        error:badarg -> ok
    end.

-spec load(tab(), [map()], [map()]) -> ok.
load(Tab, Threads, Members) ->
    lists:foreach(fun(Thread) -> insert_thread(Tab, Thread) end, Threads),
    ok = trim(Tab, thread_count(Tab) - ?MAX_THREADS),
    lists:foreach(fun(Member) -> put_member_row(Tab, Member) end, Members).

-spec put_member_row(tab(), map()) -> ok.
put_member_row(Tab, Member) ->
    case
        {
            snowflake_id:parse_optional(maps:get(<<"id">>, Member, undefined)),
            snowflake_id:parse_optional(maps:get(<<"user_id">>, Member, undefined))
        }
    of
        {ThreadId, UserId} when is_integer(ThreadId), is_integer(UserId) ->
            case ets:member(Tab, {t, ThreadId}) of
                true -> put_member(Tab, ThreadId, UserId, Member);
                false -> ok
            end;
        _ ->
            ok
    end.

-spec put_thread(tab(), map()) -> ok.
put_thread(Tab, Thread) ->
    case insert_thread(Tab, Thread) of
        new -> enforce_cap(Tab);
        _ -> ok
    end.

-spec insert_thread(tab(), map()) -> new | known | invalid.
insert_thread(Tab, Thread) ->
    case snowflake_id:parse_optional(maps:get(<<"id">>, Thread, undefined)) of
        ThreadId when is_integer(ThreadId) ->
            Known = ets:member(Tab, {t, ThreadId}),
            true = ets:insert(Tab, {{t, ThreadId}, Thread}),
            case snowflake_id:parse_optional(maps:get(<<"parent_id">>, Thread, undefined)) of
                ParentId when is_integer(ParentId) ->
                    true = ets:insert(Tab, {{p, ParentId, ThreadId}, true});
                _ ->
                    true
            end,
            case Known of
                true -> known;
                false -> new
            end;
        _ ->
            invalid
    end.

-spec enforce_cap(tab()) -> ok.
enforce_cap(Tab) ->
    case row_count(Tab) > ?MAX_THREADS of
        true -> trim(Tab, thread_count(Tab) - ?MAX_THREADS);
        false -> ok
    end.

-spec trim(tab(), integer()) -> ok.
trim(_Tab, Excess) when Excess =< 0 ->
    ok;
trim(Tab, Excess) ->
    case ets:select(Tab, [{{{t, '$1'}, '_'}, [], ['$1']}], Excess) of
        {Oldest, _Continuation} ->
            lists:foreach(fun(ThreadId) -> remove_thread(Tab, ThreadId) end, Oldest);
        '$end_of_table' ->
            ok
    end.

-spec get_thread(tab(), thread_id()) -> map() | undefined.
get_thread(Tab, ThreadId) ->
    case ets:lookup(Tab, {t, ThreadId}) of
        [{_, Thread}] when is_map(Thread) -> Thread;
        _ -> undefined
    end.

-spec remove_thread(tab(), thread_id()) -> ok.
remove_thread(Tab, ThreadId) ->
    case get_thread(Tab, ThreadId) of
        undefined ->
            ok;
        Thread ->
            lists:foreach(
                fun(UserId) -> remove_member(Tab, ThreadId, UserId) end,
                member_ids(Tab, ThreadId)
            ),
            case snowflake_id:parse_optional(maps:get(<<"parent_id">>, Thread, undefined)) of
                ParentId when is_integer(ParentId) -> ets:delete(Tab, {p, ParentId, ThreadId});
                _ -> true
            end,
            true = ets:delete(Tab, {t, ThreadId}),
            ok
    end.

-spec put_member(tab(), thread_id(), user_id(), map()) -> ok.
put_member(Tab, ThreadId, UserId, Member) ->
    true = ets:insert(Tab, [{{m, ThreadId, UserId}, Member}, {{u, UserId, ThreadId}, true}]),
    ok.

-spec get_member(tab(), thread_id(), user_id()) -> map() | undefined.
get_member(Tab, ThreadId, UserId) ->
    case ets:lookup(Tab, {m, ThreadId, UserId}) of
        [{_, Member}] when is_map(Member) -> Member;
        _ -> undefined
    end.

-spec remove_member(tab(), thread_id(), user_id()) -> ok.
remove_member(Tab, ThreadId, UserId) ->
    true = ets:delete(Tab, {m, ThreadId, UserId}),
    true = ets:delete(Tab, {u, UserId, ThreadId}),
    ok.

-spec is_member(tab(), thread_id(), user_id()) -> boolean().
is_member(Tab, ThreadId, UserId) ->
    ets:member(Tab, {m, ThreadId, UserId}).

-spec member_ids(tab(), thread_id()) -> [user_id()].
member_ids(Tab, ThreadId) ->
    ets:select(Tab, [{{{m, ThreadId, '$1'}, '_'}, [], ['$1']}]).

-spec members(tab(), thread_id()) -> [{user_id(), map()}].
members(Tab, ThreadId) ->
    ets:select(Tab, [{{{m, ThreadId, '$1'}, '$2'}, [], [{{'$1', '$2'}}]}]).

-spec user_thread_ids(tab(), user_id()) -> [thread_id()].
user_thread_ids(Tab, UserId) ->
    ets:select(Tab, [{{{u, UserId, '$1'}, '_'}, [], ['$1']}]).

-spec parent_thread_ids(tab(), integer()) -> [thread_id()].
parent_thread_ids(Tab, ParentId) ->
    ets:select(Tab, [{{{p, ParentId, '$1'}, '_'}, [], ['$1']}]).

-spec threads(tab()) -> [map()].
threads(Tab) ->
    ets:select(Tab, [{{{t, '_'}, '$1'}, [], ['$1']}]).

-spec thread_count(tab()) -> non_neg_integer().
thread_count(Tab) ->
    ets:select_count(Tab, [{{{t, '_'}, '_'}, [], [true]}]).

-spec total_threads() -> non_neg_integer().
total_threads() ->
    Tabs =
        try
            ets:select(?REGISTRY, [{{'$1', '_'}, [], ['$1']}])
        catch
            error:badarg -> []
        end,
    sum_threads(Tabs, 0).

-spec sum_threads([tab()], non_neg_integer()) -> non_neg_integer().
sum_threads([], Total) ->
    Total;
sum_threads([Tab | Rest], Total) ->
    sum_threads(Rest, Total + live_thread_count(Tab)).

-spec live_thread_count(tab()) -> non_neg_integer().
live_thread_count(Tab) ->
    try
        thread_count(Tab)
    catch
        error:badarg ->
            ok = registry(fun() -> ets:delete(?REGISTRY, Tab) end),
            0
    end.

-spec row_count(tab()) -> non_neg_integer().
row_count(Tab) ->
    case ets:info(Tab, size) of
        Size when is_integer(Size) -> Size;
        _ -> 0
    end.

-spec remove_user(tab(), user_id()) -> [thread_id()].
remove_user(Tab, UserId) ->
    ThreadIds = user_thread_ids(Tab, UserId),
    lists:foreach(fun(ThreadId) -> remove_member(Tab, ThreadId, UserId) end, ThreadIds),
    ThreadIds.

-spec export(tab()) -> [row()].
export(Tab) ->
    eqwalizer:dynamic_cast(ets:tab2list(Tab)).

-spec import([term()]) -> tab().
import(Rows) ->
    Tab = new(),
    true = ets:insert(Tab, [Row || Row <- Rows, is_valid_row(Row)]),
    Tab.

-spec is_valid_row(term()) -> boolean().
is_valid_row({{t, T}, Thread}) when is_integer(T), is_map(Thread) -> true;
is_valid_row({{m, T, U}, Member}) when is_integer(T), is_integer(U), is_map(Member) -> true;
is_valid_row({{u, U, T}, true}) when is_integer(T), is_integer(U) -> true;
is_valid_row({{p, P, T}, true}) when is_integer(T), is_integer(P) -> true;
is_valid_row(_) -> false.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

thread(Id, Parent) ->
    #{<<"id">> => Id, <<"parent_id">> => Parent, <<"type">> => 11}.

member(Thread, User) ->
    #{<<"id">> => integer_to_binary(Thread), <<"user_id">> => integer_to_binary(User)}.

store_round_trip_test() ->
    Tab = new(),
    try
        ok = load(Tab, [thread(10, 1), thread(11, 1), thread(20, 2)], [
            member(10, 100), member(10, 101), member(20, 100), member(99, 100)
        ]),
        ?assertEqual(3, thread_count(Tab)),
        ?assertEqual([100, 101], member_ids(Tab, 10)),
        ?assertEqual([10, 20], user_thread_ids(Tab, 100)),
        ?assertEqual([10, 11], parent_thread_ids(Tab, 1)),
        ?assert(is_member(Tab, 20, 100)),
        ?assertNot(is_member(Tab, 99, 100)),
        ok = remove_thread(Tab, 10),
        ?assertEqual(undefined, get_thread(Tab, 10)),
        ?assertEqual([20], user_thread_ids(Tab, 100)),
        ?assertEqual([], user_thread_ids(Tab, 101)),
        ?assertEqual([11], parent_thread_ids(Tab, 1)),
        ?assertEqual([20], remove_user(Tab, 100)),
        ?assertEqual([], member_ids(Tab, 20))
    after
        destroy(Tab)
    end.

export_import_test() ->
    Tab = new(),
    ok = load(Tab, [thread(10, 1)], [member(10, 5)]),
    Rows = export(Tab),
    destroy(Tab),
    Tab2 = import([junk | Rows]),
    try
        ?assertEqual(length(Rows), row_count(Tab2)),
        ?assert(is_member(Tab2, 10, 5)),
        ?assertEqual([10], parent_thread_ids(Tab2, 1))
    after
        destroy(Tab2)
    end.

total_threads_counts_registered_stores_only_test() ->
    ok = init(),
    Before = total_threads(),
    Tab = new(),
    ok = load(Tab, [thread(10, 1), thread(11, 1)], []),
    ?assertEqual(Before + 2, total_threads()),
    Owner = spawn(fun() ->
        Orphan = new(),
        ok = load(Orphan, [thread(12, 1)], []),
        receive
            stop -> ok
        end
    end),
    wait_for_total(Before + 3),
    Owner ! stop,
    wait_for_total(Before + 2),
    destroy(Tab),
    ?assertEqual(Before, total_threads()).

wait_for_total(Expected) ->
    wait_for_total(Expected, 50).

wait_for_total(Expected, 0) ->
    ?assertEqual(Expected, total_threads());
wait_for_total(Expected, Attempts) ->
    case total_threads() of
        Expected ->
            ok;
        _ ->
            timer:sleep(10),
            wait_for_total(Expected, Attempts - 1)
    end.

retire_unregisters_at_once_and_schedules_destroy_test() ->
    ok = init(),
    Tab = new(),
    ok = load(Tab, [thread(20, 1)], []),
    Before = total_threads(),
    ok = retire(Tab),
    ?assertEqual(Before - 1, total_threads()),
    ?assertNotEqual(undefined, ets:info(Tab)),
    destroy(Tab).

cap_keeps_newest_threads_test() ->
    Tab = new(),
    try
        ok = load(Tab, [thread(Id, 1) || Id <- lists:seq(10, ?MAX_THREADS + 9)], [
            member(10, 7), member(11, 7)
        ]),
        ?assertEqual(?MAX_THREADS, thread_count(Tab)),
        ok = put_thread(Tab, thread(10, 1)),
        ?assertEqual(?MAX_THREADS, thread_count(Tab)),
        ok = put_thread(Tab, thread(?MAX_THREADS + 10, 1)),
        ?assertEqual(?MAX_THREADS, thread_count(Tab)),
        ?assertEqual(undefined, get_thread(Tab, 10)),
        ?assertEqual([11], user_thread_ids(Tab, 7)),
        ?assertNot(lists:member(10, parent_thread_ids(Tab, 1))),
        ok = put_thread(Tab, thread(5, 1)),
        ?assertEqual(undefined, get_thread(Tab, 5)),
        ?assertEqual(?MAX_THREADS, thread_count(Tab)),
        ?assertNotEqual(undefined, get_thread(Tab, ?MAX_THREADS + 10))
    after
        destroy(Tab)
    end.

load_over_cap_keeps_newest_ids_test() ->
    Tab = new(),
    try
        Ids = lists:seq(?MAX_THREADS + 10, 1, -1),
        ok = load(Tab, [thread(Id, 1) || Id <- Ids], [member(5, 9), member(20, 9)]),
        ?assertEqual(?MAX_THREADS, thread_count(Tab)),
        ?assertEqual(undefined, get_thread(Tab, 10)),
        ?assertNotEqual(undefined, get_thread(Tab, 11)),
        ?assertEqual([20], user_thread_ids(Tab, 9))
    after
        destroy(Tab)
    end.

-endif.
