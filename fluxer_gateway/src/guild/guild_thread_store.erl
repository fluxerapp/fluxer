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
    total_threads/0,
    row_count/1,
    remove_user/2,
    export/1,
    import/1
]).

-export_type([tab/0, thread_id/0, user_id/0, row/0]).

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
            _ =
                ets:member(Tab, {t, ThreadId}) andalso
                    put_member(Tab, ThreadId, UserId, Member),
            ok;
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
            store_thread(Tab, ThreadId, Thread);
        _ ->
            invalid
    end.

-spec store_thread(tab(), thread_id(), map()) -> new | known.
store_thread(Tab, ThreadId, Thread) ->
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
            remove_threads(Tab, Oldest);
        '$end_of_table' ->
            ok
    end.

-spec remove_threads(tab(), [thread_id()]) -> ok.
remove_threads(Tab, ThreadIds) ->
    lists:foreach(fun(ThreadId) -> remove_thread(Tab, ThreadId) end, ThreadIds).

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
            remove_stored_thread(Tab, ThreadId, Thread)
    end.

-spec remove_stored_thread(tab(), thread_id(), map()) -> ok.
remove_stored_thread(Tab, ThreadId, Thread) ->
    lists:foreach(
        fun(UserId) -> remove_member(Tab, ThreadId, UserId) end, member_ids(Tab, ThreadId)
    ),
    case snowflake_id:parse_optional(maps:get(<<"parent_id">>, Thread, undefined)) of
        ParentId when is_integer(ParentId) -> ets:delete(Tab, {p, ParentId, ThreadId});
        _ -> true
    end,
    true = ets:delete(Tab, {t, ThreadId}),
    ok.

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
            ok = unregister_table(Tab),
            0
    end.

-spec unregister_table(tab()) -> ok.
unregister_table(Tab) ->
    registry(fun() -> ets:delete(?REGISTRY, Tab) end).

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
    true = ets:insert(Tab, [Row || Row = {_, _} <- Rows, is_valid_row(Row)]),
    Tab.

-spec is_valid_row(term()) -> boolean().
is_valid_row({{t, T}, Thread}) when is_integer(T), is_map(Thread) -> true;
is_valid_row({{m, T, U}, Member}) when is_integer(T), is_integer(U), is_map(Member) -> true;
is_valid_row({{u, U, T}, true}) when is_integer(T), is_integer(U) -> true;
is_valid_row({{p, P, T}, true}) when is_integer(T), is_integer(P) -> true;
is_valid_row(_) -> false.
