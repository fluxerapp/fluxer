%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(presence_cache_shard).
-typing([eqwalizer]).
-behaviour(gen_server).

-include_lib("fluxer_gateway/include/timeout_config.hrl").

-export([start_link/1, table_name/1, write_put/3, write_delete/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(TABLE_PREFIX, presence_cache).

-type state() :: #{table := atom(), shard_index := non_neg_integer()}.

-spec start_link(non_neg_integer()) -> {ok, pid()} | {error, term()}.
start_link(ShardIndex) ->
    normalize_start_link(gen_server:start_link(?MODULE, #{shard_index => ShardIndex}, [])).

-spec table_name(non_neg_integer()) -> atom().
table_name(Index) ->
    list_to_atom(atom_to_list(?TABLE_PREFIX) ++ "_" ++ integer_to_list(Index)).

-spec write_put(non_neg_integer(), integer(), map()) -> ok.
write_put(Index, UserId, Presence) when is_integer(UserId), is_map(Presence) ->
    safe_write(fun() -> do_put(table_name(Index), UserId, Presence) end).

-spec write_delete(non_neg_integer(), integer()) -> ok.
write_delete(Index, UserId) when is_integer(UserId) ->
    safe_write(fun() ->
        ets:delete(table_name(Index), UserId),
        ok
    end).

-spec safe_write(fun(() -> ok)) -> ok.
safe_write(Fun) ->
    try Fun() of
        _ -> ok
    catch
        error:badarg -> ok
    end.

-spec init(map()) -> {ok, state(), hibernate}.
init(#{shard_index := ShardIndex}) ->
    process_flag(trap_exit, true),
    erlang:process_flag(fullsweep_after, 10),
    TableName = table_name(ShardIndex),
    ensure_table(TableName, self()),
    {ok, #{table => TableName, shard_index => ShardIndex}, hibernate}.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, term(), state()}.
handle_call({put, UserId, Presence}, _From, State) when is_integer(UserId), is_map(Presence) ->
    {reply, do_put(maps:get(table, State), UserId, Presence), State};
handle_call({delete, UserId}, _From, State) when is_integer(UserId) ->
    ets:delete(maps:get(table, State), UserId),
    {reply, ok, State};
handle_call({get, UserId}, _From, State) when is_integer(UserId) ->
    {reply, do_get(maps:get(table, State), UserId), State};
handle_call({bulk_get, UserIds}, _From, State) when is_list(UserIds) ->
    {reply, do_bulk_get(maps:get(table, State), UserIds), State};
handle_call({bulk_get_map, UserIds}, _From, State) when is_list(UserIds) ->
    {reply, do_bulk_get_map(maps:get(table, State), UserIds), State};
handle_call(snapshot, _From, State) ->
    {reply, do_snapshot(maps:get(table, State)), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    erlang:garbage_collect(),
    {ok, State}.

-spec do_get(atom(), term()) -> {ok, map()} | not_found.
do_get(Table, UserId) ->
    case safe_ets_lookup(Table, UserId) of
        [{_, Presence}] -> {ok, Presence};
        _ -> not_found
    end.

-spec do_bulk_get(atom(), [term()]) -> [map()].
do_bulk_get(Table, UserIds) ->
    lists:filtermap(
        fun(Uid) -> filtermap_ets_lookup(Table, Uid) end,
        lists:usort(UserIds)
    ).

-spec filtermap_ets_lookup(atom(), term()) -> {true, map()} | false.
filtermap_ets_lookup(Table, Uid) ->
    case safe_ets_lookup(Table, Uid) of
        [{_, Presence}] -> {true, Presence};
        _ -> false
    end.

-spec do_bulk_get_map(atom(), [term()]) -> #{integer() => map()}.
do_bulk_get_map(Table, UserIds) ->
    lists:foldl(
        fun(Uid, AccMap) -> fold_ets_lookup_map(Table, Uid, AccMap) end,
        #{},
        lists:usort(UserIds)
    ).

-spec fold_ets_lookup_map(atom(), term(), #{integer() => map()}) -> #{integer() => map()}.
fold_ets_lookup_map(Table, Uid, AccMap) ->
    case safe_ets_lookup(Table, Uid) of
        [{_, Presence}] when is_integer(Uid), is_map(Presence) -> AccMap#{Uid => Presence};
        _ -> AccMap
    end.

-spec do_snapshot(atom()) -> #{term() => map()}.
do_snapshot(Table) ->
    ets:foldl(
        fun
            ({UserId, Presence}, Acc) -> Acc#{UserId => Presence};
            (_, Acc) -> Acc
        end,
        #{},
        Table
    ).

-spec do_put(atom(), integer(), map()) -> ok.
do_put(Table, UserId, Presence) ->
    Status = maps:get(<<"status">>, Presence, <<"offline">>),
    case Status of
        <<"invisible">> ->
            ets:delete(Table, UserId),
            ok;
        <<"offline">> ->
            ets:delete(Table, UserId),
            ok;
        _ ->
            ets:insert(Table, {UserId, Presence}),
            ok
    end.

-spec ensure_table(atom(), pid()) -> ok.
ensure_table(Table, OwnerPid) ->
    case ets:info(Table) of
        undefined ->
            HeirPid = find_heir_pid(),
            Opts =
                [named_table, public, set, {read_concurrency, true}] ++
                    heir_option(HeirPid),
            _ = ets:new(Table, Opts),
            ok;
        _ ->
            reclaim_table_ownership(Table, OwnerPid),
            ok
    end.

-spec find_heir_pid() -> pid() | undefined.
find_heir_pid() ->
    case whereis(presence_cache) of
        Pid when is_pid(Pid) -> Pid;
        _ -> undefined
    end.

-spec heir_option(pid() | undefined) -> list().
heir_option(undefined) -> [];
heir_option(HeirPid) -> [{heir, HeirPid, inherited}].

-spec reclaim_table_ownership(atom(), pid()) -> ok.
reclaim_table_ownership(Table, NewOwner) ->
    try
        CurrentOwner = ets:info(Table, owner),
        case CurrentOwner of
            NewOwner ->
                ok;
            HeirPid when is_pid(HeirPid) ->
                ets:give_away(Table, NewOwner, reclaimed),
                ok;
            _ ->
                ok
        end
    catch
        error:_ -> ok
    end.

-spec safe_ets_lookup(atom(), term()) -> [tuple()].
safe_ets_lookup(Table, Key) ->
    try ets:lookup(Table, Key) of
        Rows -> Rows
    catch
        error:_Reason -> [];
        exit:_Reason -> []
    end.

-spec normalize_start_link(gen_server:start_ret()) -> {ok, pid()} | {error, term()}.
normalize_start_link({ok, Pid}) ->
    {ok, Pid};
normalize_start_link({error, Reason}) ->
    {error, Reason};
normalize_start_link(ignore) ->
    {error, ignore}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

do_put_online_inserts_test() ->
    Table = test_cache_table,
    ets:new(Table, [named_table, public, set]),
    ?assertEqual(ok, do_put(Table, 1, #{<<"status">> => <<"online">>})),
    ?assertMatch([{1, _}], ets:lookup(Table, 1)),
    ets:delete(Table).

do_put_offline_deletes_test() ->
    Table = test_cache_table_2,
    ets:new(Table, [named_table, public, set]),
    ets:insert(Table, {1, #{<<"status">> => <<"online">>}}),
    ?assertEqual(ok, do_put(Table, 1, #{<<"status">> => <<"offline">>})),
    ?assertEqual([], ets:lookup(Table, 1)),
    ets:delete(Table).

do_put_invisible_deletes_test() ->
    Table = test_cache_table_3,
    ets:new(Table, [named_table, public, set]),
    ets:insert(Table, {1, #{<<"status">> => <<"online">>}}),
    ?assertEqual(ok, do_put(Table, 1, #{<<"status">> => <<"invisible">>})),
    ?assertEqual([], ets:lookup(Table, 1)),
    ets:delete(Table).

-endif.
