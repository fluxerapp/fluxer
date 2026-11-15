%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_counts_cache_shard).
-typing([eqwalizer]).
-behaviour(gen_server).

-export([start_link/1, table_name/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type state() :: #{table := atom(), shard_index := non_neg_integer()}.

-define(TABLE_PREFIX, guild_counts_cache).

-spec start_link(non_neg_integer()) -> gen_server:start_ret().
start_link(ShardIndex) ->
    gen_server:start_link(?MODULE, #{shard_index => ShardIndex}, []).

-spec table_name(non_neg_integer()) -> atom().
table_name(Index) ->
    list_to_atom(atom_to_list(?TABLE_PREFIX) ++ "_" ++ integer_to_list(Index)).

-spec init(map()) -> {ok, state()}.
init(#{shard_index := ShardIndex}) ->
    process_flag(trap_exit, true),
    erlang:process_flag(fullsweep_after, 0),
    TableName = table_name(ShardIndex),
    ensure_table(TableName),
    {ok, #{table => TableName, shard_index => ShardIndex}}.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, term(), state()}.
handle_call({update, GuildId, MemberCount, OnlineCount}, _From, State) ->
    Table = get_table(State),
    ets:insert(Table, {GuildId, MemberCount, OnlineCount}),
    {reply, ok, State};
handle_call({delete, GuildId}, _From, State) ->
    Table = get_table(State),
    ets:delete(Table, GuildId),
    {reply, ok, State};
handle_call({get, GuildId}, _From, State) ->
    {reply, lookup_single(get_table(State), require_guild_id(GuildId)), State};
handle_call({bulk_get, GuildIds}, _From, State) ->
    {reply, lookup_bulk(get_table(State), require_guild_ids(GuildIds)), State};
handle_call(snapshot, _From, State) ->
    {reply, snapshot_table(get_table(State)), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec get_table(state()) -> atom().
get_table(State) ->
    Table = maps:get(table, State),
    ensure_table(Table),
    Table.

-spec require_guild_id(term()) -> integer().
require_guild_id(GuildId) when is_integer(GuildId) ->
    GuildId;
require_guild_id(_) ->
    error(badarg).

-spec require_guild_ids(term()) -> [integer()].
require_guild_ids(GuildIds) when is_list(GuildIds) ->
    [require_guild_id(GuildId) || GuildId <- GuildIds];
require_guild_ids(_) ->
    error(badarg).

-spec lookup_single(atom(), integer()) -> {ok, integer(), integer()} | miss.
lookup_single(Table, GuildId) ->
    case safe_lookup(Table, GuildId) of
        [{GuildId, MemberCount, OnlineCount}] -> {ok, MemberCount, OnlineCount};
        _ -> miss
    end.

-spec lookup_bulk(atom(), [integer()]) -> #{integer() => {integer(), integer()}}.
lookup_bulk(Table, GuildIds) ->
    lists:foldl(
        fun(GuildId, Acc) ->
            accumulate_lookup(Table, GuildId, Acc)
        end,
        #{},
        lists:usort(GuildIds)
    ).

-spec accumulate_lookup(atom(), integer(), #{integer() => {integer(), integer()}}) ->
    #{integer() => {integer(), integer()}}.
accumulate_lookup(Table, GuildId, Acc) ->
    case safe_lookup(Table, GuildId) of
        [{GuildId, MemberCount, OnlineCount}] ->
            Acc#{GuildId => {MemberCount, OnlineCount}};
        _ ->
            Acc
    end.

-spec snapshot_table(atom()) -> #{integer() => {integer(), integer()}}.
snapshot_table(Table) ->
    maps:from_list([
        {GuildId, {MemberCount, OnlineCount}}
     || {GuildId, MemberCount, OnlineCount} <- ets:tab2list(Table)
    ]).

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

-spec ensure_table(atom()) -> ok.
ensure_table(Table) ->
    case ets:info(Table) of
        undefined ->
            _ = ets:new(Table, [
                named_table,
                public,
                set,
                {read_concurrency, true},
                {write_concurrency, true}
            ]),
            ok;
        _ ->
            ok
    end.

-spec safe_lookup(atom(), integer()) -> [{integer(), integer(), integer()}].
safe_lookup(Table, GuildId) ->
    try ets:lookup(Table, GuildId) of
        Rows -> Rows
    catch
        error:badarg -> []
    end.
