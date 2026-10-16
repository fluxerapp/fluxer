%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_counts_cache_query).
-typing([eqwalizer]).

-include_lib("fluxer_gateway/include/timeout_config.hrl").

-export([
    safe_owner_call/3,
    safe_local_call/2,
    safe_local_cast/1,
    safe_bulk_get/1,
    ensure_started/0,
    resolve_owner_node/1,
    resolve_owner_node_safe/1,
    resolve_owner_node_safe/2,
    group_guild_ids_by_owner/1,
    normalize_guild_ids/1,
    split_local_remote_groups/1,
    update_local/4,
    delete_local/2,
    get_local/2,
    bulk_get_local/2,
    bulk_get_local_groups/2,
    handle_bulk_get_call/3,
    local_snapshot/1
]).

-type guild_id() :: integer().
-type counts() :: {non_neg_integer(), non_neg_integer()}.

-export_type([guild_id/0, counts/0]).

-spec safe_local_call(term(), term()) -> term().
safe_local_call(Request, Fallback) ->
    case ensure_started() of
        ok -> do_safe_local_call(Request, Fallback);
        error -> Fallback
    end.

-spec do_safe_local_call(term(), term()) -> term().
do_safe_local_call(Request, Fallback) ->
    try
        gen_server:call(guild_counts_cache, Request, ?DEFAULT_GEN_SERVER_TIMEOUT)
    catch
        exit:_ -> Fallback
    end.

-spec safe_owner_call(guild_id(), term(), term()) -> term().
safe_owner_call(GuildId, Request, Fallback) ->
    OwnerNode = resolve_owner_node(GuildId),
    dispatch_owner_call(OwnerNode, Request, Fallback).

-spec dispatch_owner_call(node() | unavailable, term(), term()) -> term().
dispatch_owner_call(unavailable, _Request, Fallback) ->
    Fallback;
dispatch_owner_call(OwnerNode, Request, Fallback) ->
    case OwnerNode =:= node() of
        true ->
            local_call_if_enabled(Request, Fallback);
        false ->
            guild_counts_cache_remote:safe_remote_call(
                OwnerNode, Request, Fallback, ?DEFAULT_GEN_SERVER_TIMEOUT
            )
    end.

-spec local_call_if_enabled(term(), term()) -> term().
local_call_if_enabled(Request, Fallback) ->
    case fluxer_gateway_sup:role_enabled(guilds) of
        true -> safe_local_call(Request, Fallback);
        false -> Fallback
    end.

-spec safe_bulk_get([guild_id()]) -> #{guild_id() => counts()}.
safe_bulk_get(GuildIds) ->
    Groups = group_guild_ids_by_owner(GuildIds),
    lists:foldl(
        fun({OwnerNode, OwnerGuildIds}, AccMap) ->
            Reply = fetch_bulk_group(OwnerNode, OwnerGuildIds),
            merge_reply(Reply, AccMap)
        end,
        #{},
        Groups
    ).

-spec fetch_bulk_group(node(), [guild_id()]) -> term().
fetch_bulk_group(OwnerNode, OwnerGuildIds) ->
    Request = {bulk_get_local, OwnerGuildIds},
    case OwnerNode =:= node() of
        true ->
            fetch_bulk_local(Request);
        false ->
            guild_counts_cache_remote:safe_remote_call(
                OwnerNode, Request, #{}
            )
    end.

-spec fetch_bulk_local(term()) -> term().
fetch_bulk_local(Request) ->
    case fluxer_gateway_sup:role_enabled(guilds) of
        true -> safe_local_call(Request, #{});
        false -> #{}
    end.

-spec merge_reply(term(), #{guild_id() => counts()}) -> #{guild_id() => counts()}.
merge_reply(ReplyMap, AccMap) when is_map(ReplyMap) ->
    maps:merge(AccMap, ReplyMap);
merge_reply(_, AccMap) ->
    AccMap.

-spec ensure_started() -> ok | error.
ensure_started() ->
    case fluxer_gateway_sup:role_enabled(guilds) of
        true -> ensure_started_with_role();
        false -> error
    end.

-spec ensure_started_with_role() -> ok | error.
ensure_started_with_role() ->
    case whereis(guild_counts_cache) of
        Pid when is_pid(Pid) -> ok;
        undefined -> try_start_cache()
    end.

-spec try_start_cache() -> ok | error.
try_start_cache() ->
    case guild_counts_cache:start_link() of
        {ok, _Pid} -> ok;
        {error, _Reason} -> error
    end.

-spec safe_local_cast(term()) -> ok | error.
safe_local_cast(Msg) ->
    case fluxer_gateway_sup:role_enabled(guilds) of
        true -> do_safe_local_cast(Msg);
        false -> error
    end.

-spec do_safe_local_cast(term()) -> ok | error.
do_safe_local_cast(Msg) ->
    case whereis(guild_counts_cache) of
        Pid when is_pid(Pid) ->
            gen_server:cast(guild_counts_cache, Msg),
            ok;
        _ ->
            error
    end.

-spec resolve_owner_node(guild_id()) -> node() | unavailable.
resolve_owner_node(GuildId) ->
    clustered_ets_cache:resolve_owner_node(GuildId, guilds).

-spec resolve_owner_node_safe(guild_id()) -> node() | unavailable.
resolve_owner_node_safe(GuildId) ->
    resolve_owner_node_safe(GuildId, fun resolve_owner_node/1).

-spec resolve_owner_node_safe(guild_id(), fun((guild_id()) -> term())) -> node() | unavailable.
resolve_owner_node_safe(GuildId, ResolveOwnerNodeFun) ->
    try ResolveOwnerNodeFun(GuildId) of
        unavailable -> unavailable;
        OwnerNode when is_atom(OwnerNode) -> OwnerNode;
        _ -> unavailable
    catch
        _:_ -> unavailable
    end.

-spec group_guild_ids_by_owner([guild_id()]) -> [{node(), [guild_id()]}].
group_guild_ids_by_owner(GuildIds) ->
    normalize_owner_groups(
        clustered_ets_cache:group_keys_by_owner(normalize_guild_ids(GuildIds), guilds)
    ).

-spec normalize_owner_groups([{node(), [term()]}]) -> [{node(), [guild_id()]}].
normalize_owner_groups(Groups) ->
    [
        {OwnerNode, normalize_guild_ids(OwnerGuildIds)}
     || {OwnerNode, OwnerGuildIds} <- Groups
    ].

-spec normalize_guild_ids([term()]) -> [guild_id()].
normalize_guild_ids(GuildIds) ->
    lists:usort([GuildId || GuildId <- GuildIds, is_integer(GuildId), GuildId > 0]).

-spec split_local_remote_groups([{node(), [guild_id()]}]) ->
    {[{node(), [guild_id()]}], [{node(), [guild_id()]}]}.
split_local_remote_groups(OwnerGroups) ->
    lists:partition(
        fun({OwnerNode, _OwnerGuildIds}) -> OwnerNode =:= node() end,
        OwnerGroups
    ).

-spec update_local(guild_id(), non_neg_integer(), non_neg_integer(), map()) -> {ok, map()}.
update_local(GuildId, MemberCount, OnlineCount, State) ->
    {_Reply, NewState} = guild_counts_cache_shard_mgmt:forward_call(
        GuildId, {update, GuildId, MemberCount, OnlineCount}, State
    ),
    {ok, NewState}.

-spec delete_local(guild_id(), map()) -> {ok, map()}.
delete_local(GuildId, State) ->
    {_Reply, NewState} = guild_counts_cache_shard_mgmt:forward_call(
        GuildId, {delete, GuildId}, State
    ),
    {ok, NewState}.

-spec get_local(guild_id(), map()) ->
    {{ok, non_neg_integer(), non_neg_integer()} | miss, map()}.
get_local(GuildId, State) ->
    {Reply, NewState} = guild_counts_cache_shard_mgmt:forward_call(
        GuildId, {get, GuildId}, State
    ),
    {guild_counts_cache_remote:normalize_get_reply(Reply), NewState}.

-spec handle_bulk_get_call([guild_id()], gen_server:from(), map()) ->
    {reply, #{guild_id() => counts()}, map()} | {noreply, map()}.
handle_bulk_get_call(GuildIds, From, State) ->
    OwnerGroups = group_guild_ids_by_owner(GuildIds),
    {LocalGroups, RemoteGroups} = split_local_remote_groups(OwnerGroups),
    {LocalMap, State1} = bulk_get_local_groups(LocalGroups, State),
    case RemoteGroups of
        [] ->
            {reply, LocalMap, State1};
        _ ->
            guild_counts_cache_remote:start_remote_bulk_get_reply(From, LocalMap, RemoteGroups),
            {noreply, State1}
    end.

-spec bulk_get_local_groups([{node(), [guild_id()]}], map()) ->
    {#{guild_id() => counts()}, map()}.
bulk_get_local_groups(LocalGroups, State) ->
    lists:foldl(
        fun({_OwnerNode, OwnerGuildIds}, {AccMap, AccState}) ->
            {LocalMap, State1} = bulk_get_local(OwnerGuildIds, AccState),
            {maps:merge(AccMap, LocalMap), State1}
        end,
        {#{}, State},
        LocalGroups
    ).

-spec bulk_get_local([guild_id()], map()) -> {#{guild_id() => counts()}, map()}.
bulk_get_local(GuildIds, State) ->
    Count = maps:get(shard_count, State),
    UniqueGuildIds = normalize_guild_ids(GuildIds),
    Groups = rendezvous_router:group_keys(UniqueGuildIds, Count),
    lists:foldl(
        fun({Index, Ids}, {AccMap, AccState}) ->
            {SafeIdx, State1} = guild_counts_cache_shard_mgmt:ensure_shard_for_index(
                Index, AccState
            ),
            {Reply, State2} = guild_counts_cache_shard_mgmt:call_shard(
                SafeIdx, {bulk_get, Ids}, State1
            ),
            merge_bulk_reply(Reply, AccMap, State2)
        end,
        {#{}, State},
        Groups
    ).

-spec merge_bulk_reply(term(), #{guild_id() => counts()}, map()) ->
    {#{guild_id() => counts()}, map()}.
merge_bulk_reply(ReplyMap, AccMap, State) when is_map(ReplyMap) ->
    {maps:merge(AccMap, ReplyMap), State};
merge_bulk_reply(_, AccMap, State) ->
    {AccMap, State}.

-spec local_snapshot(map()) -> #{guild_id() => counts()}.
local_snapshot(State) ->
    Count = maps:get(shard_count, State),
    {SnapshotMap, _FinalState} = lists:foldl(
        fun(Index, {AccMap, AccState}) ->
            {SafeIdx, State1} = guild_counts_cache_shard_mgmt:ensure_shard_for_index(
                Index, AccState
            ),
            {Reply, State2} = guild_counts_cache_shard_mgmt:call_shard(
                SafeIdx, snapshot, State1
            ),
            merge_bulk_reply(Reply, AccMap, State2)
        end,
        {#{}, State},
        lists:seq(0, Count - 1)
    ),
    SnapshotMap.
