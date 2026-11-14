%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(presence_cache_bulk).
-typing([eqwalizer]).

-compile({no_auto_import, [get/1]}).

-export([
    bulk_get_inner/1,
    bulk_get_map_inner/1,
    get_from_cluster/1,
    get_local_fast/1,
    local_bulk_presence_map/1,
    normalize_user_ids/1,
    group_user_ids_by_owner/1,
    sanitize_presence_map/1,
    presence_values/1,
    map_from_presence_list/1,
    normalize_get_reply/1,
    safe_remote_call/3,
    safe_remote_call/4,
    fire_remote_cast/2,
    resolve_owner_nodes/1,
    replication_factor/0,
    local_shard_count/0,
    select_shard/2
]).

-define(REMOTE_CALL_TIMEOUT_MS, 1000).

-spec bulk_get_inner([integer()]) -> [map()].
bulk_get_inner(UserIds) ->
    presence_values(bulk_get_map_inner(UserIds)).

-spec bulk_get_map_inner([integer()]) -> #{integer() => map()}.
bulk_get_map_inner(UserIds) ->
    {Local, Remote} = lists:partition(
        fun({OwnerNode, _OwnerUserIds}) -> OwnerNode =:= node() end,
        group_user_ids_by_owner(UserIds)
    ),
    LocalMap = lists:foldl(
        fun({_OwnerNode, OwnerUserIds}, AccMap) ->
            maps:merge(AccMap, local_bulk_presence_map(OwnerUserIds))
        end,
        #{},
        Local
    ),
    Servers = [
        {{presence_cache, OwnerNode}, OwnerUserIds}
     || {OwnerNode, OwnerUserIds} <- Remote
    ],
    maps:merge(LocalMap, fetch_remote_groups(Servers)).

-spec fetch_remote_groups([{gen_server:server_ref(), [integer()]}]) -> #{integer() => map()}.
fetch_remote_groups([]) ->
    #{};
fetch_remote_groups(Groups) ->
    Deadline = erlang:monotonic_time(millisecond) + ?REMOTE_CALL_TIMEOUT_MS,
    Requests = [
        {Server, Ids, gen_server:send_request(Server, {bulk_get_local_map, Ids})}
     || {Server, Ids} <- Groups
    ],
    lists:foldl(
        fun({Server, Ids, ReqId}, AccMap) ->
            maps:merge(AccMap, remote_group_reply(Server, Ids, ReqId, Deadline))
        end,
        #{},
        Requests
    ).

-spec remote_group_reply(
    gen_server:server_ref(), [integer()], gen_server:request_id(), integer()
) ->
    #{integer() => map()}.
remote_group_reply(Server, Ids, ReqId, Deadline) ->
    case gen_server:receive_response(ReqId, {abs, Deadline}) of
        {reply, Reply} when is_map(Reply) ->
            sanitize_presence_map(Reply);
        {reply, _Legacy} ->
            map_from_presence_list(safe_server_call(Server, {bulk_get_local, Ids}, []));
        _ ->
            #{}
    end.

-spec get_from_cluster(integer()) -> {ok, map()} | not_found.
get_from_cluster(UserId) ->
    OwnerNodes = resolve_owner_nodes(UserId),
    case lists:member(node(), OwnerNodes) of
        true -> get_local_or_remote(UserId, OwnerNodes);
        false -> fetch_from_owner_nodes(UserId, OwnerNodes)
    end.

-spec get_local_or_remote(integer(), [node()]) -> {ok, map()} | not_found.
get_local_or_remote(UserId, OwnerNodes) ->
    case get_local_fast(UserId) of
        {ok, Presence} -> {ok, Presence};
        not_found -> fetch_from_owner_nodes(UserId, OwnerNodes)
    end.

-spec get_local_fast(integer()) -> {ok, map()} | not_found.
get_local_fast(UserId) ->
    Count = local_shard_count(),
    Index = select_shard(UserId, Count),
    TableName = presence_cache_shard:table_name(Index),
    try ets:lookup(TableName, UserId) of
        [{_, Presence}] when is_map(Presence) -> {ok, Presence};
        _ -> not_found
    catch
        error:badarg -> not_found
    end.

-spec local_bulk_presence_map([integer()]) -> #{integer() => map()}.
local_bulk_presence_map(UserIds) ->
    Count = local_shard_count(),
    Groups = normalize_shard_groups(
        rendezvous_router:group_keys(normalize_user_ids(UserIds), Count)
    ),
    lists:foldl(
        fun({Index, Ids}, AccMap) ->
            TableName = presence_cache_shard:table_name(Index),
            maps:merge(AccMap, bulk_lookup_table(TableName, Ids))
        end,
        #{},
        Groups
    ).

-spec safe_remote_call(node(), term(), term()) -> term().
safe_remote_call(TargetNode, Request, Fallback) ->
    safe_remote_call(TargetNode, Request, Fallback, ?REMOTE_CALL_TIMEOUT_MS).

-spec safe_remote_call(node(), term(), term(), pos_integer()) -> term().
safe_remote_call(TargetNode, Request, Fallback, Timeout) when
    is_integer(Timeout), Timeout > 0
->
    case is_remote_write(Request) andalso lists:member(TargetNode, nodes()) of
        true -> fire_remote_cast(TargetNode, Request);
        false -> safe_server_call({presence_cache, TargetNode}, Request, Fallback, Timeout)
    end.

-spec is_remote_write(term()) -> boolean().
is_remote_write({put_local, _UserId, _Presence}) -> true;
is_remote_write({delete_local, _UserId}) -> true;
is_remote_write(_Request) -> false.

-spec safe_server_call(gen_server:server_ref(), term(), term()) -> term().
safe_server_call(Server, Request, Fallback) ->
    safe_server_call(Server, Request, Fallback, ?REMOTE_CALL_TIMEOUT_MS).

-spec safe_server_call(gen_server:server_ref(), term(), term(), pos_integer()) -> term().
safe_server_call(Server, Request, Fallback, Timeout) ->
    try gen_server:call(Server, Request, Timeout) of
        Reply -> Reply
    catch
        error:_ -> Fallback;
        exit:_ -> Fallback
    end.

-spec fire_remote_cast(node(), term()) -> ok.
fire_remote_cast(TargetNode, Request) ->
    try gen_server:cast({presence_cache, TargetNode}, Request) of
        _ -> ok
    catch
        error:_ -> ok;
        exit:_ -> ok
    end,
    ok.

-spec resolve_owner_nodes(integer()) -> [node()].
resolve_owner_nodes(UserId) ->
    ReplicaCount = replication_factor(),
    clustered_ets_cache:resolve_owner_nodes(UserId, ReplicaCount, presence).

-spec replication_factor() -> pos_integer().
replication_factor() -> 1.

-spec normalize_user_ids([term()]) -> [integer()].
normalize_user_ids(UserIds) ->
    lists:usort([UserId || UserId <- UserIds, is_integer(UserId), UserId > 0]).

-spec group_user_ids_by_owner([integer()]) -> [{node(), [integer()]}].
group_user_ids_by_owner(UserIds) ->
    normalize_owner_groups(group_normalized_user_ids_by_owner(normalize_user_ids(UserIds))).

-spec group_normalized_user_ids_by_owner([integer()]) -> [{node(), [integer()]}].
group_normalized_user_ids_by_owner(UserIds) ->
    GroupedMap = lists:foldl(fun add_user_to_owner_group/2, #{}, UserIds),
    lists:sort(
        maps:fold(
            fun(OwnerNode, OwnerUserIds, Acc) ->
                [{OwnerNode, lists:reverse(OwnerUserIds)} | Acc]
            end,
            [],
            GroupedMap
        )
    ).

-spec add_user_to_owner_group(integer(), #{node() => [integer()]}) -> #{node() => [integer()]}.
add_user_to_owner_group(UserId, Acc) ->
    case resolve_owner_nodes(UserId) of
        [OwnerNode | _] -> Acc#{OwnerNode => [UserId | maps:get(OwnerNode, Acc, [])]};
        [] -> Acc
    end.

-spec local_shard_count() -> pos_integer().
local_shard_count() ->
    {Count, _Source} = clustered_ets_cache:determine_shard_count([presence_cache_shards]),
    Count.

-spec select_shard(term(), pos_integer()) -> non_neg_integer().
select_shard(Key, Count) when Count > 0 ->
    clustered_ets_cache:select_shard(Key, Count).

-spec normalize_get_reply(term()) -> {ok, map()} | not_found.
normalize_get_reply({ok, Presence}) when is_map(Presence) -> {ok, Presence};
normalize_get_reply(_) -> not_found.

-spec sanitize_presence_map(map()) -> #{integer() => map()}.
sanitize_presence_map(PresenceMap) ->
    maps:fold(
        fun
            (Key, Presence, AccMap) when is_integer(Key), Key > 0, is_map(Presence) ->
                AccMap#{Key => Presence};
            (_Key, _Presence, AccMap) ->
                AccMap
        end,
        #{},
        PresenceMap
    ).

-spec map_from_presence_list(term()) -> #{integer() => map()}.
map_from_presence_list(PresenceList) when is_list(PresenceList) ->
    lists:foldl(fun accumulate_presence/2, #{}, PresenceList);
map_from_presence_list(_) ->
    #{}.

-spec accumulate_presence(term(), #{integer() => map()}) -> #{integer() => map()}.
accumulate_presence(Presence, AccMap) when is_map(Presence) ->
    case presence_user_id(Presence) of
        UserId when is_integer(UserId), UserId > 0 -> AccMap#{UserId => Presence};
        _ -> AccMap
    end;
accumulate_presence(_Presence, AccMap) ->
    AccMap.

-spec presence_values(#{integer() => map()}) -> [map()].
presence_values(PresenceMap) ->
    UserIds = lists:sort(maps:keys(PresenceMap)),
    [maps:get(UserId, PresenceMap) || UserId <- UserIds].

-spec fetch_from_owner_nodes(integer(), [node()]) -> {ok, map()} | not_found.
fetch_from_owner_nodes(_UserId, []) ->
    not_found;
fetch_from_owner_nodes(UserId, [OwnerNode | Rest]) ->
    case OwnerNode =:= node() of
        true -> fetch_from_owner_nodes(UserId, Rest);
        false -> try_remote_get(UserId, OwnerNode, Rest)
    end.

-spec try_remote_get(integer(), node(), [node()]) -> {ok, map()} | not_found.
try_remote_get(UserId, OwnerNode, RestNodes) ->
    Reply = safe_remote_call(OwnerNode, {get_local, UserId}, {error, unavailable}),
    case normalize_get_reply(Reply) of
        {ok, Presence} -> {ok, Presence};
        not_found -> fetch_from_owner_nodes(UserId, RestNodes)
    end.

-spec bulk_lookup_table(atom(), [integer()]) -> #{integer() => map()}.
bulk_lookup_table(TableName, UserIds) ->
    lists:foldl(
        fun(UserId, AccMap) -> accumulate_ets_lookup(TableName, UserId, AccMap) end,
        #{},
        UserIds
    ).

-spec accumulate_ets_lookup(atom(), integer(), #{integer() => map()}) -> #{integer() => map()}.
accumulate_ets_lookup(TableName, UserId, AccMap) ->
    try ets:lookup(TableName, UserId) of
        [{_, Presence}] when is_map(Presence) -> AccMap#{UserId => Presence};
        _ -> AccMap
    catch
        error:badarg -> AccMap
    end.

-spec normalize_shard_groups([{non_neg_integer(), [term()]}]) ->
    [{non_neg_integer(), [integer()]}].
normalize_shard_groups(Groups) ->
    [{Key, filter_integers(Ids)} || {Key, Ids} <- Groups, is_integer(Key), Key >= 0].

-spec normalize_owner_groups([{node(), [term()]}]) -> [{node(), [integer()]}].
normalize_owner_groups(Groups) ->
    [{Key, filter_integers(Ids)} || {Key, Ids} <- Groups, is_atom(Key)].

-spec filter_integers([term()]) -> [integer()].
filter_integers(Ids) ->
    [Id || Id <- Ids, is_integer(Id)].

-spec presence_user_id(map()) -> integer() | undefined.
presence_user_id(Presence) ->
    case maps:get(<<"user">>, Presence, undefined) of
        UserMap when is_map(UserMap) ->
            snowflake_id:parse_optional(maps:get(<<"id">>, UserMap, undefined));
        _ ->
            undefined
    end.
