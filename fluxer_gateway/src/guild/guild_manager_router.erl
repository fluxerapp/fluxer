%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_manager_router).
-typing([eqwalizer]).

-include_lib("fluxer_gateway/include/timeout_config.hrl").

-export([
    call_via_manager/2,
    call_via_manager_local/2,
    call_via_manager_remote/2,
    extract_guild_id/1,
    forward_call/4,
    forward_call/5,
    forward_call_to_shard/4,
    forward_call_to_shard/5,
    ensure_local_owner/1,
    aggregate_counts/2,
    handle_reload_all/2
]).

-type guild_id() :: integer().
-type shard_map() :: #{pid := pid(), ref := reference()}.
-type state() :: #{shards := #{non_neg_integer() => shard_map()}, shard_count := pos_integer()}.

-export_type([guild_id/0, state/0]).

-spec call_via_manager(term(), pos_integer()) -> term().
call_via_manager(Request, Timeout) ->
    case fluxer_gateway_sup:role_enabled(guilds) of
        true -> call_via_manager_local(Request, Timeout);
        false -> call_via_manager_remote(Request, Timeout)
    end.

-spec call_via_manager_local(term(), pos_integer()) -> term().
call_via_manager_local(Request, Timeout) ->
    shard_utils:safe_gen_call_remote(
        guild_manager, {with_timeout, Request, Timeout}, Timeout + 1000
    ).

-spec call_via_manager_remote(term(), pos_integer()) -> term().
call_via_manager_remote(Request, Timeout) ->
    case extract_guild_id(Request) of
        {ok, GuildId} -> remote_manager_call(GuildId, Request, Timeout);
        error -> {error, unavailable}
    end.

-spec extract_guild_id(term()) -> {ok, guild_id()} | error.
extract_guild_id({_Op, GuildId}) when is_integer(GuildId) ->
    {ok, GuildId};
extract_guild_id({_Op, GuildId, _Extra}) when is_integer(GuildId) ->
    {ok, GuildId};
extract_guild_id(_) ->
    error.

-spec forward_call(guild_id(), term(), gen_server:from(), state()) ->
    {reply, term(), state()} | {noreply, state()}.
forward_call(GuildId, Request, From, State) ->
    forward_call(GuildId, Request, ?DEFAULT_GEN_SERVER_TIMEOUT, From, State).

-spec forward_call(guild_id(), term(), pos_integer(), gen_server:from(), state()) ->
    {reply, term(), state()} | {noreply, state()}.
forward_call(GuildId, Request, Timeout, From, State) ->
    case owner_check_for(Request, GuildId) of
        ok -> forward_owned_call(GuildId, Request, Timeout, From, State);
        {error, _Reason} = Error -> {reply, Error, State}
    end.

-spec forward_call_to_shard(guild_id(), term(), gen_server:from(), state()) ->
    {noreply, state()}.
forward_call_to_shard(GuildId, Request, From, State) ->
    forward_call_to_shard(GuildId, Request, ?DEFAULT_GEN_SERVER_TIMEOUT, From, State).

-spec forward_call_to_shard(
    guild_id(), term(), pos_integer(), gen_server:from(), state()
) -> {noreply, state()}.
forward_call_to_shard(GuildId, Request, Timeout, From, State) ->
    {Index, State1} = guild_manager_shards:ensure_shard(GuildId, State),
    Shards = maps:get(shards, State1),
    #{pid := Pid} = maps:get(Index, Shards),
    proc_lib:spawn(fun() -> reply_from_shard(Pid, GuildId, Request, Timeout, From) end),
    {noreply, State1}.

-spec ensure_local_owner(guild_id()) -> ok | {error, term()}.
ensure_local_owner(GuildId) ->
    case safe_owner_node(GuildId) of
        {ok, LocalNode} when LocalNode =:= node() -> ok;
        {ok, OwnerNode} when is_atom(OwnerNode) -> {error, {not_owner, OwnerNode}};
        error -> {error, cluster_unavailable}
    end.

-spec aggregate_counts(term(), state()) -> {{ok, non_neg_integer()}, state()}.
aggregate_counts(Request, State) ->
    Shards = maps:get(shards, State),
    Counts = [
        count_from_shard(maps:get(pid, ShardMap), Request)
     || ShardMap <- maps:values(Shards)
    ],
    {ok, Total} = sum_valid_counts(Counts),
    {{ok, Total}, State}.

-spec handle_reload_all([guild_id()], state()) -> {#{count := non_neg_integer()}, state()}.
handle_reload_all([], State) ->
    Shards = maps:get(shards, State),
    Replies = [
        shard_utils:safe_gen_call_remote(
            maps:get(pid, ShardMap), {reload_all_guilds, []}, 15000
        )
     || ShardMap <- maps:values(Shards)
    ],
    {#{count => sum_reload_counts(Replies)}, State};
handle_reload_all(GuildIds, State) ->
    Count = maps:get(shard_count, State),
    Groups = guild_manager_shards:group_ids_by_shard(GuildIds, Count),
    {TotalCount, FinalState} = lists:foldl(fun reload_group/2, {0, State}, Groups),
    {#{count => TotalCount}, FinalState}.

-spec remote_manager_call(guild_id(), term(), pos_integer()) -> term().
remote_manager_call(GuildId, Request, Timeout) ->
    case safe_owner_node(GuildId) of
        {ok, OwnerNode} when is_atom(OwnerNode), OwnerNode =/= node() ->
            remote_manager_rpc(OwnerNode, Request, Timeout);
        _ ->
            {error, unavailable}
    end.

-spec remote_manager_rpc(node(), term(), pos_integer()) -> term().
remote_manager_rpc(OwnerNode, Request, Timeout) ->
    Args = [Request, Timeout],
    case rpc:call(OwnerNode, guild_manager, call_via_manager_local, Args, Timeout + 2000) of
        {badrpc, _Reason} -> {error, unavailable};
        Reply -> Reply
    end.

-spec safe_owner_node(guild_id()) -> {ok, node()} | error.
safe_owner_node(GuildId) ->
    try gateway_node_router:owner_node_result(GuildId, guilds) of
        {ok, OwnerNode} when is_atom(OwnerNode) -> {ok, OwnerNode};
        {error, _Reason} -> error
    catch
        throw:_Reason -> error;
        error:_Reason -> error;
        exit:_Reason -> error
    end.

-spec owner_check_for(term(), guild_id()) -> ok | {error, term()}.
owner_check_for({start_or_lookup, _GuildId}, GuildId) ->
    ensure_local_owner(GuildId);
owner_check_for({lookup, _GuildId}, GuildId) ->
    ensure_local_owner(GuildId);
owner_check_for(_Request, _GuildId) ->
    ok.

-spec forward_owned_call(guild_id(), term(), pos_integer(), gen_server:from(), state()) ->
    {reply, term(), state()} | {noreply, state()}.
forward_owned_call(GuildId, Request, Timeout, From, State) ->
    case can_use_cache(Request) of
        true -> forward_with_cache(GuildId, Request, Timeout, From, State);
        false -> forward_call_to_shard(GuildId, Request, Timeout, From, State)
    end.

-spec can_use_cache(term()) -> boolean().
can_use_cache({start_or_lookup, _GuildId}) -> true;
can_use_cache({lookup, _GuildId}) -> true;
can_use_cache(_Request) -> false.

-spec forward_with_cache(guild_id(), term(), pos_integer(), gen_server:from(), state()) ->
    {reply, term(), state()} | {noreply, state()}.
forward_with_cache(GuildId, Request, Timeout, From, State) ->
    case guild_manager_cache:lookup_cached_guild_pid(GuildId) of
        {ok, GuildPid} -> {reply, {ok, GuildPid}, State};
        not_found -> forward_call_to_shard(GuildId, Request, Timeout, From, State)
    end.

-spec reply_from_shard(pid(), guild_id(), term(), pos_integer(), gen_server:from()) -> ok.
reply_from_shard(Pid, GuildId, Request, Timeout, From) ->
    erlang:process_flag(fullsweep_after, 0),
    Reply0 = shard_utils:safe_gen_call_remote(Pid, Request, Timeout),
    Reply = guild_manager_cache:maybe_cache_guild_pid(GuildId, Request, Reply0),
    gen_server:reply(From, Reply).

-spec count_from_shard(pid(), term()) -> {ok, non_neg_integer()} | error.
count_from_shard(Pid, Request) ->
    case shard_utils:safe_gen_call_remote(Pid, Request, ?DEFAULT_GEN_SERVER_TIMEOUT) of
        {ok, Count} when is_integer(Count), Count >= 0 -> {ok, Count};
        Count when is_integer(Count), Count >= 0 -> {ok, Count};
        _Other -> error
    end.

-spec sum_valid_counts([{ok, non_neg_integer()} | error]) -> {ok, non_neg_integer()}.
sum_valid_counts(Counts) ->
    {ok, lists:sum([Count || {ok, Count} <- Counts])}.

-spec reload_group({non_neg_integer(), [guild_id()]}, {non_neg_integer(), state()}) ->
    {non_neg_integer(), state()}.
reload_group({Index, Ids}, {AccCount, AccState}) ->
    {ShardIdx, State1} = guild_manager_shards:ensure_shard_for_index(Index, AccState),
    Shards = maps:get(shards, State1),
    #{pid := Pid} = maps:get(ShardIdx, Shards),
    Reply = shard_utils:safe_gen_call_remote(Pid, {reload_all_guilds, Ids}, 15000),
    {AccCount + reload_count(Reply), State1}.

-spec sum_reload_counts([term()]) -> non_neg_integer().
sum_reload_counts(Replies) ->
    trunc(lists:sum([Count || Reply <- Replies, {ok, Count} <- [reload_count_result(Reply)]])).

-spec reload_count(term()) -> non_neg_integer().
reload_count(#{count := Count}) when is_integer(Count), Count >= 0 ->
    Count;
reload_count(_Reply) ->
    0.

-spec reload_count_result(term()) -> {ok, non_neg_integer()} | error.
reload_count_result(#{count := Count}) when is_integer(Count), Count >= 0 ->
    {ok, Count};
reload_count_result(_Reply) ->
    error.
