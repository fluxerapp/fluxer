%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_manager_handoff_transfer).
-typing([eqwalizer]).

-include_lib("fluxer_gateway/include/timeout_config.hrl").

-export([handoff_guild_to_owner/3, target_shard_pid/2]).

-type guild_id() :: integer().
-type shard_map() :: #{pid := pid(), ref := reference()}.
-type state() :: #{shards := #{non_neg_integer() => shard_map()}, shard_count := pos_integer()}.

-export_type([guild_id/0, state/0]).

-define(SHARD_TABLE, guild_manager_shard_table).

-spec handoff_guild_to_owner(guild_id(), node(), state()) -> {boolean(), state()}.
handoff_guild_to_owner(GuildId, TargetNode, State) ->
    case export_local_guild_state(GuildId, State) of
        {ok, TransferState} ->
            start_transferred_guild_on_owner(
                GuildId, TargetNode, TransferState, State
            );
        {error, _Reason} ->
            {false, State}
    end.

-spec start_transferred_guild_on_owner(guild_id(), node(), map(), state()) ->
    {boolean(), state()}.
start_transferred_guild_on_owner(GuildId, TargetNode, TransferState, State) ->
    Request = {start_transferred, GuildId, TransferState},
    case call_target_shard(GuildId, TargetNode, Request) of
        {ok, Pid} when is_pid(Pid) -> stop_local_guild_after_handoff(GuildId, State);
        _Other -> {false, State}
    end.

-spec call_target_shard(guild_id(), node(), term()) -> term().
call_target_shard(GuildId, TargetNode, Request) ->
    case target_shard_pid(GuildId, TargetNode) of
        {ok, Pid} ->
            shard_utils:safe_gen_call_remote(Pid, Request, ?DEFAULT_GEN_SERVER_TIMEOUT);
        {error, _Reason} = Error ->
            Error
    end.

-spec target_shard_pid(guild_id(), node()) -> {ok, pid()} | {error, term()}.
target_shard_pid(GuildId, TargetNode) ->
    case target_shard_count(TargetNode) of
        {ok, Count} -> lookup_target_shard_pid(GuildId, TargetNode, Count);
        {error, _Reason} = Error -> Error
    end.

-spec target_shard_count(node()) -> {ok, pos_integer()} | {error, term()}.
target_shard_count(TargetNode) ->
    Request = [?SHARD_TABLE, shard_count],
    case rpc:call(TargetNode, ets, lookup, Request, ?DEFAULT_GEN_SERVER_TIMEOUT) of
        [{shard_count, Count}] when is_integer(Count), Count > 0 -> {ok, Count};
        {badrpc, Reason} -> {error, {badrpc, Reason}};
        _Other -> {error, shard_table_unavailable}
    end.

-spec lookup_target_shard_pid(guild_id(), node(), pos_integer()) ->
    {ok, pid()} | {error, term()}.
lookup_target_shard_pid(GuildId, TargetNode, Count) ->
    Index = guild_manager_shards:select_shard(GuildId, Count),
    Request = [?SHARD_TABLE, {shard_pid, Index}],
    case rpc:call(TargetNode, ets, lookup, Request, ?DEFAULT_GEN_SERVER_TIMEOUT) of
        [{{shard_pid, Index}, Pid}] when is_pid(Pid) -> {ok, Pid};
        {badrpc, Reason} -> {error, {badrpc, Reason}};
        _Other -> {error, shard_unavailable}
    end.

-spec export_local_guild_state(guild_id(), state()) -> {ok, map()} | {error, term()}.
export_local_guild_state(GuildId, State) ->
    case local_guild_pid(GuildId, State) of
        {ok, Pid} -> export_guild_state(Pid);
        {error, _Reason} = Error -> Error
    end.

-spec local_guild_pid(guild_id(), state()) -> {ok, pid()} | {error, term()}.
local_guild_pid(GuildId, State) ->
    {Index, State1} = guild_manager_shards:ensure_shard(GuildId, State),
    Shards = maps:get(shards, State1),
    #{pid := Pid} = maps:get(Index, Shards),
    case
        shard_utils:safe_gen_call_remote(Pid, {lookup, GuildId}, ?DEFAULT_GEN_SERVER_TIMEOUT)
    of
        {ok, GuildPid} when is_pid(GuildPid) -> {ok, GuildPid};
        {error, Reason} -> {error, Reason};
        Other -> {error, {unexpected_lookup_reply, Other}}
    end.

-spec stop_local_guild_after_handoff(guild_id(), state()) -> {boolean(), state()}.
stop_local_guild_after_handoff(GuildId, State) ->
    {Index, State1} = guild_manager_shards:ensure_shard(GuildId, State),
    Shards = maps:get(shards, State1),
    #{pid := Pid} = maps:get(Index, Shards),
    Reply = shard_utils:safe_gen_call_remote(
        Pid, {stop_guild, GuildId, {shutdown, handoff}}, ?DEFAULT_GEN_SERVER_TIMEOUT
    ),
    case Reply of
        ok -> {true, State1};
        _Other -> {false, State1}
    end.

-spec export_guild_state(pid()) -> {ok, map()} | {error, term()}.
export_guild_state(Pid) ->
    case
        shard_utils:safe_gen_call_remote(Pid, export_handoff_state, ?DEFAULT_GEN_SERVER_TIMEOUT)
    of
        {ok, TransferState} when is_map(TransferState) -> {ok, TransferState};
        {error, Reason} -> {error, Reason};
        Other -> {error, {unexpected_export_reply, Other}}
    end.
