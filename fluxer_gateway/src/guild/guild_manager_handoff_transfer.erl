%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_manager_handoff_transfer).
-typing([eqwalizer]).

-include_lib("fluxer_gateway/include/timeout_config.hrl").

-export([handoff_guild_to_owner/3]).

-type guild_id() :: integer().
-type shard_map() :: #{pid := pid(), ref := reference()}.
-type state() :: #{shards := #{non_neg_integer() => shard_map()}, shard_count := pos_integer()}.

-export_type([guild_id/0, state/0]).

-spec handoff_guild_to_owner(guild_id(), node(), state()) -> {boolean(), state()}.
handoff_guild_to_owner(GuildId, TargetNode, State) ->
    {Index, State1} = guild_manager_shards:ensure_shard(GuildId, State),
    #{pid := Shard} = maps:get(Index, maps:get(shards, State1)),
    case local_guild_pid(Shard, GuildId) of
        {ok, GuildPid} ->
            Result = guild_handoff_freeze:transfer(GuildId, GuildPid, Shard, TargetNode, #{}),
            {handed_off(GuildId, TargetNode, Result), State1};
        {error, _Reason} ->
            {false, State1}
    end.

-spec local_guild_pid(pid(), guild_id()) -> {ok, pid()} | {error, term()}.
local_guild_pid(Shard, GuildId) ->
    case
        shard_utils:safe_gen_call_remote(Shard, {lookup, GuildId}, ?DEFAULT_GEN_SERVER_TIMEOUT)
    of
        {ok, GuildPid} when is_pid(GuildPid) -> {ok, GuildPid};
        {error, Reason} -> {error, Reason};
        Other -> {error, {unexpected_lookup_reply, Other}}
    end.

-spec handed_off(guild_id(), node(), guild_handoff_freeze:result()) -> boolean().
handed_off(_GuildId, _TargetNode, {ok, _Report}) ->
    true;
handed_off(GuildId, TargetNode, {error, Report}) ->
    logger:warning(
        "guild_handoff_failed: guild_id=~p target=~p report=~p",
        [GuildId, TargetNode, Report]
    ),
    false.
