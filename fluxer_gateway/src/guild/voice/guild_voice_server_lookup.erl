%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_server_lookup).
-typing([eqwalizer]).

-export([registered/1, recover/1, after_miss/2]).

-define(REGISTRY_TABLE, guild_voice_registry).

-spec registered(integer()) -> {ok, pid()} | {error, not_found}.
registered(GuildId) ->
    Result = safe_lookup(fun() -> ets:lookup(?REGISTRY_TABLE, GuildId) end),
    case Result of
        [{_, Pid}] when is_pid(Pid) -> live_pid_result(Pid);
        _ -> {error, not_found}
    end.

-spec after_miss(integer(), pid()) -> {ok, pid()} | {error, not_found}.
after_miss(GuildId, FallbackPid) ->
    case resolve_from_owner(GuildId) of
        {ok, VoicePid} -> {ok, VoicePid};
        {error, not_found} -> resolve_from_guild_pid(FallbackPid)
    end.

-spec safe_lookup(fun(() -> term())) -> term().
safe_lookup(Fun) ->
    try
        Fun()
    catch
        throw:_ -> {error, not_found};
        error:_ -> {error, not_found};
        exit:_ -> {error, not_found}
    end.

-spec safe_lookup_match(term()) -> {ok, pid()} | {error, not_found}.
safe_lookup_match({ok, Pid}) when is_pid(Pid) -> {ok, Pid};
safe_lookup_match(_) -> {error, not_found}.

-spec resolve_from_owner(integer()) -> {ok, pid()} | {error, not_found}.
resolve_from_owner(GuildId) ->
    Result = safe_lookup(fun() -> gateway_node_router:owner_node_result(GuildId, guilds) end),
    case Result of
        {ok, Owner} when is_atom(Owner), Owner =/= node() ->
            lookup_on_owner(Owner, GuildId);
        _ ->
            {error, not_found}
    end.

-spec lookup_on_owner(atom(), integer()) -> {ok, pid()} | {error, not_found}.
lookup_on_owner(Owner, GuildId) ->
    Result = safe_lookup(fun() ->
        rpc:call(Owner, guild_voice_server, lookup, [GuildId], 1000)
    end),
    safe_lookup_match(Result).

-spec resolve_from_guild_pid(pid()) -> {ok, pid()} | {error, not_found}.
resolve_from_guild_pid(FallbackPid) ->
    Result = safe_lookup(fun() -> gen_server:call(FallbackPid, get_voice_server_pid, 500) end),
    case Result of
        {ok, VoicePid} when is_pid(VoicePid) -> live_pid_result(VoicePid);
        _ -> {error, not_found}
    end.

-spec recover(integer()) -> {ok, pid()} | {error, not_found}.
recover(GuildId) ->
    Result = safe_lookup(fun() -> ets:lookup(guild_pid_cache, GuildId) end),
    case Result of
        [{_, GuildPid}] when is_pid(GuildPid) ->
            ask_live_guild_for_voice_pid(GuildId, GuildPid);
        _ ->
            {error, not_found}
    end.

-spec ask_live_guild_for_voice_pid(integer(), pid()) -> {ok, pid()} | {error, not_found}.
ask_live_guild_for_voice_pid(GuildId, GuildPid) ->
    case process_liveness:is_alive(GuildPid) of
        true -> ask_guild_for_voice_pid(GuildId, GuildPid);
        false -> {error, not_found}
    end.

-spec ask_guild_for_voice_pid(integer(), pid()) -> {ok, pid()} | {error, not_found}.
ask_guild_for_voice_pid(GuildId, GuildPid) ->
    Result = safe_lookup(fun() -> gen_server:call(GuildPid, get_voice_server_pid, 500) end),
    case Result of
        {ok, VSP} when is_pid(VSP) ->
            cache_live_voice_pid(GuildId, VSP);
        _ ->
            {error, not_found}
    end.

-spec live_pid_result(pid()) -> {ok, pid()} | {error, not_found}.
live_pid_result(Pid) ->
    case process_liveness:is_alive(Pid) of
        true -> {ok, Pid};
        false -> {error, not_found}
    end.

-spec cache_live_voice_pid(integer(), pid()) -> {ok, pid()} | {error, not_found}.
cache_live_voice_pid(GuildId, VoicePid) ->
    case live_pid_result(VoicePid) of
        {ok, VoicePid} ->
            ets:insert(?REGISTRY_TABLE, {GuildId, VoicePid}),
            {ok, VoicePid};
        {error, not_found} ->
            {error, not_found}
    end.
