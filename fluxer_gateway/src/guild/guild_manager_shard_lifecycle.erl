%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_manager_shard_lifecycle).
-typing([eqwalizer]).

-include_lib("fluxer_gateway/include/timeout_config.hrl").

-export([
    do_start_transferred/3,
    do_stop_guild/2,
    do_stop_guild/3,
    do_shutdown_guild/2,
    start_guild/3,
    start_new_guild/4,
    start_transferred_guild/3,
    start_new_guild_from_state/4,
    normalize_transferred_guild_state/2
]).

-type guild_id() :: integer().
-type guild_data() :: #{binary() => term()}.
-type state() :: map().

-export_type([guild_id/0, guild_data/0, state/0]).

-spec do_start_transferred(guild_id(), map(), state()) ->
    {reply, {ok, pid()} | {error, term()}, state()}.
do_start_transferred(GuildId, TransferState, State) when is_map(TransferState) ->
    case ensure_local_owner(GuildId) of
        ok -> reply_start_transferred(GuildId, TransferState, State);
        {error, Reason} -> {reply, {error, Reason}, State}
    end;
do_start_transferred(_GuildId, _TransferState, State) ->
    {reply, {error, invalid_transfer_state}, State}.

-spec do_stop_guild(guild_id(), state()) -> {reply, ok, state()}.
do_stop_guild(GuildId, State) ->
    do_stop_guild(GuildId, normal, State).

-spec do_stop_guild(guild_id(), term(), state()) -> {reply, ok, state()}.
do_stop_guild(GuildId, Reason, State) ->
    Guilds = maps:get(guilds, State),
    GuildKey = process_registry:build_process_key(guild, GuildId),
    case maps:get(GuildId, Guilds, undefined) of
        {Pid, Ref} -> stop_tracked_guild(GuildId, GuildKey, Pid, Ref, Reason, State);
        _Other -> stop_registered_guild(GuildKey, Reason, State)
    end.

-spec do_shutdown_guild(guild_id(), state()) -> {reply, ok, state()}.
do_shutdown_guild(GuildId, State) ->
    Guilds = maps:get(guilds, State),
    GuildKey = process_registry:build_process_key(guild, GuildId),
    case maps:get(GuildId, Guilds, undefined) of
        {Pid, Ref} -> shutdown_tracked_guild(GuildId, GuildKey, Pid, Ref, State);
        _Other -> shutdown_registered_guild(GuildKey, State)
    end.

-spec start_guild(guild_id(), guild_data(), state()) -> {ok, pid(), state()} | {error, term()}.
start_guild(GuildId, Data, State) ->
    case ensure_local_owner(GuildId) of
        ok -> start_owned_guild(GuildId, Data, State);
        {error, Reason} -> {error, Reason}
    end.

-spec start_new_guild(guild_id(), guild_data(), process_registry:process_key(), state()) ->
    {ok, pid(), state()} | {error, term()}.
start_new_guild(GuildId, Data, GuildKey, State) ->
    GuildState = #{id => GuildId, data => Data, sessions => #{}},
    start_new_guild_from_state(GuildId, GuildState, GuildKey, State).

-spec start_transferred_guild(guild_id(), map(), state()) ->
    {ok, pid(), state()} | {error, term()}.
start_transferred_guild(GuildId, TransferState, State) ->
    GuildKey = process_registry:build_process_key(guild, GuildId),
    GuildState = normalize_transferred_guild_state(GuildId, TransferState),
    case process_registry:registry_whereis(GuildKey) of
        undefined -> start_new_guild_from_state(GuildId, GuildState, GuildKey, State);
        _ExistingPid -> lookup_existing_guild(GuildId, GuildKey, State)
    end.

-spec start_new_guild_from_state(guild_id(), map(), process_registry:process_key(), state()) ->
    {ok, pid(), state()} | {error, term()}.
start_new_guild_from_state(GuildId, GuildState, GuildKey, State) ->
    case process_registry:registry_whereis(GuildKey) of
        undefined -> start_unregistered_guild(GuildId, GuildState, GuildKey, State);
        _AlreadyRegistered -> lookup_existing_guild(GuildId, GuildKey, State)
    end.

-spec normalize_transferred_guild_state(guild_id(), map()) -> map().
normalize_transferred_guild_state(GuildId, TransferState) ->
    #{
        id => GuildId,
        data => maps:get(data, TransferState, #{}),
        sessions => maps:get(sessions, TransferState, #{}),
        voice_states => maps:get(voice_states, TransferState, #{}),
        virtual_channel_access => maps:get(virtual_channel_access, TransferState, #{}),
        virtual_channel_access_pending => maps:get(
            virtual_channel_access_pending, TransferState, #{}
        ),
        virtual_channel_access_preserve => maps:get(
            virtual_channel_access_preserve, TransferState, #{}
        ),
        virtual_channel_access_move_pending =>
            maps:get(virtual_channel_access_move_pending, TransferState, #{}),
        virtual_channel_access_view_only =>
            maps:get(virtual_channel_access_view_only, TransferState, #{})
    }.

-spec reply_start_transferred(guild_id(), map(), state()) ->
    {reply, {ok, pid()} | {error, term()}, state()}.
reply_start_transferred(GuildId, TransferState, State) ->
    case start_transferred_guild(GuildId, TransferState, State) of
        {ok, Pid, NewState} -> {reply, {ok, Pid}, NewState};
        {error, Reason} -> {reply, {error, Reason}, State}
    end.

-spec start_owned_guild(guild_id(), guild_data(), state()) ->
    {ok, pid(), state()} | {error, term()}.
start_owned_guild(GuildId, Data, State) ->
    GuildKey = process_registry:build_process_key(guild, GuildId),
    case process_registry:registry_whereis(GuildKey) of
        undefined -> start_new_guild(GuildId, Data, GuildKey, State);
        _ExistingPid -> lookup_existing_guild(GuildId, GuildKey, State)
    end.

-spec start_unregistered_guild(guild_id(), map(), process_registry:process_key(), state()) ->
    {ok, pid(), state()} | {error, term()}.
start_unregistered_guild(GuildId, GuildState, GuildKey, State) ->
    Guilds = maps:get(guilds, State),
    case guild:start_link(GuildState) of
        {ok, Pid} ->
            register_started_guild(GuildId, GuildKey, Pid, Guilds, State);
        {error, Reason} ->
            {error, Reason}
    end.

-spec register_started_guild(
    guild_id(), process_registry:process_key(), pid(), map(), state()
) -> {ok, pid(), state()} | {error, term()}.
register_started_guild(GuildId, GuildKey, Pid, Guilds, State) ->
    case process_registry:register_and_monitor(GuildKey, Pid, Guilds) of
        {ok, RegisteredPid, Ref, NewGuilds0} ->
            CleanGuilds = maps:remove(GuildKey, NewGuilds0),
            {ok, RegisteredPid, State#{guilds => CleanGuilds#{GuildId => {RegisteredPid, Ref}}}};
        {error, Reason} ->
            stop_orphaned_guild(Pid),
            {error, Reason}
    end.

-spec stop_orphaned_guild(pid()) -> ok.
stop_orphaned_guild(Pid) ->
    case process_liveness:is_alive(Pid) of
        true -> safe_stop(Pid, shutdown);
        false -> ok
    end.

-spec lookup_existing_guild(guild_id(), process_registry:process_key(), state()) ->
    {ok, pid(), state()} | {error, term()}.
lookup_existing_guild(GuildId, GuildKey, State) ->
    Guilds = maps:get(guilds, State),
    case process_registry:lookup_or_monitor(GuildKey, GuildId, Guilds) of
        {ok, Pid, _Ref, NewGuilds} -> {ok, Pid, State#{guilds => NewGuilds}};
        {error, not_found} -> {error, process_died}
    end.

-spec stop_tracked_guild(
    guild_id(), process_registry:process_key(), pid(), reference(), term(), state()
) ->
    {reply, ok, state()}.
stop_tracked_guild(GuildId, GuildKey, Pid, Ref, Reason, State) ->
    demonitor(Ref, [flush]),
    maybe_stop_guild(Pid, Reason),
    process_registry:safe_unregister(GuildKey),
    Guilds = maps:remove(GuildId, maps:get(guilds, State)),
    {reply, ok, State#{guilds => Guilds}}.

-spec stop_registered_guild(process_registry:process_key(), term(), state()) ->
    {reply, ok, state()}.
stop_registered_guild(GuildKey, Reason, State) ->
    case process_registry:registry_whereis(GuildKey) of
        undefined ->
            {reply, ok, State};
        ExistingPid ->
            maybe_stop_guild(ExistingPid, Reason),
            process_registry:safe_unregister(GuildKey),
            {reply, ok, State}
    end.

-spec shutdown_tracked_guild(
    guild_id(), process_registry:process_key(), pid(), reference(), state()
) ->
    {reply, ok, state()}.
shutdown_tracked_guild(GuildId, GuildKey, Pid, Ref, State) ->
    demonitor(Ref, [flush]),
    maybe_terminate_guild(Pid),
    process_registry:safe_unregister(GuildKey),
    Guilds = maps:remove(GuildId, maps:get(guilds, State)),
    {reply, ok, State#{guilds => Guilds}}.

-spec shutdown_registered_guild(process_registry:process_key(), state()) ->
    {reply, ok, state()}.
shutdown_registered_guild(GuildKey, State) ->
    case process_registry:registry_whereis(GuildKey) of
        undefined ->
            {reply, ok, State};
        ExistingPid ->
            maybe_terminate_guild(ExistingPid),
            process_registry:safe_unregister(GuildKey),
            {reply, ok, State}
    end.

-spec maybe_stop_guild(pid(), term()) -> ok.
maybe_stop_guild(Pid, Reason) ->
    case process_liveness:is_alive(Pid) of
        true -> safe_stop(Pid, Reason);
        false -> ok
    end.

-spec maybe_terminate_guild(pid()) -> ok.
maybe_terminate_guild(Pid) ->
    case process_liveness:is_alive(Pid) of
        true ->
            _ = shard_utils:safe_gen_call_detailed(Pid, {terminate}, ?SHUTDOWN_TIMEOUT),
            ok;
        false ->
            ok
    end.

-spec ensure_local_owner(guild_id()) -> ok | {error, term()}.
ensure_local_owner(GuildId) ->
    guild_manager_shard_lookup:ensure_local_owner(GuildId).

-spec safe_stop(pid(), term()) -> ok.
safe_stop(Pid, Reason) ->
    try gen_server:stop(Pid, Reason, ?SHUTDOWN_TIMEOUT) of
        _ -> ok
    catch
        exit:_Reason -> ok
    end.
