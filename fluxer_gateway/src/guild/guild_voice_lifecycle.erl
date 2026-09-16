%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_lifecycle).
-typing([eqwalizer]).

-export([
    ensure_voice_server/1,
    handle_voice_server_exit/3,
    reply_voice_server_pid/1,
    clear_stale_cached_voice_states/2
]).

-type guild_state() :: map().

-export_type([guild_state/0]).

-define(VOICE_CACHE_RECOVERY_GRACE_MS, 120000).

-spec handle_voice_server_exit(pid(), term(), guild_state()) -> guild_state().
handle_voice_server_exit(VoiceServerPid, Reason, State) ->
    GuildId = maps:get(id, State, undefined),
    VoiceStates = maps:get(voice_states, State, #{}),
    logger:warning(
        "guild_voice_server_exit:"
        " guild_id=~p voice_server_pid=~p reason=~p"
        " cached_voice_state_count=~p",
        [GuildId, VoiceServerPid, Reason, maps:size(VoiceStates)]
    ),
    case ensure_voice_server(maps:remove(voice_server_pid, State)) of
        {ok, _NewPid, NewState} -> NewState;
        {{error, _}, NewState} -> NewState
    end.

-spec ensure_voice_server(guild_state()) ->
    {ok, pid(), guild_state()} | {{error, atom()}, guild_state()}.
ensure_voice_server(State) ->
    case maps:get(voice_server_pid, State, undefined) of
        Pid when is_pid(Pid) ->
            ensure_alive_voice_server(Pid, State);
        _ ->
            adopt_or_start(State, missing_voice_server)
    end.

-spec ensure_alive_voice_server(pid(), guild_state()) ->
    {ok, pid(), guild_state()} | {{error, atom()}, guild_state()}.
ensure_alive_voice_server(Pid, State) ->
    case process_liveness:is_alive(Pid) of
        true ->
            {ok, Pid, State};
        false ->
            CleanState = maps:remove(voice_server_pid, State),
            adopt_or_start(CleanState, dead_voice_server)
    end.

-spec reply_voice_server_pid(guild_state()) ->
    {reply, {ok, pid()} | {error, term()}, guild_state()}.
reply_voice_server_pid(State) ->
    case ensure_voice_server(State) of
        {ok, Pid, NewState} -> {reply, {ok, Pid}, NewState};
        {{error, Reason}, NewState} -> {reply, {error, Reason}, NewState}
    end.

-spec clear_stale_cached_voice_states([binary()], guild_state()) -> guild_state().
clear_stale_cached_voice_states(ConnectionIds, State) ->
    case read_authoritative_voice_states(State) of
        {ok, AuthoritativeVS} ->
            clear_stale_vs(ConnectionIds, AuthoritativeVS, State);
        {error, _} ->
            State
    end.

-spec clear_stale_vs([binary()], map(), guild_state()) -> guild_state().
clear_stale_vs(ConnectionIds, AuthoritativeVS, State) ->
    LocalVS = maps:get(voice_states, State, #{}),
    StaleVS = maps:filter(
        fun(ConnId, _VS) ->
            lists:member(ConnId, ConnectionIds) andalso
                not maps:is_key(ConnId, AuthoritativeVS)
        end,
        LocalVS
    ),
    remove_stale_voice_states(StaleVS, LocalVS, State).

-spec remove_stale_voice_states(map(), map(), guild_state()) -> guild_state().
remove_stale_voice_states(StaleVS, _LocalVS, State) when map_size(StaleVS) =:= 0 ->
    State;
remove_stale_voice_states(StaleVS, LocalVS, State) ->
    NewVS = maps:without(maps:keys(StaleVS), LocalVS),
    NewState = State#{voice_states => NewVS},
    GuildId = maps:get(id, State, undefined),
    StaleCount = maps:size(StaleVS),
    logger:warning(
        "guild_stale_cached_voice_states_cleared:"
        " guild_id=~p cleared_count=~p",
        [GuildId, StaleCount]
    ),
    voice_state_utils:broadcast_disconnects(StaleVS, NewState),
    NewState.

-spec adopt_or_start(guild_state(), atom()) ->
    {ok, pid(), guild_state()} | {{error, atom()}, guild_state()}.
adopt_or_start(State, Reason) ->
    case state_guild_id(State) of
        {ok, GuildId} ->
            adopt_registered_or_start(State, GuildId, Reason);
        error ->
            {{error, no_voice_server}, maps:remove(voice_server_pid, State)}
    end.

-spec adopt_registered_or_start(guild_state(), integer(), atom()) ->
    {ok, pid(), guild_state()} | {{error, atom()}, guild_state()}.
adopt_registered_or_start(State, GuildId, Reason) ->
    case guild_voice_server:lookup_registered(GuildId) of
        {ok, VoiceServerPid} ->
            logger:warning(
                "guild_voice_server_adopted: guild_id=~p voice_server_pid=~p reason=~p",
                [GuildId, VoiceServerPid, Reason]
            ),
            {ok, VoiceServerPid, State#{voice_server_pid => VoiceServerPid}};
        {error, not_found} ->
            start_empty_replacement(State, GuildId, Reason)
    end.

-spec start_empty_replacement(guild_state(), integer(), atom()) ->
    {ok, pid(), guild_state()} | {{error, atom()}, guild_state()}.
start_empty_replacement(State, GuildId, Reason) ->
    CachedVoiceStates = voice_state_utils:ensure_voice_states(
        maps:get(voice_states, State, #{})
    ),
    CachedCount = maps:size(CachedVoiceStates),
    maybe
        {ok, VoicePid} ?= guild_voice_server:start_link(GuildId, self(), #{}),
        logger:warning(
            "guild_voice_server_restarted_empty:"
            " guild_id=~p voice_server_pid=~p reason=~p"
            " cached_voice_state_count=~p",
            [GuildId, VoicePid, Reason, CachedCount]
        ),
        ok = schedule_stale_cleanup(maps:keys(CachedVoiceStates)),
        {ok, VoicePid, State#{voice_server_pid => VoicePid}}
    else
        {error, StartReason} ->
            logger:error(
                "guild_voice_server_restart_failed: guild_id=~p reason=~p start_reason=~p",
                [GuildId, Reason, StartReason]
            ),
            {{error, no_voice_server}, maps:remove(voice_server_pid, State)}
    end.

-spec schedule_stale_cleanup([binary()]) -> ok.
schedule_stale_cleanup([]) ->
    ok;
schedule_stale_cleanup(ConnectionIds) ->
    _ = erlang:send_after(
        ?VOICE_CACHE_RECOVERY_GRACE_MS,
        self(),
        {clear_stale_cached_voice_states, lists:usort(ConnectionIds)}
    ),
    ok.

-spec read_authoritative_voice_states(guild_state()) -> {ok, map()} | {error, term()}.
read_authoritative_voice_states(State) ->
    case maps:get(voice_server_pid, State, undefined) of
        VoiceServerPid when is_pid(VoiceServerPid) ->
            read_from_pid_or_registry(VoiceServerPid, State);
        _ ->
            read_from_registry(State)
    end.

-spec read_from_pid_or_registry(pid(), guild_state()) -> {ok, map()} | {error, term()}.
read_from_pid_or_registry(Pid, State) ->
    case process_liveness:is_alive(Pid) of
        true -> read_from_pid(Pid);
        false -> read_from_registry(State)
    end.

-spec read_from_registry(guild_state()) -> {ok, map()} | {error, term()}.
read_from_registry(State) ->
    case state_guild_id(State) of
        {ok, Id} -> read_registered_pid(Id);
        error -> {error, no_guild_id}
    end.

-spec read_registered_pid(integer()) -> {ok, map()} | {error, term()}.
read_registered_pid(Id) ->
    case guild_voice_server:lookup_registered(Id) of
        {ok, Pid} -> read_from_pid(Pid);
        {error, Reason} -> {error, Reason}
    end.

-spec state_guild_id(guild_state()) -> {ok, integer()} | error.
state_guild_id(State) ->
    case maps:get(id, State, undefined) of
        Id when is_integer(Id) -> {ok, Id};
        _ -> error
    end.

-spec read_from_pid(pid()) -> {ok, map()} | {error, term()}.
read_from_pid(VoiceServerPid) ->
    try gen_server:call(VoiceServerPid, {get_voice_states_map}, 500) of
        VS when is_map(VS) -> {ok, voice_state_utils:ensure_voice_states(VS)};
        Other -> {error, Other}
    catch
        exit:Reason -> {error, Reason}
    end.
