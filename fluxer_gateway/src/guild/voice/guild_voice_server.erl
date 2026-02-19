%% Copyright (C) 2026 Fluxer Contributors
%%
%% This file is part of Fluxer.
%%
%% Fluxer is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Affero General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% Fluxer is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%% GNU Affero General Public License for more details.
%%
%% You should have received a copy of the GNU Affero General Public License
%% along with Fluxer. If not, see <https://www.gnu.org/licenses/>.

-module(guild_voice_server).
-behaviour(gen_server).

-export([
    start_link/2,
    stop/1,
    lookup/1
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(REGISTRY_TABLE, guild_voice_registry).
-define(SWEEP_INTERVAL_MS, 10000).
-define(GUILD_CALL_TIMEOUT, 10000).

-type voice_state() :: map().
-type voice_state_map() :: #{binary() => voice_state()}.
-type server_state() :: #{
    guild_id := integer(),
    guild_pid := pid(),
    voice_states := voice_state_map(),
    pending_voice_connections := map(),
    recently_disconnected_voice_states := map()
}.

-spec start_link(integer(), pid()) -> {ok, pid()} | {error, term()}.
start_link(GuildId, GuildPid) ->
    gen_server:start_link(?MODULE, #{guild_id => GuildId, guild_pid => GuildPid}, []).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid, normal, 5000).

-spec lookup(integer()) -> {ok, pid()} | {error, not_found}.
lookup(GuildId) ->
    ensure_registry(),
    case ets:lookup(?REGISTRY_TABLE, GuildId) of
        [{_, Pid}] when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true -> {ok, Pid};
                false ->
                    ets:delete(?REGISTRY_TABLE, GuildId),
                    {error, not_found}
            end;
        _ ->
            {error, not_found}
    end.

-spec init(map()) -> {ok, server_state()}.
init(#{guild_id := GuildId, guild_pid := GuildPid}) ->
    process_flag(trap_exit, true),
    ensure_registry(),
    ets:insert(?REGISTRY_TABLE, {GuildId, self()}),
    erlang:send_after(?SWEEP_INTERVAL_MS, self(), sweep_pending_joins),
    {ok, #{
        guild_id => GuildId,
        guild_pid => GuildPid,
        voice_states => #{},
        pending_voice_connections => #{},
        recently_disconnected_voice_states => #{}
    }}.

-spec handle_call(term(), gen_server:from(), server_state()) ->
    {reply, term(), server_state()}.

handle_call({voice_state_update, Request}, _From, State) ->
    GuildState = build_guild_state(State),
    case guild_voice:voice_state_update(Request, GuildState) of
        {reply, Reply, NewGuildState} ->
            {reply, Reply, apply_guild_state(NewGuildState, State)}
    end;

handle_call({get_voice_state, Request}, _From, State) ->
    GuildState = build_guild_state(State),
    case guild_voice:get_voice_state(Request, GuildState) of
        {reply, Reply, NewGuildState} ->
            {reply, Reply, apply_guild_state(NewGuildState, State)}
    end;

handle_call({update_member_voice, Request}, _From, State) ->
    GuildState = build_guild_state(State),
    case guild_voice:update_member_voice(Request, GuildState) of
        {reply, Reply, NewGuildState} ->
            {reply, Reply, apply_guild_state(NewGuildState, State)}
    end;

handle_call({disconnect_voice_user, Request}, _From, State) ->
    GuildState = build_guild_state(State),
    case guild_voice:disconnect_voice_user(Request, GuildState) of
        {reply, Reply, NewGuildState} ->
            {reply, Reply, apply_guild_state(NewGuildState, State)}
    end;

handle_call({disconnect_voice_user_if_in_channel, Request}, _From, State) ->
    GuildState = build_guild_state(State),
    case guild_voice:disconnect_voice_user_if_in_channel(Request, GuildState) of
        {reply, Reply, NewGuildState} ->
            {reply, Reply, apply_guild_state(NewGuildState, State)}
    end;

handle_call({disconnect_all_voice_users_in_channel, Request}, _From, State) ->
    GuildState = build_guild_state(State),
    case guild_voice:disconnect_all_voice_users_in_channel(Request, GuildState) of
        {reply, Reply, NewGuildState} ->
            {reply, Reply, apply_guild_state(NewGuildState, State)}
    end;

handle_call({confirm_voice_connection_from_livekit, Request}, _From, State) ->
    GuildState = build_guild_state(State),
    case guild_voice:confirm_voice_connection_from_livekit(Request, GuildState) of
        {reply, Reply, NewGuildState} ->
            {reply, Reply, apply_guild_state(NewGuildState, State)}
    end;

handle_call({move_member, Request}, _From, State) ->
    GuildState = build_guild_state(State),
    case guild_voice:move_member(Request, GuildState) of
        {reply, Reply, NewGuildState} ->
            {reply, Reply, apply_guild_state(NewGuildState, State)}
    end;

handle_call({switch_voice_region, Request}, _From, State) ->
    GuildState = build_guild_state(State),
    case guild_voice:switch_voice_region_handler(Request, GuildState) of
        {reply, Reply, NewGuildState} ->
            {reply, Reply, apply_guild_state(NewGuildState, State)}
    end;

handle_call({store_pending_connection, ConnectionId, Metadata}, _From, State) ->
    PendingConnections = maps:get(pending_voice_connections, State, #{}),
    NewPendingConnections = maps:put(ConnectionId, Metadata, PendingConnections),
    NewState = maps:put(pending_voice_connections, NewPendingConnections, State),
    {reply, ok, NewState};

handle_call({get_voice_states_for_channel, ChannelIdBin}, _From, State) ->
    VoiceStates = maps:get(voice_states, State, #{}),
    Filtered = maps:fold(
        fun(ConnId, VS, Acc) ->
            case maps:get(<<"channel_id">>, VS, null) of
                ChannelIdBin ->
                    [#{
                        connection_id => ConnId,
                        user_id => maps:get(<<"user_id">>, VS, null),
                        channel_id => ChannelIdBin
                    } | Acc];
                _ ->
                    Acc
            end
        end,
        [],
        VoiceStates
    ),
    {reply, #{voice_states => Filtered}, State};

handle_call({get_pending_joins_for_channel, ChannelIdBin}, _From, State) ->
    PendingConnections = maps:get(pending_voice_connections, State, #{}),
    ChannelIdInt = binary_to_integer(ChannelIdBin),
    Filtered = maps:fold(
        fun(ConnId, Metadata, Acc) ->
            case maps:get(channel_id, Metadata, undefined) of
                ChannelIdInt ->
                    [#{
                        connection_id => ConnId,
                        user_id => integer_to_binary(maps:get(user_id, Metadata, 0)),
                        token_nonce => maps:get(token_nonce, Metadata, null),
                        expires_at => maps:get(expires_at, Metadata, 0)
                    } | Acc];
                _ ->
                    Acc
            end
        end,
        [],
        PendingConnections
    ),
    {reply, #{pending_joins => Filtered}, State};

handle_call({get_voice_states_list}, _From, State) ->
    VoiceStates = maps:get(voice_states, State, #{}),
    {reply, maps:values(VoiceStates), State};

handle_call({get_voice_states_map}, _From, State) ->
    {reply, maps:get(voice_states, State, #{}), State};

handle_call({set_voice_states, VoiceStates}, _From, State) ->
    {reply, ok, maps:put(voice_states, VoiceStates, State)};

handle_call(_, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), server_state()) -> {noreply, server_state()}.

handle_cast({store_pending_connection, ConnectionId, Metadata}, State) ->
    PendingConnections = maps:get(pending_voice_connections, State, #{}),
    NewPendingConnections = maps:put(ConnectionId, Metadata, PendingConnections),
    NewState = maps:put(pending_voice_connections, NewPendingConnections, State),
    {noreply, NewState};

handle_cast({relay_voice_state_update, VoiceState, OldChannelIdBin}, State) ->
    GuildState = build_guild_state(State),
    State1 = relay_upsert_voice_state(VoiceState, State),
    GuildStateNoRelay = maps:remove(very_large_guild_coordinator_pid, GuildState),
    _ = guild_voice_broadcast:broadcast_voice_state_update(
        VoiceState, GuildStateNoRelay, OldChannelIdBin
    ),
    {noreply, State1};

handle_cast(
    {relay_voice_server_update, GuildId, ChannelId, SessionId, Token, Endpoint, ConnectionId},
    State
) ->
    GuildState = build_guild_state(State),
    GuildStateNoRelay = maps:remove(very_large_guild_coordinator_pid, GuildState),
    _ = guild_voice_broadcast:broadcast_voice_server_update_to_session(
        GuildId,
        ChannelId,
        SessionId,
        Token,
        Endpoint,
        ConnectionId,
        GuildStateNoRelay
    ),
    {noreply, State};

handle_cast({cleanup_virtual_access_for_user, UserId}, State) ->
    GuildState = build_guild_state(State),
    NewGuildState = guild_voice_disconnect:cleanup_virtual_channel_access_for_user(
        UserId, GuildState
    ),
    {noreply, apply_guild_state(NewGuildState, State)};

handle_cast(_, State) ->
    {noreply, State}.

-spec handle_info(term(), server_state()) ->
    {noreply, server_state()} | {stop, normal, server_state()}.

handle_info(sweep_pending_joins, State) ->
    GuildState = build_guild_state(State),
    NewGuildState = guild_voice_connection:sweep_expired_pending_joins(GuildState),
    erlang:send_after(?SWEEP_INTERVAL_MS, self(), sweep_pending_joins),
    {noreply, apply_guild_state(NewGuildState, State)};

handle_info({'EXIT', Pid, Reason}, #{guild_pid := GuildPid} = State) when Pid =:= GuildPid ->
    logger:info(
        "Voice server shutting down because guild process exited",
        #{guild_id => maps:get(guild_id, State), reason => Reason}
    ),
    {stop, normal, State};

handle_info(_, State) ->
    {noreply, State}.

-spec terminate(term(), server_state()) -> ok.
terminate(_Reason, #{guild_id := GuildId}) ->
    catch ets:delete(?REGISTRY_TABLE, GuildId),
    ok;
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), server_state(), term()) -> {ok, server_state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec build_guild_state(server_state()) -> map().
build_guild_state(#{guild_pid := GuildPid} = State) ->
    GuildData = fetch_guild_data(GuildPid),
    maps:merge(GuildData, #{
        voice_states => maps:get(voice_states, State, #{}),
        pending_voice_connections => maps:get(pending_voice_connections, State, #{}),
        recently_disconnected_voice_states =>
            maps:get(recently_disconnected_voice_states, State, #{})
    }).

-spec apply_guild_state(map(), server_state()) -> server_state().
apply_guild_state(GuildState, State) ->
    State#{
        voice_states => maps:get(voice_states, GuildState, maps:get(voice_states, State, #{})),
        pending_voice_connections =>
            maps:get(
                pending_voice_connections,
                GuildState,
                maps:get(pending_voice_connections, State, #{})
            ),
        recently_disconnected_voice_states =>
            maps:get(
                recently_disconnected_voice_states,
                GuildState,
                maps:get(recently_disconnected_voice_states, State, #{})
            )
    }.

-spec fetch_guild_data(pid()) -> map().
fetch_guild_data(GuildPid) ->
    try gen_server:call(GuildPid, {get_sessions}, ?GUILD_CALL_TIMEOUT) of
        GuildState when is_map(GuildState) ->
            GuildState;
        _ ->
            #{}
    catch
        exit:{timeout, _} ->
            logger:warning("Voice server timed out fetching guild state", #{}),
            #{};
        exit:{noproc, _} ->
            #{};
        exit:{normal, _} ->
            #{}
    end.

-spec relay_upsert_voice_state(map(), server_state()) -> server_state().
relay_upsert_voice_state(VoiceState, State) when is_map(VoiceState) ->
    ConnectionId = maps:get(<<"connection_id">>, VoiceState, undefined),
    case ConnectionId of
        undefined ->
            State;
        _ ->
            VoiceStates0 = maps:get(voice_states, State, #{}),
            ChannelId = maps:get(<<"channel_id">>, VoiceState, null),
            VoiceStates =
                case ChannelId of
                    null -> maps:remove(ConnectionId, VoiceStates0);
                    _ -> maps:put(ConnectionId, VoiceState, VoiceStates0)
                end,
            maps:put(voice_states, VoiceStates, State)
    end;
relay_upsert_voice_state(_, State) ->
    State.

-spec ensure_registry() -> ok.
ensure_registry() ->
    guild_ets_utils:ensure_table(?REGISTRY_TABLE, [
        named_table,
        public,
        set,
        {read_concurrency, true},
        {write_concurrency, true}
    ]).
