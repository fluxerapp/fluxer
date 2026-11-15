%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_ready).
-typing([eqwalizer]).

-export([
    process_guild_state/2,
    mark_guild_unavailable/2,
    mark_guild_unavailable/3,
    check_readiness/1,
    dispatch_ready_data/1
]).

-export_type([session_state/0, guild_id/0]).

-type session_state() :: session:session_state().
-type guild_id() :: session:guild_id().

-spec process_guild_state(map(), session_state()) ->
    {noreply, session_state()} | {stop, normal, session_state()}.
process_guild_state(GuildState, #{ready := undefined} = State) ->
    Event = session_ready_dispatch:guild_state_event(GuildState),
    session_dispatch:handle_dispatch(Event, GuildState, State);
process_guild_state(GuildState, State) ->
    CollectedGuilds = maps:get(collected_guild_states, State),
    DetachedGuildState = detach_guild_state(GuildState),
    NewState = State#{
        collected_guild_states => [DetachedGuildState | CollectedGuilds]
    },
    check_readiness(NewState).

-spec detach_guild_state(map()) -> map().
detach_guild_state(GuildState) when is_map(GuildState) ->
    Detached = term_detach:detach(GuildState),
    case Detached of
        M when is_map(M) -> M;
        _ -> #{}
    end.

-spec mark_guild_unavailable(guild_id(), session_state()) -> {noreply, session_state()}.
mark_guild_unavailable(GuildId, State) ->
    mark_guild_unavailable(GuildId, false, State).

-spec mark_guild_unavailable(guild_id(), boolean(), session_state()) ->
    {noreply, session_state()}.
mark_guild_unavailable(GuildId, UnavailableHidden, #{ready := undefined} = State) ->
    UnavailableState = build_unavailable_state(GuildId, UnavailableHidden),
    Guilds = maps:get(guilds, State, #{}),
    GuildMarker = unavailable_guild_marker(GuildId, Guilds),
    UpdatedState = State#{guilds => Guilds#{GuildId => GuildMarker}},
    session_dispatch:handle_dispatch(guild_delete, UnavailableState, UpdatedState);
mark_guild_unavailable(GuildId, UnavailableHidden, State) ->
    UnavailableState = build_unavailable_state(GuildId, UnavailableHidden),
    CollectedGuilds = maps:get(collected_guild_states, State),
    Guilds = maps:get(guilds, State, #{}),
    GuildMarker = unavailable_guild_marker(GuildId, Guilds),
    NewState = State#{
        guilds => Guilds#{GuildId => GuildMarker},
        collected_guild_states => [UnavailableState | CollectedGuilds]
    },
    {noreply, NewState}.

-spec unavailable_guild_marker(guild_id(), map()) -> cached_unavailable | unavailable.
unavailable_guild_marker(GuildId, Guilds) ->
    case maps:get(GuildId, Guilds, undefined) of
        cached_unavailable -> cached_unavailable;
        _ -> unavailable
    end.

-spec build_unavailable_state(guild_id(), boolean()) -> map().
build_unavailable_state(GuildId, true) ->
    #{
        <<"id">> => integer_to_binary(GuildId),
        <<"unavailable">> => true,
        <<"unavailable_hidden">> => true
    };
build_unavailable_state(GuildId, false) ->
    #{<<"id">> => integer_to_binary(GuildId), <<"unavailable">> => true}.

-spec check_readiness(session_state()) ->
    {noreply, session_state()} | {stop, normal, session_state()}.
check_readiness(#{ready := undefined} = State) ->
    {noreply, State};
check_readiness(State) ->
    case maps:get(presence_pid, State, undefined) of
        undefined -> {noreply, State};
        _ -> check_guild_readiness(State)
    end.

-spec check_guild_readiness(session_state()) ->
    {noreply, session_state()} | {stop, normal, session_state()}.
check_guild_readiness(#{guilds := Guilds} = State) ->
    case all_guilds_connected(Guilds) of
        true -> dispatch_ready_data(State);
        false -> {noreply, State}
    end;
check_guild_readiness(State) ->
    {noreply, State}.

-spec all_guilds_connected(#{guild_id() => term()}) -> boolean().
all_guilds_connected(Guilds) ->
    not lists:member(undefined, maps:values(Guilds)).

-spec dispatch_ready_data(session_state()) ->
    {noreply, session_state()} | {stop, normal, session_state()}.
dispatch_ready_data(#{socket_pid := undefined} = State) ->
    {stop, normal, State};
dispatch_ready_data(State) ->
    session_ready_dispatch:dispatch_ready_to_socket(State).
