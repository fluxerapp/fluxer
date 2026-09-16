%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_bot_guilds).
-typing([eqwalizer]).

-export([
    guild_event/3,
    track_join/2,
    forget_joins/2
]).

-export_type([session_state/0, guild_id/0, event/0]).

-type session_state() :: session:session_state().
-type guild_id() :: session:guild_id().
-type event() :: session_dispatch:event().

-spec guild_event(event(), map(), session_state()) -> {map(), session_state()}.
guild_event(guild_create, Data, #{bot := true} = State) ->
    guild_create(type_conv:extract_id(Data, <<"id">>), Data, State);
guild_event(guild_delete, #{<<"unavailable">> := true} = Data, State) ->
    {Data, State};
guild_event(guild_delete, Data, #{bot := true} = State) ->
    {Data, forget_join(type_conv:extract_id(Data, <<"id">>), State)};
guild_event(_Event, Data, State) ->
    {Data, State}.

-spec guild_create(guild_id() | undefined, map(), session_state()) -> {map(), session_state()}.
guild_create(undefined, Data, State) ->
    {Data#{<<"unavailable">> => false}, State};
guild_create(GuildId, Data, State) ->
    Pending = pending_joins(State),
    case maps:is_key(GuildId, Pending) of
        true -> {Data, State#{pending_guild_joins => maps:remove(GuildId, Pending)}};
        false -> {Data#{<<"unavailable">> => false}, State}
    end.

-spec track_join(guild_id(), session_state()) -> session_state().
track_join(GuildId, #{bot := true} = State) ->
    State#{pending_guild_joins => (pending_joins(State))#{GuildId => true}};
track_join(_GuildId, State) ->
    State.

-spec forget_join(guild_id() | undefined, session_state()) -> session_state().
forget_join(undefined, State) ->
    State;
forget_join(GuildId, State) ->
    forget_joins([GuildId], State).

-spec forget_joins([guild_id()], session_state()) -> session_state().
forget_joins(GuildIds, State) ->
    State#{pending_guild_joins => maps:without(GuildIds, pending_joins(State))}.

-spec pending_joins(session_state()) -> #{guild_id() => true}.
pending_joins(State) ->
    maps:get(pending_guild_joins, State, #{}).
