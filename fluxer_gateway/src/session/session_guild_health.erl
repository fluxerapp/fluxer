%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_guild_health).
-typing([eqwalizer]).

-export([handle_update/4, dispatch_ready/1, forget/2]).

-spec handle_update(integer(), pid(), boolean(), map()) -> {noreply, map()}.
handle_update(GuildId, GuildPid, Degraded, State) ->
    case maps:get(GuildId, maps:get(guilds, State, #{}), undefined) of
        {GuildPid, _Ref} -> apply_update(GuildId, GuildPid, Degraded, State);
        _ -> {noreply, State}
    end.

-spec apply_update(integer(), pid(), boolean(), map()) -> {noreply, map()}.
apply_update(GuildId, GuildPid, Degraded, State) ->
    Health = maps:get(guild_health, State, #{}),
    Previous =
        case maps:get(GuildId, Health, undefined) of
            {_OldPid, Value} -> Value;
            _ -> false
        end,
    State1 = State#{guild_health => Health#{GuildId => {GuildPid, Degraded}}},
    case Previous =/= Degraded andalso maps:get(ready, State, undefined) =:= undefined of
        true -> {noreply, dispatch(GuildId, Degraded, State1)};
        false -> {noreply, State1}
    end.

-spec dispatch_ready(map()) -> map().
dispatch_ready(State) ->
    Guilds = maps:get(guilds, State, #{}),
    maps:fold(
        fun(GuildId, Health, Acc) -> dispatch_degraded(GuildId, Health, Guilds, Acc) end,
        State,
        maps:get(guild_health, State, #{})
    ).

-spec dispatch_degraded(integer(), term(), map(), map()) -> map().
dispatch_degraded(GuildId, {Pid, true}, Guilds, State) ->
    case maps:get(GuildId, Guilds, undefined) of
        {Pid, _Ref} -> dispatch(GuildId, true, State);
        _ -> State
    end;
dispatch_degraded(_GuildId, _Health, _Guilds, State) ->
    State.

-spec forget(integer(), map()) -> map().
forget(GuildId, State) ->
    State#{guild_health => maps:remove(GuildId, maps:get(guild_health, State, #{}))}.

-spec dispatch(integer(), boolean(), map()) -> map().
dispatch(GuildId, Degraded, State) ->
    Payload = #{<<"guild_id">> => integer_to_binary(GuildId), <<"degraded">> => Degraded},
    {noreply, NewState} = session_dispatch:handle_dispatch(guild_health_update, Payload, State),
    NewState.
