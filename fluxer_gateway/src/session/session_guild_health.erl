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
        fun
            (GuildId, {Pid, true}, Acc) ->
                case maps:get(GuildId, Guilds, undefined) of
                    {Pid, _Ref} -> dispatch(GuildId, true, Acc);
                    _ -> Acc
                end;
            (_, _, Acc) ->
                Acc
        end,
        State,
        maps:get(guild_health, State, #{})
    ).

-spec forget(integer(), map()) -> map().
forget(GuildId, State) ->
    State#{guild_health => maps:remove(GuildId, maps:get(guild_health, State, #{}))}.

-spec dispatch(integer(), boolean(), map()) -> map().
dispatch(GuildId, Degraded, State) ->
    Payload = #{<<"guild_id">> => integer_to_binary(GuildId), <<"degraded">> => Degraded},
    {noreply, NewState} = session_dispatch:handle_dispatch(guild_health_update, Payload, State),
    NewState.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

health_state() ->
    #{
        id => <<"health">>,
        user_id => 100,
        seq => 0,
        buffer => [],
        guilds => #{42 => {self(), make_ref()}},
        socket_pid => undefined,
        channels => #{},
        relationships => #{},
        ready => undefined,
        suppress_presence_updates => false,
        pending_presences => [],
        presence_pid => undefined,
        ignored_events => #{},
        debounce_reactions => false
    }.

health_transitions_replay_without_reconnect_test() ->
    State = health_state(),
    {noreply, Degraded} = handle_update(42, self(), true, State),
    {noreply, Duplicate} = handle_update(42, self(), true, Degraded),
    ?assertEqual(1, maps:get(seq, Duplicate)),
    {noreply, Recovered} = handle_update(42, self(), false, Duplicate),
    ?assertEqual(maps:get(guilds, State), maps:get(guilds, Recovered)),
    Events = limited_deque:to_list(maps:get(buffer, Recovered)),
    ?assertEqual([guild_health_update, guild_health_update], [maps:get(event, E) || E <- Events]),
    ?assertEqual([true, false], [maps:get(<<"degraded">>, maps:get(data, E)) || E <- Events]).

health_waits_for_ready_test() ->
    State = (health_state())#{ready => #{}},
    {noreply, Degraded} = handle_update(42, self(), true, State),
    ?assertEqual(0, maps:get(seq, Degraded)),
    Ready = dispatch_ready(Degraded#{ready => undefined}),
    ?assertEqual(1, maps:get(seq, Ready)).

stale_guild_owner_is_ignored_test() ->
    Other = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    State = health_state(),
    ?assertEqual({noreply, State}, handle_update(42, Other, true, State)),
    Other ! stop.

healthy_reconnect_clears_previous_owner_state_test() ->
    State = (health_state())#{guild_health => #{42 => {old_owner, true}}},
    {noreply, Recovered} = handle_update(42, self(), false, State),
    ?assertEqual(1, maps:get(seq, Recovered)),
    ?assertEqual({self(), false}, maps:get(42, maps:get(guild_health, Recovered))).

same_owner_reconnect_resends_degraded_state_test() ->
    {noreply, Degraded} = handle_update(42, self(), true, health_state()),
    Reconnected = forget(42, Degraded),
    {noreply, Resynced} = handle_update(42, self(), true, Reconnected),
    ?assertEqual(2, maps:get(seq, Resynced)),
    Events = limited_deque:to_list(maps:get(buffer, Resynced)),
    ?assertEqual([true, true], [maps:get(<<"degraded">>, maps:get(data, E)) || E <- Events]).

-endif.
