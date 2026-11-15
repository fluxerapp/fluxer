%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_visits).
-typing([eqwalizer]).

-export([
    parse_rpc/1,
    init_fields/1,
    member_guild_ids/1,
    member_guilds/1,
    connect_fields/2,
    accept_connect/4,
    handle_start/3,
    handle_end/2,
    handle_rejected/2,
    handle_promoted/4,
    handle_guild_join/2
]).

-type visits() :: #{session:guild_id() => #{operator := boolean()}}.

-spec parse_rpc(map()) -> map().
parse_rpc(Data) ->
    Operator = maps:from_keys(
        snowflake_id:parse_list(list_field(<<"operator_visit_guild_ids">>, Data)), true
    ),
    Visits = maps:from_list([
        {GuildId, #{operator => maps:is_key(GuildId, Operator)}}
     || GuildId <- snowflake_id:parse_list(list_field(<<"visit_guild_ids">>, Data))
    ]),
    #{
        visits => Visits,
        adult_content_allowed => type_conv:is_true(
            maps:get(<<"adult_content_allowed">>, Data, false)
        ),
        account_tier => visitor_request:tier(maps:get(<<"account_tier">>, Data, undefined)),
        ip_key => visitor_request:ip_key(maps:get(<<"ip_key">>, Data, undefined))
    }.

-spec init_fields(map()) -> map().
init_fields(D) ->
    #{
        visits => normalize_visits(maps:get(visits, D, #{})),
        guild_visits_capable => type_conv:is_true(
            maps:get(guild_visits_capable, D, false)
        ),
        adult_content_allowed => type_conv:is_true(
            maps:get(adult_content_allowed, D, false)
        ),
        account_tier => visitor_request:tier(maps:get(account_tier, D, undefined)),
        ip_key => visitor_request:ip_key(maps:get(ip_key, D, undefined))
    }.

-spec visits(session:session_state()) -> map().
visits(#{visits := Visits}) when is_map(Visits) ->
    Visits.

-spec normalize_visits(term()) -> visits().
normalize_visits(Visits) when is_map(Visits) ->
    maps:fold(
        fun
            (GuildId, #{operator := Operator}, Acc) when is_integer(GuildId), GuildId > 0 ->
                Acc#{GuildId => #{operator => type_conv:is_true(Operator)}};
            (_GuildId, _Visit, Acc) ->
                Acc
        end,
        #{},
        Visits
    );
normalize_visits(_) ->
    #{}.

-spec visiting(session:guild_id(), session:session_state()) -> boolean().
visiting(GuildId, State) ->
    maps:is_key(GuildId, visits(State)).

-spec member_guild_ids(session:session_state()) -> [session:guild_id()].
member_guild_ids(State) ->
    Visits = visits(State),
    [
        GuildId
     || GuildId <- maps:keys(maps:get(guilds, State, #{})), not maps:is_key(GuildId, Visits)
    ].

-spec member_guilds(map()) -> map().
member_guilds(SessionState) ->
    maps:without(maps:keys(visits(SessionState)), maps:get(guilds, SessionState, #{})).

-spec connect_fields(session:guild_id(), session:session_state()) -> map().
connect_fields(GuildId, State) ->
    case maps:get(GuildId, visits(State), undefined) of
        #{operator := Operator} ->
            #{
                visitor => true,
                operator => Operator,
                adult_content_allowed => type_conv:is_true(
                    maps:get(adult_content_allowed, State, false)
                ),
                visitor_tier => maps:get(account_tier, State, <<"unclaimed">>),
                ip_key => maps:get(ip_key, State, undefined)
            };
        _ ->
            #{}
    end.

-spec accept_connect(session:guild_id(), pid(), map(), session:session_state()) ->
    {accept, session:session_state()} | {drop, session:session_state()}.
accept_connect(GuildId, GuildPid, GuildState, State) ->
    case
        {
            type_conv:is_true(maps:get(<<"visiting">>, GuildState, false)),
            visiting(GuildId, State)
        }
    of
        {true, true} ->
            {accept, State};
        {true, false} ->
            _ = shard_utils:safe_cast(GuildPid, {visitor_leave, maps:get(id, State)}),
            Guilds = maps:get(guilds, State, #{}),
            {drop, State#{guilds => maps:remove(GuildId, Guilds)}};
        {false, true} ->
            {accept, drop_visit(GuildId, State)};
        {false, false} ->
            {accept, State}
    end.

-spec handle_start(session:guild_id(), boolean(), session:session_state()) ->
    {noreply, session:session_state()}.
handle_start(GuildId, Operator, State) ->
    Guilds = maps:get(guilds, State, #{}),
    Capable =
        type_conv:is_true(maps:get(guild_visits_capable, State, false)) andalso
            not type_conv:is_true(maps:get(bot, State, false)),
    case {Capable, maps:is_key(GuildId, Guilds), visiting(GuildId, State)} of
        {false, _, _} ->
            {noreply, State};
        {true, true, false} ->
            {noreply, State};
        {true, true, true} ->
            {noreply, put_visit(GuildId, Operator, State)};
        {true, false, _} ->
            self() ! {guild_connect, GuildId, 0},
            {noreply, (put_visit(GuildId, Operator, State))#{
                guilds => Guilds#{GuildId => undefined}
            }}
    end.

-spec handle_end(session:guild_id(), session:session_state()) ->
    {noreply, session:session_state()} | {stop, normal, session:session_state()}.
handle_end(GuildId, State) ->
    Visit =
        case visiting(GuildId, State) of
            false -> none;
            true -> {visit, maps:get(GuildId, maps:get(guilds, State, #{}), undefined)}
        end,
    case Visit of
        {visit, {Pid, _Ref}} when is_pid(Pid) ->
            _ = shard_utils:safe_cast(Pid, {visitor_leave, maps:get(id, State)}),
            remove_visited_guild(GuildId, State);
        {visit, _} ->
            remove_visited_guild(GuildId, State);
        none ->
            {noreply, State}
    end.

-spec handle_rejected(session:guild_id(), session:session_state()) ->
    {noreply, session:session_state()} | {stop, normal, session:session_state()}.
handle_rejected(GuildId, State) ->
    Connection =
        case visiting(GuildId, State) of
            true ->
                visit;
            false ->
                Guilds = maps:get(guilds, State, #{}),
                {member, Guilds, maps:get(GuildId, Guilds, undefined)}
        end,
    case Connection of
        visit ->
            remove_visited_guild(GuildId, State);
        {member, MemberGuilds, {_Pid, Ref}} when is_reference(Ref) ->
            demonitor(Ref, [flush]),
            self() ! {guild_connect, GuildId, 0},
            {noreply, State#{guilds => MemberGuilds#{GuildId => undefined}}};
        {member, _, _} ->
            {noreply, State}
    end.

-spec handle_promoted(session:guild_id(), pid(), map(), session:session_state()) ->
    {noreply, session:session_state()} | {stop, normal, session:session_state()}.
handle_promoted(GuildId, GuildPid, GuildState, State0) ->
    State = drop_visit(GuildId, State0),
    case maps:get(GuildId, maps:get(guilds, State, #{}), undefined) of
        {GuildPid, _Ref} ->
            session_ready:process_guild_state(GuildState, forget_collected(GuildId, State));
        _ ->
            {noreply, State}
    end.

-spec handle_guild_join(session:guild_id(), session:session_state()) -> session:session_state().
handle_guild_join(GuildId, State) ->
    drop_visit(GuildId, State).

-spec remove_visited_guild(session:guild_id(), session:session_state()) ->
    {noreply, session:session_state()} | {stop, normal, session:session_state()}.
remove_visited_guild(GuildId, State0) ->
    State = drop_visit(GuildId, State0),
    Guilds = maps:get(guilds, State, #{}),
    case maps:get(GuildId, Guilds, undefined) of
        {_Pid, Ref} when is_reference(Ref) -> demonitor(Ref, [flush]);
        _ -> ok
    end,
    DeleteData = #{<<"id">> => integer_to_binary(GuildId)},
    State1 = forget_collected(GuildId, State#{guilds => maps:remove(GuildId, Guilds)}),
    {noreply, State2} = session_dispatch:handle_dispatch(guild_delete, DeleteData, State1),
    State3 = session_guild_health:forget(GuildId, State2),
    session_ready:check_readiness(
        session_guilds:remove_guild_subscription_state(GuildId, State3)
    ).

-spec forget_collected(session:guild_id(), session:session_state()) -> session:session_state().
forget_collected(GuildId, State) ->
    IdBin = integer_to_binary(GuildId),
    case maps:get(collected_guild_states, State, undefined) of
        Collected when is_list(Collected) ->
            State#{
                collected_guild_states => [
                    G
                 || G <- Collected,
                    not (is_map(G) andalso maps:get(<<"id">>, G, undefined) =:= IdBin)
                ]
            };
        _ ->
            State
    end.

-spec put_visit(session:guild_id(), boolean(), session:session_state()) ->
    session:session_state().
put_visit(GuildId, Operator, State) ->
    State#{
        visits => (visits(State))#{GuildId => #{operator => type_conv:is_true(Operator)}}
    }.

-spec drop_visit(session:guild_id(), session:session_state()) -> session:session_state().
drop_visit(GuildId, State) ->
    State#{visits => maps:remove(GuildId, visits(State))}.

-spec list_field(binary(), map()) -> list().
list_field(Key, Data) ->
    case maps:get(Key, Data, []) of
        List when is_list(List) -> List;
        _ -> []
    end.
