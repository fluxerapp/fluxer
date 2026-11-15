%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_push_engagement).
-typing([eqwalizer]).

-export([snapshot/2, guild_pid/0, held_sessions/1, grace_route/2, hold/2, send/2]).
-export_type([grace_sessions/0, grace_hold/0, held_push/0]).

-define(GRACE_RECHECK_KEY, guild_push_offline_grace_recheck_ms).
-define(DEFAULT_GRACE_RECHECK_MS, 7000).
-define(GRACE_HOLD_CALL_TIMEOUT_MS, 5000).

-type user_id() :: integer().
-type grace_sessions() :: #{user_id() => [{binary(), pid()}]}.
-type grace_hold() :: {grace_sessions(), integer()} | none.
-type held_push() :: {[user_id()], grace_hold(), term(), pid() | undefined}.

-spec snapshot(map(), map()) ->
    {#{integer() => boolean()}, grace_hold()}.
snapshot(Sessions, State) ->
    case presence_eligibility_enabled() of
        true -> build_push_presence_eligibility(Sessions, State);
        false -> build_legacy_session_eligibility(Sessions)
    end.

-spec guild_pid() -> pid() | undefined.
guild_pid() ->
    case presence_eligibility_enabled() of
        true -> self();
        false -> undefined
    end.

-spec presence_eligibility_enabled() -> boolean().
presence_eligibility_enabled() ->
    type_conv:is_true(application:get_env(fluxer_gateway, push_presence_eligibility, true)).

-spec build_legacy_session_eligibility(map()) -> {#{user_id() => boolean()}, grace_hold()}.
build_legacy_session_eligibility(Sessions) ->
    Eligibility = build_push_session_eligibility(Sessions),
    {Eligibility, grace_hold(suppressed_sessions(Sessions, Eligibility))}.

-spec build_push_presence_eligibility(map(), map()) ->
    {#{user_id() => boolean()}, grace_hold()}.
build_push_presence_eligibility(Sessions, State) ->
    Engaged = engaged_sessions(Sessions, maps:get(member_presence, State, undefined)),
    {engagement_eligibility(Sessions, Engaged), grace_hold(held_engaged_sessions(Engaged))}.

-spec engaged_sessions(map(), term()) -> grace_sessions().
engaged_sessions(Sessions, Presences) ->
    maps:fold(
        fun(Sid, Session, Acc) -> maybe_add_engaged_session(Sid, Session, Presences, Acc) end,
        #{},
        Sessions
    ).

-spec maybe_add_engaged_session(term(), term(), term(), grace_sessions()) -> grace_sessions().
maybe_add_engaged_session(Sid, #{user_id := UserId} = Session, Presences, Acc) when
    is_integer(UserId)
->
    case session_engaged(Session, UserId, Presences) of
        true -> Acc#{UserId => session_pids(Sid, Session) ++ maps:get(UserId, Acc, [])};
        false -> Acc
    end;
maybe_add_engaged_session(_Sid, _Session, _Presences, Acc) ->
    Acc.

-spec session_engaged(map(), user_id(), term()) -> boolean().
session_engaged(#{push_hold := Hold}, _UserId, _Presences) ->
    type_conv:is_true(Hold);
session_engaged(_Session, UserId, Presences) ->
    actively_engaged(UserId, Presences).

-spec session_pids(term(), map()) -> [{binary(), pid()}].
session_pids(Sid, #{pid := Pid}) when is_binary(Sid), is_pid(Pid) -> [{Sid, Pid}];
session_pids(_Sid, _Session) -> [].

-spec engagement_eligibility(map(), grace_sessions()) -> #{user_id() => boolean()}.
engagement_eligibility(Sessions, Engaged) ->
    maps:fold(
        fun
            (_Sid, #{user_id := UserId}, Acc) when is_integer(UserId) ->
                Acc#{UserId => not maps:is_key(UserId, Engaged)};
            (_Sid, _Session, Acc) ->
                Acc
        end,
        #{},
        Sessions
    ).

-spec held_engaged_sessions(grace_sessions()) -> grace_sessions().
held_engaged_sessions(Engaged) ->
    maps:filter(fun(_UserId, Pids) -> Pids =/= [] end, Engaged).

-spec actively_engaged(user_id(), term()) -> boolean().
actively_engaged(_UserId, undefined) ->
    false;
actively_engaged(UserId, Presences) ->
    case lookup_presence_safe(UserId, Presences) of
        undefined -> false;
        Presence -> presence_is_active(Presence)
    end.

-spec lookup_presence_safe(user_id(), term()) -> map() | undefined.
lookup_presence_safe(UserId, Presences) when
    is_map(Presences); is_reference(Presences); is_atom(Presences)
->
    try guild_state_member:lookup_presence(Presences, UserId) of
        Presence -> Presence
    catch
        error:badarg -> undefined
    end;
lookup_presence_safe(_UserId, _Presences) ->
    undefined.

-spec presence_is_active(map()) -> boolean().
presence_is_active(Presence) ->
    Status = maps:get(<<"status">>, Presence, <<"offline">>),
    Afk = type_conv:is_true(maps:get(<<"afk">>, Presence, false)),
    Mobile = type_conv:is_true(maps:get(<<"mobile">>, Presence, false)),
    is_engaged_status(Status) andalso not Afk andalso not Mobile.

-spec is_engaged_status(term()) -> boolean().
is_engaged_status(<<"online">>) -> true;
is_engaged_status(<<"dnd">>) -> true;
is_engaged_status(_Status) -> false.

-spec build_push_session_eligibility(map()) -> #{user_id() => boolean()}.
build_push_session_eligibility(Sessions) ->
    maps:fold(
        fun(_Sid, Session, Acc) ->
            accumulate_session_eligibility(Session, Acc)
        end,
        #{},
        Sessions
    ).

-spec accumulate_session_eligibility(map(), #{user_id() => boolean()}) ->
    #{user_id() => boolean()}.
accumulate_session_eligibility(Session, Acc) ->
    case maps:get(user_id, Session, undefined) of
        UserId when is_integer(UserId) ->
            Acc#{
                UserId =>
                    maps:get(UserId, Acc, true) andalso maps:get(afk, Session, false)
            };
        _ ->
            Acc
    end.

-spec grace_route(integer(), grace_sessions()) -> held_user_ids | skip.
grace_route(UserId, HeldSessions) ->
    case maps:is_key(UserId, HeldSessions) of
        true -> held_user_ids;
        false -> skip
    end.

-spec send([integer()], map()) -> ok.
send([], _Params) ->
    ok;
send(UserIds, Params) ->
    push:handle_message_create(Params#{user_ids => UserIds}).

-spec hold(held_push(), map()) -> ok.
hold(
    {[_ | _] = UserIds, {Sessions, RecheckAt}, Presences, GuildPid}, Params
) ->
    HeldParams = Params#{
        user_roles => maps:with(UserIds, maps:get(user_roles, Params)),
        connected_users => maps:with(UserIds, maps:get(connected_users, Params))
    },
    HeldSessions = maps:with(UserIds, Sessions),
    _ = spawn(fun() ->
        deliver_after_grace(HeldParams, HeldSessions, RecheckAt, {Presences, GuildPid})
    end),
    ok;
hold(_Held, _Params) ->
    ok.

-spec deliver_after_grace(map(), grace_sessions(), integer(), {term(), pid() | undefined}) ->
    ok.
deliver_after_grace(Params, Sessions, RecheckAt, {Presences, GuildPid}) ->
    _ = erlang:process_flag(priority, low),
    HeldUserIds = maps:keys(Sessions),
    AlreadyOffline = users_left_offline(HeldUserIds, Presences),
    LiveSessions = maps:values(
        await_grace_recheck(monitor_grace_sessions(Sessions), RecheckAt)
    ),
    Holding = still_holding(LiveSessions, released_push_holds(GuildPid, LiveSessions)),
    send(grace_recipients(HeldUserIds, Holding, Presences, AlreadyOffline), Params).

-spec still_holding([{user_id(), binary()}], [binary()]) -> [user_id()].
still_holding(LiveSessions, Released) ->
    ReleasedSet = maps:from_keys(Released, true),
    [UserId || {UserId, Sid} <- LiveSessions, not maps:is_key(Sid, ReleasedSet)].

-spec released_push_holds(pid() | undefined, [{user_id(), binary()}]) -> [binary()].
released_push_holds(GuildPid, [_ | _] = LiveSessions) when is_pid(GuildPid) ->
    SessionIds = [Sid || {_UserId, Sid} <- LiveSessions],
    try
        gen_server:call(
            GuildPid, {released_push_holds, SessionIds}, ?GRACE_HOLD_CALL_TIMEOUT_MS
        )
    of
        Released when is_list(Released) -> Released;
        _ -> []
    catch
        exit:Reason ->
            logger:warning(
                "guild_push_grace_recheck_failed: guild_pid=~p reason=~p", [GuildPid, Reason]
            ),
            []
    end;
released_push_holds(_GuildPid, _LiveSessions) ->
    [].

-spec grace_recipients([user_id()], [user_id()], term(), [user_id()]) -> [user_id()].
grace_recipients(HeldUserIds, LiveUserIds, Presences, AlreadyOffline) ->
    Live = lists:usort(LiveUserIds),
    Gone = lists:usort(HeldUserIds) -- Live,
    lists:usort(Gone ++ (users_left_offline(Live, Presences) -- AlreadyOffline)).

-spec monitor_grace_sessions(grace_sessions()) -> #{reference() => {user_id(), binary()}}.
monitor_grace_sessions(Sessions) ->
    maps:fold(fun monitor_user_sessions/3, #{}, Sessions).

-spec monitor_user_sessions(
    user_id(), [{binary(), pid()}], #{reference() => {user_id(), binary()}}
) ->
    #{reference() => {user_id(), binary()}}.
monitor_user_sessions(UserId, Held, Monitors) ->
    lists:foldl(
        fun({Sid, Pid}, Acc) -> Acc#{erlang:monitor(process, Pid) => {UserId, Sid}} end,
        Monitors,
        Held
    ).

-spec await_grace_recheck(#{reference() => {user_id(), binary()}}, integer()) ->
    #{reference() => {user_id(), binary()}}.
await_grace_recheck(Monitors, RecheckAt) ->
    Remaining = max(0, RecheckAt - gateway_clock:monotonic_ms()),
    receive
        {'DOWN', Ref, process, _Pid, _Reason} when is_map_key(Ref, Monitors) ->
            await_grace_recheck(maps:remove(Ref, Monitors), RecheckAt)
    after Remaining ->
        Monitors
    end.

-spec users_left_offline([user_id()], term()) -> [user_id()].
users_left_offline(UserIds, Presences) ->
    [UserId || UserId <- UserIds, presence_is_offline(UserId, Presences)].

-spec presence_is_offline(user_id(), term()) -> boolean().
presence_is_offline(UserId, Presences) ->
    case lookup_presence_safe(UserId, Presences) of
        undefined -> false;
        Presence -> maps:get(<<"status">>, Presence, <<"offline">>) =:= <<"offline">>
    end.

-spec grace_hold(grace_sessions()) -> grace_hold().
grace_hold(Held) when map_size(Held) =:= 0 ->
    none;
grace_hold(Held) ->
    {Held, gateway_clock:monotonic_ms() + grace_recheck_ms()}.

-spec held_sessions(grace_hold()) -> grace_sessions().
held_sessions(none) -> #{};
held_sessions({Held, _RecheckAt}) -> Held.

-spec suppressed_sessions(map(), #{user_id() => boolean()}) -> grace_sessions().
suppressed_sessions(Sessions, SessionEligibility) ->
    maps:fold(
        fun(Sid, Session, Acc) -> maybe_hold_session(Sid, Session, SessionEligibility, Acc) end,
        #{},
        Sessions
    ).

-spec maybe_hold_session(term(), term(), #{user_id() => boolean()}, grace_sessions()) ->
    grace_sessions().
maybe_hold_session(Sid, Session, SessionEligibility, Acc) when is_map(Session) ->
    hold_suppressed_session(
        maps:get(user_id, Session, undefined),
        session_pids(Sid, Session),
        SessionEligibility,
        Acc
    );
maybe_hold_session(_Sid, _Session, _SessionEligibility, Acc) ->
    Acc.

-spec hold_suppressed_session(
    term(), [{binary(), pid()}], #{user_id() => boolean()}, grace_sessions()
) ->
    grace_sessions().
hold_suppressed_session(UserId, [_ | _] = Held, SessionEligibility, Acc) when
    is_integer(UserId)
->
    case maps:get(UserId, SessionEligibility, true) of
        false ->
            Acc#{UserId => Held ++ maps:get(UserId, Acc, [])};
        true ->
            Acc
    end;
hold_suppressed_session(_UserId, _Held, _SessionEligibility, Acc) ->
    Acc.

-spec grace_recheck_ms() -> pos_integer().
grace_recheck_ms() ->
    case application:get_env(fluxer_gateway, ?GRACE_RECHECK_KEY, undefined) of
        Value when is_integer(Value), Value > 0 -> Value;
        _ -> ?DEFAULT_GRACE_RECHECK_MS
    end.
