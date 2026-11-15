%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_thread_flip).
-typing([eqwalizer]).

-export([
    start/2,
    converge/1,
    handle_result/3,
    hold/3,
    reinstall/3,
    user_flip/2,
    resend/2,
    export_handoff/1,
    restore_handoff/1,
    resend_count/0,
    init_counters/0
]).

-define(RESEND_JITTER_MS, 10000).
-define(RESEND_COUNT_KEY, {?MODULE, resend}).
-define(RETRY_BACKOFF_MS, [30000, 120000, 600000]).
-define(HOLD_CAP, 10000).

-spec start(non_neg_integer(), guild_state:guild_state()) -> guild_state:guild_state().
start(Version, #{thread_flip := {_Ref, InFlight}} = State) when InFlight >= Version ->
    State;
start(
    Version,
    #{id := GuildId, data := #{thread_gate := #{active := true, version := Adopted}}} = State
) when
    is_integer(GuildId), Adopted >= Version
->
    recompute_viewers(State);
start(Version, #{id := GuildId, data := #{thread_gate := #{active := true}}} = State) when
    is_integer(GuildId), not is_map_key(thread_flip, State)
->
    case channel_threads_config:guild_active(GuildId) of
        true -> recompute_viewers(State);
        false -> start_fetch(Version, State)
    end;
start(Version, #{id := GuildId} = State) when is_integer(GuildId) ->
    start_fetch(Version, State);
start(_Version, State) ->
    State.

-spec start_fetch(non_neg_integer(), guild_state:guild_state()) -> guild_state:guild_state().
start_fetch(Version, #{id := GuildId} = State) ->
    Self = self(),
    Ref = make_ref(),
    _ = spawn(fun() ->
        Self ! {thread_flip_result, Ref, guild_thread_load:fetch_flip_data(GuildId, Version)}
    end),
    State#{thread_flip => {Ref, Version}, thread_flip_held => {0, []}}.

-spec recompute_viewers(guild_state:guild_state()) -> guild_state:guild_state().
recompute_viewers(State) ->
    State1 = guild_thread_gate:recompute_session_viewers(State),
    schedule_resends(changed_sessions(State, State1), State1).

-spec converge(guild_state:guild_state()) -> guild_state:guild_state().
converge(#{id := GuildId} = State) when is_integer(GuildId) ->
    Loaded = channel_threads_config:enabled() orelse channel_threads_config:version() > 0,
    _ =
        case
            Loaded andalso
                channel_threads_config:guild_active(GuildId) =/= guild_thread_gate:active(State)
        of
            true -> self() ! {thread_gate_flip, channel_threads_config:version()};
            false -> ok
        end,
    State;
converge(State) ->
    State.

-spec handle_result(reference(), term(), guild_state:guild_state()) ->
    guild_state:guild_state().
handle_result(
    Ref, {ok, Data}, #{thread_flip := {Ref, Version}, thread_flip_held := overflow} = State
) when
    is_map(Data)
->
    logger:warning("guild_thread_flip_hold_overflow: guild_id=~p", [
        maps:get(id, State, undefined)
    ]),
    retry(Version, maps:without([thread_flip, thread_flip_held], State));
handle_result(Ref, {ok, Data}, #{thread_flip := {Ref, Version}} = State) when is_map(Data) ->
    {_Count, Held} = maps:get(thread_flip_held, State),
    State0 = maps:without([thread_flip, thread_flip_held], State),
    State1 = replay_held(Held, apply_flip(Data, Held, State0)),
    case maps:get(<<"config_version">>, Data, Version) of
        ApiVersion when is_integer(ApiVersion), ApiVersion < Version -> retry(Version, State1);
        _ -> clear_retry(State1)
    end;
handle_result(Ref, Error, #{thread_flip := {Ref, Version}} = State) ->
    logger:warning("guild_thread_flip_failed: guild_id=~p reason=~p", [
        maps:get(id, State, undefined), Error
    ]),
    retry(Version, maps:without([thread_flip, thread_flip_held], State));
handle_result(_Ref, _Result, State) ->
    State.

-spec retry(non_neg_integer(), guild_state:guild_state()) -> guild_state:guild_state().
retry(Version, #{id := GuildId} = State) when is_integer(GuildId) ->
    Agrees = channel_threads_config:guild_active(GuildId) =:= guild_thread_gate:active(State),
    case {Agrees, State} of
        {true, _} ->
            clear_retry(State);
        {false, #{thread_flip_retry := {_Timer, Previous}}} ->
            schedule_retry(Version, Previous + 1, State);
        {false, _} ->
            schedule_retry(Version, 1, State)
    end;
retry(_Version, State) ->
    State.

-spec schedule_retry(non_neg_integer(), pos_integer(), guild_state:guild_state()) ->
    guild_state:guild_state().
schedule_retry(Version, Attempt, State) ->
    State1 = clear_retry(State),
    Delay = lists:nth(min(Attempt, length(?RETRY_BACKOFF_MS)), ?RETRY_BACKOFF_MS),
    Timer = erlang:send_after(Delay, self(), {thread_gate_flip, Version}),
    State1#{thread_flip_retry => {Timer, Attempt}}.

-spec clear_retry(guild_state:guild_state()) -> guild_state:guild_state().
clear_retry(#{thread_flip_retry := {Timer, _Attempt}} = State) ->
    _ = erlang:cancel_timer(Timer),
    maps:remove(thread_flip_retry, State);
clear_retry(State) ->
    State.

-spec hold(atom(), map(), guild_state:guild_state()) -> guild_state:guild_state().
hold(Event, Data, #{thread_flip_held := {Count, Held}, id := GuildId} = State) when
    is_integer(GuildId)
->
    case held_entry(Event, Data) of
        undefined ->
            State;
        _Entry when Count >= ?HOLD_CAP ->
            State#{thread_flip_held => overflow};
        Entry ->
            Tagged = Entry#{<<"guild_id">> => integer_to_binary(GuildId)},
            State#{thread_flip_held => {Count + 1, [{Event, Tagged} | Held]}}
    end;
hold(_Event, _Data, State) ->
    State.

-spec held_entry(atom(), map()) -> map() | undefined.
held_entry(Event, Data) when
    Event =:= channel_create; Event =:= channel_update; Event =:= channel_delete
->
    case thread_only_channel(Data) of
        true -> Data;
        false -> undefined
    end;
held_entry(channel_update_bulk, #{<<"channels">> := Channels} = Data) when is_list(Channels) ->
    case [C || C <- Channels, thread_only_channel(C)] of
        [] -> undefined;
        Gated -> Data#{<<"channels">> => Gated}
    end;
held_entry(Event, Data) ->
    case guild_thread_dispatch:handles(Event) of
        true -> Data;
        false -> undefined
    end.

-spec thread_only_channel(term()) -> boolean().
thread_only_channel(Channel) when is_map(Channel) ->
    channel_types:is_thread_only(maps:get(<<"type">>, Channel, undefined));
thread_only_channel(_Channel) ->
    false.

-spec replay_held([{atom(), map()}], guild_state:guild_state()) -> guild_state:guild_state().
replay_held(Held, State) ->
    lists:foldl(
        fun({Event, Data}, Acc) -> replay_event(Event, Data, Acc) end,
        State,
        [
            Entry
         || {Event, _Data} = Entry <- lists:reverse(Held), guild_thread_dispatch:handles(Event)
        ]
    ).

-spec replay_event(atom(), map(), guild_state:guild_state()) -> guild_state:guild_state().
replay_event(Event, Data, State) ->
    try
        guild_state_threads:apply_event(Event, Data, State)
    catch
        Class:Reason ->
            logger:warning(
                "guild_thread_flip_replay_failed: event=~p guild_id=~p class=~p reason=~p", [
                    Event, maps:get(id, State, undefined), Class, Reason
                ]
            ),
            State
    end.

-spec apply_flip(map(), [{atom(), map()}], guild_state:guild_state()) ->
    guild_state:guild_state().
apply_flip(FlipData, Held, OldState) ->
    Gate = guild_thread_load:parse_gate(maps:get(<<"thread_gate">>, FlipData, undefined)),
    Extras0 = #{
        thread_gate => Gate,
        thread_tainted => type_conv:is_true(maps:get(<<"thread_tainted">>, FlipData, false))
    },
    {Extras, Held1} =
        case Gate of
            #{active := true} ->
                {Extras0#{thread_load => {ok, guild_thread_load:load_payload(FlipData)}}, Held};
            _ ->
                {Extras0, []}
        end,
    OldData = maps:get(data, OldState),
    Data = merge_channels(FlipData, Held1, OldData),
    ChannelsChanged = thread_only_ids(OldData) =/= thread_only_ids(Data),
    State = reinstall(Extras, OldState, OldState#{data => Data}, #{
        channels_changed => ChannelsChanged
    }),
    ok = guild_maintenance:maybe_put_permission_cache(State),
    State.

-spec merge_channels(map(), [{atom(), map()}], map()) -> map().
merge_channels(FlipData, Held, Data) ->
    case maps:get(<<"channels">>, FlipData, undefined) of
        Channels when is_list(Channels) ->
            Live = [
                C
             || C <- guild_data_index_channels:channel_list(Data), not thread_only_channel(C)
            ],
            Gated = lists:foldl(
                fun replay_channel/2,
                [C || C <- Channels, thread_only_channel(C)],
                lists:reverse(Held)
            ),
            guild_data_index:put_channels(Live ++ Gated, Data);
        _ ->
            Data
    end.

-spec thread_only_ids(map()) -> [integer() | undefined].
thread_only_ids(Data) ->
    lists:sort([
        snowflake_id:parse_optional(maps:get(<<"id">>, C, undefined))
     || C <- guild_data_index_channels:channel_list(Data), thread_only_channel(C)
    ]).

-spec replay_channel({atom(), map()}, [map()]) -> [map()].
replay_channel({channel_delete, Channel}, Channels) ->
    without_channel(Channel, Channels);
replay_channel({channel_update_bulk, #{<<"channels">> := Bulk}}, Channels) ->
    lists:foldl(fun(C, Acc) -> [C | without_channel(C, Acc)] end, Channels, Bulk);
replay_channel({Event, Channel}, Channels) when
    Event =:= channel_create; Event =:= channel_update
->
    [Channel | without_channel(Channel, Channels)];
replay_channel(_Held, Channels) ->
    Channels.

-spec without_channel(map(), [map()]) -> [map()].
without_channel(Channel, Channels) ->
    Id = snowflake_id:parse_optional(maps:get(<<"id">>, Channel, undefined)),
    [C || C <- Channels, snowflake_id:parse_optional(maps:get(<<"id">>, C, undefined)) =/= Id].

-spec reinstall(map(), guild_state:guild_state(), guild_state:guild_state()) ->
    guild_state:guild_state().
reinstall(Extras, OldState, NewState0) ->
    reinstall(Extras, OldState, NewState0, #{channels_changed => false}).

-spec reinstall(map(), guild_state:guild_state(), guild_state:guild_state(), #{
    channels_changed := boolean()
}) -> guild_state:guild_state().
reinstall(Extras0, OldState, NewState0, #{channels_changed := ChannelsChanged}) ->
    OldStore = guild_thread_gate:store(OldState),
    Extras = keep_store(Extras0, OldStore, OldState),
    NewState1 = release_store(Extras, OldStore, NewState0),
    NewState2 = guild_thread_load:install(Extras, NewState1),
    SameGate = gate_shape(OldState) =:= gate_shape(NewState2),
    case changed_sessions(OldState, NewState2) of
        [] when not ChannelsChanged andalso (OldStore =:= undefined orelse SameGate) ->
            NewState2;
        Changed ->
            NewState3 = guild_sessions:refresh_all_viewable_channels(NewState2),
            schedule_resends(Changed, NewState3)
    end.

-spec keep_store(map(), ets:table() | undefined, guild_state:guild_state()) -> map().
keep_store(#{thread_gate := #{active := true} = Gate} = Extras, Tab, OldState) when
    Tab =/= undefined, not is_map_key(thread_rows, Extras), not is_map_key(thread_load, Extras)
->
    case
        not guild_thread_load:loading(OldState) andalso
            maps:get(thread_gate, maps:get(data, OldState), undefined) =:= Gate
    of
        true -> Extras#{thread_kept => Tab};
        false -> Extras
    end;
keep_store(Extras, _Tab, _OldState) ->
    Extras.

-spec release_store(map(), ets:table() | undefined, guild_state:guild_state()) ->
    guild_state:guild_state().
release_store(#{thread_kept := _}, _OldStore, #{data := Data} = State) ->
    guild_thread_load:deactivate(State#{data => maps:remove(thread_store, Data)});
release_store(_Extras, OldStore, State) ->
    State1 = guild_thread_load:deactivate(State),
    ok = guild_thread_store:retire(OldStore),
    State1.

-spec gate_shape(guild_state:guild_state()) -> {boolean(), boolean()}.
gate_shape(State) ->
    {guild_thread_gate:active(State), guild_thread_gate:needs_variant(State)}.

-spec changed_sessions(guild_state:guild_state(), guild_state:guild_state()) -> [binary()].
changed_sessions(OldState, NewState) ->
    OldSessions = maps:get(sessions, OldState),
    lists:reverse([
        SessionId
     || SessionId := Session <- maps:iterator(maps:get(sessions, NewState)),
        guild_thread_gate:session_viewer(maps:get(SessionId, OldSessions, #{})) =/=
            guild_thread_gate:session_viewer(Session)
    ]).

-spec user_flip(binary(), guild_state:guild_state()) -> guild_state:guild_state().
user_flip(SessionId, State) ->
    Sessions = maps:get(sessions, State),
    maybe
        #{} = Session ?= maps:get(SessionId, Sessions, undefined),
        Viewer = guild_thread_gate:compute_viewer(
            guild_thread_gate:active(State),
            type_conv:is_true(maps:get(bot, Session, false)),
            type_conv:is_true(maps:get(thread_capable, Session, false)),
            maps:get(user_id, Session, undefined)
        ),
        false ?= Viewer =:= guild_thread_gate:session_viewer(Session),
        schedule_resends([SessionId], State#{
            sessions => Sessions#{
                SessionId => clear_subscriptions(Session#{thread_viewer => Viewer})
            }
        })
    else
        _ -> State
    end.

-spec clear_subscriptions(map()) -> map().
clear_subscriptions(#{thread_viewer := true} = Session) ->
    Session;
clear_subscriptions(Session) ->
    maps:without([thread_subscribed, thread_member_lists], Session).

-spec schedule_resends([binary()], guild_state:guild_state()) -> guild_state:guild_state().
schedule_resends(SessionIds, State) ->
    lists:foldl(fun schedule_resend/2, State, SessionIds).

-spec schedule_resend(binary(), guild_state:guild_state()) -> guild_state:guild_state().
schedule_resend(SessionId, State) ->
    Pending = maps:get(thread_resend_pending, State, #{}),
    case maps:is_key(SessionId, Pending) of
        true ->
            State;
        false ->
            _ = erlang:send_after(
                rand:uniform(?RESEND_JITTER_MS) - 1, self(), {thread_flip_resend, SessionId}
            ),
            State#{thread_resend_pending => Pending#{SessionId => true}}
    end.

-spec export_handoff(guild_state:guild_state()) -> map().
export_handoff(#{thread_resend_pending := Pending}) ->
    #{thread_handoff_resends => maps:keys(Pending)};
export_handoff(_State) ->
    #{}.

-spec restore_handoff(guild_state:guild_state()) -> guild_state:guild_state().
restore_handoff(#{thread_handoff_resends := SessionIds} = State) when is_list(SessionIds) ->
    schedule_resends(
        [Sid || Sid <- SessionIds, is_binary(Sid)], maps:remove(thread_handoff_resends, State)
    );
restore_handoff(State) ->
    maps:remove(thread_handoff_resends, State).

-spec resend(binary(), guild_state:guild_state()) -> guild_state:guild_state().
resend(SessionId, #{id := GuildId} = State0) when is_integer(GuildId) ->
    State = clear_pending_resend(SessionId, State0),
    case maps:get(SessionId, maps:get(sessions, State), undefined) of
        #{pending_connect := true} ->
            State;
        #{user_id := _} = Session ->
            resend_guild_create(Session, GuildId, State);
        _ ->
            State
    end;
resend(SessionId, State) ->
    clear_pending_resend(SessionId, State).

-spec clear_pending_resend(binary(), guild_state:guild_state()) -> guild_state:guild_state().
clear_pending_resend(SessionId, #{thread_resend_pending := Pending0} = State) ->
    case maps:remove(SessionId, Pending0) of
        Pending when map_size(Pending) =:= 0 -> maps:remove(thread_resend_pending, State);
        Pending -> State#{thread_resend_pending => Pending}
    end;
clear_pending_resend(_SessionId, State) ->
    State.

-spec resend_guild_create(map(), integer(), guild_state:guild_state()) ->
    guild_state:guild_state().
resend_guild_create(#{user_id := UserId} = Session, GuildId, State) ->
    case guild_availability:is_guild_unavailable_for_user(UserId, State) of
        true ->
            State;
        false ->
            count_resend(),
            ok = guild_availability:send_guild_create_to_session(
                Session, GuildId, State, [], resend_health()
            ),
            State
    end.

-spec resend_health() -> map().
resend_health() ->
    case guild_health:is_degraded(self()) of
        true -> #{<<"degraded">> => true};
        false -> #{}
    end.

-spec count_resend() -> ok.
count_resend() ->
    guild_thread_store:add_counter(?RESEND_COUNT_KEY, 1, 1).

-spec init_counters() -> ok.
init_counters() ->
    guild_thread_store:ensure_counter(?RESEND_COUNT_KEY, 1).

-spec resend_count() -> non_neg_integer().
resend_count() ->
    case persistent_term:get(?RESEND_COUNT_KEY, undefined) of
        undefined -> 0;
        Counter -> counters:get(Counter, 1)
    end.
