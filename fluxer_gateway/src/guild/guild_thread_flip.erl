%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_thread_flip).
-typing([eqwalizer]).

-export([
    start/2,
    converge/1,
    handle_result/3,
    hold/3,
    apply_flip/2,
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

-type guild_state() :: map().

-spec start(non_neg_integer(), guild_state()) -> guild_state().
start(Version, #{thread_flip := {_Ref, InFlight}} = State) when InFlight >= Version ->
    State;
start(Version, #{id := GuildId} = State) when is_integer(GuildId) ->
    case
        adopted_version(State) >= Version orelse
            (not is_map_key(thread_flip, State) andalso settled(GuildId, State))
    of
        true ->
            recompute_viewers(State);
        false ->
            Self = self(),
            Ref = make_ref(),
            _ = spawn(fun() ->
                Self !
                    {thread_flip_result, Ref,
                        guild_thread_load:fetch_flip_data(GuildId, Version)}
            end),
            State#{thread_flip => {Ref, Version}, thread_flip_held => {0, []}}
    end;
start(_Version, State) ->
    State.

-spec settled(integer(), guild_state()) -> boolean().
settled(GuildId, State) ->
    guild_thread_gate:active(State) andalso channel_threads_config:guild_active(GuildId).

-spec recompute_viewers(guild_state()) -> guild_state().
recompute_viewers(State) ->
    State1 = guild_thread_gate:recompute_session_viewers(State),
    schedule_resends(changed_sessions(State, State1), State1).

-spec converge(guild_state()) -> guild_state().
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

-spec adopted_version(guild_state()) -> integer().
adopted_version(State) ->
    case maps:get(data, State, #{}) of
        #{thread_gate := #{active := true, version := Version}} -> Version;
        _ -> -1
    end.

-spec handle_result(reference(), term(), guild_state()) -> guild_state().
handle_result(Ref, {ok, Data}, #{thread_flip := {Ref, Version}} = State) when is_map(Data) ->
    case maps:get(thread_flip_held, State, {0, []}) of
        {_Count, Held} ->
            State0 = maps:without([thread_flip, thread_flip_held], State),
            State1 = replay_held(Held, apply_flip(Data, Held, State0)),
            case maps:get(<<"config_version">>, Data, Version) of
                ApiVersion when is_integer(ApiVersion), ApiVersion < Version ->
                    retry(Version, State1);
                _ ->
                    clear_retry(State1)
            end;
        overflow ->
            logger:warning("guild_thread_flip_hold_overflow: guild_id=~p", [
                maps:get(id, State, undefined)
            ]),
            retry(Version, maps:without([thread_flip, thread_flip_held], State))
    end;
handle_result(Ref, Error, #{thread_flip := {Ref, Version}} = State) ->
    logger:warning("guild_thread_flip_failed: guild_id=~p reason=~p", [
        maps:get(id, State, undefined), Error
    ]),
    retry(Version, maps:without([thread_flip, thread_flip_held], State));
handle_result(_Ref, _Result, State) ->
    State.

-spec retry(non_neg_integer(), guild_state()) -> guild_state().
retry(Version, #{id := GuildId} = State) when is_integer(GuildId) ->
    case channel_threads_config:guild_active(GuildId) =:= guild_thread_gate:active(State) of
        true ->
            clear_retry(State);
        false ->
            Attempt =
                case State of
                    #{thread_flip_retry := {_Timer, Previous}} -> Previous + 1;
                    _ -> 1
                end,
            State1 = clear_retry(State),
            Delay = lists:nth(min(Attempt, length(?RETRY_BACKOFF_MS)), ?RETRY_BACKOFF_MS),
            Timer = erlang:send_after(Delay, self(), {thread_gate_flip, Version}),
            State1#{thread_flip_retry => {Timer, Attempt}}
    end;
retry(_Version, State) ->
    State.

-spec clear_retry(guild_state()) -> guild_state().
clear_retry(#{thread_flip_retry := {Timer, _Attempt}} = State) ->
    _ = erlang:cancel_timer(Timer),
    maps:remove(thread_flip_retry, State);
clear_retry(State) ->
    State.

-spec hold(atom(), map(), guild_state()) -> guild_state().
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
    guild_thread_gate:is_thread_only_type(maps:get(<<"type">>, Channel, undefined));
thread_only_channel(_Channel) ->
    false.

-spec replay_held([{atom(), map()}], guild_state()) -> guild_state().
replay_held(Held, State) ->
    lists:foldl(
        fun({Event, Data}, Acc) ->
            case guild_thread_dispatch:handles(Event) of
                true -> replay_event(Event, Data, Acc);
                false -> Acc
            end
        end,
        State,
        lists:reverse(Held)
    ).

-spec replay_event(atom(), map(), guild_state()) -> guild_state().
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

-spec apply_flip(map(), guild_state()) -> guild_state().
apply_flip(FlipData, OldState) ->
    apply_flip(FlipData, [], OldState).

-spec apply_flip(map(), [{atom(), map()}], guild_state()) -> guild_state().
apply_flip(FlipData, Held, OldState) ->
    Gate = guild_thread_load:parse_gate(maps:get(<<"thread_gate">>, FlipData, undefined)),
    Extras0 = #{
        thread_gate => Gate,
        thread_tainted => maps:get(<<"thread_tainted">>, FlipData, false) =:= true
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
    State = reinstall(Extras, OldState, OldState#{data => Data}, ChannelsChanged),
    ok = guild_maintenance:maybe_put_permission_cache(State),
    State.

-spec merge_channels(map(), [{atom(), map()}], map()) -> map().
merge_channels(FlipData, Held, Data) ->
    case maps:get(<<"channels">>, FlipData, undefined) of
        Channels when is_list(Channels) ->
            Live = [C || C <- guild_data_index:channel_list(Data), not thread_only_channel(C)],
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
     || C <- guild_data_index:channel_list(Data), thread_only_channel(C)
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

-spec reinstall(map(), guild_state(), guild_state()) -> guild_state().
reinstall(Extras, OldState, NewState0) ->
    reinstall(Extras, OldState, NewState0, false).

-spec reinstall(map(), guild_state(), guild_state(), boolean()) -> guild_state().
reinstall(Extras0, OldState, NewState0, ChannelsChanged) ->
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

-spec keep_store(map(), ets:table() | undefined, guild_state()) -> map().
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

-spec release_store(map(), ets:table() | undefined, guild_state()) -> guild_state().
release_store(#{thread_kept := _}, _OldStore, #{data := Data} = State) ->
    guild_thread_load:deactivate(State#{data => maps:remove(thread_store, Data)});
release_store(_Extras, OldStore, State) ->
    State1 = guild_thread_load:deactivate(State),
    ok = guild_thread_store:retire(OldStore),
    State1.

-spec gate_shape(guild_state()) -> {boolean(), boolean()}.
gate_shape(State) ->
    {guild_thread_gate:active(State), guild_thread_gate:needs_variant(State)}.

-spec changed_sessions(guild_state(), guild_state()) -> [binary()].
changed_sessions(OldState, NewState) ->
    OldSessions = maps:get(sessions, OldState, #{}),
    maps:fold(
        fun(SessionId, Session, Acc) ->
            Old = maps:get(SessionId, OldSessions, #{}),
            case
                guild_thread_gate:session_viewer(Old) =/=
                    guild_thread_gate:session_viewer(Session)
            of
                true -> [SessionId | Acc];
                false -> Acc
            end
        end,
        [],
        maps:get(sessions, NewState, #{})
    ).

-spec user_flip(binary(), guild_state()) -> guild_state().
user_flip(SessionId, State) ->
    Sessions = maps:get(sessions, State, #{}),
    case maps:get(SessionId, Sessions, undefined) of
        Session when is_map(Session) ->
            Viewer = guild_thread_gate:compute_viewer(
                guild_thread_gate:active(State),
                maps:get(bot, Session, false) =:= true,
                maps:get(thread_capable, Session, false) =:= true,
                maps:get(user_id, Session, undefined)
            ),
            case Viewer =:= guild_thread_gate:session_viewer(Session) of
                true ->
                    State;
                false ->
                    schedule_resends([SessionId], State#{
                        sessions => Sessions#{
                            SessionId => clear_subscriptions(Session#{thread_viewer => Viewer})
                        }
                    })
            end;
        _ ->
            State
    end.

-spec clear_subscriptions(map()) -> map().
clear_subscriptions(#{thread_viewer := true} = Session) ->
    Session;
clear_subscriptions(Session) ->
    maps:without([thread_subscribed, thread_member_lists], Session).

-spec schedule_resends([binary()], guild_state()) -> guild_state().
schedule_resends(SessionIds, State) ->
    lists:foldl(fun schedule_resend/2, State, SessionIds).

-spec schedule_resend(binary(), guild_state()) -> guild_state().
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

-spec export_handoff(guild_state()) -> map().
export_handoff(#{thread_resend_pending := Pending}) ->
    #{thread_handoff_resends => maps:keys(Pending)};
export_handoff(_State) ->
    #{}.

-spec restore_handoff(guild_state()) -> guild_state().
restore_handoff(#{thread_handoff_resends := SessionIds} = State) when is_list(SessionIds) ->
    schedule_resends(
        [Sid || Sid <- SessionIds, is_binary(Sid)], maps:remove(thread_handoff_resends, State)
    );
restore_handoff(State) ->
    maps:remove(thread_handoff_resends, State).

-spec resend(binary(), guild_state()) -> guild_state().
resend(SessionId, #{id := GuildId} = State0) when is_integer(GuildId) ->
    State = clear_pending_resend(SessionId, State0),
    case maps:get(SessionId, maps:get(sessions, State, #{}), undefined) of
        #{pending_connect := true} ->
            State;
        #{user_id := UserId} = Session ->
            case guild_availability:is_guild_unavailable_for_user(UserId, State) of
                true -> State;
                false -> resend_guild_create(Session, GuildId, State)
            end;
        _ ->
            State
    end;
resend(SessionId, State) ->
    clear_pending_resend(SessionId, State).

-spec clear_pending_resend(binary(), guild_state()) -> guild_state().
clear_pending_resend(SessionId, #{thread_resend_pending := Pending0} = State) ->
    case maps:remove(SessionId, Pending0) of
        Pending when map_size(Pending) =:= 0 -> maps:remove(thread_resend_pending, State);
        Pending -> State#{thread_resend_pending => Pending}
    end;
clear_pending_resend(_SessionId, State) ->
    State.

-spec resend_guild_create(map(), integer(), guild_state()) -> guild_state().
resend_guild_create(Session, GuildId, State) ->
    count_resend(),
    ok = guild_availability:send_guild_create_to_session(
        Session, GuildId, State, [], resend_health()
    ),
    State.

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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

start_ignores_a_repeat_flip_while_one_is_in_flight_test() ->
    Parent = self(),
    meck:new(guild_thread_load, [passthrough, no_link]),
    meck:expect(guild_thread_load, fetch_flip_data, fun(_GuildId, Version) ->
        Parent ! {fetch_started, Version},
        timer:sleep(1000),
        {error, timeout}
    end),
    try
        State0 = start(3, #{id => 5}),
        State1 = start(3, State0),
        State2 = start(2, State1),
        ?assertEqual(maps:get(thread_flip, State0), maps:get(thread_flip, State2)),
        #{thread_flip := {Superseded, 3}} = State2,
        State3 = start(4, State2),
        ?assertMatch(#{thread_flip := {_, 4}}, State3),
        ?assertEqual([3, 4], collect_fetches([])),
        ?assertEqual(State3, handle_result(Superseded, {ok, #{}}, State3))
    after
        meck:unload(guild_thread_load)
    end.

settled_config_supersedes_an_in_flight_flip_test() ->
    Parent = self(),
    meck:new(guild_thread_load, [passthrough, no_link]),
    meck:expect(guild_thread_load, fetch_flip_data, fun(_GuildId, Version) ->
        Parent ! {fetch_started, Version},
        {error, timeout}
    end),
    meck:new(channel_threads_config, [passthrough, no_link]),
    meck:expect(channel_threads_config, guild_active, fun(_GuildId) -> true end),
    try
        Stale = make_ref(),
        State0 = #{
            id => 5,
            data => #{thread_gate => #{active => true, version => 4}},
            sessions => #{},
            thread_flip => {Stale, 5},
            thread_flip_held => {0, []}
        },
        State1 = start(6, State0),
        ?assertMatch(#{thread_flip := {Ref, 6}} when Ref =/= Stale, State1),
        ?assertEqual([6], collect_fetches([])),
        Inactive = #{<<"thread_gate">> => #{<<"active">> => false, <<"config_version">> => 5}},
        ?assertEqual(State1, handle_result(Stale, {ok, Inactive}, State1)),
        receive
            {thread_flip_result, _, _} -> ok
        after 1000 -> ok
        end
    after
        meck:unload(channel_threads_config),
        meck:unload(guild_thread_load)
    end.

failed_flip_rearms_while_local_config_disagrees_test() ->
    meck:new(guild_thread_load, [passthrough, no_link]),
    meck:expect(guild_thread_load, fetch_flip_data, fun(_GuildId, _Version) ->
        {error, timeout}
    end),
    meck:new(channel_threads_config, [passthrough, no_link]),
    meck:expect(channel_threads_config, guild_active, fun(_GuildId) -> true end),
    try
        State1 = run_failed_flip(3, #{id => 5, data => #{}}),
        #{thread_flip_retry := {Timer1, 1}} = State1,
        ?assert(erlang:read_timer(Timer1) > 0),
        State2 = run_failed_flip(3, State1),
        #{thread_flip_retry := {Timer2, 2}} = State2,
        ?assertEqual(false, erlang:read_timer(Timer1)),
        ?assert(erlang:read_timer(Timer2) > 30000),
        meck:expect(channel_threads_config, guild_active, fun(_GuildId) -> false end),
        State3 = run_failed_flip(3, State2),
        ?assertNot(maps:is_key(thread_flip_retry, State3)),
        ?assertEqual(false, erlang:read_timer(Timer2))
    after
        meck:unload(channel_threads_config),
        meck:unload(guild_thread_load)
    end.

run_failed_flip(Version, State) ->
    #{thread_flip := {Ref, Version}} = State1 = start(Version, State),
    receive
        {thread_flip_result, Ref, Result} -> handle_result(Ref, Result, State1)
    after 1000 -> error(no_flip_result)
    end.

start_skips_a_version_the_active_gate_already_adopted_test() ->
    State = #{
        id => 5, data => #{thread_gate => #{active => true, version => 4}}, sessions => #{}
    },
    ?assertEqual(State, start(4, State)),
    ?assertEqual(State, start(3, State)).

adopted_flip_recomputes_viewers_stored_before_the_config_loaded_test() ->
    Key = channel_threads_config,
    Previous = persistent_term:get(Key, undefined),
    persistent_term:erase(Key),
    try
        Gate = #{id => 5, data => #{thread_gate => #{active => true, version => 4}}},
        Request = #{thread_channels_capable => true},
        Session = maps:merge(
            #{user_id => 7}, guild_thread_gate:session_fields(Request, 7, Gate)
        ),
        ?assertMatch(#{thread_viewer := false}, Session),
        State = Gate#{sessions => #{<<"s">> => Session}},
        persistent_term:put(Key, (channel_threads_config:default_config())#{
            enabled => true, config_version => 4, included_users => #{<<"7">> => true}
        }),
        ok = seed_short_jitter(),
        State1 = start(4, State),
        ?assertMatch(#{sessions := #{<<"s">> := #{thread_viewer := true}}}, State1),
        receive
            {thread_flip_resend, <<"s">>} -> ok
        after 1000 -> error(no_resend)
        end,
        ?assertEqual(State1, start(4, State1)),
        receive
            {thread_flip_resend, _} -> error(unexpected_resend)
        after 1000 -> ok
        end
    after
        case Previous of
            undefined -> persistent_term:erase(Key);
            _ -> persistent_term:put(Key, Previous)
        end
    end.

activation_without_viewer_changes_refreshes_viewable_channels_test() ->
    Key = channel_threads_config,
    Previous = persistent_term:get(Key, undefined),
    persistent_term:put(Key, (channel_threads_config:default_config())#{
        enabled => true, config_version => 4
    }),
    ok = guild_thread_store:init(),
    Cache = ets:new(viewable_channels_cache, [set, public]),
    ets:insert(Cache, {{}, #{200 => true}}),
    Text = #{<<"id">> => <<"200">>, <<"type">> => 0, <<"permission_overwrites">> => []},
    Forum = #{<<"id">> => <<"300">>, <<"type">> => 15, <<"permission_overwrites">> => []},
    Data = guild_data_index:put_channels([Text], #{
        <<"guild">> => #{<<"owner_id">> => <<"999">>},
        <<"roles">> => [
            #{
                <<"id">> => <<"5">>,
                <<"permissions">> => integer_to_binary(constants:view_channel_permission())
            }
        ],
        <<"members">> => #{
            7 => #{<<"user">> => #{<<"id">> => <<"7">>}, <<"roles">> => []}
        }
    }),
    Session = #{
        session_id => <<"s">>,
        user_id => 7,
        pid => self(),
        thread_capable => true,
        thread_viewer => false,
        viewable_channels => #{200 => true}
    },
    State0 = #{
        id => 5,
        data => Data,
        sessions => #{<<"s">> => Session},
        viewable_channels_cache => Cache,
        disable_permission_cache_updates => true
    },
    try
        State1 = apply_flip(
            #{
                <<"thread_gate">> => #{<<"active">> => true, <<"config_version">> => 4},
                <<"channels">> => [Text, Forum]
            },
            State0
        ),
        ?assertMatch(#{sessions := #{<<"s">> := #{thread_viewer := false}}}, State1),
        ?assertEqual([], ets:tab2list(Cache)),
        persistent_term:put(Key, (channel_threads_config:default_config())#{
            enabled => true, config_version => 5, included_users => #{<<"7">> => true}
        }),
        State2 = user_flip(<<"s">>, State1),
        ?assertMatch(#{sessions := #{<<"s">> := #{thread_viewer := true}}}, State2),
        #{data := Data2, sessions := Sessions2} = State2,
        Deleted = State2#{data => guild_data_index:put_channels([Text], Data2)},
        ?assertMatch(
            [{<<"s">>, _}],
            guild_sessions:filter_sessions_for_channel(Sessions2, 300, undefined, Deleted)
        )
    after
        ets:delete(Cache),
        case Previous of
            undefined -> persistent_term:erase(Key);
            _ -> persistent_term:put(Key, Previous)
        end
    end.

user_only_config_change_recomputes_an_active_guild_without_a_refetch_test() ->
    Key = channel_threads_config,
    Previous = persistent_term:get(Key, undefined),
    try
        Session = #{user_id => 7, thread_capable => true, thread_viewer => true},
        State = #{
            id => 5,
            data => #{thread_gate => #{active => true, version => 4}},
            sessions => #{<<"s">> => Session}
        },
        persistent_term:put(Key, (channel_threads_config:default_config())#{
            enabled => true,
            config_version => 6,
            enabled_guilds => #{<<"5">> => true},
            included_users => #{<<"7">> => true},
            excluded_users => #{<<"7">> => true}
        }),
        State1 = start(6, State),
        ?assertNot(maps:is_key(thread_flip, State1)),
        ?assertMatch(#{sessions := #{<<"s">> := #{thread_viewer := false}}}, State1),
        ?assertMatch(#{thread_resend_pending := #{<<"s">> := true}}, State1)
    after
        case Previous of
            undefined -> persistent_term:erase(Key);
            _ -> persistent_term:put(Key, Previous)
        end
    end.

seed_short_jitter() ->
    Seed = hd([N || N <- lists:seq(1, 100000), short_jitter_seed(N)]),
    _ = rand:seed(exsss, {Seed, Seed, Seed}),
    ok.

short_jitter_seed(N) ->
    _ = rand:seed(exsss, {N, N, N}),
    rand:uniform(?RESEND_JITTER_MS) < 200.

hold_records_thread_events_only_while_a_flip_is_in_flight_test() ->
    Idle = #{id => 5},
    ?assertEqual(Idle, hold(thread_create, #{}, Idle)),
    InFlight = Idle#{thread_flip_held => {0, []}},
    ?assertEqual(InFlight, hold(message_create, #{}, InFlight)),
    ?assertMatch(
        #{thread_flip_held := {1, [{thread_create, #{<<"guild_id">> := <<"5">>}}]}},
        hold(thread_create, #{}, InFlight)
    ).

hold_keeps_forum_channel_events_and_flags_overflow_test() ->
    InFlight = #{id => 5, thread_flip_held => {0, []}},
    Text = #{<<"id">> => <<"1">>, <<"type">> => 0},
    Forum = #{<<"id">> => <<"2">>, <<"type">> => 15},
    ?assertEqual(InFlight, hold(channel_create, Text, InFlight)),
    ?assertEqual(InFlight, hold(guild_role_update, #{}, InFlight)),
    ?assertMatch(
        #{thread_flip_held := {1, [{channel_delete, #{<<"id">> := <<"2">>}}]}},
        hold(channel_delete, Forum, InFlight)
    ),
    #{thread_flip_held := {1, [{channel_update_bulk, Bulk}]}} =
        hold(channel_update_bulk, #{<<"channels">> => [Text, Forum]}, InFlight),
    ?assertEqual([Forum], maps:get(<<"channels">>, Bulk)),
    Full = InFlight#{thread_flip_held => {?HOLD_CAP, []}},
    Overflow = hold(thread_create, #{}, Full),
    ?assertMatch(#{thread_flip_held := overflow}, Overflow),
    ?assertEqual(Overflow, hold(thread_create, #{}, Overflow)),
    ?assertEqual(Full, hold(message_create, #{}, Full)).

overflowed_flip_is_discarded_and_rearmed_test() ->
    meck:new(channel_threads_config, [passthrough, no_link]),
    meck:expect(channel_threads_config, guild_active, fun(_GuildId) -> true end),
    try
        Ref = make_ref(),
        State0 = #{id => 5, data => #{}, thread_flip => {Ref, 3}, thread_flip_held => overflow},
        State1 = handle_result(Ref, {ok, #{<<"config_version">> => 3}}, State0),
        ?assertNot(maps:is_key(thread_flip, State1)),
        ?assertNot(maps:is_key(thread_flip_held, State1)),
        ?assertEqual(#{}, maps:get(data, State1)),
        #{thread_flip_retry := {Timer, 1}} = State1,
        ?assert(erlang:read_timer(Timer) > 0),
        _ = erlang:cancel_timer(Timer)
    after
        meck:unload(channel_threads_config)
    end.

replay_skips_a_malformed_held_event_and_keeps_going_test() ->
    ok = guild_thread_store:init(),
    Tab = guild_thread_store:new(),
    try
        ok = guild_thread_store:put_thread(Tab, #{<<"id">> => 9, <<"parent_id">> => 1}),
        State = #{id => 5, data => #{thread_store => Tab, thread_gate => #{active => true}}},
        Held = [
            {thread_delete, #{<<"id">> => <<"9">>}},
            {thread_update, #{<<"name">> => <<"no-id">>}}
        ],
        ?assertEqual(State, replay_held(Held, State)),
        ?assertEqual(undefined, guild_thread_store:get_thread(Tab, 9))
    after
        guild_thread_store:destroy(Tab)
    end.

merge_channels_keeps_live_channels_and_replays_forum_events_test() ->
    Channel = fun(Id, Type, Name) ->
        #{<<"id">> => integer_to_binary(Id), <<"type">> => Type, <<"name">> => Name}
    end,
    Live = guild_data_index:put_channels(
        [Channel(1, 0, <<"live">>), Channel(2, 15, <<"old-forum">>)], #{}
    ),
    Snapshot = #{
        <<"channels">> => [
            Channel(1, 0, <<"stale">>),
            Channel(3, 0, <<"deleted-live">>),
            Channel(2, 15, <<"forum">>),
            Channel(4, 16, <<"media">>)
        ]
    },
    Held = [
        {channel_delete, Channel(4, 16, <<"media">>)},
        {channel_create, Channel(5, 15, <<"new-forum">>)},
        {channel_update, Channel(2, 15, <<"renamed-forum">>)}
    ],
    Merged = merge_channels(Snapshot, Held, Live),
    Names = maps:from_list([
        {maps:get(<<"id">>, C), maps:get(<<"name">>, C)}
     || C <- guild_data_index:channel_list(Merged)
    ]),
    ?assertEqual(
        #{1 => <<"live">>, 2 => <<"renamed-forum">>, 5 => <<"new-forum">>},
        maps:fold(fun(K, V, Acc) -> Acc#{snowflake_id:parse_optional(K) => V} end, #{}, Names)
    ).

collect_fetches(Acc) ->
    receive
        {fetch_started, Version} -> collect_fetches([Version | Acc])
    after 200 -> lists:sort(Acc)
    end.

pending_resends_and_syncs_survive_a_handoff_test_() ->
    {spawn, fun pending_resends_and_syncs_survive_a_handoff/0}.

pending_resends_and_syncs_survive_a_handoff() ->
    State0 = schedule_resends([<<"a">>, <<"b">>, <<"a">>], #{
        id => 5,
        data => #{},
        sessions => #{},
        thread_sync_pending => #{<<"c">> => queued},
        thread_list_dirty => #{10 => true, 11 => true},
        thread_list_flush => true
    }),
    ?assertEqual(#{<<"a">> => true, <<"b">> => true}, maps:get(thread_resend_pending, State0)),
    Exported = guild_manager_shard_lifecycle:normalize_transferred_guild_state(
        5, guild_handoff:export_handoff_state(State0)
    ),
    ?assertNot(maps:is_key(thread_resend_pending, Exported)),
    ?assertNot(maps:is_key(thread_sync_pending, Exported)),
    ?assertNot(maps:is_key(thread_list_dirty, Exported)),
    ?assertNot(maps:is_key(thread_list_flush, Exported)),
    Imported = guild_thread_subscriptions:restore_handoff(restore_handoff(Exported)),
    ?assertEqual(
        #{<<"a">> => true, <<"b">> => true}, maps:get(thread_resend_pending, Imported)
    ),
    ?assertEqual(#{<<"c">> => scheduled}, maps:get(thread_sync_pending, Imported)),
    ?assertNot(maps:is_key(thread_handoff_resends, Imported)),
    ?assertNot(maps:is_key(thread_handoff_syncs, Imported)),
    ?assertNot(maps:is_key(thread_handoff_lists, Imported)),
    ?assertMatch(
        #{
            thread_list_dirty := #{10 := true, 11 := true},
            thread_list_refresh := true,
            thread_list_flush := true
        },
        Imported
    ),
    receive
        thread_member_list_flush -> ok
    after 1000 -> error(list_flush_not_armed)
    end,
    Loading = guild_thread_subscriptions:handle_list_flush(Imported#{
        thread_load => #{status => loading, queue => [], queued => 0}
    }),
    ?assertMatch(
        #{thread_list_dirty := #{10 := true, 11 := true}, thread_list_refresh := true}, Loading
    ),
    ?assertNot(maps:is_key(thread_list_flush, Loading)),
    ?assertMatch(#{thread_resend_pending := #{<<"b">> := true}}, resend(<<"a">>, Imported)),
    ?assertNot(maps:is_key(thread_resend_pending, resend(<<"b">>, resend(<<"a">>, Imported)))),
    ?assertEqual(#{id => 5, data => #{}}, restore_handoff(#{id => 5, data => #{}})),
    Plain = guild_handoff:export_handoff_state(#{id => 5, data => #{}, sessions => #{}}),
    ?assertNot(maps:is_key(thread_handoff_resends, Plain)),
    ?assertNot(maps:is_key(thread_handoff_syncs, Plain)),
    ?assertNot(maps:is_key(thread_handoff_lists, Plain)).

-endif.
