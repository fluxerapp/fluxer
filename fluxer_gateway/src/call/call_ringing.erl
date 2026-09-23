%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(call_ringing).
-typing([eqwalizer]).

-export([
    ensure_initiator_ready/1,
    maybe_dispatch_pending_ringing/1,
    maybe_dispatch_pending_ringing/2,
    maybe_dispatch_state_update/2,
    remove_users_from_ringing/2,
    start_ringing_timers/2,
    cancel_ringing_timers/2,
    cancel_all_ringing_timers/1,
    reset_idle_timer/1,
    dispatch_call_create/1,
    dispatch_call_update/1,
    dispatch_call_delete/1,
    handle_ring_timeout/2,
    handle_idle_timeout/1,
    maybe_stop_or_reply/3,
    maybe_stop_or_map_reply/3,
    maybe_stop_or_noreply/2,
    maybe_stop_if_empty/1
]).

-define(RING_TIMEOUT_MS, 30000).
-define(IDLE_TIMEOUT_MS, 120000).

-spec ensure_initiator_ready(map()) -> map().
ensure_initiator_ready(State) ->
    case maps:get(initiator_ready, State) of
        true -> State;
        false -> State#{initiator_ready => true}
    end.

-spec maybe_dispatch_pending_ringing(map()) -> {map(), boolean()}.
maybe_dispatch_pending_ringing(State) ->
    maybe_dispatch_pending_ringing(State, true).

-spec maybe_dispatch_pending_ringing(map(), boolean()) -> {map(), boolean()}.
maybe_dispatch_pending_ringing(State, DispatchUpdates) ->
    case maps:get(initiator_ready, State) of
        false ->
            {State, false};
        true ->
            dispatch_pending_ringing_ready(State, DispatchUpdates)
    end.

-spec dispatch_pending_ringing_ready(map(), boolean()) -> {map(), boolean()}.
dispatch_pending_ringing_ready(State, DispatchUpdates) ->
    PendingUnique = lists:usort(maps:get(pending_ringing, State)),
    case PendingUnique of
        [] ->
            {State#{pending_ringing => []}, false};
        _ ->
            apply_pending_ringing(PendingUnique, State, DispatchUpdates)
    end.

-spec apply_pending_ringing([integer()], map(), boolean()) -> {map(), boolean()}.
apply_pending_ringing(PendingUnique, State, DispatchUpdates) ->
    ConnectedUsers = maps:keys(maps:get(voice_states, State)),
    AlreadyRinging = maps:get(ringing, State),
    ToAdd = [
        User
     || User <- PendingUnique,
        not lists:member(User, ConnectedUsers),
        not lists:member(User, AlreadyRinging)
    ],
    NewRinging =
        case ToAdd of
            [] -> AlreadyRinging;
            _ -> lists:usort(AlreadyRinging ++ ToAdd)
        end,
    StateWithRinging = State#{pending_ringing => [], ringing => NewRinging},
    ok = publish_rings(ToAdd, StateWithRinging),
    StateWithTimers = start_ringing_timers(ToAdd, StateWithRinging),
    case ToAdd of
        [] ->
            {StateWithTimers, false};
        _ when DispatchUpdates ->
            UpdatedState = dispatch_call_update(StateWithTimers),
            {UpdatedState, true};
        _ ->
            {StateWithTimers, false}
    end.

-spec publish_rings([integer()], map()) -> ok.
publish_rings([], _State) ->
    ok;
publish_rings(UserIds, State) ->
    ChannelId = maps:get(channel_id, State),
    MessageId = maps:get(message_id, State),
    Recipients = maps:get(recipients, State),
    StartedAt = erlang:system_time(millisecond),
    ExpiresAt = StartedAt + ?RING_TIMEOUT_MS,
    _ = proc_lib:spawn(fun() ->
        publish_ring_jobs(UserIds, Recipients, ChannelId, MessageId, StartedAt, ExpiresAt)
    end),
    ok.

-spec publish_ring_jobs(
    [integer()], [integer()], integer(), integer(), integer(), integer()
) -> ok.
publish_ring_jobs(UserIds, Recipients, ChannelId, MessageId, StartedAt, ExpiresAt) ->
    lists:foreach(
        fun(UserId) ->
            publish_ring_job(UserId, Recipients, ChannelId, MessageId, StartedAt, ExpiresAt)
        end,
        UserIds
    ).

-spec publish_ring_job(
    integer(), [integer()], integer(), integer(), integer(), integer()
) -> ok.
publish_ring_job(UserId, Recipients, ChannelId, MessageId, StartedAt, ExpiresAt) ->
    case ring_suppressed(UserId, Recipients) of
        true ->
            ok;
        false ->
            _ = push_job_publisher:publish_ring(
                UserId, ChannelId, MessageId, StartedAt, ExpiresAt
            ),
            ok
    end.

-spec ring_suppressed(integer(), [integer()]) -> boolean().
ring_suppressed(UserId, Recipients) ->
    all_blocked(UserId, [Other || Other <- Recipients, Other =/= UserId]).

-spec all_blocked(integer(), [integer()]) -> boolean().
all_blocked(_UserId, []) ->
    false;
all_blocked(UserId, Others) ->
    lists:all(fun(Other) -> push_eligibility:is_user_blocked(UserId, Other) end, Others).

-spec maybe_dispatch_state_update(map(), map()) -> {map(), boolean()}.
maybe_dispatch_state_update(PrevState, NewState) ->
    CountedState = call_state:sync_voice_state_count_diff(PrevState, NewState),
    case maps:get(initiator_ready, PrevState) of
        true ->
            maybe_dispatch_ringing_change(PrevState, CountedState);
        false ->
            {CountedState, false}
    end.

-spec maybe_dispatch_ringing_change(map(), map()) -> {map(), boolean()}.
maybe_dispatch_ringing_change(PrevState, CountedState) ->
    case maps:get(ringing, PrevState) =:= maps:get(ringing, CountedState) of
        true ->
            {CountedState, false};
        false ->
            UpdatedState = dispatch_call_update(CountedState),
            {UpdatedState, true}
    end.

-spec remove_users_from_ringing([integer()], map()) -> map().
remove_users_from_ringing(Users, State) ->
    {NewRinging, NewPending} = lists:foldl(
        fun(User, {RingingAcc, PendingAcc}) ->
            {lists:delete(User, RingingAcc), lists:delete(User, PendingAcc)}
        end,
        {maps:get(ringing, State), maps:get(pending_ringing, State)},
        Users
    ),
    State#{ringing => NewRinging, pending_ringing => NewPending}.

-spec start_ringing_timers([integer()], map()) -> map().
start_ringing_timers([], State) ->
    State;
start_ringing_timers([User | Rest], State) ->
    case maps:is_key(User, maps:get(ringing_timers, State)) of
        true ->
            start_ringing_timers(Rest, State);
        false ->
            Ref = erlang:send_after(?RING_TIMEOUT_MS, self(), {ring_timeout, User}),
            UpdatedTimers = (maps:get(ringing_timers, State))#{User => Ref},
            start_ringing_timers(Rest, State#{ringing_timers => UpdatedTimers})
    end.

-spec cancel_ringing_timers([integer()], map()) -> map().
cancel_ringing_timers([], State) ->
    State;
cancel_ringing_timers([User | Rest], State) ->
    case maps:is_key(User, maps:get(ringing_timers, State)) of
        true ->
            Ref = maps:get(User, maps:get(ringing_timers, State)),
            _ = erlang:cancel_timer(Ref),
            UpdatedTimers = maps:remove(User, maps:get(ringing_timers, State)),
            cancel_ringing_timers(Rest, State#{ringing_timers => UpdatedTimers});
        false ->
            cancel_ringing_timers(Rest, State)
    end.

-spec cancel_all_ringing_timers(map()) -> map().
cancel_all_ringing_timers(State) ->
    TimerRefs = maps:values(maps:get(ringing_timers, State)),
    lists:foreach(fun cancel_timer/1, TimerRefs),
    State#{ringing_timers => #{}}.

-spec reset_idle_timer(map()) -> map().
reset_idle_timer(State) ->
    case maps:get(idle_timer, State) of
        undefined -> ok;
        OldRef -> cancel_timer(OldRef)
    end,
    NewRef = erlang:send_after(?IDLE_TIMEOUT_MS, self(), idle_timeout),
    State#{idle_timer => NewRef}.

-spec cancel_timer(reference()) -> ok.
cancel_timer(Ref) ->
    _ = erlang:cancel_timer(Ref),
    ok.

-spec dispatch_call_create(map()) -> map().
dispatch_call_create(State) ->
    Event = call_state:build_call_event(State),
    Recipients = maps:get(recipients, State),
    dispatch_to_recipients(Recipients, call_create, Event),
    State#{last_call_event => Event}.

-spec dispatch_call_update(map()) -> map().
dispatch_call_update(State) ->
    Event = call_state:build_call_event(State),
    case maps:get(last_call_event, State, undefined) of
        Event ->
            State;
        _ ->
            Recipients = maps:get(recipients, State),
            dispatch_to_recipients(Recipients, call_update, Event),
            State#{last_call_event => Event}
    end.

-spec dispatch_call_delete(map()) -> ok.
dispatch_call_delete(State) ->
    Event = #{
        channel_id => integer_to_binary(maps:get(channel_id, State))
    },
    Recipients = maps:get(recipients, State),
    dispatch_to_recipients(Recipients, call_delete, Event),
    notify_call_ended(cancel_all_ringing_timers(State)),
    ok.

-spec dispatch_to_recipients([integer()], atom(), map()) -> pid().
dispatch_to_recipients(Recipients, EventType, Event) ->
    proc_lib:spawn(fun() ->
        dispatch_recipients(Recipients, EventType, Event)
    end).

-spec dispatch_recipients([integer()], atom(), map()) -> ok.
dispatch_recipients(Recipients, EventType, Event) ->
    lists:foreach(
        fun(RecipientId) ->
            dispatch_to_recipient(RecipientId, EventType, Event)
        end,
        Recipients
    ).

-spec dispatch_to_recipient(integer(), atom(), map()) -> ok.
dispatch_to_recipient(RecipientId, EventType, Event) ->
    try
        _ = presence_manager:dispatch_to_user(RecipientId, EventType, Event),
        ok
    catch
        error:_ -> ok;
        exit:_ -> ok
    end.

-spec notify_call_ended(map()) -> ok.
notify_call_ended(State) ->
    Participants = sets:to_list(maps:get(participants_history, State)),
    EndedAt = erlang:system_time(millisecond),
    ChannelId = maps:get(channel_id, State),
    MessageId = maps:get(message_id, State),
    Request = #{
        <<"type">> => <<"call_ended">>,
        <<"channel_id">> => integer_to_binary(ChannelId),
        <<"message_id">> => integer_to_binary(MessageId),
        <<"participants">> => call_state:integer_list_to_binaries(Participants),
        <<"ended_timestamp">> => EndedAt
    },
    proc_lib:spawn(fun() -> rpc_client:call(Request) end),
    ok.

-spec handle_ring_timeout(integer(), map()) -> {noreply, map()} | {stop, normal, map()}.
handle_ring_timeout(UserId, State) ->
    case maps:get(UserId, maps:get(ringing_timers, State), undefined) of
        undefined ->
            {noreply, State};
        _ ->
            CancelState = cancel_ringing_timers([UserId], State),
            RingState = remove_users_from_ringing([UserId], CancelState),
            {UpdState, _} = maybe_dispatch_state_update(State, RingState),
            maybe_stop_if_empty(UpdState)
    end.

-spec handle_idle_timeout(map()) -> {noreply, map()} | {stop, normal, map()}.
handle_idle_timeout(State) ->
    case call_has_activity(State) of
        true ->
            {noreply, reset_idle_timer(State)};
        false ->
            dispatch_call_delete(State),
            {stop, normal, State}
    end.

-spec maybe_stop_or_reply(map(), boolean(), Reply) ->
    {reply, Reply, map()} | {stop, normal, Reply, map()}
when
    Reply :: term().
maybe_stop_or_reply(State, Dispatched, Reply) ->
    case maps:size(maps:get(voice_states, State)) of
        0 ->
            dispatch_call_delete(State),
            {stop, normal, Reply, State};
        _ ->
            {reply, Reply, ensure_call_update(State, Dispatched)}
    end.

-spec maybe_stop_or_map_reply(map(), boolean(), map()) ->
    {reply, map(), map()} | {stop, normal, map(), map()}.
maybe_stop_or_map_reply(State, Dispatched, Reply) ->
    map_reply_result(maybe_stop_or_reply(State, Dispatched, Reply)).

-spec map_reply_result({reply, term(), map()} | {stop, normal, term(), map()}) ->
    {reply, map(), map()} | {stop, normal, map(), map()}.
map_reply_result({reply, Reply, State}) when is_map(Reply) ->
    {reply, Reply, State};
map_reply_result({stop, normal, Reply, State}) when is_map(Reply) ->
    {stop, normal, Reply, State}.

-spec maybe_stop_or_noreply(map(), boolean()) ->
    {noreply, map()} | {stop, normal, map()}.
maybe_stop_or_noreply(State, Dispatched) ->
    case maps:size(maps:get(voice_states, State)) of
        0 ->
            dispatch_call_delete(State),
            {stop, normal, State};
        _ ->
            {noreply, ensure_call_update(State, Dispatched)}
    end.

-spec ensure_call_update(map(), boolean()) -> map().
ensure_call_update(State, true) ->
    State;
ensure_call_update(State, false) ->
    dispatch_call_update(State).

-spec maybe_stop_if_empty(map()) -> {noreply, map()} | {stop, normal, map()}.
maybe_stop_if_empty(State) ->
    case call_has_activity(State) of
        true ->
            {noreply, State};
        false ->
            dispatch_call_delete(State),
            {stop, normal, State}
    end.

-spec call_has_activity(map()) -> boolean().
call_has_activity(State) ->
    maps:size(maps:get(voice_states, State)) > 0 orelse maps:get(ringing, State) =/= [].

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

ring_state(Recipients, Ringing, Pending, VoiceStates) ->
    #{
        channel_id => 100,
        message_id => 200,
        region => undefined,
        ringing => Ringing,
        pending_ringing => Pending,
        recipients => Recipients,
        voice_states => VoiceStates,
        ringing_timers => #{},
        initiator_ready => true,
        last_call_event => undefined
    }.

published_ring_args() ->
    History = meck:history(push_job_publisher),
    [Args || {_Pid, {push_job_publisher, publish_ring, Args}, _Result} <- History].

published_ring_users() ->
    lists:usort([UserId || [UserId, _C, _M, _S, _E] <- published_ring_args()]).

mock_ring_publisher() ->
    ok = meck:new(push_job_publisher, [passthrough, no_link]),
    meck:expect(push_job_publisher, publish_ring, fun(_U, _C, _M, _S, _E) -> ok end).

apply_pending_ringing_publishes_one_ring_per_new_user_test() ->
    ok = mock_ring_publisher(),
    try
        State = ring_state([1, 2, 3], [], [2, 3], #{}),
        _ = maybe_dispatch_pending_ringing(State, false),
        ok = meck:wait(2, push_job_publisher, publish_ring, '_', 2000),
        ?assertEqual([2, 3], published_ring_users())
    after
        meck:unload(push_job_publisher)
    end.

apply_pending_ringing_publishes_the_frozen_ring_fields_test() ->
    ok = mock_ring_publisher(),
    try
        State = ring_state([1, 2], [], [2], #{}),
        _ = maybe_dispatch_pending_ringing(State, false),
        ok = meck:wait(1, push_job_publisher, publish_ring, '_', 2000),
        [[UserId, ChannelId, MessageId, StartedAt, ExpiresAt]] = published_ring_args(),
        ?assertEqual({2, 100, 200}, {UserId, ChannelId, MessageId}),
        ?assertEqual(?RING_TIMEOUT_MS, ExpiresAt - StartedAt)
    after
        meck:unload(push_job_publisher)
    end.

apply_pending_ringing_does_not_publish_for_a_user_already_ringing_test() ->
    ok = mock_ring_publisher(),
    try
        State = ring_state([1, 2], [2], [2], #{}),
        _ = maybe_dispatch_pending_ringing(State, false),
        ?assertEqual([], published_ring_args())
    after
        meck:unload(push_job_publisher)
    end.

apply_pending_ringing_does_not_publish_for_a_connected_user_test() ->
    ok = mock_ring_publisher(),
    try
        State = ring_state([1, 2], [], [2], #{2 => #{}}),
        _ = maybe_dispatch_pending_ringing(State, false),
        ?assertEqual([], published_ring_args())
    after
        meck:unload(push_job_publisher)
    end.

start_ringing_timers_does_not_publish_test() ->
    ok = mock_ring_publisher(),
    try
        State = ring_state([1, 2], [2], [], #{}),
        _ = start_ringing_timers([2], State),
        ?assertEqual([], published_ring_args())
    after
        meck:unload(push_job_publisher)
    end.

publish_ring_jobs_suppresses_a_fully_blocked_ring_test() ->
    ok = mock_ring_publisher(),
    ok = meck:new(push_eligibility, [passthrough, no_link]),
    try
        ok = meck:expect(push_eligibility, is_user_blocked, fun(_U, _O) -> true end),
        ok = publish_ring_jobs([2], [1, 2], 100, 200, 1, 2),
        ?assertEqual([], published_ring_args())
    after
        meck:unload(push_eligibility),
        meck:unload(push_job_publisher)
    end.

ring_is_suppressed_when_every_other_recipient_is_blocked_test() ->
    ok = meck:new(push_eligibility, [passthrough, no_link]),
    try
        ok = meck:expect(push_eligibility, is_user_blocked, fun(_U, _O) -> true end),
        ?assertEqual(true, ring_suppressed(2, [1, 2, 3]))
    after
        meck:unload(push_eligibility)
    end.

ring_is_not_suppressed_when_one_other_recipient_is_unblocked_test() ->
    ok = meck:new(push_eligibility, [passthrough, no_link]),
    try
        ok = meck:expect(push_eligibility, is_user_blocked, fun(_U, Other) -> Other =:= 1 end),
        ?assertEqual(false, ring_suppressed(2, [1, 2, 3]))
    after
        meck:unload(push_eligibility)
    end.

ring_is_not_suppressed_for_a_lone_recipient_test() ->
    ok = meck:new(push_eligibility, [passthrough, no_link]),
    try
        ok = meck:expect(push_eligibility, is_user_blocked, fun(_U, _O) -> true end),
        ?assertEqual(false, ring_suppressed(2, [2]))
    after
        meck:unload(push_eligibility)
    end.

-endif.
