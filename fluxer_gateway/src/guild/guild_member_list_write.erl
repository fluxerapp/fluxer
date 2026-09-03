%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_member_list_write).
-typing([eqwalizer]).

-export([
    broadcast_member_list_updates/3,
    broadcast_member_list_updates/5,
    broadcast_all_member_list_updates/1,
    broadcast_member_list_updates_for_channel/2,
    broadcast_channel_engine_connection_change/2,
    flush_pending_member_list_syncs/1,
    resync_hoisted_member_lists/1,
    rebuild_channels_for_permission_change/2,
    resync_channels_for_permission_change/2,
    resync_all_channels_for_permission_change/1
]).

-type guild_state() :: map().
-type list_id() :: binary().
-type user_id() :: integer().
-type channel_id() :: integer().
-type engine_ref() :: ets:table().
-type absence() :: {absent, engine_ref()} | present.

-define(MAX_MEMBER_LIST_SYNC_SKIPPED_ABSENT, 1000000000).

-export_type([guild_state/0, list_id/0, user_id/0, channel_id/0]).

-spec broadcast_member_list_updates(user_id() | undefined, guild_state(), guild_state()) ->
    {ok, guild_state()}.
broadcast_member_list_updates(undefined, _OldState, UpdatedState) ->
    {ok, UpdatedState};
broadcast_member_list_updates(UserId, OldState, UpdatedState) ->
    broadcast_member_list_updates(UserId, OldState, UpdatedState, undefined, undefined).

-spec broadcast_member_list_updates(
    user_id() | undefined,
    guild_state(),
    guild_state(),
    map() | undefined,
    map() | undefined
) -> {ok, guild_state()}.
broadcast_member_list_updates(
    undefined,
    _OldState,
    UpdatedState,
    _OldPresence,
    _NewPresence
) ->
    {ok, UpdatedState};
broadcast_member_list_updates(UserId, OldState, UpdatedState, OldPresence, NewPresence) ->
    guild_member_list_write_context:with_guild_id(UpdatedState, fun(_GuildId) ->
        OldMember = find_member_in_state_data(UserId, OldState),
        NewMember = find_member_in_state_data(UserId, UpdatedState),
        State1 = dispatch_presence_delta(
            UserId, OldMember, NewMember, OldPresence, NewPresence, UpdatedState
        ),
        {ok, State1}
    end).

-spec dispatch_presence_delta(
    user_id(),
    map() | undefined,
    map() | undefined,
    map() | undefined,
    map() | undefined,
    guild_state()
) -> guild_state().
dispatch_presence_delta(UserId, OldMember, NewMember, OldPresence, NewPresence, State) ->
    case presence_delta_is_inert(OldPresence, NewPresence, OldMember, NewMember) of
        true ->
            State;
        false ->
            SubsTab = maps:get(member_list_subscriptions, State),
            dispatch_user_change_to_subscribed_lists(
                UserId, OldMember, NewMember, SubsTab, State
            )
    end.

-spec presence_delta_is_inert(
    map() | undefined,
    map() | undefined,
    map() | undefined,
    map() | undefined
) -> boolean().
presence_delta_is_inert(OldPresence, NewPresence, Member, Member) when
    is_map(OldPresence), is_map(NewPresence), is_map(Member)
->
    member_list_presence_fields(OldPresence) =:= member_list_presence_fields(NewPresence);
presence_delta_is_inert(_OldPresence, _NewPresence, _OldMember, _NewMember) ->
    false.

-spec member_list_presence_fields(map()) -> {binary(), term()}.
member_list_presence_fields(Presence) ->
    {
        maps:get(<<"status">>, Presence, <<"offline">>),
        maps:get(<<"custom_status">>, Presence, null)
    }.

-spec find_member_in_state_data(user_id(), guild_state()) -> map() | undefined.
find_member_in_state_data(UserId, State) ->
    guild_data_index:get_member(UserId, maps:get(data, State, #{})).

-spec broadcast_all_member_list_updates(guild_state()) -> {ok, guild_state()}.
broadcast_all_member_list_updates(State) ->
    guild_member_list_write_context:with_guild_id(State, fun(_GuildId) ->
        SubsTab = maps:get(member_list_subscriptions, State),
        Rebuilt = rebuild_subscribed_channel_lists(State, SubsTab),
        {ok, guild_member_list_sync_batch:queue_subscribed_list_syncs(Rebuilt, SubsTab)}
    end).

-spec resync_channels_for_permission_change([channel_id()], guild_state()) -> guild_state().
resync_channels_for_permission_change(ChannelIds, State) ->
    State1 = rebuild_channels_for_permission_change(ChannelIds, State),
    lists:foldl(
        fun(ChannelId, Acc) ->
            {ok, Next} = broadcast_member_list_updates_for_channel(ChannelId, Acc),
            Next
        end,
        State1,
        ChannelIds
    ).

-spec rebuild_channels_for_permission_change([channel_id()], guild_state()) -> guild_state().
rebuild_channels_for_permission_change(ChannelIds, State) ->
    guild_member_list_channel_engine:rebuild_channels(ChannelIds, State).

-spec resync_all_channels_for_permission_change(guild_state()) -> guild_state().
resync_all_channels_for_permission_change(State) ->
    State1 = guild_member_list_channel_engine:rebuild_all(State),
    resync_hoisted_member_lists(State1).

-spec resync_hoisted_member_lists(guild_state()) -> guild_state().
resync_hoisted_member_lists(State) ->
    case maps:get(member_list_subscriptions, State, undefined) of
        undefined ->
            State;
        SubsTab ->
            resync_hoisted_member_lists(State, SubsTab)
    end.

-spec resync_hoisted_member_lists(guild_state(), ets:table()) -> guild_state().
resync_hoisted_member_lists(State, SubsTab) ->
    guild_member_list_sync_batch:queue_subscribed_list_syncs(State, SubsTab).

-spec broadcast_member_list_updates_for_channel(channel_id(), guild_state()) ->
    {ok, guild_state()}.
broadcast_member_list_updates_for_channel(ChannelId, State) when
    is_integer(ChannelId), ChannelId > 0
->
    guild_member_list_write_context:with_guild_id(State, fun(GuildId) ->
        broadcast_channel_with_guild_id(GuildId, ChannelId, State)
    end);
broadcast_member_list_updates_for_channel(_ChannelId, State) ->
    {ok, State}.

-spec broadcast_channel_with_guild_id(integer(), channel_id(), guild_state()) ->
    {ok, guild_state()}.
broadcast_channel_with_guild_id(GuildId, ChannelId, State) ->
    case guild_member_list:calculate_list_id(ChannelId, State) of
        undefined -> {ok, State};
        ListId -> broadcast_list_by_id(GuildId, ChannelId, ListId, State)
    end.

-spec broadcast_channel_engine_connection_change(user_id(), guild_state()) -> guild_state().
broadcast_channel_engine_connection_change(UserId, State) ->
    case maps:get(member_list_subscriptions, State, undefined) of
        undefined ->
            State;
        SubsTab ->
            broadcast_channel_engine_connection_change(UserId, State, SubsTab)
    end.

-spec broadcast_channel_engine_connection_change(user_id(), guild_state(), ets:table()) ->
    guild_state().
broadcast_channel_engine_connection_change(UserId, State, SubsTab) ->
    {ok, NewState} = guild_member_list_write_context:with_guild_id(State, fun(GuildId) ->
        {ok, fold_connection_change_lists(GuildId, UserId, State, SubsTab)}
    end),
    NewState.

-spec fold_connection_change_lists(integer(), user_id(), guild_state(), ets:table()) ->
    guild_state().
fold_connection_change_lists(GuildId, UserId, State, SubsTab) ->
    Sessions = maps:get(sessions, State, #{}),
    lists:foldl(
        fun(ListId, AccState) ->
            sync_connection_change_for_subscribed_list(
                GuildId, UserId, ListId, Sessions, AccState
            )
        end,
        State,
        guild_member_list_subs:list_ids(SubsTab)
    ).

-spec dispatch_user_change_to_subscribed_lists(
    user_id(),
    map() | undefined,
    map() | undefined,
    ets:table(),
    guild_state()
) -> guild_state().
dispatch_user_change_to_subscribed_lists(
    UserId, OldMember, NewMember, SubsTab, State
) ->
    lists:foldl(
        fun(ListId, AccState) ->
            dispatch_user_change_to_subscribed_list(
                UserId, OldMember, NewMember, ListId, AccState
            )
        end,
        State,
        guild_member_list_subs:list_ids(SubsTab)
    ).

-spec dispatch_user_change_to_subscribed_list(
    user_id(),
    map() | undefined,
    map() | undefined,
    list_id(),
    guild_state()
) -> guild_state().
dispatch_user_change_to_subscribed_list(UserId, OldMember, NewMember, ListId, State) ->
    Absence = user_change_absence(UserId, ListId, OldMember, NewMember, State),
    State1 = apply_user_change_to_channel_store(UserId, ListId, OldMember, NewMember, State),
    case sync_body_unchanged(UserId, ListId, Absence, State1) of
        true -> record_member_list_sync_skipped_absent(State1);
        false -> guild_member_list_sync_batch:queue_list_sync(ListId, State1)
    end.

-spec user_change_absence(
    user_id(), list_id(), map() | undefined, map() | undefined, guild_state()
) -> absence().
user_change_absence(UserId, ListId, Member, Member, State) when is_map(Member) ->
    list_member_absence(UserId, ListId, State);
user_change_absence(_UserId, _ListId, _OldMember, _NewMember, _State) ->
    present.

-spec apply_user_change_to_channel_store(
    user_id(), list_id(), map() | undefined, map() | undefined, guild_state()
) -> guild_state().
apply_user_change_to_channel_store(UserId, ListId, OldMember, NewMember, State) ->
    case guild_member_list_channel_engine:is_engine_list(ListId, State) of
        true ->
            State1 = guild_member_list_channel_engine:ensure(ListId, State),
            ok = apply_channel_member_change(UserId, ListId, OldMember, NewMember, State1),
            State1;
        false ->
            State
    end.

-spec apply_channel_member_change(
    user_id(), list_id(), map() | undefined, map() | undefined, guild_state()
) -> ok.
apply_channel_member_change(UserId, ListId, _OldMember, undefined, State) ->
    guild_member_list_channel_engine:remove_user(UserId, ListId, State);
apply_channel_member_change(UserId, ListId, OldMember, NewMember, State) when
    OldMember =/= NewMember
->
    guild_member_list_channel_engine:update_user(UserId, ListId, State);
apply_channel_member_change(_UserId, _ListId, _OldMember, _NewMember, _State) ->
    ok.

-spec sync_connection_change_for_subscribed_list(
    integer(), user_id(), list_id(), map(), guild_state()
) -> guild_state().
sync_connection_change_for_subscribed_list(_GuildId, UserId, ListId, _Sessions, State) ->
    case guild_member_list_channel_engine:is_engine_list(ListId, State) of
        true ->
            queue_connection_list_sync_unless_absent(UserId, ListId, State);
        false ->
            State
    end.

-spec broadcast_list_by_id(integer(), channel_id(), list_id(), guild_state()) ->
    {ok, guild_state()}.
broadcast_list_by_id(_GuildId, _ChannelId, ListId, State) ->
    SubsTab = maps:get(member_list_subscriptions, State),
    ListSubs = guild_member_list_subs:get_list_subs(ListId, SubsTab),
    case map_size(ListSubs) of
        0 ->
            {ok, State};
        _ ->
            State1 = rebuild_channel_store(ListId, State),
            {ok, guild_member_list_sync_batch:queue_list_sync(ListId, State1)}
    end.

-spec rebuild_subscribed_channel_lists(guild_state(), ets:table()) -> guild_state().
rebuild_subscribed_channel_lists(State, SubsTab) ->
    lists:foldl(
        fun rebuild_channel_store/2,
        State,
        guild_member_list_subs:list_ids(SubsTab)
    ).

-spec rebuild_channel_store(list_id(), guild_state()) -> guild_state().
rebuild_channel_store(ListId, State) ->
    case guild_member_list_channel_engine:is_engine_list(ListId, State) of
        true -> guild_member_list_channel_engine:rebuild(ListId, State);
        false -> State
    end.

-spec queue_connection_list_sync_unless_absent(user_id(), list_id(), guild_state()) ->
    guild_state().
queue_connection_list_sync_unless_absent(UserId, ListId, State) ->
    case list_member_absence(UserId, ListId, State) of
        {absent, _Ref} -> record_member_list_sync_skipped_absent(State);
        present -> queue_connection_list_sync(ListId, State)
    end.

-spec queue_connection_list_sync(list_id(), guild_state()) -> guild_state().
queue_connection_list_sync(ListId, State) ->
    case maps:get(pending_member_list_sync_batch, State, undefined) of
        #{pending_list_ids := PendingListIds} = Batch when is_map(PendingListIds) ->
            State#{
                pending_member_list_sync_batch => Batch#{
                    pending_list_ids => PendingListIds#{ListId => true}
                }
            };
        _ ->
            TimerRef = erlang:send_after(
                connection_sync_delay_ms(State), self(), flush_member_list_sync_batch
            ),
            State#{
                pending_member_list_sync_batch => #{
                    pending_list_ids => #{ListId => true},
                    timer_ref => TimerRef
                }
            }
    end.

-spec connection_sync_delay_ms(guild_state()) -> pos_integer().
connection_sync_delay_ms(State) ->
    case State of
        #{member_count := MemberCount} when is_integer(MemberCount), MemberCount >= 5000 ->
            positive_env(member_list_large_connection_sync_delay_ms, 1000);
        _ ->
            positive_env(member_list_connection_sync_delay_ms, 250)
    end.

-spec positive_env(atom(), pos_integer()) -> pos_integer().
positive_env(Key, Default) ->
    case application:get_env(fluxer_gateway, Key, Default) of
        Value when is_integer(Value), Value >= 1 -> Value;
        _ -> Default
    end.

-spec record_member_list_sync_skipped_absent(guild_state()) -> guild_state().
record_member_list_sync_skipped_absent(State) ->
    case maps:get(member_list_sync_skipped_absent, State, 0) of
        Count when
            is_integer(Count), Count >= 0, Count < ?MAX_MEMBER_LIST_SYNC_SKIPPED_ABSENT
        ->
            State#{member_list_sync_skipped_absent => Count + 1};
        ?MAX_MEMBER_LIST_SYNC_SKIPPED_ABSENT ->
            State;
        Invalid ->
            erlang:error({invalid_member_list_sync_skipped_absent, Invalid})
    end.

-spec sync_body_unchanged(user_id(), list_id(), absence(), guild_state()) -> boolean().
sync_body_unchanged(_UserId, _ListId, present, _State) ->
    false;
sync_body_unchanged(UserId, ListId, {absent, Ref}, State) ->
    list_store_ref(ListId, State) =:= Ref andalso member_row_absent(UserId, Ref).

-spec list_member_absence(user_id(), list_id(), guild_state()) -> absence().
list_member_absence(UserId, ListId, State) ->
    absence_for_store(UserId, list_store_ref(ListId, State)).

-spec absence_for_store(user_id(), engine_ref() | undefined) -> absence().
absence_for_store(_UserId, undefined) ->
    present;
absence_for_store(UserId, Ref) ->
    case member_row_absent(UserId, Ref) of
        true -> {absent, Ref};
        false -> present
    end.

-spec member_row_absent(user_id(), engine_ref()) -> boolean().
member_row_absent(UserId, Ref) when is_integer(UserId), UserId > 0 ->
    try guild_member_list_engine:is_member_online(Ref, UserId) of
        not_present -> true;
        _Online -> false
    catch
        _:_ -> false
    end;
member_row_absent(_UserId, _Ref) ->
    false.

-spec list_store_ref(list_id(), guild_state()) -> engine_ref() | undefined.
list_store_ref(<<"0">>, State) ->
    case maps:get(member_list_engine, State, undefined) of
        undefined -> undefined;
        Ref when is_reference(Ref); is_atom(Ref) -> Ref;
        _Other -> undefined
    end;
list_store_ref(ListId, State) ->
    guild_member_list_channel_engine:ref(ListId, State).

-spec flush_pending_member_list_syncs(guild_state()) -> guild_state().
flush_pending_member_list_syncs(State) ->
    guild_member_list_sync_batch:flush_pending_syncs(State).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

apply_user_change_to_channel_store_updates_engine_test() ->
    Ref = guild_member_list_engine:new(),
    State = #{channel_member_list_engines => #{<<"123">> => Ref}, member_presence => #{}},
    Member = #{<<"user">> => #{<<"id">> => <<"2">>}, <<"roles">> => []},
    State1 = apply_user_change_to_channel_store(2, <<"123">>, undefined, Member, State),
    ?assert(maps:is_key(<<"123">>, maps:get(channel_member_list_engines, State1))).

absence_is_absent_for_unloaded_member_test() ->
    Ref = guild_member_list_engine:new(),
    State = engine_state(Ref),
    try
        ?assertEqual({absent, Ref}, list_member_absence(7, <<"500">>, State))
    after
        guild_member_list_engine:destroy(Ref)
    end.

absence_is_present_for_loaded_member_test() ->
    Ref = guild_member_list_engine:new(),
    State = engine_state(Ref),
    try
        ok = guild_member_list_engine:bulk_load(Ref, [{7, <<"seven">>, [], true}], []),
        ?assertEqual(present, list_member_absence(7, <<"500">>, State)),
        ?assertEqual({absent, Ref}, list_member_absence(8, <<"500">>, State))
    after
        guild_member_list_engine:destroy(Ref)
    end.

absence_is_present_without_store_test() ->
    ?assertEqual(present, list_member_absence(7, <<"500">>, #{})),
    ?assertEqual(present, list_member_absence(7, <<"0">>, #{})),
    ?assertEqual(present, list_member_absence(0, <<"500">>, #{})).

default_list_absence_reads_default_engine_test() ->
    Ref = guild_member_list_engine:new(),
    State = #{member_list_engine => Ref},
    try
        ?assertEqual({absent, Ref}, list_member_absence(7, <<"0">>, State)),
        ok = guild_member_list_engine:add_member(Ref, 7, <<"seven">>, [], true),
        ?assertEqual(present, list_member_absence(7, <<"0">>, State))
    after
        guild_member_list_engine:destroy(Ref)
    end.

sync_body_unchanged_requires_stable_ref_test() ->
    Ref = guild_member_list_engine:new(),
    Other = guild_member_list_engine:new(),
    State = engine_state(Ref),
    try
        ?assert(sync_body_unchanged(7, <<"500">>, {absent, Ref}, State)),
        ?assertNot(sync_body_unchanged(7, <<"500">>, present, State)),
        ?assertNot(sync_body_unchanged(7, <<"500">>, {absent, Other}, State)),
        ?assertNot(sync_body_unchanged(7, <<"500">>, {absent, Ref}, #{}))
    after
        guild_member_list_engine:destroy(Ref),
        guild_member_list_engine:destroy(Other)
    end.

sync_body_unchanged_false_when_member_reappears_test() ->
    Ref = guild_member_list_engine:new(),
    State = engine_state(Ref),
    try
        ok = guild_member_list_engine:add_member(Ref, 7, <<"seven">>, [], true),
        ?assertNot(sync_body_unchanged(7, <<"500">>, {absent, Ref}, State))
    after
        guild_member_list_engine:destroy(Ref)
    end.

user_change_absence_forces_sync_on_member_change_test() ->
    Ref = guild_member_list_engine:new(),
    State = engine_state(Ref),
    Member = #{<<"user">> => #{<<"id">> => <<"7">>}, <<"roles">> => []},
    try
        ?assertEqual({absent, Ref}, user_change_absence(7, <<"500">>, Member, Member, State)),
        ?assertEqual(present, user_change_absence(7, <<"500">>, Member, undefined, State)),
        ?assertEqual(present, user_change_absence(7, <<"500">>, undefined, Member, State)),
        ?assertEqual(
            present,
            user_change_absence(7, <<"500">>, Member, Member#{<<"nick">> => <<"x">>}, State)
        )
    after
        guild_member_list_engine:destroy(Ref)
    end.

subscribed_list_sync_is_skipped_only_for_absent_member_test() ->
    Ref = guild_member_list_engine:new(),
    Member = #{<<"user">> => #{<<"id">> => <<"7">>}, <<"roles">> => []},
    State = (engine_state(Ref))#{pending_member_list_sync_batch => #{pending_list_ids => #{}}},
    try
        with_batch_window(false, fun() ->
            ?assert(
                maps:is_key(
                    pending_member_list_sync_batch,
                    dispatch_user_change_to_subscribed_list(7, Member, Member, <<"500">>, State)
                )
            ),
            ok = guild_member_list_engine:add_member(Ref, 7, <<"seven">>, [], true),
            ?assertNot(
                maps:is_key(
                    pending_member_list_sync_batch,
                    dispatch_user_change_to_subscribed_list(7, Member, Member, <<"500">>, State)
                )
            )
        end)
    after
        guild_member_list_engine:destroy(Ref)
    end.

list_id_fold_matches_fold_lists_test() ->
    Tab = guild_member_list_subs:new(),
    try
        guild_member_list_subs:subscribe(<<"s1">>, <<"500">>, [{0, 99}], Tab),
        guild_member_list_subs:subscribe(<<"s2">>, <<"500">>, [{0, 9}], Tab),
        guild_member_list_subs:subscribe(<<"s1">>, <<"600">>, [{0, 99}], Tab),
        Sizes = guild_member_list_subs:fold_lists(
            fun(ListId, ListSubs, Acc) -> Acc#{ListId => map_size(ListSubs)} end,
            #{},
            Tab
        ),
        ?assertEqual(lists:sort(maps:keys(Sizes)), guild_member_list_subs:list_ids(Tab)),
        ?assertEqual([], [ListId || {ListId, 0} <- maps:to_list(Sizes)])
    after
        guild_member_list_subs:destroy(Tab)
    end.

connection_sync_delay_defaults_test() ->
    ?assertEqual(250, connection_sync_delay_ms(#{})),
    ?assertEqual(250, connection_sync_delay_ms(#{member_count => 4999})),
    ?assertEqual(250, connection_sync_delay_ms(#{member_count => undefined})),
    ?assertEqual(1000, connection_sync_delay_ms(#{member_count => 5000})),
    ?assertEqual(1000, connection_sync_delay_ms(#{member_count => 60000})).

connection_sync_delay_reads_env_test() ->
    with_env(member_list_connection_sync_delay_ms, 40, fun() ->
        ?assertEqual(40, connection_sync_delay_ms(#{member_count => 10}))
    end),
    with_env(member_list_large_connection_sync_delay_ms, 90, fun() ->
        ?assertEqual(90, connection_sync_delay_ms(#{member_count => 5000}))
    end).

connection_sync_delay_rejects_invalid_env_test() ->
    lists:foreach(
        fun(Invalid) ->
            with_env(member_list_connection_sync_delay_ms, Invalid, fun() ->
                ?assertEqual(250, connection_sync_delay_ms(#{}))
            end)
        end,
        [0, -1, undefined, 250.0, <<"250">>]
    ).

connection_list_sync_arms_one_timer_per_window_test() ->
    State1 = queue_connection_list_sync(<<"500">>, #{}),
    Batch1 = maps:get(pending_member_list_sync_batch, State1),
    TimerRef = maps:get(timer_ref, Batch1),
    try
        ?assert(is_reference(TimerRef)),
        ?assertEqual(#{<<"500">> => true}, maps:get(pending_list_ids, Batch1)),
        State2 = queue_connection_list_sync(<<"600">>, State1),
        Batch2 = maps:get(pending_member_list_sync_batch, State2),
        ?assertEqual(TimerRef, maps:get(timer_ref, Batch2)),
        ?assertEqual(
            #{<<"500">> => true, <<"600">> => true}, maps:get(pending_list_ids, Batch2)
        ),
        State3 = queue_connection_list_sync(<<"500">>, State2),
        ?assertEqual(Batch2, maps:get(pending_member_list_sync_batch, State3))
    after
        _ = erlang:cancel_timer(TimerRef)
    end.

connection_change_sync_is_debounced_test() ->
    Ref = guild_member_list_engine:new(),
    try
        ok = guild_member_list_engine:add_member(Ref, 7, <<"seven">>, [], true),
        Next = sync_connection_change_for_subscribed_list(
            1, 7, <<"500">>, #{}, engine_state(Ref)
        ),
        Batch = maps:get(pending_member_list_sync_batch, Next),
        ?assertEqual(#{<<"500">> => true}, maps:get(pending_list_ids, Batch)),
        _ = erlang:cancel_timer(maps:get(timer_ref, Batch))
    after
        guild_member_list_engine:destroy(Ref)
    end.

connection_change_skip_absent_arms_no_timer_test() ->
    Ref = guild_member_list_engine:new(),
    try
        Next = sync_connection_change_for_subscribed_list(
            1, 7, <<"500">>, #{}, engine_state(Ref)
        ),
        ?assertNot(maps:is_key(pending_member_list_sync_batch, Next)),
        ?assertEqual(1, maps:get(member_list_sync_skipped_absent, Next))
    after
        guild_member_list_engine:destroy(Ref)
    end.

connection_change_skip_absent_keeps_pending_batch_test() ->
    Ref = guild_member_list_engine:new(),
    TimerRef = erlang:send_after(60000, self(), flush_member_list_sync_batch),
    Batch = #{pending_list_ids => #{<<"600">> => true}, timer_ref => TimerRef},
    State = (engine_state(Ref))#{pending_member_list_sync_batch => Batch},
    try
        Next = sync_connection_change_for_subscribed_list(1, 7, <<"500">>, #{}, State),
        ?assertEqual(Batch, maps:get(pending_member_list_sync_batch, Next))
    after
        _ = erlang:cancel_timer(TimerRef),
        guild_member_list_engine:destroy(Ref)
    end.

member_update_sync_stays_immediate_test() ->
    Ref = guild_member_list_engine:new(),
    TimerRef = erlang:send_after(60000, self(), flush_member_list_sync_batch),
    Member = #{<<"user">> => #{<<"id">> => <<"7">>}, <<"roles">> => []},
    State = (engine_state(Ref))#{
        pending_member_list_sync_batch => #{
            pending_list_ids => #{<<"600">> => true},
            timer_ref => TimerRef
        }
    },
    try
        ok = guild_member_list_engine:add_member(Ref, 7, <<"seven">>, [], true),
        ?assertNot(
            maps:is_key(
                pending_member_list_sync_batch,
                dispatch_user_change_to_subscribed_list(7, Member, Member, <<"500">>, State)
            )
        )
    after
        _ = erlang:cancel_timer(TimerRef),
        guild_member_list_engine:destroy(Ref)
    end.

engine_state(Ref) ->
    #{channel_member_list_engines => #{<<"500">> => Ref}, member_presence => #{}}.

with_batch_window(Value, Fun) ->
    with_env(guild_member_list_sync_batch_window, Value, Fun).

with_env(Key, Value, Fun) ->
    Previous = application:get_env(fluxer_gateway, Key),
    application:set_env(fluxer_gateway, Key, Value),
    try
        Fun()
    after
        restore_env(Key, Previous)
    end.

restore_env(Key, {ok, Previous}) ->
    application:set_env(fluxer_gateway, Key, Previous);
restore_env(Key, undefined) ->
    application:unset_env(fluxer_gateway, Key).

-endif.
