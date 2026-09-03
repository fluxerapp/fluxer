%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_subscription_handler).
-typing([eqwalizer]).

-export([
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type guild_state() :: map().
-type user_id() :: integer().
-type session_id() :: binary().
-type channel_id() :: integer().
-type lazy_subscribe_key() :: {session_id(), channel_id()}.
-export_type([guild_state/0]).

-define(LAZY_SUBSCRIBE_COALESCE_MS, 100).
-define(MAX_BUFFERED_LAZY_SUBSCRIBE_RANGES, 10).
-define(ENGINES_KEY, channel_member_list_engines).
-define(DISPATCHED_KEY, lazy_subscribe_dispatched).

-spec handle_call(term(), gen_server:from(), guild_state()) ->
    {reply, term(), guild_state()}.
handle_call({lazy_subscribe, Request}, _From, State) when is_map(Request) ->
    NewState = buffer_lazy_subscribe(Request, State),
    {reply, ok, NewState};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), guild_state()) -> {noreply, guild_state()}.
handle_cast({update_member_subscriptions, SessionId, MemberIds}, State) when
    is_binary(SessionId), is_list(MemberIds)
->
    NewState = handle_update_member_subscriptions(SessionId, filter_user_ids(MemberIds), State),
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), guild_state()) -> {noreply, guild_state()}.
handle_info(flush_lazy_subscribe_buffer, State) ->
    NewState = flush_lazy_subscribe_buffer(State),
    {noreply, NewState}.

-spec buffer_lazy_subscribe(map(), guild_state()) -> guild_state().
buffer_lazy_subscribe(Request, State) ->
    case dispatch_immediately(Request, State) of
        true -> dispatch_lazy_subscribe_now(Request, State);
        false -> enqueue_lazy_subscribe(Request, State)
    end.

-spec dispatch_immediately(map(), guild_state()) -> boolean().
dispatch_immediately(Request, State) ->
    maps:get(lazy_subscribe_timer, State, undefined) =:= undefined andalso
        map_size(maps:get(lazy_subscribe_buffer, State, #{})) =:= 0 andalso
        engine_already_built(Request, State).

-spec engine_already_built(map(), guild_state()) -> boolean().
engine_already_built(#{channel_id := ChannelId}, State) ->
    case guild_member_list:calculate_list_id(ChannelId, State) of
        ListId when is_binary(ListId) ->
            maps:is_key(ListId, maps:get(?ENGINES_KEY, State, #{}));
        _ ->
            false
    end;
engine_already_built(_Request, _State) ->
    false.

%% A leading-edge dispatch still buffers its request, marked as dispatched, so a second
%% subscribe for the same key inside the window merges into it exactly as it did before
%% the leading edge existed. The mark only survives while nothing merges in, because every
%% merge_lazy_subscribe_request/2 clause returns a map derived from the incoming request.
-spec dispatch_lazy_subscribe_now(map(), guild_state()) -> guild_state().
dispatch_lazy_subscribe_now(Request, State) ->
    try process_lazy_subscribe(Request, State) of
        NewState -> enqueue_lazy_subscribe(mark_lazy_subscribe_dispatched(Request), NewState)
    catch
        Class:Reason:Stack ->
            log_lazy_subscribe_dispatch_error(Request, Class, Reason, Stack),
            State
    end.

-spec mark_lazy_subscribe_dispatched(map()) -> map().
mark_lazy_subscribe_dispatched(Request) ->
    Request#{?DISPATCHED_KEY => true}.

-spec log_lazy_subscribe_dispatch_error(map(), atom(), term(), list()) -> ok.
log_lazy_subscribe_dispatch_error(Request, Class, Reason, Stack) ->
    logger:warning(
        "guild_lazy_subscribe_dispatch_failed: session_id=~p channel_id=~p error=~p:~p ~p",
        [
            maps:get(session_id, Request, undefined),
            maps:get(channel_id, Request, undefined),
            Class,
            Reason,
            Stack
        ]
    ).

-spec arm_lazy_subscribe_timer(guild_state()) -> guild_state().
arm_lazy_subscribe_timer(State) ->
    case maps:get(lazy_subscribe_timer, State, undefined) of
        undefined ->
            Ref = erlang:send_after(
                ?LAZY_SUBSCRIBE_COALESCE_MS, self(), flush_lazy_subscribe_buffer
            ),
            State#{lazy_subscribe_timer => Ref};
        _ ->
            State
    end.

-spec enqueue_lazy_subscribe(map(), guild_state()) -> guild_state().
enqueue_lazy_subscribe(Request, State) ->
    #{session_id := SessionId, channel_id := ChannelId} = Request,
    BufferKey = {SessionId, ChannelId},
    Buffer = maps:get(lazy_subscribe_buffer, State, #{}),
    Order = maps:get(lazy_subscribe_order, State, []),
    BufferedRequest = merge_lazy_subscribe_request(
        maps:get(BufferKey, Buffer, undefined), Request
    ),
    NewBuffer = Buffer#{BufferKey => BufferedRequest},
    NewOrder = move_buffer_key_to_tail(BufferKey, Order),
    TimerRef = maps:get(lazy_subscribe_timer, State, undefined),
    NewState = State#{lazy_subscribe_buffer => NewBuffer, lazy_subscribe_order => NewOrder},
    case TimerRef of
        undefined -> arm_lazy_subscribe_timer(NewState);
        _ -> NewState
    end.

-spec merge_lazy_subscribe_request(map() | undefined, map()) -> map().
merge_lazy_subscribe_request(undefined, Request) ->
    Request;
merge_lazy_subscribe_request(#{ranges := _ExistingRanges}, #{ranges := []} = Request) ->
    Request;
merge_lazy_subscribe_request(#{ranges := []}, #{ranges := _Ranges} = Request) ->
    Request;
merge_lazy_subscribe_request(
    #{ranges := ExistingRanges}, #{ranges := Ranges} = Request
) when
    is_list(ExistingRanges), is_list(Ranges)
->
    Request#{ranges := merge_lazy_subscribe_ranges(ExistingRanges, Ranges)};
merge_lazy_subscribe_request(_ExistingRequest, Request) ->
    Request.

-spec merge_lazy_subscribe_ranges(
    [guild_member_list:range()], [guild_member_list:range()]
) -> [guild_member_list:range()].
merge_lazy_subscribe_ranges(ExistingRanges, Ranges) ->
    limit_lazy_subscribe_ranges(
        guild_member_list:normalize_ranges(ExistingRanges ++ Ranges), Ranges
    ).

-spec limit_lazy_subscribe_ranges(
    [guild_member_list:range()], [guild_member_list:range()]
) -> [guild_member_list:range()].
limit_lazy_subscribe_ranges(MergedRanges, Ranges) ->
    case length(MergedRanges) > ?MAX_BUFFERED_LAZY_SUBSCRIBE_RANGES of
        true ->
            lists:sublist(
                guild_member_list:normalize_ranges(Ranges),
                ?MAX_BUFFERED_LAZY_SUBSCRIBE_RANGES
            );
        false ->
            MergedRanges
    end.

-spec flush_lazy_subscribe_buffer(guild_state()) -> guild_state().
flush_lazy_subscribe_buffer(State) ->
    Buffer = maps:get(lazy_subscribe_buffer, State, #{}),
    Order = ordered_lazy_subscribe_keys(State, Buffer),
    State1 = maps:remove(lazy_subscribe_buffer, State),
    State2 = maps:remove(lazy_subscribe_order, State1),
    State3 = maps:remove(lazy_subscribe_timer, State2),
    lists:foldl(
        fun(BufferKey, AccState) ->
            process_buffered_lazy_subscribe(BufferKey, Buffer, AccState)
        end,
        State3,
        Order
    ).

-spec move_buffer_key_to_tail(lazy_subscribe_key(), [lazy_subscribe_key()]) ->
    [lazy_subscribe_key()].
move_buffer_key_to_tail(BufferKey, Order) ->
    [Key || Key <- Order, Key =/= BufferKey] ++ [BufferKey].

-spec ordered_lazy_subscribe_keys(guild_state(), map()) -> [lazy_subscribe_key()].
ordered_lazy_subscribe_keys(State, Buffer) ->
    case maps:get(lazy_subscribe_order, State, undefined) of
        Order when is_list(Order) ->
            [Key || Key <- Order, maps:is_key(Key, Buffer)];
        _ ->
            maps:keys(Buffer)
    end.

-spec process_buffered_lazy_subscribe(lazy_subscribe_key(), map(), guild_state()) ->
    guild_state().
process_buffered_lazy_subscribe(BufferKey, Buffer, State) ->
    case maps:find(BufferKey, Buffer) of
        {ok, #{?DISPATCHED_KEY := true}} ->
            State;
        {ok, Request} ->
            process_lazy_subscribe(Request, State);
        error ->
            State
    end.

-spec process_lazy_subscribe(map(), guild_state()) -> guild_state().
process_lazy_subscribe(Request, State) ->
    #{session_id := SessionId, channel_id := ChannelId, ranges := Ranges} = Request,
    case should_ignore_member_list_subscribe(Ranges, State) of
        true ->
            State;
        false ->
            do_process_lazy_subscribe(SessionId, ChannelId, Ranges, State)
    end.

-spec do_process_lazy_subscribe(session_id(), channel_id(), list(), guild_state()) ->
    guild_state().
do_process_lazy_subscribe(SessionId, ChannelId, Ranges, State) ->
    Sessions0 = maps:get(sessions, State, #{}),
    SessionUserId = get_session_user_id(SessionId, Sessions0),
    case maps:get(id, State, undefined) of
        GuildId when is_integer(GuildId) ->
            process_lazy_subscribe_for_guild(
                GuildId, ChannelId, SessionId, SessionUserId, Ranges, State
            );
        _ ->
            State
    end.

-spec process_lazy_subscribe_for_guild(
    integer(),
    channel_id(),
    session_id(),
    user_id() | undefined,
    list(),
    guild_state()
) -> guild_state().
process_lazy_subscribe_for_guild(
    GuildId,
    ChannelId,
    SessionId,
    SessionUserId,
    Ranges,
    State
) ->
    CanView =
        is_integer(SessionUserId) andalso
            guild_permissions:can_view_channel(
                SessionUserId, ChannelId, undefined, State
            ) andalso
            guild_permissions:can_view_channel_members(
                SessionUserId, ChannelId, undefined, State
            ),
    case CanView of
        true ->
            ListId = guild_member_list:calculate_list_id(ChannelId, State),
            subscribe_member_list_ranges(
                ListId, GuildId, ChannelId, SessionId, Ranges, State
            );
        false ->
            State
    end.

-spec subscribe_member_list_ranges(
    guild_member_list:list_id() | undefined,
    integer(),
    channel_id(),
    session_id(),
    list(),
    guild_state()
) -> guild_state().
subscribe_member_list_ranges(undefined, _GuildId, _ChannelId, _SessionId, _Ranges, State) ->
    State;
subscribe_member_list_ranges(ListId, GuildId, ChannelId, SessionId, Ranges, State) ->
    {NewState, ShouldSendSync, NormalizedRanges} =
        guild_member_list:subscribe_ranges(SessionId, ListId, Ranges, State),
    process_lazy_subscribe_sync(
        ShouldSendSync, NormalizedRanges, GuildId, ListId, ChannelId, SessionId, NewState
    ).

-spec should_ignore_member_list_subscribe(list(), guild_state()) -> boolean().
should_ignore_member_list_subscribe([], _State) ->
    false;
should_ignore_member_list_subscribe(_Ranges, State) ->
    not guild_dispatch:is_member_list_updates_enabled(State).

-spec process_lazy_subscribe_sync(
    boolean(),
    list(),
    integer(),
    guild_member_list:list_id(),
    channel_id(),
    session_id(),
    guild_state()
) ->
    guild_state().
process_lazy_subscribe_sync(true, [], _GuildId, _ListId, _ChannelId, _SessionId, State) ->
    State;
process_lazy_subscribe_sync(true, RangesToSend, GuildId, ListId, ChannelId, SessionId, State) ->
    SyncResponse = guild_member_list:build_sync_response(GuildId, ListId, RangesToSend, State),
    dispatch_lazy_subscribe_sync(SyncResponse, ChannelId, GuildId, SessionId, State);
process_lazy_subscribe_sync(_, _, _GuildId, _ListId, _ChannelId, _SessionId, State) ->
    State.

-spec dispatch_lazy_subscribe_sync(map(), channel_id(), integer(), session_id(), guild_state()) ->
    guild_state().
dispatch_lazy_subscribe_sync(SyncResponse, ChannelId, GuildId, SessionId, State) ->
    SyncResponseWithChannel =
        case maps:is_key(<<"channel_id">>, SyncResponse) of
            true -> SyncResponse;
            false -> SyncResponse#{<<"channel_id">> => integer_to_binary(ChannelId)}
        end,
    Sessions = maps:get(sessions, State, #{}),
    case maps:get(SessionId, Sessions, undefined) of
        #{pid := SessionPid} when is_pid(SessionPid) ->
            gateway_dispatch_relay:dispatch(
                SessionPid, guild_member_list_update, SyncResponseWithChannel, GuildId
            );
        _ ->
            ok
    end,
    State.

-spec get_session_user_id(session_id(), map()) -> user_id() | undefined.
get_session_user_id(SessionId, Sessions) ->
    case maps:get(SessionId, Sessions, undefined) of
        #{user_id := Uid} -> Uid;
        _ -> undefined
    end.

-spec handle_update_member_subscriptions(session_id(), [user_id()], guild_state()) ->
    guild_state().
handle_update_member_subscriptions(SessionId, MemberIds, State) ->
    case snowflake_id:parse_optional(maps:get(id, State, undefined)) of
        GuildId when is_integer(GuildId), GuildId > 0 ->
            handle_update_member_subscriptions_local(GuildId, SessionId, MemberIds, State);
        _ ->
            State
    end.

-spec filter_user_ids([term()]) -> [user_id()].
filter_user_ids(UserIds) ->
    [UserId || UserId <- UserIds, is_integer(UserId)].

-spec handle_update_member_subscriptions_local(
    integer(), session_id(), [user_id()], guild_state()
) -> guild_state().
handle_update_member_subscriptions_local(GuildId, SessionId, MemberIds, State) ->
    MemberSubs = maps:get(member_subscriptions, State, guild_subscriptions:init_state()),
    Sessions = maps:get(sessions, State, #{}),
    SessionUserId = get_session_user_id(SessionId, Sessions),
    FilteredMemberIds = filter_member_ids_for_subscription(
        GuildId, SessionUserId, MemberIds, State
    ),
    {NewMemberSubs, Added, Removed} = guild_subscriptions:update_subscriptions_with_delta(
        SessionId, FilteredMemberIds, MemberSubs
    ),
    State1 = State#{member_subscriptions => NewMemberSubs},
    State2 = handle_added_subscriptions(Added, SessionId, State1),
    handle_removed_subscriptions(Removed, State2).

-spec filter_member_ids_for_subscription(
    integer(), user_id() | undefined, [user_id()], guild_state()
) ->
    [user_id()].
filter_member_ids_for_subscription(_GuildId, undefined, _MemberIds, _State) ->
    [];
filter_member_ids_for_subscription(_GuildId, SessionUserId, MemberIds, State) ->
    guild_subscription_mutual_channels:filter_member_ids(SessionUserId, MemberIds, State).

-spec handle_added_subscriptions([user_id()], session_id(), guild_state()) -> guild_state().
handle_added_subscriptions(Added, SessionId, State) ->
    lists:foldl(
        fun(UserId, Acc) ->
            StateWithPresence = guild_sessions:subscribe_to_user_presence(UserId, Acc),
            guild_presence:send_cached_presence_to_session(UserId, SessionId, StateWithPresence)
        end,
        State,
        Added
    ).

-spec handle_removed_subscriptions([user_id()], guild_state()) -> guild_state().
handle_removed_subscriptions(Removed, State) ->
    lists:foldl(
        fun guild_sessions:unsubscribe_from_user_presence/2,
        State,
        Removed
    ).

-ifdef(TEST).

-spec disabled_operations_state(integer() | binary()) -> guild_state().
disabled_operations_state(Value) ->
    #{data => #{<<"guild">> => #{<<"disabled_operations">> => Value}}}.

should_ignore_member_list_subscribe_ignores_non_empty_ranges_when_disabled_test() ->
    ?assertEqual(
        true,
        should_ignore_member_list_subscribe(
            [{0, 99}],
            disabled_operations_state(1 bsl 6)
        )
    ).

should_ignore_member_list_subscribe_allows_empty_ranges_when_disabled_test() ->
    ?assertEqual(
        false,
        should_ignore_member_list_subscribe(
            [],
            disabled_operations_state(1 bsl 6)
        )
    ).

buffer_lazy_subscribe_creates_buffer_entry_test() ->
    State = #{},
    Request = #{session_id => <<"s1">>, channel_id => 500, ranges => [{0, 99}]},
    NewState = buffer_lazy_subscribe(Request, State),
    Buffer = maps:get(lazy_subscribe_buffer, NewState),
    ?assertEqual(Request, maps:get({<<"s1">>, 500}, Buffer)),
    ?assertEqual([{<<"s1">>, 500}], maps:get(lazy_subscribe_order, NewState)),
    ?assertNotEqual(undefined, maps:get(lazy_subscribe_timer, NewState, undefined)).

buffer_lazy_subscribe_merges_older_request_ranges_test() ->
    State = #{},
    Request1 = #{session_id => <<"s1">>, channel_id => 500, ranges => [{0, 1}]},
    State1 = buffer_lazy_subscribe(Request1, State),
    Request2 = #{session_id => <<"s1">>, channel_id => 500, ranges => [{2, 99}]},
    State2 = buffer_lazy_subscribe(Request2, State1),
    Buffer = maps:get(lazy_subscribe_buffer, State2),
    ?assertEqual(Request2#{ranges := [{0, 99}]}, maps:get({<<"s1">>, 500}, Buffer)),
    ?assertEqual(1, map_size(Buffer)),
    ?assertEqual([{<<"s1">>, 500}], maps:get(lazy_subscribe_order, State2)).

buffer_lazy_subscribe_empty_ranges_replace_buffered_subscribe_test() ->
    State = #{},
    Request1 = #{session_id => <<"s1">>, channel_id => 500, ranges => [{0, 99}]},
    State1 = buffer_lazy_subscribe(Request1, State),
    Request2 = #{session_id => <<"s1">>, channel_id => 500, ranges => []},
    State2 = buffer_lazy_subscribe(Request2, State1),
    Buffer = maps:get(lazy_subscribe_buffer, State2),
    ?assertEqual(Request2, maps:get({<<"s1">>, 500}, Buffer)),
    ?assertEqual(1, map_size(Buffer)),
    ?assertEqual([{<<"s1">>, 500}], maps:get(lazy_subscribe_order, State2)).

buffer_lazy_subscribe_keeps_separate_sessions_test() ->
    State = #{},
    Request1 = #{session_id => <<"s1">>, channel_id => 500, ranges => [{0, 99}]},
    State1 = buffer_lazy_subscribe(Request1, State),
    Request2 = #{session_id => <<"s2">>, channel_id => 500, ranges => [{0, 50}]},
    State2 = buffer_lazy_subscribe(Request2, State1),
    Buffer = maps:get(lazy_subscribe_buffer, State2),
    ?assertEqual(2, map_size(Buffer)),
    ?assertEqual(Request1, maps:get({<<"s1">>, 500}, Buffer)),
    ?assertEqual(Request2, maps:get({<<"s2">>, 500}, Buffer)),
    ?assertEqual([{<<"s1">>, 500}, {<<"s2">>, 500}], maps:get(lazy_subscribe_order, State2)).

buffer_lazy_subscribe_moves_replaced_key_to_tail_test() ->
    State = #{},
    Request1 = #{session_id => <<"s1">>, channel_id => 500, ranges => [{0, 99}]},
    State1 = buffer_lazy_subscribe(Request1, State),
    Request2 = #{session_id => <<"s1">>, channel_id => 600, ranges => [{0, 99}]},
    State2 = buffer_lazy_subscribe(Request2, State1),
    Request3 = #{session_id => <<"s1">>, channel_id => 500, ranges => [{100, 199}]},
    State3 = buffer_lazy_subscribe(Request3, State2),
    ?assertEqual([{<<"s1">>, 600}, {<<"s1">>, 500}], maps:get(lazy_subscribe_order, State3)),
    ?assertEqual(
        [{<<"s1">>, 600}, {<<"s1">>, 500}],
        ordered_lazy_subscribe_keys(State3, maps:get(lazy_subscribe_buffer, State3))
    ).

buffer_lazy_subscribe_keeps_newest_ranges_over_limit_test() ->
    Existing = [{Start * 200, Start * 200 + 99} || Start <- lists:seq(0, 9)],
    Request1 = #{session_id => <<"s1">>, channel_id => 500, ranges => Existing},
    State1 = buffer_lazy_subscribe(Request1, #{}),
    Request2 = #{session_id => <<"s1">>, channel_id => 500, ranges => [{0, 99}, {5000, 5099}]},
    State2 = buffer_lazy_subscribe(Request2, State1),
    Buffer = maps:get(lazy_subscribe_buffer, State2),
    ?assertEqual(
        Request2#{ranges := [{0, 99}, {5000, 5099}]}, maps:get({<<"s1">>, 500}, Buffer)
    ).

buffer_lazy_subscribe_caps_merged_ranges_test() ->
    Existing = [{Start * 200, Start * 200 + 99} || Start <- lists:seq(0, 9)],
    Request1 = #{session_id => <<"s1">>, channel_id => 500, ranges => Existing},
    State1 = buffer_lazy_subscribe(Request1, #{}),
    Ranges2 = [{Start * 200, Start * 200 + 99} || Start <- lists:seq(20, 30)],
    Request2 = #{session_id => <<"s1">>, channel_id => 500, ranges => Ranges2},
    State2 = buffer_lazy_subscribe(Request2, State1),
    Buffer = maps:get(lazy_subscribe_buffer, State2),
    #{ranges := Merged} = maps:get({<<"s1">>, 500}, Buffer),
    ?assertEqual(?MAX_BUFFERED_LAZY_SUBSCRIBE_RANGES, length(Merged)),
    ?assertEqual(lists:sublist(Ranges2, ?MAX_BUFFERED_LAZY_SUBSCRIBE_RANGES), Merged).

flush_lazy_subscribe_buffer_clears_state_test() ->
    Buffer = #{{<<"s1">>, 500} => #{session_id => <<"s1">>, channel_id => 500, ranges => []}},
    State = #{lazy_subscribe_buffer => Buffer, lazy_subscribe_timer => make_ref()},
    NewState = flush_lazy_subscribe_buffer(State),
    ?assertEqual(error, maps:find(lazy_subscribe_buffer, NewState)),
    ?assertEqual(error, maps:find(lazy_subscribe_order, NewState)),
    ?assertEqual(error, maps:find(lazy_subscribe_timer, NewState)).

warm_list_state(ChannelId) ->
    ListId = integer_to_binary(ChannelId),
    #{
        data => #{
            <<"channel_index">> => #{
                ChannelId => #{<<"id">> => ListId}
            }
        },
        ?ENGINES_KEY => #{ListId => warm_list_state_engine_ref}
    }.

engine_already_built_detects_built_engine_test() ->
    Request = #{session_id => <<"s1">>, channel_id => 500, ranges => [{0, 99}]},
    ?assertEqual(true, engine_already_built(Request, warm_list_state(500))).

engine_already_built_false_without_engine_test() ->
    Request = #{session_id => <<"s1">>, channel_id => 500, ranges => [{0, 99}]},
    State = maps:put(?ENGINES_KEY, #{}, warm_list_state(500)),
    ?assertEqual(false, engine_already_built(Request, State)).

engine_already_built_false_for_unknown_channel_test() ->
    Request = #{session_id => <<"s1">>, channel_id => 999, ranges => [{0, 99}]},
    ?assertEqual(false, engine_already_built(Request, warm_list_state(500))).

engine_already_built_false_on_bare_state_test() ->
    Request = #{session_id => <<"s1">>, channel_id => 500, ranges => [{0, 99}]},
    ?assertEqual(false, engine_already_built(Request, #{})).

dispatch_immediately_requires_idle_window_test() ->
    Request = #{session_id => <<"s1">>, channel_id => 500, ranges => [{0, 99}]},
    Warm = warm_list_state(500),
    ?assertEqual(true, dispatch_immediately(Request, Warm)),
    ?assertEqual(
        false, dispatch_immediately(Request, Warm#{lazy_subscribe_timer => make_ref()})
    ),
    ?assertEqual(
        false,
        dispatch_immediately(Request, Warm#{
            lazy_subscribe_buffer => #{{<<"s2">>, 501} => Request}
        })
    ).

warm_subscribe_records_the_dispatched_request_test() ->
    Request = #{session_id => <<"s1">>, channel_id => 500, ranges => [{0, 99}]},
    NewState = buffer_lazy_subscribe(Request, warm_list_state(500)),
    Buffer = maps:get(lazy_subscribe_buffer, NewState),
    ?assertEqual(Request#{?DISPATCHED_KEY => true}, maps:get({<<"s1">>, 500}, Buffer)),
    ?assertEqual([{<<"s1">>, 500}], maps:get(lazy_subscribe_order, NewState)),
    ?assertNotEqual(undefined, maps:get(lazy_subscribe_timer, NewState, undefined)).

subscribe_behind_a_warm_dispatch_is_coalesced_test() ->
    Request1 = #{session_id => <<"s1">>, channel_id => 500, ranges => [{0, 99}]},
    State1 = buffer_lazy_subscribe(Request1, warm_list_state(500)),
    Request2 = #{session_id => <<"s2">>, channel_id => 500, ranges => [{0, 99}]},
    State2 = buffer_lazy_subscribe(Request2, State1),
    Buffer = maps:get(lazy_subscribe_buffer, State2),
    ?assertEqual(Request2, maps:get({<<"s2">>, 500}, Buffer)),
    ?assertEqual(Request1#{?DISPATCHED_KEY => true}, maps:get({<<"s1">>, 500}, Buffer)),
    ?assertEqual(2, map_size(Buffer)).

%% Reference oracle: what enqueue_lazy_subscribe/2 buffered for a repeated
%% {session, channel} before the leading-edge dispatch existed.
reference_coalesced_request(Request1, Request2) ->
    merge_lazy_subscribe_request(Request1, Request2).

repeated_warm_subscribe_merges_like_the_pre_leading_edge_path_test() ->
    Request1 = #{session_id => <<"s1">>, channel_id => 500, ranges => [{0, 99}]},
    Request2 = #{session_id => <<"s1">>, channel_id => 500, ranges => [{100, 199}]},
    State1 = buffer_lazy_subscribe(Request1, warm_list_state(500)),
    State2 = buffer_lazy_subscribe(Request2, State1),
    Buffer = maps:get(lazy_subscribe_buffer, State2),
    Merged = maps:get({<<"s1">>, 500}, Buffer),
    ?assertEqual(1, map_size(Buffer)),
    ?assertEqual(reference_coalesced_request(Request1, Request2), Merged),
    ?assertNot(maps:is_key(?DISPATCHED_KEY, Merged)),
    ?assertEqual([{<<"s1">>, 500}], maps:get(lazy_subscribe_order, State2)).

repeated_warm_subscribe_with_empty_ranges_replaces_test() ->
    Request1 = #{session_id => <<"s1">>, channel_id => 500, ranges => [{0, 99}]},
    Request2 = #{session_id => <<"s1">>, channel_id => 500, ranges => []},
    State1 = buffer_lazy_subscribe(Request1, warm_list_state(500)),
    State2 = buffer_lazy_subscribe(Request2, State1),
    Merged = maps:get({<<"s1">>, 500}, maps:get(lazy_subscribe_buffer, State2)),
    ?assertEqual(reference_coalesced_request(Request1, Request2), Merged),
    ?assertNot(maps:is_key(?DISPATCHED_KEY, Merged)).

dispatched_entry_state() ->
    Key = {<<"s1">>, 500},
    Dispatched = #{session_id => <<"s1">>, channel_id => 500, ?DISPATCHED_KEY => true},
    State = #{
        lazy_subscribe_buffer => #{Key => Dispatched},
        lazy_subscribe_order => [Key],
        lazy_subscribe_timer => make_ref()
    },
    {Key, Dispatched, State}.

flush_skips_an_already_dispatched_entry_test() ->
    {_Key, _Dispatched, State} = dispatched_entry_state(),
    NewState = flush_lazy_subscribe_buffer(State),
    ?assertEqual(error, maps:find(lazy_subscribe_buffer, NewState)),
    ?assertEqual(error, maps:find(lazy_subscribe_order, NewState)),
    ?assertEqual(error, maps:find(lazy_subscribe_timer, NewState)).

flush_processes_the_same_entry_once_unmarked_test() ->
    {Key, Dispatched, State} = dispatched_entry_state(),
    Unmarked = maps:remove(?DISPATCHED_KEY, Dispatched),
    ?assertError(
        {badmatch, _},
        flush_lazy_subscribe_buffer(State#{lazy_subscribe_buffer => #{Key => Unmarked}})
    ).

failed_warm_dispatch_is_not_re_enqueued_test() ->
    Warm = warm_list_state(500),
    Broken = #{session_id => <<"s1">>, channel_id => 500},
    ?assertEqual(Warm, buffer_lazy_subscribe(Broken, Warm)).

failed_warm_dispatch_does_not_escape_handle_call_test() ->
    Warm = warm_list_state(500),
    Broken = #{session_id => <<"s1">>, channel_id => 500},
    From = {self(), make_ref()},
    ?assertEqual({reply, ok, Warm}, handle_call({lazy_subscribe, Broken}, From, Warm)).

cold_list_still_uses_the_coalesce_buffer_test() ->
    Request = #{session_id => <<"s1">>, channel_id => 500, ranges => [{0, 99}]},
    State = maps:put(?ENGINES_KEY, #{}, warm_list_state(500)),
    NewState = buffer_lazy_subscribe(Request, State),
    Buffer = maps:get(lazy_subscribe_buffer, NewState),
    ?assertEqual(Request, maps:get({<<"s1">>, 500}, Buffer)).

arm_lazy_subscribe_timer_is_idempotent_test() ->
    Ref = make_ref(),
    State = #{lazy_subscribe_timer => Ref},
    ?assertEqual(Ref, maps:get(lazy_subscribe_timer, arm_lazy_subscribe_timer(State))).

-endif.
