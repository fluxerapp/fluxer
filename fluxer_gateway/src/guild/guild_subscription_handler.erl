%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_subscription_handler).

-typing([eqwalizer]).

-export([
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

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
    flush_lazy_subscribe_keys(Order, Buffer, State3).

-spec flush_lazy_subscribe_keys([lazy_subscribe_key()], map(), guild_state()) -> guild_state().
flush_lazy_subscribe_keys([], _Buffer, State) ->
    State;
flush_lazy_subscribe_keys([BufferKey | Rest], Buffer, State) ->
    Engines = map_size(maps:get(?ENGINES_KEY, State, #{})),
    State1 = process_buffered_lazy_subscribe(BufferKey, Buffer, State),
    case Rest =/= [] andalso map_size(maps:get(?ENGINES_KEY, State1, #{})) > Engines of
        true -> defer_lazy_subscribe_keys(Rest, Buffer, State1);
        false -> flush_lazy_subscribe_keys(Rest, Buffer, State1)
    end.

-spec defer_lazy_subscribe_keys([lazy_subscribe_key()], map(), guild_state()) -> guild_state().
defer_lazy_subscribe_keys(Keys, Buffer, State) ->
    Ref = erlang:send_after(0, self(), flush_lazy_subscribe_buffer),
    State#{
        lazy_subscribe_buffer => maps:with(Keys, Buffer),
        lazy_subscribe_order => Keys,
        lazy_subscribe_timer => Ref
    }.

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
            guild_permissions_check:can_view_channel(
                SessionUserId, ChannelId, undefined, State
            ) andalso
            guild_permissions_check:can_view_channel_members(
                SessionUserId, ChannelId, undefined, State
            ) andalso
            member_list_channel_allowed(SessionId, ChannelId, State),
    case CanView of
        true ->
            ListId = guild_member_list:calculate_list_id(ChannelId, State),
            subscribe_member_list_ranges(
                ListId, GuildId, ChannelId, SessionId, Ranges, State
            );
        false ->
            State
    end.

-spec member_list_channel_allowed(session_id(), channel_id(), guild_state()) -> boolean().
member_list_channel_allowed(SessionId, ChannelId, State) ->
    case guild_thread_gate:needs_variant(State) of
        false ->
            true;
        true ->
            Session = maps:get(SessionId, maps:get(sessions, State, #{}), #{}),
            not guild_thread_gate:is_thread_id(ChannelId, State) andalso
                guild_thread_gate:channel_visible(Session, ChannelId, State)
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
        guild_member_list_subscribe:subscribe_ranges(SessionId, ListId, Ranges, State),
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
    SyncResponse = guild_member_list_read:build_sync_response(
        GuildId, ListId, RangesToSend, State
    ),
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
handle_update_member_subscriptions_local(_GuildId, SessionId, MemberIds, State) ->
    MemberSubs = maps:get(member_subscriptions, State, guild_subscriptions:init_state()),
    Sessions = maps:get(sessions, State, #{}),
    SessionUserId = get_session_user_id(SessionId, Sessions),
    FilteredMemberIds = filter_member_ids_for_subscription(
        maps:get(SessionId, Sessions, #{}), SessionUserId, MemberIds, State
    ),
    {NewMemberSubs, Added, Removed} = guild_subscriptions:update_subscriptions_with_delta(
        SessionId, FilteredMemberIds, MemberSubs
    ),
    State1 = State#{member_subscriptions => NewMemberSubs},
    State2 = handle_added_subscriptions(Added, SessionId, State1),
    handle_removed_subscriptions(Removed, State2).

-spec filter_member_ids_for_subscription(
    map(), user_id() | undefined, [user_id()], guild_state()
) ->
    [user_id()].
filter_member_ids_for_subscription(_Session, undefined, _MemberIds, _State) ->
    [];
filter_member_ids_for_subscription(Session, SessionUserId, MemberIds, State) ->
    guild_subscription_mutual_channels:filter_member_ids(
        Session, SessionUserId, MemberIds, State
    ).

-spec handle_added_subscriptions([user_id()], session_id(), guild_state()) -> guild_state().
handle_added_subscriptions(Added, SessionId, State) ->
    Prefetched = guild_presence:cached_presences(
        [UserId || UserId <- Added, presence_watched(UserId, State)]
    ),
    lists:foldl(
        fun(UserId, Acc) -> subscribe_added_member(UserId, SessionId, Prefetched, Acc) end,
        State,
        Added
    ).

-spec subscribe_added_member(user_id(), session_id(), map(), guild_state()) -> guild_state().
subscribe_added_member(UserId, SessionId, Prefetched, State) ->
    Watched = presence_watched(UserId, State),
    StateWithPresence = guild_sessions_presence:subscribe_to_user_presence(UserId, State),
    case {Watched, maps:find(UserId, Prefetched)} of
        {true, {ok, Lookup}} ->
            guild_presence:send_presence_lookup_to_session(
                UserId, SessionId, Lookup, StateWithPresence
            );
        _ ->
            guild_presence:send_cached_presence_to_session(UserId, SessionId, StateWithPresence)
    end.

-spec presence_watched(user_id(), guild_state()) -> boolean().
presence_watched(UserId, State) ->
    maps:get(UserId, maps:get(presence_subscriptions, State, #{}), 0) > 0.

-spec handle_removed_subscriptions([user_id()], guild_state()) -> guild_state().
handle_removed_subscriptions(Removed, State) ->
    lists:foldl(
        fun guild_sessions_presence:unsubscribe_from_user_presence/2,
        State,
        Removed
    ).
