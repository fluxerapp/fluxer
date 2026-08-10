%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(presence).
-typing([eqwalizer]).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type user_id() :: integer().
-type session_id() :: binary().
-type status() :: online | offline | idle | dnd | invisible.
-type custom_status() :: map() | null.
-type session_entry() :: #{
    session_id := session_id(),
    status := status(),
    afk := boolean(),
    mobile := boolean(),
    pid := pid(),
    mref := reference(),
    socket_pid := pid() | undefined
}.
-type sessions() :: #{session_id() => session_entry()}.
-type push_buffer_entry() :: #{
    channel_id := integer(), message_id := integer(), params := map()
}.
-type state() :: #{
    user_id := user_id(),
    user_data := map(),
    sessions := sessions(),
    push_buffer := [push_buffer_entry()],
    custom_status := custom_status(),
    status := status(),
    guild_ids := #{integer() => true},
    temporary_guild_ids := #{integer() => true},
    friends := #{user_id() => true},
    group_dm_recipients := #{integer() => #{user_id() => true}},
    subscriptions := map(),
    is_bot := boolean(),
    initial_presences_sent := boolean(),
    last_published_presence := map() | undefined
}.
-type presence_data() :: #{
    user_id := user_id(),
    user_data := map(),
    guild_ids => [integer()],
    friend_ids => [user_id()],
    group_dm_recipients => #{integer() => [user_id()] | #{user_id() => true}},
    status := status(),
    custom_status => custom_status()
}.

-spec start_link(presence_data()) -> {ok, pid()} | {error, term()}.
start_link(PresenceData) ->
    normalize_start_link(gen_server:start_link(?MODULE, PresenceData, [])).

-spec init(presence_data()) -> {ok, state()}.
init(PresenceData) ->
    process_flag(trap_exit, true),
    erlang:process_flag(fullsweep_after, 20),
    State = build_initial_state(PresenceData),
    StateWithSubs = presence_broadcast:ensure_initial_global_subscriptions(State),
    {ok, StateWithSubs}.

-spec terminate(term(), state() | term()) -> ok.
terminate(_Reason, State) when not is_map(State) ->
    ok;
terminate(_Reason, State) ->
    _ = presence_update:flush_push_buffer(State),
    case maps:get(user_id, State, undefined) of
        UserId when is_integer(UserId) ->
            presence_cache:delete(UserId),
            presence_connect:publish_offline_on_terminate(UserId, State),
            presence_connect:kick_temporary_members_on_terminate(UserId, State);
        _ ->
            ok
    end.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    erlang:garbage_collect(),
    {ok, State}.

-spec handle_call(term(), gen_server:from(), state()) ->
    {reply, term(), state()} | {stop, normal, ok, state()}.
handle_call({session_connect, Request}, {Pid, _}, State) when is_map(Request), is_pid(Pid) ->
    {reply, Reply, NewState} = presence_session:handle_session_connect(Request, Pid, State),
    FinalState = presence_broadcast:publish_global_presence(
        maps:get(sessions, NewState), NewState
    ),
    presence_broadcast:send_cached_presences_to_session(Pid, FinalState),
    {reply, Reply, FinalState};
handle_call(get_current_visible_presence, _From, State) ->
    {reply, presence_broadcast:current_visible_presence(State), State};
handle_call({terminate_session, SessionIdHashes}, _From, State) when is_list(SessionIdHashes) ->
    presence_connect:handle_terminate_session_call(binary_list(SessionIdHashes), State);
handle_call({dispatch, EventAtom, Data}, _From, State) when is_atom(EventAtom), is_map(Data) ->
    handle_dispatch_call(EventAtom, Data, State);
handle_call({join_guild, GuildId}, _From, State) when is_integer(GuildId) ->
    presence_connect:handle_join_guild(GuildId, State);
handle_call({leave_guild, GuildId}, _From, State) when is_integer(GuildId) ->
    presence_connect:handle_leave_guild(GuildId, State);
handle_call({add_temporary_guild, GuildId}, _From, State) when is_integer(GuildId) ->
    presence_connect:handle_add_temporary_guild(GuildId, State);
handle_call({remove_temporary_guild, GuildId}, _From, State) when is_integer(GuildId) ->
    {reply, ok, presence_connect:remove_temporary_guild_id(GuildId, State)};
handle_call({terminate, SessionIdHashes}, _From, State) when is_list(SessionIdHashes) ->
    presence_connect:terminate_all_session_pids(binary_list(SessionIdHashes), State),
    {stop, normal, ok, State};
handle_call(_, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({dispatch, Event, Data}, State) when is_atom(Event), is_map(Data) ->
    handle_dispatch_cast(Event, Data, State);
handle_cast(presence_rejoin, State) ->
    handle_presence_rejoin(State);
handle_cast(reconcile_flattened_presence, State) ->
    {noreply,
        presence_broadcast:publish_global_presence(
            maps:get(sessions, State), State
        )};
handle_cast({presence_update, Request}, State) when is_map(Request) ->
    handle_presence_update_cast(Request, State);
handle_cast({terminate_session, SessionIdHashes}, State) when is_list(SessionIdHashes) ->
    presence_connect:terminate_all_session_pids(binary_list(SessionIdHashes), State),
    {noreply, State};
handle_cast({terminate_all_sessions}, State) ->
    presence_connect:force_terminate_all_sessions(State),
    {noreply, State};
handle_cast({sync_friends, FriendIds}, State) when is_list(FriendIds) ->
    handle_sync_friends_cast(FriendIds, [], State);
handle_cast({sync_friends, FriendIds, FlushedIds}, State) when
    is_list(FriendIds), is_list(FlushedIds)
->
    handle_sync_friends_cast(FriendIds, FlushedIds, State);
handle_cast({sync_group_dm_recipients, RecipientsByChannel}, State) when
    is_map(RecipientsByChannel)
->
    {noreply, presence_broadcast:sync_group_dm_subscriptions(RecipientsByChannel, State)};
handle_cast(Msg, State) ->
    handle_cast_guild(Msg, State).

-spec handle_sync_friends_cast([term()], [term()], state()) -> {noreply, state()}.
handle_sync_friends_cast(FriendIds, FlushedIds, State) ->
    {noreply,
        presence_broadcast:sync_friend_subscriptions(
            user_ids(FriendIds), user_ids(FlushedIds), State
        )}.

-spec handle_cast_guild(term(), state()) -> {noreply, state()}.
handle_cast_guild({join_guild, GuildId}, State) when is_integer(GuildId) ->
    {noreply, cast_guild_op(fun presence_connect:handle_join_guild/2, GuildId, State)};
handle_cast_guild({leave_guild, GuildId}, State) when is_integer(GuildId) ->
    {noreply, cast_guild_op(fun presence_connect:handle_leave_guild/2, GuildId, State)};
handle_cast_guild({add_temporary_guild, GuildId}, State) when is_integer(GuildId) ->
    {noreply, cast_guild_op(fun presence_connect:handle_add_temporary_guild/2, GuildId, State)};
handle_cast_guild({remove_temporary_guild, GuildId}, State) when is_integer(GuildId) ->
    {noreply, presence_connect:remove_temporary_guild_id(GuildId, State)};
handle_cast_guild(_, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()} | {stop, normal, state()}.
handle_info({presence, TargetId, Payload}, State) when is_integer(TargetId), is_map(Payload) ->
    presence_broadcast:dispatch_global_presence(TargetId, Payload, State);
handle_info({initial_presences, Presences}, State) when is_list(Presences) ->
    presence_broadcast:dispatch_initial_presences(map_list(Presences), State),
    {noreply, State};
handle_info({'DOWN', Ref, process, _Pid, Reason}, State) when is_reference(Ref) ->
    presence_connect:handle_process_down(Ref, Reason, State);
handle_info(_, State) ->
    {noreply, State}.

-spec build_initial_state(presence_data()) -> state().
build_initial_state(PresenceData) ->
    UserId = maps:get(user_id, PresenceData),
    UserData = maps:get(user_data, PresenceData),
    Status = maps:get(status, PresenceData),
    IsBot = maps:get(<<"bot">>, UserData, false),
    GuildIds = maps:get(guild_ids, PresenceData, []),
    FriendIds = select_friend_ids(IsBot, maps:get(friend_ids, PresenceData, [])),
    GroupDmRecipients0 = maps:get(group_dm_recipients, PresenceData, #{}),
    #{
        user_id => UserId,
        user_data => UserData,
        sessions => #{},
        push_buffer => [],
        custom_status => maps:get(custom_status, PresenceData, null),
        status => Status,
        guild_ids => presence_targets:map_from_ids(GuildIds),
        temporary_guild_ids => #{},
        friends => presence_targets:map_from_ids(FriendIds),
        group_dm_recipients =>
            presence_broadcast:normalize_group_dm_recipients(GroupDmRecipients0, UserId, IsBot),
        subscriptions => #{},
        is_bot => IsBot,
        initial_presences_sent => false,
        last_published_presence => undefined
    }.

-spec select_friend_ids(boolean(), [user_id()]) -> [user_id()].
select_friend_ids(true, _FriendIds) ->
    [];
select_friend_ids(false, FriendIds) ->
    FriendIds.

-spec handle_presence_update_cast(map(), state()) -> {noreply, state()}.
handle_presence_update_cast(Request, State) ->
    {UpdatedRequest, StateWithCustomStatus} = presence_update:maybe_handle_custom_status(
        Request, State
    ),
    {noreply, NewState} = presence_session:handle_presence_update(
        UpdatedRequest, StateWithCustomStatus
    ),
    FinalState = presence_broadcast:publish_global_presence(
        maps:get(sessions, NewState), NewState
    ),
    {noreply, FinalState}.

-spec cast_guild_op(fun((integer(), state()) -> {reply, term(), state()}), integer(), state()) ->
    state().
cast_guild_op(Fun, GuildId, State) ->
    {reply, _Reply, NewState} = Fun(GuildId, State),
    NewState.

-spec handle_dispatch_call(atom(), map(), state()) -> {reply, ok, state()}.
handle_dispatch_call(EventAtom, Data, State) ->
    presence_broadcast:dispatch_to_all_sessions(EventAtom, Data, State),
    {reply, ok, process_dispatch_event(EventAtom, Data, State)}.

-spec handle_dispatch_cast(atom(), map(), state()) -> {noreply, state()}.
handle_dispatch_cast(Event, Data, State) ->
    presence_broadcast:dispatch_to_all_sessions(Event, Data, State),
    {noreply, process_dispatch_event(Event, Data, State)}.

-spec handle_presence_rejoin(state()) -> {noreply, state()}.
handle_presence_rejoin(State) ->
    SessionPids = presence_connect:collect_session_pids(State),
    lists:foreach(
        fun(Pid) ->
            Pid ! {presence_rejoin_check}
        end,
        SessionPids
    ),
    {noreply, State}.

-spec process_dispatch_event(atom(), map(), state()) -> state().
process_dispatch_event(user_update, Data, State) ->
    presence_update:handle_user_update_event(Data, State);
process_dispatch_event(user_settings_update, Data, State) ->
    NewState = presence_update:handle_user_settings_update(Data, State),
    presence_broadcast:force_publish_global_presence(NewState);
process_dispatch_event(message_create, Data, State) ->
    presence_update:handle_message_create_event(Data, State);
process_dispatch_event(message_ack, Data, State) ->
    presence_update:handle_message_ack_event(Data, State);
process_dispatch_event(_, _Data, State) ->
    State.

-spec binary_list([term()]) -> [binary()].
binary_list(Items) ->
    [Item || Item <- Items, is_binary(Item)].

-spec user_ids([term()]) -> [user_id()].
user_ids(Items) ->
    [Item || Item <- Items, is_integer(Item)].

-spec map_list([term()]) -> [map()].
map_list(Items) ->
    [Item || Item <- Items, is_map(Item)].

-spec normalize_start_link(gen_server:start_ret()) -> {ok, pid()} | {error, term()}.
normalize_start_link({ok, Pid}) ->
    {ok, Pid};
normalize_start_link({error, Reason}) ->
    {error, Reason};
normalize_start_link(ignore) ->
    {error, ignore}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

presence_rejoin_notifies_all_sessions_test() ->
    Parent = self(),
    Session1 = spawn(fun() -> rejoin_check_receiver(Parent, one) end),
    Session2 = spawn(fun() -> rejoin_check_receiver(Parent, two) end),
    State = test_state(#{
        <<"s1">> => test_session_entry(Session1),
        <<"s2">> => test_session_entry(Session2)
    }),
    ?assertEqual({noreply, State}, handle_cast(presence_rejoin, State)),
    ?assert(received_rejoin_ack(one)),
    ?assert(received_rejoin_ack(two)).

presence_rejoin_with_no_sessions_is_noop_test() ->
    State = test_state(#{}),
    ?assertEqual({noreply, State}, handle_cast(presence_rejoin, State)).

reconcile_flattened_presence_uses_current_session_state_test() ->
    State = test_state(#{}),
    PublishedState = State#{last_published_presence => #{status => <<"offline">>}},
    meck:new(presence_broadcast, [passthrough]),
    meck:expect(
        presence_broadcast,
        publish_global_presence,
        fun(Sessions, ReceivedState) ->
            ?assertEqual(#{}, Sessions),
            ?assertEqual(State, ReceivedState),
            PublishedState
        end
    ),
    try
        ?assertEqual(
            {noreply, PublishedState},
            handle_cast(reconcile_flattened_presence, State)
        )
    after
        meck:unload(presence_broadcast)
    end.

-spec test_state(sessions()) -> state().
test_state(Sessions) ->
    #{
        user_id => 1,
        user_data => #{},
        sessions => Sessions,
        push_buffer => [],
        custom_status => null,
        status => online,
        guild_ids => #{},
        temporary_guild_ids => #{},
        friends => #{},
        group_dm_recipients => #{},
        subscriptions => #{},
        is_bot => false,
        initial_presences_sent => false,
        last_published_presence => undefined
    }.

-spec test_session_entry(pid()) -> session_entry().
test_session_entry(Pid) ->
    #{
        session_id => <<"s">>,
        status => online,
        afk => false,
        mobile => false,
        pid => Pid,
        mref => make_ref(),
        socket_pid => undefined
    }.

-spec rejoin_check_receiver(pid(), atom()) -> term().
rejoin_check_receiver(Parent, Tag) ->
    receive
        {presence_rejoin_check} -> Parent ! {rejoin_ack, Tag}
    after 1000 -> Parent ! {rejoin_timeout, Tag}
    end.

-spec received_rejoin_ack(atom()) -> boolean().
received_rejoin_ack(Tag) ->
    receive
        {rejoin_ack, Tag} -> true;
        {rejoin_timeout, Tag} -> false
    after 1500 -> false
    end.

-endif.
