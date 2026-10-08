%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_thread_dispatch).
-typing([eqwalizer]).

-include_lib("kernel/include/logger.hrl").

-export([
    handles/1,
    handle/3,
    dispatch_counts/0,
    count_masked/2,
    init_counters/0
]).

-define(RECENT_CREATE_CAP, 256).
-define(COUNTS_KEY, {?MODULE, variant_counts}).
-define(EVENTS, [
    thread_create,
    thread_update,
    thread_delete,
    thread_member_update,
    thread_members_update,
    thread_list_sync,
    thread_member_list_update,
    forum_unreads
]).
-define(MASKED_EVENTS, [
    message_create,
    message_update,
    channel_create,
    channel_update,
    channel_update_bulk,
    guild_role_create,
    guild_role_update,
    guild_role_update_bulk
]).

-type guild_state() :: map().
-type session_pair() :: {binary(), map()}.
-type delivery() :: {atom(), pid(), map()}.

-spec handles(atom()) -> boolean().
handles(thread_create) -> true;
handles(thread_update) -> true;
handles(thread_delete) -> true;
handles(thread_member_update) -> true;
handles(thread_members_update) -> true;
handles(thread_list_sync) -> true;
handles(thread_member_list_update) -> true;
handles(forum_unreads) -> true;
handles(_) -> false.

-spec handle(atom(), map(), guild_state()) -> {noreply, guild_state()}.
handle(Event, EventData, #{id := GuildId} = State) when is_integer(GuildId) ->
    Data = EventData#{<<"guild_id">> => integer_to_binary(GuildId)},
    try
        State1 =
            case guild_thread_load:loading(State) of
                true -> guild_thread_load:queue_event(Event, Data, State);
                false -> State
            end,
        {Deliveries, State2} = handle_event(Event, Data, State1),
        ok = deliver(Deliveries, GuildId),
        {noreply, after_delivery(Event, Data, State2)}
    catch
        Class:Reason:Stack ->
            ?LOG_WARNING(
                "guild_thread_dispatch_failed: event=~p guild_id=~p class=~p reason=~p stack=~p",
                [
                    Event, GuildId, Class, Reason, Stack
                ]
            ),
            {noreply, State}
    end;
handle(_Event, _EventData, State) ->
    {noreply, State}.

-spec after_delivery(atom(), map(), guild_state()) -> guild_state().
after_delivery(thread_members_update, Data, State) ->
    case snowflake_id:parse_maybe(maps:get(<<"id">>, Data, undefined)) of
        ThreadId when is_integer(ThreadId) ->
            guild_thread_subscriptions:member_lists_changed(ThreadId, State);
        _ ->
            State
    end;
after_delivery(_Event, _Data, State) ->
    State.

-spec handle_event(atom(), map(), guild_state()) -> {[delivery()], guild_state()}.
handle_event(thread_create, Data, State) ->
    thread_create(Data, State);
handle_event(thread_update, Data, State) ->
    thread_update(Data, State);
handle_event(thread_delete, Data, State) ->
    thread_delete(Data, State);
handle_event(thread_member_update, Data, State) ->
    thread_member_update(Data, State);
handle_event(thread_members_update, Data, State) ->
    thread_members_update(Data, State);
handle_event(_Event, _Data, State) ->
    {[], State}.

-spec thread_create(map(), guild_state()) -> {[delivery()], guild_state()}.
thread_create(Data, State0) ->
    ThreadId = snowflake_id:parse(maps:get(<<"id">>, Data)),
    State = guild_state_threads:apply_event(thread_create, Data, State0),
    Thread = thread_or_context(ThreadId, Data, State),
    Initial = [
        UserId
     || M <- list_field(<<"_fluxer_members">>, Data),
        UserId <- [snowflake_id:parse_maybe(maps:get(<<"user_id">>, M, undefined))],
        is_integer(UserId)
    ],
    InitialSet = maps:from_keys(Initial, true),
    Base = public_payload(Data),
    {Recipients, _Memo} = recipients(
        Thread,
        viewer_sessions(State),
        State,
        fun(Session, Passive) ->
            is_map_key(maps:get(user_id, Session, undefined), InitialSet) orelse
                (not Passive andalso interested(Session, ThreadId, State))
        end,
        #{}
    ),
    Deliveries = [
        {thread_create, Pid, with_member(Base, ThreadId, UserId, State)}
     || {_Sid, #{pid := Pid, user_id := UserId}} <- Recipients
    ],
    count(thread_create, length(Deliveries)),
    {Deliveries, remember_create(ThreadId, Initial, State)}.

-spec thread_update(map(), guild_state()) -> {[delivery()], guild_state()}.
thread_update(Data, State0) ->
    ThreadId = snowflake_id:parse(maps:get(<<"id">>, Data)),
    WasActive = guild_thread_gate:thread(ThreadId, State0) =/= undefined,
    Viewers = viewer_sessions(State0),
    Before = thread_or_context(ThreadId, Data, State0),
    {BeforeRecipients, Memo} = recipients(
        Before, Viewers, State0, joined_or_interested(ThreadId, State0), #{}
    ),
    State = guild_state_threads:apply_event(thread_update, Data, State0),
    After = thread_or_context(ThreadId, Data, State),
    {AfterRecipients, AfterMemo} = recipients(
        After,
        Viewers,
        State,
        joined_or_interested(ThreadId, State),
        shared_memo(Before, After, Memo)
    ),
    Payload = public_payload(Data),
    Deliveries = [
        {thread_update, Pid, Payload}
     || {_Sid, #{pid := Pid}} <- union(BeforeRecipients, AfterRecipients)
    ],
    count(thread_update, length(Deliveries)),
    MemberDeliveries =
        case {WasActive, guild_thread_gate:thread(ThreadId, State)} of
            {false, Restored} when is_map(Restored) ->
                unarchive_member_updates(ThreadId, Restored, Viewers, State, AfterMemo);
            _ ->
                []
        end,
    count(thread_member_update, length(MemberDeliveries)),
    {Deliveries ++ MemberDeliveries, State}.

-spec shared_memo(map() | undefined, map() | undefined, map()) -> map().
shared_memo(Before, After, Memo) when is_map(Before), is_map(After) ->
    ParentId = guild_thread_permissions:parent_id(Before),
    case
        guild_thread_permissions:parent_id(After) =:= ParentId andalso
            maps:get(<<"type">>, Before, undefined) =/= 12 andalso
            maps:get(<<"type">>, After, undefined) =/= 12
    of
        true -> Memo;
        false -> #{}
    end;
shared_memo(_Before, _After, _Memo) ->
    #{}.

-spec unarchive_member_updates(integer(), map(), [session_pair()], guild_state(), map()) ->
    [delivery()].
unarchive_member_updates(ThreadId, Thread, Viewers, #{id := GuildId} = State, Memo) ->
    case guild_thread_gate:store(State) of
        undefined ->
            [];
        Tab ->
            {Targets, _Memo} = recipients(
                Thread,
                Viewers,
                State,
                fun(Session, _Passive) -> is_member(Session, ThreadId, State) end,
                Memo
            ),
            [
                {thread_member_update, Pid, Member#{
                    <<"guild_id">> => integer_to_binary(GuildId)
                }}
             || {_Sid, #{pid := Pid, user_id := UserId}} <- Targets,
                Member <- [guild_thread_store:get_member(Tab, ThreadId, UserId)],
                is_map(Member)
            ]
    end.

-spec thread_delete(map(), guild_state()) -> {[delivery()], guild_state()}.
thread_delete(Data, State0) ->
    ThreadId = snowflake_id:parse(maps:get(<<"id">>, Data)),
    Thread = thread_or_context(ThreadId, Data, State0),
    Joined = joined_or_interested(ThreadId, State0),
    Listed = maps:from_keys(
        snowflake_id:parse_list(maps:get(<<"_fluxer_member_ids">>, Data, [])), true
    ),
    Wants = fun(Session, Passive) ->
        Joined(Session, Passive) orelse
            is_map_key(maps:get(user_id, Session, undefined), Listed)
    end,
    {Recipients, _Memo} = recipients(Thread, viewer_sessions(State0), State0, Wants, #{}),
    Payload = maps:with([<<"id">>, <<"guild_id">>, <<"parent_id">>, <<"type">>], Data),
    State = guild_state_threads:apply_event(thread_delete, Data, State0),
    Deliveries = [{thread_delete, Pid, Payload} || {_Sid, #{pid := Pid}} <- Recipients],
    count(thread_delete, length(Deliveries)),
    {Deliveries, State}.

-spec thread_member_update(map(), guild_state()) -> {[delivery()], guild_state()}.
thread_member_update(Data, State0) ->
    State = guild_state_threads:apply_event(thread_member_update, Data, State0),
    ThreadId = snowflake_id:parse_maybe(maps:get(<<"id">>, Data, undefined)),
    TargetId = snowflake_id:parse_maybe(maps:get(<<"user_id">>, Data, undefined)),
    Thread = member_update_thread(ThreadId, TargetId, Data, State),
    Payload = guild_thread_gate:strip_internal(Data),
    Deliveries = [
        {thread_member_update, Pid, Payload}
     || Thread =/= undefined,
        {_Sid, #{pid := Pid, user_id := UserId} = Session} <- viewer_sessions(State),
        UserId =:= TargetId,
        can_view(Session, Thread, State)
    ],
    count(thread_member_update, length(Deliveries)),
    {Deliveries, State}.

-spec member_update_thread(term(), term(), map(), guild_state()) -> map() | undefined.
member_update_thread(ThreadId, TargetId, Data, State) when
    is_integer(ThreadId), is_integer(TargetId)
->
    case thread_or_context(ThreadId, Data, State) of
        undefined -> parent_context(ThreadId, TargetId, Data);
        Thread -> Thread
    end;
member_update_thread(_ThreadId, _TargetId, _Data, _State) ->
    undefined.

-spec parent_context(integer(), integer(), map()) -> map() | undefined.
parent_context(ThreadId, TargetId, #{<<"_fluxer_parent_id">> := ParentId}) ->
    #{
        <<"id">> => ThreadId,
        <<"parent_id">> => ParentId,
        <<"_fluxer_member_ids">> => [integer_to_binary(TargetId)]
    };
parent_context(_ThreadId, _TargetId, _Data) ->
    undefined.

-spec thread_members_update(map(), guild_state()) -> {[delivery()], guild_state()}.
thread_members_update(Data, State0) ->
    ThreadId = snowflake_id:parse(maps:get(<<"id">>, Data)),
    State = guild_state_threads:apply_event(thread_members_update, Data, State0),
    Thread = thread_or_context(ThreadId, Data, State),
    Added = maps:from_keys(
        [
            UserId
         || M <- list_field(<<"added_members">>, Data),
            UserId <- [snowflake_id:parse_maybe(maps:get(<<"user_id">>, M, undefined))],
            is_integer(UserId)
        ],
        true
    ),
    Removed = maps:from_keys(
        [
            UserId
         || Raw <- list_value(maps:get(<<"removed_member_ids">>, Data, [])),
            UserId <- [snowflake_id:parse_maybe(Raw)],
            is_integer(UserId)
        ],
        true
    ),
    {Suppressed, State1} = take_recent_create(ThreadId, State),
    Viewers = viewer_sessions(State1),
    {Synthesized, Memo} = synthesized_creates(
        Thread, ThreadId, maps:without(Suppressed, Added), Viewers, State1
    ),
    Payload = members_update_payload(Data, State1),
    Affected = [
        Pair
     || {_Sid, #{user_id := UserId}} = Pair <- Viewers,
        is_map_key(UserId, Added) orelse is_map_key(UserId, Removed)
    ],
    {Interested, _Memo} = recipients(
        Thread,
        Viewers,
        State1,
        fun(Session, Passive) ->
            not Passive andalso interested(Session, ThreadId, State1)
        end,
        Memo
    ),
    Deliveries = [
        {thread_members_update, Pid, Payload}
     || {_Sid, #{pid := Pid}} <- union(Affected, Interested)
    ],
    count(thread_members_update, length(Deliveries)),
    {Synthesized ++ Deliveries, State1}.

-spec synthesized_creates(
    map() | undefined, integer(), #{integer() => true}, [session_pair()], guild_state()
) ->
    {[delivery()], map()}.
synthesized_creates(undefined, _ThreadId, _Users, _Viewers, _State) ->
    {[], #{}};
synthesized_creates(_Thread, _ThreadId, Users, _Viewers, _State) when map_size(Users) =:= 0 ->
    {[], #{}};
synthesized_creates(Thread, ThreadId, Users, Viewers, State) ->
    Base = guild_thread_view:thread_payload(Thread),
    {Targets, Memo} = recipients(
        Thread,
        Viewers,
        State,
        fun(Session, _Passive) -> is_map_key(maps:get(user_id, Session, undefined), Users) end,
        #{}
    ),
    Deliveries = [
        {thread_create, Pid, with_member(Base, ThreadId, UserId, State)}
     || {_Sid, #{pid := Pid, user_id := UserId}} <- Targets
    ],
    count(thread_create, length(Deliveries)),
    {Deliveries, Memo}.

-spec members_update_payload(map(), guild_state()) -> map().
members_update_payload(Data, State) ->
    Base = guild_thread_gate:strip_internal(Data),
    case maps:get(<<"added_members">>, Base, undefined) of
        Added when is_list(Added) ->
            Ctx = guild_member_list_connected:presence_context(State),
            Base#{
                <<"added_members">> => [decorate_added(M, Ctx, State) || M <- Added, is_map(M)]
            };
        _ ->
            Base
    end.

-spec decorate_added(map(), map(), guild_state()) -> map().
decorate_added(Member, PresenceCtx, State) ->
    Stripped = maps:without([<<"muted">>, <<"mute_config">>], Member),
    case snowflake_id:parse_maybe(maps:get(<<"user_id">>, Member, undefined)) of
        UserId when is_integer(UserId) ->
            GuildMember = guild_data_members:find_member_by_user_id(UserId, State),
            Stripped#{
                <<"member">> => null_if_undefined(GuildMember),
                <<"presence">> => presence(UserId, PresenceCtx)
            };
        _ ->
            Stripped#{<<"member">> => null, <<"presence">> => null}
    end.

-spec presence(integer(), map()) -> map() | null.
presence(UserId, Ctx) ->
    #{<<"presence">> := Presence} = guild_member_list_connected:add_presence_to_member(
        #{}, UserId, Ctx
    ),
    case maps:get(<<"status">>, Presence, <<"offline">>) of
        <<"offline">> -> null;
        _ -> Presence#{<<"user">> => #{<<"id">> => integer_to_binary(UserId)}}
    end.

-spec null_if_undefined(map() | undefined) -> map() | null.
null_if_undefined(undefined) -> null;
null_if_undefined(Value) -> Value.

-spec recipients(
    map() | undefined,
    [session_pair()],
    guild_state(),
    fun((map(), boolean()) -> boolean()),
    map()
) -> {[session_pair()], map()}.
recipients(undefined, _Viewers, _State, _Wants, Memo) ->
    {[], Memo};
recipients(Thread, Viewers, State, Wants, Memo0) ->
    GuildId = maps:get(id, State),
    Small = session_passive:is_small_guild(State),
    {Pairs, Memo} = lists:foldl(
        fun({_Sid, Session} = Pair, {Acc, Memo1}) ->
            case
                Wants(Session, (not Small) andalso session_passive:is_passive(GuildId, Session))
            of
                true ->
                    case guild_thread_gate:thread_view_access(Session, Thread, State, Memo1) of
                        {true, Memo2} -> {[Pair | Acc], Memo2};
                        {false, Memo2} -> {Acc, Memo2}
                    end;
                false ->
                    {Acc, Memo1}
            end
        end,
        {[], Memo0},
        Viewers
    ),
    {lists:reverse(Pairs), Memo}.

-spec joined_or_interested(integer(), guild_state()) -> fun((map(), boolean()) -> boolean()).
joined_or_interested(ThreadId, State) ->
    fun(Session, Passive) ->
        is_member(Session, ThreadId, State) orelse
            (not Passive andalso interested(Session, ThreadId, State))
    end.

-spec interested(map(), integer(), guild_state()) -> boolean().
interested(Session, ThreadId, State) ->
    is_bot(Session) orelse is_member(Session, ThreadId, State) orelse
        maps:get(thread_subscribed, Session, false) =:= true.

-spec is_member(map(), integer(), guild_state()) -> boolean().
is_member(#{user_id := UserId}, ThreadId, State) ->
    case guild_thread_gate:store(State) of
        undefined -> false;
        Tab -> guild_thread_store:is_member(Tab, ThreadId, UserId)
    end;
is_member(_Session, _ThreadId, _State) ->
    false.

-spec is_bot(map()) -> boolean().
is_bot(Session) ->
    maps:get(bot, Session, false) =:= true.

-spec can_view(map(), map(), guild_state()) -> boolean().
can_view(#{user_id := UserId}, Thread, State) when is_integer(UserId) ->
    Member = guild_permissions:find_member_by_user_id(UserId, State),
    Member =/= undefined andalso guild_thread_view:can_view(UserId, Thread, Member, State);
can_view(_Session, _Thread, _State) ->
    false.

-spec viewer_sessions(guild_state()) -> [session_pair()].
viewer_sessions(State) ->
    [
        {Sid, Session}
     || {Sid, Session} <- maps:to_list(maps:get(sessions, State, #{})),
        is_map(Session),
        guild_thread_gate:session_viewer(Session),
        maps:get(pending_connect, Session, false) =/= true,
        is_pid(maps:get(pid, Session, undefined))
    ].

-spec union([session_pair()], [session_pair()]) -> [session_pair()].
union(A, B) ->
    maps:to_list(maps:merge(maps:from_list(A), maps:from_list(B))).

-spec thread_or_context(integer(), map(), guild_state()) -> map() | undefined.
thread_or_context(ThreadId, Data, State) ->
    case guild_thread_gate:thread(ThreadId, State) of
        undefined ->
            case guild_state_threads:thread_context(ThreadId, Data) of
                undefined -> fallback_context(ThreadId, Data);
                Context -> Context
            end;
        Thread ->
            Thread
    end.

-spec fallback_context(integer(), map()) -> map() | undefined.
fallback_context(ThreadId, #{<<"parent_id">> := _, <<"type">> := _} = Data) ->
    (maps:with([<<"parent_id">>, <<"type">>, <<"_fluxer_member_ids">>], Data))#{
        <<"id">> => ThreadId
    };
fallback_context(_ThreadId, _Data) ->
    undefined.

-spec public_payload(map()) -> map().
public_payload(Data) ->
    maps:without(
        [<<"member">>, <<"member_ids_preview">>], guild_thread_gate:strip_internal(Data)
    ).

-spec with_member(map(), integer(), term(), guild_state()) -> map().
with_member(Base, ThreadId, UserId, State) when is_integer(UserId) ->
    case guild_thread_gate:store(State) of
        undefined ->
            Base;
        Tab ->
            case guild_thread_store:get_member(Tab, ThreadId, UserId) of
                undefined -> Base;
                Member -> Base#{<<"member">> => Member}
            end
    end;
with_member(Base, _ThreadId, _UserId, _State) ->
    Base.

-spec remember_create(integer(), [integer()], guild_state()) -> guild_state().
remember_create(_ThreadId, [], State) ->
    State;
remember_create(ThreadId, Initial, State) ->
    Recent0 = maps:get(thread_recent_create, State, #{}),
    Recent =
        case map_size(Recent0) >= ?RECENT_CREATE_CAP of
            true -> #{};
            false -> Recent0
        end,
    State#{thread_recent_create => Recent#{ThreadId => Initial}}.

-spec take_recent_create(integer(), guild_state()) -> {[integer()], guild_state()}.
take_recent_create(ThreadId, State) ->
    case maps:get(thread_recent_create, State, #{}) of
        #{ThreadId := Initial} = Recent ->
            {Initial, State#{thread_recent_create => maps:remove(ThreadId, Recent)}};
        _ ->
            {[], State}
    end.

-spec deliver([delivery()], integer()) -> ok.
deliver(Deliveries, GuildId) ->
    Grouped = lists:foldl(
        fun({Event, Pid, Payload}, Acc) ->
            Key = {Event, Payload},
            Acc#{Key => [Pid | maps:get(Key, Acc, [])]}
        end,
        #{},
        Deliveries
    ),
    Ordered = lists:sort(
        fun({{EventA, _}, _}, {{EventB, _}, _}) -> order(EventA) =< order(EventB) end,
        maps:to_list(Grouped)
    ),
    lists:foreach(
        fun({{Event, Payload}, Pids}) ->
            Encoded =
                {pre_encoded,
                    iolist_to_binary(
                        json:encode(guild_data_wire:payload(Payload), fun json:encode_value/2)
                    )},
            gateway_dispatch_relay:dispatch_many(lists:reverse(Pids), Event, Encoded, GuildId)
        end,
        Ordered
    ).

-spec order(atom()) -> non_neg_integer().
order(thread_create) -> 0;
order(thread_update) -> 1;
order(thread_delete) -> 1;
order(thread_member_update) -> 2;
order(thread_members_update) -> 3;
order(_) -> 4.

-spec list_field(binary(), map()) -> [map()].
list_field(Key, Map) ->
    [M || M <- list_value(maps:get(Key, Map, [])), is_map(M)].

-spec list_value(term()) -> list().
list_value(List) when is_list(List) -> List;
list_value(_) -> [].

-spec count(atom(), non_neg_integer()) -> ok.
count(Event, N) ->
    count_key({Event, viewer}, N).

-spec count_masked(atom(), non_neg_integer()) -> ok.
count_masked(Event, N) ->
    count_key({Event, masked}, N).

-spec count_keys() -> [{atom(), viewer | masked}].
count_keys() ->
    [{E, viewer} || E <- ?EVENTS] ++ [{E, masked} || E <- ?MASKED_EVENTS].

-spec count_key({atom(), viewer | masked}, non_neg_integer()) -> ok.
count_key(_Key, 0) ->
    ok;
count_key(Key, N) ->
    case key_index(Key, count_keys(), 1) of
        undefined ->
            ok;
        Index ->
            guild_thread_store:add_counter(?COUNTS_KEY, Index, N)
    end.

-spec init_counters() -> ok.
init_counters() ->
    guild_thread_store:ensure_counter(?COUNTS_KEY, length(count_keys())).

-spec key_index(term(), [term()], pos_integer()) -> pos_integer() | undefined.
key_index(Key, [Key | _], Index) -> Index;
key_index(Key, [_ | Rest], Index) -> key_index(Key, Rest, Index + 1);
key_index(_Key, [], _Index) -> undefined.

-spec dispatch_counts() -> #{{atom(), viewer | masked} => non_neg_integer()}.
dispatch_counts() ->
    Keys = count_keys(),
    case persistent_term:get(?COUNTS_KEY, undefined) of
        undefined ->
            maps:from_list([{K, 0} || K <- Keys]);
        Counters ->
            maps:from_list([
                {K, counters:get(Counters, I)}
             || {K, I} <- lists:zip(Keys, lists:seq(1, length(Keys)))
            ])
    end.
