%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_dispatch_presence).
-typing([eqwalizer]).

-export([
    should_buffer_presence/3,
    buffer_presence/3,
    maybe_flush_pending_presences/3,
    flush_all_pending_presences/1,
    dispatch_presence_now/2,
    maybe_sync_presence_targets/3,
    sync_presence_targets/1,
    event_changes_presence_targets/1,
    presence_user_id/1,
    relationship_target_id/1
]).

-export_type([session_state/0, event/0, user_id/0]).

-define(MAX_EVENT_BUFFER_SIZE, 4096).
-define(MAX_SINGLE_EVENT_BUFFER_BYTES, 2097152).
-define(MAX_TOTAL_BUFFER_BYTES, 16777216).
-define(MAX_PENDING_PRESENCE_BUFFER_SIZE, 2048).

-type session_state() :: session:session_state().
-type event() :: atom() | binary().
-type user_id() :: session:user_id().

-spec should_buffer_presence(event(), map(), session_state()) -> boolean().
should_buffer_presence(presence_update, Data, State) ->
    case maps:get(suppress_presence_updates, State, true) of
        true ->
            true;
        false ->
            check_presence_scope(Data, State)
    end;
should_buffer_presence(_, _, _) ->
    false.

-spec check_presence_scope(map(), session_state()) -> boolean().
check_presence_scope(Data, State) ->
    case maps:find(<<"guild_id">>, Data) of
        error ->
            UserId = presence_user_id(Data),
            check_user_presence_buffering(UserId, State);
        {ok, GuildIdValue} ->
            should_buffer_guild_presence(GuildIdValue, State)
    end.

-spec should_buffer_guild_presence(term(), session_state()) -> boolean().
should_buffer_guild_presence(GuildIdValue, State) ->
    case snowflake_id:parse_maybe(GuildIdValue) of
        GuildId when is_integer(GuildId) ->
            not maps:is_key(GuildId, maps:get(guilds, State, #{}));
        undefined ->
            true
    end.

-spec check_user_presence_buffering(user_id() | undefined, session_state()) -> boolean().
check_user_presence_buffering(undefined, _State) ->
    false;
check_user_presence_buffering(UserId, State) ->
    Relationships = maps:get(relationships, State, #{}),
    IsRelationship = relationship_allows_presence(UserId, Relationships),
    IsGroupDmRecipient = is_group_dm_recipient(UserId, State),
    not (IsRelationship orelse IsGroupDmRecipient).

-spec relationship_allows_presence(user_id(), #{user_id() => integer()}) -> boolean().
relationship_allows_presence(UserId, Relationships) when
    is_integer(UserId), is_map(Relationships)
->
    case maps:get(UserId, Relationships, undefined) of
        1 -> true;
        _ -> false
    end;
relationship_allows_presence(_, _) ->
    false.

-spec is_group_dm_recipient(user_id(), session_state()) -> boolean().
is_group_dm_recipient(UserId, State) when is_map(State) ->
    GroupDmRecipients = presence_targets:group_dm_recipients_from_state(State),
    maps:fold(
        fun
            (_, Recipients, false) -> maps:is_key(UserId, Recipients);
            (_, _, true) -> true
        end,
        false,
        GroupDmRecipients
    ).

-spec buffer_presence(event(), map(), session_state()) -> session_state().
buffer_presence(Event, Data, State) ->
    Pending = ensure_queue(maps:get(pending_presences, State, [])),
    UserId = presence_user_id(Data),
    Entry = #{event => Event, data => Data, user_id => UserId},
    Trimmed = trim_queue_from_tail(Pending, ?MAX_PENDING_PRESENCE_BUFFER_SIZE - 1),
    NewPending = queue:in_r(Entry, Trimmed),
    State#{pending_presences => NewPending}.

-spec maybe_flush_pending_presences(event(), map(), session_state()) ->
    {session_state(), [user_id()]}.
maybe_flush_pending_presences(relationship_add, Data, State) ->
    maybe_flush_relationship_pending_presences(Data, State);
maybe_flush_pending_presences(relationship_update, Data, State) ->
    maybe_flush_relationship_pending_presences(Data, State);
maybe_flush_pending_presences(channel_create, Data, State) ->
    flush_dm_channel_pending_presences(Data, State);
maybe_flush_pending_presences(channel_update, Data, State) ->
    flush_dm_channel_pending_presences(Data, State);
maybe_flush_pending_presences(channel_recipient_add, Data, State) ->
    flush_added_recipient_pending_presences(Data, State);
maybe_flush_pending_presences(_, _, State) ->
    {State, []}.

-spec flush_dm_channel_pending_presences(map(), session_state()) ->
    {session_state(), [user_id()]}.
flush_dm_channel_pending_presences(Data, State) ->
    SelfUserId = maps:get(user_id, State, undefined),
    RecipientIds = presence_targets:group_dm_channel_recipient_ids(Data, SelfUserId),
    flush_pending_presences_for_ids(RecipientIds, State).

-spec flush_added_recipient_pending_presences(map(), session_state()) ->
    {session_state(), [user_id()]}.
flush_added_recipient_pending_presences(Data, State) ->
    case presence_user_id(Data) of
        undefined -> {State, []};
        RecipientId -> flush_pending_presences_for_ids([RecipientId], State)
    end.

-spec flush_pending_presences_for_ids([user_id()], session_state()) ->
    {session_state(), [user_id()]}.
flush_pending_presences_for_ids(UserIds, State) ->
    lists:foldl(
        fun(UserId, {AccState, Flushed}) ->
            {flush_pending_presences(UserId, AccState), [UserId | Flushed]}
        end,
        {State, []},
        UserIds
    ).

-spec maybe_flush_relationship_pending_presences(map(), session_state()) ->
    {session_state(), [user_id()]}.
maybe_flush_relationship_pending_presences(Data, State) ->
    case maps:get(<<"type">>, Data, undefined) of
        1 ->
            TargetId = relationship_target_id(Data),
            {flush_pending_presences(TargetId, State), flushed_id_list(TargetId)};
        _ ->
            {State, []}
    end.

-spec flushed_id_list(user_id() | undefined) -> [user_id()].
flushed_id_list(undefined) -> [];
flushed_id_list(Id) -> [Id].

-spec flush_pending_presences(user_id() | undefined, session_state()) -> session_state().
flush_pending_presences(undefined, State) ->
    State;
flush_pending_presences(UserId, State) ->
    PendingQ = ensure_queue(maps:get(pending_presences, State, [])),
    PendingList = queue:to_list(PendingQ),
    {ToSend, RemainingList} = lists:partition(
        fun(P) -> maps:get(user_id, P, undefined) =:= UserId end,
        PendingList
    ),
    FlushedState = lists:foldl(
        fun dispatch_presence_now/2, State, ToSend
    ),
    FlushedState#{pending_presences => queue:from_list(RemainingList)}.

-spec dispatch_presence_now(map(), session_state()) -> session_state().
dispatch_presence_now(P, State) ->
    Event = maps:get(event, P),
    Data = maps:get(data, P),
    Seq = maps:get(seq, State),
    Buffer = maps:get(buffer, State),
    SocketPid = maps:get(socket_pid, State, undefined),
    NewSeq = Seq + 1,
    Request = #{event => Event, data => Data, seq => NewSeq},
    Deque =
        case is_list(Buffer) of
            true ->
                limited_deque:from_list(
                    Buffer, ?MAX_EVENT_BUFFER_SIZE, ?MAX_TOTAL_BUFFER_BYTES
                );
            false ->
                Buffer
        end,
    NewBuffer = limited_deque:push(Request, Deque),
    send_to_socket(SocketPid, Event, Data, NewSeq),
    State#{
        seq => NewSeq,
        buffer => NewBuffer,
        buffer_bytes => limited_deque:bytes(NewBuffer)
    }.

-spec flush_all_pending_presences(session_state()) -> session_state().
flush_all_pending_presences(State) ->
    PendingQ = ensure_queue(maps:get(pending_presences, State, [])),
    PendingList = queue:to_list(PendingQ),
    FlushedState = lists:foldl(
        fun dispatch_presence_now/2, State, PendingList
    ),
    FlushedState#{pending_presences => queue:new()}.

-spec maybe_sync_presence_targets(event(), [user_id()], session_state()) -> session_state().
maybe_sync_presence_targets(Event, FlushedIds, State) ->
    case event_changes_presence_targets(Event) of
        true -> sync_presence_targets(FlushedIds, State);
        false -> State
    end.

-spec sync_presence_targets(session_state()) -> session_state().
sync_presence_targets(State) ->
    sync_presence_targets([], State).

-spec event_changes_presence_targets(event()) -> boolean().
event_changes_presence_targets(relationship_add) -> true;
event_changes_presence_targets(relationship_update) -> true;
event_changes_presence_targets(relationship_remove) -> true;
event_changes_presence_targets(channel_create) -> true;
event_changes_presence_targets(channel_update) -> true;
event_changes_presence_targets(channel_delete) -> true;
event_changes_presence_targets(channel_recipient_add) -> true;
event_changes_presence_targets(channel_recipient_remove) -> true;
event_changes_presence_targets(_) -> false.

-spec sync_presence_targets([user_id()], session_state()) -> session_state().
sync_presence_targets(FlushedIds, State) when is_map(State) ->
    PresencePid = maps:get(presence_pid, State, undefined),
    case PresencePid of
        undefined ->
            State;
        Pid when is_pid(Pid) ->
            FriendIds = presence_targets:friend_ids_from_state(State),
            GroupDmRecipients = presence_targets:group_dm_recipients_from_state(State),
            gen_server:cast(Pid, {sync_friends, FriendIds, FlushedIds}),
            gen_server:cast(Pid, {sync_group_dm_recipients, GroupDmRecipients}),
            State
    end.

-spec presence_user_id(map()) -> user_id() | undefined.
presence_user_id(Data) ->
    case maps:find(<<"user">>, Data) of
        {ok, User} when is_map(User) ->
            user_id(User);
        _ ->
            undefined
    end.

-spec user_id(map()) -> user_id() | undefined.
user_id(User) ->
    snowflake_id:parse_maybe(maps:get(<<"id">>, User, undefined)).

-spec relationship_target_id(map()) -> user_id() | undefined.
relationship_target_id(Data) when is_map(Data) ->
    type_conv:extract_id(Data, <<"id">>).

-spec ensure_queue(queue:queue(T) | [T]) -> queue:queue(T).
ensure_queue(List) when is_list(List) -> queue:from_list(List);
ensure_queue(Q) -> Q.

-spec trim_queue_from_tail(queue:queue(T), non_neg_integer()) -> queue:queue(T).
trim_queue_from_tail(Queue, MaxLen) ->
    case queue:len(Queue) > MaxLen of
        true -> trim_queue_from_tail(queue:drop_r(Queue), MaxLen);
        false -> Queue
    end.

-spec send_to_socket(pid() | undefined, event(), map(), non_neg_integer()) -> ok.
send_to_socket(undefined, _Event, _Data, _Seq) ->
    ok;
send_to_socket(Pid, Event, Data, Seq) when is_pid(Pid) ->
    Pid ! {dispatch, Event, guild_data_wire:payload(Data), Seq},
    ok.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

relationship_allows_presence_test() ->
    ?assertEqual(true, relationship_allows_presence(1, #{1 => 1})),
    ?assertEqual(false, relationship_allows_presence(1, #{1 => 3})),
    ?assertEqual(false, relationship_allows_presence(1, #{1 => 0})),
    ?assertEqual(false, relationship_allows_presence(1, #{1 => 2})),
    ?assertEqual(false, relationship_allows_presence(1, #{1 => 4})),
    ?assertEqual(false, relationship_allows_presence(1, #{})),
    ?assertEqual(false, relationship_allows_presence(0, #{})),
    ok.

buffering_test_state() ->
    #{
        user_id => 1,
        suppress_presence_updates => false,
        relationships => #{},
        channels => #{
            100 => #{
                <<"id">> => <<"100">>,
                <<"type">> => 1,
                <<"recipients">> => [#{<<"id">> => <<"2">>, <<"username">> => <<"dm-user">>}]
            },
            200 => #{
                <<"id">> => <<"200">>,
                <<"type">> => 3,
                <<"recipients">> => [#{<<"id">> => <<"3">>, <<"username">> => <<"gdm-user">>}]
            }
        }
    }.

presence_data(UserIdBin) ->
    #{<<"user">> => #{<<"id">> => UserIdBin}, <<"status">> => <<"online">>}.

should_buffer_presence_buffers_one_to_one_dm_recipient_test() ->
    State = buffering_test_state(),
    ?assertEqual(
        true, should_buffer_presence(presence_update, presence_data(<<"2">>), State)
    ).

should_buffer_presence_passes_group_dm_recipient_test() ->
    State = buffering_test_state(),
    ?assertEqual(
        false, should_buffer_presence(presence_update, presence_data(<<"3">>), State)
    ).

should_buffer_presence_buffers_unrelated_user_test() ->
    State = buffering_test_state(),
    ?assertEqual(
        true, should_buffer_presence(presence_update, presence_data(<<"99">>), State)
    ).

should_buffer_presence_passes_friend_test() ->
    State = (buffering_test_state())#{channels => #{}, relationships => #{4 => 1}},
    ?assertEqual(
        false, should_buffer_presence(presence_update, presence_data(<<"4">>), State)
    ).

one_to_one_dm_presence_passes_attached_guild_scope_test() ->
    State = (buffering_test_state())#{guilds => #{42 => undefined}},
    Data = (presence_data(<<"2">>))#{<<"guild_id">> => <<"42">>},
    ?assertEqual(false, should_buffer_presence(presence_update, Data, State)).

one_to_one_dm_presence_buffers_unattached_guild_scope_test() ->
    State = (buffering_test_state())#{guilds => #{42 => undefined}},
    Data = (presence_data(<<"2">>))#{<<"guild_id">> => <<"99">>},
    ?assertEqual(true, should_buffer_presence(presence_update, Data, State)).

flush_test_state(PendingUserIds) ->
    #{
        user_id => 1,
        seq => 0,
        buffer => [],
        socket_pid => undefined,
        pending_presences => [
            #{
                event => presence_update,
                data => presence_data(integer_to_binary(Uid)),
                user_id => Uid
            }
         || Uid <- PendingUserIds
        ]
    }.

channel_create_does_not_flush_one_to_one_dm_recipient_presence_test() ->
    State = flush_test_state([2, 99]),
    ChannelData = #{
        <<"id">> => <<"100">>,
        <<"type">> => 1,
        <<"recipients">> => [#{<<"id">> => <<"2">>, <<"username">> => <<"dm-user">>}]
    },
    {NewState, FlushedIds} = maybe_flush_pending_presences(
        channel_create, ChannelData, State
    ),
    ?assertEqual([], FlushedIds),
    ?assertEqual(0, maps:get(seq, NewState)),
    Remaining = queue:to_list(ensure_queue(maps:get(pending_presences, NewState))),
    ?assertEqual([2, 99], [maps:get(user_id, P) || P <- Remaining]).

channel_create_ignores_guild_channels_test() ->
    State = flush_test_state([2]),
    ChannelData = #{
        <<"id">> => <<"100">>,
        <<"type">> => 0,
        <<"recipients">> => [#{<<"id">> => <<"2">>, <<"username">> => <<"dm-user">>}]
    },
    {NewState, FlushedIds} = maybe_flush_pending_presences(
        channel_create, ChannelData, State
    ),
    ?assertEqual([], FlushedIds),
    ?assertEqual(0, maps:get(seq, NewState)).

channel_recipient_add_flushes_pending_presence_test() ->
    State = flush_test_state([3, 99]),
    Data = #{
        <<"channel_id">> => <<"200">>,
        <<"user">> => #{<<"id">> => <<"3">>, <<"username">> => <<"gdm-user">>}
    },
    {NewState, FlushedIds} = maybe_flush_pending_presences(
        channel_recipient_add, Data, State
    ),
    ?assertEqual([3], FlushedIds),
    Remaining = queue:to_list(ensure_queue(maps:get(pending_presences, NewState))),
    ?assertEqual([99], [maps:get(user_id, P) || P <- Remaining]).

relationship_update_top_level_id_flushes_pending_presence_test() ->
    State = flush_test_state([2, 99]),
    Data = #{<<"id">> => <<"2">>, <<"type">> => 1, <<"user">> => #{<<"id">> => <<"2">>}},
    {NewState, FlushedIds} = maybe_flush_pending_presences(relationship_update, Data, State),
    ?assertEqual([2], FlushedIds),
    Remaining = queue:to_list(ensure_queue(maps:get(pending_presences, NewState))),
    ?assertEqual([99], [maps:get(user_id, P) || P <- Remaining]).

presence_user_id_test() ->
    ?assertEqual(123, presence_user_id(#{<<"user">> => #{<<"id">> => <<"123">>}})),
    ?assertEqual(undefined, presence_user_id(#{<<"user">> => #{<<"id">> => <<"001">>}})),
    ?assertEqual(undefined, presence_user_id(#{<<"user">> => #{}})),
    ?assertEqual(undefined, presence_user_id(#{})),
    ?assertEqual(undefined, presence_user_id(#{<<"user">> => not_a_map})),
    ok.

event_changes_presence_targets_test() ->
    ?assertEqual(true, event_changes_presence_targets(relationship_add)),
    ?assertEqual(true, event_changes_presence_targets(relationship_update)),
    ?assertEqual(true, event_changes_presence_targets(relationship_remove)),
    ?assertEqual(true, event_changes_presence_targets(channel_create)),
    ?assertEqual(true, event_changes_presence_targets(channel_update)),
    ?assertEqual(true, event_changes_presence_targets(channel_delete)),
    ?assertEqual(true, event_changes_presence_targets(channel_recipient_add)),
    ?assertEqual(true, event_changes_presence_targets(channel_recipient_remove)),
    ?assertEqual(false, event_changes_presence_targets(presence_update)),
    ?assertEqual(false, event_changes_presence_targets(message_create)),
    ?assertEqual(false, event_changes_presence_targets(guild_member_update)),
    ?assertEqual(false, event_changes_presence_targets(typing_start)),
    ok.

flushed_id_list_test() ->
    ?assertEqual([], flushed_id_list(undefined)),
    ?assertEqual([42], flushed_id_list(42)),
    ok.

-endif.
