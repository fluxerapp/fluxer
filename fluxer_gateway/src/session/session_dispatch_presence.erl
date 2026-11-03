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
    Trimmed = trim_queue_from_front(Pending, ?MAX_PENDING_PRESENCE_BUFFER_SIZE - 1),
    NewPending = queue:in(Entry, Trimmed),
    State#{pending_presences => NewPending}.

-spec maybe_flush_pending_presences(event(), map() | list(), session_state()) ->
    {session_state(), [user_id()]}.
maybe_flush_pending_presences(relationship_add, Data, State) when is_map(Data) ->
    maybe_flush_relationship_pending_presences(Data, State);
maybe_flush_pending_presences(relationship_update, Data, State) when is_map(Data) ->
    maybe_flush_relationship_pending_presences(Data, State);
maybe_flush_pending_presences(channel_create, Data, State) ->
    flush_dm_channel_pending_presences(Data, maybe_register_dm_partners(Data, State));
maybe_flush_pending_presences(channel_update, Data, State) ->
    flush_dm_channel_pending_presences(Data, maybe_register_dm_partners(Data, State));
maybe_flush_pending_presences(channel_delete, Data, State) ->
    {maybe_register_dm_partners(Data, State), []};
maybe_flush_pending_presences(guild_delete, Data, State) ->
    {forget_deleted_guild(Data, State), []};
maybe_flush_pending_presences(channel_recipient_add, Data, State) when is_map(Data) ->
    flush_added_recipient_pending_presences(Data, State);
maybe_flush_pending_presences(_, _, State) ->
    {State, []}.

-spec maybe_register_dm_partners(term(), session_state()) -> session_state().
maybe_register_dm_partners(#{<<"type">> := 1}, State) ->
    session_dm_partners:register_all(State);
maybe_register_dm_partners(_Data, State) ->
    State.

-spec forget_deleted_guild(term(), session_state()) -> session_state().
forget_deleted_guild(#{<<"id">> := RawGuildId}, State) ->
    case snowflake_id:parse_maybe(RawGuildId) of
        GuildId when is_integer(GuildId) -> session_dm_partners:forget_guild(GuildId, State);
        _ -> State
    end;
forget_deleted_guild(_Data, State) ->
    State.

-spec flush_dm_channel_pending_presences(term(), session_state()) ->
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
    {NewBuffer, Dropped} = limited_deque:push_trimmed(
        Request, limited_deque:entry_bytes(Request), Deque
    ),
    send_to_socket(SocketPid, Event, Data, NewSeq),
    State#{
        seq => NewSeq,
        buffer => NewBuffer,
        buffer_bytes => limited_deque:bytes(NewBuffer),
        replay_floor => session_dispatch:replay_floor_after_eviction(
            Dropped, maps:get(replay_floor, State, 0)
        )
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
event_changes_presence_targets(guild_delete) -> true;
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

-spec trim_queue_from_front(queue:queue(T), non_neg_integer()) -> queue:queue(T).
trim_queue_from_front(Queue, MaxLen) ->
    case queue:len(Queue) > MaxLen of
        true -> trim_queue_from_front(queue:drop(Queue), MaxLen);
        false -> Queue
    end.

-spec send_to_socket(pid() | undefined, event(), map() | list(), non_neg_integer()) -> ok.
send_to_socket(undefined, _Event, _Data, _Seq) ->
    ok;
send_to_socket(Pid, Event, Data, Seq) when is_pid(Pid) ->
    Pid ! {dispatch, Event, guild_data_wire:payload(Data), Seq},
    ok.
