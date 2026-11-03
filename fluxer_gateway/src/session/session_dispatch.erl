%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_dispatch).
-typing([eqwalizer]).

-export([
    handle_dispatch/3,
    flush_all_pending_presences/1,
    flush_reaction_buffer/1,
    replay_floor_after_eviction/2
]).

-export_type([session_state/0, event/0]).

-define(MAX_EVENT_BUFFER_SIZE, 4096).
-define(MAX_SINGLE_EVENT_BUFFER_BYTES, 2097152).
-define(MAX_TOTAL_BUFFER_BYTES, 16777216).

-type session_state() :: session:session_state().
-type event() :: atom() | binary().

-spec handle_dispatch(event(), map() | list() | {pre_encoded, binary()}, session_state()) ->
    {noreply, session_state()}.
handle_dispatch(Event, {pre_encoded, _} = Data, State) ->
    case skip_pre_encoded(Event, Data, State) of
        true -> {noreply, State};
        false -> do_handle_dispatch_pre_encoded(Event, Data, State)
    end;
handle_dispatch(Event, Data, State) ->
    case session_thread_backstop:check(Event, Data, State) of
        drop -> {noreply, State};
        {ok, Data1} -> handle_plain_dispatch(Event, Data1, State)
    end.

-spec skip_pre_encoded(event(), {pre_encoded, binary()}, session_state()) -> boolean().
skip_pre_encoded(Event, Data, State) ->
    should_skip_for_shard(Event, Data, State) orelse
        should_ignore_event(Event, Data, State) orelse
        session_thread_backstop:check(Event, #{}, State) =:= drop.

-spec handle_plain_dispatch(event(), map() | list(), session_state()) ->
    {noreply, session_state()}.
handle_plain_dispatch(Event, Data, State) ->
    case
        should_skip_for_shard(Event, Data, State) orelse should_ignore_event(Event, Data, State)
    of
        true -> {noreply, State};
        false -> route_bot_guild_event(Event, Data, State)
    end.

-spec route_bot_guild_event(event(), map() | list(), session_state()) ->
    {noreply, session_state()}.
route_bot_guild_event(Event, Data, State) when is_map(Data) ->
    {BotData, BotState} = session_bot_guilds:guild_event(Event, Data, State),
    route_dispatch(Event, BotData, BotState);
route_bot_guild_event(Event, Data, State) ->
    route_dispatch(Event, Data, State).

-spec should_skip_for_shard(
    event(), map() | list() | {pre_encoded, binary()}, session_state()
) ->
    boolean().
should_skip_for_shard(Event, {pre_encoded, EncodedData}, State) ->
    case shard_filter_active(State) of
        true -> should_skip_pre_encoded_for_shard(Event, EncodedData);
        false -> false
    end;
should_skip_for_shard(Event, Data, State) when is_map(Data) ->
    case shard_filter_active(State) of
        true -> not has_guild_context(Event, Data);
        false -> false
    end;
should_skip_for_shard(_Event, Data, State) when is_list(Data) ->
    shard_filter_active(State).

-spec should_skip_pre_encoded_for_shard(event(), binary()) -> boolean().
should_skip_pre_encoded_for_shard(Event, EncodedData) ->
    case decode_pre_encoded_data(EncodedData) of
        {ok, Data} -> not has_guild_context(Event, Data);
        error -> false
    end.

-spec shard_filter_active(session_state()) -> boolean().
shard_filter_active(State) ->
    case maps:get(shard, State, undefined) of
        {ShardId, _NumShards} when ShardId =/= 0 -> true;
        _Other -> false
    end.

-spec decode_pre_encoded_data(binary()) -> {ok, map()} | error.
decode_pre_encoded_data(EncodedData) ->
    try json:decode(EncodedData) of
        Data when is_map(Data) -> {ok, Data};
        _Other -> error
    catch
        error:_Reason -> error;
        exit:_Reason -> error
    end.

-spec has_guild_context(event(), map()) -> boolean().
has_guild_context(Event, Data) ->
    case session_reply_event(Event) orelse has_nonempty_field(<<"guild_id">>, Data) of
        true -> true;
        false -> guild_id_event(Event) andalso has_nonempty_field(<<"id">>, Data)
    end.

-spec session_reply_event(event()) -> boolean().
session_reply_event(Event) ->
    case event_name(Event) of
        <<"RATE_LIMITED">> -> true;
        <<"GUILD_COUNTS_UPDATE">> -> true;
        <<"CHANNEL_MEMBER_COUNTS_UPDATE">> -> true;
        _Other -> false
    end.

-spec has_nonempty_field(binary(), map()) -> boolean().
has_nonempty_field(Key, Data) ->
    case maps:get(Key, Data, undefined) of
        Value when is_integer(Value), Value > 0 -> true;
        Value when is_binary(Value), byte_size(Value) > 0 -> true;
        _Other -> false
    end.

-spec guild_id_event(event()) -> boolean().
guild_id_event(Event) ->
    case event_name(Event) of
        <<"GUILD_CREATE">> -> true;
        <<"GUILD_UPDATE">> -> true;
        <<"GUILD_DELETE">> -> true;
        <<"GUILD_SYNC">> -> true;
        _Other -> false
    end.

-spec route_dispatch(event(), map() | list(), session_state()) -> {noreply, session_state()}.
route_dispatch(Event, Data, State) when is_map(Data) ->
    case session_dispatch_voice:should_buffer_reaction(Event, State) of
        true ->
            {noreply, session_dispatch_voice:buffer_reaction(Data, State)};
        false ->
            route_after_reaction(Event, Data, State)
    end;
route_dispatch(Event, Data, State) ->
    route_after_reaction(Event, Data, State).

-spec route_after_reaction(event(), map() | list(), session_state()) ->
    {noreply, session_state()}.
route_after_reaction(Event, Data, State) ->
    case session_dispatch_voice:maybe_cancel_buffered_reaction(Event, Data, State) of
        {cancelled, NewState} ->
            {noreply, NewState};
        not_applicable ->
            route_after_cancel(Event, Data, State)
    end.

-spec route_after_cancel(event(), map() | list(), session_state()) ->
    {noreply, session_state()}.
route_after_cancel(Event, Data, State) when is_map(Data) ->
    case session_dispatch_presence:should_buffer_presence(Event, Data, State) of
        true ->
            {noreply, session_dispatch_presence:buffer_presence(Event, Data, State)};
        false ->
            do_handle_dispatch(Event, Data, State)
    end;
route_after_cancel(Event, Data, State) ->
    do_handle_dispatch(Event, Data, State).

-spec do_handle_dispatch(event(), map() | list(), session_state()) ->
    {noreply, session_state()}.
do_handle_dispatch(Event, Data, State) ->
    Seq = maps:get(seq, State),
    NewSeq = Seq + 1,
    case should_skip_replay_buffer(Event) of
        true ->
            dispatch_without_replay(Event, Data, NewSeq, State);
        false ->
            dispatch_replayable_event(Event, Data, NewSeq, State)
    end.

-spec dispatch_replayable_event(event(), map() | list(), non_neg_integer(), session_state()) ->
    {noreply, session_state()}.
dispatch_replayable_event(Event, Data, NewSeq, State) ->
    Request = #{event => Event, data => Data, seq => NewSeq},
    RequestBytes = buffer_entry_bytes(Request),
    case is_oversized_event(RequestBytes) of
        true ->
            dispatch_without_replay(Event, Data, NewSeq, State#{replay_floor => NewSeq});
        false ->
            dispatch_with_replay(Event, Data, NewSeq, Request, RequestBytes, State)
    end.

-spec dispatch_with_replay(
    event(), map() | list(), non_neg_integer(), map(), non_neg_integer(), session_state()
) ->
    {noreply, session_state()}.
dispatch_with_replay(Event, Data, NewSeq, Request, RequestBytes, State) ->
    Buffer = maps:get(buffer, State),
    Deque =
        case is_list(Buffer) of
            true ->
                limited_deque:from_list(
                    Buffer, ?MAX_EVENT_BUFFER_SIZE, ?MAX_TOTAL_BUFFER_BYTES
                );
            false ->
                Buffer
        end,
    {NewBuffer, Dropped} = limited_deque:push_trimmed(Request, RequestBytes, Deque),
    send_to_socket(maps:get(socket_pid, State, undefined), Event, Data, NewSeq),
    StateAfterMain = apply_state_updates(Event, Data, State, #{
        seq => NewSeq,
        buffer => NewBuffer,
        buffer_bytes => limited_deque:bytes(NewBuffer),
        replay_floor => replay_floor_after_eviction(Dropped, maps:get(replay_floor, State, 0))
    }),
    finalize_dispatch(Event, Data, StateAfterMain).

-spec dispatch_without_replay(event(), map() | list(), non_neg_integer(), session_state()) ->
    {noreply, session_state()}.
dispatch_without_replay(Event, Data, NewSeq, State) ->
    send_to_socket(maps:get(socket_pid, State, undefined), Event, Data, NewSeq),
    StateAfterMain = apply_state_updates(Event, Data, State, #{seq => NewSeq}),
    finalize_dispatch(Event, Data, StateAfterMain).

-spec apply_state_updates(event(), map() | list(), session_state(), map()) -> session_state().
apply_state_updates(Event, Data, State, Extra) ->
    S1 = session_dispatch_guild:update_channels_map(Event, Data, State),
    S2 = session_dispatch_guild:update_dm_voice_states_map(Event, Data, S1),
    S3 = session_dispatch_guild:update_relationships_map(Event, Data, S2),
    maps:merge(S3, Extra).

-spec finalize_dispatch(event(), map() | list(), session_state()) ->
    {noreply, session_state()}.
finalize_dispatch(Event, Data, State) ->
    {S1, FlushedIds} = session_dispatch_presence:maybe_flush_pending_presences(
        Event, Data, State
    ),
    {noreply, session_dispatch_presence:maybe_sync_presence_targets(Event, FlushedIds, S1)}.

-spec do_handle_dispatch_pre_encoded(event(), {pre_encoded, binary()}, session_state()) ->
    {noreply, session_state()}.
do_handle_dispatch_pre_encoded(Event, {pre_encoded, EncodedData} = Data, State) ->
    Seq = maps:get(seq, State),
    NewSeq = Seq + 1,
    BufferedState = buffer_pre_encoded_event(Event, Data, NewSeq, State),
    send_to_socket(maps:get(socket_pid, State, undefined), Event, Data, NewSeq),
    StateAfterMain =
        case needs_state_update(Event) of
            true -> apply_pre_encoded_state_update(Event, EncodedData, BufferedState, NewSeq);
            false -> BufferedState#{seq => NewSeq}
        end,
    {noreply, StateAfterMain}.

-spec buffer_pre_encoded_event(
    event(), {pre_encoded, binary()}, non_neg_integer(), session_state()
) ->
    session_state().
buffer_pre_encoded_event(Event, Data, NewSeq, State) ->
    case should_buffer_pre_encoded(Event) of
        false -> State;
        true -> buffer_request(#{event => Event, data => Data, seq => NewSeq}, NewSeq, State)
    end.

-spec buffer_request(map(), non_neg_integer(), session_state()) -> session_state().
buffer_request(Request, NewSeq, State) ->
    RequestBytes = buffer_entry_bytes(Request),
    case is_oversized_event(RequestBytes) of
        true -> State#{replay_floor => NewSeq};
        false -> push_buffered_request(Request, RequestBytes, State)
    end.

-spec push_buffered_request(map(), non_neg_integer(), session_state()) -> session_state().
push_buffered_request(Request, RequestBytes, State) ->
    Deque = buffer_deque(maps:get(buffer, State)),
    {NewBuffer, Dropped} = limited_deque:push_trimmed(Request, RequestBytes, Deque),
    State#{
        buffer => NewBuffer,
        buffer_bytes => limited_deque:bytes(NewBuffer),
        replay_floor => replay_floor_after_eviction(Dropped, maps:get(replay_floor, State, 0))
    }.

-spec buffer_deque(list() | limited_deque:deque()) -> limited_deque:deque().
buffer_deque(Buffer) when is_list(Buffer) ->
    limited_deque:from_list(Buffer, ?MAX_EVENT_BUFFER_SIZE, ?MAX_TOTAL_BUFFER_BYTES);
buffer_deque(Buffer) ->
    Buffer.

-spec apply_pre_encoded_state_update(event(), binary(), session_state(), non_neg_integer()) ->
    session_state().
apply_pre_encoded_state_update(Event, EncodedData, State, NewSeq) ->
    case json:decode(EncodedData) of
        DecodedData when is_map(DecodedData) ->
            S1 = apply_state_updates(Event, DecodedData, State, #{seq => NewSeq}),
            {S2, FlushedIds} = session_dispatch_presence:maybe_flush_pending_presences(
                Event, DecodedData, S1
            ),
            session_dispatch_presence:maybe_sync_presence_targets(Event, FlushedIds, S2);
        _ ->
            State#{seq => NewSeq}
    end.

-spec needs_state_update(event()) -> boolean().
needs_state_update(channel_create) -> true;
needs_state_update(channel_update) -> true;
needs_state_update(channel_delete) -> true;
needs_state_update(channel_recipient_add) -> true;
needs_state_update(channel_recipient_remove) -> true;
needs_state_update(relationship_add) -> true;
needs_state_update(relationship_update) -> true;
needs_state_update(relationship_remove) -> true;
needs_state_update(_) -> false.

-spec is_oversized_event(non_neg_integer()) -> boolean().
is_oversized_event(RequestBytes) ->
    RequestBytes > ?MAX_SINGLE_EVENT_BUFFER_BYTES.

-spec should_buffer_pre_encoded(event()) -> boolean().
should_buffer_pre_encoded(Event) ->
    not should_skip_replay_buffer(Event).

-spec should_skip_replay_buffer(event()) -> boolean().
should_skip_replay_buffer(Event) ->
    case event_name(Event) of
        <<"GUILD_MEMBERS_CHUNK">> -> true;
        <<"GUILD_MEMBER_LIST_UPDATE">> -> true;
        <<"GUILD_SYNC">> -> true;
        <<"THREAD_LIST_SYNC">> -> true;
        <<"THREAD_MEMBER_LIST_UPDATE">> -> true;
        _Other -> false
    end.

-spec buffer_entry_bytes(term()) -> non_neg_integer().
buffer_entry_bytes(Request) ->
    limited_deque:entry_bytes(Request).

-spec replay_floor_after_eviction([term()], non_neg_integer()) -> non_neg_integer().
replay_floor_after_eviction(Dropped, Floor) ->
    lists:foldl(fun evicted_seq_max/2, Floor, Dropped).

-spec evicted_seq_max(term(), non_neg_integer()) -> non_neg_integer().
evicted_seq_max(Event, Floor) when is_map(Event) ->
    case maps:get(seq, Event, undefined) of
        Seq when is_integer(Seq), Seq > Floor -> Seq;
        _Other -> Floor
    end;
evicted_seq_max(_Event, Floor) ->
    Floor.

-spec send_to_socket(pid() | undefined, event(), term(), non_neg_integer()) -> ok.
send_to_socket(undefined, _Event, _Data, _Seq) ->
    ok;
send_to_socket(Pid, Event, Data, Seq) when is_pid(Pid) ->
    Pid ! {dispatch, Event, guild_data_wire:payload(Data), Seq},
    ok.

-spec should_ignore_event(event(), map() | list() | {pre_encoded, binary()}, session_state()) ->
    boolean().
should_ignore_event(Event, Data, State) ->
    IgnoredEvents = maps:get(ignored_events, State, #{}),
    case event_name(Event) of
        undefined ->
            false;
        EventName ->
            maps:is_key(EventName, IgnoredEvents) andalso
                not ignored_event_must_dispatch(Event, Data, State)
    end.

-spec event_name(event()) -> binary() | undefined.
event_name(Event) when is_binary(Event) -> Event;
event_name(Event) when is_atom(Event) ->
    try constants:dispatch_event_atom(Event) of
        Name when is_binary(Name) -> Name
    catch
        error:_Reason -> undefined;
        exit:_Reason -> undefined
    end;
event_name(_) ->
    undefined.

-spec ignored_event_must_dispatch(
    event(), map() | list() | {pre_encoded, binary()}, session_state()
) ->
    boolean().
ignored_event_must_dispatch(message_create, {pre_encoded, EncodedData}, State) ->
    case decode_pre_encoded_data(EncodedData) of
        {ok, Data} -> ignored_event_must_dispatch(message_create, Data, State);
        error -> false
    end;
ignored_event_must_dispatch(message_create, Data, State) when is_map(Data) ->
    session_passive:is_user_mentioned(Data, State);
ignored_event_must_dispatch(_, _Data, _State) ->
    false.

-spec flush_all_pending_presences(session_state()) -> session_state().
flush_all_pending_presences(State) ->
    session_dispatch_presence:flush_all_pending_presences(State).

-spec flush_reaction_buffer(session_state()) -> session_state().
flush_reaction_buffer(State) ->
    session_dispatch_voice:flush_reaction_buffer(fun do_handle_dispatch/3, State).
