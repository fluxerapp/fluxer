%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(presence_update).
-typing([eqwalizer]).

-export([
    maybe_handle_custom_status/2,
    handle_user_settings_update/2,
    handle_user_update_event/2,
    handle_message_create_event/2,
    handle_message_ack_event/2,
    flush_push_buffer/1,
    maybe_update_push_eligibility/1,
    push_buffer_counters/0
]).

-export_type([state/0]).

-type user_id() :: integer().
-type state() :: map().
-type push_buffer_entry() :: #{
    channel_id := integer(), message_id := integer(), params := map(), buffered_at => integer()
}.

-define(DEFAULT_PUSH_BUFFER_MAX_ENTRIES, 128).
-define(DEFAULT_PUSH_BUFFER_MAX_BYTES, 1048576).
-define(PUSH_READ_MARKS_MAX_CHANNELS, 32).
-define(PUSH_BUFFER_MAX_ENTRIES_CONFIG_KEY, presence_push_buffer_max_entries).
-define(PUSH_BUFFER_MAX_BYTES_CONFIG_KEY, presence_push_buffer_max_bytes).
-define(PUSH_BUFFER_COUNTERS, presence_push_buffer_counters).
-define(PUSH_BUFFER_OVERFLOW, push_buffer_overflow).

-spec maybe_handle_custom_status(map(), state()) -> {map(), state()}.
maybe_handle_custom_status(Request, State) ->
    case maps:find(<<"custom_status">>, Request) of
        error ->
            {Request, State};
        {ok, null} ->
            {Request#{<<"custom_status">> => null}, State#{custom_status := null}};
        {ok, CustomStatus} when is_map(CustomStatus) ->
            compare_and_validate(CustomStatus, Request, State);
        _ ->
            {Request, State}
    end.

-spec handle_user_settings_update(map(), state()) -> state().
handle_user_settings_update(Data, State) ->
    State1 = maybe_update_custom_status(Data, State),
    update_settings_invisible(Data, State1).

-spec maybe_update_custom_status(map(), state()) -> state().
maybe_update_custom_status(Data, State) ->
    case maps:find(<<"custom_status">>, Data) of
        error ->
            State;
        {ok, CustomStatus} ->
            Normalized = normalize_state_custom_status(CustomStatus),
            State#{custom_status := Normalized}
    end.

-spec update_settings_invisible(map(), state()) -> state().
update_settings_invisible(Data, State) ->
    case user_settings_presence_status(Data) of
        undefined -> State;
        Status -> State#{settings_invisible => Status =:= invisible}
    end.

-spec user_settings_presence_status(map()) -> online | idle | dnd | invisible | undefined.
user_settings_presence_status(Data) ->
    case maps:find(<<"status">>, Data) of
        {ok, Status} -> normalize_user_settings_status(Status);
        error -> undefined
    end.

-spec normalize_user_settings_status(term()) -> online | idle | dnd | invisible | undefined.
normalize_user_settings_status(<<"online">>) -> online;
normalize_user_settings_status(<<"idle">>) -> idle;
normalize_user_settings_status(<<"dnd">>) -> dnd;
normalize_user_settings_status(<<"invisible">>) -> invisible;
normalize_user_settings_status(<<"offline">>) -> invisible;
normalize_user_settings_status(online) -> online;
normalize_user_settings_status(idle) -> idle;
normalize_user_settings_status(dnd) -> dnd;
normalize_user_settings_status(invisible) -> invisible;
normalize_user_settings_status(offline) -> invisible;
normalize_user_settings_status(_) -> undefined.

-spec handle_user_update_event(map(), state()) -> state().
handle_user_update_event(Data, State) ->
    CurrentUserData = maps:get(user_data, State, #{}),
    case utils:check_user_data_differs(CurrentUserData, Data) of
        true ->
            UserId = maps:get(user_id, State),
            presence_broadcast:publish_user_update_to_bus(UserId, Data, State),
            State#{user_data := Data};
        false ->
            State
    end.

-spec handle_message_create_event(map(), state()) -> state().
handle_message_create_event(Data, State) ->
    UserId = maps:get(user_id, State),
    case build_push_create_params(UserId, Data) of
        undefined -> State;
        #{author_id := UserId} -> ack_own_message(Data, State);
        Params -> route_unread_push_notification(Data, Params, State)
    end.

-spec route_unread_push_notification(map(), map(), state()) -> state().
route_unread_push_notification(Data, Params, State) ->
    ChannelId = extract_snowflake(<<"channel_id">>, Data),
    MessageId = extract_snowflake(<<"id">>, Data),
    ReadMarks = maps:get(push_read_marks, State, #{}),
    Read = is_integer(MessageId) andalso MessageId =< maps:get(ChannelId, ReadMarks, 0),
    case Read orelse push_message_params:suppresses_notifications(Data) of
        true -> State;
        false -> route_push_notification(Params, State)
    end.

-spec ack_own_message(map(), state()) -> state().
ack_own_message(Data, State) ->
    ChannelId = extract_snowflake(<<"channel_id">>, Data),
    MessageId = extract_snowflake(<<"id">>, Data),
    maybe_ack_push_buffer(ChannelId, MessageId, State).

-spec handle_message_ack_event(map(), state()) -> state().
handle_message_ack_event(Data, State) ->
    ChannelId = extract_snowflake(<<"channel_id">>, Data),
    MessageId = extract_snowflake(<<"message_id">>, Data),
    maybe_ack_push_buffer(ChannelId, MessageId, State).

-spec flush_push_buffer(state()) -> state().
flush_push_buffer(#{push_buffer := []} = State) ->
    State;
flush_push_buffer(#{push_buffer := Buffer} = State) ->
    ok = push:handle_buffered_message_creates([
        (maps:get(params, Entry))#{buffered_at => entry_buffered_at(Entry)}
     || Entry <- lists:reverse(Buffer)
    ]),
    State#{push_buffer := []}.

-spec entry_buffered_at(push_buffer_entry()) -> integer() | undefined.
entry_buffered_at(#{buffered_at := BufferedAt}) ->
    BufferedAt;
entry_buffered_at(#{message_id := MessageId}) ->
    snowflake_id:extract_timestamp(MessageId).

-spec maybe_update_push_eligibility(state()) -> state().
maybe_update_push_eligibility(State) ->
    Eligible = push_eligible(State),
    flush_when_eligible(Eligible, record_push_eligibility(Eligible, State)).

-spec flush_when_eligible(boolean(), state()) -> state().
flush_when_eligible(Eligible, State) ->
    case {Eligible, maps:get(push_buffer, State, [])} of
        {true, [_ | _]} -> flush_push_buffer(State);
        _ -> State
    end.

-spec record_push_eligibility(boolean(), state()) -> state().
record_push_eligibility(true, State) ->
    State#{push_eligible => true};
record_push_eligibility(false, State) ->
    case maps:get(push_eligible, State, true) of
        true -> announce_session_active(maps:get(user_id, State, undefined));
        false -> ok
    end,
    State#{push_eligible => false}.

-spec announce_session_active(user_id() | undefined) -> ok.
announce_session_active(UserId) when is_integer(UserId) ->
    push_outbox:note_session_active(UserId);
announce_session_active(_UserId) ->
    ok.

-spec compare_and_validate(map(), map(), state()) -> {map(), state()}.
compare_and_validate(CustomStatus, Request, State) ->
    PreviousCustomStatus = maps:get(custom_status, State, null),
    case
        custom_status_comparator(PreviousCustomStatus) =:=
            custom_status_comparator(CustomStatus)
    of
        true -> {Request#{<<"custom_status">> => PreviousCustomStatus}, State};
        false -> validate_custom_status(CustomStatus, Request, State)
    end.

-spec validate_custom_status(map(), map(), state()) -> {map(), state()}.
validate_custom_status(CustomStatus, Request, State) ->
    UserId = maps:get(user_id, State),
    case custom_status_validation:validate(UserId, CustomStatus) of
        {ok, #{<<"custom_status">> := Validated}} ->
            {Request#{<<"custom_status">> => Validated}, State#{custom_status := Validated}};
        {ok, _} ->
            {Request#{<<"custom_status">> => null}, State#{custom_status := null}};
        {error, _Reason} ->
            {Request, State}
    end.

-spec custom_status_comparator(map() | null) -> map() | null.
custom_status_comparator(null) ->
    null;
custom_status_comparator(Map) when is_map(Map) ->
    #{
        <<"text">> => field_or_null(Map, <<"text">>),
        <<"expires_at">> => field_or_null(Map, <<"expires_at">>),
        <<"emoji_id">> => field_or_null(Map, <<"emoji_id">>),
        <<"emoji_name">> => field_or_null(Map, <<"emoji_name">>)
    }.

-spec normalize_state_custom_status(term()) -> map() | null.
normalize_state_custom_status(null) -> null;
normalize_state_custom_status(Map) when is_map(Map) -> Map;
normalize_state_custom_status(_) -> null.

-spec field_or_null(map(), binary()) -> term() | null.
field_or_null(Map, Key) ->
    case maps:get(Key, Map, undefined) of
        undefined -> null;
        Value -> Value
    end.

-spec route_push_notification(map(), state()) -> state().
route_push_notification(Params, State) ->
    case push_eligible(State) of
        true ->
            FlushedState = flush_push_buffer(State),
            push:handle_message_create(Params),
            FlushedState;
        false ->
            buffer_push_notification(Params, State)
    end.

-spec push_eligible(state()) -> boolean().
push_eligible(State) ->
    no_session_holds_push(maps:get(sessions, State, #{})).

-spec build_push_create_params(user_id(), map()) -> map() | undefined.
build_push_create_params(UserId, Data) ->
    AuthorIdBin = maps:get(<<"id">>, maps:get(<<"author">>, Data, #{}), undefined),
    case guild_dispatch_decorate:parse_snowflake(<<"author_id">>, AuthorIdBin) of
        undefined ->
            undefined;
        AuthorId ->
            #{
                message_data => Data,
                user_ids => [UserId],
                guild_id => 0,
                author_id => AuthorId
            }
    end.

-spec buffer_push_notification(map(), state()) -> state().
buffer_push_notification(Params, State) ->
    case make_push_buffer_entry(Params) of
        undefined ->
            State;
        Entry ->
            Buffer = [Entry | maps:get(push_buffer, State, [])],
            Capped = cap_push_buffer(Buffer),
            ok = count_push_buffer_overflow(
                maps:get(user_id, State, undefined), length(Buffer) - length(Capped)
            ),
            State#{push_buffer := Capped}
    end.

-spec count_push_buffer_overflow(term(), non_neg_integer()) -> ok.
count_push_buffer_overflow(_UserId, 0) ->
    ok;
count_push_buffer_overflow(UserId, Dropped) ->
    ok = guild_ets_utils:ensure_table(?PUSH_BUFFER_COUNTERS, [
        named_table, public, set, {write_concurrency, true}
    ]),
    try
        _ = ets:update_counter(
            ?PUSH_BUFFER_COUNTERS,
            ?PUSH_BUFFER_OVERFLOW,
            {2, Dropped},
            {?PUSH_BUFFER_OVERFLOW, 0}
        ),
        ok
    catch
        error:badarg -> ok
    end,
    logger:warning(
        "presence_push_buffer_overflow: user_id=~p dropped=~p", [UserId, Dropped]
    ).

-spec push_buffer_counters() -> #{atom() => non_neg_integer()}.
push_buffer_counters() ->
    try ets:lookup(?PUSH_BUFFER_COUNTERS, ?PUSH_BUFFER_OVERFLOW) of
        [{?PUSH_BUFFER_OVERFLOW, Count}] when is_integer(Count), Count >= 0 ->
            #{?PUSH_BUFFER_OVERFLOW => Count};
        _ ->
            #{?PUSH_BUFFER_OVERFLOW => 0}
    catch
        error:badarg -> #{?PUSH_BUFFER_OVERFLOW => 0}
    end.

-spec cap_push_buffer([push_buffer_entry()]) -> [push_buffer_entry()].
cap_push_buffer(Buffer) ->
    MaxEntries = env_non_neg_integer(
        ?PUSH_BUFFER_MAX_ENTRIES_CONFIG_KEY, ?DEFAULT_PUSH_BUFFER_MAX_ENTRIES
    ),
    MaxBytes = env_non_neg_integer(
        ?PUSH_BUFFER_MAX_BYTES_CONFIG_KEY, ?DEFAULT_PUSH_BUFFER_MAX_BYTES
    ),
    cap_push_buffer_bytes(take_newest_push_buffer_entries(Buffer, MaxEntries), MaxBytes).

-spec take_newest_push_buffer_entries([push_buffer_entry()], non_neg_integer()) ->
    [push_buffer_entry()].
take_newest_push_buffer_entries(_Buffer, 0) ->
    [];
take_newest_push_buffer_entries(Buffer, MaxEntries) ->
    lists:sublist(Buffer, MaxEntries).

-spec cap_push_buffer_bytes([push_buffer_entry()], non_neg_integer()) -> [push_buffer_entry()].
cap_push_buffer_bytes(Buffer, 0) ->
    Buffer;
cap_push_buffer_bytes(Buffer, MaxBytes) ->
    cap_push_buffer_bytes(Buffer, MaxBytes, 0, []).

-spec cap_push_buffer_bytes(
    [push_buffer_entry()], non_neg_integer(), non_neg_integer(), [push_buffer_entry()]
) -> [push_buffer_entry()].
cap_push_buffer_bytes([], _MaxBytes, _UsedBytes, Acc) ->
    lists:reverse(Acc);
cap_push_buffer_bytes([Entry | Rest], MaxBytes, UsedBytes, Acc) ->
    EntryBytes = push_buffer_entry_bytes(Entry),
    case UsedBytes + EntryBytes =< MaxBytes of
        true -> cap_push_buffer_bytes(Rest, MaxBytes, UsedBytes + EntryBytes, [Entry | Acc]);
        false -> lists:reverse(Acc)
    end.

-spec push_buffer_entry_bytes(push_buffer_entry()) -> non_neg_integer().
push_buffer_entry_bytes(Entry) ->
    erts_debug:flat_size(Entry) * erlang:system_info(wordsize).

-spec env_non_neg_integer(atom(), non_neg_integer()) -> non_neg_integer().
env_non_neg_integer(Key, Default) ->
    case fluxer_gateway_env:get_optional(Key) of
        Value when is_integer(Value), Value >= 0 -> Value;
        _ -> Default
    end.

-spec maybe_ack_push_buffer(integer() | undefined, integer() | undefined, state()) -> state().
maybe_ack_push_buffer(ChannelId, MessageId, State) when
    is_integer(ChannelId), is_integer(MessageId)
->
    ack_push_buffer(ChannelId, MessageId, State);
maybe_ack_push_buffer(_, _, State) ->
    State.

-spec ack_push_buffer(integer(), integer(), state()) -> state().
ack_push_buffer(ChannelId, MessageId, State) when ChannelId > 0, MessageId > 0 ->
    Buffer = maps:get(push_buffer, State, []),
    FilteredBuffer = [E || E <- Buffer, not should_drop_buffer_entry(E, ChannelId, MessageId)],
    record_read_mark(ChannelId, MessageId, State#{push_buffer := FilteredBuffer});
ack_push_buffer(_, _, State) ->
    State.

-spec record_read_mark(integer(), integer(), state()) -> state().
record_read_mark(ChannelId, MessageId, State) ->
    ReadMarks = maps:get(push_read_marks, State, #{}),
    ReadMark = max(MessageId, maps:get(ChannelId, ReadMarks, 0)),
    State#{push_read_marks => cap_read_marks(ReadMarks#{ChannelId => ReadMark})}.

-spec cap_read_marks(#{integer() => integer()}) -> #{integer() => integer()}.
cap_read_marks(ReadMarks) when map_size(ReadMarks) > ?PUSH_READ_MARKS_MAX_CHANNELS ->
    {_, OldestChannelId} = lists:min([{Mark, Id} || {Id, Mark} <- maps:to_list(ReadMarks)]),
    maps:remove(OldestChannelId, ReadMarks);
cap_read_marks(ReadMarks) ->
    ReadMarks.

-spec should_drop_buffer_entry(push_buffer_entry(), integer(), integer()) -> boolean().
should_drop_buffer_entry(Entry, ChannelId, MessageId) ->
    maps:get(channel_id, Entry) =:= ChannelId andalso
        maps:get(message_id, Entry) =< MessageId.

-spec make_push_buffer_entry(map()) -> push_buffer_entry() | undefined.
make_push_buffer_entry(Params) ->
    MessageData = maps:get(message_data, Params, #{}),
    ChannelId = extract_snowflake(<<"channel_id">>, MessageData),
    MessageId = extract_snowflake(<<"id">>, MessageData),
    build_buffer_entry(ChannelId, MessageId, Params).

-spec build_buffer_entry(integer() | undefined, integer() | undefined, map()) ->
    push_buffer_entry() | undefined.
build_buffer_entry(ChannelId, MessageId, Params) when
    is_integer(ChannelId), is_integer(MessageId)
->
    #{
        channel_id => ChannelId,
        message_id => MessageId,
        params => Params,
        buffered_at => erlang:system_time(millisecond)
    };
build_buffer_entry(_, _, _) ->
    undefined.

-spec no_session_holds_push(map()) -> boolean().
no_session_holds_push(Sessions) ->
    not lists:any(fun session_holds_push/1, maps:values(Sessions)).

-spec session_holds_push(map()) -> boolean().
session_holds_push(Session) ->
    not maps:get(afk, Session, false) andalso maps:get(status, Session, online) =/= offline.

-spec extract_snowflake(binary(), map()) -> integer() | undefined.
extract_snowflake(FieldName, Data) ->
    guild_dispatch_decorate:parse_snowflake(FieldName, maps:get(FieldName, Data, undefined)).
