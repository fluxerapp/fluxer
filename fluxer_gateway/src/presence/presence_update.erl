%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(presence_update).
-typing([eqwalizer]).

-export([
    maybe_handle_custom_status/2,
    maybe_handle_activities/2,
    handle_user_settings_update/2,
    handle_user_update_event/2,
    handle_message_create_event/2,
    handle_message_ack_event/2,
    flush_push_buffer/1,
    maybe_update_push_eligibility/1
]).

-export_type([state/0]).

-type user_id() :: integer().
-type state() :: map().
-type push_buffer_entry() :: #{
    channel_id := integer(), message_id := integer(), params := map()
}.

-define(DEFAULT_PUSH_BUFFER_MAX_ENTRIES, 128).
-define(DEFAULT_PUSH_BUFFER_MAX_BYTES, 1048576).
-define(PUSH_BUFFER_MAX_ENTRIES_CONFIG_KEY, presence_push_buffer_max_entries).
-define(PUSH_BUFFER_MAX_BYTES_CONFIG_KEY, presence_push_buffer_max_bytes).

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

-spec maybe_handle_activities(map(), state()) -> {map(), state()}.
maybe_handle_activities(Request, State) ->
    case maps:find(<<"activities">>, Request) of
        error ->
            {Request, State};
        {ok, null} ->
            {Request#{<<"activities">> => null}, State#{activities := null}};
        {ok, Activities} when is_list(Activities) ->
            Normalized = normalize_activities(Activities),
            {Request#{<<"activities">> => Normalized}, State#{activities := Normalized}};
        _ ->
            {Request, State}
    end.

-spec normalize_activities([term()]) -> [map()] | null.
normalize_activities(Activities) ->
    Valid = [Activity || Activity <- Activities, is_map(Activity), is_valid_activity(Activity)],
    case Valid of
        [] -> null;
        List -> lists:sublist(List, 5)
    end.

-spec is_valid_activity(map()) -> boolean().
is_valid_activity(Activity) ->
    case maps:get(<<"name">>, Activity, undefined) of
        Name when is_binary(Name), byte_size(Name) > 0, byte_size(Name) =< 128 ->
            case maps:get(<<"type">>, Activity, undefined) of
                Type when is_integer(Type), Type >= 0, Type =< 5 -> true;
                _ -> false
            end;
        _ ->
            false
    end.

-spec handle_user_settings_update(map(), state()) -> state().
handle_user_settings_update(Data, State) ->
    State1 = maybe_update_custom_status(Data, State),
    maybe_force_invisible_status(Data, State1).

-spec maybe_update_custom_status(map(), state()) -> state().
maybe_update_custom_status(Data, State) ->
    case maps:find(<<"custom_status">>, Data) of
        error ->
            State;
        {ok, CustomStatus} ->
            Normalized = normalize_state_custom_status(CustomStatus),
            State#{custom_status := Normalized}
    end.

-spec maybe_force_invisible_status(map(), state()) -> state().
maybe_force_invisible_status(Data, State) ->
    case user_settings_presence_status(Data) of
        invisible ->
            update_all_session_statuses(invisible, State);
        Status when Status =:= online; Status =:= idle; Status =:= dnd ->
            maybe_clear_forced_invisible_status(Status, State);
        undefined ->
            State
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

-spec maybe_clear_forced_invisible_status(online | idle | dnd, state()) -> state().
maybe_clear_forced_invisible_status(Status, State) ->
    Sessions = maps:get(sessions, State, #{}),
    case has_invisible_session(Sessions) of
        true -> update_all_session_statuses(Status, State);
        false -> State
    end.

-spec has_invisible_session(map()) -> boolean().
has_invisible_session(Sessions) ->
    lists:any(
        fun(Session) ->
            maps:get(status, Session, offline) =:= invisible
        end,
        maps:values(Sessions)
    ).

-spec update_all_session_statuses(online | idle | dnd | invisible, state()) -> state().
update_all_session_statuses(Status, State) ->
    Sessions = maps:get(sessions, State, #{}),
    UpdatedSessions = maps:map(
        fun(_SessionId, Session) ->
            Session#{status => Status}
        end,
        Sessions
    ),
    State#{sessions => UpdatedSessions}.

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
        Params -> route_push_notification(Params, State)
    end.

-spec handle_message_ack_event(map(), state()) -> state().
handle_message_ack_event(Data, State) ->
    ChannelId = extract_snowflake(<<"channel_id">>, Data),
    MessageId = extract_snowflake(<<"message_id">>, Data),
    maybe_ack_push_buffer(ChannelId, MessageId, State).

-spec flush_push_buffer(state()) -> state().
flush_push_buffer(#{push_buffer := []} = State) ->
    State;
flush_push_buffer(#{push_buffer := Buffer} = State) ->
    Entries = lists:reverse(Buffer),
    lists:foreach(
        fun(Entry) -> push:handle_message_create(maps:get(params, Entry)) end,
        Entries
    ),
    State#{push_buffer := []}.

-spec maybe_update_push_eligibility(state()) -> state().
maybe_update_push_eligibility(State) ->
    Sessions = maps:get(sessions, State, #{}),
    case {is_push_eligible(Sessions), maps:get(push_buffer, State, [])} of
        {true, [_ | _]} -> flush_push_buffer(State);
        _ -> State
    end.

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
    Sessions = maps:get(sessions, State, #{}),
    case is_push_eligible(Sessions) of
        true ->
            FlushedState = flush_push_buffer(State),
            push:handle_message_create(Params),
            FlushedState;
        false ->
            buffer_push_notification(Params, State)
    end.

-spec build_push_create_params(user_id(), map()) -> map() | undefined.
build_push_create_params(UserId, Data) ->
    AuthorIdBin = maps:get(<<"id">>, maps:get(<<"author">>, Data, #{}), undefined),
    case parse_snowflake(<<"author_id">>, AuthorIdBin) of
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
            Buffer = maps:get(push_buffer, State, []),
            State#{push_buffer := cap_push_buffer([Entry | Buffer])}
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
    State#{push_buffer := FilteredBuffer};
ack_push_buffer(_, _, State) ->
    State.

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
    #{channel_id => ChannelId, message_id => MessageId, params => Params};
build_buffer_entry(_, _, _) ->
    undefined.

-spec is_push_eligible(map()) -> boolean().
is_push_eligible(Sessions) ->
    case map_size(Sessions) of
        0 -> true;
        _ -> all_sessions_afk(Sessions)
    end.

-spec all_sessions_afk(map()) -> boolean().
all_sessions_afk(Sessions) ->
    lists:all(fun(S) -> maps:get(afk, S, false) end, maps:values(Sessions)).

-spec extract_snowflake(binary(), map()) -> integer() | undefined.
extract_snowflake(FieldName, Data) ->
    parse_snowflake(FieldName, maps:get(FieldName, Data, undefined)).

-spec parse_snowflake(binary(), term()) -> integer() | undefined.
parse_snowflake(FieldName, Value) ->
    case validation:validate_snowflake(FieldName, Value) of
        {ok, Id} -> Id;
        {error, _, _} -> undefined
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

is_push_eligible_test() ->
    ?assertEqual(true, is_push_eligible(#{})),
    ?assertEqual(false, is_push_eligible(#{<<"s1">> => #{mobile => true, afk => false}})),
    ?assertEqual(true, is_push_eligible(#{<<"s1">> => #{mobile => true, afk => true}})),
    ?assertEqual(true, is_push_eligible(#{<<"s1">> => #{mobile => false, afk => true}})),
    ?assertEqual(false, is_push_eligible(#{<<"s1">> => #{mobile => false, afk => false}})).

custom_status_comparator_test() ->
    Expected = #{
        <<"text">> => <<"hello">>,
        <<"expires_at">> => null,
        <<"emoji_id">> => null,
        <<"emoji_name">> => null
    },
    ?assertEqual(null, custom_status_comparator(null)),
    ?assertEqual(Expected, custom_status_comparator(#{<<"text">> => <<"hello">>})).

normalize_state_custom_status_test() ->
    ?assertEqual(null, normalize_state_custom_status(null)),
    ?assertEqual(
        #{<<"text">> => <<"hi">>}, normalize_state_custom_status(#{<<"text">> => <<"hi">>})
    ),
    ?assertEqual(null, normalize_state_custom_status(<<"invalid">>)).

handle_user_settings_update_forces_invisible_status_test() ->
    State = #{
        custom_status => null,
        sessions => #{
            <<"s1">> => #{status => online, afk => false, mobile => false},
            <<"s2">> => #{status => idle, afk => false, mobile => false}
        }
    },
    Updated = handle_user_settings_update(#{<<"status">> => <<"invisible">>}, State),
    ?assertEqual(
        #{
            <<"s1">> => #{status => invisible, afk => false, mobile => false},
            <<"s2">> => #{status => invisible, afk => false, mobile => false}
        },
        maps:get(sessions, Updated)
    ).

handle_user_settings_update_does_not_force_online_status_test() ->
    State = #{
        custom_status => null,
        sessions => #{
            <<"s1">> => #{status => idle, afk => true, mobile => false}
        }
    },
    Updated = handle_user_settings_update(#{<<"status">> => <<"online">>}, State),
    ?assertEqual(maps:get(sessions, State), maps:get(sessions, Updated)).

handle_user_settings_update_visible_status_clears_forced_invisible_test() ->
    State = #{
        custom_status => null,
        sessions => #{
            <<"s1">> => #{status => invisible, afk => true, mobile => false},
            <<"s2">> => #{status => dnd, afk => false, mobile => false}
        }
    },
    Updated = handle_user_settings_update(#{<<"status">> => <<"dnd">>}, State),
    ?assertEqual(
        #{
            <<"s1">> => #{status => dnd, afk => true, mobile => false},
            <<"s2">> => #{status => dnd, afk => false, mobile => false}
        },
        maps:get(sessions, Updated)
    ).

buffer_push_notification_caps_entries_test() ->
    with_gateway_config(
        #{presence_push_buffer_max_entries => 2, presence_push_buffer_max_bytes => 0},
        fun() ->
            State0 = #{push_buffer => []},
            State1 = buffer_push_notification(push_params(1, 1), State0),
            State2 = buffer_push_notification(push_params(1, 2), State1),
            State3 = buffer_push_notification(push_params(1, 3), State2),
            MessageIds = [
                maps:get(message_id, Entry)
             || Entry <- maps:get(push_buffer, State3)
            ],
            ?assertEqual([3, 2], MessageIds)
        end
    ).

buffer_push_notification_caps_bytes_test() ->
    with_gateway_config(
        #{presence_push_buffer_max_entries => 10, presence_push_buffer_max_bytes => 1},
        fun() ->
            State = buffer_push_notification(push_params(1, 1), #{push_buffer => []}),
            ?assertEqual([], maps:get(push_buffer, State))
        end
    ).

push_params(ChannelId, MessageId) ->
    #{
        message_data => #{
            <<"channel_id">> => integer_to_binary(ChannelId),
            <<"id">> => integer_to_binary(MessageId)
        },
        user_ids => [10],
        guild_id => 0,
        author_id => 20
    }.

with_gateway_config(Config, Fun) ->
    Key = {fluxer_gateway, runtime_config},
    Previous = persistent_term:get(Key, undefined),
    persistent_term:put(Key, Config),
    try
        Fun()
    after
        restore_gateway_config(Key, Previous)
    end.

restore_gateway_config(Key, undefined) ->
    try persistent_term:erase(Key) of
        _ -> ok
    catch
        error:badarg -> ok
    end;
restore_gateway_config(Key, Previous) ->
    persistent_term:put(Key, Previous).

-endif.
