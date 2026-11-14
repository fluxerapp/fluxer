%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_state_threads).
-typing([eqwalizer]).

-export([
    apply_event/3,
    after_regular_event/3,
    after_replayed_event/3,
    thread_context/2
]).

-define(STORE_EVENT_KEYS, [
    <<"id">>, <<"channel_id">>, <<"type">>, <<"ids">>, <<"last_pin_timestamp">>, <<"user">>
]).

-spec apply_event(atom(), map(), map()) -> map().
apply_event(Event, Data, State) ->
    case guild_thread_gate:store(State) of
        undefined -> State;
        Tab -> apply_store_event(Event, Data, Tab, State)
    end.

-spec apply_store_event(atom(), map(), ets:table(), map()) -> map().
apply_store_event(thread_create, Data, Tab, State) ->
    Thread = normalize_thread(Data),
    case archived(Thread) of
        true ->
            State;
        false ->
            ok = guild_thread_store:put_thread(Tab, Thread),
            ok = put_members(Tab, thread_members(Data)),
            bump_forum_parent(Thread, State)
    end;
apply_store_event(thread_update, Data, Tab, State) ->
    ok = update_thread(Tab, Data),
    State;
apply_store_event(thread_delete, Data, Tab, State) ->
    case snowflake_id:parse_maybe(maps:get(<<"id">>, Data, undefined)) of
        ThreadId when is_integer(ThreadId) ->
            ok = guild_thread_store:remove_thread(Tab, ThreadId);
        _ ->
            ok
    end,
    State;
apply_store_event(thread_member_update, Data, Tab, State) ->
    ok = put_known_member(Tab, Data),
    State;
apply_store_event(thread_members_update, Data, Tab, State) ->
    ok = update_thread_members(Tab, Data),
    State;
apply_store_event(_Event, _Data, _Tab, State) ->
    State.

-spec update_thread(ets:table(), map()) -> ok.
update_thread(Tab, Data) ->
    Thread = normalize_thread(Data),
    ThreadId = maps:get(<<"id">>, Thread),
    RestoreMembers =
        case archived(Thread) of
            true ->
                ok = guild_thread_store:remove_thread(Tab, ThreadId),
                false;
            false ->
                Known = guild_thread_store:get_thread(Tab, ThreadId) =/= undefined,
                ok = guild_thread_store:put_thread(Tab, Thread),
                not Known
        end,
    case RestoreMembers of
        true -> ok = put_members(Tab, list_field(<<"_fluxer_members">>, Data));
        false -> ok
    end.

-spec update_thread_members(ets:table(), map()) -> ok.
update_thread_members(Tab, Data) ->
    ThreadId = snowflake_id:parse_maybe(maps:get(<<"id">>, Data, undefined)),
    ok = ensure_thread(Tab, ThreadId, Data),
    Known =
        is_integer(ThreadId) andalso guild_thread_store:get_thread(Tab, ThreadId) =/= undefined,
    case Known andalso ThreadId of
        KnownThreadId when is_integer(KnownThreadId) ->
            apply_member_changes(Tab, KnownThreadId, Data);
        false ->
            ok
    end.

-spec apply_member_changes(ets:table(), integer(), map()) -> ok.
apply_member_changes(Tab, ThreadId, Data) ->
    ok = put_members(Tab, list_field(<<"added_members">>, Data)),
    lists:foreach(
        fun(UserId) -> guild_thread_store:remove_member(Tab, ThreadId, UserId) end,
        parse_ids(maps:get(<<"removed_member_ids">>, Data, []))
    ),
    ok = update_member_count(Tab, ThreadId, Data).

-spec ensure_thread(ets:table(), integer() | undefined, map()) -> ok.
ensure_thread(Tab, ThreadId, Data) when is_integer(ThreadId) ->
    Candidate =
        case
            {
                guild_thread_store:get_thread(Tab, ThreadId),
                maps:get(<<"_fluxer_thread">>, Data, undefined)
            }
        of
            {undefined, Context} when is_map(Context) ->
                Thread = normalize_thread(Context),
                not archived(Thread) andalso Thread;
            {Existing, Context} when is_map(Existing), is_map(Context) ->
                maps:merge(Existing, normalize_thread(Context));
            _ ->
                false
        end,
    case Candidate of
        false -> ok;
        Updated -> guild_thread_store:put_thread(Tab, Updated)
    end;
ensure_thread(_Tab, _ThreadId, _Data) ->
    ok.

-spec update_member_count(ets:table(), integer(), map()) -> ok.
update_member_count(Tab, ThreadId, Data) ->
    case
        {
            guild_thread_store:get_thread(Tab, ThreadId),
            maps:get(<<"member_count">>, Data, undefined)
        }
    of
        {Thread, Count} when is_map(Thread), is_integer(Count) ->
            guild_thread_store:put_thread(Tab, Thread#{<<"member_count">> => Count});
        _ ->
            ok
    end.

-spec put_members(ets:table(), [map()]) -> ok.
put_members(Tab, Members) ->
    lists:foreach(fun(Member) -> put_known_member(Tab, Member) end, Members).

-spec put_known_member(ets:table(), map()) -> ok.
put_known_member(Tab, Raw) ->
    Member = guild_data_normalize:thread_member(Raw),
    Known =
        case Member of
            #{<<"id">> := ThreadId, <<"user_id">> := UserId} when
                is_integer(ThreadId), is_integer(UserId)
            ->
                guild_thread_store:get_thread(Tab, ThreadId) =/= undefined andalso
                    {ThreadId, UserId};
            _ ->
                false
        end,
    case Known of
        {KnownThreadId, KnownUserId} ->
            guild_thread_store:put_member(Tab, KnownThreadId, KnownUserId, Member);
        false ->
            ok
    end.

-spec thread_members(map()) -> [map()].
thread_members(Data) ->
    Members = list_field(<<"_fluxer_members">>, Data),
    case maps:get(<<"member">>, Data, undefined) of
        #{<<"user_id">> := _} = Own -> [Own#{<<"id">> => maps:get(<<"id">>, Data)} | Members];
        _ -> Members
    end.

-spec normalize_thread(map()) -> map().
normalize_thread(Data) ->
    Preview = maps:get(<<"_fluxer_member_ids_preview">>, Data, undefined),
    WithPreview =
        case Preview of
            List when is_list(List) -> Data#{<<"member_ids_preview">> => List};
            _ -> Data
        end,
    case guild_data_normalize:thread(WithPreview) of
        Thread when is_map(Thread) -> Thread;
        _ -> WithPreview
    end.

-spec archived(map()) -> boolean().
archived(#{<<"thread_metadata">> := #{<<"archived">> := true}}) -> true;
archived(_) -> false.

-spec bump_forum_parent(map(), map()) -> map().
bump_forum_parent(Thread, State) ->
    Data = maps:get(data, State),
    ParentId = guild_thread_permissions:parent_id(Thread),
    Index = guild_data_index_channels:channel_index(Data),
    case maps:get(ParentId, Index, undefined) of
        #{<<"type">> := Type} = Parent when Type =:= 15; Type =:= 16 ->
            Updated = Parent#{<<"last_message_id">> => maps:get(<<"id">>, Thread)},
            State#{
                data => Data#{
                    <<"channel_index">> => Index#{ParentId => Updated}, channels_stale => true
                }
            };
        _ ->
            State
    end.

-spec after_regular_event(atom(), map(), map()) -> map().
after_regular_event(Event, Data, State) ->
    case guild_thread_gate:store(State) of
        undefined -> State;
        Tab -> after_store_event(Event, Data, Tab, queue_while_loading(Event, Data, State))
    end.

-spec after_replayed_event(atom(), map(), map()) -> map().
after_replayed_event(message_create, Data, State) ->
    case guild_thread_gate:store(State) of
        undefined ->
            State;
        Tab ->
            ok = count_replayed_message(Data, Tab),
            State
    end;
after_replayed_event(Event, Data, State) ->
    after_regular_event(Event, Data, State).

-spec count_replayed_message(map(), ets:table()) -> ok.
count_replayed_message(Data, Tab) ->
    MessageId = snowflake_id:parse_maybe(maps:get(<<"id">>, Data, undefined)),
    with_thread(Data, Tab, fun(ThreadId, Thread) ->
        count_if_newer(MessageId, ThreadId, Data, Thread)
    end).

-spec count_if_newer(term(), integer(), map(), map()) -> map().
count_if_newer(MessageId, ThreadId, Data, Thread) ->
    case newer(MessageId, Thread) of
        true -> count_message(ThreadId, Data, Thread);
        false -> Thread
    end.

-spec queue_while_loading(atom(), map(), map()) -> map().
queue_while_loading(Event, Data, State) ->
    case
        store_event(Event) andalso guild_thread_load:loading(State) andalso
            not regular_channel_event(Event, Data, State)
    of
        true -> guild_thread_load:queue_event(Event, maps:with(?STORE_EVENT_KEYS, Data), State);
        false -> State
    end.

-spec regular_channel_event(atom(), map(), map()) -> boolean().
regular_channel_event(Event, Data, State) when
    Event =:= message_create;
    Event =:= message_delete;
    Event =:= message_delete_bulk;
    Event =:= channel_pins_update
->
    case snowflake_id:parse_maybe(maps:get(<<"channel_id">>, Data, undefined)) of
        ChannelId when is_integer(ChannelId) ->
            maps:is_key(
                ChannelId, guild_data_index_channels:channel_index(maps:get(data, State, #{}))
            );
        _ ->
            true
    end;
regular_channel_event(_Event, _Data, _State) ->
    false.

-spec store_event(atom()) -> boolean().
store_event(message_create) -> true;
store_event(message_delete) -> true;
store_event(message_delete_bulk) -> true;
store_event(channel_pins_update) -> true;
store_event(channel_delete) -> true;
store_event(guild_member_remove) -> true;
store_event(_) -> false.

-spec after_store_event(atom(), map(), ets:table(), map()) -> map().
after_store_event(message_create, Data, Tab, State) ->
    with_thread(Data, Tab, fun(ThreadId, Thread) -> count_message(ThreadId, Data, Thread) end),
    State;
after_store_event(message_delete, Data, Tab, State) ->
    with_thread(Data, Tab, fun(ThreadId, Thread) ->
        decrement_unless_starter(ThreadId, Data, Thread)
    end),
    State;
after_store_event(message_delete_bulk, Data, Tab, State) ->
    with_thread(Data, Tab, fun(ThreadId, Thread) ->
        Ids = parse_ids(maps:get(<<"ids">>, Data, [])),
        decrement(Thread, length([Id || Id <- Ids, Id =/= ThreadId]))
    end),
    State;
after_store_event(channel_pins_update, Data, Tab, State) ->
    with_thread(Data, Tab, fun(_ThreadId, Thread) ->
        Thread#{<<"last_pin_timestamp">> => maps:get(<<"last_pin_timestamp">>, Data, null)}
    end),
    State;
after_store_event(channel_delete, Data, Tab, State) ->
    case snowflake_id:parse_maybe(maps:get(<<"id">>, Data, undefined)) of
        ParentId when is_integer(ParentId) ->
            remove_parent_threads(Tab, ParentId);
        _ ->
            ok
    end,
    State;
after_store_event(guild_member_remove, Data, Tab, State) ->
    case guild_state_member:extract_user_id(Data) of
        UserId when is_integer(UserId) ->
            lists:foldl(
                fun guild_thread_subscriptions:member_lists_changed/2,
                State,
                guild_thread_store:remove_user(Tab, UserId)
            );
        _ ->
            State
    end;
after_store_event(guild_member_update, Data, _Tab, State) ->
    case guild_state_member:extract_user_id(Data) of
        UserId when is_integer(UserId) ->
            guild_thread_subscriptions:member_changed(UserId, State);
        _ ->
            State
    end;
after_store_event(_Event, _Data, _Tab, State) ->
    State.

-spec decrement_unless_starter(integer(), map(), map()) -> map().
decrement_unless_starter(ThreadId, Data, Thread) ->
    case snowflake_id:parse_maybe(maps:get(<<"id">>, Data, undefined)) of
        ThreadId -> Thread;
        _ -> decrement(Thread, 1)
    end.

-spec remove_parent_threads(ets:table(), integer()) -> ok.
remove_parent_threads(Tab, ParentId) ->
    lists:foreach(
        fun(ThreadId) -> guild_thread_store:remove_thread(Tab, ThreadId) end,
        guild_thread_store:parent_thread_ids(Tab, ParentId)
    ).

-spec with_thread(map(), ets:table(), fun((integer(), map()) -> map())) -> ok.
with_thread(Data, Tab, Fun) ->
    Entry =
        case snowflake_id:parse_maybe(maps:get(<<"channel_id">>, Data, undefined)) of
            ChannelId when is_integer(ChannelId) ->
                {ChannelId, guild_thread_store:get_thread(Tab, ChannelId)};
            _ ->
                undefined
        end,
    case Entry of
        {ThreadId, Thread} when Thread =/= undefined ->
            guild_thread_store:put_thread(Tab, Fun(ThreadId, Thread));
        _ ->
            ok
    end.

-spec count_message(integer(), map(), map()) -> map().
count_message(ThreadId, Data, Thread) ->
    MessageId = snowflake_id:parse_maybe(maps:get(<<"id">>, Data, undefined)),
    case MessageId =:= ThreadId orelse maps:get(<<"type">>, Data, 0) =:= 21 of
        true ->
            Thread;
        false ->
            Thread#{
                <<"last_message_id">> => newest_message_id(MessageId, Thread),
                <<"message_count">> => counter(Thread, <<"message_count">>) + 1,
                <<"total_message_sent">> => counter(Thread, <<"total_message_sent">>) + 1
            }
    end.

-spec newest_message_id(term(), map()) -> term().
newest_message_id(MessageId, Thread) ->
    case newer(MessageId, Thread) of
        true -> MessageId;
        false -> maps:get(<<"last_message_id">>, Thread)
    end.

-spec newer(term(), map()) -> boolean().
newer(MessageId, Thread) when is_integer(MessageId) ->
    case snowflake_id:parse_maybe(maps:get(<<"last_message_id">>, Thread, null)) of
        LastId when is_integer(LastId) -> MessageId > LastId;
        _ -> true
    end;
newer(_MessageId, _Thread) ->
    true.

-spec decrement(map(), non_neg_integer()) -> map().
decrement(Thread, N) ->
    Thread#{<<"message_count">> => max(0, counter(Thread, <<"message_count">>) - N)}.

-spec counter(map(), binary()) -> non_neg_integer().
counter(Thread, Key) ->
    case maps:get(Key, Thread, 0) of
        N when is_integer(N), N >= 0 -> N;
        _ -> 0
    end.

-spec thread_context(integer(), map()) -> map() | undefined.
thread_context(ThreadId, Data) ->
    Context =
        case maps:get(<<"_fluxer_thread">>, Data, undefined) of
            Raw when is_map(Raw) -> normalize_thread(Raw);
            _ -> undefined
        end,
    case is_map(Context) andalso maps:get(<<"id">>, Context, undefined) =:= ThreadId of
        true -> Context;
        false -> undefined
    end.

-spec parse_ids(term()) -> [integer()].
parse_ids(Ids) when is_list(Ids) ->
    [Id || Raw <- Ids, Id <- [snowflake_id:parse_maybe(Raw)], is_integer(Id)];
parse_ids(_) ->
    [].

-spec list_field(binary(), map()) -> [map()].
list_field(Key, Map) ->
    case maps:get(Key, Map, []) of
        List when is_list(List) -> [Item || Item <- List, is_map(Item)];
        _ -> []
    end.
