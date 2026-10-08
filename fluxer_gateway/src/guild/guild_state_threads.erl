%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_state_threads).
-typing([eqwalizer]).

-export([
    apply_event/3,
    after_regular_event/3,
    after_replayed_event/3,
    thread_context/2
]).

-type guild_state() :: map().

-define(STORE_EVENT_KEYS, [
    <<"id">>, <<"channel_id">>, <<"type">>, <<"ids">>, <<"last_pin_timestamp">>, <<"user">>
]).

-spec apply_event(atom(), map(), guild_state()) -> guild_state().
apply_event(Event, Data, State) ->
    case guild_thread_gate:store(State) of
        undefined -> State;
        Tab -> apply_store_event(Event, Data, Tab, State)
    end.

-spec apply_store_event(atom(), map(), ets:table(), guild_state()) -> guild_state().
apply_store_event(thread_create, Data, Tab, State) ->
    Thread = normalize_thread(Data),
    case archived(Thread) of
        true ->
            State;
        false ->
            ok = guild_thread_store:put_thread(Tab, Thread),
            ok = put_members(Tab, thread_members(Data), State),
            bump_forum_parent(Thread, State)
    end;
apply_store_event(thread_update, Data, Tab, State) ->
    Thread = normalize_thread(Data),
    ThreadId = maps:get(<<"id">>, Thread),
    case archived(Thread) of
        true ->
            ok = guild_thread_store:remove_thread(Tab, ThreadId),
            State;
        false ->
            Known = guild_thread_store:get_thread(Tab, ThreadId) =/= undefined,
            ok = guild_thread_store:put_thread(Tab, Thread),
            case Known of
                true -> State;
                false -> restore_members(Tab, Data, State)
            end
    end;
apply_store_event(thread_delete, Data, Tab, State) ->
    case snowflake_id:parse_maybe(maps:get(<<"id">>, Data, undefined)) of
        ThreadId when is_integer(ThreadId) ->
            ok = guild_thread_store:remove_thread(Tab, ThreadId);
        _ ->
            ok
    end,
    State;
apply_store_event(thread_member_update, Data, Tab, State) ->
    Member = guild_data_normalize:thread_member(Data),
    case member_key(Member) of
        {ThreadId, UserId} ->
            case guild_thread_store:get_thread(Tab, ThreadId) of
                undefined -> ok;
                _ -> guild_thread_store:put_member(Tab, ThreadId, UserId, Member)
            end;
        undefined ->
            ok
    end,
    State;
apply_store_event(thread_members_update, Data, Tab, State) ->
    ThreadId = snowflake_id:parse_maybe(maps:get(<<"id">>, Data, undefined)),
    ok = ensure_thread(Tab, ThreadId, Data),
    case
        is_integer(ThreadId) andalso guild_thread_store:get_thread(Tab, ThreadId) =/= undefined
    of
        true ->
            ok = put_members(Tab, list_field(<<"added_members">>, Data), State),
            lists:foreach(
                fun(UserId) -> guild_thread_store:remove_member(Tab, ThreadId, UserId) end,
                parse_ids(maps:get(<<"removed_member_ids">>, Data, []))
            ),
            ok = update_member_count(Tab, ThreadId, Data);
        false ->
            ok
    end,
    State;
apply_store_event(_Event, _Data, _Tab, State) ->
    State.

-spec ensure_thread(ets:table(), integer() | undefined, map()) -> ok.
ensure_thread(Tab, ThreadId, Data) when is_integer(ThreadId) ->
    case
        {
            guild_thread_store:get_thread(Tab, ThreadId),
            maps:get(<<"_fluxer_thread">>, Data, undefined)
        }
    of
        {undefined, Context} when is_map(Context) ->
            Thread = normalize_thread(Context),
            case archived(Thread) of
                true -> ok;
                false -> guild_thread_store:put_thread(Tab, Thread)
            end;
        {Existing, Context} when is_map(Existing), is_map(Context) ->
            guild_thread_store:put_thread(Tab, maps:merge(Existing, normalize_thread(Context)));
        _ ->
            ok
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

-spec restore_members(ets:table(), map(), guild_state()) -> guild_state().
restore_members(Tab, Data, State) ->
    ok = put_members(Tab, list_field(<<"_fluxer_members">>, Data), State),
    State.

-spec put_members(ets:table(), [map()], guild_state()) -> ok.
put_members(Tab, Members, _State) ->
    lists:foreach(
        fun(Raw) ->
            Member = guild_data_normalize:thread_member(Raw),
            case member_key(Member) of
                {ThreadId, UserId} ->
                    case guild_thread_store:get_thread(Tab, ThreadId) of
                        undefined -> ok;
                        _ -> guild_thread_store:put_member(Tab, ThreadId, UserId, Member)
                    end;
                undefined ->
                    ok
            end
        end,
        Members
    ).

-spec member_key(term()) -> {integer(), integer()} | undefined.
member_key(#{<<"id">> := ThreadId, <<"user_id">> := UserId}) when
    is_integer(ThreadId), is_integer(UserId)
->
    {ThreadId, UserId};
member_key(_) ->
    undefined.

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

-spec bump_forum_parent(map(), guild_state()) -> guild_state().
bump_forum_parent(Thread, State) ->
    Data = maps:get(data, State),
    ParentId = guild_thread_permissions:parent_id(Thread),
    Index = guild_data_index:channel_index(Data),
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

-spec after_regular_event(atom(), map(), guild_state()) -> guild_state().
after_regular_event(Event, Data, State) ->
    case guild_thread_gate:store(State) of
        undefined -> State;
        Tab -> after_store_event(Event, Data, Tab, queue_while_loading(Event, Data, State))
    end.

-spec after_replayed_event(atom(), map(), guild_state()) -> guild_state().
after_replayed_event(message_create, Data, State) ->
    case guild_thread_gate:store(State) of
        undefined ->
            State;
        Tab ->
            MessageId = snowflake_id:parse_maybe(maps:get(<<"id">>, Data, undefined)),
            with_thread(Data, Tab, fun(ThreadId, Thread) ->
                case newer(MessageId, Thread) of
                    true -> count_message(ThreadId, Data, Thread);
                    false -> Thread
                end
            end),
            State
    end;
after_replayed_event(Event, Data, State) ->
    after_regular_event(Event, Data, State).

-spec queue_while_loading(atom(), map(), guild_state()) -> guild_state().
queue_while_loading(Event, Data, State) ->
    case
        store_event(Event) andalso guild_thread_load:loading(State) andalso
            not regular_channel_event(Event, Data, State)
    of
        true -> guild_thread_load:queue_event(Event, maps:with(?STORE_EVENT_KEYS, Data), State);
        false -> State
    end.

-spec regular_channel_event(atom(), map(), guild_state()) -> boolean().
regular_channel_event(Event, Data, State) when
    Event =:= message_create;
    Event =:= message_delete;
    Event =:= message_delete_bulk;
    Event =:= channel_pins_update
->
    case snowflake_id:parse_maybe(maps:get(<<"channel_id">>, Data, undefined)) of
        ChannelId when is_integer(ChannelId) ->
            maps:is_key(ChannelId, guild_data_index:channel_index(maps:get(data, State, #{})));
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

-spec after_store_event(atom(), map(), ets:table(), guild_state()) -> guild_state().
after_store_event(message_create, Data, Tab, State) ->
    with_thread(Data, Tab, fun(ThreadId, Thread) -> count_message(ThreadId, Data, Thread) end),
    State;
after_store_event(message_delete, Data, Tab, State) ->
    with_thread(Data, Tab, fun(ThreadId, Thread) ->
        case snowflake_id:parse_maybe(maps:get(<<"id">>, Data, undefined)) of
            ThreadId -> Thread;
            _ -> decrement(Thread, 1)
        end
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
            lists:foreach(
                fun(ThreadId) -> guild_thread_store:remove_thread(Tab, ThreadId) end,
                guild_thread_store:parent_thread_ids(Tab, ParentId)
            );
        _ ->
            ok
    end,
    State;
after_store_event(guild_member_remove, Data, Tab, State) ->
    _ =
        case guild_state_member:extract_user_id(Data) of
            UserId when is_integer(UserId) -> guild_thread_store:remove_user(Tab, UserId);
            _ -> []
        end,
    State;
after_store_event(_Event, _Data, _Tab, State) ->
    State.

-spec with_thread(map(), ets:table(), fun((integer(), map()) -> map())) -> ok.
with_thread(Data, Tab, Fun) ->
    case snowflake_id:parse_maybe(maps:get(<<"channel_id">>, Data, undefined)) of
        ChannelId when is_integer(ChannelId) ->
            case guild_thread_store:get_thread(Tab, ChannelId) of
                undefined -> ok;
                Thread -> guild_thread_store:put_thread(Tab, Fun(ChannelId, Thread))
            end;
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
            Last =
                case newer(MessageId, Thread) of
                    true -> MessageId;
                    false -> maps:get(<<"last_message_id">>, Thread)
                end,
            Thread#{
                <<"last_message_id">> => Last,
                <<"message_count">> => counter(Thread, <<"message_count">>) + 1,
                <<"total_message_sent">> => counter(Thread, <<"total_message_sent">>) + 1
            }
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
    case maps:get(<<"_fluxer_thread">>, Data, undefined) of
        Context when is_map(Context) ->
            Thread = normalize_thread(Context),
            case maps:get(<<"id">>, Thread, undefined) of
                ThreadId -> Thread;
                _ -> undefined
            end;
        _ ->
            undefined
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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

state() ->
    Tab = guild_thread_store:new(),
    Data = guild_data_index:normalize_map(#{
        <<"guild">> => #{<<"id">> => <<"1">>, <<"owner_id">> => <<"2">>},
        <<"channels">> => [
            #{<<"id">> => <<"100">>, <<"type">> => 0},
            #{<<"id">> => <<"200">>, <<"type">> => 15}
        ],
        <<"roles">> => [],
        <<"members">> => []
    }),
    #{
        id => 1,
        data => Data#{thread_store => Tab, thread_gate => #{active => true, version => 1}}
    }.

thread_payload(Id, Parent) ->
    #{
        <<"id">> => integer_to_binary(Id),
        <<"parent_id">> => integer_to_binary(Parent),
        <<"guild_id">> => <<"1">>,
        <<"type">> => 11,
        <<"name">> => <<"t">>,
        <<"message_count">> => 0,
        <<"total_message_sent">> => 0,
        <<"member_count">> => 1,
        <<"thread_metadata">> => #{<<"archived">> => false},
        <<"_fluxer_member_ids_preview">> => [<<"5">>],
        <<"_fluxer_members">> => [
            #{<<"id">> => integer_to_binary(Id), <<"user_id">> => <<"5">>, <<"flags">> => 1}
        ]
    }.

counters_exclude_the_starter_and_type_21_test() ->
    State0 = state(),
    Tab = guild_thread_gate:store(State0),
    State1 = apply_event(thread_create, thread_payload(300, 100), State0),
    Msg = fun(Id, Type) ->
        #{<<"id">> => integer_to_binary(Id), <<"channel_id">> => <<"300">>, <<"type">> => Type}
    end,
    State2 = after_regular_event(message_create, Msg(300, 21), State1),
    State3 = after_regular_event(message_create, Msg(301, 0), State2),
    State4 = after_regular_event(message_create, Msg(302, 0), State3),
    Thread = guild_thread_store:get_thread(Tab, 300),
    ?assertEqual(2, maps:get(<<"message_count">>, Thread)),
    ?assertEqual(2, maps:get(<<"total_message_sent">>, Thread)),
    ?assertEqual(302, maps:get(<<"last_message_id">>, Thread)),
    ?assertEqual([5], maps:get(<<"member_ids_preview">>, Thread)),
    State5 = after_regular_event(message_delete, Msg(300, 0), State4),
    State6 = after_regular_event(
        message_delete_bulk,
        #{<<"channel_id">> => <<"300">>, <<"ids">> => [<<"300">>, <<"301">>, <<"302">>]},
        State5
    ),
    Thread2 = guild_thread_store:get_thread(Tab, 300),
    ?assertEqual(0, maps:get(<<"message_count">>, Thread2)),
    ?assertEqual(2, maps:get(<<"total_message_sent">>, Thread2)),
    _ = State6,
    guild_thread_store:destroy(Tab).

out_of_order_live_messages_still_count_test() ->
    State0 = state(),
    Tab = guild_thread_gate:store(State0),
    State1 = apply_event(thread_create, thread_payload(300, 100), State0),
    Msg = fun(Id) ->
        #{<<"id">> => integer_to_binary(Id), <<"channel_id">> => <<"300">>, <<"type">> => 0}
    end,
    State2 = after_regular_event(message_create, Msg(302), State1),
    State3 = after_regular_event(message_create, Msg(301), State2),
    Thread = guild_thread_store:get_thread(Tab, 300),
    ?assertEqual(2, maps:get(<<"message_count">>, Thread)),
    ?assertEqual(2, maps:get(<<"total_message_sent">>, Thread)),
    ?assertEqual(302, maps:get(<<"last_message_id">>, Thread)),
    State4 = after_replayed_event(message_create, Msg(301), State3),
    ?assertEqual(2, maps:get(<<"message_count">>, guild_thread_store:get_thread(Tab, 300))),
    State5 = after_replayed_event(message_create, Msg(303), State4),
    Thread2 = guild_thread_store:get_thread(Tab, 300),
    ?assertEqual(3, maps:get(<<"message_count">>, Thread2)),
    ?assertEqual(303, maps:get(<<"last_message_id">>, Thread2)),
    _ = State5,
    guild_thread_store:destroy(Tab).

forum_parent_last_message_id_follows_new_posts_test() ->
    State0 = state(),
    State1 = apply_event(thread_create, thread_payload(300, 200), State0),
    Index = guild_data_index:channel_index(maps:get(data, State1)),
    ?assertEqual(300, maps:get(<<"last_message_id">>, maps:get(200, Index))),
    guild_thread_store:destroy(guild_thread_gate:store(State0)).

archive_removes_and_parent_delete_purges_test() ->
    State0 = state(),
    Tab = guild_thread_gate:store(State0),
    State1 = apply_event(thread_create, thread_payload(300, 100), State0),
    State2 = apply_event(thread_create, thread_payload(301, 100), State1),
    Archived = (thread_payload(300, 100))#{<<"thread_metadata">> => #{<<"archived">> => true}},
    State3 = apply_event(thread_update, Archived, State2),
    ?assertEqual(undefined, guild_thread_store:get_thread(Tab, 300)),
    ?assertEqual([], guild_thread_store:user_thread_ids(Tab, 5) -- [301]),
    _ = after_regular_event(channel_delete, #{<<"id">> => <<"100">>}, State3),
    ?assertEqual(0, guild_thread_store:thread_count(Tab)),
    guild_thread_store:destroy(Tab).

unarchive_restores_members_from_the_event_only_test() ->
    State0 = state(),
    Tab = guild_thread_gate:store(State0),
    Archived = (thread_payload(300, 100))#{<<"thread_metadata">> => #{<<"archived">> => true}},
    State1 = apply_event(thread_update, Archived, State0),
    State2 = apply_event(thread_update, thread_payload(300, 100), State1),
    ?assertEqual([5], guild_thread_store:member_ids(Tab, 300)),
    State3 = apply_event(thread_update, Archived, State2),
    _ = apply_event(
        thread_update, maps:remove(<<"_fluxer_members">>, thread_payload(300, 100)), State3
    ),
    ?assertNotEqual(undefined, guild_thread_store:get_thread(Tab, 300)),
    ?assertEqual([], guild_thread_store:member_ids(Tab, 300)),
    guild_thread_store:destroy(Tab).

members_update_adds_and_removes_test() ->
    State0 = state(),
    Tab = guild_thread_gate:store(State0),
    State1 = apply_event(thread_create, thread_payload(300, 100), State0),
    _ = apply_event(
        thread_members_update,
        #{
            <<"id">> => <<"300">>,
            <<"member_count">> => 2,
            <<"added_members">> => [
                #{<<"id">> => <<"300">>, <<"user_id">> => <<"6">>, <<"flags">> => 0}
            ],
            <<"removed_member_ids">> => [<<"5">>]
        },
        State1
    ),
    ?assertEqual([6], guild_thread_store:member_ids(Tab, 300)),
    ?assertEqual(2, maps:get(<<"member_count">>, guild_thread_store:get_thread(Tab, 300))),
    _ = after_regular_event(
        guild_member_remove, #{<<"user">> => #{<<"id">> => <<"6">>}}, State1
    ),
    ?assertEqual([], guild_thread_store:member_ids(Tab, 300)),
    guild_thread_store:destroy(Tab).

-endif.
