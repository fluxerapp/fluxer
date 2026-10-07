%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_member_list_subscribe).
-typing([eqwalizer]).

-export([
    subscribe_ranges/4,
    unsubscribe_session/2,
    send_member_list_update_to_sessions/5,
    dispatch_sync_to_subscribed_list/7,
    handle_sync_item_cache_timeout/1,
    sync_cache_find/1,
    sync_cache_store/2
]).

-type guild_state() :: map().
-type list_id() :: binary().
-type range() :: {non_neg_integer(), non_neg_integer()}.
-type channel_id() :: integer().

-export_type([guild_state/0, list_id/0, range/0, channel_id/0]).

-define(SYNC_ITEM_CACHE_KEY, guild_member_list_sync_item_cache).
-define(SYNC_ITEM_CACHE_MAX_ENTRIES, 4096).
-define(SYNC_ITEM_CACHE_GENERATION_MS, 30000).
-define(SYNC_ITEM_CACHE_TIMEOUT_MSG, member_list_sync_item_cache_rotate).

-spec subscribe_ranges(binary(), list_id(), [range()], guild_state()) ->
    {guild_state(), boolean(), [range()]}.
subscribe_ranges(SessionId, ListId, Ranges, State) ->
    case valid_list_id(ListId) of
        true ->
            NormalizedRanges = guild_member_list:normalize_ranges(Ranges),
            SubsTab = maps:get(member_list_subscriptions, State),
            {OldRanges, ShouldSync} =
                guild_member_list_subs:subscribe(SessionId, ListId, NormalizedRanges, SubsTab),
            StateWithStore = maybe_ensure_store(ListId, SubsTab, State),
            StateExclusive = enforce_single_session_list(
                SessionId, ListId, NormalizedRanges, SubsTab, StateWithStore
            ),
            RangesToSync = compute_ranges_to_sync(NormalizedRanges, OldRanges, ShouldSync),
            ShouldActuallySync = ShouldSync andalso RangesToSync =/= [],
            {StateExclusive, ShouldActuallySync, RangesToSync};
        false ->
            {State, false, []}
    end.

-spec unsubscribe_session(binary(), guild_state()) -> guild_state().
unsubscribe_session(SessionId, State) ->
    SubsTab = maps:get(member_list_subscriptions, State),
    RemovedListIds = guild_member_list_subs:unsubscribe_session(SessionId, SubsTab),
    lists:foldl(
        fun(ListId, AccState) -> maybe_drop_channel_engine(ListId, SubsTab, AccState) end,
        State,
        RemovedListIds
    ).

-spec enforce_single_session_list(
    binary(), list_id(), [range()], ets:table(), guild_state()
) -> guild_state().
enforce_single_session_list(_SessionId, _ListId, [], _SubsTab, State) ->
    State;
enforce_single_session_list(SessionId, ListId, _Ranges, SubsTab, State) ->
    RemovedListIds = guild_member_list_subs:retain_only_session_list(
        SessionId, ListId, SubsTab
    ),
    lists:foldl(
        fun(RemovedListId, AccState) ->
            maybe_drop_channel_engine(RemovedListId, SubsTab, AccState)
        end,
        State,
        RemovedListIds
    ).

-spec maybe_drop_channel_engine(list_id(), ets:table(), guild_state()) -> guild_state().
maybe_drop_channel_engine(ListId, SubsTab, State) ->
    case map_size(guild_member_list_subs:get_list_subs(ListId, SubsTab)) =:= 0 of
        true -> guild_member_list_channel_engine:drop(ListId, State);
        false -> State
    end.

-spec send_member_list_update_to_sessions(list_id(), map(), map(), map(), guild_state()) -> ok.
send_member_list_update_to_sessions(ListId, ListSubs, Sessions, Payload, State) ->
    case dispatch_context(ListId, State) of
        undefined ->
            ok;
        {GuildId, ChannelId} ->
            do_send_update(ListSubs, Sessions, Payload, ChannelId, GuildId, State)
    end.

-spec do_send_update(
    map(), map(), map(), channel_id() | undefined, pos_integer(), guild_state()
) -> ok.
do_send_update(ListSubs, Sessions, Payload, ChId, GuildId, State) ->
    Encoded = encode_wire_payload(Payload),
    Pids = collect_list_pids(ListSubs, Sessions, ChId, State),
    gateway_dispatch_relay:dispatch_many(
        Pids, guild_member_list_update, Encoded, GuildId
    ).

-spec collect_list_pids(
    map(), map(), channel_id() | undefined, guild_state()
) -> [pid()].
collect_list_pids(ListSubs, Sessions, ChId, State) ->
    maps:fold(
        fun(Sid, _Ranges, Acc) ->
            eligible_list_pid(Sid, Sessions, ChId, State, Acc)
        end,
        [],
        ListSubs
    ).

-spec eligible_list_pid(
    binary(), map(), channel_id() | undefined, guild_state(), [pid()]
) -> [pid()].
eligible_list_pid(Sid, Sessions, ChId, State, Acc) ->
    case maps:get(Sid, Sessions, undefined) of
        #{pid := Pid} = SD when is_pid(Pid) ->
            add_if_viewable(Pid, SD, ChId, State, Acc);
        _ ->
            Acc
    end.

-spec add_if_viewable(pid(), map(), channel_id() | undefined, guild_state(), [pid()]) ->
    [pid()].
add_if_viewable(Pid, SD, ChId, State, Acc) ->
    case session_can_view_list_members(SD, ChId, State) of
        true -> [Pid | Acc];
        false -> Acc
    end.

-spec dispatch_sync_to_subscribed_list(
    list_id(),
    ets:table(),
    map(),
    channel_id() | undefined,
    integer(),
    guild_state(),
    fun(([range()]) -> map())
) -> ok.
dispatch_sync_to_subscribed_list(
    _ListId, _SubsTab, _Sessions, _ChannelId, GuildId, _State, _SyncFun
) when
    not is_integer(GuildId); GuildId =< 0
->
    ok;
dispatch_sync_to_subscribed_list(ListId, SubsTab, Sessions, ChannelId, GuildId, State, SyncFun) ->
    Groups = guild_member_list_subs:fold_list_subs(
        ListId,
        SubsTab,
        fun(SessionId, Ranges, Acc) ->
            collect_sync_session_group(SessionId, Ranges, Sessions, ChannelId, State, Acc)
        end,
        #{}
    ),
    maps:foreach(
        fun(Ranges, Pids) ->
            dispatch_sync_group(Ranges, Pids, GuildId, SyncFun)
        end,
        Groups
    ).

-spec maybe_ensure_store(list_id(), ets:table(), guild_state()) -> guild_state().
maybe_ensure_store(ListId, SubsTab, State) ->
    ensure_channel_engine_for_subs(ListId, SubsTab, State).

-spec ensure_channel_engine_for_subs(list_id(), ets:table(), guild_state()) -> guild_state().
ensure_channel_engine_for_subs(ListId, SubsTab, State) ->
    case map_size(guild_member_list_subs:get_list_subs(ListId, SubsTab)) > 0 of
        true -> guild_member_list_channel_engine:ensure(ListId, State);
        false -> guild_member_list_channel_engine:drop(ListId, State)
    end.

-spec compute_ranges_to_sync([range()], [range()], boolean()) -> [range()].
compute_ranges_to_sync(NormalizedRanges, _OldRanges, false) ->
    NormalizedRanges;
compute_ranges_to_sync(NormalizedRanges, OldRanges, true) ->
    case guild_member_list_sync:is_subset_of_ranges(NormalizedRanges, OldRanges) of
        true ->
            [];
        false ->
            compute_delta_or_full(NormalizedRanges, OldRanges)
    end.

-spec compute_delta_or_full([range()], [range()]) -> [range()].
compute_delta_or_full(NormalizedRanges, OldRanges) ->
    case guild_member_list_sync:compute_range_delta(NormalizedRanges, OldRanges) of
        [] -> NormalizedRanges;
        Delta -> Delta
    end.

-spec collect_sync_session_group(
    binary(), [range()], map(), channel_id() | undefined, guild_state(), #{[range()] => [pid()]}
) -> #{[range()] => [pid()]}.
collect_sync_session_group(SessionId, Ranges, Sessions, ChannelId, State, Acc) ->
    case maps:get(SessionId, Sessions, undefined) of
        #{pid := Pid} = SD when is_pid(Pid) ->
            add_sync_pid_if_viewable(Pid, SD, ChannelId, Ranges, State, Acc);
        _ ->
            Acc
    end.

-spec add_sync_pid_if_viewable(
    pid(),
    map(),
    channel_id() | undefined,
    [range()],
    guild_state(),
    #{[range()] => [pid()]}
) -> #{[range()] => [pid()]}.
add_sync_pid_if_viewable(SessionPid, SessionData, ChannelId, Ranges, State, Acc) ->
    case session_can_view_list_members(SessionData, ChannelId, State) of
        true -> Acc#{Ranges => [SessionPid | maps:get(Ranges, Acc, [])]};
        false -> Acc
    end.

-spec dispatch_sync_group([range()], [pid()], integer(), fun(([range()]) -> map())) -> ok.
dispatch_sync_group(_Ranges, [], _GuildId, _SyncFun) ->
    ok;
dispatch_sync_group(Ranges, Pids, GuildId, SyncFun) ->
    SyncResponse = SyncFun(Ranges),
    Encoded = encode_sync_payload(SyncResponse),
    gateway_dispatch_relay:dispatch_many(Pids, guild_member_list_update, Encoded, GuildId).

-spec encode_wire_payload(map()) -> {pre_encoded, binary()}.
encode_wire_payload(Payload) ->
    WirePayload = eqwalizer:dynamic_cast(guild_data_wire:payload(Payload)),
    {pre_encoded, iolist_to_binary(json:encode(WirePayload))}.

-spec encode_sync_payload(map()) -> {pre_encoded, binary()}.
encode_sync_payload(#{<<"ops">> := Ops} = Payload) when is_list(Ops) ->
    case sync_item_cache_enabled() of
        true ->
            encode_sync_payload_cached(Payload, Ops);
        false ->
            ok = erase_sync_item_cache(),
            encode_wire_payload(Payload)
    end;
encode_sync_payload(Payload) ->
    encode_wire_payload(Payload).

-spec encode_sync_payload_cached(map(), list()) -> {pre_encoded, binary()}.
encode_sync_payload_cached(Payload, Ops) ->
    {TimerRef, Current, Previous} = sync_item_cache(),
    Key = sync_payload_key(Payload, Ops),
    {Encoded, {Current1, Previous1}} =
        case Current of
            #{Key := {Payload, Cached}} ->
                {Cached, {Current, Previous}};
            _ ->
                encode_sync_payload_fragments(Key, Payload, Ops, {Current, Previous})
        end,
    _ = erlang:put(?SYNC_ITEM_CACHE_KEY, {TimerRef, Current1, Previous1}),
    Encoded.

-spec encode_sync_payload_fragments(term(), map(), list(), {map(), map()}) ->
    {{pre_encoded, binary()}, {map(), map()}}.
encode_sync_payload_fragments(Key, Payload, Ops, Generations) ->
    {FragmentOps, Generations1} = lists:mapfoldl(fun fragment_op/2, Generations, Ops),
    WirePayload = eqwalizer:dynamic_cast(
        guild_data_wire:payload(Payload#{<<"ops">> => FragmentOps})
    ),
    Encoded =
        {pre_encoded, iolist_to_binary(json:encode(WirePayload, fun encode_fragment_value/2))},
    {Encoded, cache_fragment(Key, {Payload, Encoded}, Generations1)}.

-spec sync_payload_key(map(), list()) -> term().
sync_payload_key(Payload, Ops) ->
    {sync_payload, maps:get(<<"id">>, Payload, undefined), [op_range(Op) || Op <- Ops]}.

-spec op_range(term()) -> term().
op_range(#{<<"range">> := Range}) ->
    Range;
op_range(_Op) ->
    undefined.

-spec fragment_op(term(), {map(), map()}) -> {term(), {map(), map()}}.
fragment_op(#{<<"items">> := Items} = Op, Generations) when is_list(Items) ->
    {FragmentItems, Generations1} = lists:mapfoldl(fun fragment_item/2, Generations, Items),
    {Op#{<<"items">> => FragmentItems}, Generations1};
fragment_op(Op, Generations) ->
    {Op, Generations}.

-spec fragment_item(term(), {map(), map()}) -> {term(), {map(), map()}}.
fragment_item(
    #{<<"member">> := #{<<"user">> := #{<<"id">> := Id}}} = Item,
    {Current, Previous} = Generations
) ->
    case Current of
        #{Id := {Item, Fragment}} ->
            {Fragment, Generations};
        _ ->
            Fragment = previous_or_encoded_fragment(Id, Item, Previous),
            {Fragment, cache_fragment(Id, {Item, Fragment}, Generations)}
    end;
fragment_item(Item, Generations) ->
    {Item, Generations}.

-spec previous_or_encoded_fragment(term(), term(), map()) -> {json_fragment, binary()}.
previous_or_encoded_fragment(Id, Item, Previous) ->
    case Previous of
        #{Id := {Item, Fragment}} ->
            Fragment;
        _ ->
            {json_fragment, iolist_to_binary(json:encode(guild_data_wire:payload(Item)))}
    end.

-spec cache_fragment(term(), {term(), term()}, {map(), map()}) -> {map(), map()}.
cache_fragment(Id, Entry, {Current, _Previous}) when
    map_size(Current) >= ?SYNC_ITEM_CACHE_MAX_ENTRIES
->
    {#{Id => Entry}, Current};
cache_fragment(Id, Entry, {Current, Previous}) ->
    {Current#{Id => Entry}, Previous}.

-spec encode_fragment_value(dynamic(), json:encoder()) -> iodata().
encode_fragment_value({json_fragment, Encoded}, _Encode) ->
    Encoded;
encode_fragment_value(Value, Encode) ->
    json:encode_value(Value, Encode).

-spec sync_cache_find(term()) -> {ok, term()} | error.
sync_cache_find(Key) ->
    case {sync_item_cache_enabled(), erlang:get(?SYNC_ITEM_CACHE_KEY)} of
        {true, {_TimerRef, #{Key := Value}, _Previous}} -> {ok, Value};
        {true, {_TimerRef, _Current, #{Key := Value}}} -> {ok, Value};
        _ -> error
    end.

-spec sync_cache_store(term(), term()) -> ok.
sync_cache_store(Key, Value) ->
    case sync_item_cache_enabled() of
        true ->
            {TimerRef, Current, Previous} = sync_item_cache(),
            {Current1, Previous1} = cache_fragment(Key, Value, {Current, Previous}),
            _ = erlang:put(?SYNC_ITEM_CACHE_KEY, {TimerRef, Current1, Previous1}),
            ok;
        false ->
            ok = erase_sync_item_cache()
    end.

-spec sync_item_cache() -> {reference(), map(), map()}.
sync_item_cache() ->
    case erlang:get(?SYNC_ITEM_CACHE_KEY) of
        {TimerRef, Current, Previous} = Cache when
            is_reference(TimerRef), is_map(Current), is_map(Previous)
        ->
            Cache;
        _ ->
            {start_sync_item_cache_timer(), #{}, #{}}
    end.

-spec handle_sync_item_cache_timeout(reference()) -> ok.
handle_sync_item_cache_timeout(TimerRef) ->
    case erlang:get(?SYNC_ITEM_CACHE_KEY) of
        {TimerRef, Current, _Previous} when map_size(Current) =:= 0 ->
            ok = erase_sync_item_cache();
        {TimerRef, Current, _Previous} when is_map(Current) ->
            _ = erlang:put(
                ?SYNC_ITEM_CACHE_KEY, {start_sync_item_cache_timer(), #{}, Current}
            ),
            ok;
        _ ->
            ok
    end.

-spec erase_sync_item_cache() -> ok.
erase_sync_item_cache() ->
    case erlang:erase(?SYNC_ITEM_CACHE_KEY) of
        {TimerRef, _, _} when is_reference(TimerRef) ->
            _ = erlang:cancel_timer(TimerRef, [{async, true}, {info, false}]),
            ok;
        _ ->
            ok
    end.

-spec start_sync_item_cache_timer() -> reference().
start_sync_item_cache_timer() ->
    erlang:start_timer(
        sync_item_cache_generation_ms(), self(), ?SYNC_ITEM_CACHE_TIMEOUT_MSG
    ).

-spec sync_item_cache_generation_ms() -> pos_integer().
sync_item_cache_generation_ms() ->
    case application:get_env(fluxer_gateway, member_list_sync_item_cache_generation_ms) of
        {ok, Ms} when is_integer(Ms), Ms > 0 -> Ms;
        _ -> ?SYNC_ITEM_CACHE_GENERATION_MS
    end.

-spec sync_item_cache_enabled() -> boolean().
sync_item_cache_enabled() ->
    case application:get_env(fluxer_gateway, member_list_sync_item_cache_enabled, true) of
        false -> false;
        _ -> true
    end.

-spec session_can_view_list_members(map(), channel_id() | undefined, guild_state()) ->
    boolean().
session_can_view_list_members(SessionData, undefined, State) ->
    case maps:get(user_id, SessionData, undefined) of
        UserId when is_integer(UserId), UserId > 0 ->
            guild_permissions:find_member_by_user_id(UserId, State) =/= undefined;
        _ ->
            false
    end;
session_can_view_list_members(SessionData, ChannelId, State) ->
    guild_member_list_connected:session_can_view_channel_members(SessionData, ChannelId, State).

-spec dispatch_context(list_id(), guild_state()) ->
    {pos_integer(), channel_id() | undefined} | undefined.
dispatch_context(ListId, State) ->
    case {valid_list_id(ListId), guild_id(State)} of
        {true, GuildId} when is_integer(GuildId), GuildId > 0 ->
            {GuildId, list_channel_id(ListId)};
        _ ->
            undefined
    end.

-spec guild_id(guild_state()) -> integer() | undefined.
guild_id(State) ->
    case snowflake_id:parse_optional(maps:get(id, State, undefined)) of
        GuildId when is_integer(GuildId), GuildId > 0 -> GuildId;
        _ -> undefined
    end.

-spec valid_list_id(term()) -> boolean().
valid_list_id(<<"0">>) ->
    true;
valid_list_id(ListId) when is_binary(ListId) ->
    case snowflake_id:parse_optional(ListId) of
        Id when is_integer(Id), Id > 0 -> true;
        _ -> false
    end;
valid_list_id(_) ->
    false.

-spec list_channel_id(term()) -> channel_id() | undefined.
list_channel_id(<<"0">>) ->
    undefined;
list_channel_id(ListId) when is_binary(ListId) ->
    case snowflake_id:parse_optional(ListId) of
        Id when is_integer(Id), Id > 0 -> Id;
        _ -> undefined
    end;
list_channel_id(_) ->
    undefined.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

sync_payload_matches_uncached_encoding_test() ->
    with_clean_cache(fun() ->
        Members = [test_member(N) || N <- lists:seq(1, 60)],
        Payloads = [
            sync_payload(<<"500">>, [{0, 99}], Members),
            sync_payload(<<"600">>, [{0, 99}], lists:reverse(Members)),
            sync_payload(<<"600">>, [{0, 99}, {100, 199}], Members ++ Members),
            sync_payload(<<"0">>, [{0, 99}], lists:sublist(Members, 7, 30))
        ],
        [assert_matches_uncached(P) || P <- Payloads ++ Payloads],
        ?assertEqual(60, map_size(current_entries()))
    end).

sync_payload_reencodes_changed_member_test() ->
    with_clean_cache(fun() ->
        Members = [test_member(N) || N <- lists:seq(1, 10)],
        assert_matches_uncached(sync_payload(<<"500">>, [{0, 99}], Members)),
        [First | Rest] = Members,
        Renamed = put_in(First, [<<"member">>, <<"nick">>], <<"renamed \"x\"">>),
        Idle = put_in(
            hd(Rest), [<<"member">>, <<"presence">>, <<"status">>], <<"idle">>
        ),
        Changed = [Renamed, Idle | tl(Rest)],
        Old = encode_sync_payload(sync_payload(<<"500">>, [{0, 99}], Members)),
        New = assert_matches_uncached(sync_payload(<<"500">>, [{0, 99}], Changed)),
        ?assertNotEqual(Old, New)
    end).

sync_payload_keeps_non_member_items_inline_test() ->
    with_clean_cache(fun() ->
        Items = [
            #{<<"group">> => #{<<"id">> => <<"online">>, <<"count">> => 3}},
            #{<<"member">> => #{<<"nick">> => <<"no user">>}},
            #{<<"member">> => #{<<"user">> => #{<<"username">> => <<"no id">>}}},
            test_member(1)
        ],
        assert_matches_uncached(payload_with_ops([sync_op_items({0, 99}, Items)])),
        ?assertEqual([<<"1427764882469228557">>], maps:keys(current_entries()))
    end).

sync_payload_without_ops_matches_uncached_test() ->
    with_clean_cache(fun() ->
        assert_matches_uncached(#{<<"id">> => <<"500">>, <<"guild_id">> => 7}),
        ?assertEqual(undefined, erlang:get(?SYNC_ITEM_CACHE_KEY)),
        assert_matches_uncached(payload_with_ops([#{<<"op">> => <<"INVALIDATE">>}])),
        ?assertEqual(#{}, current_entries())
    end).

sync_item_cache_is_bounded_test() ->
    with_clean_cache(fun() ->
        Count = ?SYNC_ITEM_CACHE_MAX_ENTRIES + 5,
        Members = [test_member(N) || N <- lists:seq(1, Count)],
        Payload = sync_payload(<<"500">>, [{0, Count - 1}], Members),
        assert_matches_uncached(Payload),
        ?assertEqual(5, map_size(current_entries())),
        ?assertEqual(?SYNC_ITEM_CACHE_MAX_ENTRIES, map_size(previous_entries())),
        assert_matches_uncached(Payload),
        ?assert(
            map_size(current_entries()) + map_size(previous_entries()) =<
                2 * ?SYNC_ITEM_CACHE_MAX_ENTRIES
        )
    end).

sync_item_cache_flag_off_encodes_uncached_and_clears_test() ->
    with_clean_cache(fun() ->
        Payload = sync_payload(<<"500">>, [{0, 99}], [test_member(N) || N <- lists:seq(1, 5)]),
        assert_matches_uncached(Payload),
        ?assertEqual(5, map_size(current_entries())),
        with_cache_flag(false, fun() ->
            assert_matches_uncached(Payload),
            ?assertEqual(undefined, erlang:get(?SYNC_ITEM_CACHE_KEY))
        end)
    end).

sync_item_cache_released_after_two_idle_generations_test() ->
    with_clean_cache(fun() ->
        with_generation_ms(5, fun() ->
            Members = [test_member(N) || N <- lists:seq(1, 5)],
            Payload = sync_payload(<<"500">>, [{0, 99}], Members),
            assert_matches_uncached(Payload),
            ?assertNotEqual(undefined, erlang:get(?SYNC_ITEM_CACHE_KEY)),
            ?assertEqual(2, deliver_rotation_timers_until_released(10)),
            ?assertEqual(undefined, erlang:get(?SYNC_ITEM_CACHE_KEY))
        end)
    end).

sync_item_cache_keeps_entries_touched_each_generation_test() ->
    with_clean_cache(fun() ->
        Payload = sync_payload(<<"500">>, [{0, 99}], [test_member(N) || N <- lists:seq(1, 8)]),
        assert_matches_uncached(Payload),
        rotate_now(),
        ?assertEqual(#{}, current_entries()),
        ?assertEqual(8, map_size(previous_entries())),
        assert_matches_uncached(Payload),
        ?assertEqual(8, map_size(current_entries())),
        ?assertEqual(1, map_size(payload_entries())),
        rotate_now(),
        ?assertEqual(8, map_size(previous_entries())),
        rotate_now(),
        ?assertEqual(undefined, erlang:get(?SYNC_ITEM_CACHE_KEY)),
        assert_matches_uncached(Payload),
        ?assertEqual(8, map_size(current_entries()))
    end).

sync_item_cache_previous_generation_changed_member_reencodes_test() ->
    with_clean_cache(fun() ->
        Members = [test_member(N) || N <- lists:seq(1, 4)],
        assert_matches_uncached(sync_payload(<<"500">>, [{0, 99}], Members)),
        rotate_now(),
        [First | Rest] = Members,
        Renamed = put_in(First, [<<"member">>, <<"nick">>], <<"renamed">>),
        assert_matches_uncached(sync_payload(<<"500">>, [{0, 99}], [Renamed | Rest])),
        #{<<"member">> := #{<<"user">> := #{<<"id">> := Id}}} = Renamed,
        ?assertMatch(#{Id := {Renamed, _}}, current_entries())
    end).

sync_item_cache_ignores_stale_timer_test() ->
    with_clean_cache(fun() ->
        Payload = sync_payload(<<"500">>, [{0, 99}], [test_member(N) || N <- lists:seq(1, 5)]),
        assert_matches_uncached(Payload),
        {StaleRef, _, _} = erlang:get(?SYNC_ITEM_CACHE_KEY),
        with_cache_flag(false, fun() -> assert_matches_uncached(Payload) end),
        assert_matches_uncached(Payload),
        ?assertMatch({noreply, #{}}, guild:handle_info(rotation_msg(StaleRef), #{})),
        ?assertEqual(5, map_size(current_entries())),
        ?assertNotMatch({StaleRef, _, _}, erlang:get(?SYNC_ITEM_CACHE_KEY))
    end).

dispatch_sync_group_sends_uncached_bytes_test() ->
    with_clean_cache(fun() ->
        Members = [test_member(N) || N <- lists:seq(1, 20)],
        SyncFun = fun(Ranges) -> sync_payload(<<"500">>, Ranges, Members) end,
        Expected = uncached(SyncFun([{0, 99}])),
        ok = dispatch_sync_group([{0, 99}], [self()], 7, SyncFun),
        ok = dispatch_sync_group([{0, 99}], [self()], 7, SyncFun),
        ?assertEqual([Expected, Expected], received_dispatches())
    end).

sync_payload_equal_content_reuses_encoded_bytes_test() ->
    with_clean_cache(fun() ->
        Members = [test_member(N) || N <- lists:seq(1, 30)],
        Payload = sync_payload(<<"500">>, [{0, 99}], Members),
        assert_matches_uncached(Payload),
        [Key] = maps:keys(payload_entries()),
        Sentinel = {pre_encoded, <<"sentinel">>},
        {TimerRef, Current, Previous} = erlang:get(?SYNC_ITEM_CACHE_KEY),
        _ = erlang:put(
            ?SYNC_ITEM_CACHE_KEY, {TimerRef, Current#{Key => {Payload, Sentinel}}, Previous}
        ),
        Copy = binary_to_term(term_to_binary(Payload)),
        ?assertEqual(Sentinel, encode_sync_payload(Copy)),
        [First | Rest] = Members,
        Afk = put_in(First, [<<"member">>, <<"presence">>, <<"afk">>], true),
        assert_matches_uncached(sync_payload(<<"500">>, [{0, 99}], [Afk | Rest])),
        ?assertEqual([Key], maps:keys(payload_entries()))
    end).

sync_payload_cache_is_keyed_by_list_and_ranges_test() ->
    with_clean_cache(fun() ->
        Members = [test_member(N) || N <- lists:seq(1, 40)],
        Payloads = [
            sync_payload(<<"500">>, [{0, 99}], Members),
            sync_payload(<<"600">>, [{0, 99}], Members),
            sync_payload(<<"500">>, [{0, 19}], Members),
            sync_payload(<<"500">>, [{0, 19}, {20, 39}], Members)
        ],
        [assert_matches_uncached(P) || P <- Payloads ++ lists:reverse(Payloads)],
        ?assertEqual(4, map_size(payload_entries())),
        Header = (hd(Payloads))#{<<"online_count">> => 1},
        assert_matches_uncached(Header),
        ?assertEqual(4, map_size(payload_entries()))
    end).

sync_payload_flag_off_ignores_cached_payload_test() ->
    with_clean_cache(fun() ->
        Payload = sync_payload(<<"500">>, [{0, 99}], [test_member(N) || N <- lists:seq(1, 5)]),
        assert_matches_uncached(Payload),
        ?assertEqual(1, map_size(payload_entries())),
        with_cache_flag(false, fun() ->
            assert_matches_uncached(Payload),
            ?assertEqual(#{}, payload_entries())
        end)
    end).

assert_matches_uncached(Payload) ->
    Expected = uncached(Payload),
    Actual = encode_sync_payload(Payload),
    ?assertEqual(Expected, Actual),
    ?assertEqual(
        json:decode(element(2, Expected)), json:decode(element(2, Actual))
    ),
    Actual.

uncached(Payload) ->
    {pre_encoded, iolist_to_binary(json:encode(guild_data_wire:payload(Payload)))}.

received_dispatches() ->
    receive
        {'$gen_cast', {dispatch, guild_member_list_update, Encoded}} ->
            [Encoded | received_dispatches()];
        {dispatch, guild_member_list_update, Encoded} ->
            [Encoded | received_dispatches()]
    after 200 ->
        []
    end.

sync_payload(ListId, Ranges, Items) ->
    Ops = [
        sync_op_items(Range, lists:sublist(Items, Start + 1, End - Start + 1))
     || {Start, End} = Range <- Ranges
    ],
    (payload_with_ops(Ops))#{<<"id">> => ListId}.

payload_with_ops(Ops) ->
    #{
        <<"guild_id">> => <<"1427764882469228556">>,
        <<"id">> => <<"500">>,
        <<"channel_id">> => <<"500">>,
        <<"member_count">> => 55278,
        <<"online_count">> => 1365,
        <<"groups">> => [
            #{<<"id">> => <<"1427764882469228600">>, <<"count">> => 12},
            #{<<"id">> => <<"online">>, <<"count">> => 1353}
        ],
        <<"ops">> => Ops
    }.

sync_op_items({Start, End}, Items) ->
    #{<<"op">> => <<"SYNC">>, <<"range">> => [Start, End], <<"items">> => Items}.

test_member(N) ->
    Id = integer_to_binary(1427764882469228556 + N),
    User = #{
        <<"id">> => Id,
        <<"username">> => <<"user_", (integer_to_binary(N))/binary>>,
        <<"global_name">> => pick(N, [
            null, <<"Ünïcødé \\ \"q\" "/utf8, 240, 159, 152, 128>>, <<"g">>
        ]),
        <<"avatar">> => pick(N, [null, <<"a_0123456789abcdef">>]),
        <<"discriminator">> => <<"0000">>,
        <<"flags">> => N * 4096,
        <<"bot">> => N rem 11 =:= 0,
        <<"avatar_decoration_data">> => pick(N, [
            null, #{<<"sku_id">> => 99, <<"asset">> => <<"x">>}
        ])
    },
    Member = #{
        <<"user">> => User,
        <<"nick">> => pick(N, [null, <<"nick\n\t", 1, " ", (integer_to_binary(N))/binary>>]),
        <<"roles">> => pick(N, [
            [], [<<"1427764882469228600">>, <<"1427764882469228601">>], [42]
        ]),
        <<"joined_at">> => <<"2026-01-01T00:00:00.000Z">>,
        <<"communication_disabled_until">> => null,
        <<"deaf">> => false,
        <<"mute">> => N rem 5 =:= 0,
        <<"premium_since">> => pick(N, [null, <<"2026-02-02T00:00:00Z">>]),
        <<"guild_id">> => 1427764882469228556,
        voice_channel_id => pick(N, [null, 1427764882469228777]),
        <<"presence">> => #{
            <<"user">> => #{<<"id">> => Id},
            <<"status">> => pick(N, [<<"online">>, <<"idle">>, <<"dnd">>]),
            <<"mobile">> => N rem 2 =:= 0,
            <<"afk">> => false,
            <<"custom_status">> => pick(N, [
                null, #{<<"text">> => <<"hi">>, <<"emoji_id">> => 5, <<"emoji_name">> => null}
            ])
        }
    },
    #{<<"member">> => Member}.

pick(N, Options) ->
    lists:nth(N rem length(Options) + 1, Options).

put_in(Map, [Key], Value) ->
    Map#{Key => Value};
put_in(Map, [Key | Rest], Value) ->
    Map#{Key => put_in(maps:get(Key, Map), Rest, Value)}.

with_clean_cache(Fun) ->
    clear_cache(),
    try
        Fun()
    after
        clear_cache()
    end.

clear_cache() ->
    ok = erase_sync_item_cache(),
    flush_rotation_timers().

flush_rotation_timers() ->
    receive
        {timeout, _, ?SYNC_ITEM_CACHE_TIMEOUT_MSG} -> flush_rotation_timers()
    after 0 ->
        ok
    end.

current_entries() ->
    case erlang:get(?SYNC_ITEM_CACHE_KEY) of
        {_, Current, _} -> fragment_entries(Current);
        _ -> #{}
    end.

previous_entries() ->
    case erlang:get(?SYNC_ITEM_CACHE_KEY) of
        {_, _, Previous} -> fragment_entries(Previous);
        _ -> #{}
    end.

fragment_entries(Generation) ->
    maps:filter(fun(Key, _) -> is_binary(Key) end, Generation).

payload_entries() ->
    case erlang:get(?SYNC_ITEM_CACHE_KEY) of
        {_, Current, _} -> maps:filter(fun(Key, _) -> not is_binary(Key) end, Current);
        _ -> #{}
    end.

rotation_msg(TimerRef) ->
    {timeout, TimerRef, ?SYNC_ITEM_CACHE_TIMEOUT_MSG}.

rotate_now() ->
    {TimerRef, _, _} = erlang:get(?SYNC_ITEM_CACHE_KEY),
    _ = erlang:cancel_timer(TimerRef),
    ?assertMatch({noreply, #{}}, guild:handle_info(rotation_msg(TimerRef), #{})).

deliver_rotation_timers_until_released(0) ->
    erlang:error(sync_item_cache_not_released);
deliver_rotation_timers_until_released(Remaining) ->
    receive
        {timeout, _, ?SYNC_ITEM_CACHE_TIMEOUT_MSG} = Msg ->
            {noreply, _} = guild:handle_info(Msg, #{}),
            case erlang:get(?SYNC_ITEM_CACHE_KEY) of
                undefined -> 1;
                _ -> 1 + deliver_rotation_timers_until_released(Remaining - 1)
            end
    after 1000 ->
        erlang:error(sync_item_cache_timer_not_armed)
    end.

with_generation_ms(Ms, Fun) ->
    Key = member_list_sync_item_cache_generation_ms,
    Previous = application:get_env(fluxer_gateway, Key),
    application:set_env(fluxer_gateway, Key, Ms),
    try
        Fun()
    after
        case Previous of
            {ok, Old} -> application:set_env(fluxer_gateway, Key, Old);
            undefined -> application:unset_env(fluxer_gateway, Key)
        end
    end.

with_cache_flag(Value, Fun) ->
    Previous = application:get_env(fluxer_gateway, member_list_sync_item_cache_enabled),
    application:set_env(fluxer_gateway, member_list_sync_item_cache_enabled, Value),
    try
        Fun()
    after
        case Previous of
            {ok, Old} ->
                application:set_env(fluxer_gateway, member_list_sync_item_cache_enabled, Old);
            undefined ->
                application:unset_env(fluxer_gateway, member_list_sync_item_cache_enabled)
        end
    end.

-endif.
