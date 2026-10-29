%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_member_list_subscribe).
-typing([eqwalizer]).

-export([
    subscribe_ranges/4,
    unsubscribe_session/2,
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
            {StateExclusive, RangesToSync =/= [], RangesToSync};
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
            Payload = eqwalizer:dynamic_cast(guild_data_wire:payload(Item)),
            {json_fragment, iolist_to_binary(json:encode(Payload))}
    end.

-spec cache_fragment(term(), term(), {map(), map()}) -> {map(), map()}.
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
            guild_permissions_check:find_member_by_user_id(UserId, State) =/= undefined;
        _ ->
            false
    end;
session_can_view_list_members(SessionData, ChannelId, State) ->
    guild_member_list_connected:session_can_view_channel_members(SessionData, ChannelId, State).

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
