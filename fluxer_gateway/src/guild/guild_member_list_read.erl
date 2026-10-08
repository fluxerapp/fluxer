%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_member_list_read).
-typing([eqwalizer]).

-export([
    get_counts/2,
    get_online_count/1,
    build_sync_response/4,
    build_sync_response_builder/3,
    build_normalized_sync_response_builder/3,
    member_list_snapshot/2,
    snapshot/2,
    with_member_item_memo/1,
    note_presence_write/1,
    get_members_cursor/2
]).

-type guild_state() :: map().
-type list_id() :: binary().
-type range() :: {non_neg_integer(), non_neg_integer()}.
-type group_item() :: map().
-type list_item() :: map().
-type store_item() :: {group, binary(), non_neg_integer()} | {member, integer()}.
-type read_context() :: #{
    list_id := list_id(),
    hide_offline := boolean(),
    member_map := map(),
    member_revision := term(),
    presence_context := map(),
    state := guild_state()
}.

-export_type([guild_state/0, list_id/0, range/0, group_item/0, list_item/0, store_item/0]).

-define(MEMBER_ITEM_MEMO_KEY, guild_member_list_member_item_memo).
-define(PRESENCE_WRITES_KEY, guild_member_list_presence_writes).
-define(PRESENCE_WRITES_KEPT, 256).

-type range_stamp() :: {non_neg_integer(), boolean(), reference(), ets:tid()}.
-type range_entry() :: {range_stamp(), non_neg_integer(), map(), [term()], map(), map()}.

-spec note_presence_write(integer()) -> ok.
note_presence_write(UserId) ->
    {Seq, Kept, Log} = presence_writes(),
    {Kept1, Log1} =
        case Kept >= 2 * ?PRESENCE_WRITES_KEPT of
            true -> {?PRESENCE_WRITES_KEPT, lists:sublist(Log, ?PRESENCE_WRITES_KEPT)};
            false -> {Kept, Log}
        end,
    _ = erlang:put(?PRESENCE_WRITES_KEY, {Seq + 1, Kept1 + 1, [{Seq + 1, UserId} | Log1]}),
    ok.

-spec presence_writes() -> {non_neg_integer(), non_neg_integer(), [{pos_integer(), integer()}]}.
presence_writes() ->
    case erlang:get(?PRESENCE_WRITES_KEY) of
        {Seq, Kept, Log} when is_integer(Seq), is_integer(Kept), is_list(Log) ->
            {Seq, Kept, Log};
        _ ->
            {0, 0, []}
    end.

-spec presence_written_since(non_neg_integer()) -> {ok, [integer()]} | stale.
presence_written_since(Since) ->
    {Seq, _Kept, Log} = presence_writes(),
    case Seq - Since of
        0 ->
            {ok, []};
        Count when Count > 0 ->
            Written = [
                UserId
             || {WriteSeq, UserId} <- lists:sublist(Log, Count), WriteSeq > Since
            ],
            case length(Written) =:= Count of
                true -> {ok, Written};
                false -> stale
            end;
        _ ->
            stale
    end.

-spec with_member_item_memo(fun(() -> T)) -> T.
with_member_item_memo(Fun) ->
    case erlang:get(?MEMBER_ITEM_MEMO_KEY) of
        undefined ->
            _ = erlang:put(?MEMBER_ITEM_MEMO_KEY, #{}),
            try
                Fun()
            after
                erlang:erase(?MEMBER_ITEM_MEMO_KEY)
            end;
        _ ->
            Fun()
    end.

-spec get_counts(list_id(), guild_state()) -> {non_neg_integer(), non_neg_integer()}.
get_counts(ListId, State) ->
    case store_ref_for(ListId, State) of
        undefined -> {0, 0};
        Ref -> guild_member_list_store:get_counts(Ref)
    end.

-spec get_online_count(guild_state()) -> non_neg_integer().
get_online_count(State) ->
    case store_ref_for(<<"0">>, State) of
        undefined ->
            0;
        Ref ->
            {_Total, Online} = guild_member_list_store:get_counts(Ref),
            Online
    end.

-spec build_sync_response(integer(), list_id(), [range()], guild_state()) -> map().
build_sync_response(GuildId, ListId, Ranges, State) ->
    Builder = build_sync_response_builder(GuildId, ListId, State),
    Builder(Ranges).

-spec build_sync_response_builder(integer(), list_id(), guild_state()) ->
    fun(([range()]) -> map()).
build_sync_response_builder(GuildId, ListId, State) ->
    NormalizedBuilder = build_normalized_sync_response_builder(GuildId, ListId, State),
    fun(Ranges) ->
        NormalizedBuilder(guild_member_list:normalize_ranges(Ranges))
    end.

-spec build_normalized_sync_response_builder(integer(), list_id(), guild_state()) ->
    fun(([range()]) -> map()).
build_normalized_sync_response_builder(GuildId, ListId, State) ->
    build_normalized_sync_response_builder_for_store(
        integer_to_binary(GuildId), ListId, store_ref_for(ListId, State), State
    ).

-spec build_normalized_sync_response_builder_for_store(
    binary(), list_id(), guild_member_list_store:store_ref() | undefined, guild_state()
) -> fun(([range()]) -> map()).
build_normalized_sync_response_builder_for_store(GuildIdBin, ListId, undefined, _State) ->
    fun(NormalizedRanges) ->
        #{
            <<"guild_id">> => GuildIdBin,
            <<"id">> => ListId,
            <<"member_count">> => 0,
            <<"online_count">> => 0,
            <<"groups">> => [],
            <<"ops">> => [sync_op(Range, []) || Range <- NormalizedRanges]
        }
    end;
build_normalized_sync_response_builder_for_store(GuildIdBin, ListId, Ref, State) ->
    {MemberCount, OnlineCount} = guild_member_list_store:get_counts(Ref),
    StoreGroups = guild_member_list_store:get_groups(Ref),
    VisibleGroups = visible_groups(StoreGroups),
    ReadCtx = read_context(ListId, StoreGroups, State),
    fun(NormalizedRanges) ->
        #{
            <<"guild_id">> => GuildIdBin,
            <<"id">> => ListId,
            <<"member_count">> => MemberCount,
            <<"online_count">> => OnlineCount,
            <<"groups">> => VisibleGroups,
            <<"ops">> => [
                range_sync_op_from_context(Ref, Range, StoreGroups, ReadCtx)
             || Range <- NormalizedRanges
            ]
        }
    end.

-spec read_context(list_id(), [{binary(), non_neg_integer()}], guild_state()) -> read_context().
read_context(ListId, StoreGroups, State) ->
    Data = maps:get(data, State, #{}),
    #{
        list_id => ListId,
        hide_offline => offline_hidden(StoreGroups),
        member_map => guild_data_index:member_map(Data),
        member_revision => maps:get(member_list_revision, Data, undefined),
        presence_context => guild_member_list_connected:presence_context(State),
        state => State
    }.

-spec range_sync_op_from_context(
    guild_member_list_store:store_ref(),
    range(),
    [{binary(), non_neg_integer()}],
    read_context()
) -> map().
range_sync_op_from_context(Ref, {Start, End} = Range, StoreGroups, ReadCtx) ->
    case range_stamp(Ref, ReadCtx) of
        undefined ->
            {_Keys, Op} = build_range_sync_op(Ref, Range, StoreGroups, ReadCtx),
            Op;
        Stamp ->
            Key = {sync_range, Ref, Start, End},
            Entry = cached_range_entry(Key, Stamp, Ref, Range, StoreGroups, ReadCtx),
            ok = guild_member_list_subscribe:sync_cache_store(Key, Entry),
            element(5, Entry)
    end.

-spec range_stamp(guild_member_list_store:store_ref(), read_context()) ->
    range_stamp() | undefined.
range_stamp(Ref, #{hide_offline := HideOffline, member_revision := Revision} = ReadCtx) ->
    #{presence_context := PresenceCtx} = ReadCtx,
    Presence = maps:get(member_presence, PresenceCtx, undefined),
    case is_reference(Revision) andalso presence_writes_seen_here(Presence) of
        true ->
            case guild_member_list_engine:version(Ref) of
                Version when is_integer(Version) -> {Version, HideOffline, Revision, Presence};
                _ -> undefined
            end;
        false ->
            undefined
    end.

-spec presence_writes_seen_here(term()) -> boolean().
presence_writes_seen_here(Presence) when is_reference(Presence) ->
    try ets:info(eqwalizer:dynamic_cast(Presence), owner) of
        Owner -> Owner =:= self()
    catch
        _:_ -> false
    end;
presence_writes_seen_here(_) ->
    false.

-spec cached_range_entry(
    term(),
    range_stamp(),
    guild_member_list_store:store_ref(),
    range(),
    [{binary(), non_neg_integer()}],
    read_context()
) -> range_entry().
cached_range_entry(Key, Stamp, Ref, Range, StoreGroups, ReadCtx) ->
    case guild_member_list_subscribe:sync_cache_find(Key) of
        {ok, {Stamp0, Seq0, Users, Keys, Op, Connected0}} ->
            Connected = range_connected_users(Users, ReadCtx),
            case changed_range_users(Stamp0, Stamp, Seq0, Users, Connected0, Connected) of
                {ok, Changed} ->
                    patch_range_entry(Stamp, Users, Keys, Op, Changed, Connected, ReadCtx);
                rebuild ->
                    new_range_entry(Stamp, Ref, Range, StoreGroups, ReadCtx)
            end;
        _ ->
            new_range_entry(Stamp, Ref, Range, StoreGroups, ReadCtx)
    end.

-spec changed_range_users(term(), range_stamp(), term(), term(), map(), map()) ->
    {ok, #{integer() => true}} | rebuild.
changed_range_users(Stamp, Stamp, Seq0, Users, Connected0, Connected) when
    is_integer(Seq0), is_map(Users)
->
    case presence_written_since(Seq0) of
        {ok, Written} ->
            Rewritten = maps:from_list([{U, true} || U <- Written, maps:is_key(U, Users)]),
            Reconnected =
                case Connected0 =:= Connected of
                    true ->
                        #{};
                    false ->
                        maps:filter(
                            fun(UserId, _) ->
                                maps:is_key(UserId, Connected0) =/=
                                    maps:is_key(UserId, Connected)
                            end,
                            Users
                        )
                end,
            {ok, maps:merge(Rewritten, Reconnected)};
        stale ->
            rebuild
    end;
changed_range_users(_Stamp0, _Stamp, _Seq0, _Users, _Connected0, _Connected) ->
    rebuild.

-spec range_connected_users(map(), read_context()) -> map().
range_connected_users(Users, #{presence_context := PresenceCtx}) ->
    Connected = maps:get(connected_user_ids, PresenceCtx, undefined),
    maps:filter(fun(UserId, _) -> is_connected(UserId, Connected) end, Users).

-spec is_connected(integer(), term()) -> boolean().
is_connected(UserId, Connected) ->
    try
        sets:is_element(UserId, eqwalizer:dynamic_cast(Connected))
    catch
        _:_ -> false
    end.

-spec patch_range_entry(
    range_stamp(), map(), [term()], map(), #{integer() => true}, map(), read_context()
) -> range_entry().
patch_range_entry(Stamp, Users, Keys, Op, Changed, Connected, _ReadCtx) when
    map_size(Changed) =:= 0
->
    {Stamp, presence_seq(), Users, Keys, Op, Connected};
patch_range_entry(
    Stamp, Users, Keys, #{<<"items">> := Items} = Op, Changed, Connected, ReadCtx
) ->
    #{member_map := MemberMap, presence_context := PresenceCtx} = ReadCtx,
    Patched = lists:zipwith(
        fun
            (UserId, _Item) when is_map_key(UserId, Changed) ->
                {true, New} = hydrate_member_ref(UserId, MemberMap, PresenceCtx),
                New;
            (_Key, Item) ->
                Item
        end,
        Keys,
        Items
    ),
    {Stamp, presence_seq(), Users, Keys, Op#{<<"items">> => Patched}, Connected}.

-spec new_range_entry(
    range_stamp(),
    guild_member_list_store:store_ref(),
    range(),
    [{binary(), non_neg_integer()}],
    read_context()
) -> range_entry().
new_range_entry(Stamp, Ref, Range, StoreGroups, ReadCtx) ->
    Seq = presence_seq(),
    {Keys, Op} = build_range_sync_op(Ref, Range, StoreGroups, ReadCtx),
    Users = maps:from_list([{UserId, true} || UserId <- Keys, is_integer(UserId)]),
    {Stamp, Seq, Users, Keys, Op, range_connected_users(Users, ReadCtx)}.

-spec presence_seq() -> non_neg_integer().
presence_seq() ->
    element(1, presence_writes()).

-spec build_range_sync_op(
    guild_member_list_store:store_ref(),
    range(),
    [{binary(), non_neg_integer()}],
    read_context()
) -> {[term()], map()}.
build_range_sync_op(Ref, {Start, End} = Range, StoreGroups, ReadCtx) ->
    StoreItems = store_range_items_from_context(Ref, StoreGroups, Start, End, ReadCtx),
    Keyed = hydrate_keyed_items_from_context(StoreItems, ReadCtx),
    {[K || {K, _} <- Keyed], sync_op(Range, [Item || {_, Item} <- Keyed])}.

-spec store_range_items_from_context(
    guild_member_list_store:store_ref(),
    [{binary(), non_neg_integer()}],
    non_neg_integer(),
    non_neg_integer(),
    read_context()
) -> [store_item()].
store_range_items_from_context(Ref, _StoreGroups, Start, End, #{hide_offline := false}) ->
    guild_member_list_store:get_items(Ref, Start, End);
store_range_items_from_context(Ref, StoreGroups, Start, End, #{hide_offline := true}) ->
    visible_range_items(Ref, StoreGroups, Start, End).

-spec sync_op(range(), [list_item()]) -> map().
sync_op({Start, End}, Items) ->
    #{<<"op">> => <<"SYNC">>, <<"range">> => [Start, End], <<"items">> => Items}.

-spec member_list_snapshot(list_id(), guild_state()) ->
    {non_neg_integer(), non_neg_integer(), [group_item()], [list_item()]}.
member_list_snapshot(ListId, State) ->
    snapshot(ListId, State).

-spec snapshot(list_id(), guild_state()) ->
    {non_neg_integer(), non_neg_integer(), [group_item()], [list_item()]}.
snapshot(ListId, State) ->
    case store_ref_for(ListId, State) of
        undefined ->
            {0, 0, [], []};
        Ref ->
            {Total, Online} = guild_member_list_store:get_counts(Ref),
            StoreGroups = guild_member_list_store:get_groups(Ref),
            StoreItems = guild_member_list_store:get_items(Ref, 0, Total * 2),
            Items = hydrate_engine_items(ListId, StoreItems, StoreGroups, State),
            {Total, Online, visible_groups(StoreGroups), Items}
    end.

-spec hydrate_engine_items(
    list_id(), [store_item()], [{binary(), non_neg_integer()}], guild_state()
) ->
    [list_item()].
hydrate_engine_items(ListId, StoreItems, StoreGroups, State) ->
    Data = maps:get(data, State, #{}),
    MemberMap = guild_data_index:member_map(Data),
    HideOffline = offline_hidden(StoreGroups),
    hydrate_engine_items_with_context(StoreItems, ListId, HideOffline, MemberMap, State).

-spec hydrate_keyed_items_from_context([store_item()], read_context()) ->
    [{integer() | group, list_item()}].
hydrate_keyed_items_from_context(StoreItems, #{
    list_id := ListId,
    hide_offline := HideOffline,
    member_map := MemberMap,
    presence_context := PresenceCtx,
    state := State
}) ->
    {Keyed, _Section} = lists:foldl(
        fun(StoreItem, {Acc, Section}) ->
            case
                hydrate_store_item(
                    StoreItem, ListId, HideOffline, MemberMap, PresenceCtx, State, {[], Section}
                )
            of
                {[Item], Section1} -> {[{store_item_key(StoreItem), Item} | Acc], Section1};
                {[], Section1} -> {Acc, Section1}
            end
        end,
        {[], undefined},
        StoreItems
    ),
    lists:reverse(Keyed).

-spec store_item_key(store_item()) -> integer() | group.
store_item_key({member, UserId}) -> UserId;
store_item_key({group, _Id, _Count}) -> group.

-spec hydrate_engine_items_with_context(
    [store_item()], list_id(), boolean(), map(), guild_state()
) -> [list_item()].
hydrate_engine_items_with_context(StoreItems, ListId, HideOffline, MemberMap, State) ->
    PresenceCtx = guild_member_list_connected:presence_context(State),
    hydrate_engine_items_with_context(
        StoreItems, ListId, HideOffline, MemberMap, PresenceCtx, State
    ).

-spec hydrate_engine_items_with_context(
    [store_item()], list_id(), boolean(), map(), map(), guild_state()
) -> [list_item()].
hydrate_engine_items_with_context(
    StoreItems, ListId, HideOffline, MemberMap, PresenceCtx, State
) ->
    {Items, _Section} = lists:foldl(
        fun(Item, Acc) ->
            hydrate_store_item(Item, ListId, HideOffline, MemberMap, PresenceCtx, State, Acc)
        end,
        {[], undefined},
        StoreItems
    ),
    lists:reverse(Items).

-spec hydrate_store_item(
    store_item(), list_id(), boolean(), map(), map(), guild_state(), {[list_item()], term()}
) -> {[list_item()], term()}.
hydrate_store_item(
    {group, <<"offline">>, _Count}, _ListId, true, _MemberMap, _PresenceCtx, _State, {Acc, _}
) ->
    {Acc, offline};
hydrate_store_item(
    {group, Id, Count}, _ListId, _HideOffline, _MemberMap, _PresenceCtx, _State, {Acc, _}
) ->
    {
        [
            #{<<"group">> => #{<<"id">> => Id, <<"count">> => Count}}
            | Acc
        ],
        Id
    };
hydrate_store_item(
    {member, _UserId}, _ListId, true, _MemberMap, _PresenceCtx, _State, {Acc, offline}
) ->
    {Acc, offline};
hydrate_store_item(
    {member, UserId}, _ListId, _HideOffline, MemberMap, PresenceCtx, _State, {Acc, Current}
) ->
    case hydrate_member_ref(UserId, MemberMap, PresenceCtx) of
        {true, Item} -> {[Item | Acc], Current};
        false -> {Acc, Current}
    end.

-spec hydrate_member_ref(integer(), map(), map()) -> {true, list_item()} | false.
hydrate_member_ref(UserId, MemberMap, PresenceCtx) ->
    case maps:find(UserId, MemberMap) of
        error ->
            false;
        {ok, Member} ->
            {true, member_item(UserId, Member, PresenceCtx)}
    end.

-spec member_item(integer(), map(), map()) -> list_item().
member_item(UserId, Member, PresenceCtx) ->
    Presence = maps:get(member_presence, PresenceCtx, undefined),
    case {erlang:get(?MEMBER_ITEM_MEMO_KEY), is_reference(Presence)} of
        {Items, true} when is_map(Items) ->
            Connected = is_connected(
                UserId, maps:get(connected_user_ids, PresenceCtx, undefined)
            ),
            Stamp = {Presence, Connected, presence_seq()},
            case maps:find(UserId, Items) of
                {ok, {Member, Stamp, Item}} ->
                    Item;
                _ ->
                    Item = build_member_item(UserId, Member, PresenceCtx),
                    _ = erlang:put(?MEMBER_ITEM_MEMO_KEY, Items#{
                        UserId => {Member, Stamp, Item}
                    }),
                    Item
            end;
        _ ->
            build_member_item(UserId, Member, PresenceCtx)
    end.

-spec build_member_item(integer(), map(), map()) -> list_item().
build_member_item(UserId, Member, PresenceCtx) ->
    #{
        <<"member">> =>
            guild_member_list_connected:add_presence_to_member(Member, UserId, PresenceCtx)
    }.

-spec get_members_cursor(map(), guild_state()) -> {reply, map(), guild_state()}.
get_members_cursor(Request, State) ->
    guild_member_list_read_cursor:get_members_cursor(Request, State).

-spec store_ref_for(list_id(), guild_state()) ->
    guild_member_list_store:store_ref() | undefined.
store_ref_for(<<"0">>, State) ->
    case maps:get(member_list_engine, State, undefined) of
        Ref when is_reference(Ref); is_atom(Ref) -> Ref;
        _ -> undefined
    end;
store_ref_for(ListId, State) ->
    guild_member_list_channel_engine:ref(ListId, State).

-spec visible_groups([{binary(), non_neg_integer()}]) -> [group_item()].
visible_groups(StoreGroups) ->
    Threshold = guild_member_list_offline:threshold(),
    [
        #{<<"id">> => Id, <<"count">> => Count}
     || {Id, Count} <- StoreGroups,
        group_visible(Id, Count, Threshold)
    ].

-spec group_visible(binary(), non_neg_integer(), pos_integer()) -> boolean().
group_visible(<<"offline">>, Count, Threshold) ->
    Count > 0 andalso Count =< Threshold;
group_visible(_Id, Count, _Threshold) ->
    Count > 0.

-spec visible_range_items(
    guild_member_list_store:store_ref(),
    [{binary(), non_neg_integer()}],
    non_neg_integer(),
    non_neg_integer()
) -> [store_item()].
visible_range_items(Ref, StoreGroups, Start, End) ->
    VisibleEnd = visible_online_last_index(StoreGroups),
    case Start > VisibleEnd of
        true -> [];
        false -> guild_member_list_store:get_items(Ref, Start, min(End, VisibleEnd))
    end.

-spec visible_online_last_index([{binary(), non_neg_integer()}]) -> integer().
visible_online_last_index(StoreGroups) ->
    visible_online_rows(StoreGroups) - 1.

-spec visible_online_rows([{binary(), non_neg_integer()}]) -> non_neg_integer().
visible_online_rows([]) ->
    0;
visible_online_rows([{<<"offline">>, _Count} | Rest]) ->
    visible_online_rows(Rest);
visible_online_rows([{_Id, Count} | Rest]) when Count > 0 ->
    1 + Count + visible_online_rows(Rest);
visible_online_rows([_ | Rest]) ->
    visible_online_rows(Rest).

-spec offline_hidden([{binary(), non_neg_integer()}]) -> boolean().
offline_hidden(StoreGroups) ->
    OfflineCount = maps:get(<<"offline">>, maps:from_list(StoreGroups), 0),
    OfflineCount > guild_member_list_offline:threshold().

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

group_visible_is_threshold_based_test() ->
    Threshold = guild_member_list_offline:threshold(),
    ?assert(group_visible(<<"offline">>, Threshold, Threshold)),
    ?assert(group_visible(<<"offline">>, 1, Threshold)),
    ?assertNot(group_visible(<<"offline">>, 0, Threshold)),
    ?assertNot(group_visible(<<"offline">>, Threshold + 1, Threshold)),
    ?assert(group_visible(<<"online">>, 1, Threshold)),
    ?assertNot(group_visible(<<"online">>, 0, Threshold)).

hidden_offline_range_is_clamped_test() ->
    Ref = guild_member_list_engine:new(),
    HiddenOfflineCount = guild_member_list_offline:threshold() + 1,
    [
        guild_member_list_engine:add_member(
            Ref, U, <<"online_", (integer_to_binary(U))/binary>>, [], true
        )
     || U <- lists:seq(1, 5)
    ],
    [
        guild_member_list_engine:add_member(
            Ref, U, <<"offline_", (integer_to_binary(U))/binary>>, [], false
        )
     || U <- lists:seq(101, 100 + HiddenOfflineCount)
    ],
    StoreGroups = guild_member_list_engine:get_groups(Ref),
    ?assertEqual(5, visible_online_last_index(StoreGroups)),
    ?assertEqual([], [G || #{<<"id">> := <<"offline">>} = G <- visible_groups(StoreGroups)]),
    ?assertEqual([], [
        I
     || {member, U} = I <- visible_range_items(Ref, StoreGroups, 0, 100), U >= 101
    ]),
    guild_member_list_engine:destroy(Ref).

build_sync_response_without_store_still_emits_sync_windows_test() ->
    Resp = build_sync_response(123, <<"0">>, [{0, 2}], #{}),
    ?assertEqual(0, maps:get(<<"member_count">>, Resp)),
    ?assertMatch(
        [#{<<"op">> := <<"SYNC">>, <<"range">> := [0, 2], <<"items">> := []}],
        maps:get(<<"ops">>, Resp)
    ).

memo_member(UserId, Username) ->
    #{<<"user">> => #{<<"id">> => integer_to_binary(UserId), <<"username">> => Username}}.

memo_presence_ctx(Tab, Connected) ->
    #{member_presence => Tab, connected_user_ids => sets:from_list(Connected)}.

with_memo_presence_tab(Fun) ->
    Tab = ets:new(memo_presence, [set, public]),
    try
        true = ets:insert(Tab, [
            {1, #{<<"status">> => <<"online">>, <<"mobile">> => true, <<"afk">> => false}},
            {2, #{<<"status">> => <<"idle">>, <<"mobile">> => false, <<"afk">> => true}}
        ]),
        Fun(Tab)
    after
        ets:delete(Tab)
    end.

member_item_memo_shares_items_within_scope_test() ->
    with_memo_presence_tab(fun(Tab) ->
        MemberMap = #{1 => memo_member(1, <<"one">>), 2 => memo_member(2, <<"two">>)},
        Ctx = memo_presence_ctx(Tab, [1, 2]),
        Unscoped1 = hydrate_member_ref(1, MemberMap, Ctx),
        Unscoped2 = hydrate_member_ref(1, MemberMap, Ctx),
        ?assertNot(erts_debug:same(element(2, Unscoped1), element(2, Unscoped2))),
        {Scoped1, Scoped2, Other} = with_member_item_memo(fun() ->
            {
                hydrate_member_ref(1, MemberMap, Ctx),
                hydrate_member_ref(1, MemberMap, memo_presence_ctx(Tab, [1, 2])),
                hydrate_member_ref(2, MemberMap, Ctx)
            }
        end),
        ?assertEqual(Unscoped1, Scoped1),
        ?assertEqual(Scoped1, Scoped2),
        ?assert(erts_debug:same(element(2, Scoped1), element(2, Scoped2))),
        ?assertEqual(hydrate_member_ref(2, MemberMap, Ctx), Other),
        ?assertEqual(
            false, with_member_item_memo(fun() -> hydrate_member_ref(3, MemberMap, Ctx) end)
        ),
        ?assertEqual(undefined, erlang:get(?MEMBER_ITEM_MEMO_KEY))
    end).

member_item_memo_rebuilds_changed_member_test() ->
    with_memo_presence_tab(fun(Tab) ->
        Ctx = memo_presence_ctx(Tab, [1]),
        Before = #{1 => memo_member(1, <<"one">>)},
        After = #{1 => memo_member(1, <<"renamed">>)},
        {Old, New} = with_member_item_memo(fun() ->
            {hydrate_member_ref(1, Before, Ctx), hydrate_member_ref(1, After, Ctx)}
        end),
        ?assertEqual(hydrate_member_ref(1, Before, Ctx), Old),
        ?assertEqual(hydrate_member_ref(1, After, Ctx), New),
        ?assertNotEqual(Old, New)
    end).

member_item_memo_rebuilds_on_presence_context_change_test() ->
    with_memo_presence_tab(fun(Tab) ->
        MemberMap = #{1 => memo_member(1, <<"one">>)},
        Connected = memo_presence_ctx(Tab, [1]),
        Disconnected = memo_presence_ctx(Tab, []),
        {Online, Offline, OnlineAgain} = with_member_item_memo(fun() ->
            {
                hydrate_member_ref(1, MemberMap, Connected),
                hydrate_member_ref(1, MemberMap, Disconnected),
                hydrate_member_ref(1, MemberMap, Connected)
            }
        end),
        {true, #{<<"member">> := #{<<"presence">> := OnlinePresence}}} = Online,
        {true, #{<<"member">> := #{<<"presence">> := OfflinePresence}}} = Offline,
        ?assertEqual(<<"online">>, maps:get(<<"status">>, OnlinePresence)),
        ?assertEqual(true, maps:get(<<"mobile">>, OnlinePresence)),
        ?assertEqual(guild_member_list_connected:default_presence(), OfflinePresence),
        ?assertEqual(Online, OnlineAgain)
    end).

member_item_memo_scope_nests_and_clears_on_error_test() ->
    with_memo_presence_tab(fun(Tab) ->
        MemberMap = #{1 => memo_member(1, <<"one">>)},
        Ctx = memo_presence_ctx(Tab, [1]),
        with_member_item_memo(fun() ->
            Outer = hydrate_member_ref(1, MemberMap, Ctx),
            Inner = with_member_item_memo(fun() -> hydrate_member_ref(1, MemberMap, Ctx) end),
            ?assert(erts_debug:same(element(2, Outer), element(2, Inner))),
            ?assertNotEqual(undefined, erlang:get(?MEMBER_ITEM_MEMO_KEY))
        end),
        ?assertEqual(undefined, erlang:get(?MEMBER_ITEM_MEMO_KEY)),
        ?assertError(boom, with_member_item_memo(fun() -> erlang:error(boom) end)),
        ?assertEqual(undefined, erlang:get(?MEMBER_ITEM_MEMO_KEY))
    end).

member_item_memo_sync_responses_match_unscoped_test() ->
    with_memo_presence_tab(fun(Tab) ->
        Ref = guild_member_list_engine:new(),
        Other = guild_member_list_engine:new(),
        try
            ok = guild_member_list_engine:bulk_load(
                Ref, [{1, <<"one">>, [], true}, {2, <<"two">>, [], false}], []
            ),
            ok = guild_member_list_engine:bulk_load(Other, [{1, <<"one">>, [], true}], []),
            State = #{
                id => 9,
                data => #{
                    <<"members">> => #{
                        1 => memo_member(1, <<"one">>), 2 => memo_member(2, <<"two">>)
                    }
                },
                member_presence => Tab,
                connected_user_ids => sets:from_list([1]),
                channel_member_list_engines => #{<<"500">> => Ref, <<"600">> => Other}
            },
            Build = fun() ->
                [
                    build_sync_response(9, ListId, [{0, 99}], State)
                 || ListId <- [<<"500">>, <<"600">>, <<"500">>]
                ]
            end,
            ?assertEqual(Build(), with_member_item_memo(Build))
        after
            guild_member_list_engine:destroy(Ref),
            guild_member_list_engine:destroy(Other)
        end
    end).

range_presence(Status) ->
    #{
        <<"status">> => Status,
        <<"mobile">> => false,
        <<"afk">> => false,
        <<"custom_status">> => null
    }.

with_range_world(Fun) ->
    Tab = ets:new(range_presence, [set, public]),
    Ref = guild_member_list_engine:new(),
    _ = erlang:erase(guild_member_list_sync_item_cache),
    _ = erlang:erase(?PRESENCE_WRITES_KEY),
    try
        Users = lists:seq(1, 6),
        true = ets:insert(Tab, [{U, range_presence(<<"online">>)} || U <- Users]),
        ok = guild_member_list_engine:bulk_load(
            Ref,
            [{U, <<"u", (integer_to_binary(U))/binary>>, [], U =< 4} || U <- Users],
            []
        ),
        State = #{
            id => 9,
            data => #{
                member_list_revision => make_ref(),
                <<"members">> => maps:from_list([
                    {U, memo_member(U, <<"u", (integer_to_binary(U))/binary>>)}
                 || U <- Users
                ])
            },
            member_presence => Tab,
            connected_user_ids => sets:from_list([1, 2, 3, 4]),
            channel_member_list_engines => #{<<"500">> => Ref}
        },
        Fun(State, Tab, Ref)
    after
        _ = erlang:erase(guild_member_list_sync_item_cache),
        _ = erlang:erase(?PRESENCE_WRITES_KEY),
        guild_member_list_engine:destroy(Ref),
        ets:delete(Tab),
        flush_range_timers()
    end.

flush_range_timers() ->
    receive
        {timeout, _, member_list_sync_item_cache_rotate} -> flush_range_timers()
    after 0 ->
        ok
    end.

range_op(State) ->
    [Op] = maps:get(<<"ops">>, build_sync_response(9, <<"500">>, [{0, 99}], State)),
    Op.

uncached_range_op(State) ->
    Parent = self(),
    Pid = spawn(fun() -> Parent ! {range_op, self(), range_op(State)} end),
    receive
        {range_op, Pid, Op} -> Op
    after 5000 ->
        erlang:error(uncached_range_op_timeout)
    end.

range_items_by_user(Op) ->
    maps:from_list([
        {maps:get(<<"id">>, maps:get(<<"user">>, M)), I}
     || #{<<"member">> := M} = I <- maps:get(<<"items">>, Op)
    ]).

range_cache_reuses_unchanged_range_test() ->
    with_range_world(fun(State, _Tab, _Ref) ->
        First = range_op(State),
        Second = range_op(State),
        ?assert(erts_debug:same(First, Second)),
        ?assertEqual(uncached_range_op(State), Second),
        ok = note_presence_write(77),
        ?assert(erts_debug:same(First, range_op(State)))
    end).

range_cache_patches_only_the_written_member_test() ->
    with_range_world(fun(State, Tab, _Ref) ->
        Before = range_items_by_user(range_op(State)),
        true = ets:insert(Tab, {2, range_presence(<<"dnd">>)}),
        ok = note_presence_write(2),
        Op = range_op(State),
        ?assertEqual(uncached_range_op(State), Op),
        After = range_items_by_user(Op),
        ?assertNotEqual(maps:get(<<"2">>, Before), maps:get(<<"2">>, After)),
        [
            ?assert(erts_debug:same(maps:get(Id, Before), maps:get(Id, After)))
         || Id <- maps:keys(Before), Id =/= <<"2">>
        ]
    end).

range_cache_follows_presence_deletes_test() ->
    with_range_world(fun(State, Tab, _Ref) ->
        _ = range_op(State),
        true = ets:delete(Tab, 3),
        ok = note_presence_write(3),
        ?assertEqual(uncached_range_op(State), range_op(State))
    end).

range_cache_rebuilds_after_engine_change_test() ->
    with_range_world(fun(State, _Tab, Ref) ->
        First = range_op(State),
        ok = guild_member_list_engine:set_online(Ref, 2, false),
        Op = range_op(State),
        ?assertNotEqual(First, Op),
        ?assertEqual(uncached_range_op(State), Op)
    end).

range_cache_patches_connectivity_change_test() ->
    with_range_world(fun(State, _Tab, _Ref) ->
        _ = range_op(State),
        Disconnected = State#{connected_user_ids => sets:from_list([1, 3, 4])},
        Op = range_op(Disconnected),
        ?assertEqual(uncached_range_op(Disconnected), Op),
        ?assertEqual(uncached_range_op(State), range_op(State))
    end).

range_cache_rebuilds_on_member_change_test() ->
    with_range_world(fun(State, _Tab, _Ref) ->
        _ = range_op(State),
        #{data := Data} = State,
        Renamed = State#{
            data => guild_data_index:put_member(memo_member(5, <<"renamed">>), Data)
        },
        ?assertEqual(uncached_range_op(Renamed), range_op(Renamed))
    end).

range_cache_rebuilds_when_write_log_overflows_test() ->
    with_range_world(fun(State, Tab, _Ref) ->
        _ = range_op(State),
        true = ets:insert(Tab, {1, range_presence(<<"idle">>)}),
        ok = note_presence_write(1),
        [ok = note_presence_write(1000 + N) || N <- lists:seq(1, 3 * ?PRESENCE_WRITES_KEPT)],
        ?assertEqual(stale, presence_written_since(0)),
        ?assertEqual(uncached_range_op(State), range_op(State))
    end).

range_cache_is_off_when_disabled_test() ->
    with_range_world(fun(State, _Tab, _Ref) ->
        application:set_env(fluxer_gateway, member_list_sync_item_cache_enabled, false),
        try
            First = range_op(State),
            ?assertNot(erts_debug:same(First, range_op(State))),
            ?assertEqual(undefined, erlang:get(guild_member_list_sync_item_cache))
        after
            application:unset_env(fluxer_gateway, member_list_sync_item_cache_enabled)
        end
    end).

presence_written_since_reports_writes_in_order_test() ->
    _ = erlang:erase(?PRESENCE_WRITES_KEY),
    try
        ?assertEqual({ok, []}, presence_written_since(0)),
        ok = note_presence_write(5),
        ok = note_presence_write(6),
        ?assertEqual({ok, [6, 5]}, presence_written_since(0)),
        ?assertEqual({ok, [6]}, presence_written_since(1)),
        ?assertEqual({ok, []}, presence_written_since(2)),
        ?assertEqual(stale, presence_written_since(3))
    after
        erlang:erase(?PRESENCE_WRITES_KEY)
    end.

range_cache_requires_a_member_revision_test() ->
    with_range_world(fun(State, _Tab, _Ref) ->
        #{data := Data} = State,
        lists:foreach(
            fun(LegacyData) ->
                Legacy = State#{data => LegacyData},
                First = range_op(Legacy),
                ?assertEqual(uncached_range_op(Legacy), First),
                ?assertNot(erts_debug:same(First, range_op(Legacy))),
                ?assertEqual(undefined, erlang:get(guild_member_list_sync_item_cache))
            end,
            [maps:remove(member_list_revision, Data), Data#{member_list_revision => invalid}]
        )
    end).

range_cache_does_not_retain_unrendered_members_or_connections_test() ->
    with_range_world(fun(State, _Tab, _Ref) ->
        #{data := Data} = State,
        Members = guild_data_index:member_map(Data),
        SmallData = guild_data_index:put_member_map(Members, Data),
        Small = State#{data => SmallData},
        SmallOp = range_op(Small),
        SmallSize = erts_debug:flat_size(erlang:get(guild_member_list_sync_item_cache)),
        Extra = maps:from_list([{U, memo_member(U, <<"extra">>)} || U <- lists:seq(7, 5000)]),
        LargeData = guild_data_index:put_member_map(maps:merge(Members, Extra), SmallData),
        Large = Small#{
            data => LargeData,
            connected_user_ids => sets:from_list([1, 2, 3, 4] ++ lists:seq(7, 5000))
        },
        _ = erlang:erase(guild_member_list_sync_item_cache),
        ?assertEqual(SmallOp, range_op(Large)),
        ?assertEqual(
            SmallSize, erts_debug:flat_size(erlang:get(guild_member_list_sync_item_cache))
        )
    end).

range_cache_does_not_retain_map_backed_presence_test() ->
    with_range_world(fun(State, Tab, _Ref) ->
        MapState = State#{member_presence => maps:from_list(ets:tab2list(Tab))},
        First = range_op(MapState),
        ?assertEqual(uncached_range_op(MapState), First),
        ?assertNot(erts_debug:same(First, range_op(MapState))),
        ?assertEqual(undefined, erlang:get(guild_member_list_sync_item_cache))
    end).

member_item_memo_does_not_retain_unrelated_connections_test() ->
    with_memo_presence_tab(fun(Tab) ->
        MemberMap = #{1 => memo_member(1, <<"one">>)},
        Small = memo_presence_ctx(Tab, [1]),
        Large = memo_presence_ctx(Tab, lists:seq(1, 5000)),
        with_member_item_memo(fun() ->
            {true, First} = hydrate_member_ref(1, MemberMap, Small),
            SmallSize = erts_debug:flat_size(erlang:get(?MEMBER_ITEM_MEMO_KEY)),
            {true, Second} = hydrate_member_ref(1, MemberMap, Large),
            ?assert(erts_debug:same(First, Second)),
            ?assertEqual(SmallSize, erts_debug:flat_size(erlang:get(?MEMBER_ITEM_MEMO_KEY)))
        end)
    end).

member_item_memo_follows_presence_writes_in_scope_test() ->
    with_memo_presence_tab(fun(Tab) ->
        MemberMap = #{1 => memo_member(1, <<"one">>)},
        Ctx = memo_presence_ctx(Tab, [1]),
        with_member_item_memo(fun() ->
            Before = hydrate_member_ref(1, MemberMap, Ctx),
            true = ets:insert(Tab, {1, range_presence(<<"dnd">>)}),
            ok = note_presence_write(1),
            After = hydrate_member_ref(1, MemberMap, Ctx),
            ?assertNotEqual(Before, After)
        end)
    end).

-endif.
