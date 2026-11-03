%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_member_list_read).
-typing([eqwalizer]).

-export([
    get_counts/2,
    get_online_count/1,
    build_sync_response/4,
    build_sync_response_builder/3,
    build_normalized_sync_response_builder/3,
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
-type users() :: #{integer() => true}.
-type range_entry() :: {range_stamp(), non_neg_integer(), users(), [term()], map(), users()}.

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
            complete_writes(Count, [
                UserId
             || {WriteSeq, UserId} <- lists:sublist(Log, Count), WriteSeq > Since
            ]);
        _ ->
            stale
    end.

-spec complete_writes(pos_integer(), [integer()]) -> {ok, [integer()]} | stale.
complete_writes(Count, Written) when length(Written) =:= Count -> {ok, Written};
complete_writes(_Count, _Written) -> stale.

-spec with_member_item_memo(fun(() -> T)) -> T.
with_member_item_memo(Fun) ->
    case erlang:get(?MEMBER_ITEM_MEMO_KEY) of
        undefined -> with_fresh_member_item_memo(Fun);
        _ -> Fun()
    end.

-spec with_fresh_member_item_memo(fun(() -> T)) -> T.
with_fresh_member_item_memo(Fun) ->
    _ = erlang:put(?MEMBER_ITEM_MEMO_KEY, #{}),
    try
        Fun()
    after
        erlang:erase(?MEMBER_ITEM_MEMO_KEY)
    end.

-spec get_counts(list_id(), guild_state()) -> {non_neg_integer(), non_neg_integer()}.
get_counts(ListId, State) ->
    case store_ref_for(ListId, State) of
        undefined -> {0, 0};
        Ref -> guild_member_list_engine:get_counts(Ref)
    end.

-spec get_online_count(guild_state()) -> non_neg_integer().
get_online_count(State) ->
    case store_ref_for(<<"0">>, State) of
        undefined ->
            0;
        Ref ->
            {_Total, Online} = guild_member_list_engine:get_counts(Ref),
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
    {MemberCount, OnlineCount} = guild_member_list_engine:get_counts(Ref),
    StoreGroups = guild_member_list_engine:get_groups(Ref),
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
        member_map => guild_data_index_members:member_map(Data),
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
range_stamp(Ref, #{hide_offline := HideOffline, member_revision := Revision} = ReadCtx) when
    is_reference(Revision)
->
    #{presence_context := PresenceCtx} = ReadCtx,
    Presence = maps:get(member_presence, PresenceCtx, undefined),
    case presence_writes_seen_here(Presence) of
        true -> versioned_stamp(Ref, HideOffline, Revision, Presence);
        false -> undefined
    end;
range_stamp(_Ref, #{hide_offline := _, member_revision := _, presence_context := _}) ->
    undefined.

-spec versioned_stamp(guild_member_list_store:store_ref(), boolean(), reference(), term()) ->
    range_stamp() | undefined.
versioned_stamp(Ref, HideOffline, Revision, Presence) ->
    case guild_member_list_engine:version(Ref) of
        Version when is_integer(Version) ->
            {Version, HideOffline, Revision, eqwalizer:dynamic_cast(Presence)};
        _ ->
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
    case eqwalizer:dynamic_cast(guild_member_list_subscribe:sync_cache_find(Key)) of
        {ok, {_Stamp0, _Seq0, _Users, _Keys, _Op, _Connected0} = Cached} ->
            reuse_range_entry(Cached, Stamp, Ref, Range, StoreGroups, ReadCtx);
        _ ->
            new_range_entry(Stamp, Ref, Range, StoreGroups, ReadCtx)
    end.

-spec reuse_range_entry(
    {term(), term(), users(), [term()], map(), users()},
    range_stamp(),
    guild_member_list_store:store_ref(),
    range(),
    [{binary(), non_neg_integer()}],
    read_context()
) -> range_entry().
reuse_range_entry(
    {Stamp0, Seq0, Users, Keys, Op, Connected0}, Stamp, Ref, Range, StoreGroups, ReadCtx
) ->
    Connected = range_connected_users(Users, ReadCtx),
    case changed_range_users(Stamp0, Stamp, Seq0, Users, Connected0, Connected) of
        {ok, Changed} ->
            patch_range_entry(Stamp, Users, Keys, Op, Changed, Connected, ReadCtx);
        rebuild ->
            new_range_entry(Stamp, Ref, Range, StoreGroups, ReadCtx)
    end.

-spec changed_range_users(term(), range_stamp(), term(), users(), users(), users()) ->
    {ok, users()} | rebuild.
changed_range_users(Stamp, Stamp, Seq0, Users, Connected0, Connected) when
    is_integer(Seq0), is_map(Users)
->
    case presence_written_since(Seq0) of
        {ok, Written} ->
            Rewritten = maps:from_list([{U, true} || U <- Written, maps:is_key(U, Users)]),
            {ok, maps:merge(Rewritten, reconnected_users(Users, Connected0, Connected))};
        stale ->
            rebuild
    end;
changed_range_users(_Stamp0, _Stamp, _Seq0, _Users, _Connected0, _Connected) ->
    rebuild.

-spec reconnected_users(users(), users(), users()) -> users().
reconnected_users(_Users, Connected, Connected) ->
    #{};
reconnected_users(Users, Connected0, Connected) ->
    maps:filter(
        fun(UserId, _) ->
            maps:is_key(UserId, Connected0) =/= maps:is_key(UserId, Connected)
        end,
        Users
    ).

-spec range_connected_users(users(), read_context()) -> users().
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
    range_stamp(), users(), [term()], map(), users(), users(), read_context()
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
            (UserId, _Item) when is_integer(UserId), is_map_key(UserId, Changed) ->
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
    guild_member_list_engine:get_items(Ref, Start, End);
store_range_items_from_context(Ref, StoreGroups, Start, End, #{hide_offline := true}) ->
    visible_range_items(Ref, StoreGroups, Start, End).

-spec sync_op(range(), [list_item()]) -> map().
sync_op({Start, End}, Items) ->
    #{<<"op">> => <<"SYNC">>, <<"range">> => [Start, End], <<"items">> => Items}.

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
            Hydrated = hydrate_store_item(
                StoreItem, ListId, HideOffline, MemberMap, PresenceCtx, State, {[], Section}
            ),
            add_keyed_item(StoreItem, Hydrated, Acc)
        end,
        {[], undefined},
        StoreItems
    ),
    lists:reverse(Keyed).

-spec add_keyed_item(store_item(), {[list_item()], term()}, [{integer() | group, list_item()}]) ->
    {[{integer() | group, list_item()}], term()}.
add_keyed_item(StoreItem, {[Item], Section}, Acc) ->
    {[{store_item_key(StoreItem), Item} | Acc], Section};
add_keyed_item(_StoreItem, {[], Section}, Acc) ->
    {Acc, Section}.

-spec store_item_key(store_item()) -> integer() | group.
store_item_key({member, UserId}) -> UserId;
store_item_key({group, _Id, _Count}) -> group.

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
            memo_member_item(UserId, Member, PresenceCtx, Items, Stamp);
        _ ->
            build_member_item(UserId, Member, PresenceCtx)
    end.

-spec memo_member_item(integer(), map(), map(), map(), tuple()) -> list_item().
memo_member_item(UserId, Member, PresenceCtx, Items, Stamp) ->
    case maps:find(UserId, Items) of
        {ok, {Member, Stamp, Item}} ->
            eqwalizer:dynamic_cast(Item);
        _ ->
            Item = build_member_item(UserId, Member, PresenceCtx),
            _ = erlang:put(?MEMBER_ITEM_MEMO_KEY, Items#{UserId => {Member, Stamp, Item}}),
            Item
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
        false -> guild_member_list_engine:get_items(Ref, Start, min(End, VisibleEnd))
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
