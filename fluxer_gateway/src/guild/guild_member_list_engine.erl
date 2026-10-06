%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_member_list_engine).
-typing([eqwalizer]).

-export([
    new/0,
    clone/1,
    destroy/1,
    bulk_load/3,
    add_member/5,
    remove_member/2,
    update_member/5,
    set_online/3,
    set_hoisted_roles/2,
    get_counts/1,
    get_groups/1,
    get_items/3,
    get_all_item_keys/1,
    get_sorted_user_ids/1,
    index_of/2,
    is_member_online/2,
    version/1
]).

-export([info/1]).

-define(ONLINE_IDX, 16#F0000000).
-define(OFFLINE_IDX, 16#F0000001).

-type item() :: {group, binary(), non_neg_integer()} | {member, integer()}.

-export_type([item/0]).

-spec new() -> ets:table().
new() ->
    Ref = ets:new(?MODULE, [set, public]),
    OSet = guild_member_list_oset:new(),
    ITab = ets:new(engine_index, [set, public]),
    ets:insert(Ref, [
        {tabs, OSet, ITab},
        {section_order, default_section_order()},
        {hoisted_idx_map, #{}},
        {hoisted_role_ids, []},
        {total_count, 0},
        {online_count, 0},
        {version, 0},
        {{section_count, ?ONLINE_IDX}, 0},
        {{section_count, ?OFFLINE_IDX}, 0}
    ]),
    Ref.

-spec clone(ets:table()) -> ets:table().
clone(Source) ->
    {SourceOSet, SourceITab} = lookup_tabs(Source),
    Ref = new(),
    {OSet, ITab} = lookup_tabs(Ref),
    true = ets:insert(ITab, ets:tab2list(SourceITab)),
    ok = guild_member_list_oset:from_sorted(OSet, guild_member_list_oset:to_list(SourceOSet)),
    true = ets:insert(Ref, [Row || Row <- ets:tab2list(Source), element(1, Row) =/= tabs]),
    ok = bump_version(Ref),
    Ref.

-spec destroy(term()) -> ok.
destroy(Ref) ->
    try ets:lookup(eqwalizer:dynamic_cast(Ref), tabs) of
        [{tabs, OSet, ITab}] ->
            guild_member_list_oset:destroy(OSet),
            safe_delete(ITab);
        _ ->
            ok
    catch
        _:_ -> ok
    end,
    safe_delete(Ref),
    ok.

-spec safe_delete(term()) -> ok.
safe_delete(Tab) ->
    try ets:delete(eqwalizer:dynamic_cast(Tab)) of
        _ -> ok
    catch
        _:_ -> ok
    end.

-spec bulk_load(ets:table(), [{integer(), binary(), [integer()], boolean()}], [integer()]) ->
    ok.
bulk_load(Ref, Members, HoistedRoleIds) ->
    {OSet, ITab} = lookup_tabs(Ref),
    {SectionOrder, HIdxMap} = build_section_config(HoistedRoleIds),
    store_section_config(Ref, SectionOrder, HIdxMap, HoistedRoleIds),
    SC0 = maps:from_list([{Idx, 0} || {Idx, _} <- SectionOrder]),
    {Total, Online, SC, Keys} = lists:foldl(
        fun(Member, Acc) ->
            bulk_load_member(Member, Acc, ITab, HIdxMap)
        end,
        {0, 0, SC0, []},
        Members
    ),
    guild_member_list_oset:from_sorted(OSet, Keys),
    store_counts(Ref, Total, Online),
    store_section_counts(Ref, SC),
    bump_version(Ref).

-spec bulk_load_member(
    {integer(), binary(), [integer()], boolean()},
    {non_neg_integer(), non_neg_integer(), #{non_neg_integer() => non_neg_integer()}, [
        guild_member_list_oset:key()
    ]},
    ets:table(),
    #{integer() => non_neg_integer()}
) ->
    {non_neg_integer(), non_neg_integer(), #{non_neg_integer() => non_neg_integer()}, [
        guild_member_list_oset:key()
    ]}.
bulk_load_member({UserId, _SK, _RI, _IO}, Acc, _ITab, _HIdxMap) when
    not is_integer(UserId); UserId =< 0
->
    Acc;
bulk_load_member(
    {UserId, SortKey, RoleIds, IsOnline},
    {T, O, Counts, Keys},
    ITab,
    HIdxMap
) ->
    SIdx = compute_section(IsOnline, RoleIds, HIdxMap),
    ets:insert(ITab, {UserId, SortKey, SIdx, RoleIds, IsOnline}),
    O2 = online_delta(O, IsOnline, 1),
    {T + 1, O2, incr_count(SIdx, Counts), [{SIdx, SortKey, UserId} | Keys]}.

-spec add_member(ets:table(), integer(), binary(), [integer()], boolean()) -> ok.
add_member(_Ref, UserId, _SortKey, _RoleIds, _IsOnline) when
    not is_integer(UserId); UserId =< 0
->
    ok;
add_member(Ref, UserId, SortKey, RoleIds, IsOnline) ->
    {OSet, ITab} = lookup_tabs(Ref),
    HMap = lookup_hoisted_idx_map(Ref),
    _ = remove_existing_entry(Ref, OSet, ITab, UserId),
    SIdx = compute_section(IsOnline, RoleIds, HMap),
    _ = guild_member_list_oset:insert(OSet, {SIdx, SortKey, UserId}),
    ets:insert(ITab, {UserId, SortKey, SIdx, RoleIds, IsOnline}),
    ensure_section_counter(Ref, SIdx),
    _ = ets:update_counter(Ref, {section_count, SIdx}, 1),
    _ = ets:update_counter(Ref, total_count, 1),
    ok = adjust_online_counter(Ref, IsOnline, 1),
    bump_version(Ref).

-spec remove_member(ets:table(), integer()) -> ok.
remove_member(Ref, UserId) ->
    do_remove(Ref, UserId).

-spec update_member(ets:table(), integer(), binary(), [integer()], boolean()) -> ok.
update_member(Ref, UserId, SortKey, RoleIds, IsOnline) ->
    add_member(Ref, UserId, SortKey, RoleIds, IsOnline).

-spec set_online(ets:table(), integer(), boolean()) -> ok.
set_online(Ref, UserId, IsOnline) ->
    {OSet, ITab} = lookup_tabs(Ref),
    case ets:lookup(ITab, UserId) of
        [{UserId, _, _, _, OldOnline}] when OldOnline =:= IsOnline ->
            ok;
        [{UserId, SortKey, OldSIdx, RoleIds, _OldOnline}] ->
            move_section(Ref, OSet, ITab, UserId, SortKey, OldSIdx, RoleIds, IsOnline);
        [] ->
            ok
    end.

-spec set_hoisted_roles(ets:table(), [integer()]) -> changed | unchanged.
set_hoisted_roles(Ref, NewHoistedRoleIds) ->
    [{hoisted_role_ids, OldIds}] = ets:lookup(Ref, hoisted_role_ids),
    case OldIds =:= NewHoistedRoleIds of
        true ->
            unchanged;
        false ->
            ok = do_set_hoisted_roles(Ref, NewHoistedRoleIds),
            changed
    end.

-spec get_counts(ets:table()) -> {non_neg_integer(), non_neg_integer()}.
get_counts(Ref) ->
    Total = ets:lookup_element(Ref, total_count, 2),
    Online = ets:lookup_element(Ref, online_count, 2),
    {Total, Online}.

-spec get_groups(ets:table()) -> [{binary(), non_neg_integer()}].
get_groups(Ref) ->
    SO = lookup_section_order(Ref),
    [{DisplayId, read_section_count(Ref, Idx)} || {Idx, DisplayId} <- SO].

-spec get_items(ets:table(), non_neg_integer(), non_neg_integer()) -> [item()].
get_items(_Ref, Start, End) when Start > End ->
    [];
get_items(Ref, Start, End) ->
    {OSet, _ITab} = lookup_tabs(Ref),
    SO = lookup_section_order(Ref),
    lists:reverse(collect_range(OSet, Ref, SO, Start, End, 0, 0, [])).

-spec index_of(ets:table(), integer()) -> non_neg_integer() | not_found.
index_of(Ref, UserId) ->
    {OSet, ITab} = lookup_tabs(Ref),
    case ets:lookup(ITab, UserId) of
        [{UserId, SortKey, SIdx, _RoleIds, _IsOnline}] ->
            index_of_member(OSet, Ref, SIdx, SortKey, UserId);
        [] ->
            not_found
    end.

-spec index_of_member(
    guild_member_list_oset:oset(), ets:table(), non_neg_integer(), binary(), integer()
) ->
    non_neg_integer() | not_found.
index_of_member(OSet, Ref, SIdx, SortKey, UserId) ->
    case guild_member_list_oset:rank(OSet, {SIdx, SortKey, UserId}) of
        not_found -> not_found;
        MemberRank -> MemberRank + headers_before(Ref, SIdx)
    end.

-spec is_member_online(ets:table(), integer()) -> boolean() | not_present.
is_member_online(Ref, UserId) ->
    {_OSet, ITab} = lookup_tabs(Ref),
    case ets:lookup(ITab, UserId) of
        [{UserId, _SortKey, _SIdx, _RoleIds, IsOnline}] -> IsOnline;
        [] -> not_present
    end.

-spec get_all_item_keys(ets:table()) -> [item()].
get_all_item_keys(Ref) ->
    {OSet, _ITab} = lookup_tabs(Ref),
    SO = lookup_section_order(Ref),
    {_Base, Rev} = lists:foldl(
        fun(Section, Acc) -> collect_all_section_items(Section, Ref, OSet, Acc) end,
        {0, []},
        SO
    ),
    lists:reverse(Rev).

-spec collect_all_section_items(
    {non_neg_integer(), binary()},
    ets:table(),
    guild_member_list_oset:oset(),
    {non_neg_integer(), [item()]}
) ->
    {non_neg_integer(), [item()]}.
collect_all_section_items({SIdx, DisplayId}, Ref, OSet, {Base, Acc}) ->
    case read_section_count(Ref, SIdx) of
        0 ->
            {Base, Acc};
        Count ->
            Members = section_members(OSet, Base, Count),
            {Base + Count, prepend_members(Members, [{group, DisplayId, 0} | Acc])}
    end.

-spec get_sorted_user_ids(ets:table()) -> [integer()].
get_sorted_user_ids(Ref) ->
    {OSet, _ITab} = lookup_tabs(Ref),
    [UId || {_SIdx, _SK, UId} <- guild_member_list_oset:to_list(OSet), is_integer(UId)].

-spec info(ets:table()) -> map().
info(Ref) ->
    {OSet, ITab} = lookup_tabs(Ref),
    {Total, Online} = get_counts(Ref),
    WordSize = erlang:system_info(wordsize),
    MembersMem = guild_member_list_oset:memory_bytes(OSet),
    IndexMem = ets_mem(ITab) * WordSize,
    RefMem = ets_mem(Ref) * WordSize,
    #{
        total => Total,
        online => Online,
        members_table_bytes => MembersMem,
        index_table_bytes => IndexMem,
        ref_table_bytes => RefMem,
        total_bytes => MembersMem + IndexMem + RefMem
    }.

-spec ets_mem(ets:table()) -> non_neg_integer().
ets_mem(Tab) ->
    case ets:info(Tab, memory) of
        N when is_integer(N), N >= 0 -> N;
        _ -> 0
    end.

-spec do_remove(ets:table(), integer()) -> ok.
do_remove(Ref, UserId) ->
    {OSet, ITab} = lookup_tabs(Ref),
    case ets:lookup(ITab, UserId) of
        [{UserId, SortKey, SIdx, _RoleIds, IsOnline}] ->
            _ = guild_member_list_oset:delete(OSet, {SIdx, SortKey, UserId}),
            ets:delete(ITab, UserId),
            _ = ets:update_counter(Ref, {section_count, SIdx}, -1),
            _ = ets:update_counter(Ref, total_count, -1),
            ok = adjust_online_counter(Ref, IsOnline, -1),
            bump_version(Ref);
        [] ->
            ok
    end.

-spec ensure_section_counter(ets:table(), non_neg_integer()) -> true | ok.
ensure_section_counter(Ref, SIdx) ->
    case ets:lookup(Ref, {section_count, SIdx}) of
        [] -> ets:insert(Ref, {{section_count, SIdx}, 0});
        _ -> ok
    end.

-spec remove_existing_entry(ets:table(), guild_member_list_oset:oset(), ets:table(), integer()) ->
    ok.
remove_existing_entry(Ref, OSet, ITab, UserId) ->
    case ets:lookup(ITab, UserId) of
        [{UserId, OldSK, OldSIdx, _OldRI, OldOnline}] ->
            _ = guild_member_list_oset:delete(OSet, {OldSIdx, OldSK, UserId}),
            _ = ets:update_counter(Ref, {section_count, OldSIdx}, -1),
            _ = ets:update_counter(Ref, total_count, -1),
            adjust_online_counter(Ref, OldOnline, -1);
        [] ->
            ok
    end.

-spec move_section(
    ets:table(),
    guild_member_list_oset:oset(),
    ets:table(),
    integer(),
    binary(),
    non_neg_integer(),
    [integer()],
    boolean()
) -> ok.
move_section(Ref, OSet, ITab, UserId, SortKey, OldSIdx, RoleIds, IsOnline) ->
    HMap = lookup_hoisted_idx_map(Ref),
    NewSIdx = compute_section(IsOnline, RoleIds, HMap),
    _ = guild_member_list_oset:delete(OSet, {OldSIdx, SortKey, UserId}),
    _ = ets:update_counter(Ref, {section_count, OldSIdx}, -1),
    _ = guild_member_list_oset:insert(OSet, {NewSIdx, SortKey, UserId}),
    ets:insert(ITab, {UserId, SortKey, NewSIdx, RoleIds, IsOnline}),
    ensure_section_counter(Ref, NewSIdx),
    _ = ets:update_counter(Ref, {section_count, NewSIdx}, 1),
    OnlineDelta =
        case IsOnline of
            true -> 1;
            false -> -1
        end,
    _ = ets:update_counter(Ref, online_count, OnlineDelta),
    bump_version(Ref).

-spec version(ets:table()) -> non_neg_integer() | undefined.
version(Ref) ->
    try ets:lookup_element(Ref, version, 2) of
        Version when is_integer(Version) -> Version;
        _ -> undefined
    catch
        error:badarg -> undefined
    end.

-spec bump_version(ets:table()) -> ok.
bump_version(Ref) ->
    _ = ets:update_counter(Ref, version, 1, {version, 0}),
    ok.

-spec adjust_online_counter(ets:table(), boolean(), integer()) -> ok.
adjust_online_counter(_Ref, false, _Delta) ->
    ok;
adjust_online_counter(Ref, true, Delta) ->
    _ = ets:update_counter(Ref, online_count, Delta),
    ok.

-spec online_delta(non_neg_integer(), boolean(), integer()) -> non_neg_integer().
online_delta(Current, true, Delta) -> Current + Delta;
online_delta(Current, false, _Delta) -> Current.

-spec incr_count(non_neg_integer(), #{non_neg_integer() => non_neg_integer()}) ->
    #{non_neg_integer() => non_neg_integer()}.
incr_count(Key, Map) ->
    maps:update_with(Key, fun(C) -> C + 1 end, 1, Map).

-spec store_counts(ets:table(), non_neg_integer(), non_neg_integer()) -> true.
store_counts(Ref, Total, Online) ->
    ets:insert(Ref, [{total_count, Total}, {online_count, Online}]).

-spec store_section_counts(ets:table(), #{non_neg_integer() => non_neg_integer()}) -> ok.
store_section_counts(Ref, SC) ->
    maps:foreach(
        fun(Idx, Count) ->
            ets:insert(Ref, {{section_count, Idx}, Count})
        end,
        SC
    ).

-spec store_section_config(
    ets:table(),
    [{non_neg_integer(), binary()}],
    #{integer() => non_neg_integer()},
    [integer()]
) -> true.
store_section_config(Ref, SectionOrder, HIdxMap, HoistedRoleIds) ->
    ets:insert(Ref, [
        {section_order, SectionOrder},
        {hoisted_idx_map, HIdxMap},
        {hoisted_role_ids, HoistedRoleIds}
    ]).

-spec clear_old_section_counts(ets:table(), [{non_neg_integer(), binary()}]) -> ok.
clear_old_section_counts(Ref, OldSO) ->
    lists:foreach(
        fun({OldIdx, _}) ->
            ets:delete(Ref, {section_count, OldIdx})
        end,
        OldSO
    ).

-spec lookup_tabs(ets:table()) -> {guild_member_list_oset:oset(), ets:table()}.
lookup_tabs(Ref) ->
    [{tabs, T1, ITab}] = ets:lookup(Ref, tabs),
    {T1, ITab}.

-spec lookup_hoisted_idx_map(ets:table()) -> #{integer() => non_neg_integer()}.
lookup_hoisted_idx_map(Ref) ->
    [{hoisted_idx_map, HMap}] = ets:lookup(Ref, hoisted_idx_map),
    HMap.

-spec lookup_section_order(ets:table()) -> [{non_neg_integer(), binary()}].
lookup_section_order(Ref) ->
    [{section_order, SO}] = ets:lookup(Ref, section_order),
    SO.

-spec do_set_hoisted_roles(ets:table(), [integer()]) -> ok.
do_set_hoisted_roles(Ref, NewHoistedRoleIds) ->
    {OldOSet, ITab} = lookup_tabs(Ref),
    OldSO = lookup_section_order(Ref),
    {NewSO, NewIdxMap} = build_section_config(NewHoistedRoleIds),
    NewOSet = guild_member_list_oset:new(),
    SC = rebuild_members_into(ITab, NewOSet, NewIdxMap),
    guild_member_list_oset:destroy(OldOSet),
    clear_old_section_counts(Ref, OldSO),
    ets:insert(Ref, [
        {tabs, NewOSet, ITab},
        {section_order, NewSO},
        {hoisted_idx_map, NewIdxMap},
        {hoisted_role_ids, NewHoistedRoleIds}
    ]),
    store_section_counts(Ref, SC),
    bump_version(Ref).

-spec rebuild_members_into(
    ets:table(), guild_member_list_oset:oset(), #{integer() => non_neg_integer()}
) -> #{non_neg_integer() => non_neg_integer()}.
rebuild_members_into(ITab, NewOSet, NewIdxMap) ->
    {Counts, Keys, Rows} = ets:foldl(
        fun(Row, Acc) ->
            rehoist_member(Row, NewIdxMap, Acc)
        end,
        {#{}, [], []},
        ITab
    ),
    ets:insert(ITab, Rows),
    guild_member_list_oset:from_sorted(NewOSet, Keys),
    Counts.

-spec rehoist_member(
    {integer(), binary(), non_neg_integer(), [integer()], boolean()},
    #{integer() => non_neg_integer()},
    {#{non_neg_integer() => non_neg_integer()}, [guild_member_list_oset:key()], [
        {integer(), binary(), non_neg_integer(), [integer()], boolean()}
    ]}
) ->
    {#{non_neg_integer() => non_neg_integer()}, [guild_member_list_oset:key()], [
        {integer(), binary(), non_neg_integer(), [integer()], boolean()}
    ]}.
rehoist_member(
    {UserId, SortKey, _OldSIdx, RoleIds, IsOnline},
    NewIdxMap,
    {Counts, Keys, Rows}
) ->
    NewSIdx = compute_section(IsOnline, RoleIds, NewIdxMap),
    Row = {UserId, SortKey, NewSIdx, RoleIds, IsOnline},
    {incr_count(NewSIdx, Counts), [{NewSIdx, SortKey, UserId} | Keys], [Row | Rows]}.

-spec default_section_order() -> [{non_neg_integer(), binary()}].
default_section_order() ->
    [{?ONLINE_IDX, <<"online">>}, {?OFFLINE_IDX, <<"offline">>}].

-spec build_section_config([integer()]) ->
    {[{non_neg_integer(), binary()}], #{integer() => non_neg_integer()}}.
build_section_config([]) ->
    {default_section_order(), #{}};
build_section_config(HoistedRoleIds) ->
    Indexed = lists:zip(
        lists:seq(0, length(HoistedRoleIds) - 1),
        HoistedRoleIds
    ),
    HoistedSections = [{Idx, integer_to_binary(RId)} || {Idx, RId} <- Indexed],
    IdxMap = maps:from_list(
        [{RId, Idx} || {Idx, RId} <- Indexed]
    ),
    SO = HoistedSections ++ default_section_order(),
    {SO, IdxMap}.

-spec compute_section(boolean(), [integer()], #{integer() => non_neg_integer()}) ->
    non_neg_integer().
compute_section(false, _, _) ->
    ?OFFLINE_IDX;
compute_section(true, _, M) when map_size(M) =:= 0 ->
    ?ONLINE_IDX;
compute_section(true, RoleIds, HIdxMap) ->
    case find_best_section(RoleIds, HIdxMap) of
        undefined -> ?ONLINE_IDX;
        Idx -> Idx
    end.

-spec find_best_section([integer()], #{integer() => non_neg_integer()}) ->
    non_neg_integer() | undefined.
find_best_section(RoleIds, HIdxMap) ->
    find_best_loop(RoleIds, HIdxMap, undefined).

-spec find_best_loop(
    [integer()], #{integer() => non_neg_integer()}, non_neg_integer() | undefined
) -> non_neg_integer() | undefined.
find_best_loop([], _, Best) ->
    Best;
find_best_loop([RId | Rest], Map, Best) ->
    case maps:find(RId, Map) of
        {ok, Idx} when Best =:= undefined orelse Idx < Best ->
            find_best_loop(Rest, Map, Idx);
        _ ->
            find_best_loop(Rest, Map, Best)
    end.

-spec read_section_count(ets:table(), non_neg_integer()) -> non_neg_integer().
read_section_count(Ref, SIdx) ->
    try ets:lookup_element(Ref, {section_count, SIdx}, 2) of
        Count -> Count
    catch
        error:badarg -> 0
    end.

-spec headers_before(ets:table(), non_neg_integer()) -> non_neg_integer().
headers_before(Ref, SIdx) ->
    headers_before_loop(lookup_section_order(Ref), Ref, SIdx, 1).

-spec headers_before_loop(
    [{non_neg_integer(), binary()}], ets:table(), non_neg_integer(), non_neg_integer()
) -> non_neg_integer().
headers_before_loop([], _Ref, _SIdx, Acc) ->
    Acc;
headers_before_loop([{Idx, _} | _Rest], _Ref, SIdx, Acc) when Idx >= SIdx ->
    Acc;
headers_before_loop([{Idx, _} | Rest], Ref, SIdx, Acc) ->
    case read_section_count(Ref, Idx) > 0 of
        true -> headers_before_loop(Rest, Ref, SIdx, Acc + 1);
        false -> headers_before_loop(Rest, Ref, SIdx, Acc)
    end.

-spec collect_range(
    guild_member_list_oset:oset(),
    ets:table(),
    [{non_neg_integer(), binary()}],
    non_neg_integer(),
    non_neg_integer(),
    non_neg_integer(),
    non_neg_integer(),
    [item()]
) -> [item()].
collect_range(_OSet, _Ref, [], _Start, _End, _Pos, _Base, Acc) ->
    Acc;
collect_range(_OSet, _Ref, _, _Start, End, Pos, _Base, Acc) when Pos > End ->
    Acc;
collect_range(OSet, Ref, [{SIdx, DisplayId} | Rest], Start, End, Pos, Base, Acc) ->
    case read_section_count(Ref, SIdx) of
        0 ->
            collect_range(OSet, Ref, Rest, Start, End, Pos, Base, Acc);
        Count ->
            collect_nonempty_section(
                OSet, Ref, {DisplayId, Rest}, {Start, End}, {Pos, Base}, Count, Acc
            )
    end.

-spec collect_nonempty_section(
    guild_member_list_oset:oset(),
    ets:table(),
    {binary(), [{non_neg_integer(), binary()}]},
    {non_neg_integer(), non_neg_integer()},
    {non_neg_integer(), non_neg_integer()},
    non_neg_integer(),
    [item()]
) -> [item()].
collect_nonempty_section(OSet, Ref, {DisplayId, Rest}, {Start, End}, {Pos, Base}, Count, Acc) ->
    SectionLastPos = Pos + Count,
    case SectionLastPos < Start of
        true ->
            collect_next_section(OSet, Ref, Rest, Start, End, Pos, Base, Count, Acc);
        false ->
            Acc1 = maybe_add_group(Pos, Start, End, DisplayId, Count, Acc),
            Acc2 = collect_members(OSet, Base, Pos, Start, End, Count, Acc1),
            collect_next_section(OSet, Ref, Rest, Start, End, Pos, Base, Count, Acc2)
    end.

-spec collect_next_section(
    guild_member_list_oset:oset(),
    ets:table(),
    [{non_neg_integer(), binary()}],
    non_neg_integer(),
    non_neg_integer(),
    non_neg_integer(),
    non_neg_integer(),
    non_neg_integer(),
    [item()]
) -> [item()].
collect_next_section(OSet, Ref, Rest, Start, End, Pos, Base, Count, Acc) ->
    collect_range(OSet, Ref, Rest, Start, End, Pos + 1 + Count, Base + Count, Acc).

-spec maybe_add_group(
    non_neg_integer(),
    non_neg_integer(),
    non_neg_integer(),
    binary(),
    non_neg_integer(),
    [item()]
) -> [item()].
maybe_add_group(Pos, Start, End, DisplayId, Count, Acc) ->
    case Pos >= Start andalso Pos =< End of
        true -> [{group, DisplayId, Count} | Acc];
        false -> Acc
    end.

-spec collect_members(
    guild_member_list_oset:oset(),
    non_neg_integer(),
    non_neg_integer(),
    non_neg_integer(),
    non_neg_integer(),
    non_neg_integer(),
    [item()]
) -> [item()].
collect_members(OSet, Base, Pos, Start, End, Count, Acc) ->
    MemberStart = Pos + 1,
    Skip = max(0, Start - MemberStart),
    LastWanted = min(MemberStart + Count - 1, End),
    Take = max(0, LastWanted - max(Start, MemberStart) + 1),
    case Take > 0 of
        true -> prepend_members(section_members(OSet, Base + Skip, Take), Acc);
        false -> Acc
    end.

-spec section_members(guild_member_list_oset:oset(), non_neg_integer(), non_neg_integer()) ->
    [guild_member_list_oset:key()].
section_members(OSet, Start, Count) ->
    guild_member_list_oset:range(OSet, Start, Count).

-spec prepend_members([guild_member_list_oset:key()], [item()]) -> [item()].
prepend_members(Keys, Acc) ->
    lists:foldl(
        fun
            ({_SIdx, _SK, UId}, A) when is_integer(UId) -> [{member, UId} | A];
            (_, A) -> A
        end,
        Acc,
        Keys
    ).
