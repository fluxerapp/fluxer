%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_member_list_subs).
-typing([eqwalizer]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
    new/0,
    destroy/1,
    subscribe/4,
    retain_only_session_list/3,
    unsubscribe_session/2,
    get_list_subs/2,
    has_subs/2,
    get_session_ranges/3,
    list_ids/1,
    fold_list_subs/4,
    fold_lists/3,
    fold_list_ids/3,
    foreach_list/2,
    is_subscribed/3
]).

-type range() :: {non_neg_integer(), non_neg_integer()}.
-type list_subs() :: #{binary() => [range()]}.

-export_type([range/0, list_subs/0]).

-spec new() -> ets:table().
new() ->
    Tab = ets:new(member_list_subs, [ordered_set, public]),
    IdxTab = ets:new(member_list_subs_idx, [set, public]),
    ets:insert(Tab, {idx_tab, IdxTab}),
    Tab.

-spec destroy(term()) -> ok.
destroy(Tab) ->
    case safe_ets_lookup(eqwalizer:dynamic_cast(Tab), idx_tab) of
        [{idx_tab, IdxTab}] -> safe_ets_delete(IdxTab);
        _ -> ok
    end,
    safe_ets_delete(eqwalizer:dynamic_cast(Tab)),
    ok.

-spec safe_ets_lookup(ets:table(), term()) -> list().
safe_ets_lookup(Tab, Key) ->
    try
        ets:lookup(Tab, Key)
    catch
        _:_ -> []
    end.

-spec safe_ets_delete(ets:table()) -> ok | true.
safe_ets_delete(Tab) ->
    try
        ets:delete(Tab)
    catch
        _:_ -> ok
    end.

-spec subscribe(binary(), binary(), [range()], ets:table()) -> {[range()], boolean()}.
subscribe(SessionId, ListId, NormalizedRanges, Tab) ->
    [{idx_tab, IdxTab}] = ets:lookup(Tab, idx_tab),
    Key = {ListId, SessionId},
    OldRanges = lookup_list(Tab, Key),
    case NormalizedRanges of
        [] ->
            ets:delete(Tab, Key),
            remove_from_session_index(IdxTab, SessionId, ListId);
        _ ->
            ets:insert(Tab, {Key, NormalizedRanges}),
            add_to_session_index(IdxTab, SessionId, ListId)
    end,
    ShouldSync = NormalizedRanges =/= [] andalso NormalizedRanges =/= OldRanges,
    {OldRanges, ShouldSync}.

-spec retain_only_session_list(binary(), binary(), ets:table()) -> [binary()].
retain_only_session_list(SessionId, KeepListId, Tab) ->
    [{idx_tab, IdxTab}] = ets:lookup(Tab, idx_tab),
    case lookup_list(IdxTab, SessionId) of
        [] ->
            [];
        [KeepListId] ->
            [];
        ListIds ->
            Removed = [LId || LId <- ListIds, LId =/= KeepListId],
            delete_session_lists(Tab, SessionId, Removed),
            ets:insert(IdxTab, {SessionId, [KeepListId]}),
            Removed
    end.

-spec delete_session_lists(ets:table(), binary(), [binary()]) -> ok.
delete_session_lists(Tab, SessionId, Removed) ->
    lists:foreach(
        fun(LId) -> ets:delete(Tab, {LId, SessionId}) end,
        Removed
    ).

-spec unsubscribe_session(binary(), ets:table()) -> [binary()].
unsubscribe_session(SessionId, Tab) ->
    [{idx_tab, IdxTab}] = ets:lookup(Tab, idx_tab),
    ListIds = lookup_list(IdxTab, SessionId),
    lists:foreach(
        fun(LId) ->
            ets:delete(Tab, {LId, SessionId})
        end,
        ListIds
    ),
    ets:delete(IdxTab, SessionId),
    ListIds.

-spec lookup_list(ets:table(), term()) -> list().
lookup_list(Tab, Key) ->
    case ets:lookup(Tab, Key) of
        [{_, Values}] when is_list(Values) -> Values;
        [] -> []
    end.

-spec get_list_subs(binary(), ets:table()) -> list_subs().
get_list_subs(ListId, Tab) ->
    collect_list(Tab, ets:next(Tab, {ListId, <<>>}), ListId, #{}).

-spec has_subs(binary(), ets:table()) -> boolean().
has_subs(ListId, Tab) ->
    case ets:next(Tab, {ListId, <<>>}) of
        {ListId, SessionId} when is_binary(SessionId) -> true;
        _ -> false
    end.

-spec get_session_ranges(binary(), binary(), ets:table()) -> [range()].
get_session_ranges(SessionId, ListId, Tab) ->
    case ets:lookup(Tab, {ListId, SessionId}) of
        [{_, Ranges}] -> Ranges;
        [] -> []
    end.

-spec list_ids(ets:table()) -> [binary()].
list_ids(Tab) ->
    list_ids_loop(Tab, ets:next(Tab, {<<>>, <<>>}), []).

-spec fold_list_subs(binary(), ets:table(), fun((binary(), [range()], Acc) -> Acc), Acc) -> Acc.
fold_list_subs(ListId, Tab, Fun, Acc0) ->
    fold_list_subs_loop(Tab, ets:next(Tab, {ListId, <<>>}), ListId, Fun, Acc0).

-spec fold_lists(fun((binary(), list_subs(), Acc) -> Acc), Acc, ets:table()) -> Acc.
fold_lists(Fun, Acc0, Tab) ->
    fold_lists_loop(Fun, Acc0, Tab, ets:next(Tab, {<<>>, <<>>})).

-spec fold_list_ids(fun((binary(), Acc) -> Acc), Acc, ets:table()) -> Acc.
fold_list_ids(Fun, Acc0, Tab) ->
    fold_list_ids_loop(Fun, Acc0, Tab, ets:next(Tab, {<<>>, <<>>})).

-spec foreach_list(fun((binary(), list_subs()) -> term()), ets:table()) -> ok.
foreach_list(Fun, Tab) ->
    _ = fold_lists(
        fun(ListId, ListSubs, Acc) ->
            _ = Fun(ListId, ListSubs),
            Acc
        end,
        #{},
        Tab
    ),
    ok.

-spec is_subscribed(binary(), binary(), ets:table()) -> boolean().
is_subscribed(ListId, SessionId, Tab) ->
    ets:member(Tab, {ListId, SessionId}).

-spec collect_list(ets:table(), term(), binary(), list_subs()) -> list_subs().
collect_list(_Tab, '$end_of_table', _ListId, Acc) ->
    Acc;
collect_list(_Tab, {Other, _}, ListId, Acc) when Other =/= ListId -> Acc;
collect_list(Tab, {ListId, SId} = Key, ListId, Acc) when is_binary(SId) ->
    [{_, Ranges}] = ets:lookup(Tab, Key),
    collect_list(Tab, ets:next(Tab, Key), ListId, Acc#{SId => Ranges}).

-spec list_ids_loop(ets:table(), term(), [binary()]) -> [binary()].
list_ids_loop(_Tab, '$end_of_table', Acc) ->
    lists:reverse(Acc);
list_ids_loop(_Tab, {idx_tab, _}, Acc) ->
    lists:reverse(Acc);
list_ids_loop(Tab, {ListId, _} = Key, Acc) when is_binary(ListId) ->
    NextListKey = skip_past_list(Tab, Key, ListId),
    list_ids_loop(Tab, NextListKey, [ListId | Acc]);
list_ids_loop(Tab, Key, Acc) ->
    list_ids_loop(Tab, ets:next(Tab, Key), Acc).

-spec fold_list_subs_loop(
    ets:table(), term(), binary(), fun((binary(), [range()], Acc) -> Acc), Acc
) -> Acc.
fold_list_subs_loop(_Tab, '$end_of_table', _ListId, _Fun, Acc) ->
    Acc;
fold_list_subs_loop(_Tab, {Other, _}, ListId, _Fun, Acc) when Other =/= ListId ->
    Acc;
fold_list_subs_loop(Tab, {ListId, SId} = Key, ListId, Fun, Acc) when is_binary(SId) ->
    [{_, Ranges}] = ets:lookup(Tab, Key),
    fold_list_subs_loop(Tab, ets:next(Tab, Key), ListId, Fun, Fun(SId, Ranges, Acc)).

-spec fold_lists_loop(
    fun((binary(), list_subs(), Acc) -> Acc), Acc, ets:table(), term()
) -> Acc.
fold_lists_loop(_Fun, Acc, _Tab, '$end_of_table') ->
    Acc;
fold_lists_loop(_Fun, Acc, _Tab, {idx_tab, _}) ->
    Acc;
fold_lists_loop(Fun, Acc, Tab, {ListId, _} = Key) when is_binary(ListId) ->
    ListSubs = collect_list(Tab, Key, ListId, #{}),
    Acc2 = Fun(ListId, ListSubs, Acc),
    NextListKey = skip_past_list(Tab, Key, ListId),
    fold_lists_loop(Fun, Acc2, Tab, NextListKey).

-spec fold_list_ids_loop(fun((binary(), Acc) -> Acc), Acc, ets:table(), term()) -> Acc.
fold_list_ids_loop(_Fun, Acc, _Tab, '$end_of_table') ->
    Acc;
fold_list_ids_loop(_Fun, Acc, _Tab, {idx_tab, _}) ->
    Acc;
fold_list_ids_loop(Fun, Acc, Tab, {ListId, _} = Key) when is_binary(ListId) ->
    Acc2 = Fun(ListId, Acc),
    NextListKey = skip_past_list(Tab, Key, ListId),
    fold_list_ids_loop(Fun, Acc2, Tab, NextListKey).

-spec skip_past_list(ets:table(), term(), binary()) -> term().
skip_past_list(Tab, Key, ListId) ->
    case ets:next(Tab, Key) of
        '$end_of_table' -> '$end_of_table';
        {ListId, _} = K -> skip_past_list(Tab, K, ListId);
        Other -> Other
    end.

-spec add_to_session_index(ets:table(), binary(), binary()) -> ok | true.
add_to_session_index(IdxTab, SessionId, ListId) ->
    case ets:lookup(IdxTab, SessionId) of
        [{_, Ids}] ->
            update_session_index(IdxTab, SessionId, ListId, Ids);
        [] ->
            ets:insert(IdxTab, {SessionId, [ListId]})
    end.

-spec update_session_index(ets:table(), binary(), binary(), [binary()]) -> ok | true.
update_session_index(IdxTab, SessionId, ListId, Ids) ->
    case lists:member(ListId, Ids) of
        true -> ok;
        false -> ets:insert(IdxTab, {SessionId, [ListId | Ids]})
    end.

-spec remove_from_session_index(ets:table(), binary(), binary()) -> ok | true.
remove_from_session_index(IdxTab, SessionId, ListId) ->
    case ets:lookup(IdxTab, SessionId) of
        [{_, Ids}] ->
            remove_list_from_session_index(IdxTab, SessionId, ListId, Ids);
        [] ->
            ok
    end.

-spec remove_list_from_session_index(ets:table(), binary(), binary(), [binary()]) -> ok | true.
remove_list_from_session_index(IdxTab, SessionId, ListId, Ids) ->
    NewIds = lists:delete(ListId, Ids),
    case NewIds of
        [] -> ets:delete(IdxTab, SessionId);
        _ -> ets:insert(IdxTab, {SessionId, NewIds})
    end.

-ifdef(TEST).

retain_only_session_list_drops_other_lists_test() ->
    Tab = new(),
    subscribe(<<"s1">>, <<"100">>, [{0, 99}], Tab),
    subscribe(<<"s1">>, <<"200">>, [{0, 99}], Tab),
    subscribe(<<"s1">>, <<"300">>, [{0, 99}], Tab),
    Removed = retain_only_session_list(<<"s1">>, <<"300">>, Tab),
    ?assertEqual([<<"100">>, <<"200">>], lists:sort(Removed)),
    ?assert(is_subscribed(<<"300">>, <<"s1">>, Tab)),
    ?assertNot(is_subscribed(<<"100">>, <<"s1">>, Tab)),
    ?assertNot(is_subscribed(<<"200">>, <<"s1">>, Tab)),
    destroy(Tab).

retain_only_session_list_noop_when_single_list_test() ->
    Tab = new(),
    subscribe(<<"s1">>, <<"100">>, [{0, 99}], Tab),
    ?assertEqual([], retain_only_session_list(<<"s1">>, <<"100">>, Tab)),
    ?assert(is_subscribed(<<"100">>, <<"s1">>, Tab)),
    destroy(Tab).

retain_only_session_list_leaves_other_sessions_untouched_test() ->
    Tab = new(),
    subscribe(<<"s1">>, <<"100">>, [{0, 99}], Tab),
    subscribe(<<"s2">>, <<"100">>, [{0, 99}], Tab),
    subscribe(<<"s2">>, <<"200">>, [{0, 99}], Tab),
    retain_only_session_list(<<"s1">>, <<"100">>, Tab),
    ?assert(is_subscribed(<<"100">>, <<"s2">>, Tab)),
    ?assert(is_subscribed(<<"200">>, <<"s2">>, Tab)),
    destroy(Tab).

-endif.
