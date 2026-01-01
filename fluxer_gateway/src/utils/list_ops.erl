%% Copyright (C) 2026 Fluxer Contributors
%%
%% This file is part of Fluxer.
%%
%% Fluxer is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Affero General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% Fluxer is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%% GNU Affero General Public License for more details.
%%
%% You should have received a copy of the GNU Affero General Public License
%% along with Fluxer. If not, see <https://www.gnu.org/licenses/>.

-module(list_ops).

-export([
    replace_by_id/3,
    remove_by_id/2,
    replace_by_user_id/3,
    remove_by_user_id/2,
    bulk_update/2,
    extract_user_id/1
]).

-type item() :: map() | term().
-type id() :: binary() | integer().
-type item_list() :: [item()].

-spec replace_by_id(item_list(), id(), item()) -> item_list().
replace_by_id(Items, Id, NewItem) when is_list(Items) ->
    lists:map(
        fun
            (Item) when is_map(Item) ->
                case maps:get(<<"id">>, Item, undefined) of
                    Id -> NewItem;
                    _ -> Item
                end;
            (Item) ->
                Item
        end,
        Items
    );
replace_by_id(_, _, _) ->
    [].

-spec remove_by_id(item_list(), id()) -> item_list().
remove_by_id(Items, Id) when is_list(Items) ->
    lists:filter(
        fun
            (Item) when is_map(Item) ->
                maps:get(<<"id">>, Item, undefined) =/= Id;
            (_Item) ->
                true
        end,
        Items
    );
remove_by_id(_, _) ->
    [].

-spec replace_by_user_id(item_list(), integer(), item()) -> item_list().
replace_by_user_id(Items, UserId, NewItem) when is_list(Items), is_integer(UserId) ->
    lists:map(
        fun
            (Item) when is_map(Item) ->
                ItemUserId = extract_user_id(Item),
                case ItemUserId =:= UserId of
                    true -> NewItem;
                    false -> Item
                end;
            (Item) ->
                Item
        end,
        Items
    );
replace_by_user_id(_, _, _) ->
    [].

-spec remove_by_user_id(item_list(), integer()) -> item_list().
remove_by_user_id(Items, UserId) when is_list(Items), is_integer(UserId) ->
    lists:filter(
        fun
            (Item) when is_map(Item) ->
                ItemUserId = extract_user_id(Item),
                ItemUserId =/= UserId;
            (_Item) ->
                true
        end,
        Items
    );
remove_by_user_id(_, _) ->
    [].

-spec bulk_update(item_list(), item_list()) -> item_list().
bulk_update(Items, Updates) when is_list(Items), is_list(Updates) ->
    UpdateMap = lists:foldl(
        fun
            (Item, Acc) when is_map(Item) ->
                case maps:get(<<"id">>, Item, undefined) of
                    undefined -> Acc;
                    ItemId -> maps:put(ItemId, Item, Acc)
                end;
            (_, Acc) ->
                Acc
        end,
        #{},
        Updates
    ),

    lists:map(
        fun
            (Item) when is_map(Item) ->
                ItemId = maps:get(<<"id">>, Item, undefined),
                case maps:get(ItemId, UpdateMap, undefined) of
                    undefined -> Item;
                    UpdatedItem -> UpdatedItem
                end;
            (Item) ->
                Item
        end,
        Items
    );
bulk_update(Items, _) when is_list(Items) ->
    Items;
bulk_update(_, _) ->
    [].

-spec extract_user_id(map() | term()) -> integer().
extract_user_id(Item) ->
    UserMap = map_utils:ensure_map(map_utils:get_safe(Item, <<"user">>, #{})),
    case maps:find(<<"id">>, UserMap) of
        error ->
            0;
        {ok, RawId} ->
            case type_conv:to_integer(RawId) of
                undefined -> undefined;
                Value -> Value
            end
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

make_item_with_id(Id) ->
    #{<<"id">> => Id, <<"data">> => <<"test">>}.

make_item_with_user_id(UserId) ->
    #{
        <<"user">> => #{<<"id">> => integer_to_binary(UserId)},
        <<"data">> => <<"member">>
    }.

replace_by_id_success_test() ->
    Items = [
        make_item_with_id(<<"1">>),
        make_item_with_id(<<"2">>),
        make_item_with_id(<<"3">>)
    ],
    NewItem = #{<<"id">> => <<"2">>, <<"data">> => <<"updated">>},
    Result = replace_by_id(Items, <<"2">>, NewItem),

    ?assertEqual(3, length(Result)),
    ?assertEqual(make_item_with_id(<<"1">>), lists:nth(1, Result)),
    ?assertEqual(NewItem, lists:nth(2, Result)),
    ?assertEqual(make_item_with_id(<<"3">>), lists:nth(3, Result)).

replace_by_id_no_match_test() ->
    Items = [
        make_item_with_id(<<"1">>),
        make_item_with_id(<<"2">>)
    ],
    NewItem = #{<<"id">> => <<"99">>, <<"data">> => <<"new">>},
    Result = replace_by_id(Items, <<"99">>, NewItem),

    ?assertEqual(Items, Result).

replace_by_id_empty_list_test() ->
    Result = replace_by_id([], <<"1">>, #{<<"id">> => <<"1">>}),
    ?assertEqual([], Result).

replace_by_id_mixed_list_test() ->
    Items = [
        make_item_with_id(<<"1">>),
        <<"non_map_item">>,
        make_item_with_id(<<"2">>),
        {tuple, item},
        make_item_with_id(<<"3">>)
    ],
    NewItem = #{<<"id">> => <<"2">>, <<"data">> => <<"replaced">>},
    Result = replace_by_id(Items, <<"2">>, NewItem),

    ?assertEqual(5, length(Result)),
    ?assertEqual(make_item_with_id(<<"1">>), lists:nth(1, Result)),
    ?assertEqual(<<"non_map_item">>, lists:nth(2, Result)),
    ?assertEqual(NewItem, lists:nth(3, Result)),
    ?assertEqual({tuple, item}, lists:nth(4, Result)),
    ?assertEqual(make_item_with_id(<<"3">>), lists:nth(5, Result)).

replace_by_id_integer_id_test() ->
    Items = [
        #{<<"id">> => 1, <<"data">> => <<"a">>},
        #{<<"id">> => 2, <<"data">> => <<"b">>}
    ],
    NewItem = #{<<"id">> => 2, <<"data">> => <<"updated">>},
    Result = replace_by_id(Items, 2, NewItem),

    ?assertEqual(2, length(Result)),
    ?assertEqual(#{<<"id">> => 1, <<"data">> => <<"a">>}, lists:nth(1, Result)),
    ?assertEqual(NewItem, lists:nth(2, Result)).

replace_by_id_invalid_input_test() ->
    ?assertEqual([], replace_by_id(not_a_list, <<"1">>, #{})),
    ?assertEqual([], replace_by_id(#{}, <<"1">>, #{})),
    ?assertEqual([], replace_by_id(undefined, <<"1">>, #{})).

replace_by_id_item_without_id_test() ->
    Items = [
        #{<<"id">> => <<"1">>},
        #{<<"name">> => <<"no_id">>},
        #{<<"id">> => <<"2">>}
    ],
    NewItem = #{<<"id">> => <<"2">>, <<"updated">> => true},
    Result = replace_by_id(Items, <<"2">>, NewItem),

    ?assertEqual(3, length(Result)),
    ?assertEqual(#{<<"id">> => <<"1">>}, lists:nth(1, Result)),
    ?assertEqual(#{<<"name">> => <<"no_id">>}, lists:nth(2, Result)),
    ?assertEqual(NewItem, lists:nth(3, Result)).

remove_by_id_success_test() ->
    Items = [
        make_item_with_id(<<"1">>),
        make_item_with_id(<<"2">>),
        make_item_with_id(<<"3">>)
    ],
    Result = remove_by_id(Items, <<"2">>),

    ?assertEqual(2, length(Result)),
    ?assertEqual(make_item_with_id(<<"1">>), lists:nth(1, Result)),
    ?assertEqual(make_item_with_id(<<"3">>), lists:nth(2, Result)).

remove_by_id_no_match_test() ->
    Items = [
        make_item_with_id(<<"1">>),
        make_item_with_id(<<"2">>)
    ],
    Result = remove_by_id(Items, <<"99">>),

    ?assertEqual(Items, Result).

remove_by_id_multiple_matches_test() ->
    Items = [
        make_item_with_id(<<"1">>),
        #{<<"id">> => <<"2">>, <<"version">> => 1},
        #{<<"id">> => <<"2">>, <<"version">> => 2},
        make_item_with_id(<<"3">>)
    ],
    Result = remove_by_id(Items, <<"2">>),

    ?assertEqual(2, length(Result)),
    ?assertEqual(make_item_with_id(<<"1">>), lists:nth(1, Result)),
    ?assertEqual(make_item_with_id(<<"3">>), lists:nth(2, Result)).

remove_by_id_empty_list_test() ->
    Result = remove_by_id([], <<"1">>),
    ?assertEqual([], Result).

remove_by_id_mixed_list_test() ->
    Items = [
        make_item_with_id(<<"1">>),
        <<"non_map">>,
        make_item_with_id(<<"2">>),
        [list, item],
        make_item_with_id(<<"3">>)
    ],
    Result = remove_by_id(Items, <<"2">>),

    ?assertEqual(4, length(Result)),
    ?assertEqual(make_item_with_id(<<"1">>), lists:nth(1, Result)),
    ?assertEqual(<<"non_map">>, lists:nth(2, Result)),
    ?assertEqual([list, item], lists:nth(3, Result)),
    ?assertEqual(make_item_with_id(<<"3">>), lists:nth(4, Result)).

remove_by_id_invalid_input_test() ->
    ?assertEqual([], remove_by_id(not_a_list, <<"1">>)),
    ?assertEqual([], remove_by_id(undefined, <<"1">>)),
    ?assertEqual([], remove_by_id(123, <<"1">>)).

remove_by_id_all_items_match_test() ->
    Items = [
        make_item_with_id(<<"1">>),
        make_item_with_id(<<"1">>),
        make_item_with_id(<<"1">>)
    ],
    Result = remove_by_id(Items, <<"1">>),
    ?assertEqual([], Result).

replace_by_user_id_success_test() ->
    Items = [
        make_item_with_user_id(100),
        make_item_with_user_id(200),
        make_item_with_user_id(300)
    ],
    NewItem = #{
        <<"user">> => #{<<"id">> => <<"200">>},
        <<"data">> => <<"updated">>
    },
    Result = replace_by_user_id(Items, 200, NewItem),

    ?assertEqual(3, length(Result)),
    ?assertEqual(make_item_with_user_id(100), lists:nth(1, Result)),
    ?assertEqual(NewItem, lists:nth(2, Result)),
    ?assertEqual(make_item_with_user_id(300), lists:nth(3, Result)).

replace_by_user_id_no_match_test() ->
    Items = [
        make_item_with_user_id(100),
        make_item_with_user_id(200)
    ],
    NewItem = make_item_with_user_id(999),
    Result = replace_by_user_id(Items, 999, NewItem),

    ?assertEqual(Items, Result).

replace_by_user_id_empty_list_test() ->
    Result = replace_by_user_id([], 100, make_item_with_user_id(100)),
    ?assertEqual([], Result).

replace_by_user_id_nested_extraction_test() ->
    Items = [
        #{
            <<"user">> => #{<<"id">> => <<"123">>, <<"name">> => <<"alice">>},
            <<"role">> => <<"admin">>
        },
        #{<<"user">> => #{<<"id">> => <<"456">>, <<"name">> => <<"bob">>}, <<"role">> => <<"user">>}
    ],
    NewItem = #{<<"user">> => #{<<"id">> => <<"456">>}, <<"role">> => <<"moderator">>},
    Result = replace_by_user_id(Items, 456, NewItem),

    ?assertEqual(2, length(Result)),
    ?assertEqual(lists:nth(1, Items), lists:nth(1, Result)),
    ?assertEqual(NewItem, lists:nth(2, Result)).

replace_by_user_id_mixed_list_test() ->
    Items = [
        make_item_with_user_id(100),
        <<"string_item">>,
        make_item_with_user_id(200),
        {tuple},
        #{<<"other">> => <<"map">>}
    ],
    NewItem = make_item_with_user_id(200),
    Result = replace_by_user_id(Items, 200, NewItem),

    ?assertEqual(5, length(Result)),
    ?assertEqual(make_item_with_user_id(100), lists:nth(1, Result)),
    ?assertEqual(<<"string_item">>, lists:nth(2, Result)),
    ?assertEqual(NewItem, lists:nth(3, Result)),
    ?assertEqual({tuple}, lists:nth(4, Result)),
    ?assertEqual(#{<<"other">> => <<"map">>}, lists:nth(5, Result)).

replace_by_user_id_invalid_structure_test() ->
    Items = [
        #{<<"user">> => <<"not_a_map">>, <<"data">> => <<"x">>},
        #{<<"no_user_key">> => <<"y">>},
        make_item_with_user_id(100)
    ],
    NewItem = make_item_with_user_id(100),
    Result = replace_by_user_id(Items, 100, NewItem),

    ?assertEqual(3, length(Result)),
    ?assertEqual(lists:nth(1, Items), lists:nth(1, Result)),
    ?assertEqual(lists:nth(2, Items), lists:nth(2, Result)),
    ?assertEqual(NewItem, lists:nth(3, Result)).

replace_by_user_id_invalid_input_test() ->
    ?assertEqual([], replace_by_user_id(not_a_list, 100, #{})),
    ?assertEqual([], replace_by_user_id(undefined, 100, #{})),
    ?assertEqual([], replace_by_user_id([make_item_with_user_id(100)], <<"not_integer">>, #{})).

remove_by_user_id_success_test() ->
    Items = [
        make_item_with_user_id(100),
        make_item_with_user_id(200),
        make_item_with_user_id(300)
    ],
    Result = remove_by_user_id(Items, 200),

    ?assertEqual(2, length(Result)),
    ?assertEqual(make_item_with_user_id(100), lists:nth(1, Result)),
    ?assertEqual(make_item_with_user_id(300), lists:nth(2, Result)).

remove_by_user_id_no_match_test() ->
    Items = [
        make_item_with_user_id(100),
        make_item_with_user_id(200)
    ],
    Result = remove_by_user_id(Items, 999),

    ?assertEqual(Items, Result).

remove_by_user_id_multiple_matches_test() ->
    Items = [
        make_item_with_user_id(100),
        #{<<"user">> => #{<<"id">> => <<"200">>}, <<"version">> => 1},
        #{<<"user">> => #{<<"id">> => <<"200">>}, <<"version">> => 2},
        make_item_with_user_id(300)
    ],
    Result = remove_by_user_id(Items, 200),

    ?assertEqual(2, length(Result)),
    ?assertEqual(make_item_with_user_id(100), lists:nth(1, Result)),
    ?assertEqual(make_item_with_user_id(300), lists:nth(2, Result)).

remove_by_user_id_empty_list_test() ->
    Result = remove_by_user_id([], 100),
    ?assertEqual([], Result).

remove_by_user_id_mixed_list_test() ->
    Items = [
        make_item_with_user_id(100),
        <<"non_map">>,
        make_item_with_user_id(200),
        [list],
        #{<<"invalid">> => <<"structure">>}
    ],
    Result = remove_by_user_id(Items, 200),

    ?assertEqual(4, length(Result)),
    ?assertEqual(make_item_with_user_id(100), lists:nth(1, Result)),
    ?assertEqual(<<"non_map">>, lists:nth(2, Result)),
    ?assertEqual([list], lists:nth(3, Result)),
    ?assertEqual(#{<<"invalid">> => <<"structure">>}, lists:nth(4, Result)).

remove_by_user_id_invalid_nested_structure_test() ->
    Items = [
        #{<<"user">> => <<"not_a_map">>},
        #{<<"no_user">> => <<"field">>},
        #{<<"user">> => #{<<"no_id">> => <<"field">>}},
        make_item_with_user_id(100)
    ],
    Result = remove_by_user_id(Items, 0),

    ?assertEqual(1, length(Result)),
    ?assertEqual(make_item_with_user_id(100), lists:nth(1, Result)).

remove_by_user_id_invalid_input_test() ->
    ?assertEqual([], remove_by_user_id(not_a_list, 100)),
    ?assertEqual([], remove_by_user_id(undefined, 100)),
    ?assertEqual([], remove_by_user_id([make_item_with_user_id(100)], <<"not_integer">>)).

bulk_update_multiple_updates_test() ->
    Items = [
        make_item_with_id(<<"1">>),
        make_item_with_id(<<"2">>),
        make_item_with_id(<<"3">>),
        make_item_with_id(<<"4">>)
    ],
    Updates = [
        #{<<"id">> => <<"2">>, <<"data">> => <<"updated_2">>},
        #{<<"id">> => <<"4">>, <<"data">> => <<"updated_4">>}
    ],
    Result = bulk_update(Items, Updates),

    ?assertEqual(4, length(Result)),
    ?assertEqual(make_item_with_id(<<"1">>), lists:nth(1, Result)),
    ?assertEqual(#{<<"id">> => <<"2">>, <<"data">> => <<"updated_2">>}, lists:nth(2, Result)),
    ?assertEqual(make_item_with_id(<<"3">>), lists:nth(3, Result)),
    ?assertEqual(#{<<"id">> => <<"4">>, <<"data">> => <<"updated_4">>}, lists:nth(4, Result)).

bulk_update_partial_updates_test() ->
    Items = [
        make_item_with_id(<<"1">>),
        make_item_with_id(<<"2">>),
        make_item_with_id(<<"3">>)
    ],
    Updates = [
        #{<<"id">> => <<"2">>, <<"data">> => <<"updated">>}
    ],
    Result = bulk_update(Items, Updates),

    ?assertEqual(3, length(Result)),
    ?assertEqual(make_item_with_id(<<"1">>), lists:nth(1, Result)),
    ?assertEqual(#{<<"id">> => <<"2">>, <<"data">> => <<"updated">>}, lists:nth(2, Result)),
    ?assertEqual(make_item_with_id(<<"3">>), lists:nth(3, Result)).

bulk_update_no_matches_test() ->
    Items = [
        make_item_with_id(<<"1">>),
        make_item_with_id(<<"2">>)
    ],
    Updates = [
        #{<<"id">> => <<"99">>, <<"data">> => <<"new">>},
        #{<<"id">> => <<"98">>, <<"data">> => <<"new2">>}
    ],
    Result = bulk_update(Items, Updates),

    ?assertEqual(Items, Result).

bulk_update_empty_lists_test() ->
    ?assertEqual([], bulk_update([], [])),
    ?assertEqual([], bulk_update([], [make_item_with_id(<<"1">>)])),

    Items = [make_item_with_id(<<"1">>)],
    ?assertEqual(Items, bulk_update(Items, [])).

bulk_update_updates_without_id_test() ->
    Items = [
        make_item_with_id(<<"1">>),
        make_item_with_id(<<"2">>)
    ],
    Updates = [
        #{<<"name">> => <<"no_id">>},
        #{<<"id">> => <<"2">>, <<"data">> => <<"updated">>}
    ],
    Result = bulk_update(Items, Updates),

    ?assertEqual(2, length(Result)),
    ?assertEqual(make_item_with_id(<<"1">>), lists:nth(1, Result)),
    ?assertEqual(#{<<"id">> => <<"2">>, <<"data">> => <<"updated">>}, lists:nth(2, Result)).

bulk_update_mixed_items_list_test() ->
    Items = [
        make_item_with_id(<<"1">>),
        <<"non_map_item">>,
        make_item_with_id(<<"2">>),
        {tuple, item}
    ],
    Updates = [
        #{<<"id">> => <<"2">>, <<"data">> => <<"updated">>}
    ],
    Result = bulk_update(Items, Updates),

    ?assertEqual(4, length(Result)),
    ?assertEqual(make_item_with_id(<<"1">>), lists:nth(1, Result)),
    ?assertEqual(<<"non_map_item">>, lists:nth(2, Result)),
    ?assertEqual(#{<<"id">> => <<"2">>, <<"data">> => <<"updated">>}, lists:nth(3, Result)),
    ?assertEqual({tuple, item}, lists:nth(4, Result)).

bulk_update_mixed_updates_list_test() ->
    Items = [
        make_item_with_id(<<"1">>),
        make_item_with_id(<<"2">>)
    ],
    Updates = [
        <<"non_map">>,
        #{<<"id">> => <<"1">>, <<"data">> => <<"updated">>},
        {tuple},
        #{<<"no_id">> => <<"field">>}
    ],
    Result = bulk_update(Items, Updates),

    ?assertEqual(2, length(Result)),
    ?assertEqual(#{<<"id">> => <<"1">>, <<"data">> => <<"updated">>}, lists:nth(1, Result)),
    ?assertEqual(make_item_with_id(<<"2">>), lists:nth(2, Result)).

bulk_update_duplicate_ids_in_updates_test() ->
    Items = [
        make_item_with_id(<<"1">>),
        make_item_with_id(<<"2">>)
    ],
    Updates = [
        #{<<"id">> => <<"1">>, <<"data">> => <<"first_update">>},
        #{<<"id">> => <<"1">>, <<"data">> => <<"second_update">>}
    ],
    Result = bulk_update(Items, Updates),

    ?assertEqual(2, length(Result)),
    ?assertEqual(#{<<"id">> => <<"1">>, <<"data">> => <<"second_update">>}, lists:nth(1, Result)),
    ?assertEqual(make_item_with_id(<<"2">>), lists:nth(2, Result)).

bulk_update_invalid_input_test() ->
    Items = [make_item_with_id(<<"1">>)],

    ?assertEqual(Items, bulk_update(Items, not_a_list)),
    ?assertEqual(Items, bulk_update(Items, undefined)),
    ?assertEqual(Items, bulk_update(Items, #{})),

    ?assertEqual([], bulk_update(not_a_list, [make_item_with_id(<<"1">>)])),
    ?assertEqual([], bulk_update(undefined, [])).

bulk_update_item_without_id_preserved_test() ->
    Items = [
        make_item_with_id(<<"1">>),
        #{<<"name">> => <<"no_id_item">>},
        make_item_with_id(<<"2">>)
    ],
    Updates = [
        #{<<"id">> => <<"1">>, <<"data">> => <<"updated">>}
    ],
    Result = bulk_update(Items, Updates),

    ?assertEqual(3, length(Result)),
    ?assertEqual(#{<<"id">> => <<"1">>, <<"data">> => <<"updated">>}, lists:nth(1, Result)),
    ?assertEqual(#{<<"name">> => <<"no_id_item">>}, lists:nth(2, Result)),
    ?assertEqual(make_item_with_id(<<"2">>), lists:nth(3, Result)).

extract_user_id_valid_structure_test() ->
    Item = #{<<"user">> => #{<<"id">> => <<"12345">>}},
    ?assertEqual(12345, extract_user_id(Item)).

extract_user_id_missing_user_test() ->
    Item = #{<<"other">> => <<"field">>},
    ?assertEqual(0, extract_user_id(Item)).

extract_user_id_missing_id_test() ->
    Item = #{<<"user">> => #{<<"name">> => <<"alice">>}},
    ?assertEqual(0, extract_user_id(Item)).

extract_user_id_non_map_test() ->
    ?assertEqual(0, extract_user_id(<<"string">>)),
    ?assertEqual(0, extract_user_id([list])),
    ?assertEqual(0, extract_user_id({tuple})),
    ?assertEqual(0, extract_user_id(undefined)),
    ?assertEqual(0, extract_user_id(123)).

extract_user_id_user_not_map_test() ->
    Item = #{<<"user">> => <<"not_a_map">>},
    ?assertEqual(0, extract_user_id(Item)).

extract_user_id_nested_structure_test() ->
    Item = #{
        <<"user">> => #{
            <<"id">> => <<"999">>,
            <<"name">> => <<"bob">>,
            <<"extra">> => #{<<"nested">> => <<"data">>}
        },
        <<"role">> => <<"admin">>
    },
    ?assertEqual(999, extract_user_id(Item)).

extract_user_id_empty_id_test() ->
    Item = #{<<"user">> => #{<<"id">> => <<>>}},
    ?assertEqual(undefined, extract_user_id(Item)).

extract_user_id_empty_user_map_test() ->
    Item = #{<<"user">> => #{}},
    ?assertEqual(0, extract_user_id(Item)).

extract_user_id_invalid_id_format_test() ->
    Item = #{<<"user">> => #{<<"id">> => <<"not_a_number">>}},
    ?assertEqual(undefined, extract_user_id(Item)).

-endif.
