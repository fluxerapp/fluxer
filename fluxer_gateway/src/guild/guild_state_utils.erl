%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_state_utils).
-typing([eqwalizer]).

-export([
    replace_item_by_id/3,
    remove_item_by_id/2,
    bulk_update_items/2,
    needs_visibility_check/1
]).

-type event() :: atom().

-export_type([event/0]).

-spec replace_item_by_id([map()], term(), map()) -> [map()].
replace_item_by_id(Items, Id, NewItem) ->
    NormalizedId = snowflake_id:parse_optional(Id),
    case NormalizedId of
        undefined -> Items;
        _ -> do_replace_items(Items, NormalizedId, NewItem)
    end.

-spec do_replace_items([map()], integer(), map()) -> [map()].
do_replace_items(Items, NormalizedId, NewItem) ->
    lists:map(
        fun(Item) -> replace_if_match(Item, NormalizedId, NewItem) end,
        Items
    ).

-spec replace_if_match(map(), integer(), map()) -> map().
replace_if_match(Item, NormalizedId, NewItem) when is_map(Item) ->
    ItemId = snowflake_id:parse_optional(maps:get(<<"id">>, Item, undefined)),
    case ItemId of
        NormalizedId -> NewItem;
        _ -> Item
    end.

-spec remove_item_by_id([map()], term()) -> [map()].
remove_item_by_id(Items, Id) ->
    NormalizedId = snowflake_id:parse_optional(Id),
    case NormalizedId of
        undefined -> Items;
        _ -> do_remove_items(Items, NormalizedId)
    end.

-spec do_remove_items([map()], integer()) -> [map()].
do_remove_items(Items, NormalizedId) ->
    lists:filter(
        fun(Item) -> not is_id_match(Item, NormalizedId) end,
        Items
    ).

-spec is_id_match(map(), integer()) -> boolean().
is_id_match(Item, NormalizedId) when is_map(Item) ->
    ItemId = snowflake_id:parse_optional(maps:get(<<"id">>, Item, undefined)),
    ItemId =:= NormalizedId.

-spec bulk_update_items([map()], [map()]) -> [map()].
bulk_update_items(Items, BulkItems) ->
    BulkMap = build_bulk_map(BulkItems),
    lists:map(
        fun(Item) -> apply_bulk_update(Item, BulkMap) end,
        Items
    ).

-spec build_bulk_map([map()]) -> map().
build_bulk_map(BulkItems) ->
    lists:foldl(fun index_bulk_item/2, #{}, BulkItems).

-spec index_bulk_item(term(), map()) -> map().
index_bulk_item(Item, Acc) when is_map(Item) ->
    case snowflake_id:parse_optional(maps:get(<<"id">>, Item, undefined)) of
        undefined -> Acc;
        ItemId -> Acc#{ItemId => Item}
    end;
index_bulk_item(_, Acc) ->
    Acc.

-spec apply_bulk_update(map(), map()) -> map().
apply_bulk_update(Item, BulkMap) when is_map(Item) ->
    ItemId = snowflake_id:parse_optional(maps:get(<<"id">>, Item, undefined)),
    case maps:get(ItemId, BulkMap, undefined) of
        undefined -> Item;
        UpdatedItem -> UpdatedItem
    end.

-spec needs_visibility_check(event()) -> boolean().
needs_visibility_check(guild_role_create) -> true;
needs_visibility_check(guild_role_update) -> true;
needs_visibility_check(guild_role_update_bulk) -> true;
needs_visibility_check(guild_role_delete) -> true;
needs_visibility_check(guild_member_update) -> true;
needs_visibility_check(channel_update) -> true;
needs_visibility_check(channel_update_bulk) -> true;
needs_visibility_check(_) -> false.
