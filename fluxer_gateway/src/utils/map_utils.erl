%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(map_utils).
-typing([eqwalizer]).

-export([
    get_safe/3,
    ensure_map/1,
    ensure_list/1,
    get_integer/3,
    maps_share_any_key/2
]).

-export_type([key/0, default/0]).

-type key() :: atom() | binary() | term().
-type default() :: term().

-spec get_safe(Map :: map() | term(), Key :: key(), Default :: default()) -> term().
get_safe(Map, Key, Default) when is_map(Map) ->
    maps:get(Key, Map, Default);
get_safe(_NotMap, _Key, Default) ->
    Default.

-spec ensure_map(term()) -> map().
ensure_map(Map) when is_map(Map) ->
    Map;
ensure_map(_NotMap) ->
    #{}.

-spec ensure_list(term()) -> list().
ensure_list(List) when is_list(List) ->
    List;
ensure_list(_NotList) ->
    [].

-spec get_integer(term(), key(), term()) -> integer() | term().
get_integer(Map, Key, Default) when is_map(Map) ->
    Value = maps:get(Key, Map, undefined),
    case type_conv:to_integer(Value) of
        undefined -> Default;
        Converted -> Converted
    end;
get_integer(_NotMap, _Key, Default) ->
    Default.

-spec maps_share_any_key(map(), map()) -> boolean().
maps_share_any_key(MapA, MapB) ->
    {Smaller, Larger} =
        case map_size(MapA) =< map_size(MapB) of
            true -> {MapA, MapB};
            false -> {MapB, MapA}
        end,
    maps_share_any_key_iter(maps:iterator(Smaller), Larger).

-spec maps_share_any_key_iter(maps:iterator(), map()) -> boolean().
maps_share_any_key_iter(Iterator, LargerMap) ->
    case maps:next(Iterator) of
        none ->
            false;
        {Key, _, Next} ->
            maps:is_key(Key, LargerMap) orelse maps_share_any_key_iter(Next, LargerMap)
    end.
