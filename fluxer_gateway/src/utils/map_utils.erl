%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(map_utils).
-typing([eqwalizer]).

-export([
    get_safe/3,
    ensure_map/1,
    ensure_list/1,
    get_integer/3
]).

-export_type([key/0, default/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

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
