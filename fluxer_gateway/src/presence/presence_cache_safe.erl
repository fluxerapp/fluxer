%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(presence_cache_safe).
-typing([eqwalizer]).

-export([get/1, get_visible/1, bulk_get/1, visible_bulk_get/1]).

-define(LIVE_FALLBACK_LOOKUP_TIMEOUT_MS, 250).
-define(MAX_LIVE_FALLBACK_LOOKUPS, 128).

-spec get(term()) -> {ok, map()} | not_found.
get(UserId) when is_integer(UserId) ->
    try presence_cache:get(UserId) of
        {ok, Presence} when is_map(Presence) -> {ok, Presence};
        _ -> not_found
    catch
        error:_ -> not_found;
        exit:_ -> not_found
    end;
get(_) ->
    not_found.

-spec get_visible(term()) -> {ok, map()} | not_found.
get_visible(UserId) when is_integer(UserId) ->
    case visible_bulk_get([UserId]) of
        [Presence | _] -> {ok, Presence};
        [] -> not_found
    end;
get_visible(_) ->
    not_found.

-spec bulk_get([term()]) -> [map()].
bulk_get(UserIds) when is_list(UserIds) ->
    ValidIds = [UserId || UserId <- UserIds, is_integer(UserId)],
    try presence_cache:bulk_get(ValidIds) of
        Presences -> [Presence || Presence <- Presences, is_map(Presence)]
    catch
        error:_ -> [];
        exit:_ -> []
    end;
bulk_get(_) ->
    [].

-spec visible_bulk_get([term()]) -> [map()].
visible_bulk_get(UserIds) when is_list(UserIds) ->
    ValidIds = presence_cache_bulk:normalize_user_ids(UserIds),
    Cached = [Presence || Presence <- bulk_get(ValidIds), visible_presence(Presence)],
    CachedMap = presence_map(Cached),
    Missing = missing_user_ids(ValidIds, CachedMap),
    LiveMap = live_presence_map(limit_live_fallback_ids(Missing)),
    presence_cache_bulk:presence_values(maps:merge(CachedMap, LiveMap));
visible_bulk_get(_) ->
    [].

-spec missing_user_ids([integer()], #{integer() => map()}) -> [integer()].
missing_user_ids(UserIds, PresenceMap) ->
    [UserId || UserId <- UserIds, not maps:is_key(UserId, PresenceMap)].

-spec limit_live_fallback_ids([integer()]) -> [integer()].
limit_live_fallback_ids(UserIds) when length(UserIds) =< ?MAX_LIVE_FALLBACK_LOOKUPS ->
    UserIds;
limit_live_fallback_ids(UserIds) ->
    logger:warning("Presence cache live fallback capped", #{
        requested => length(UserIds),
        cap => ?MAX_LIVE_FALLBACK_LOOKUPS
    }),
    lists:sublist(UserIds, ?MAX_LIVE_FALLBACK_LOOKUPS).

-spec live_presence_map([integer()]) -> #{integer() => map()}.
live_presence_map(UserIds) ->
    lists:foldl(fun add_live_presence/2, #{}, UserIds).

-spec add_live_presence(integer(), #{integer() => map()}) -> #{integer() => map()}.
add_live_presence(UserId, Acc) ->
    case live_visible_presence(UserId) of
        {ok, Presence} -> Acc#{UserId => Presence};
        not_found -> Acc
    end.

-spec live_visible_presence(integer()) -> {ok, map()} | not_found.
live_visible_presence(UserId) ->
    case
        presence_manager_routing:call_owner_manager(
            UserId, {lookup, UserId}, ?LIVE_FALLBACK_LOOKUP_TIMEOUT_MS
        )
    of
        {ok, Pid} when is_pid(Pid) -> call_live_presence(UserId, Pid);
        _ -> not_found
    end.

-spec call_live_presence(integer(), pid()) -> {ok, map()} | not_found.
call_live_presence(UserId, Pid) ->
    try gen_server:call(Pid, get_current_visible_presence, ?LIVE_FALLBACK_LOOKUP_TIMEOUT_MS) of
        {ok, Presence} when is_map(Presence) ->
            maybe_accept_live_presence(UserId, Presence);
        _ ->
            not_found
    catch
        error:_ -> not_found;
        exit:_ -> not_found
    end.

-spec maybe_accept_live_presence(integer(), map()) -> {ok, map()} | not_found.
maybe_accept_live_presence(UserId, Presence) ->
    case presence_user_id(Presence) of
        UserId ->
            presence_cache:put(UserId, Presence),
            {ok, Presence};
        _ ->
            not_found
    end.

-spec presence_map([map()]) -> #{integer() => map()}.
presence_map(Presences) ->
    lists:foldl(fun add_presence/2, #{}, Presences).

-spec add_presence(map(), #{integer() => map()}) -> #{integer() => map()}.
add_presence(Presence, Acc) ->
    case presence_user_id(Presence) of
        UserId when is_integer(UserId), UserId > 0 -> Acc#{UserId => Presence};
        _ -> Acc
    end.

-spec presence_user_id(map()) -> integer() | undefined.
presence_user_id(Presence) ->
    case maps:get(<<"user">>, Presence, undefined) of
        User when is_map(User) ->
            snowflake_id:parse_maybe(maps:get(<<"id">>, User, undefined));
        _ ->
            undefined
    end.

-spec visible_presence(map()) -> boolean().
visible_presence(Presence) ->
    Status = maps:get(<<"status">>, Presence, <<"offline">>),
    Status =/= <<"offline">> andalso Status =/= <<"invisible">>.
