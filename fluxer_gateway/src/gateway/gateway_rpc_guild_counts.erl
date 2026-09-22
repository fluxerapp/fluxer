%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_rpc_guild_counts).

-typing([eqwalizer]).

-export([handle/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec handle(binary(), map()) -> term().
handle(<<"guild.get_counts">>, #{<<"guild_id">> := GuildIdBin}) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    case gateway_rpc_guild_routing:safe_guild_counts_get(GuildId) of
        {ok, MemberCount, PresenceCount} ->
            #{<<"member_count">> => MemberCount, <<"presence_count">> => PresenceCount};
        miss ->
            #{<<"member_count">> => 0, <<"presence_count">> => 0}
    end;
handle(<<"guild.get_online_counts_batch">>, #{<<"guild_ids">> := GuildIdsBin}) ->
    GuildIds = validation:snowflake_list_or_throw(<<"guild_ids">>, GuildIdsBin),
    UniqueGuildIds = lists:usort(GuildIds),
    gateway_rpc_guild_routing:validate_batch_size(length(UniqueGuildIds)),
    #{<<"online_counts">> => fetch_entries(UniqueGuildIds)}.

-spec fetch_entries([integer()]) -> [map()].
fetch_entries(GuildIds) ->
    case guild_counts_cache_supports_bulk_get() of
        true -> normalize_entries(fetch_bulk(GuildIds));
        false -> [E || GId <- GuildIds, E <- [fetch_single(GId)], E =/= undefined]
    end.

-spec fetch_single(integer()) -> map() | undefined.
fetch_single(GuildId) ->
    case gateway_rpc_guild_routing:safe_guild_counts_get(GuildId) of
        {ok, MC, OC} -> count_entry(GuildId, MC, OC);
        miss -> undefined
    end.

-spec fetch_bulk([integer()]) -> term().
fetch_bulk(GuildIds) ->
    try
        erlang:apply(guild_counts_cache, bulk_get, [GuildIds])
    catch
        error:undef -> [];
        throw:_Reason -> [];
        error:_Reason -> [];
        exit:_Reason -> []
    end.

-spec guild_counts_cache_supports_bulk_get() -> boolean().
guild_counts_cache_supports_bulk_get() ->
    case code:ensure_loaded(guild_counts_cache) of
        {module, guild_counts_cache} ->
            erlang:function_exported(guild_counts_cache, bulk_get, 1);
        _ ->
            false
    end.

-spec normalize_entries(term()) -> [map()].
normalize_entries(Entries) when is_list(Entries) ->
    [N || E <- Entries, N <- [normalize_entry(E)], N =/= undefined];
normalize_entries(Entries) when is_map(Entries) ->
    normalize_entries(map_entries(Entries));
normalize_entries(_) ->
    [].

-spec map_entries(map()) -> [{term(), term()}].
map_entries(Entries) ->
    maps:fold(fun(K, V, Acc) -> [{K, V} | Acc] end, [], Entries).

-spec normalize_entry(term()) -> map() | undefined.
normalize_entry(#{guild_id := G, member_count := M, online_count := O}) ->
    build_entry(G, M, O);
normalize_entry(#{guild_id := G, member_count := M, presence_count := P}) ->
    build_entry(G, M, P);
normalize_entry(#{<<"guild_id">> := G, <<"member_count">> := M, <<"online_count">> := O}) ->
    build_entry(G, M, O);
normalize_entry(#{<<"guild_id">> := G, <<"member_count">> := M, <<"presence_count">> := P}) ->
    build_entry(G, M, P);
normalize_entry({G, M, O}) ->
    build_entry(G, M, O);
normalize_entry({G, {M, O}}) ->
    build_entry(G, M, O);
normalize_entry({G, #{member_count := M, online_count := O}}) ->
    build_entry(G, M, O);
normalize_entry({G, #{member_count := M, presence_count := P}}) ->
    build_entry(G, M, P);
normalize_entry({G, #{<<"member_count">> := M, <<"online_count">> := O}}) ->
    build_entry(G, M, O);
normalize_entry({G, #{<<"member_count">> := M, <<"presence_count">> := P}}) ->
    build_entry(G, M, P);
normalize_entry(_) ->
    undefined.

-spec build_entry(term(), term(), term()) -> map() | undefined.
build_entry(GuildIdRaw, MC, OC) ->
    case snowflake_id:parse_optional(GuildIdRaw) of
        GuildId when is_integer(GuildId) -> count_entry(GuildId, MC, OC);
        _ -> undefined
    end.

-spec count_entry(integer(), term(), term()) -> map().
count_entry(GuildId, MemberCount, OnlineCount) ->
    #{
        <<"guild_id">> => integer_to_binary(GuildId),
        <<"member_count">> => MemberCount,
        <<"online_count">> => OnlineCount
    }.

-ifdef(TEST).

execute_method_get_counts_uses_cache_hit_test() ->
    GuildId = 910001,
    ok = guild_counts_cache:delete(GuildId),
    ok = guild_counts_cache:update(GuildId, 12, 7),
    Params = #{<<"guild_id">> => integer_to_binary(GuildId)},
    try
        ?assertEqual(
            #{<<"member_count">> => 12, <<"presence_count">> => 7},
            handle(<<"guild.get_counts">>, Params)
        )
    after
        ok = guild_counts_cache:delete(GuildId)
    end.

execute_method_get_counts_returns_zero_on_cache_miss_test() ->
    GuildId = 910002,
    ok = guild_counts_cache:delete(GuildId),
    Params = #{<<"guild_id">> => integer_to_binary(GuildId)},
    ?assertEqual(
        #{<<"member_count">> => 0, <<"presence_count">> => 0},
        handle(<<"guild.get_counts">>, Params)
    ).

-endif.
