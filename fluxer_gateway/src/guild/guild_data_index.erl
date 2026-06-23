%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_data_index).
-typing([eqwalizer]).

-export([
    normalize_data/1,
    member_map/1,
    member_values/1,
    member_list/1,
    member_count/1,
    member_ids/1,
    member_role_index/1,
    get_member/2,
    put_member/2,
    put_member_map/2,
    put_member_list/2,
    remove_member/2,
    role_list/1,
    role_index/1,
    put_roles/2,
    channel_list/1,
    channel_index/1,
    put_channels/2,
    build_id_index/1,
    build_role_perms_cache/1,
    build_overwrite_perms_cache/1,
    ensure_list/1,
    extract_integer_list/1,
    ensure_data_map/1
]).

-type guild_data() :: map().

-export_type([guild_data/0]).

-spec normalize_data(term()) -> term().
normalize_data(Data) when is_map(Data) ->
    Data0 =
        case guild_data_normalize:guild_data(Data) of
            Normalized when is_map(Normalized) -> Normalized;
            _ -> Data
        end,
    MemberMap = guild_data_index_members:normalize_member_map(
        guild_data_index_members:member_map(Data0)
    ),
    Roles = guild_data_index_roles:role_list(Data0),
    Channels = guild_data_index_channels:channel_list(Data0),
    Data0#{
        <<"members">> => MemberMap,
        members_normalized => MemberMap,
        members_sorted_ids => lists:sort(maps:keys(MemberMap)),
        <<"roles">> => Roles,
        <<"channels">> => Channels,
        <<"role_index">> => build_id_index(Roles),
        <<"channel_index">> => build_id_index(Channels),
        <<"member_role_index">> =>
            guild_data_index_members:build_member_role_index(MemberMap),
        role_perms_cache =>
            guild_data_index_roles:build_role_perms_cache(Roles),
        overwrite_perms_cache =>
            guild_data_index_channels:build_overwrite_perms_cache(Channels)
    };
normalize_data(Data) ->
    Data.

-spec member_map(term()) -> map().
member_map(Data) -> guild_data_index_members:member_map(Data).

-spec member_values(term()) -> [map()].
member_values(Data) -> guild_data_index_members:member_values(Data).

-spec member_list(term()) -> [map()].
member_list(Data) -> guild_data_index_members:member_list(Data).

-spec member_count(term()) -> non_neg_integer().
member_count(Data) -> guild_data_index_members:member_count(Data).

-spec member_ids(term()) -> [integer()].
member_ids(Data) -> guild_data_index_members:member_ids(Data).

-spec member_role_index(term()) -> map().
member_role_index(Data) -> guild_data_index_members:member_role_index(Data).

-spec get_member(term(), term()) -> map() | undefined.
get_member(UserId, Data) -> guild_data_index_members:get_member(UserId, Data).

-spec put_member(term(), guild_data()) -> guild_data().
put_member(Member, Data) -> guild_data_index_members:put_member(Member, Data).

-spec put_member_map(term(), guild_data()) -> guild_data().
put_member_map(MemberMap, Data) -> guild_data_index_members:put_member_map(MemberMap, Data).

-spec put_member_list(term(), guild_data()) -> guild_data().
put_member_list(Members, Data) -> guild_data_index_members:put_member_list(Members, Data).

-spec remove_member(term(), guild_data()) -> guild_data().
remove_member(UserId, Data) -> guild_data_index_members:remove_member(UserId, Data).

-spec role_list(term()) -> [map()].
role_list(Data) -> guild_data_index_roles:role_list(Data).

-spec role_index(term()) -> map().
role_index(Data) -> guild_data_index_roles:role_index(Data).

-spec put_roles(term(), guild_data()) -> guild_data().
put_roles(Roles, Data) -> guild_data_index_roles:put_roles(Roles, Data).

-spec channel_list(term()) -> [map()].
channel_list(Data) -> guild_data_index_channels:channel_list(Data).

-spec channel_index(term()) -> map().
channel_index(Data) -> guild_data_index_channels:channel_index(Data).

-spec put_channels(term(), guild_data()) -> guild_data().
put_channels(Channels, Data) -> guild_data_index_channels:put_channels(Channels, Data).

-spec build_role_perms_cache([map()]) -> map().
build_role_perms_cache(Roles) -> guild_data_index_roles:build_role_perms_cache(Roles).

-spec build_overwrite_perms_cache([map()]) -> map().
build_overwrite_perms_cache(Channels) ->
    guild_data_index_channels:build_overwrite_perms_cache(Channels).

-spec build_id_index([map()]) -> #{integer() => map()}.
build_id_index(Items) ->
    lists:foldl(fun add_item_to_id_index/2, #{}, Items).

-spec add_item_to_id_index(map(), #{integer() => map()}) -> #{integer() => map()}.
add_item_to_id_index(Item, Acc) ->
    case snowflake_id:parse_optional(maps:get(<<"id">>, Item, undefined)) of
        undefined -> Acc;
        Id -> Acc#{Id => Item}
    end.

-spec ensure_list(term()) -> list().
ensure_list(Value) -> map_utils:ensure_list(Value).

-spec extract_integer_list(term()) -> [integer()].
extract_integer_list(List) when is_list(List) ->
    snowflake_id:parse_list(List);
extract_integer_list(undefined) ->
    [];
extract_integer_list(null) ->
    [];
extract_integer_list(Value) ->
    erlang:error({invalid_snowflake_list, Value}).

-spec ensure_data_map(map()) -> map().
ensure_data_map(State) ->
    map_utils:ensure_map(map_utils:get_safe(State, data, #{})).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

normalize_data_builds_indexes_test() ->
    Data = #{
        <<"members">> => [#{<<"user">> => #{<<"id">> => <<"1">>}}],
        <<"roles">> => [#{<<"id">> => <<"100">>}],
        <<"channels">> => [#{<<"id">> => <<"200">>}]
    },
    Normalized = normalize_data(Data),
    ?assert(is_map(maps:get(<<"members">>, Normalized))),
    ?assertMatch(#{100 := _}, role_index(Normalized)),
    ?assertMatch(#{200 := _}, channel_index(Normalized)).

normalize_data_empty_lists_test() ->
    Normalized = normalize_data(#{<<"members">> => [], <<"roles">> => [], <<"channels">> => []}),
    ?assertEqual(#{}, maps:get(<<"members">>, Normalized)),
    ?assertEqual([], maps:get(<<"roles">>, Normalized)),
    ?assertEqual([], maps:get(<<"channels">>, Normalized)).

normalize_data_non_map_input_test() ->
    ?assertEqual(not_a_map, normalize_data(not_a_map)),
    ?assertEqual(42, normalize_data(42)).

normalize_data_missing_keys_defaults_test() ->
    Normalized = normalize_data(#{}),
    ?assertEqual(#{}, maps:get(<<"members">>, Normalized)),
    ?assertEqual([], maps:get(<<"roles">>, Normalized)),
    ?assertEqual([], maps:get(<<"channels">>, Normalized)).

build_id_index_skips_items_without_id_test() ->
    Items = [
        #{<<"id">> => <<"1">>, <<"name">> => <<"first">>},
        #{<<"name">> => <<"no_id">>}
    ],
    Index = build_id_index(Items),
    ?assertEqual(1, map_size(Index)).

build_id_index_rejects_invalid_id_test() ->
    ?assertError({invalid_snowflake, <<"0">>}, build_id_index([#{<<"id">> => <<"0">>}])).

build_id_index_empty_list_test() ->
    ?assertEqual(#{}, build_id_index([])).

extract_integer_list_mixed_types_test() ->
    ?assertEqual([1, 2, 3], extract_integer_list([<<"1">>, 2, <<"3">>])),
    ?assertEqual([], extract_integer_list(undefined)),
    ?assertError({invalid_snowflake, <<"0">>}, extract_integer_list([<<"0">>, 4])),
    ?assertError({invalid_snowflake_list, not_a_list}, extract_integer_list(not_a_list)).

ensure_list_test() ->
    ?assertEqual([1, 2], ensure_list([1, 2])),
    ?assertEqual([], ensure_list(not_a_list)).

-endif.
