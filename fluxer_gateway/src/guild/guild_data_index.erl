%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_data_index).
-typing([eqwalizer]).

-export([
    normalize_data/1,
    normalize_map/1,
    put_roles/2,
    put_channels/2,
    build_id_index/1,
    ensure_data_map/1
]).

-type guild_data() :: map().

-export_type([guild_data/0]).

-spec normalize_data(term()) -> term().
normalize_data(Data) when is_map(Data) ->
    normalize_map(Data);
normalize_data(Data) ->
    Data.

-spec normalize_map(map()) -> guild_data().
normalize_map(Data) ->
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
    Data1 = guild_data_index_channels:put_forum_categories(Channels, Data0),
    Data1#{
        <<"members">> => MemberMap,
        members_normalized => MemberMap,
        member_list_revision => make_ref(),
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
    }.

-spec put_roles(term(), guild_data()) -> guild_data().
put_roles(Roles, Data) ->
    ensure_guild_data(guild_data_index_roles:put_roles(Roles, Data)).

-spec put_channels(term(), guild_data()) -> guild_data().
put_channels(Channels, Data) ->
    ensure_guild_data(guild_data_index_channels:put_channels(Channels, Data)).

-spec build_id_index([map()]) -> #{integer() => map()}.
build_id_index(Items) ->
    lists:foldl(fun add_item_to_id_index/2, #{}, Items).

-spec add_item_to_id_index(map(), #{integer() => map()}) -> #{integer() => map()}.
add_item_to_id_index(Item, Acc) ->
    case snowflake_id:parse_optional(maps:get(<<"id">>, Item, undefined)) of
        undefined -> Acc;
        Id -> Acc#{Id => Item}
    end.

-spec ensure_data_map(map()) -> map().
ensure_data_map(State) ->
    map_utils:ensure_map(map_utils:get_safe(State, data, #{})).

-spec ensure_guild_data(term()) -> guild_data().
ensure_guild_data(Data) when is_map(Data) ->
    Data.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

member_list_revision_rotates_on_every_member_writer_test() ->
    Member = #{<<"user">> => #{<<"id">> => 1}},
    Data0 = normalize_map(#{<<"members">> => [Member]}),
    Data1 = normalize_map(Data0),
    Data2 = guild_data_index_members:put_member(Member#{<<"nick">> => <<"renamed">>}, Data1),
    Data3 = guild_data_index_members:put_member_map(
        guild_data_index_members:member_map(Data2), Data2
    ),
    Data4 = guild_data_index_members:put_member_list(
        guild_data_index_members:member_list(Data3), Data3
    ),
    Data5 = guild_data_index_members:remove_member(1, Data4),
    Revisions = [
        maps:get(member_list_revision, D)
     || D <- [Data0, Data1, Data2, Data3, Data4, Data5]
    ],
    ?assert(lists:all(fun is_reference/1, Revisions)),
    ?assertEqual(length(Revisions), length(lists:usort(Revisions))).

-endif.
