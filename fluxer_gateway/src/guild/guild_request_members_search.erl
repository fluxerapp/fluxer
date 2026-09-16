%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_request_members_search).
-typing([eqwalizer]).

-export([
    fetch_members_with_rollout/6,
    filter_members_by_query/3,
    member_matches_normalized_query/2,
    get_display_name/1,
    get_username/1,
    extract_user_id/1,
    resolve_member_limit/2
]).

-export_type([member/0, presence/0]).

-define(FULL_MEMBER_LIST_LIMIT, 100000).
-define(DEFAULT_QUERY_LIMIT, 25).

-type member() :: map().
-type presence() :: map().

-spec fetch_members_with_rollout(
    integer(), pid(), binary(), non_neg_integer(), [integer()], boolean()
) -> {[member()], [presence()]}.
fetch_members_with_rollout(_GuildId, GuildPid, Query, Limit, UserIds, Presences) ->
    fetch_members_from_guild(GuildPid, Query, Limit, UserIds, Presences).

-spec fetch_members_from_guild(
    pid(), binary(), non_neg_integer(), [integer()], boolean()
) -> {[member()], [presence()]}.
fetch_members_from_guild(GuildPid, Query, Limit, UserIds, Presences) ->
    Members = fetch_members(GuildPid, Query, Limit, UserIds),
    PresencesList = guild_request_members_presence:maybe_fetch_presences(
        Presences, GuildPid, Members
    ),
    {Members, PresencesList}.

-spec fetch_members(pid(), binary(), non_neg_integer(), [integer()]) -> [member()].
fetch_members(GuildPid, _Query, _Limit, UserIds) when UserIds =/= [] ->
    fetch_members_by_user_ids(GuildPid, UserIds);
fetch_members(GuildPid, Query, Limit, []) ->
    ActualLimit = resolve_member_limit(Query, Limit),
    apply_query_filter(GuildPid, Query, ActualLimit).

-spec apply_query_filter(pid(), binary(), pos_integer()) -> [member()].
apply_query_filter(GuildPid, <<>>, ActualLimit) ->
    case
        gen_server:call(
            GuildPid,
            {list_guild_members, #{limit => ActualLimit, offset => 0}},
            10000
        )
    of
        #{members := AllMembers} ->
            AllMembers;
        _ ->
            []
    end;
apply_query_filter(GuildPid, Query, ActualLimit) ->
    case
        gen_server:call(
            GuildPid,
            {search_guild_members, #{query => Query, limit => ActualLimit}},
            10000
        )
    of
        #{members := Members} when is_list(Members) ->
            Members;
        _ ->
            []
    end.

-spec fetch_members_by_user_ids(pid(), [integer()]) -> [member()].
fetch_members_by_user_ids(_GuildPid, []) ->
    [];
fetch_members_by_user_ids(GuildPid, UserIds) ->
    UniqueIds = lists:usort(UserIds),
    try
        case
            gen_server:call(
                GuildPid,
                {get_guild_members_batch, #{user_ids => UniqueIds}},
                5000
            )
        of
            #{members := MembersMap} when is_map(MembersMap) ->
                collect_found_members(UniqueIds, MembersMap);
            _ ->
                []
        end
    catch
        exit:_ -> []
    end.

-spec collect_found_members([integer()], map()) -> [member()].
collect_found_members(UniqueIds, MembersMap) ->
    [
        M
     || Id <- UniqueIds,
        (M = maps:get(Id, MembersMap, undefined)) =/= undefined
    ].

-spec resolve_member_limit(binary(), non_neg_integer()) -> pos_integer().
resolve_member_limit(<<>>, 0) ->
    ?FULL_MEMBER_LIST_LIMIT;
resolve_member_limit(_Query, 0) ->
    ?DEFAULT_QUERY_LIMIT;
resolve_member_limit(_Query, Limit) ->
    Limit.

-spec filter_members_by_query([member()], binary(), non_neg_integer()) -> [member()].
filter_members_by_query(_Members, _Query, 0) ->
    [];
filter_members_by_query(Members, Query, Limit) ->
    NormalizedQuery = string:lowercase(Query),
    filter_members_by_query(Members, NormalizedQuery, Limit, []).

-spec filter_members_by_query([member()], binary(), non_neg_integer(), [member()]) ->
    [member()].
filter_members_by_query(_Members, _NormalizedQuery, 0, Acc) ->
    lists:reverse(Acc);
filter_members_by_query([], _NormalizedQuery, _Limit, Acc) ->
    lists:reverse(Acc);
filter_members_by_query([Member | Rest], NormalizedQuery, Limit, Acc) ->
    case member_matches_normalized_query(Member, NormalizedQuery) of
        true -> filter_members_by_query(Rest, NormalizedQuery, Limit - 1, [Member | Acc]);
        false -> filter_members_by_query(Rest, NormalizedQuery, Limit, Acc)
    end.

-spec member_matches_normalized_query(member(), binary()) -> boolean().
member_matches_normalized_query(_Member, <<>>) ->
    true;
member_matches_normalized_query(Member, NormalizedQuery) when is_map(Member) ->
    User = map_utils:ensure_map(maps:get(<<"user">>, Member, #{})),
    name_matches(maps:get(<<"nick">>, Member, undefined), NormalizedQuery) orelse
        name_matches(maps:get(<<"global_name">>, User, undefined), NormalizedQuery) orelse
        name_matches(maps:get(<<"username">>, User, undefined), NormalizedQuery);
member_matches_normalized_query(_Member, _NormalizedQuery) ->
    false.

-spec name_matches(term(), binary()) -> boolean().
name_matches(Name, NormalizedQuery) when is_binary(Name) ->
    prefix_binary(NormalizedQuery, string:lowercase(Name));
name_matches(_Name, _NormalizedQuery) ->
    false.

-spec prefix_binary(binary(), binary()) -> boolean().
prefix_binary(Prefix, Value) ->
    PrefixSize = byte_size(Prefix),
    byte_size(Value) >= PrefixSize andalso
        binary:part(Value, 0, PrefixSize) =:= Prefix.

-spec get_display_name(member()) -> binary().
get_display_name(Member) when is_map(Member) ->
    Nick = maps:get(<<"nick">>, Member, undefined),
    resolve_nick(Nick, Member);
get_display_name(_) ->
    <<>>.

-spec resolve_nick(term(), member()) -> binary().
resolve_nick(undefined, Member) -> get_fallback_name(Member);
resolve_nick(null, Member) -> get_fallback_name(Member);
resolve_nick(Nick, _Member) when is_binary(Nick) -> Nick;
resolve_nick(_, Member) -> get_fallback_name(Member).

-spec get_fallback_name(member()) -> binary().
get_fallback_name(Member) ->
    User = maps:get(<<"user">>, Member, #{}),
    GlobalName = maps:get(<<"global_name">>, User, undefined),
    resolve_global_name(GlobalName, User).

-spec resolve_global_name(term(), map()) -> binary().
resolve_global_name(undefined, User) -> get_username(User);
resolve_global_name(null, User) -> get_username(User);
resolve_global_name(Name, _User) when is_binary(Name) -> Name;
resolve_global_name(_, User) -> get_username(User).

-spec get_username(map()) -> binary().
get_username(User) ->
    Username = maps:get(<<"username">>, User, <<>>),
    safe_binary(Username).

-spec safe_binary(term()) -> binary().
safe_binary(null) -> <<>>;
safe_binary(undefined) -> <<>>;
safe_binary(V) when is_binary(V) -> V;
safe_binary(_) -> <<>>.

-spec extract_user_id(member()) -> integer() | undefined.
extract_user_id(Member) when is_map(Member) ->
    User = map_utils:ensure_map(maps:get(<<"user">>, Member, #{})),
    snowflake_id:parse_maybe(maps:get(<<"id">>, User, undefined));
extract_user_id(_) ->
    undefined.
