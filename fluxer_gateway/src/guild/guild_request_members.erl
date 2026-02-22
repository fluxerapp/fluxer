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

-module(guild_request_members).

-export([
    handle_request/3
]).

-define(CHUNK_SIZE, 1000).
-define(MAX_USER_IDS, 100).
-define(MAX_NONCE_LENGTH, 32).
-define(FULL_MEMBER_LIST_LIMIT, 100000).
-define(DEFAULT_QUERY_LIMIT, 25).
-define(MAX_MEMBER_QUERY_LIMIT, 100).
-define(REQUEST_MEMBERS_RATE_LIMIT_TABLE, guild_request_members_rate_limit).
-define(REQUEST_MEMBERS_RATE_LIMIT_WINDOW_MS, 10000).
-define(REQUEST_MEMBERS_RATE_LIMIT_MAX_EVENTS, 5).
-define(REQUEST_MEMBERS_GUILD_RATE_LIMIT_TABLE, guild_request_members_guild_rate_limit).
-define(REQUEST_MEMBERS_GUILD_RATE_LIMIT_WINDOW_MS, 10000).
-define(REQUEST_MEMBERS_GUILD_RATE_LIMIT_MAX_EVENTS, 25).

-type session_state() :: map().
-type request_data() :: map().
-type member() :: map().
-type presence() :: map().

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec handle_request(request_data(), pid(), session_state()) -> ok | {error, atom()}.
handle_request(Data, SocketPid, SessionState) when is_map(Data), is_pid(SocketPid) ->
    case parse_request(Data) of
        {ok, Request} ->
            process_request(Request, SocketPid, SessionState);
        {error, Reason} ->
            {error, Reason}
    end;
handle_request(_, _, _) ->
    {error, invalid_request}.

-spec parse_request(request_data()) -> {ok, map()} | {error, atom()}.
parse_request(Data) ->
    GuildIdRaw = maps:get(<<"guild_id">>, Data, undefined),
    Query = maps:get(<<"query">>, Data, <<>>),
    Limit = maps:get(<<"limit">>, Data, 0),
    UserIdsRaw = maps:get(<<"user_ids">>, Data, []),
    Presences = maps:get(<<"presences">>, Data, false),
    Nonce = maps:get(<<"nonce">>, Data, null),
    NormalizedNonce = normalize_nonce(Nonce),
    case validate_guild_id(GuildIdRaw) of
        {ok, GuildId} ->
            case validate_user_ids(UserIdsRaw) of
                {ok, UserIds} ->
                    {ok, #{
                        guild_id => GuildId,
                        query => ensure_binary(Query),
                        limit => ensure_limit(Limit),
                        user_ids => UserIds,
                        presences => Presences =:= true,
                        nonce => NormalizedNonce
                    }};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec validate_guild_id(term()) -> {ok, integer()} | {error, atom()}.
validate_guild_id(GuildId) when is_integer(GuildId), GuildId > 0 ->
    {ok, GuildId};
validate_guild_id(GuildId) when is_binary(GuildId) ->
    case validation:validate_snowflake(<<"guild_id">>, GuildId) of
        {ok, Id} -> {ok, Id};
        {error, _, _} -> {error, invalid_guild_id}
    end;
validate_guild_id(_) ->
    {error, invalid_guild_id}.

-spec validate_user_ids(term()) -> {ok, [integer()]} | {error, atom()}.
validate_user_ids(UserIds) when is_list(UserIds) ->
    case length(UserIds) > ?MAX_USER_IDS of
        true ->
            {error, too_many_user_ids};
        false ->
            ParsedIds = lists:filtermap(
                fun(Id) ->
                    case parse_user_id(Id) of
                        {ok, ParsedId} -> {true, ParsedId};
                        error -> false
                    end
                end,
                UserIds
            ),
            {ok, ParsedIds}
    end;
validate_user_ids(_) ->
    {ok, []}.

-spec parse_user_id(term()) -> {ok, integer()} | error.
parse_user_id(Id) when is_integer(Id), Id > 0 ->
    {ok, Id};
parse_user_id(Id) when is_binary(Id) ->
    case type_conv:to_integer(Id) of
        undefined -> error;
        ParsedId when ParsedId > 0 -> {ok, ParsedId};
        _ -> error
    end;
parse_user_id(_) ->
    error.

-spec ensure_binary(term()) -> binary().
ensure_binary(Value) when is_binary(Value) -> Value;
ensure_binary(_) -> <<>>.

-spec ensure_limit(term()) -> non_neg_integer().
ensure_limit(Limit) when is_integer(Limit), Limit >= 0 ->
    min(Limit, ?MAX_MEMBER_QUERY_LIMIT);
ensure_limit(_) -> 0.

-spec normalize_nonce(term()) -> binary() | null.
normalize_nonce(Nonce) when is_binary(Nonce), byte_size(Nonce) =< ?MAX_NONCE_LENGTH ->
    Nonce;
normalize_nonce(_) ->
    null.

-spec process_request(map(), pid(), session_state()) -> ok | {error, atom()}.
process_request(Request, SocketPid, SessionState) ->
    #{guild_id := GuildId, query := Query, limit := Limit, user_ids := UserIds} = Request,
    UserIdBin = maps:get(user_id, SessionState),
    UserId = type_conv:to_integer(UserIdBin),
    case check_request_rate_limit(UserId) of
        ok ->
            case check_guild_request_rate_limit(GuildId) of
                ok ->
                    case check_permission(UserId, GuildId, Query, Limit, UserIds, SessionState) of
                        ok ->
                            fetch_and_send_members(Request, SocketPid, SessionState);
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec check_request_rate_limit(integer() | undefined) -> ok | {error, atom()}.
check_request_rate_limit(UserId) when is_integer(UserId), UserId > 0 ->
    ensure_request_rate_limit_table(),
    Now = erlang:system_time(millisecond),
    case ets:lookup(?REQUEST_MEMBERS_RATE_LIMIT_TABLE, UserId) of
        [] ->
            ets:insert(?REQUEST_MEMBERS_RATE_LIMIT_TABLE, {UserId, [Now]}),
            ok;
        [{UserId, Timestamps}] ->
            RecentTimestamps =
                [T || T <- Timestamps, (Now - T) < ?REQUEST_MEMBERS_RATE_LIMIT_WINDOW_MS],
            case length(RecentTimestamps) >= ?REQUEST_MEMBERS_RATE_LIMIT_MAX_EVENTS of
                true ->
                    {error, rate_limited};
                false ->
                    ets:insert(?REQUEST_MEMBERS_RATE_LIMIT_TABLE, {UserId, [Now | RecentTimestamps]}),
                    ok
            end
    end;
check_request_rate_limit(_) ->
    {error, invalid_session}.

-spec check_guild_request_rate_limit(integer()) -> ok | {error, atom()}.
check_guild_request_rate_limit(GuildId) when is_integer(GuildId), GuildId > 0 ->
    ensure_guild_request_rate_limit_table(),
    Now = erlang:system_time(millisecond),
    case ets:lookup(?REQUEST_MEMBERS_GUILD_RATE_LIMIT_TABLE, GuildId) of
        [] ->
            ets:insert(?REQUEST_MEMBERS_GUILD_RATE_LIMIT_TABLE, {GuildId, [Now]}),
            ok;
        [{GuildId, Timestamps}] ->
            RecentTimestamps =
                [T || T <- Timestamps, (Now - T) < ?REQUEST_MEMBERS_GUILD_RATE_LIMIT_WINDOW_MS],
            case length(RecentTimestamps) >= ?REQUEST_MEMBERS_GUILD_RATE_LIMIT_MAX_EVENTS of
                true ->
                    {error, rate_limited};
                false ->
                    ets:insert(
                        ?REQUEST_MEMBERS_GUILD_RATE_LIMIT_TABLE, {GuildId, [Now | RecentTimestamps]}
                    ),
                    ok
            end
    end;
check_guild_request_rate_limit(_) ->
    {error, invalid_guild_id}.

-spec ensure_request_rate_limit_table() -> ok.
ensure_request_rate_limit_table() ->
    case ets:whereis(?REQUEST_MEMBERS_RATE_LIMIT_TABLE) of
        undefined ->
            try
                ets:new(?REQUEST_MEMBERS_RATE_LIMIT_TABLE, [named_table, public, set]),
                ok
            catch
                error:badarg ->
                    ok
            end;
        _ ->
            ok
    end.

-spec ensure_guild_request_rate_limit_table() -> ok.
ensure_guild_request_rate_limit_table() ->
    case ets:whereis(?REQUEST_MEMBERS_GUILD_RATE_LIMIT_TABLE) of
        undefined ->
            try
                ets:new(?REQUEST_MEMBERS_GUILD_RATE_LIMIT_TABLE, [named_table, public, set]),
                ok
            catch
                error:badarg ->
                    ok
            end;
        _ ->
            ok
    end.

-spec check_permission(
    integer(), integer(), binary(), non_neg_integer(), [integer()], session_state()
) ->
    ok | {error, atom()}.
check_permission(UserId, GuildId, Query, Limit, UserIds, SessionState) ->
    RequiresPermission = Query =:= <<>> andalso Limit =:= 0 andalso UserIds =:= [],
    case RequiresPermission of
        false ->
            ok;
        true ->
            case lookup_guild(GuildId, SessionState) of
                {ok, GuildPid} ->
                    check_management_permission(UserId, GuildId, GuildPid);
                {error, _} ->
                    {error, guild_not_found}
            end
    end.

-spec check_management_permission(integer(), integer(), pid()) -> ok | {error, atom()}.
check_management_permission(UserId, _GuildId, GuildPid) ->
    ManageRoles = constants:manage_roles_permission(),
    KickMembers = constants:kick_members_permission(),
    BanMembers = constants:ban_members_permission(),
    RequiredPermission = ManageRoles bor KickMembers bor BanMembers,
    PermRequest = #{
        user_id => UserId,
        permission => RequiredPermission,
        channel_id => undefined
    },
    case gen_server:call(GuildPid, {check_permission, PermRequest}, 5000) of
        #{has_permission := true} -> ok;
        #{has_permission := false} -> {error, missing_permission};
        _ -> {error, permission_check_failed}
    end.

-spec lookup_guild(integer(), session_state()) -> {ok, pid()} | {error, not_found}.
lookup_guild(GuildId, SessionState) ->
    Guilds = maps:get(guilds, SessionState, #{}),
    case maps:get(GuildId, Guilds, undefined) of
        {Pid, _Ref} when is_pid(Pid) ->
            {ok, Pid};
        undefined ->
            case guild_manager:lookup(GuildId) of
                {ok, Pid} when is_pid(Pid) -> {ok, Pid};
                _ -> {error, not_found}
            end;
        _ ->
            {error, not_found}
    end.

-spec fetch_and_send_members(map(), pid(), session_state()) -> ok | {error, atom()}.
fetch_and_send_members(Request, _SocketPid, SessionState) ->
    #{
        guild_id := GuildId,
        query := Query,
        limit := Limit,
        user_ids := UserIds,
        presences := Presences,
        nonce := Nonce
    } = Request,
    SessionId = maps:get(session_id, SessionState),
    case lookup_guild(GuildId, SessionState) of
        {ok, GuildPid} ->
            Members = fetch_members(GuildPid, Query, Limit, UserIds),
            PresencesList = maybe_fetch_presences(Presences, GuildPid, Members),
            send_member_chunks(GuildPid, SessionId, Members, PresencesList, Nonce),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

-spec fetch_members(pid(), binary(), non_neg_integer(), [integer()]) -> [member()].
fetch_members(GuildPid, _Query, _Limit, UserIds) when UserIds =/= [] ->
    fetch_members_by_user_ids(GuildPid, UserIds);
fetch_members(GuildPid, Query, Limit, []) ->
    ActualLimit = resolve_member_limit(Query, Limit),
    case
        gen_server:call(GuildPid, {list_guild_members, #{limit => ActualLimit, offset => 0}}, 10000)
    of
        #{members := AllMembers} ->
            case Query of
                <<>> ->
                    lists:sublist(AllMembers, ActualLimit);
                _ ->
                    filter_members_by_query(AllMembers, Query, ActualLimit)
            end;
        _ ->
            []
    end.

-spec fetch_members_by_user_ids(pid(), [integer()]) -> [member()].
fetch_members_by_user_ids(GuildPid, UserIds) ->
    lists:filtermap(
        fun(UserId) ->
            try
                case gen_server:call(GuildPid, {get_guild_member, #{user_id => UserId}}, 5000) of
                    #{success := true, member_data := Member} when is_map(Member) ->
                        {true, Member};
                    _ ->
                        false
                end
            catch
                exit:_ ->
                    false
            end
        end,
        lists:usort(UserIds)
    ).

-spec resolve_member_limit(binary(), non_neg_integer()) -> pos_integer().
resolve_member_limit(<<>>, 0) ->
    ?FULL_MEMBER_LIST_LIMIT;
resolve_member_limit(_Query, 0) ->
    ?DEFAULT_QUERY_LIMIT;
resolve_member_limit(_Query, Limit) ->
    Limit.

-spec filter_members_by_query([member()], binary(), non_neg_integer()) -> [member()].
filter_members_by_query(Members, Query, Limit) ->
    NormalizedQuery = string:lowercase(binary_to_list(Query)),
    Matches = lists:filter(
        fun(Member) ->
            DisplayName = get_display_name(Member),
            NormalizedName = string:lowercase(binary_to_list(DisplayName)),
            lists:prefix(NormalizedQuery, NormalizedName)
        end,
        Members
    ),
    lists:sublist(Matches, Limit).

-spec get_display_name(member()) -> binary().
get_display_name(Member) when is_map(Member) ->
    Nick = maps:get(<<"nick">>, Member, undefined),
    case Nick of
        undefined -> get_fallback_name(Member);
        null -> get_fallback_name(Member);
        _ when is_binary(Nick) -> Nick;
        _ -> get_fallback_name(Member)
    end;
get_display_name(_) ->
    <<>>.

-spec get_fallback_name(member()) -> binary().
get_fallback_name(Member) ->
    User = maps:get(<<"user">>, Member, #{}),
    GlobalName = maps:get(<<"global_name">>, User, undefined),
    case GlobalName of
        undefined -> get_username(User);
        null -> get_username(User);
        _ when is_binary(GlobalName) -> GlobalName;
        _ -> get_username(User)
    end.

-spec get_username(map()) -> binary().
get_username(User) ->
    Username = maps:get(<<"username">>, User, <<>>),
    case Username of
        null -> <<>>;
        undefined -> <<>>;
        _ when is_binary(Username) -> Username;
        _ -> <<>>
    end.

-spec extract_user_id(member()) -> integer() | undefined.
extract_user_id(Member) when is_map(Member) ->
    User = maps:get(<<"user">>, Member, #{}),
    map_utils:get_integer(User, <<"id">>, undefined);
extract_user_id(_) ->
    undefined.

-spec maybe_fetch_presences(boolean(), pid(), [member()]) -> [presence()].
maybe_fetch_presences(false, _GuildPid, _Members) ->
    [];
maybe_fetch_presences(true, _GuildPid, Members) ->
    UserIds = lists:filtermap(
        fun(Member) ->
            case extract_user_id(Member) of
                undefined -> false;
                UserId -> {true, UserId}
            end
        end,
        Members
    ),
    case UserIds of
        [] ->
            [];
        _ ->
            Cached = presence_cache:bulk_get(UserIds),
            [P || P <- Cached, presence_visible(P)]
    end.

-spec presence_visible(presence()) -> boolean().
presence_visible(P) ->
    Status = maps:get(<<"status">>, P, <<"offline">>),
    Status =/= <<"offline">> andalso Status =/= <<"invisible">>.

-spec send_member_chunks(pid(), binary(), [member()], [presence()], term()) -> ok.
send_member_chunks(GuildPid, SessionId, Members, Presences, Nonce) ->
    TotalChunks = max(1, (length(Members) + ?CHUNK_SIZE - 1) div ?CHUNK_SIZE),
    MemberChunks = chunk_list(Members, ?CHUNK_SIZE),
    PresenceChunks = chunk_presences(Presences, MemberChunks),
    lists:foldl(
        fun({MemberChunk, PresenceChunk}, ChunkIndex) ->
            ChunkData = build_chunk_data(
                MemberChunk, PresenceChunk, ChunkIndex, TotalChunks, Nonce
            ),
            gen_server:cast(GuildPid, {send_members_chunk, SessionId, ChunkData}),
            ChunkIndex + 1
        end,
        0,
        lists:zip(MemberChunks, PresenceChunks)
    ),
    ok.

-spec build_chunk_data([member()], [presence()], non_neg_integer(), non_neg_integer(), term()) ->
    map().
build_chunk_data(Members, Presences, ChunkIndex, TotalChunks, Nonce) ->
    Base = #{
        <<"members">> => Members,
        <<"chunk_index">> => ChunkIndex,
        <<"chunk_count">> => TotalChunks
    },
    WithPresences =
        case Presences of
            [] -> Base;
            _ -> maps:put(<<"presences">>, Presences, Base)
        end,
    case Nonce of
        null -> WithPresences;
        _ -> maps:put(<<"nonce">>, Nonce, WithPresences)
    end.

-spec chunk_list([T], pos_integer()) -> [[T]] when T :: term().
chunk_list([], _Size) ->
    [[]];
chunk_list(List, Size) ->
    chunk_list(List, Size, []).

-spec chunk_list([T], pos_integer(), [[T]]) -> [[T]] when T :: term().
chunk_list([], _Size, Acc) ->
    lists:reverse(Acc);
chunk_list(List, Size, Acc) ->
    {Chunk, Rest} = lists:split(min(Size, length(List)), List),
    chunk_list(Rest, Size, [Chunk | Acc]).

-spec chunk_presences([presence()], [[member()]]) -> [[presence()]].
chunk_presences(Presences, MemberChunks) ->
    lists:map(
        fun(MemberChunk) ->
            ChunkUserIds = sets:from_list([extract_user_id(M) || M <- MemberChunk]),
            lists:filter(
                fun(Presence) ->
                    User = maps:get(<<"user">>, Presence, #{}),
                    UserId = map_utils:get_integer(User, <<"id">>, undefined),
                    UserId =/= undefined andalso sets:is_element(UserId, ChunkUserIds)
                end,
                Presences
            )
        end,
        MemberChunks
    ).

-ifdef(TEST).

parse_request_valid_test() ->
    Data = #{
        <<"guild_id">> => <<"123456789">>,
        <<"query">> => <<"test">>,
        <<"limit">> => 10,
        <<"presences">> => true,
        <<"nonce">> => <<"abc123">>
    },
    {ok, Request} = parse_request(Data),
    ?assertEqual(123456789, maps:get(guild_id, Request)),
    ?assertEqual(<<"test">>, maps:get(query, Request)),
    ?assertEqual(10, maps:get(limit, Request)),
    ?assertEqual(true, maps:get(presences, Request)),
    ?assertEqual(<<"abc123">>, maps:get(nonce, Request)).

parse_request_with_user_ids_test() ->
    Data = #{
        <<"guild_id">> => <<"123">>,
        <<"user_ids">> => [<<"1">>, <<"2">>, <<"3">>]
    },
    {ok, Request} = parse_request(Data),
    ?assertEqual([1, 2, 3], maps:get(user_ids, Request)).

parse_request_invalid_guild_id_test() ->
    Data = #{<<"guild_id">> => <<"invalid">>},
    {error, invalid_guild_id} = parse_request(Data).

chunk_list_test() ->
    ?assertEqual([[1, 2], [3, 4], [5]], chunk_list([1, 2, 3, 4, 5], 2)),
    ?assertEqual([[1, 2, 3]], chunk_list([1, 2, 3], 5)),
    ?assertEqual([[]], chunk_list([], 5)).

filter_members_by_query_test() ->
    Members = [
        #{<<"user">> => #{<<"id">> => <<"1">>, <<"username">> => <<"alice">>}},
        #{<<"user">> => #{<<"id">> => <<"2">>, <<"username">> => <<"bob">>}},
        #{<<"user">> => #{<<"id">> => <<"3">>, <<"username">> => <<"alicia">>}}
    ],
    Results = filter_members_by_query(Members, <<"ali">>, 10),
    ?assertEqual(2, length(Results)).

display_name_priority_test() ->
    MemberWithNick = #{
        <<"user">> => #{<<"username">> => <<"user">>, <<"global_name">> => <<"Global">>},
        <<"nick">> => <<"Nick">>
    },
    ?assertEqual(<<"Nick">>, get_display_name(MemberWithNick)),
    MemberWithGlobal = #{
        <<"user">> => #{<<"username">> => <<"user">>, <<"global_name">> => <<"Global">>}
    },
    ?assertEqual(<<"Global">>, get_display_name(MemberWithGlobal)),
    MemberWithUsername = #{
        <<"user">> => #{<<"username">> => <<"user">>}
    },
    ?assertEqual(<<"user">>, get_display_name(MemberWithUsername)).

normalize_nonce_test() ->
    ?assertEqual(<<"abc">>, normalize_nonce(<<"abc">>)),
    ?assertEqual(null, normalize_nonce(<<"this_nonce_is_way_too_long_to_be_valid">>)),
    ?assertEqual(null, normalize_nonce(undefined)).

validate_user_ids_too_many_test() ->
    UserIds = lists:seq(1, 101),
    ?assertEqual({error, too_many_user_ids}, validate_user_ids(UserIds)).

validate_user_ids_exactly_max_test() ->
    UserIds = lists:seq(1, 100),
    {ok, Parsed} = validate_user_ids(UserIds),
    ?assertEqual(100, length(Parsed)).

validate_user_ids_non_list_test() ->
    {ok, []} = validate_user_ids(not_a_list).

validate_user_ids_filters_invalid_test() ->
    {ok, Parsed} = validate_user_ids([<<"1">>, <<"invalid">>, 3, -5, 0]),
    ?assertEqual([1, 3], Parsed).

validate_user_ids_empty_test() ->
    {ok, []} = validate_user_ids([]).

parse_user_id_integer_test() ->
    ?assertEqual({ok, 42}, parse_user_id(42)).

parse_user_id_binary_test() ->
    ?assertEqual({ok, 123}, parse_user_id(<<"123">>)).

parse_user_id_zero_test() ->
    ?assertEqual(error, parse_user_id(0)).

parse_user_id_negative_test() ->
    ?assertEqual(error, parse_user_id(-1)).

parse_user_id_invalid_binary_test() ->
    ?assertEqual(error, parse_user_id(<<"abc">>)).

parse_user_id_other_type_test() ->
    ?assertEqual(error, parse_user_id(1.5)).

ensure_binary_binary_test() ->
    ?assertEqual(<<"hello">>, ensure_binary(<<"hello">>)).

ensure_binary_integer_test() ->
    ?assertEqual(<<>>, ensure_binary(42)).

ensure_binary_undefined_test() ->
    ?assertEqual(<<>>, ensure_binary(undefined)).

ensure_limit_valid_test() ->
    ?assertEqual(10, ensure_limit(10)).

ensure_limit_zero_test() ->
    ?assertEqual(0, ensure_limit(0)).

ensure_limit_negative_test() ->
    ?assertEqual(0, ensure_limit(-1)).

ensure_limit_non_integer_test() ->
    ?assertEqual(0, ensure_limit(<<"10">>)).

ensure_limit_clamped_test() ->
    ?assertEqual(?MAX_MEMBER_QUERY_LIMIT, ensure_limit(?MAX_MEMBER_QUERY_LIMIT + 1)).

resolve_member_limit_full_scan_test() ->
    ?assertEqual(?FULL_MEMBER_LIST_LIMIT, resolve_member_limit(<<>>, 0)).

resolve_member_limit_query_default_test() ->
    ?assertEqual(?DEFAULT_QUERY_LIMIT, resolve_member_limit(<<"ab">>, 0)).

resolve_member_limit_explicit_test() ->
    ?assertEqual(25, resolve_member_limit(<<"ab">>, 25)).

check_request_rate_limit_allows_initial_request_test() ->
    UserId = 987654321,
    clear_request_rate_limit(UserId),
    ?assertEqual(ok, check_request_rate_limit(UserId)),
    clear_request_rate_limit(UserId).

check_request_rate_limit_blocks_burst_test() ->
    UserId = 987654322,
    clear_request_rate_limit(UserId),
    ensure_request_rate_limit_table(),
    Now = erlang:system_time(millisecond),
    Timestamps = lists:duplicate(?REQUEST_MEMBERS_RATE_LIMIT_MAX_EVENTS, Now - 1000),
    ets:insert(?REQUEST_MEMBERS_RATE_LIMIT_TABLE, {UserId, Timestamps}),
    ?assertEqual({error, rate_limited}, check_request_rate_limit(UserId)),
    clear_request_rate_limit(UserId).

check_request_rate_limit_invalid_user_test() ->
    ?assertEqual({error, invalid_session}, check_request_rate_limit(undefined)).

check_guild_request_rate_limit_allows_initial_request_test() ->
    GuildId = 87654321,
    clear_guild_request_rate_limit(GuildId),
    ?assertEqual(ok, check_guild_request_rate_limit(GuildId)),
    clear_guild_request_rate_limit(GuildId).

check_guild_request_rate_limit_blocks_burst_test() ->
    GuildId = 87654322,
    clear_guild_request_rate_limit(GuildId),
    ensure_guild_request_rate_limit_table(),
    Now = erlang:system_time(millisecond),
    Timestamps = lists:duplicate(?REQUEST_MEMBERS_GUILD_RATE_LIMIT_MAX_EVENTS, Now - 1000),
    ets:insert(?REQUEST_MEMBERS_GUILD_RATE_LIMIT_TABLE, {GuildId, Timestamps}),
    ?assertEqual({error, rate_limited}, check_guild_request_rate_limit(GuildId)),
    clear_guild_request_rate_limit(GuildId).

check_guild_request_rate_limit_invalid_guild_test() ->
    ?assertEqual({error, invalid_guild_id}, check_guild_request_rate_limit(undefined)).

clear_request_rate_limit(UserId) ->
    ensure_request_rate_limit_table(),
    ets:delete(?REQUEST_MEMBERS_RATE_LIMIT_TABLE, UserId).

clear_guild_request_rate_limit(GuildId) ->
    ensure_guild_request_rate_limit_table(),
    ets:delete(?REQUEST_MEMBERS_GUILD_RATE_LIMIT_TABLE, GuildId).

validate_guild_id_integer_test() ->
    ?assertEqual({ok, 123}, validate_guild_id(123)).

validate_guild_id_zero_test() ->
    ?assertEqual({error, invalid_guild_id}, validate_guild_id(0)).

validate_guild_id_negative_test() ->
    ?assertEqual({error, invalid_guild_id}, validate_guild_id(-1)).

validate_guild_id_atom_test() ->
    ?assertEqual({error, invalid_guild_id}, validate_guild_id(undefined)).

build_chunk_data_basic_test() ->
    Members = [#{<<"user">> => #{<<"id">> => <<"1">>}}],
    Result = build_chunk_data(Members, [], 0, 1, null),
    ?assertEqual(Members, maps:get(<<"members">>, Result)),
    ?assertEqual(0, maps:get(<<"chunk_index">>, Result)),
    ?assertEqual(1, maps:get(<<"chunk_count">>, Result)),
    ?assertNot(maps:is_key(<<"presences">>, Result)),
    ?assertNot(maps:is_key(<<"nonce">>, Result)).

build_chunk_data_with_presences_test() ->
    Members = [#{<<"user">> => #{<<"id">> => <<"1">>}}],
    Presences = [#{<<"user">> => #{<<"id">> => <<"1">>}, <<"status">> => <<"online">>}],
    Result = build_chunk_data(Members, Presences, 0, 1, null),
    ?assertEqual(Presences, maps:get(<<"presences">>, Result)),
    ?assertNot(maps:is_key(<<"nonce">>, Result)).

build_chunk_data_with_nonce_test() ->
    Members = [],
    Result = build_chunk_data(Members, [], 0, 1, <<"my_nonce">>),
    ?assertEqual(<<"my_nonce">>, maps:get(<<"nonce">>, Result)).

build_chunk_data_with_presences_and_nonce_test() ->
    Members = [#{<<"user">> => #{<<"id">> => <<"1">>}}],
    Presences = [#{<<"user">> => #{<<"id">> => <<"1">>}, <<"status">> => <<"online">>}],
    Result = build_chunk_data(Members, Presences, 2, 5, <<"nonce1">>),
    ?assertEqual(Presences, maps:get(<<"presences">>, Result)),
    ?assertEqual(<<"nonce1">>, maps:get(<<"nonce">>, Result)),
    ?assertEqual(2, maps:get(<<"chunk_index">>, Result)),
    ?assertEqual(5, maps:get(<<"chunk_count">>, Result)).

chunk_presences_aligns_with_member_chunks_test() ->
    Members1 = [
        #{<<"user">> => #{<<"id">> => <<"1">>}},
        #{<<"user">> => #{<<"id">> => <<"2">>}}
    ],
    Members2 = [
        #{<<"user">> => #{<<"id">> => <<"3">>}}
    ],
    Presences = [
        #{<<"user">> => #{<<"id">> => <<"1">>}, <<"status">> => <<"online">>},
        #{<<"user">> => #{<<"id">> => <<"3">>}, <<"status">> => <<"idle">>}
    ],
    Result = chunk_presences(Presences, [Members1, Members2]),
    ?assertEqual(2, length(Result)),
    [P1, P2] = Result,
    ?assertEqual(1, length(P1)),
    ?assertEqual(1, length(P2)).

chunk_presences_empty_presences_test() ->
    Members = [#{<<"user">> => #{<<"id">> => <<"1">>}}],
    Result = chunk_presences([], [Members]),
    ?assertEqual([[]], Result).

chunk_presences_no_matching_presences_test() ->
    Members = [#{<<"user">> => #{<<"id">> => <<"1">>}}],
    Presences = [#{<<"user">> => #{<<"id">> => <<"999">>}, <<"status">> => <<"online">>}],
    Result = chunk_presences(Presences, [Members]),
    ?assertEqual([[]], Result).

filter_members_by_query_case_insensitive_test() ->
    Members = [
        #{<<"user">> => #{<<"id">> => <<"1">>, <<"username">> => <<"Alice">>}},
        #{<<"user">> => #{<<"id">> => <<"2">>, <<"username">> => <<"bob">>}}
    ],
    Results = filter_members_by_query(Members, <<"ALICE">>, 10),
    ?assertEqual(1, length(Results)).

filter_members_by_query_respects_limit_test() ->
    Members = [
        #{<<"user">> => #{<<"id">> => <<"1">>, <<"username">> => <<"alice1">>}},
        #{<<"user">> => #{<<"id">> => <<"2">>, <<"username">> => <<"alice2">>}},
        #{<<"user">> => #{<<"id">> => <<"3">>, <<"username">> => <<"alice3">>}}
    ],
    Results = filter_members_by_query(Members, <<"alice">>, 2),
    ?assertEqual(2, length(Results)).

filter_members_by_query_empty_query_matches_all_test() ->
    Members = [
        #{<<"user">> => #{<<"id">> => <<"1">>, <<"username">> => <<"alice">>}},
        #{<<"user">> => #{<<"id">> => <<"2">>, <<"username">> => <<"bob">>}}
    ],
    Results = filter_members_by_query(Members, <<>>, 10),
    ?assertEqual(2, length(Results)).

filter_members_by_query_no_match_test() ->
    Members = [
        #{<<"user">> => #{<<"id">> => <<"1">>, <<"username">> => <<"alice">>}}
    ],
    Results = filter_members_by_query(Members, <<"zzz">>, 10),
    ?assertEqual(0, length(Results)).

filter_members_by_query_matches_nick_test() ->
    Members = [
        #{
            <<"user">> => #{<<"id">> => <<"1">>, <<"username">> => <<"alice">>},
            <<"nick">> => <<"SuperNick">>
        }
    ],
    Results = filter_members_by_query(Members, <<"super">>, 10),
    ?assertEqual(1, length(Results)).

get_display_name_null_nick_test() ->
    Member = #{
        <<"user">> => #{<<"username">> => <<"user">>},
        <<"nick">> => null
    },
    ?assertEqual(<<"user">>, get_display_name(Member)).

get_display_name_non_binary_nick_test() ->
    Member = #{
        <<"user">> => #{<<"username">> => <<"user">>},
        <<"nick">> => 12345
    },
    ?assertEqual(<<"user">>, get_display_name(Member)).

get_display_name_non_map_test() ->
    ?assertEqual(<<>>, get_display_name(not_a_map)).

get_display_name_null_global_name_test() ->
    Member = #{<<"user">> => #{<<"username">> => <<"user">>, <<"global_name">> => null}},
    ?assertEqual(<<"user">>, get_display_name(Member)).

get_display_name_non_binary_global_name_test() ->
    Member = #{<<"user">> => #{<<"username">> => <<"user">>, <<"global_name">> => 12345}},
    ?assertEqual(<<"user">>, get_display_name(Member)).

get_username_null_test() ->
    ?assertEqual(<<>>, get_username(#{<<"username">> => null})).

get_username_undefined_test() ->
    ?assertEqual(<<>>, get_username(#{<<"username">> => undefined})).

get_username_non_binary_test() ->
    ?assertEqual(<<>>, get_username(#{<<"username">> => 12345})).

get_username_missing_test() ->
    ?assertEqual(<<>>, get_username(#{})).

extract_user_id_valid_test() ->
    ?assertEqual(42, extract_user_id(#{<<"user">> => #{<<"id">> => <<"42">>}})).

extract_user_id_missing_user_test() ->
    ?assertEqual(undefined, extract_user_id(#{})).

extract_user_id_non_map_test() ->
    ?assertEqual(undefined, extract_user_id(not_a_map)).

presence_visible_online_test() ->
    ?assertEqual(true, presence_visible(#{<<"status">> => <<"online">>})).

presence_visible_idle_test() ->
    ?assertEqual(true, presence_visible(#{<<"status">> => <<"idle">>})).

presence_visible_dnd_test() ->
    ?assertEqual(true, presence_visible(#{<<"status">> => <<"dnd">>})).

presence_visible_offline_test() ->
    ?assertEqual(false, presence_visible(#{<<"status">> => <<"offline">>})).

presence_visible_invisible_test() ->
    ?assertEqual(false, presence_visible(#{<<"status">> => <<"invisible">>})).

presence_visible_missing_status_test() ->
    ?assertEqual(false, presence_visible(#{})).

normalize_nonce_exactly_max_length_test() ->
    Nonce = list_to_binary(lists:duplicate(32, $a)),
    ?assertEqual(Nonce, normalize_nonce(Nonce)).

normalize_nonce_one_over_max_test() ->
    Nonce = list_to_binary(lists:duplicate(33, $a)),
    ?assertEqual(null, normalize_nonce(Nonce)).

normalize_nonce_empty_binary_test() ->
    ?assertEqual(<<>>, normalize_nonce(<<>>)).

normalize_nonce_integer_test() ->
    ?assertEqual(null, normalize_nonce(42)).

normalize_nonce_null_atom_test() ->
    ?assertEqual(null, normalize_nonce(null)).

parse_request_defaults_test() ->
    Data = #{<<"guild_id">> => 12345},
    {ok, Request} = parse_request(Data),
    ?assertEqual(12345, maps:get(guild_id, Request)),
    ?assertEqual(<<>>, maps:get(query, Request)),
    ?assertEqual(0, maps:get(limit, Request)),
    ?assertEqual([], maps:get(user_ids, Request)),
    ?assertEqual(false, maps:get(presences, Request)),
    ?assertEqual(null, maps:get(nonce, Request)).

parse_request_non_binary_query_test() ->
    Data = #{<<"guild_id">> => 123, <<"query">> => 42},
    {ok, Request} = parse_request(Data),
    ?assertEqual(<<>>, maps:get(query, Request)).

parse_request_negative_limit_test() ->
    Data = #{<<"guild_id">> => 123, <<"limit">> => -5},
    {ok, Request} = parse_request(Data),
    ?assertEqual(0, maps:get(limit, Request)).

parse_request_presences_not_true_test() ->
    Data = #{<<"guild_id">> => 123, <<"presences">> => <<"yes">>},
    {ok, Request} = parse_request(Data),
    ?assertEqual(false, maps:get(presences, Request)).

parse_request_missing_guild_id_test() ->
    Data = #{<<"query">> => <<"test">>},
    ?assertEqual({error, invalid_guild_id}, parse_request(Data)).

handle_request_invalid_data_test() ->
    ?assertEqual({error, invalid_request}, handle_request(not_a_map, self(), #{})).

chunk_list_single_element_test() ->
    ?assertEqual([[1]], chunk_list([1], 1)).

chunk_list_exact_multiple_test() ->
    ?assertEqual([[1, 2], [3, 4]], chunk_list([1, 2, 3, 4], 2)).

chunk_list_large_size_test() ->
    ?assertEqual([[1, 2, 3]], chunk_list([1, 2, 3], 1000)).

-endif.
