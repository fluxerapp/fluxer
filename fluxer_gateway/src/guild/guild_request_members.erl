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

-type session_state() :: map().
-type request_data() :: map().

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec handle_request(request_data(), pid(), session_state()) -> ok | {error, atom()}.
handle_request(Data, SocketPid, SessionState) when is_map(Data), is_pid(SocketPid) ->
    logger:debug("[guild_request_members] Handling guild members request: ~p", [Data]),
    case parse_request(Data) of
        {ok, Request} ->
            logger:debug("[guild_request_members] Request parsed successfully: ~p", [Request]),
            process_request(Request, SocketPid, SessionState);
        {error, Reason} ->
            logger:warning("[guild_request_members] Failed to parse request: ~p", [Reason]),
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
ensure_limit(Limit) when is_integer(Limit), Limit >= 0 -> Limit;
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

    logger:debug(
        "[guild_request_members] Processing request for guild ~p, user ~p, user_ids: ~p",
        [GuildId, UserId, UserIds]
    ),

    case check_permission(UserId, GuildId, Query, Limit, UserIds, SessionState) of
        ok ->
            logger:debug("[guild_request_members] Permission check passed, fetching members"),
            fetch_and_send_members(Request, SocketPid, SessionState);
        {error, Reason} ->
            logger:warning(
                "[guild_request_members] Permission check failed: ~p",
                [Reason]
            ),
            {error, Reason}
    end.

-spec check_permission(integer(), integer(), binary(), non_neg_integer(), [integer()], session_state()) ->
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
            case gen_server:call(guild_manager, {lookup, GuildId}, 5000) of
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

    logger:debug(
        "[guild_request_members] Looking up guild ~p for member request",
        [GuildId]
    ),

    case lookup_guild(GuildId, SessionState) of
        {ok, GuildPid} ->
            logger:debug("[guild_request_members] Guild ~p found, fetching members", [GuildId]),
            Members = fetch_members(GuildPid, Query, Limit, UserIds),
            logger:debug("[guild_request_members] Found ~p members", [length(Members)]),
            PresencesList = maybe_fetch_presences(Presences, GuildPid, Members),
            send_member_chunks(GuildPid, SessionId, Members, PresencesList, Nonce),
            logger:debug(
                "[guild_request_members] Sent ~p member chunks for guild ~p with nonce ~p",
                [max(1, (length(Members) + ?CHUNK_SIZE - 1) div ?CHUNK_SIZE), GuildId, Nonce]
            ),
            ok;
        {error, Reason} ->
            logger:warning(
                "[guild_request_members] Failed to lookup guild ~p: ~p",
                [GuildId, Reason]
            ),
            {error, Reason}
    end.

-spec fetch_members(pid(), binary(), non_neg_integer(), [integer()]) -> [map()].
fetch_members(GuildPid, _Query, _Limit, UserIds) when UserIds =/= [] ->
    logger:debug("[guild_request_members] Fetching members by user_ids: ~p", [UserIds]),
    case gen_server:call(GuildPid, {list_guild_members, #{limit => 100000, offset => 0}}, 10000) of
        #{members := AllMembers} ->
            logger:debug("[guild_request_members] Got ~p members from guild, filtering by user_ids", [length(AllMembers)]),
            Filtered = filter_members_by_ids(AllMembers, UserIds),
            logger:debug("[guild_request_members] Filtered to ~p members", [length(Filtered)]),
            Filtered;
        Other ->
            logger:warning("[guild_request_members] Unexpected response from guild: ~p", [Other]),
            []
    end;
fetch_members(GuildPid, Query, Limit, []) ->
    ActualLimit = case Limit of 0 -> 100000; L -> L end,
    logger:debug("[guild_request_members] Fetching members with query '~s', limit ~p", [Query, ActualLimit]),
    case gen_server:call(GuildPid, {list_guild_members, #{limit => ActualLimit, offset => 0}}, 10000) of
        #{members := AllMembers} ->
            logger:debug("[guild_request_members] Got ~p members from guild", [length(AllMembers)]),
            Result = case Query of
                <<>> ->
                    lists:sublist(AllMembers, ActualLimit);
                _ ->
                    filter_members_by_query(AllMembers, Query, ActualLimit)
            end,
            logger:debug("[guild_request_members] Returning ~p members after query/filter", [length(Result)]),
            Result;
        Other ->
            logger:warning("[guild_request_members] Unexpected response from guild: ~p", [Other]),
            []
    end.

-spec filter_members_by_ids([map()], [integer()]) -> [map()].
filter_members_by_ids(Members, UserIds) ->
    UserIdSet = sets:from_list(UserIds),
    lists:filter(
        fun(Member) ->
            UserId = extract_user_id(Member),
            UserId =/= undefined andalso sets:is_element(UserId, UserIdSet)
        end,
        Members
    ).

-spec filter_members_by_query([map()], binary(), non_neg_integer()) -> [map()].
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

-spec get_display_name(map()) -> binary().
get_display_name(Member) when is_map(Member) ->
    Nick = maps:get(<<"nick">>, Member, undefined),
    case Nick of
        undefined -> nick_isundefined(Member);
        null -> nick_isundefined(Member);
        _ when is_binary(Nick) -> Nick;
        _ -> nick_isundefined(Member)
    end;
get_display_name(_) ->
    <<>>.

nick_isundefined(Member) ->
    User = maps:get(<<"user">>, Member, #{}),
    GlobalName = maps:get(<<"global_name">>, User, undefined),
    case GlobalName of
        undefined ->
            Username = maps:get(<<"username">>, User, <<>>),
            case Username of
                null -> <<>>;
                undefined -> <<>>;
                _ when is_binary(Username) -> Username;
                _ -> <<>>
            end;
        null ->
            Username = maps:get(<<"username">>, User, <<>>),
            case Username of
                null -> <<>>;
                undefined -> <<>>;
                _ when is_binary(Username) -> Username;
                _ -> <<>>
            end;
        _ when is_binary(GlobalName) -> GlobalName;
        _ -> <<>>
    end.

-spec extract_user_id(map()) -> integer() | undefined.
extract_user_id(Member) when is_map(Member) ->
    User = maps:get(<<"user">>, Member, #{}),
    map_utils:get_integer(User, <<"id">>, undefined);
extract_user_id(_) ->
    undefined.

-spec maybe_fetch_presences(boolean(), pid(), [map()]) -> [map()].
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

-spec presence_visible(map()) -> boolean().
presence_visible(P) ->
    Status = maps:get(<<"status">>, P, <<"offline">>),
    Status =/= <<"offline">> andalso Status =/= <<"invisible">>.

-spec send_member_chunks(pid(), binary(), [map()], [map()], term()) -> ok.
send_member_chunks(GuildPid, SessionId, Members, Presences, Nonce) ->
    TotalChunks = max(1, (length(Members) + ?CHUNK_SIZE - 1) div ?CHUNK_SIZE),
    MemberChunks = chunk_list(Members, ?CHUNK_SIZE),
    PresenceChunks = chunk_presences(Presences, MemberChunks),

    logger:debug(
        "[guild_request_members] Sending ~p member chunks (total members: ~p, nonce: ~p)",
        [TotalChunks, length(Members), Nonce]
    ),

    lists:foldl(
        fun({MemberChunk, PresenceChunk}, ChunkIndex) ->
            ChunkData = build_chunk_data(
                MemberChunk, PresenceChunk, ChunkIndex, TotalChunks, Nonce
            ),
            logger:debug(
                "[guild_request_members] Sending chunk ~p/~p with ~p members, nonce: ~p",
                [ChunkIndex + 1, TotalChunks, length(MemberChunk), Nonce]
            ),
            gen_server:cast(GuildPid, {send_members_chunk, SessionId, ChunkData}),
            ChunkIndex + 1
        end,
        0,
        lists:zip(MemberChunks, PresenceChunks)
    ),
    logger:debug("[guild_request_members] All chunks sent successfully"),
    ok.

-spec build_chunk_data([map()], [map()], non_neg_integer(), non_neg_integer(), term()) ->
    map().
build_chunk_data(Members, Presences, ChunkIndex, TotalChunks, Nonce) ->
    Base = #{
        <<"members">> => Members,
        <<"chunk_index">> => ChunkIndex,
        <<"chunk_count">> => TotalChunks
    },
    WithPresences = case Presences of
        [] -> Base;
        _ -> maps:put(<<"presences">>, Presences, Base)
    end,
    WithNonce = case Nonce of
        null -> WithPresences;
        _ -> maps:put(<<"nonce">>, Nonce, WithPresences)
    end,
    WithNonce.

-spec chunk_list([T], pos_integer()) -> [[T]] when T :: term().
chunk_list([], _Size) ->
    [[]];
chunk_list(List, Size) ->
    chunk_list(List, Size, []).

chunk_list([], _Size, Acc) ->
    lists:reverse(Acc);
chunk_list(List, Size, Acc) ->
    {Chunk, Rest} = lists:split(min(Size, length(List)), List),
    chunk_list(Rest, Size, [Chunk | Acc]).

-spec chunk_presences([map()], [[map()]]) -> [[map()]].
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

-endif.
