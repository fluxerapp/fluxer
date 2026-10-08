%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_request_members).
-typing([eqwalizer]).

-export([
    handle_request/3,
    parse_request/1,
    validate_guild_id/1,
    normalize_nonce/1,
    validate_user_ids/1,
    parse_user_id/1,
    ensure_binary/1,
    ensure_limit/1
]).

-export_type([session_state/0, request_data/0]).

-define(MAX_USER_IDS, 100).
-define(MAX_NONCE_LENGTH, 32).
-define(MAX_MEMBER_QUERY_LIMIT, 100).

-type session_state() :: map().
-type request_data() :: map().

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
    GuildIdsRaw = maps:get(<<"guild_ids">>, Data, undefined),
    Query = maps:get(<<"query">>, Data, <<>>),
    Limit = maps:get(<<"limit">>, Data, 0),
    UserIdsRaw = maps:get(<<"user_ids">>, Data, []),
    Presences = maps:get(<<"presences">>, Data, false),
    Nonce = maps:get(<<"nonce">>, Data, null),
    NormalizedNonce = normalize_nonce(Nonce),
    case validate_request_guild_ids(GuildIdRaw, GuildIdsRaw) of
        {ok, GuildIds} ->
            build_request(GuildIds, Query, Limit, UserIdsRaw, Presences, NormalizedNonce);
        {error, Reason} ->
            {error, Reason}
    end.

-spec build_request(
    [integer()], term(), term(), term(), term(), binary() | null
) -> {ok, map()} | {error, atom()}.
build_request(GuildIds, Query, Limit, UserIdsRaw, Presences, NormalizedNonce) ->
    RequestNonce = resolve_nonce(GuildIds, NormalizedNonce),
    case validate_user_ids(UserIdsRaw) of
        {ok, UserIds} ->
            {ok, #{
                guild_ids => GuildIds,
                query => ensure_binary(Query),
                limit => ensure_limit(Limit),
                user_ids => UserIds,
                presences => truthy(Presences),
                nonce => RequestNonce
            }};
        {error, Reason} ->
            {error, Reason}
    end.

-spec resolve_nonce([integer()], binary() | null) -> binary() | null.
resolve_nonce([_SingleGuildId], Nonce) -> Nonce;
resolve_nonce(_, _Nonce) -> null.

-spec validate_request_guild_ids(term(), term()) -> {ok, [integer()]} | {error, atom()}.
validate_request_guild_ids(_GuildIdRaw, GuildIdsRaw) when
    is_list(GuildIdsRaw), GuildIdsRaw =/= []
->
    validate_guild_ids_list(GuildIdsRaw);
validate_request_guild_ids(GuildIdRaw, _) ->
    case validate_guild_id(GuildIdRaw) of
        {ok, GuildId} -> {ok, [GuildId]};
        {error, Reason} -> {error, Reason}
    end.

-spec validate_guild_ids_list([term()]) -> {ok, [integer()]} | {error, atom()}.
validate_guild_ids_list(GuildIds) ->
    validate_guild_ids_list(GuildIds, #{}, []).

-spec validate_guild_ids_list([term()], map(), [integer()]) ->
    {ok, [integer()]} | {error, atom()}.
validate_guild_ids_list([], _Seen, Acc) ->
    {ok, lists:reverse(Acc)};
validate_guild_ids_list([GuildId | Rest], Seen, Acc) ->
    case validate_guild_id(GuildId) of
        {ok, ParsedGuildId} ->
            dedup_guild_id(ParsedGuildId, Rest, Seen, Acc);
        {error, _} ->
            {error, invalid_guild_id}
    end.

-spec dedup_guild_id(integer(), [term()], map(), [integer()]) ->
    {ok, [integer()]} | {error, atom()}.
dedup_guild_id(ParsedGuildId, Rest, Seen, Acc) ->
    case maps:is_key(ParsedGuildId, Seen) of
        true ->
            validate_guild_ids_list(Rest, Seen, Acc);
        false ->
            validate_guild_ids_list(
                Rest, Seen#{ParsedGuildId => true}, [ParsedGuildId | Acc]
            )
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
    case has_too_many_user_ids(UserIds) of
        true -> {error, too_many_user_ids};
        false -> {ok, parse_user_ids(UserIds)}
    end;
validate_user_ids(_) ->
    {ok, []}.

-spec has_too_many_user_ids([term()]) -> boolean().
has_too_many_user_ids(UserIds) ->
    length(UserIds) > ?MAX_USER_IDS.

-spec parse_user_ids([term()]) -> [integer()].
parse_user_ids(UserIds) ->
    lists:filtermap(
        fun parse_user_id_filter/1,
        UserIds
    ).

-spec parse_user_id_filter(term()) -> {true, integer()} | false.
parse_user_id_filter(Id) ->
    case parse_user_id(Id) of
        {ok, ParsedId} -> {true, ParsedId};
        error -> false
    end.

-spec parse_user_id(term()) -> {ok, integer()} | error.
parse_user_id(Id) when is_integer(Id), Id > 0 ->
    {ok, Id};
parse_user_id(Id) when is_binary(Id) ->
    case validation:validate_snowflake(<<"user_id">>, Id) of
        {ok, ParsedId} -> {ok, ParsedId};
        {error, _, _} -> error
    end;
parse_user_id(_) ->
    error.

-spec ensure_binary(term()) -> binary().
ensure_binary(Value) when is_binary(Value) -> Value;
ensure_binary(_) -> <<>>.

-spec ensure_limit(term()) -> non_neg_integer().
ensure_limit(Limit) when is_integer(Limit), Limit >= 0 ->
    min(Limit, ?MAX_MEMBER_QUERY_LIMIT);
ensure_limit(_) ->
    0.

-spec normalize_nonce(term()) -> binary() | null.
normalize_nonce(Nonce) when is_binary(Nonce), byte_size(Nonce) =< ?MAX_NONCE_LENGTH ->
    Nonce;
normalize_nonce(_) ->
    null.

-spec process_request(map(), pid(), session_state()) -> ok | {error, atom()}.
process_request(Request, SocketPid, SessionState) ->
    UserId = snowflake_id:parse_optional(maps:get(user_id, SessionState)),
    case guild_request_members_filter:check_request_rate_limit(UserId) of
        ok -> process_admitted_request(Request, UserId, SocketPid, SessionState);
        {error, Reason} -> {error, Reason}
    end.

-spec process_admitted_request(map(), integer() | undefined, pid(), session_state()) ->
    ok | {error, atom()}.
process_admitted_request(Request, UserId, SocketPid, SessionState) ->
    #{guild_ids := ReqGuildIds, query := Query, limit := Limit, user_ids := UserIds} = Request,
    IsBot = truthy(maps:get(bot, SessionState, false)),
    case guild_request_members_filter:enforce_single_guild_for_bots(IsBot, ReqGuildIds) of
        ok ->
            Connected = filter_connected_guild_ids(ReqGuildIds, SessionState),
            dispatch_guild_requests(
                Connected,
                UserId,
                IsBot,
                Query,
                Limit,
                UserIds,
                Request,
                SocketPid,
                SessionState
            );
        {error, Reason} ->
            {error, Reason}
    end.

-spec filter_connected_guild_ids([integer()], session_state()) -> [integer()].
filter_connected_guild_ids(GuildIds, SessionState) ->
    lists:filter(
        fun(GId) ->
            session_guild_connected(GId, SessionState)
        end,
        GuildIds
    ).

-spec session_guild_connected(integer(), session_state()) -> boolean().
session_guild_connected(GuildId, SessionState) ->
    case guild_request_members_filter:lookup_session_guild(GuildId, SessionState) of
        {ok, _} -> true;
        {error, _} -> false
    end.

-spec dispatch_guild_requests(
    [integer()],
    integer() | undefined,
    boolean(),
    binary(),
    non_neg_integer(),
    [integer()],
    map(),
    pid(),
    session_state()
) -> ok | {error, atom()}.
dispatch_guild_requests([], _, _, _, _, _, _, _, _) ->
    {error, guild_not_found};
dispatch_guild_requests([GId], UID, Bot, Q, L, UI, Req, SP, SS) ->
    process_single_guild(GId, UID, Bot, Q, L, UI, Req, SP, SS);
dispatch_guild_requests(GIds, UID, Bot, Q, L, UI, Req, SP, SS) ->
    process_multi_guild(GIds, UID, Bot, Q, L, UI, Req, SP, SS).

-spec process_single_guild(
    integer(),
    integer() | undefined,
    boolean(),
    binary(),
    non_neg_integer(),
    [integer()],
    map(),
    pid(),
    session_state()
) -> ok | {error, atom()}.
process_single_guild(GId, UID, Bot, Q, L, UI, Req, _SP, SS) ->
    case guild_request_members_filter:check_guild_request_rate_limit(GId) of
        ok -> process_admitted_guild(GId, UID, Bot, Q, L, UI, Req, SS);
        {error, Reason} -> {error, Reason}
    end.

-spec process_admitted_guild(
    integer(),
    integer() | undefined,
    boolean(),
    binary(),
    non_neg_integer(),
    [integer()],
    map(),
    session_state()
) -> ok | {error, atom()}.
process_admitted_guild(GId, UID, Bot, Q, L, UI, Req, SS) ->
    IsFull = guild_request_members_filter:is_full_member_list(Q, L, UI),
    case guild_request_members_filter:check_full_list_bot_rate_limit(IsFull, Bot, UID, GId) of
        ok ->
            check_perm_and_send(GId, UID, Bot, IsFull, Req, SS);
        {rate_limited, Ms} ->
            guild_request_members_filter:dispatch_full_list_rate_limited(SS, GId, Req, Ms),
            {error, rate_limited}
    end.

-spec check_perm_and_send(
    integer(), integer() | undefined, boolean(), boolean(), map(), session_state()
) -> ok | {error, atom()}.
check_perm_and_send(GId, UID, Bot, IsFull, Req, SS) when is_integer(UID) ->
    case guild_request_members_filter:check_permission(UID, Bot, GId, IsFull, SS) of
        ok -> fetch_and_send(GId, Req, SS);
        {error, Reason} -> {error, Reason}
    end;
check_perm_and_send(_GId, _UID, _Bot, _IsFull, _Req, _SS) ->
    {error, invalid_user}.

-spec process_multi_guild(
    [integer()],
    integer() | undefined,
    boolean(),
    binary(),
    non_neg_integer(),
    [integer()],
    map(),
    pid(),
    session_state()
) -> ok | {error, atom()}.
process_multi_guild(GIds, UID, Bot, Q, L, UI, Req, _SP, SS) ->
    ProcessedAny = lists:foldl(
        fun(GId, Acc) ->
            try_guild_or_keep_processed(GId, UID, Bot, Q, L, UI, Req, SS, Acc)
        end,
        false,
        GIds
    ),
    case ProcessedAny of
        true -> ok;
        false -> {error, guild_not_found}
    end.

-spec try_guild_or_keep_processed(
    integer(),
    integer() | undefined,
    boolean(),
    binary(),
    non_neg_integer(),
    [integer()],
    map(),
    session_state(),
    boolean()
) -> boolean().
try_guild_or_keep_processed(GId, UID, Bot, Q, L, UI, Req, SS, Acc) ->
    case try_guild(GId, UID, Bot, Q, L, UI, Req, SS) of
        true -> true;
        false -> Acc
    end.

-spec try_guild(
    integer(),
    integer() | undefined,
    boolean(),
    binary(),
    non_neg_integer(),
    [integer()],
    map(),
    session_state()
) -> boolean().
try_guild(GId, UID, Bot, Q, L, UI, Req, SS) ->
    case guild_request_members_filter:check_guild_request_rate_limit(GId) of
        ok -> try_admitted_guild(GId, UID, Bot, Q, L, UI, Req, SS);
        {error, _Reason} -> false
    end.

-spec try_admitted_guild(
    integer(),
    integer() | undefined,
    boolean(),
    binary(),
    non_neg_integer(),
    [integer()],
    map(),
    session_state()
) -> boolean().
try_admitted_guild(GId, UID, Bot, Q, L, UI, Req, SS) ->
    IsFull = guild_request_members_filter:is_full_member_list(Q, L, UI),
    case guild_request_members_filter:check_full_list_bot_rate_limit(IsFull, Bot, UID, GId) of
        ok ->
            try_guild_with_perm(GId, UID, Bot, IsFull, Req, SS);
        {rate_limited, Ms} ->
            guild_request_members_filter:dispatch_full_list_rate_limited(SS, GId, Req, Ms),
            false
    end.

-spec try_guild_with_perm(
    integer(), integer() | undefined, boolean(), boolean(), map(), session_state()
) -> boolean().
try_guild_with_perm(GId, UID, Bot, IsFull, Req, SS) when is_integer(UID) ->
    case guild_request_members_filter:check_permission(UID, Bot, GId, IsFull, SS) of
        ok ->
            fetch_succeeded(GId, Req, SS);
        {error, _} ->
            false
    end;
try_guild_with_perm(_GId, _UID, _Bot, _IsFull, _Req, _SS) ->
    false.

-spec fetch_succeeded(integer(), map(), session_state()) -> boolean().
fetch_succeeded(GId, Req, SS) ->
    case fetch_and_send(GId, Req, SS) of
        ok -> true;
        {error, _} -> false
    end.

-spec fetch_and_send(integer(), map(), session_state()) -> ok | {error, atom()}.
fetch_and_send(GuildId, Request, SessionState) ->
    #{query := Q, limit := L, user_ids := UI, presences := P, nonce := N} = Request,
    SessionPid = maps:get(session_pid, SessionState, undefined),
    case guild_request_members_filter:lookup_session_guild(GuildId, SessionState) of
        {ok, GuildPid} ->
            {Members, Presences} = guild_request_members_search:fetch_members_with_rollout(
                GuildId, GuildPid, Q, L, UI, P
            ),
            guild_request_members_chunk:send_member_chunks(
                GuildId, SessionPid, Members, Presences, N
            ),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

-spec truthy(term()) -> boolean().
truthy(true) -> true;
truthy(_) -> false.
