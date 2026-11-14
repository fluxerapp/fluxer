%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_rpc_guild_mentions).

-typing([eqwalizer]).

-export([handle/2]).

-define(GUILD_CALL_TIMEOUT, 4000).
-define(MENTION_SOURCE_PAGE_DEFAULT_LIMIT, 1000).
-define(MENTION_SOURCE_PAGE_MAX_LIMIT, 5000).

-spec handle(binary(), map()) -> term().
handle(<<"guild.resolve_mention_sources_page">>, P) -> handle_resolve_sources_page(P).

-spec handle_resolve_sources_page(term()) -> term().
handle_resolve_sources_page(Params) ->
    case parse_page_params(Params) of
        {ok, GuildId, Request} ->
            do_resolve_sources_page(GuildId, Request);
        error ->
            ErrBin = <<"resolve_mention_sources_page_validation_error">>,
            gateway_rpc_error:raise(ErrBin)
    end.

-spec do_resolve_sources_page(integer(), map()) -> term().
do_resolve_sources_page(GuildId, Request) ->
    gateway_rpc_guild_infra:with_guild(GuildId, fun(Pid) ->
        guild_call_sources_page(Pid, Request)
    end).

-spec guild_call_sources_page(pid(), map()) -> map().
guild_call_sources_page(Pid, Request) ->
    Msg = {resolve_mention_sources_page, Request},
    case eqwalizer:dynamic_cast(guild_query_handler:call(Pid, Msg, ?GUILD_CALL_TIMEOUT)) of
        #{mentions := Mentions, next_cursor := NextCursor} ->
            #{
                <<"mentions">> => fmt_mention_entries(Mentions),
                <<"next_cursor">> => fmt_cursor(NextCursor)
            };
        _ ->
            ErrBin = <<"resolve_mention_sources_page_error">>,
            gateway_rpc_error:raise(ErrBin)
    end.

-spec fmt_mention_entries([map()]) -> [map()].
fmt_mention_entries(Entries) ->
    [fmt_mention_entry(E) || E <- Entries].

-spec fmt_mention_entry(map()) -> map().
fmt_mention_entry(E) ->
    #{
        <<"user_id">> => integer_to_binary(maps:get(user_id, E)),
        <<"direct">> => maps:get(direct, E, false),
        <<"role">> => maps:get(role, E, false),
        <<"everyone">> => maps:get(everyone, E, false)
    }.

-spec fmt_cursor(integer() | undefined) -> binary() | null.
fmt_cursor(undefined) -> null;
fmt_cursor(C) -> integer_to_binary(C).

-spec parse_page_params(term()) -> {ok, integer(), map()} | error.
parse_page_params(Params) when is_map(Params) ->
    RequiredIds = {
        validate_id(<<"guild_id">>, Params),
        validate_id(<<"channel_id">>, Params),
        validate_id(<<"author_id">>, Params)
    },
    ParsedOptionalIds = {
        parse_snowflake_list(maps:get(<<"role_ids">>, Params, [])),
        parse_snowflake_list(maps:get(<<"user_ids">>, Params, [])),
        parse_opt_cursor(maps:get(<<"cursor">>, Params, undefined))
    },
    build_page_request(RequiredIds, ParsedOptionalIds, Params);
parse_page_params(_) ->
    error.

-spec validate_id(binary(), map()) -> {ok, pos_integer()} | {error, atom(), atom()}.
validate_id(Key, Params) ->
    validation:validate_snowflake(Key, maps:get(Key, Params, undefined)).

-spec build_page_request(
    {
        {ok, pos_integer()} | {error, atom(), atom()},
        {ok, pos_integer()} | {error, atom(), atom()},
        {ok, pos_integer()} | {error, atom(), atom()}
    },
    {
        {ok, [integer()]} | error,
        {ok, [integer()]} | error,
        {ok, integer() | undefined} | error
    },
    map()
) -> {ok, integer(), map()} | error.
build_page_request(
    {{ok, GId}, {ok, CId}, {ok, AId}},
    {{ok, RoleIds}, {ok, UserIds}, {ok, Cursor}},
    Params
) ->
    Limit = clamp_page_limit(
        maps:get(<<"limit">>, Params, ?MENTION_SOURCE_PAGE_DEFAULT_LIMIT)
    ),
    GuildId = GId,
    {ok, GuildId, #{
        channel_id => CId,
        author_id => AId,
        mention_everyone => norm_bool(maps:get(<<"mention_everyone">>, Params, false)),
        mention_here => norm_bool(maps:get(<<"mention_here">>, Params, false)),
        role_ids => RoleIds,
        user_ids => UserIds,
        limit => Limit,
        cursor => Cursor
    }};
build_page_request(_, _, _) ->
    error.

-spec norm_bool(term()) -> boolean().
norm_bool(true) -> true;
norm_bool(<<"true">>) -> true;
norm_bool(1) -> true;
norm_bool(_) -> false.

-spec parse_snowflake_list(term()) -> {ok, [integer()]} | error.
parse_snowflake_list(Vs) when is_list(Vs) ->
    case validation:validate_snowflake_list(Vs) of
        {ok, Ids} -> {ok, Ids};
        _ -> error
    end;
parse_snowflake_list(_) ->
    error.

-spec parse_opt_cursor(term()) -> {ok, integer() | undefined} | error.
parse_opt_cursor(undefined) ->
    {ok, undefined};
parse_opt_cursor(null) ->
    {ok, undefined};
parse_opt_cursor(V) ->
    case validation:validate_snowflake(<<"cursor">>, V) of
        {ok, Id} -> {ok, Id};
        _ -> error
    end.

-spec clamp_page_limit(term()) -> pos_integer().
clamp_page_limit(V) ->
    case type_conv:to_integer(V) of
        undefined ->
            ?MENTION_SOURCE_PAGE_DEFAULT_LIMIT;
        N when N < 1 -> 1;
        N when N > ?MENTION_SOURCE_PAGE_MAX_LIMIT ->
            ?MENTION_SOURCE_PAGE_MAX_LIMIT;
        N ->
            N
    end.
