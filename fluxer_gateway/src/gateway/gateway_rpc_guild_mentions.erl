%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_rpc_guild_mentions).

-typing([eqwalizer]).

-export([handle/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(GUILD_CALL_TIMEOUT, 4000).
-define(MENTION_SOURCE_PAGE_DEFAULT_LIMIT, 1000).
-define(MENTION_SOURCE_PAGE_MAX_LIMIT, 5000).

-spec handle(binary(), map()) -> term().
handle(<<"guild.get_users_to_mention_by_roles">>, P) -> handle_mention_by_roles(P);
handle(<<"guild.get_users_to_mention_by_user_ids">>, P) -> handle_mention_by_user_ids(P);
handle(<<"guild.get_all_users_to_mention">>, P) -> handle_mention_all(P);
handle(<<"guild.resolve_all_mentions">>, P) -> handle_resolve_all(P);
handle(<<"guild.resolve_mention_sources">>, P) -> handle_resolve_sources(P);
handle(<<"guild.resolve_mention_sources_page">>, P) -> handle_resolve_sources_page(P).

-spec handle_mention_by_roles(map()) -> term().
handle_mention_by_roles(
    #{
        <<"guild_id">> := GIB,
        <<"channel_id">> := CIB,
        <<"role_ids">> := RIds,
        <<"author_id">> := AIB
    }
) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GIB),
    ChannelId = validation:snowflake_or_throw(<<"channel_id">>, CIB),
    AuthorId = validation:snowflake_or_throw(<<"author_id">>, AIB),
    RoleIdsList = validation:snowflake_list_or_throw(<<"role_ids">>, RIds),
    Req = #{channel_id => ChannelId, role_ids => RoleIdsList, author_id => AuthorId},
    mention_guild_call(GuildId, {get_users_to_mention_by_roles, Req}, <<"users_error">>).

-spec handle_mention_by_user_ids(map()) -> term().
handle_mention_by_user_ids(
    #{
        <<"guild_id">> := GIB,
        <<"channel_id">> := CIB,
        <<"user_ids">> := UIDs,
        <<"author_id">> := AIB
    }
) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GIB),
    ChannelId = validation:snowflake_or_throw(<<"channel_id">>, CIB),
    AuthorId = validation:snowflake_or_throw(<<"author_id">>, AIB),
    UserIdsList = validation:snowflake_list_or_throw(<<"user_ids">>, UIDs),
    Req = #{channel_id => ChannelId, user_ids => UserIdsList, author_id => AuthorId},
    mention_guild_call(GuildId, {get_users_to_mention_by_user_ids, Req}, <<"users_error">>).

-spec handle_mention_all(map()) -> term().
handle_mention_all(
    #{<<"guild_id">> := GIB, <<"channel_id">> := CIB, <<"author_id">> := AIB}
) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GIB),
    ChannelId = validation:snowflake_or_throw(<<"channel_id">>, CIB),
    AuthorId = validation:snowflake_or_throw(<<"author_id">>, AIB),
    Req = #{channel_id => ChannelId, author_id => AuthorId},
    mention_guild_call(GuildId, {get_all_users_to_mention, Req}, <<"users_error">>).

-spec handle_resolve_all(map()) -> term().
handle_resolve_all(
    #{
        <<"guild_id">> := GIB,
        <<"channel_id">> := CIB,
        <<"author_id">> := AIB,
        <<"mention_everyone">> := ME,
        <<"mention_here">> := MH,
        <<"role_ids">> := RIds,
        <<"user_ids">> := UIDs
    }
) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GIB),
    ChannelId = validation:snowflake_or_throw(<<"channel_id">>, CIB),
    AuthorId = validation:snowflake_or_throw(<<"author_id">>, AIB),
    Req = build_mention_req(ChannelId, AuthorId, ME, MH, RIds, UIDs),
    mention_guild_call(GuildId, {resolve_all_mentions, Req}, <<"resolve_mentions_error">>).

-spec handle_resolve_sources(map()) -> term().
handle_resolve_sources(
    #{
        <<"guild_id">> := GIB,
        <<"channel_id">> := CIB,
        <<"author_id">> := AIB,
        <<"mention_everyone">> := ME,
        <<"mention_here">> := MH,
        <<"role_ids">> := RIds,
        <<"user_ids">> := UIDs
    }
) ->
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GIB),
    ChannelId = validation:snowflake_or_throw(<<"channel_id">>, CIB),
    AuthorId = validation:snowflake_or_throw(<<"author_id">>, AIB),
    Req = build_mention_req(ChannelId, AuthorId, ME, MH, RIds, UIDs),
    do_resolve_sources(GuildId, Req).

-spec do_resolve_sources(integer(), map()) -> term().
do_resolve_sources(GuildId, Req) ->
    gateway_rpc_guild_infra:with_guild(GuildId, fun(Pid) ->
        guild_call_sources(Pid, Req)
    end).

-spec guild_call_sources(pid(), map()) -> map().
guild_call_sources(Pid, Req) ->
    Msg = {resolve_mention_sources, Req},
    case guild_query_handler:call(Pid, Msg, ?GUILD_CALL_TIMEOUT) of
        #{direct_user_ids := D, role_user_ids := R, everyone_user_ids := E} ->
            #{
                <<"direct_user_ids">> => fmt_ids(D),
                <<"role_user_ids">> => fmt_ids(R),
                <<"everyone_user_ids">> => fmt_ids(E)
            };
        _ ->
            gateway_rpc_error:raise(<<"resolve_mention_sources_error">>)
    end.

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
    case guild_query_handler:call(Pid, Msg, ?GUILD_CALL_TIMEOUT) of
        #{mentions := Mentions, next_cursor := NextCursor} ->
            #{
                <<"mentions">> => fmt_mention_entries(Mentions),
                <<"next_cursor">> => fmt_cursor(NextCursor)
            };
        _ ->
            ErrBin = <<"resolve_mention_sources_page_error">>,
            gateway_rpc_error:raise(ErrBin)
    end.

-spec build_mention_req(integer(), integer(), term(), term(), term(), term()) -> map().
build_mention_req(ChannelId, AuthorId, ME, MH, RIds, UIDs) ->
    #{
        channel_id => ChannelId,
        author_id => AuthorId,
        mention_everyone => ME,
        mention_here => MH,
        role_ids => validation:snowflake_list_or_throw(<<"role_ids">>, RIds),
        user_ids => validation:snowflake_list_or_throw(<<"user_ids">>, UIDs)
    }.

-spec mention_guild_call(integer(), {atom(), map()}, binary()) -> term().
mention_guild_call(GuildId, Msg, ErrorBin) ->
    gateway_rpc_guild_infra:with_guild(GuildId, fun(Pid) ->
        guild_call_user_ids(Pid, Msg, ErrorBin)
    end).

-spec guild_call_user_ids(pid(), {atom(), map()}, binary()) -> map().
guild_call_user_ids(Pid, Msg, ErrorBin) ->
    case guild_query_handler:call(Pid, Msg, ?GUILD_CALL_TIMEOUT) of
        #{user_ids := Ids} ->
            #{<<"user_ids">> => [integer_to_binary(U) || U <- Ids]};
        _ ->
            gateway_rpc_error:raise(ErrorBin)
    end.

-spec fmt_ids([integer()]) -> [binary()].
fmt_ids(Ids) -> [integer_to_binary(I) || I <- Ids].

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

-ifdef(TEST).

clamp_page_limit_test() ->
    ?assertEqual(?MENTION_SOURCE_PAGE_DEFAULT_LIMIT, clamp_page_limit(undefined)),
    ?assertEqual(1, clamp_page_limit(0)),
    ?assertEqual(50, clamp_page_limit(50)),
    ?assertEqual(?MENTION_SOURCE_PAGE_MAX_LIMIT, clamp_page_limit(100000)).

resolve_mention_sources_page_malformed_payload_rejected_test() ->
    ?assertError(
        {gateway_rpc_error, <<"resolve_mention_sources_page_validation_error">>},
        handle(<<"guild.resolve_mention_sources_page">>, #{})
    ),
    ?assertError(
        {gateway_rpc_error, <<"resolve_mention_sources_page_validation_error">>},
        handle_resolve_sources_page(null)
    ).

parse_page_params_defaults_optional_fields_test() ->
    Params = #{
        <<"guild_id">> => <<"1">>,
        <<"channel_id">> => <<"2">>,
        <<"author_id">> => <<"3">>
    },
    {ok, 1, Req} = parse_page_params(Params),
    ?assertEqual(2, maps:get(channel_id, Req)),
    ?assertEqual(false, maps:get(mention_everyone, Req)),
    ?assertEqual([], maps:get(role_ids, Req)),
    ?assertEqual(
        ?MENTION_SOURCE_PAGE_DEFAULT_LIMIT,
        maps:get(limit, Req)
    ).

mention_guild_calls_carry_the_caller_deadline_test() ->
    Cases = [
        {
            fun(Pid) -> guild_call_sources_page(Pid, #{limit => 5}) end,
            #{mentions => [], next_cursor => undefined},
            #{<<"mentions">> => [], <<"next_cursor">> => null}
        },
        {
            fun(Pid) -> guild_call_sources(Pid, #{user_ids => [7]}) end,
            #{direct_user_ids => [7], role_user_ids => [], everyone_user_ids => []},
            #{
                <<"direct_user_ids">> => [<<"7">>],
                <<"role_user_ids">> => [],
                <<"everyone_user_ids">> => []
            }
        },
        {
            fun(Pid) ->
                guild_call_user_ids(
                    Pid,
                    {get_users_to_mention_by_user_ids, #{user_ids => [7]}},
                    <<"users_error">>
                )
            end,
            #{user_ids => [7]},
            #{<<"user_ids">> => [<<"7">>]}
        }
    ],
    lists:foreach(fun assert_call_carries_deadline/1, Cases).

assert_call_carries_deadline({Call, GuildReply, Expected}) ->
    Self = self(),
    Guild = spawn(fun() ->
        receive
            {'$gen_call', From, Msg} ->
                Self ! {guild_request, Msg},
                gen_server:reply(From, GuildReply)
        end
    end),
    Before = os:system_time(millisecond),
    ?assertEqual(Expected, Call(Guild)),
    receive
        {guild_request, {_Tag, #{deadline := Deadline}}} ->
            ?assert(Deadline >= Before + ?GUILD_CALL_TIMEOUT)
    after 1000 ->
        ?assert(false, guild_request_not_received)
    end.

parse_page_params_rejects_malformed_optional_ids_test() ->
    Base = #{
        <<"guild_id">> => <<"1">>,
        <<"channel_id">> => <<"2">>,
        <<"author_id">> => <<"3">>
    },
    ?assertEqual(error, parse_page_params(Base#{<<"role_ids">> => [<<"4">>, <<"0">>]})),
    ?assertEqual(error, parse_page_params(Base#{<<"user_ids">> => [<<"5">>, <<"bad">>]})),
    ?assertEqual(error, parse_page_params(Base#{<<"cursor">> => <<"001">>})),
    ?assertEqual(error, parse_page_params(Base#{<<"role_ids">> => not_a_list})).

-endif.
