%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_request_members_search_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

-define(FULL_MEMBER_LIST_LIMIT, 100000).
-define(DEFAULT_QUERY_LIMIT, 25).

resolve_member_limit_full_scan_test() ->
    ?assertEqual(
        ?FULL_MEMBER_LIST_LIMIT, guild_request_members_search:resolve_member_limit(<<>>, 0)
    ).

resolve_member_limit_query_default_test() ->
    ?assertEqual(
        ?DEFAULT_QUERY_LIMIT, guild_request_members_search:resolve_member_limit(<<"ab">>, 0)
    ).

resolve_member_limit_explicit_test() ->
    ?assertEqual(25, guild_request_members_search:resolve_member_limit(<<"ab">>, 25)).

member_matches_every_name_test() ->
    Member = #{
        <<"user">> => #{
            <<"id">> => <<"1">>,
            <<"username">> => <<"jiralite">>,
            <<"global_name">> => <<"Jira Lite">>
        },
        <<"nick">> => <<"Specsaver engineer">>
    },
    Matches = fun(Query) ->
        guild_request_members_search:member_matches_normalized_query(Member, Query)
    end,
    ?assert(Matches(<<"specsaver">>)),
    ?assert(Matches(<<"jira l">>)),
    ?assert(Matches(<<"jiral">>)),
    ?assert(Matches(<<>>)),
    ?assertNot(Matches(<<"engineer">>)),
    ?assertNot(Matches(<<"lite">>)).

member_matches_ignores_non_binary_names_test() ->
    Member = #{
        <<"user">> => #{<<"username">> => <<"user">>, <<"global_name">> => null},
        <<"nick">> => 12345
    },
    ?assert(guild_request_members_search:member_matches_normalized_query(Member, <<"us">>)),
    ?assertNot(guild_request_members_search:member_matches_normalized_query(Member, <<"12">>)).

member_matches_non_map_member_test() ->
    ?assert(
        guild_request_members_search:member_matches_normalized_query(invalid_member(), <<>>)
    ),
    ?assertNot(
        guild_request_members_search:member_matches_normalized_query(invalid_member(), <<"a">>)
    ).

fetch_members_with_query_uses_guild_search_call_test() ->
    Parent = self(),
    Member = #{<<"user">> => #{<<"id">> => <<"1">>, <<"username">> => <<"Alice">>}},
    GuildPid = spawn(fun() ->
        reply_once_to_guild_call(Parent, #{members => [Member], total => 1})
    end),
    {Members, Presences} = guild_request_members_search:fetch_members_with_rollout(
        1, GuildPid, <<"ali">>, 1, [], false
    ),
    ?assertEqual([Member], Members),
    ?assertEqual([], Presences),
    ?assertEqual(
        {guild_call, {search_guild_members, #{query => <<"ali">>, limit => 1}}},
        receive_guild_call()
    ).

fetch_members_empty_query_uses_paginated_list_call_test() ->
    Parent = self(),
    Member = #{<<"user">> => #{<<"id">> => <<"1">>, <<"username">> => <<"Alice">>}},
    GuildPid = spawn(fun() ->
        reply_once_to_guild_call(Parent, #{members => [Member], total => 1})
    end),
    {Members, Presences} = guild_request_members_search:fetch_members_with_rollout(
        1, GuildPid, <<>>, 1, [], false
    ),
    ?assertEqual([Member], Members),
    ?assertEqual([], Presences),
    ?assertEqual(
        {guild_call, {list_guild_members, #{limit => 1, offset => 0}}},
        receive_guild_call()
    ).

extract_user_id_valid_test() ->
    ?assertEqual(
        42,
        guild_request_members_search:extract_user_id(#{<<"user">> => #{<<"id">> => <<"42">>}})
    ).

extract_user_id_rejects_malformed_snowflake_test() ->
    ?assertEqual(
        undefined,
        guild_request_members_search:extract_user_id(
            #{<<"user">> => #{<<"id">> => <<"042">>}}
        )
    ).

extract_user_id_missing_user_test() ->
    ?assertEqual(undefined, guild_request_members_search:extract_user_id(#{})).

extract_user_id_non_map_test() ->
    ?assertEqual(undefined, guild_request_members_search:extract_user_id(invalid_member())).

invalid_member() ->
    eqwalizer:dynamic_cast(not_a_map).

receive_guild_call() ->
    receive
        {guild_call, _Msg} = Msg ->
            Msg;
        _Other ->
            receive_guild_call()
    after 1000 ->
        timeout
    end.

reply_once_to_guild_call(Parent, Reply) ->
    receive
        {'$gen_call', {From, Tag}, Msg} ->
            Parent ! {guild_call, Msg},
            From ! {Tag, Reply}
    after 1000 ->
        Parent ! guild_call_timeout
    end.
