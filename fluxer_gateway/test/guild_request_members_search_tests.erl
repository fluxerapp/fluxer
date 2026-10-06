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

filter_members_by_query_test() ->
    Members = [
        #{<<"user">> => #{<<"id">> => <<"1">>, <<"username">> => <<"alice">>}},
        #{<<"user">> => #{<<"id">> => <<"2">>, <<"username">> => <<"bob">>}},
        #{<<"user">> => #{<<"id">> => <<"3">>, <<"username">> => <<"alicia">>}}
    ],
    Results = guild_request_members_search:filter_members_by_query(Members, <<"ali">>, 10),
    ?assertEqual(2, length(Results)).

display_name_priority_test() ->
    MemberWithNick = #{
        <<"user">> => #{<<"username">> => <<"user">>, <<"global_name">> => <<"Global">>},
        <<"nick">> => <<"Nick">>
    },
    ?assertEqual(<<"Nick">>, guild_request_members_search:get_display_name(MemberWithNick)),
    MemberWithGlobal = #{
        <<"user">> => #{<<"username">> => <<"user">>, <<"global_name">> => <<"Global">>}
    },
    ?assertEqual(<<"Global">>, guild_request_members_search:get_display_name(MemberWithGlobal)),
    MemberWithUsername = #{
        <<"user">> => #{<<"username">> => <<"user">>}
    },
    ?assertEqual(<<"user">>, guild_request_members_search:get_display_name(MemberWithUsername)).

filter_members_by_query_case_insensitive_test() ->
    Members = [
        #{<<"user">> => #{<<"id">> => <<"1">>, <<"username">> => <<"Alice">>}},
        #{<<"user">> => #{<<"id">> => <<"2">>, <<"username">> => <<"bob">>}}
    ],
    Results = guild_request_members_search:filter_members_by_query(Members, <<"ALICE">>, 10),
    ?assertEqual(1, length(Results)).

filter_members_by_query_respects_limit_test() ->
    Members = [
        #{<<"user">> => #{<<"id">> => <<"1">>, <<"username">> => <<"alice1">>}},
        #{<<"user">> => #{<<"id">> => <<"2">>, <<"username">> => <<"alice2">>}},
        #{<<"user">> => #{<<"id">> => <<"3">>, <<"username">> => <<"alice3">>}}
    ],
    Results = guild_request_members_search:filter_members_by_query(Members, <<"alice">>, 2),
    ?assertEqual(2, length(Results)).

filter_members_by_query_zero_limit_test() ->
    Members = [
        #{<<"user">> => #{<<"id">> => <<"1">>, <<"username">> => <<"alice1">>}}
    ],
    ?assertEqual(
        [], guild_request_members_search:filter_members_by_query(Members, <<"alice">>, 0)
    ).

filter_members_by_query_empty_query_matches_all_test() ->
    Members = [
        #{<<"user">> => #{<<"id">> => <<"1">>, <<"username">> => <<"alice">>}},
        #{<<"user">> => #{<<"id">> => <<"2">>, <<"username">> => <<"bob">>}}
    ],
    Results = guild_request_members_search:filter_members_by_query(Members, <<>>, 10),
    ?assertEqual(2, length(Results)).

filter_members_by_query_no_match_test() ->
    Members = [
        #{<<"user">> => #{<<"id">> => <<"1">>, <<"username">> => <<"alice">>}}
    ],
    Results = guild_request_members_search:filter_members_by_query(Members, <<"zzz">>, 10),
    ?assertEqual(0, length(Results)).

filter_members_by_query_matches_nick_test() ->
    Members = [
        #{
            <<"user">> => #{<<"id">> => <<"1">>, <<"username">> => <<"alice">>},
            <<"nick">> => <<"SuperNick">>
        }
    ],
    Results = guild_request_members_search:filter_members_by_query(Members, <<"super">>, 10),
    ?assertEqual(1, length(Results)).

filter_members_by_query_matches_username_behind_nick_test() ->
    Members = [
        #{
            <<"user">> => #{
                <<"id">> => <<"1">>,
                <<"username">> => <<"jiralite">>,
                <<"global_name">> => <<"Jiralite">>
            },
            <<"nick">> => <<"Specsaver engineer">>
        },
        #{<<"user">> => #{<<"id">> => <<"2">>, <<"username">> => <<"bob">>}}
    ],
    [Match] = guild_request_members_search:filter_members_by_query(Members, <<"JIRA">>, 10),
    ?assertEqual(1, guild_request_members_search:extract_user_id(Match)).

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

get_display_name_null_nick_test() ->
    Member = #{
        <<"user">> => #{<<"username">> => <<"user">>},
        <<"nick">> => null
    },
    ?assertEqual(<<"user">>, guild_request_members_search:get_display_name(Member)).

get_display_name_non_binary_nick_test() ->
    Member = #{
        <<"user">> => #{<<"username">> => <<"user">>},
        <<"nick">> => 12345
    },
    ?assertEqual(<<"user">>, guild_request_members_search:get_display_name(Member)).

get_display_name_non_map_test() ->
    ?assertEqual(<<>>, guild_request_members_search:get_display_name(invalid_member())).

get_display_name_null_global_name_test() ->
    Member = #{<<"user">> => #{<<"username">> => <<"user">>, <<"global_name">> => null}},
    ?assertEqual(<<"user">>, guild_request_members_search:get_display_name(Member)).

get_display_name_non_binary_global_name_test() ->
    Member = #{<<"user">> => #{<<"username">> => <<"user">>, <<"global_name">> => 12345}},
    ?assertEqual(<<"user">>, guild_request_members_search:get_display_name(Member)).

get_username_null_test() ->
    ?assertEqual(<<>>, guild_request_members_search:get_username(#{<<"username">> => null})).

get_username_undefined_test() ->
    ?assertEqual(
        <<>>, guild_request_members_search:get_username(#{<<"username">> => undefined})
    ).

get_username_non_binary_test() ->
    ?assertEqual(<<>>, guild_request_members_search:get_username(#{<<"username">> => 12345})).

get_username_missing_test() ->
    ?assertEqual(<<>>, guild_request_members_search:get_username(#{})).

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
