%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_utils_tests).
-typing([eqwalizer]).
-include_lib("eunit/include/eunit.hrl").

get_default_avatar_url_test() ->
    Url = push_utils:get_default_avatar_url(<<"123">>),
    ?assert(is_binary(Url)),
    ?assertMatch(<<"http://localhost:8088/avatars/", _/binary>>, Url).

avatar_index_test() ->
    ?assertEqual(undefined, push_utils:avatar_index(<<"0">>)),
    ?assertEqual(1, push_utils:avatar_index(<<"1">>)),
    ?assertEqual(2, push_utils:avatar_index(<<"2">>)),
    ?assertEqual(0, push_utils:avatar_index(<<"6">>)),
    ?assertEqual(undefined, push_utils:avatar_index(<<"invalid">>)),
    ?assertEqual(undefined, push_utils:avatar_index(<<"001">>)).

wrap_avatar_index_test() ->
    ?assertEqual(0, push_utils:wrap_avatar_index(0)),
    ?assertEqual(1, push_utils:wrap_avatar_index(1)),
    ?assertEqual(0, push_utils:wrap_avatar_index(6)),
    ?assertEqual(1, push_utils:wrap_avatar_index(7)).
