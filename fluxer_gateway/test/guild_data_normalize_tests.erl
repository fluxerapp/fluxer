%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_data_normalize_tests).
-typing([eqwalizer]).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

role_keeps_icon_fields_when_set_test() ->
    Role = guild_data_normalize:role(role_with(<<"a1b2c3d4">>, null)),
    ?assertEqual(<<"a1b2c3d4">>, maps:get(<<"icon">>, Role)),
    ?assertEqual(null, maps:get(<<"unicode_emoji">>, Role)).

role_keeps_unicode_emoji_test() ->
    Emoji = <<240, 159, 155, 161, 239, 184, 143>>,
    Role = guild_data_normalize:role(role_with(null, Emoji)),
    ?assertEqual(null, maps:get(<<"icon">>, Role)),
    ?assertEqual(Emoji, maps:get(<<"unicode_emoji">>, Role)).

role_still_normalises_the_other_fields_test() ->
    Role = guild_data_normalize:role(role_with(<<"a1b2c3d4">>, null)),
    ?assertEqual(123456789012345678, maps:get(<<"id">>, Role)),
    ?assertEqual(3, maps:get(<<"position">>, Role)).

role_with(Icon, UnicodeEmoji) ->
    #{
        <<"id">> => <<"123456789012345678">>,
        <<"name">> => <<"Mods">>,
        <<"color">> => 0,
        <<"position">> => 3,
        <<"hoist_position">> => null,
        <<"permissions">> => <<"0">>,
        <<"hoist">> => false,
        <<"mentionable">> => false,
        <<"icon">> => Icon,
        <<"unicode_emoji">> => UnicodeEmoji
    }.

-endif.
