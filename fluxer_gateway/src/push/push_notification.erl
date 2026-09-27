%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_notification).
-typing([eqwalizer]).

-export([build_notification_title/5]).

-spec build_notification_title(
    binary(), map(), integer(), binary() | undefined, binary() | undefined
) -> binary().
build_notification_title(AuthorUsername, MessageData, GuildId, GuildName, ChannelName) ->
    ChannelType = maps:get(<<"channel_type">>, MessageData, 1),
    case GuildId of
        0 -> format_dm_title(AuthorUsername, ChannelType);
        _ -> format_guild_title(AuthorUsername, GuildName, ChannelName)
    end.

-spec format_dm_title(binary(), term()) -> binary().
format_dm_title(AuthorUsername, 3) ->
    iolist_to_binary([AuthorUsername, <<" (Group DM)">>]);
format_dm_title(AuthorUsername, _ChannelType) ->
    AuthorUsername.

-spec format_guild_title(binary(), binary() | undefined, binary() | undefined) -> binary().
format_guild_title(AuthorUsername, undefined, _) ->
    AuthorUsername;
format_guild_title(AuthorUsername, _, undefined) ->
    AuthorUsername;
format_guild_title(AuthorUsername, GName, ChanName) ->
    iolist_to_binary([AuthorUsername, <<" (#">>, ChanName, <<", ">>, GName, <<")">>]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

build_notification_title_dm_test() ->
    ?assertEqual(
        <<"Alice">>,
        build_notification_title(<<"Alice">>, #{}, 0, undefined, undefined)
    ).

build_notification_title_group_dm_test() ->
    MessageData = #{<<"channel_type">> => 3},
    ?assertEqual(
        <<"Alice (Group DM)">>,
        build_notification_title(<<"Alice">>, MessageData, 0, undefined, undefined)
    ).

build_notification_title_guild_test() ->
    ?assertEqual(
        <<"Alice (#general, My Server)">>,
        build_notification_title(<<"Alice">>, #{}, 123, <<"My Server">>, <<"general">>)
    ).

build_url_dm_test() ->
    ?assertEqual(<<"/channels/@me/456/789">>, push_notification_format:build_url(0, 456, 789)).

build_url_guild_test() ->
    ?assertEqual(
        <<"/channels/123/456/789">>, push_notification_format:build_url(123, 456, 789)
    ).

-endif.
