%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_push_silent_tests).

-include_lib("eunit/include/eunit.hrl").

-define(GUILD_ID, 7300).
-define(CHANNEL_ID, 7400).
-define(USER, 30).
-define(AUTHOR, 40).
-define(SUPPRESS_NOTIFICATIONS, 4096).

member() ->
    #{<<"user">> => #{<<"id">> => integer_to_binary(?USER)}, <<"roles">> => []}.

guild_state(MembersTab) ->
    #{
        id => ?GUILD_ID,
        data => #{
            <<"guild">> => #{
                <<"id">> => ?GUILD_ID,
                <<"name">> => <<"Guild">>,
                <<"owner_id">> => ?USER,
                <<"default_message_notifications">> => 0
            },
            <<"channels">> => [#{<<"id">> => ?CHANNEL_ID, <<"name">> => <<"general">>}],
            <<"channel_index">> => #{
                ?CHANNEL_ID => #{<<"id">> => ?CHANNEL_ID, <<"name">> => <<"general">>}
            },
            <<"roles">> => [],
            <<"role_index">> => #{},
            members_ets => MembersTab
        },
        sessions => #{}
    }.

guild_message(Id, Flags) ->
    #{
        <<"id">> => integer_to_binary(Id),
        <<"channel_id">> => integer_to_binary(?CHANNEL_ID),
        <<"author">> => #{<<"id">> => integer_to_binary(?AUTHOR)},
        <<"content">> => <<"<@30> hi @everyone">>,
        <<"mentions">> => [#{<<"id">> => integer_to_binary(?USER)}],
        <<"mention_everyone">> => true,
        <<"flags">> => Flags
    }.

pushed_message_ids(Messages) ->
    Self = self(),
    MembersTab = ets:new(guild_push_silent_members, [set, public]),
    true = ets:insert(MembersTab, {?USER, member()}),
    ok = meck:new(push, [passthrough, no_link]),
    try
        ok = meck:expect(push, handle_message_create, fun(Params) ->
            Self ! {pushed, maps:get(<<"id">>, maps:get(message_data, Params))},
            ok
        end),
        lists:foreach(
            fun(Message) ->
                ok = guild_dispatch_push:maybe_send_push_notifications(
                    message_create, Message, ?GUILD_ID, guild_state(MembersTab)
                )
            end,
            Messages
        ),
        lists:sort(collect_pushed(1000))
    after
        meck:unload(push),
        ets:delete(MembersTab)
    end.

collect_pushed(Timeout) ->
    receive
        {pushed, Id} -> [binary_to_integer(Id) | collect_pushed(300)]
    after Timeout -> []
    end.

a_silent_guild_message_mentioning_the_member_is_not_pushed_test() ->
    Messages = [
        guild_message(1, ?SUPPRESS_NOTIFICATIONS),
        guild_message(2, ?SUPPRESS_NOTIFICATIONS bor 4),
        guild_message(3, 0),
        guild_message(4, 4)
    ],
    ?assertEqual([3, 4], pushed_message_ids(Messages)).
