%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_thread_push_tests).

-include_lib("eunit/include/eunit.hrl").

-define(G, 1).
-define(A, 10).
-define(B, 11).
-define(C, 12).
-define(OUT, 13).
-define(AUTHOR, 14).
-define(TEXT, 100).
-define(THREAD, 500).
-define(VIEW, 1024).
-define(SEND, 2048).
-define(CONFIG_KEY, channel_threads_config).

with_config(Fun) ->
    Previous = persistent_term:get(?CONFIG_KEY, undefined),
    Config = (channel_threads_config:default_config())#{
        enabled => true,
        included_users =>
            #{integer_to_binary(Id) => true || Id <- [?A, ?B, ?C, ?AUTHOR]}
    },
    persistent_term:put(?CONFIG_KEY, Config),
    try
        Fun()
    after
        case Previous of
            undefined -> persistent_term:erase(?CONFIG_KEY);
            _ -> persistent_term:put(?CONFIG_KEY, Previous)
        end
    end.

raw_data() ->
    Gid = integer_to_binary(?G),
    Member = fun(Id) ->
        #{
            <<"user">> => #{<<"id">> => integer_to_binary(Id)},
            <<"roles">> => [],
            <<"joined_at">> => <<"2026-01-01T00:00:00Z">>
        }
    end,
    #{
        <<"guild">> => #{<<"id">> => Gid, <<"owner_id">> => <<"999">>, <<"features">> => []},
        <<"roles">> => [
            #{
                <<"id">> => Gid,
                <<"permissions">> => integer_to_binary(?VIEW bor ?SEND),
                <<"position">> => 0
            }
        ],
        <<"channels">> => [
            #{<<"id">> => <<"100">>, <<"type">> => 0, <<"permission_overwrites">> => []}
        ],
        <<"members">> => [Member(Id) || Id <- [?A, ?B, ?C, ?OUT, ?AUTHOR]],
        <<"emojis">> => [],
        <<"stickers">> => []
    }.

thread_member(UserId) ->
    #{
        <<"id">> => integer_to_binary(?THREAD),
        <<"user_id">> => integer_to_binary(UserId),
        <<"join_timestamp">> => <<"2026-01-01T00:00:00.000Z">>,
        <<"flags">> => 1
    }.

state(Active) ->
    Payload = guild_thread_load:load_payload(#{
        <<"threads">> => [
            #{
                <<"id">> => integer_to_binary(?THREAD),
                <<"guild_id">> => integer_to_binary(?G),
                <<"parent_id">> => integer_to_binary(?TEXT),
                <<"type">> => 11,
                <<"name">> => <<"ideas">>,
                <<"thread_metadata">> => #{<<"archived">> => false, <<"locked">> => false}
            }
        ],
        <<"thread_members">> => [thread_member(?A), thread_member(?OUT)]
    }),
    Base = #{
        id => ?G,
        member_count => 5,
        data => guild_data_index:normalize_map(raw_data()),
        sessions => #{
            <<"b">> => #{session_id => <<"b">>, user_id => ?B, pid => self()},
            <<"a">> => #{session_id => <<"a">>, user_id => ?A, pid => self()}
        }
    },
    Gate =
        case Active of
            true ->
                #{
                    thread_gate => #{active => true, version => 1},
                    thread_tainted => true,
                    thread_load => {ok, Payload}
                };
            false ->
                #{}
        end,
    guild_thread_load:install(Gate, Base).

with_state(Active, Fun) ->
    with_config(fun() ->
        State = state(Active),
        try
            Fun(State)
        after
            guild_thread_store:destroy(guild_thread_gate:store(State))
        end
    end).

mention_request(ChannelId, Overrides) ->
    maps:merge(
        #{
            channel_id => ChannelId,
            author_id => ?AUTHOR,
            mention_everyone => false,
            mention_here => false,
            role_ids => [],
            user_ids => []
        },
        Overrides
    ).

everyone(Request, State) ->
    {reply, #{everyone_user_ids := Ids}, _} =
        guild_members_mutation:resolve_mention_sources(Request, State),
    lists:sort(Ids).

everyone_in_a_thread_reaches_recipient_active_members_only_test() ->
    with_state(true, fun(State) ->
        Request = mention_request(?THREAD, #{mention_everyone => true}),
        ?assertEqual([?A], everyone(Request, State)),
        ?assertEqual(
            [?A, ?B, ?C, ?OUT],
            everyone(mention_request(?TEXT, #{mention_everyone => true}), State)
        )
    end).

here_in_a_thread_needs_membership_and_a_connection_test() ->
    with_state(true, fun(State) ->
        Request = mention_request(?THREAD, #{mention_here => true}),
        ?assertEqual([?A], everyone(Request, State)),
        ?assertEqual([?A, ?B], everyone(mention_request(?TEXT, #{mention_here => true}), State))
    end).

direct_mentions_in_a_thread_skip_inactive_recipients_test() ->
    with_state(true, fun(State) ->
        {reply, #{direct_user_ids := Ids}, _} =
            guild_members_mutation:resolve_mention_sources(
                mention_request(?THREAD, #{user_ids => [?B, ?OUT]}), State
            ),
        ?assertEqual([?B], lists:sort(Ids))
    end).

paged_everyone_in_a_thread_lists_members_only_test() ->
    with_state(true, fun(State) ->
        Request = (mention_request(?THREAD, #{mention_everyone => true}))#{
            limit => 100, cursor => undefined
        },
        {reply, #{mentions := Entries}, _} =
            guild_members_mutation:resolve_mention_sources_page(Request, State),
        ?assertEqual([?A], lists:sort([entry_user_id(E) || E <- Entries]))
    end).

inactive_guild_mentions_are_unchanged_test() ->
    with_state(false, fun(State) ->
        ?assertEqual(undefined, guild_thread_push:mention_universe(?THREAD, State)),
        ?assertEqual(
            [?A, ?B, ?C, ?OUT],
            everyone(mention_request(?TEXT, #{mention_everyone => true}), State)
        )
    end).

passive_threads_follow_membership_test() ->
    with_state(true, fun(State) ->
        ?assertMatch(
            [#{<<"name">> := <<"ideas">>}],
            guild_thread_push:passive_threads(#{thread_viewer => true}, ?A, State)
        ),
        ?assertEqual(
            [], guild_thread_push:passive_threads(#{thread_viewer => true}, ?B, State)
        ),
        ?assertEqual([], guild_thread_push:passive_threads(#{}, ?A, State))
    end).

entry_user_id(#{user_id := UserId}) -> UserId;
entry_user_id(#{<<"user_id">> := UserId}) -> snowflake_id:parse_optional(UserId);
entry_user_id(UserId) when is_integer(UserId) -> UserId;
entry_user_id(Other) -> Other.
