%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_state_member).
-typing([eqwalizer]).

-export([
    handle_member_add/2,
    handle_member_update/2,
    handle_member_remove/3,
    sync_member/2,
    sync_member_remove/2,
    cleanup_removed_member_sessions/1,
    cleanup_removed_member_sessions/2,
    maybe_disconnect_removed_member/2,
    extract_user_id/1,
    lookup_presence/2
]).

-type guild_state() :: #{member_presence => ets:tid(), term() => term()}.
-type guild_data() :: map().
-type event_data() :: map().

-export_type([guild_state/0, guild_data/0, event_data/0, user_id/0]).
-type user_id() :: integer().

-spec handle_member_add(event_data(), guild_data()) -> guild_data().
handle_member_add(EventData, Data) ->
    guild_data_index:put_member(EventData, Data).

-spec handle_member_update(event_data(), guild_data()) -> guild_data().
handle_member_update(EventData, Data) ->
    UserId = extract_user_id(EventData),
    Members = guild_data_index:member_map(Data),
    case maps:is_key(UserId, Members) of
        true ->
            guild_data_index:put_member(EventData, Data);
        false ->
            Data
    end.

-spec handle_member_remove(event_data(), guild_data(), guild_state()) -> guild_data().
handle_member_remove(EventData, Data, _State) ->
    UserId = extract_user_id(EventData),
    guild_data_index:remove_member(UserId, Data).

-spec sync_member(event_data(), guild_state()) -> ok.
sync_member(EventData, State) ->
    UserId = extract_user_id(EventData),
    case is_integer(UserId) andalso UserId > 0 of
        true -> sync_valid_member(UserId, State);
        false -> ok
    end.

-spec sync_valid_member(user_id(), guild_state()) -> ok.
sync_valid_member(UserId, State) ->
    Data = maps:get(data, State, #{}),
    case guild_data_index:get_member(UserId, Data) of
        undefined -> ok;
        Member -> sync_local_nif_member(UserId, Member, State)
    end.

-spec sync_local_nif_member(user_id(), map(), guild_state()) -> ok.
sync_local_nif_member(UserId, Member, State) ->
    ok = sync_default_member_list_member(UserId, Member, State),
    guild_member_list_channel_engine:update_user_all(UserId, State).

-spec sync_default_member_list_member(user_id(), map(), guild_state()) -> ok.
sync_default_member_list_member(UserId, Member, State) ->
    case member_list_engine_ref(State) of
        undefined ->
            ok;
        Ref ->
            DisplayName = guild_member_list_common:get_member_display_name(Member),
            SortKey = guild_member_list_common:casefold_binary(DisplayName),
            RoleIds = guild_member_list_store:extract_role_ids(Member),
            ConnectedUserIds = guild_member_list_common:connected_session_user_ids(State),
            Presence = lookup_presence(maps:get(member_presence, State), UserId),
            Status = maps:get(<<"status">>, Presence, <<"offline">>),
            IsConnected = sets:is_element(UserId, ConnectedUserIds),
            NotOffline = Status =/= <<"offline">> andalso Status =/= <<"invisible">>,
            IsOnline = IsConnected andalso NotOffline,
            guild_member_list_store:update_member(Ref, UserId, SortKey, RoleIds, IsOnline)
    end.

-spec sync_member_remove(user_id() | undefined, guild_state()) -> ok.
sync_member_remove(UserId, State) ->
    case is_integer(UserId) andalso UserId > 0 of
        false ->
            ok;
        true ->
            sync_local_nif_member_remove(UserId, State)
    end.

-spec sync_local_nif_member_remove(user_id(), guild_state()) -> ok.
sync_local_nif_member_remove(UserId, State) ->
    ok = sync_default_member_list_remove(UserId, State),
    guild_member_list_channel_engine:remove_user_all(UserId, State).

-spec sync_default_member_list_remove(user_id(), guild_state()) -> ok.
sync_default_member_list_remove(UserId, State) ->
    case member_list_engine_ref(State) of
        undefined -> ok;
        Ref -> guild_member_list_store:remove_member(Ref, UserId)
    end.

-spec cleanup_removed_member_sessions(guild_state()) -> guild_state().
cleanup_removed_member_sessions(State) ->
    Data = state_data(State),
    MemberUserIds = sets:from_list(guild_data_index:member_ids(Data)),
    Sessions = state_sessions(State),
    FilteredSessions = maps:filter(
        fun(_K, S) ->
            UserId = maps:get(user_id, S),
            sets:is_element(UserId, MemberUserIds)
        end,
        Sessions
    ),
    State#{sessions => FilteredSessions}.

-spec cleanup_removed_member_sessions(user_id() | undefined, guild_state()) -> guild_state().
cleanup_removed_member_sessions(UserId, State) when is_integer(UserId), UserId > 0 ->
    Sessions = state_sessions(State),
    FilteredSessions = maps:filter(
        fun(_SessionId, SessionData) ->
            maps:get(user_id, SessionData, undefined) =/= UserId
        end,
        Sessions
    ),
    State#{sessions => FilteredSessions};
cleanup_removed_member_sessions(_UserId, State) ->
    cleanup_removed_member_sessions(State).

-spec maybe_disconnect_removed_member(user_id() | undefined, guild_state()) -> guild_state().
maybe_disconnect_removed_member(UserId, State) when is_integer(UserId), UserId > 0 ->
    ok = guild_voice_lifecycle:cast_disconnect_voice_user(UserId, State),
    State;
maybe_disconnect_removed_member(_, State) ->
    State.

-spec extract_user_id(event_data()) -> user_id() | undefined.
extract_user_id(EventData) ->
    MUser = maps:get(<<"user">>, EventData, #{}),
    snowflake_id:parse_optional(maps:get(<<"id">>, MUser, undefined)).

-spec lookup_presence(ets:tid() | map(), user_id()) -> map().
lookup_presence(Map, UserId) when is_map(Map) ->
    maps:get(UserId, Map, guild_member_list_common:default_presence());
lookup_presence(Tab, UserId) ->
    case ets:lookup(Tab, UserId) of
        [{_, P}] -> P;
        [] -> guild_member_list_common:default_presence()
    end.

-spec member_list_engine_ref(guild_state()) -> ets:table() | undefined.
member_list_engine_ref(State) ->
    case maps:get(member_list_engine, State, undefined) of
        Ref when is_reference(Ref); is_atom(Ref) -> Ref;
        _ -> undefined
    end.

-spec state_data(guild_state()) -> guild_data().
state_data(State) ->
    case maps:get(data, State, #{}) of
        Data when is_map(Data) -> Data;
        _ -> #{}
    end.

-spec state_sessions(guild_state()) -> map().
state_sessions(State) ->
    case maps:get(sessions, State, #{}) of
        Sessions when is_map(Sessions) -> Sessions;
        _ -> #{}
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

handle_member_add_test() ->
    Data = #{<<"members">> => #{1 => #{<<"user">> => #{<<"id">> => <<"1">>}}}},
    EventData = #{<<"user">> => #{<<"id">> => <<"2">>}},
    Result = handle_member_add(EventData, Data),
    Members = maps:get(<<"members">>, Result),
    ?assertEqual(2, map_size(Members)).

handle_member_update_test() ->
    Data = #{
        <<"members">> => #{
            1 => #{<<"user">> => #{<<"id">> => <<"1">>}, <<"nick">> => <<"OldNick">>}
        }
    },
    EventData = #{<<"user">> => #{<<"id">> => <<"1">>}, <<"nick">> => <<"NewNick">>},
    Result = handle_member_update(EventData, Data),
    Members = maps:get(<<"members">>, Result),
    Member = maps:get(1, Members),
    ?assertEqual(<<"NewNick">>, maps:get(<<"nick">>, Member)).

handle_member_update_ignores_non_member_test() ->
    Data = #{
        <<"members">> => #{
            1 => #{<<"user">> => #{<<"id">> => <<"1">>}, <<"nick">> => <<"nick">>}
        }
    },
    EventData = #{<<"user">> => #{<<"id">> => <<"999">>}, <<"nick">> => <<"new">>},
    Result = handle_member_update(EventData, Data),
    ?assertEqual(1, map_size(maps:get(<<"members">>, Result))),
    ?assertEqual(undefined, guild_data_index:get_member(999, Result)).

cleanup_removed_member_sessions_removes_non_members_test() ->
    Data = #{
        <<"members">> => #{
            1 => #{<<"user">> => #{<<"id">> => <<"1">>}}
        }
    },
    Sessions = #{
        <<"s1">> => #{user_id => 1},
        <<"s2">> => #{user_id => 999}
    },
    State = #{data => Data, sessions => Sessions},
    Result = cleanup_removed_member_sessions(State),
    #{} = NewSessions = maps:get(sessions, Result),
    ?assertEqual(1, map_size(NewSessions)),
    ?assert(maps:is_key(<<"s1">>, NewSessions)).

cleanup_removed_member_sessions_for_user_removes_only_that_user_test() ->
    Sessions = #{
        <<"s1">> => #{user_id => 1},
        <<"s2">> => #{user_id => 2},
        <<"s3">> => #{user_id => 3}
    },
    State = #{data => #{<<"members">> => #{}}, sessions => Sessions},
    Result = cleanup_removed_member_sessions(2, State),
    #{} = NewSessions = maps:get(sessions, Result),
    ?assertEqual([<<"s1">>, <<"s3">>], lists:sort(maps:keys(NewSessions))).

sync_member_updates_loaded_channel_engines_test() ->
    GuildId = 100,
    UserId = 10,
    ChannelId = 500,
    BotsRoleId = 200,
    CountingRoleId = 300,
    DefaultRef = guild_member_list_engine:new(),
    ChannelRef = guild_member_list_engine:new(),
    try
        ok = load_sync_member_test_ref(DefaultRef, UserId, CountingRoleId, BotsRoleId),
        ok = load_sync_member_test_ref(ChannelRef, UserId, CountingRoleId, BotsRoleId),
        State = sync_test_state(
            GuildId, UserId, ChannelId, BotsRoleId, CountingRoleId, DefaultRef, ChannelRef
        ),
        ?assertEqual(stale_groups(BotsRoleId), guild_member_list_engine:get_groups(ChannelRef)),
        EventData = #{<<"user">> => #{<<"id">> => integer_to_binary(UserId)}},
        ok = sync_member(EventData, State),
        ?assertEqual(
            hoisted_groups(BotsRoleId), guild_member_list_engine:get_groups(ChannelRef)
        )
    after
        guild_member_list_engine:destroy(DefaultRef),
        guild_member_list_engine:destroy(ChannelRef)
    end.

sync_member_remove_removes_loaded_channel_engines_test() ->
    UserId = 10,
    RoleId = 200,
    DefaultRef = guild_member_list_engine:new(),
    ChannelRef = guild_member_list_engine:new(),
    try
        ok = load_sync_member_test_ref(DefaultRef, UserId, RoleId, RoleId),
        ok = load_sync_member_test_ref(ChannelRef, UserId, RoleId, RoleId),
        State = #{
            member_list_engine => DefaultRef,
            channel_member_list_engines => #{<<"500">> => ChannelRef}
        },
        ok = sync_member_remove(UserId, State),
        ?assertEqual({0, 0}, guild_member_list_engine:get_counts(ChannelRef))
    after
        guild_member_list_engine:destroy(DefaultRef),
        guild_member_list_engine:destroy(ChannelRef)
    end.

-spec load_sync_member_test_ref(
    guild_member_list_store:store_ref(), integer(), integer(), integer()
) -> ok.
load_sync_member_test_ref(Ref, UserId, MemberRoleId, HoistedRoleId) ->
    guild_member_list_engine:bulk_load(
        Ref, [{UserId, <<"counting">>, [MemberRoleId], true}], [HoistedRoleId]
    ).

sync_test_state(
    GuildId, UserId, ChannelId, BotsRoleId, CountingRoleId, DefaultRef, ChannelRef
) ->
    Data = guild_data_index:normalize_data(#{
        <<"guild">> => #{<<"owner_id">> => integer_to_binary(UserId)},
        <<"roles">> => [
            role(GuildId, <<"everyone">>, false, 0),
            role(BotsRoleId, <<"bots">>, true, 1),
            role(CountingRoleId, <<"Counting">>, false, 1)
        ],
        <<"members">> => [
            #{
                <<"user">> => #{
                    <<"id">> => integer_to_binary(UserId),
                    <<"username">> => <<"Counting">>
                },
                <<"roles">> => [
                    integer_to_binary(BotsRoleId), integer_to_binary(CountingRoleId)
                ]
            }
        ],
        <<"channels">> => [
            #{<<"id">> => integer_to_binary(ChannelId), <<"permission_overwrites">> => []}
        ]
    }),
    #{
        id => GuildId,
        data => Data,
        sessions => #{},
        member_presence => #{UserId => #{<<"status">> => <<"online">>}},
        connected_user_ids => sets:from_list([UserId]),
        member_list_engine => DefaultRef,
        channel_member_list_engines => #{integer_to_binary(ChannelId) => ChannelRef}
    }.

role(RoleId, Name, Hoist, Position) ->
    #{
        <<"id">> => integer_to_binary(RoleId),
        <<"name">> => Name,
        <<"hoist">> => Hoist,
        <<"position">> => Position,
        <<"permissions">> => <<"0">>
    }.

stale_groups(RoleId) ->
    [{integer_to_binary(RoleId), 0}, {<<"online">>, 1}, {<<"offline">>, 0}].

hoisted_groups(RoleId) ->
    [{integer_to_binary(RoleId), 1}, {<<"online">>, 0}, {<<"offline">>, 0}].

-endif.
