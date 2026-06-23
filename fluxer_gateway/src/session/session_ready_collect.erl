%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_ready_collect).
-typing([eqwalizer]).

-export([
    collect_ready_users/2,
    collect_ready_presences/2,
    collect_relationship_users/1,
    collect_channel_users/1,
    collect_guild_users/1,
    dedup_users/1,
    strip_users_from_guild_members/1,
    strip_user_from_relationships/1
]).

-export_type([session_state/0]).

-type session_state() :: session:session_state().
-type user_id() :: session:user_id().

-spec collect_ready_users(session_state(), [map()]) -> [map()].
collect_ready_users(#{bot := true}, _CollectedGuilds) ->
    [];
collect_ready_users(State, CollectedGuilds) ->
    collect_ready_users_nonbot(State, CollectedGuilds).

-spec collect_ready_users_nonbot(session_state(), [map()]) -> [map()].
collect_ready_users_nonbot(State, CollectedGuilds) ->
    Ready = maps:get(ready, State, #{}),
    Relationships = map_utils:ensure_list(map_utils:get_safe(Ready, <<"relationships">>, [])),
    Channels = maps:values(maps:get(channels, State, #{})),
    UserMap0 = add_relationship_users(Relationships, #{}),
    UserMap1 = add_channel_users(Channels, UserMap0),
    UserMap2 = add_guild_users(CollectedGuilds, UserMap1),
    maps:values(UserMap2).

-spec collect_relationship_users(session_state()) -> [map()].
collect_relationship_users(State) ->
    Ready = maps:get(ready, State, #{}),
    Relationships = map_utils:ensure_list(map_utils:get_safe(Ready, <<"relationships">>, [])),
    normalize_users_safe([maps:get(<<"user">>, Rel, undefined) || Rel <- Relationships]).

-spec collect_ready_presences(session_state(), [map()]) -> [map()].
collect_ready_presences(#{bot := true}, _CollectedGuilds) ->
    [];
collect_ready_presences(State, _CollectedGuilds) ->
    CurrentUserId = maps:get(user_id, State),
    Targets = collect_presence_targets(State, CurrentUserId),
    fetch_visible_presences(Targets).

-spec collect_presence_targets(session_state(), user_id()) -> [user_id()].
collect_presence_targets(State, CurrentUserId) when is_map(State) ->
    FIds = presence_targets:friend_ids_from_state(State),
    DmMap = presence_targets:dm_recipients_from_state(State),
    TargetMap0 = add_presence_target_ids(FIds, CurrentUserId, #{}),
    TargetMap = maps:fold(
        fun(_Cid, Recipients, Acc) ->
            add_presence_target_map(Recipients, CurrentUserId, Acc)
        end,
        TargetMap0,
        DmMap
    ),
    maps:keys(TargetMap).

-spec fetch_visible_presences([user_id()]) -> [map()].
fetch_visible_presences([]) ->
    [];
fetch_visible_presences(Targets) ->
    dedup_presences(presence_cache_safe:visible_bulk_get(Targets)).

-spec presence_user_id(map()) -> user_id() | undefined.
presence_user_id(P) when is_map(P) ->
    User = maps:get(<<"user">>, P, #{}),
    user_id(User);
presence_user_id(_) ->
    undefined.

-spec user_id(term()) -> user_id() | undefined.
user_id(User) when is_map(User) ->
    snowflake_id:parse_maybe(maps:get(<<"id">>, User, undefined));
user_id(_) ->
    undefined.

-spec dedup_presences([map()]) -> [map()].
dedup_presences(Presences) ->
    Map = lists:foldl(fun add_presence_by_id/2, #{}, Presences),
    maps:values(Map).

-spec add_presence_by_id(map(), #{user_id() => map()}) -> #{user_id() => map()}.
add_presence_by_id(P, Acc) ->
    case presence_user_id(P) of
        undefined -> Acc;
        Id -> Acc#{Id => P}
    end.

-spec collect_channel_users([map()]) -> [map()].
collect_channel_users(Channels) ->
    lists:foldl(
        fun(Channel, Acc) ->
            collect_dm_recipients(Channel) ++ Acc
        end,
        [],
        Channels
    ).

-spec collect_dm_recipients(map()) -> [map()].
collect_dm_recipients(Channel) ->
    Type = maps:get(<<"type">>, Channel, undefined),
    case Type =:= 1 orelse Type =:= 3 of
        true ->
            RecipientsRaw = map_utils:ensure_list(maps:get(<<"recipients">>, Channel, [])),
            normalize_users_safe(RecipientsRaw);
        false ->
            []
    end.

-spec add_presence_target_ids([term()], user_id(), #{user_id() => true}) ->
    #{
        user_id() => true
    }.
add_presence_target_ids(UserIds, CurrentUserId, Acc) ->
    lists:foldl(
        fun(UserId, Acc0) -> add_presence_target(UserId, CurrentUserId, Acc0) end,
        Acc,
        UserIds
    ).

-spec add_presence_target_map(map(), user_id(), #{user_id() => true}) -> #{user_id() => true}.
add_presence_target_map(Recipients, CurrentUserId, Acc) when is_map(Recipients) ->
    maps:fold(
        fun(UserId, _Value, Acc0) -> add_presence_target(UserId, CurrentUserId, Acc0) end,
        Acc,
        Recipients
    );
add_presence_target_map(_Recipients, _CurrentUserId, Acc) ->
    Acc.

-spec add_presence_target(term(), user_id(), #{user_id() => true}) -> #{user_id() => true}.
add_presence_target(UserId, CurrentUserId, Acc) when
    is_integer(UserId), UserId =/= CurrentUserId
->
    Acc#{UserId => true};
add_presence_target(_UserId, _CurrentUserId, Acc) ->
    Acc.

-spec add_relationship_users([term()], #{user_id() => map()}) -> #{user_id() => map()}.
add_relationship_users(Relationships, Acc) ->
    lists:foldl(fun add_relationship_user/2, Acc, Relationships).

-spec add_relationship_user(term(), #{user_id() => map()}) -> #{user_id() => map()}.
add_relationship_user(Rel, Acc) when is_map(Rel) ->
    add_user_by_id(normalize_user_safe(maps:get(<<"user">>, Rel, undefined)), Acc);
add_relationship_user(_Rel, Acc) ->
    Acc.

-spec add_channel_users([term()], #{user_id() => map()}) -> #{user_id() => map()}.
add_channel_users(Channels, Acc) ->
    lists:foldl(fun add_channel_user/2, Acc, Channels).

-spec add_channel_user(term(), #{user_id() => map()}) -> #{user_id() => map()}.
add_channel_user(Channel, Acc) when is_map(Channel) ->
    Type = maps:get(<<"type">>, Channel, undefined),
    case Type =:= 1 orelse Type =:= 3 of
        true ->
            RecipientsRaw = map_utils:ensure_list(maps:get(<<"recipients">>, Channel, [])),
            add_channel_recipient_users(RecipientsRaw, Acc);
        false ->
            Acc
    end;
add_channel_user(_Channel, Acc) ->
    Acc.

-spec add_channel_recipient_users([term()], #{user_id() => map()}) ->
    #{user_id() => map()}.
add_channel_recipient_users(Recipients, Acc) ->
    lists:foldl(fun add_channel_recipient_user/2, Acc, Recipients).

-spec add_channel_recipient_user(term(), #{user_id() => map()}) -> #{user_id() => map()}.
add_channel_recipient_user(Recipient, Acc) ->
    add_user_by_id(normalize_user_safe(Recipient), Acc).

-spec add_guild_users([term()], #{user_id() => map()}) -> #{user_id() => map()}.
add_guild_users(GuildStates, Acc) ->
    lists:foldl(fun add_guild_user/2, Acc, GuildStates).

-spec add_guild_user(term(), #{user_id() => map()}) -> #{user_id() => map()}.
add_guild_user(GuildState, Acc) when is_map(GuildState) ->
    Members = map_utils:ensure_list(maps:get(<<"members">>, GuildState, [])),
    lists:foldl(fun add_guild_member_user/2, Acc, Members);
add_guild_user(_GuildState, Acc) ->
    Acc.

-spec add_guild_member_user(term(), #{user_id() => map()}) -> #{user_id() => map()}.
add_guild_member_user(Member, Acc) when is_map(Member) ->
    add_user_by_id(normalize_user_safe(maps:get(<<"user">>, Member, undefined)), Acc);
add_guild_member_user(_Member, Acc) ->
    Acc.

-spec collect_guild_users([map()]) -> [map()].
collect_guild_users(GuildStates) ->
    lists:foldl(
        fun(GuildState, Acc) ->
            extract_guild_member_users(GuildState) ++ Acc
        end,
        [],
        GuildStates
    ).

-spec extract_guild_member_users(map()) -> [map()].
extract_guild_member_users(GuildState) ->
    Members = map_utils:ensure_list(maps:get(<<"members">>, GuildState, [])),
    normalize_users_safe([maps:get(<<"user">>, M, undefined) || M <- Members]).

-spec dedup_users([map()]) -> [map()].
dedup_users(Users) ->
    Map = lists:foldl(fun add_user_by_id/2, #{}, Users),
    maps:values(Map).

-spec add_user_by_id(term(), #{user_id() => map()}) -> #{user_id() => map()}.
add_user_by_id(undefined, Acc) ->
    Acc;
add_user_by_id(U, Acc) ->
    case user_id(U) of
        undefined -> Acc;
        Id -> Acc#{Id => U#{<<"id">> => Id}}
    end.

-spec strip_users_from_guild_members(map()) -> map().
strip_users_from_guild_members(GuildState) ->
    case maps:get(<<"unavailable">>, GuildState, false) of
        true ->
            GuildState;
        false ->
            Members = map_utils:ensure_list(maps:get(<<"members">>, GuildState, [])),
            StrippedMembers = [strip_user_from_member(M) || M <- Members],
            GuildState#{<<"members">> => StrippedMembers}
    end.

-spec strip_user_from_member(map()) -> map().
strip_user_from_member(Member) when is_map(Member) ->
    case maps:get(<<"user">>, Member, undefined) of
        User when is_map(User) -> replace_user_with_ref(Member, User);
        _ -> Member
    end;
strip_user_from_member(Member) ->
    Member.

-spec replace_user_with_ref(map(), map()) -> map().
replace_user_with_ref(Member, User) ->
    case user_ref(User) of
        undefined -> Member;
        Ref -> Member#{<<"user">> => Ref}
    end.

-spec user_ref(map()) -> map() | undefined.
user_ref(User) ->
    case normalize_user_safe(User) of
        undefined -> undefined;
        Normalized -> user_ref_from_normalized(Normalized)
    end.

-spec normalize_users_safe([term()]) -> [map()].
normalize_users_safe(Users) ->
    lists:filtermap(fun normalize_user_filter/1, Users).

-spec normalize_user_filter(term()) -> {true, map()} | false.
normalize_user_filter(User) ->
    case normalize_user_safe(User) of
        undefined -> false;
        Normalized -> {true, Normalized}
    end.

-spec normalize_user_safe(term()) -> map() | undefined.
normalize_user_safe(User) when is_map(User) ->
    try user_utils:normalize_user(User) of
        Normalized -> Normalized
    catch
        error:{invalid_snowflake, _} -> undefined
    end;
normalize_user_safe(_User) ->
    undefined.

-spec user_ref_from_normalized(map()) -> map() | undefined.
user_ref_from_normalized(Normalized) ->
    case maps:get(<<"id">>, Normalized, undefined) of
        undefined -> undefined;
        UserId -> #{<<"id">> => UserId}
    end.

-spec strip_user_from_relationships(map()) -> map().
strip_user_from_relationships(ReadyData) when is_map(ReadyData) ->
    Relationships = map_utils:ensure_list(maps:get(<<"relationships">>, ReadyData, [])),
    Stripped = [strip_user_from_relationship(R) || R <- Relationships],
    ReadyData#{<<"relationships">> => Stripped};
strip_user_from_relationships(ReadyData) ->
    ReadyData.

-spec strip_user_from_relationship(map()) -> map().
strip_user_from_relationship(Relationship) when is_map(Relationship) ->
    case maps:get(<<"user">>, Relationship, undefined) of
        User when is_map(User) ->
            UserId = user_id(User),
            RelWithoutUser = maps:remove(<<"user">>, Relationship),
            ensure_relationship_id(RelWithoutUser, UserId);
        _ ->
            Relationship
    end;
strip_user_from_relationship(Relationship) ->
    Relationship.

-spec ensure_relationship_id(map(), user_id() | undefined) -> map().
ensure_relationship_id(Rel, undefined) ->
    Rel;
ensure_relationship_id(Rel, UserId) ->
    case maps:get(<<"id">>, Rel, undefined) of
        undefined -> Rel#{<<"id">> => UserId};
        _ -> Rel
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

presence_user_id_rejects_malformed_id_test() ->
    ?assertEqual(undefined, presence_user_id(#{<<"user">> => #{<<"id">> => <<"001">>}})).

dedup_users_test() ->
    Users = [
        #{<<"id">> => <<"1">>, <<"username">> => <<"alice">>},
        #{<<"id">> => <<"2">>, <<"username">> => <<"bob">>},
        #{<<"id">> => <<"1">>, <<"username">> => <<"alice_duplicate">>}
    ],
    Result = dedup_users(Users),
    ?assertEqual(2, length(Result)),
    ok.

collect_presence_targets_deduplicates_before_fetch_test() ->
    State = #{
        user_id => 1,
        relationships => #{2 => 1, 3 => 3, 4 => 2},
        channels => #{
            10 => #{
                <<"type">> => 3,
                <<"recipients">> => [
                    #{<<"id">> => <<"1">>},
                    #{<<"id">> => <<"2">>},
                    #{<<"id">> => <<"5">>},
                    #{<<"id">> => <<"5">>}
                ]
            }
        }
    },
    ?assertEqual([2, 3, 5], lists:sort(collect_presence_targets(State, 1))).

collect_ready_users_collects_directly_into_dedup_map_test() ->
    UserA = #{<<"id">> => <<"10">>, <<"username">> => <<"a">>},
    UserB = #{<<"id">> => <<"11">>, <<"username">> => <<"b">>},
    State = #{
        bot => false,
        ready => #{<<"relationships">> => [#{<<"user">> => UserA}, not_a_relationship]},
        channels => #{1 => #{<<"type">> => 1, <<"recipients">> => [UserA, UserB]}}
    },
    Guilds = [#{<<"members">> => [#{<<"user">> => UserB}]}],
    Users = collect_ready_users(State, Guilds),
    ?assertEqual([10, 11], lists:sort([maps:get(<<"id">>, U) || U <- Users])).

collect_ready_users_skips_invalid_dm_recipient_ids_test() ->
    ValidUser = #{<<"id">> => <<"10">>, <<"username">> => <<"valid">>},
    InvalidUser = #{<<"id">> => <<"0">>, <<"username">> => <<"invalid">>},
    State = #{
        bot => false,
        ready => #{<<"relationships">> => []},
        channels => #{
            1 => #{<<"type">> => 1, <<"recipients">> => [InvalidUser, ValidUser]}
        }
    },
    Users = collect_ready_users(State, []),
    ?assertEqual([10], lists:sort([maps:get(<<"id">>, U) || U <- Users])).

collect_channel_users_skips_invalid_dm_recipient_ids_test() ->
    ValidUser = #{<<"id">> => <<"11">>, <<"username">> => <<"valid">>},
    InvalidUser = #{<<"id">> => <<"0">>, <<"username">> => <<"invalid">>},
    Channels = [#{<<"type">> => 3, <<"recipients">> => [InvalidUser, ValidUser]}],
    Users = collect_channel_users(Channels),
    ?assertEqual([11], lists:sort([maps:get(<<"id">>, U) || U <- Users])).

strip_user_from_member_test() ->
    Member = #{
        <<"user">> => #{<<"id">> => <<"123">>, <<"username">> => <<"test">>},
        <<"nick">> => <<"nickname">>
    },
    Stripped = strip_user_from_member(Member),
    ?assertEqual(#{<<"id">> => 123}, maps:get(<<"user">>, Stripped)),
    ?assertEqual(<<"nickname">>, maps:get(<<"nick">>, Stripped)),
    ok.

strip_user_from_member_keeps_bad_user_unchanged_test() ->
    Member = #{<<"user">> => #{<<"id">> => <<"bad">>, <<"username">> => <<"test">>}},
    ?assertEqual(Member, strip_user_from_member(Member)).

strip_user_from_relationship_test() ->
    Rel = #{
        <<"user">> => #{<<"id">> => <<"100">>, <<"username">> => <<"friend">>},
        <<"type">> => 1
    },
    Stripped = strip_user_from_relationship(Rel),
    ?assertEqual(undefined, maps:get(<<"user">>, Stripped, undefined)),
    ?assertEqual(100, maps:get(<<"id">>, Stripped)),
    ?assertEqual(1, maps:get(<<"type">>, Stripped)),
    ok.

-endif.
