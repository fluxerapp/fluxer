%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_dispatch_push).
-typing([eqwalizer]).

-export([
    maybe_send_push_notifications/4,
    collect_and_send_push_notifications/3
]).

-type event() :: atom().
-type event_data() :: map().
-type guild_state() :: map().
-type guild_id() :: integer().
-type user_id() :: integer().
-export_type([event/0, event_data/0, guild_state/0, guild_id/0]).

-spec maybe_send_push_notifications(event(), event_data(), guild_id(), guild_state()) -> ok.
maybe_send_push_notifications(message_create, FinalData, GuildId, UpdatedState) ->
    case maps:get(disable_push_notifications, UpdatedState, false) of
        true -> ok;
        false -> maybe_spawn_push(FinalData, GuildId, UpdatedState)
    end;
maybe_send_push_notifications(_Event, _FinalData, _GuildId, _UpdatedState) ->
    ok.

-spec maybe_spawn_push(event_data(), guild_id(), guild_state()) -> ok.
maybe_spawn_push(FinalData, GuildId, UpdatedState) ->
    case push_inflight_alive() of
        true -> ok;
        false -> spawn_push(FinalData, GuildId, UpdatedState)
    end.

-spec push_inflight_alive() -> boolean().
push_inflight_alive() ->
    case get(push_inflight) of
        Pid when is_pid(Pid) -> is_process_alive(Pid);
        _ -> false
    end.

-spec spawn_push(event_data(), guild_id(), guild_state()) -> ok.
spawn_push(FinalData, GuildId, UpdatedState) ->
    PushState = #{
        id => maps:get(id, UpdatedState, GuildId),
        data => maps:get(data, UpdatedState, #{}),
        sessions => maps:get(sessions, UpdatedState, #{}),
        virtual_channel_access => maps:get(virtual_channel_access, UpdatedState, #{})
    },
    Pid = spawn(fun() ->
        collect_and_send_push_notifications(FinalData, GuildId, PushState)
    end),
    put(push_inflight, Pid),
    ok.

-spec collect_and_send_push_notifications(event_data(), guild_id(), guild_state()) -> ok.
collect_and_send_push_notifications(MessageData, GuildId, State) ->
    case guild_dispatch_config:should_send_push_notifications(State) of
        false -> ok;
        true -> send_push_notifications(MessageData, GuildId, State)
    end.

-spec send_push_notifications(event_data(), guild_id(), guild_state()) -> ok.
send_push_notifications(MessageData, GuildId, State) ->
    Data = maps:get(data, State),
    Members = guild_data_index:member_map(Data),
    Sessions = maps:get(sessions, State, #{}),
    SessionEligibility = build_push_session_eligibility(Sessions),
    CandidateUserIds = push_candidate_user_ids(Members, SessionEligibility, MessageData),
    ChannelIdBin = maps:get(<<"channel_id">>, MessageData, undefined),
    case guild_dispatch_decorate:parse_snowflake(<<"channel_id">>, ChannelIdBin) of
        undefined ->
            ok;
        ChannelId ->
            send_to_eligible(
                MessageData,
                GuildId,
                Members,
                CandidateUserIds,
                ChannelId,
                SessionEligibility,
                Data,
                State
            )
    end.

-spec send_to_eligible(
    event_data(),
    guild_id(),
    map(),
    [user_id()],
    integer(),
    map(),
    map(),
    guild_state()
) -> ok.
send_to_eligible(
    MessageData,
    GuildId,
    Members,
    CandidateUserIds,
    ChannelId,
    SessionEligibility,
    Data,
    State
) ->
    case find_eligible_users_for_push(Members, CandidateUserIds, ChannelId, State) of
        [] ->
            ok;
        EligibleUserIds ->
            UserRolesMap = build_user_roles_map(Members, EligibleUserIds),
            send_push_to_eligible_users(
                MessageData,
                GuildId,
                EligibleUserIds,
                UserRolesMap,
                SessionEligibility,
                Data
            )
    end.

-spec push_candidate_user_ids(map(), map(), event_data()) -> [user_id()].
push_candidate_user_ids(Members, SessionEligibility, MessageData) ->
    push_candidate_user_ids(
        maps:get(<<"mention_everyone">>, MessageData, false),
        Members,
        SessionEligibility,
        MessageData
    ).

-spec push_candidate_user_ids(term(), map(), map(), event_data()) -> [user_id()].
push_candidate_user_ids(true, Members, _SessionEligibility, _MessageData) ->
    maps:keys(Members);
push_candidate_user_ids(_MentionEveryone, Members, SessionEligibility, MessageData) ->
    BaseAcc = base_candidate_acc(Members, SessionEligibility),
    {CandidateUserIds, _Seen} =
        add_mentioned_candidate_user_ids(Members, MessageData, BaseAcc),
    lists:reverse(CandidateUserIds).

-spec base_candidate_acc(map(), map()) -> {[user_id()], map()}.
base_candidate_acc(Members, SessionEligibility) ->
    maps:fold(
        fun(UserId, _Member, Acc) ->
            maybe_add_session_candidate(UserId, SessionEligibility, Acc)
        end,
        {[], #{}},
        Members
    ).

-spec maybe_add_session_candidate(user_id(), map(), {[user_id()], map()}) ->
    {[user_id()], map()}.
maybe_add_session_candidate(UserId, SessionEligibility, Acc) ->
    case maps:get(UserId, SessionEligibility, true) of
        true -> add_candidate_user_id(UserId, Acc);
        false -> Acc
    end.

-spec add_candidate_user_id(user_id(), {[user_id()], map()}) -> {[user_id()], map()}.
add_candidate_user_id(UserId, {UserIds, Seen} = Acc) ->
    case maps:is_key(UserId, Seen) of
        true -> Acc;
        false -> {[UserId | UserIds], Seen#{UserId => true}}
    end.

-spec add_mentioned_candidate_user_ids(map(), event_data(), {[user_id()], map()}) ->
    {[user_id()], map()}.
add_mentioned_candidate_user_ids(Members, MessageData, Acc) ->
    Acc1 = add_direct_mention_user_ids(MessageData, Acc),
    add_role_mention_user_ids(Members, MessageData, Acc1).

-spec add_direct_mention_user_ids(event_data(), {[user_id()], map()}) ->
    {[user_id()], map()}.
add_direct_mention_user_ids(MessageData, Acc) ->
    lists:foldl(
        fun add_direct_mention_user/2,
        Acc,
        maps:get(<<"mentions">>, MessageData, [])
    ).

-spec add_direct_mention_user(term(), {[user_id()], map()}) -> {[user_id()], map()}.
add_direct_mention_user(Mention, Acc) ->
    case direct_mention_user_id(Mention) of
        {true, UserId} -> add_candidate_user_id(UserId, Acc);
        false -> Acc
    end.

-spec direct_mention_user_id(term()) -> {true, user_id()} | false.
direct_mention_user_id(Mention) when is_map(Mention) ->
    case
        validation:validate_snowflake(<<"mention.id">>, maps:get(<<"id">>, Mention, undefined))
    of
        {ok, UserId} -> {true, UserId};
        _ -> false
    end;
direct_mention_user_id(_) ->
    false.

-spec add_role_mention_user_ids(map(), event_data(), {[user_id()], map()}) ->
    {[user_id()], map()}.
add_role_mention_user_ids(Members, MessageData, Acc) ->
    MentionRoleSet = mention_role_id_set(maps:get(<<"mention_roles">>, MessageData, [])),
    add_role_mention_user_ids_for_set(MentionRoleSet, Members, Acc).

-spec add_role_mention_user_ids_for_set(map(), map(), {[user_id()], map()}) ->
    {[user_id()], map()}.
add_role_mention_user_ids_for_set(MentionRoleSet, _Members, Acc) when
    map_size(MentionRoleSet) =:= 0
->
    Acc;
add_role_mention_user_ids_for_set(MentionRoleSet, Members, Acc) ->
    maps:fold(
        fun(UserId, Member, AccIn) ->
            maybe_add_role_mention_user(UserId, Member, MentionRoleSet, AccIn)
        end,
        Acc,
        Members
    ).

-spec maybe_add_role_mention_user(user_id(), map(), map(), {[user_id()], map()}) ->
    {[user_id()], map()}.
maybe_add_role_mention_user(UserId, Member, MentionRoleSet, Acc) ->
    HasMentionedRole = member_has_mentioned_role(Member, MentionRoleSet),
    case HasMentionedRole of
        true -> add_candidate_user_id(UserId, Acc);
        false -> Acc
    end.

-spec mention_role_id_set(list()) -> map().
mention_role_id_set(MentionRoles) ->
    lists:foldl(fun add_mention_role_id/2, #{}, MentionRoles).

-spec add_mention_role_id(term(), map()) -> map().
add_mention_role_id(RoleId, Acc) ->
    case snowflake_id:parse_optional(RoleId) of
        Id when is_integer(Id), Id > 0 -> Acc#{Id => true};
        _ -> Acc
    end.

-spec member_has_mentioned_role(map(), map()) -> boolean().
member_has_mentioned_role(Member, MentionRoleSet) ->
    member_roles_include_mentioned(maps:get(<<"roles">>, Member, []), MentionRoleSet).

-spec member_roles_include_mentioned(list(), map()) -> boolean().
member_roles_include_mentioned([], _MentionRoleSet) ->
    false;
member_roles_include_mentioned([Role | Rest], MentionRoleSet) ->
    case snowflake_id:parse_optional(Role) of
        RoleId when is_integer(RoleId), RoleId > 0 ->
            maps:is_key(RoleId, MentionRoleSet) orelse
                member_roles_include_mentioned(Rest, MentionRoleSet);
        _ ->
            member_roles_include_mentioned(Rest, MentionRoleSet)
    end.

-spec find_eligible_users_for_push(map(), [user_id()], integer(), guild_state()) -> [user_id()].
find_eligible_users_for_push(Members, CandidateUserIds, ChannelId, State) ->
    lists:filtermap(
        fun(UserId) -> is_push_eligible(UserId, Members, ChannelId, State) end,
        CandidateUserIds
    ).

-spec is_push_eligible(user_id(), map(), integer(), guild_state()) -> {true, user_id()} | false.
is_push_eligible(UserId, Members, ChannelId, State) ->
    case maps:get(UserId, Members, undefined) of
        undefined ->
            false;
        Member ->
            view_to_filtermap(UserId, ChannelId, Member, State)
    end.

-spec view_to_filtermap(
    user_id(), integer(), map(), guild_state()
) -> {true, user_id()} | false.
view_to_filtermap(UserId, ChannelId, Member, State) ->
    case guild_permissions:can_view_channel(UserId, ChannelId, Member, State) of
        true -> {true, UserId};
        false -> false
    end.

-spec build_push_session_eligibility(map()) -> #{user_id() => boolean()}.
build_push_session_eligibility(Sessions) ->
    maps:fold(
        fun(_Sid, Session, Acc) ->
            accumulate_session_eligibility(Session, Acc)
        end,
        #{},
        Sessions
    ).

-spec accumulate_session_eligibility(map(), #{user_id() => boolean()}) ->
    #{user_id() => boolean()}.
accumulate_session_eligibility(Session, Acc) ->
    case maps:get(user_id, Session, undefined) of
        UserId when is_integer(UserId) ->
            Acc#{
                UserId =>
                    maps:get(UserId, Acc, true) andalso maps:get(afk, Session, false)
            };
        _ ->
            Acc
    end.

-spec send_push_to_eligible_users(event_data(), guild_id(), [user_id()], map(), map(), map()) ->
    ok.
send_push_to_eligible_users(
    MessageData,
    GuildId,
    EligibleUserIds,
    UserRolesMap,
    ConnectedUsers,
    Data
) ->
    AuthorIdBin = maps:get(<<"id">>, maps:get(<<"author">>, MessageData, #{}), undefined),
    case guild_dispatch_decorate:parse_snowflake(<<"author.id">>, AuthorIdBin) of
        undefined ->
            ok;
        AuthorId ->
            Guild = maps:get(<<"guild">>, Data),
            ChannelIdBin = maps:get(<<"channel_id">>, MessageData),
            ChannelName = find_channel_name(ChannelIdBin, Data),
            RoleNames = build_role_names_map(Data),
            do_send_push(
                MessageData,
                GuildId,
                EligibleUserIds,
                UserRolesMap,
                ConnectedUsers,
                Guild,
                ChannelName,
                RoleNames,
                Data,
                AuthorId
            )
    end.

-spec do_send_push(
    event_data(),
    guild_id(),
    [user_id()],
    map(),
    map(),
    map(),
    binary(),
    map(),
    map(),
    integer()
) -> ok.
do_send_push(
    MessageData,
    GuildId,
    EligibleUserIds,
    UserRolesMap,
    ConnectedUsers,
    Guild,
    ChannelName,
    RoleNames,
    Data,
    AuthorId
) ->
    DefaultMessageNotifications = maps:get(<<"default_message_notifications">>, Guild, 0),
    GuildName = maps:get(<<"name">>, Guild, <<"Unknown">>),
    PushMessageData = add_parent_channel_id(MessageData, Data),
    push:handle_message_create(#{
        message_data => PushMessageData,
        user_ids => EligibleUserIds,
        guild_id => GuildId,
        author_id => AuthorId,
        guild_default_notifications => DefaultMessageNotifications,
        guild_name => GuildName,
        channel_name => ChannelName,
        role_names => RoleNames,
        markdown_context =>
            push_notification_format:build_markdown_context(
                MessageData, GuildId, RoleNames, Data
            ),
        user_roles => UserRolesMap,
        connected_users => ConnectedUsers
    }).

-spec add_parent_channel_id(event_data(), map()) -> event_data().
add_parent_channel_id(MessageData, Data) ->
    case maps:is_key(<<"parent_id">>, MessageData) of
        true ->
            MessageData;
        false ->
            add_parent_channel_id_from_channel(MessageData, Data)
    end.

-spec add_parent_channel_id_from_channel(event_data(), map()) -> event_data().
add_parent_channel_id_from_channel(MessageData, Data) ->
    ChannelIdBin = maps:get(<<"channel_id">>, MessageData, undefined),
    case guild_dispatch_decorate:parse_snowflake(<<"channel_id">>, ChannelIdBin) of
        undefined ->
            MessageData;
        ChannelId ->
            add_parent_channel_id_from_index(MessageData, ChannelId, Data)
    end.

-spec add_parent_channel_id_from_index(event_data(), integer(), map()) -> event_data().
add_parent_channel_id_from_index(MessageData, ChannelId, Data) ->
    Channels = guild_data_index:channel_index(Data),
    case maps:get(ChannelId, Channels, undefined) of
        undefined ->
            MessageData;
        Channel ->
            case maps:get(<<"parent_id">>, Channel, undefined) of
                ParentId when is_binary(ParentId), byte_size(ParentId) > 0 ->
                    MessageData#{<<"parent_id">> => ParentId};
                _ ->
                    MessageData
            end
    end.

-spec find_channel_name(binary(), map()) -> binary().
find_channel_name(ChannelIdBin, Data) ->
    case guild_dispatch_decorate:parse_snowflake(<<"channel_id">>, ChannelIdBin) of
        undefined ->
            <<"unknown">>;
        ChannelId ->
            lookup_channel_name(ChannelId, Data)
    end.

-spec lookup_channel_name(integer(), map()) -> binary().
lookup_channel_name(ChannelId, Data) ->
    Channels = guild_data_index:channel_index(Data),
    case maps:get(ChannelId, Channels, undefined) of
        undefined -> <<"unknown">>;
        Channel -> maps:get(<<"name">>, Channel, <<"unknown">>)
    end.

-spec build_role_names_map(map()) -> #{integer() => binary()}.
build_role_names_map(Data) ->
    maps:fold(
        fun add_role_name/3,
        #{},
        guild_data_index:role_index(Data)
    ).

-spec add_role_name(term(), term(), #{integer() => binary()}) -> #{integer() => binary()}.
add_role_name(RoleId, Role, Acc) when is_integer(RoleId), is_map(Role) ->
    case push_utils:normalize_binary(maps:get(<<"name">>, Role, undefined)) of
        Name when is_binary(Name), byte_size(Name) > 0 -> Acc#{RoleId => Name};
        _ -> Acc
    end;
add_role_name(_RoleId, _Role, Acc) ->
    Acc.

-spec build_user_roles_map(map(), [user_id()]) -> #{user_id() => [integer()]}.
build_user_roles_map(Members, EligibleUserIds) ->
    lists:foldl(
        fun(UserId, Acc) -> add_user_roles(UserId, Members, Acc) end,
        #{},
        EligibleUserIds
    ).

-spec add_user_roles(user_id(), map(), #{user_id() => [integer()]}) ->
    #{user_id() => [integer()]}.
add_user_roles(UserId, Members, Acc) ->
    case maps:get(UserId, Members, undefined) of
        undefined -> Acc;
        Member -> Acc#{UserId => extract_role_ids(Member)}
    end.

-spec extract_role_ids(map()) -> [integer()].
extract_role_ids(Member) ->
    Roles = maps:get(<<"roles">>, Member, []),
    lists:foldl(
        fun collect_role_id/2,
        [],
        Roles
    ).

-spec collect_role_id(term(), [integer()]) -> [integer()].
collect_role_id(Role, Acc) ->
    case validation:validate_snowflake(<<"role">>, Role) of
        {ok, RoleId} -> [RoleId | Acc];
        _ -> Acc
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

build_push_session_eligibility_test() ->
    Sessions = #{
        <<"s1">> => #{user_id => 1, mobile => false, afk => true},
        <<"s2">> => #{user_id => 1, mobile => false, afk => true},
        <<"s3">> => #{user_id => 2, mobile => true, afk => true},
        <<"s4">> => #{user_id => 3, mobile => false, afk => false}
    },
    Eligibility = build_push_session_eligibility(Sessions),
    ?assertEqual(true, maps:get(1, Eligibility)),
    ?assertEqual(true, maps:get(2, Eligibility)),
    ?assertEqual(false, maps:get(3, Eligibility)).

push_candidate_user_ids_prefers_sessionless_and_eligible_sessions_test() ->
    Members = #{1 => #{}, 2 => #{}, 3 => #{}, 4 => #{}},
    SessionEligibility = #{1 => false, 2 => true},
    CandidateUserIds = push_candidate_user_ids(Members, SessionEligibility, #{}),
    ?assertEqual([2, 3, 4], lists:sort(CandidateUserIds)).

push_candidate_user_ids_includes_connected_mentioned_users_test() ->
    Members = #{
        1 => #{<<"roles">> => [<<"10">>]},
        2 => #{<<"roles">> => [<<"20">>]},
        3 => #{<<"roles">> => []},
        4 => #{<<"roles">> => []}
    },
    SessionEligibility = #{1 => false, 2 => false, 3 => false, 4 => false},
    MessageData = #{
        <<"mentions">> => [#{<<"id">> => <<"3">>}],
        <<"mention_roles">> => [<<"10">>]
    },
    CandidateUserIds = push_candidate_user_ids(Members, SessionEligibility, MessageData),
    ?assertEqual([1, 3], lists:sort(CandidateUserIds)).

push_candidate_user_ids_includes_everyone_even_when_connected_test() ->
    Members = #{1 => #{}, 2 => #{}, 3 => #{}},
    SessionEligibility = #{1 => false, 2 => false, 3 => false},
    CandidateUserIds = push_candidate_user_ids(
        Members, SessionEligibility, #{<<"mention_everyone">> => true}
    ),
    ?assertEqual([1, 2, 3], lists:sort(CandidateUserIds)).

push_candidate_user_ids_deduplicates_mentions_test() ->
    Members = #{1 => #{<<"roles">> => [<<"10">>]}, 2 => #{<<"roles">> => []}},
    SessionEligibility = #{1 => false, 2 => false},
    MessageData = #{
        <<"mentions">> => [#{<<"id">> => <<"1">>}, #{<<"id">> => <<"1">>}],
        <<"mention_roles">> => [<<"10">>]
    },
    CandidateUserIds = push_candidate_user_ids(Members, SessionEligibility, MessageData),
    ?assertEqual([1], CandidateUserIds).

build_user_roles_map_uses_member_map_test() ->
    Members = #{
        1 => #{<<"roles">> => [<<"10">>, <<"11">>]},
        2 => #{<<"roles">> => [<<"20">>]}
    },
    Result = build_user_roles_map(Members, [2, 1]),
    ?assertEqual([10, 11], lists:sort(maps:get(1, Result))),
    ?assertEqual([20], maps:get(2, Result)).

find_channel_name_found_test() ->
    Data = #{
        <<"channels">> => [
            #{<<"id">> => <<"100">>, <<"name">> => <<"general">>},
            #{<<"id">> => <<"101">>, <<"name">> => <<"random">>}
        ]
    },
    ?assertEqual(<<"general">>, find_channel_name(<<"100">>, Data)).

find_channel_name_not_found_test() ->
    Data = #{<<"channels">> => []},
    ?assertEqual(<<"unknown">>, find_channel_name(<<"100">>, Data)).

find_channel_name_uses_index_test() ->
    Data = #{
        <<"channels">> => [
            #{<<"id">> => <<"100">>, <<"name">> => <<"general">>}
        ],
        <<"channel_index">> => #{100 => #{<<"id">> => <<"100">>, <<"name">> => <<"general">>}}
    },
    ?assertEqual(<<"general">>, find_channel_name(<<"100">>, Data)).

send_push_to_eligible_users_uses_full_data_for_channel_name_test() ->
    Self = self(),
    ok = meck:new(push, [passthrough, no_link]),
    try
        ok = meck:expect(push, handle_message_create, fun(Params) ->
            Self ! {push_params, Params},
            ok
        end),
        MessageData = #{
            <<"channel_id">> => <<"100">>,
            <<"author">> => #{<<"id">> => <<"42">>}
        },
        Data = #{
            <<"guild">> => #{
                <<"name">> => <<"Test Guild">>,
                <<"default_message_notifications">> => 0
            },
            <<"channels">> => [
                #{
                    <<"id">> => <<"100">>,
                    <<"name">> => <<"general">>,
                    <<"parent_id">> => <<"50">>
                }
            ],
            <<"channel_index">> => #{
                100 => #{
                    <<"id">> => <<"100">>,
                    <<"name">> => <<"general">>,
                    <<"parent_id">> => <<"50">>
                }
            },
            <<"roles">> => [
                #{<<"id">> => <<"200">>, <<"name">> => <<"Alerts">>}
            ],
            <<"role_index">> => #{
                200 => #{<<"id">> => <<"200">>, <<"name">> => <<"Alerts">>}
            }
        },
        ?assertEqual(
            ok, send_push_to_eligible_users(MessageData, 10, [1], #{1 => []}, #{}, Data)
        ),
        receive
            {push_params, Params} ->
                ?assertEqual(<<"general">>, maps:get(channel_name, Params)),
                ?assertEqual(<<"Test Guild">>, maps:get(guild_name, Params)),
                ?assertEqual(
                    <<"50">>, maps:get(<<"parent_id">>, maps:get(message_data, Params))
                ),
                ?assertEqual(#{200 => <<"Alerts">>}, maps:get(role_names, Params))
        after 1000 ->
            ?assert(false)
        end,
        ?assert(meck:validate(push))
    after
        meck:unload(push)
    end.

find_channel_name_invalid_id_test() ->
    Data = #{<<"channels">> => []},
    ?assertEqual(<<"unknown">>, find_channel_name(<<"invalid">>, Data)).

build_role_names_map_uses_role_index_test() ->
    Data = #{
        <<"roles">> => [
            #{<<"id">> => <<"100">>, <<"name">> => <<"Fallback">>}
        ],
        <<"role_index">> => #{
            100 => #{<<"id">> => <<"100">>, <<"name">> => <<"Mods">>},
            200 => #{<<"id">> => <<"200">>, <<"name">> => <<>>},
            bad => #{<<"id">> => <<"300">>, <<"name">> => <<"Bad">>}
        }
    },
    ?assertEqual(#{100 => <<"Mods">>}, build_role_names_map(Data)).

extract_role_ids_test() ->
    Member = #{<<"roles">> => [<<"10">>, <<"20">>, <<"invalid">>]},
    Result = lists:sort(extract_role_ids(Member)),
    ?assertEqual([10, 20], Result).

extract_role_ids_empty_test() ->
    Member = #{<<"roles">> => []},
    ?assertEqual([], extract_role_ids(Member)).

extract_role_ids_missing_key_test() ->
    Member = #{},
    ?assertEqual([], extract_role_ids(Member)).

-endif.
