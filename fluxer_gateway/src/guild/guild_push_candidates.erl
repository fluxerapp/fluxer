%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_push_candidates).
-typing([eqwalizer]).

-export([
    user_ids/3,
    is_candidate/3,
    direct_mention_ids/1,
    mention_role_ids/1,
    format_member_ids/1,
    author_id/1,
    has_mentioned_role/2,
    filtermap_members/2,
    lookup_member/2
]).

-define(MAX_FORMAT_MEMBERS, 50).

-type user_id() :: integer().
-type event_data() :: map().

-spec filtermap_members(
    fun((integer(), map()) -> false | {true, Value}), {ets, ets:tid()} | {map, map()}
) ->
    [Value].
filtermap_members(Fun, {ets, MembersTab}) ->
    ets:foldl(
        fun
            ({UserId, Member}, Acc) -> filtermap_member(UserId, Member, Fun, Acc);
            (_Row, Acc) -> Acc
        end,
        [],
        MembersTab
    );
filtermap_members(Fun, {map, Members}) ->
    maps:fold(
        fun(UserId, Member, Acc) -> filtermap_member(UserId, Member, Fun, Acc) end,
        [],
        Members
    ).

-spec filtermap_member(
    term(), term(), fun((user_id(), map()) -> false | {true, Value}), [Value]
) -> [Value].
filtermap_member(UserId, Member, Fun, Acc) when is_integer(UserId), is_map(Member) ->
    case Fun(UserId, Member) of
        {true, Value} -> [Value | Acc];
        false -> Acc
    end;
filtermap_member(_UserId, _Member, _Fun, Acc) ->
    Acc.

-spec lookup_member(integer(), {ets, ets:tid()} | {map, map()}) -> map() | undefined.
lookup_member(UserId, {ets, MembersTab}) ->
    case ets:lookup(MembersTab, UserId) of
        [{_, Member}] when is_map(Member) -> Member;
        _ -> undefined
    end;
lookup_member(UserId, {map, Members}) ->
    case maps:get(UserId, Members, undefined) of
        Member when is_map(Member) -> Member;
        _ -> undefined
    end.

-spec author_id(map()) -> integer() | undefined.
author_id(MessageData) ->
    case maps:get(<<"author">>, MessageData, undefined) of
        #{<<"id">> := Id} -> snowflake_id:parse_maybe(Id);
        _ -> undefined
    end.

-spec is_candidate(integer(), map(), map()) -> boolean().
is_candidate(UserId, Member, Context) ->
    maps:get(UserId, maps:get(session_eligibility, Context), true) orelse
        (not mentions_respect_active_session() andalso
            is_mention_candidate(UserId, Member, Context)).

-spec is_mention_candidate(user_id(), map(), map()) -> boolean().
is_mention_candidate(UserId, Member, Context) ->
    maps:get(mention_everyone, Context) orelse
        maps:is_key(UserId, maps:get(direct_mentions, Context)) orelse
        has_mentioned_role(Member, maps:get(mention_roles, Context)).

-spec mentions_respect_active_session() -> boolean().
mentions_respect_active_session() ->
    type_conv:is_true(
        application:get_env(fluxer_gateway, push_mentions_respect_active_session, true)
    ).

-spec direct_mention_ids(map()) -> map().
direct_mention_ids(MessageData) ->
    lists:foldl(
        fun add_direct_mention_id/2,
        #{},
        maps:get(<<"mentions">>, MessageData, [])
    ).

-spec add_direct_mention_id(term(), map()) -> map().
add_direct_mention_id(Mention, Acc) ->
    case direct_mention_user_id(Mention) of
        {true, UserId} -> Acc#{UserId => true};
        false -> Acc
    end.

-spec format_member_ids(map()) -> map().
format_member_ids(MessageData) ->
    AuthorSet = author_format_id_set(maps:get(<<"author">>, MessageData, undefined)),
    collect_format_mention_ids(
        maps:get(<<"mentions">>, MessageData, []),
        ?MAX_FORMAT_MEMBERS,
        AuthorSet
    ).

-spec author_format_id_set(term()) -> map().
author_format_id_set(Author) when is_map(Author) ->
    case direct_mention_user_id(Author) of
        {true, UserId} -> #{UserId => true};
        false -> #{}
    end;
author_format_id_set(_Author) ->
    #{}.

-spec collect_format_mention_ids([term()], non_neg_integer(), map()) -> map().
collect_format_mention_ids(_Mentions, 0, Acc) ->
    Acc;
collect_format_mention_ids([], _Remaining, Acc) ->
    Acc;
collect_format_mention_ids([Mention | Rest], Remaining, Acc) ->
    case direct_mention_user_id(Mention) of
        {true, UserId} -> collect_format_mention_id(UserId, Rest, Remaining, Acc);
        false -> collect_format_mention_ids(Rest, Remaining, Acc)
    end.

-spec collect_format_mention_id(user_id(), [term()], non_neg_integer(), map()) -> map().
collect_format_mention_id(UserId, Rest, Remaining, Acc) ->
    case maps:is_key(UserId, Acc) of
        true -> collect_format_mention_ids(Rest, Remaining, Acc);
        false -> collect_format_mention_ids(Rest, Remaining - 1, Acc#{UserId => true})
    end.

-spec user_ids(map(), map(), map()) -> [integer()].
user_ids(Members, SessionEligibility, MessageData) ->
    MentionEveryone = maps:get(<<"mention_everyone">>, MessageData, false),
    BaseAcc = base_candidate_acc(Members, SessionEligibility),
    {CandidateUserIds, _Seen} =
        case MentionEveryone of
            true -> BaseAcc;
            _ -> add_mentioned_candidate_user_ids(Members, MessageData, BaseAcc)
        end,
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
    MentionRoleSet = mention_role_ids(maps:get(<<"mention_roles">>, MessageData, [])),
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
    HasMentionedRole = has_mentioned_role(Member, MentionRoleSet),
    case HasMentionedRole of
        true -> add_candidate_user_id(UserId, Acc);
        false -> Acc
    end.

-spec mention_role_ids(list()) -> map().
mention_role_ids(MentionRoles) ->
    lists:foldl(fun add_mention_role_id/2, #{}, MentionRoles).

-spec add_mention_role_id(term(), map()) -> map().
add_mention_role_id(RoleId, Acc) ->
    case snowflake_id:parse_optional(RoleId) of
        Id when is_integer(Id), Id > 0 -> Acc#{Id => true};
        _ -> Acc
    end.

-spec has_mentioned_role(map(), map()) -> boolean().
has_mentioned_role(Member, MentionRoleSet) ->
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
