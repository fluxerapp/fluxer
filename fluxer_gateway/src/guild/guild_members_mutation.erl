%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_members_mutation).
-typing([eqwalizer]).

-export([
    resolve_all_mentions/2,
    resolve_mention_sources/2,
    resolve_mention_sources_page/2,
    resolve_channel_mentions/2
]).

-export_type([guild_state/0, guild_reply/1]).

-type guild_state() :: map().
-type guild_reply(T) :: {reply, T, guild_state()}.
-type member() :: map().
-type user_id() :: integer().
-type channel_id() :: integer().
-type role_id() :: integer().

-spec resolve_all_mentions(map(), guild_state()) -> guild_reply(map()).
resolve_all_mentions(Request, State) ->
    #{mention_everyone := MentionEveryone} = Request,
    Members = guild_members_common:guild_members(State),
    MemberMap = guild_data_index:member_map(guild_members_common:guild_data(State)),
    Context = request_mention_context(Request, State),
    UserIds =
        case MentionEveryone of
            true ->
                resolve_mentions(thread_universe_members(Members, Context), Context);
            false ->
                CandidateUserIds = candidate_user_ids(Context),
                resolve_mentions_for_ids(CandidateUserIds, MemberMap, Context)
        end,
    {reply, #{user_ids => UserIds}, State}.

-spec resolve_mention_sources(map(), guild_state()) -> guild_reply(map()).
resolve_mention_sources(Request, State) ->
    #{
        channel_id := ChannelId,
        author_id := AuthorId,
        mention_everyone := MentionEveryone,
        mention_here := MentionHere,
        role_ids := RoleIds,
        user_ids := DirectUserIds
    } = Request,
    RoleUserIds = collect_role_user_ids(RoleIds, AuthorId, ChannelId, State),
    DirectMentionedUserIds = collect_direct_user_ids(DirectUserIds, AuthorId, ChannelId, State),
    EveryoneUserIds =
        case MentionEveryone orelse MentionHere of
            true ->
                MassMentionRequest = Request#{role_ids => [], user_ids => []},
                {reply, #{user_ids := MassUserIds}, _} =
                    resolve_all_mentions(MassMentionRequest, State),
                MassUserIds;
            false ->
                []
        end,
    {reply,
        #{
            direct_user_ids => DirectMentionedUserIds,
            role_user_ids => RoleUserIds,
            everyone_user_ids => EveryoneUserIds
        },
        State}.

-spec collect_role_user_ids(term(), user_id(), channel_id(), guild_state()) -> [user_id()].
collect_role_user_ids(RoleIds, AuthorId, ChannelId, State) ->
    case guild_members_common:normalize_int_list(RoleIds) of
        [] ->
            [];
        RoleIdList ->
            Candidates = guild_members_roles:user_ids_for_any_role(RoleIdList, State),
            collect_user_mentions(Candidates, AuthorId, ChannelId, State)
    end.

-spec collect_direct_user_ids(term(), user_id(), channel_id(), guild_state()) -> [user_id()].
collect_direct_user_ids(DirectUserIds, AuthorId, ChannelId, State) ->
    case guild_members_common:normalize_int_list(DirectUserIds) of
        [] -> [];
        TargetIds -> collect_user_mentions(TargetIds, AuthorId, ChannelId, State)
    end.

-spec collect_user_mentions([user_id()], user_id(), channel_id(), guild_state()) -> [user_id()].
collect_user_mentions(UserIds, AuthorId, ChannelId, State) ->
    ThreadRecipient = guild_thread_gate:recipient_active(ChannelId, State),
    guild_members_common:collect_mentions_for_user_ids(
        UserIds, AuthorId, ChannelId, State, fun(UserId, _Member) -> ThreadRecipient(UserId) end
    ).

-spec resolve_mention_sources_page(map(), guild_state()) -> guild_reply(map()).
resolve_mention_sources_page(Request, State) ->
    #{limit := Limit, cursor := Cursor} = Request,
    Context = request_mention_context(Request, State),
    MemberMap = guild_data_index:member_map(guild_members_common:guild_data(State)),
    CandidateUserIds = paged_candidates(Context, MemberMap),
    CandidatesAfterCursor = drop_candidates_after_cursor(Cursor, CandidateUserIds),
    {Entries, NextCursor} = guild_members_search:take_mention_source_page(
        CandidatesAfterCursor, Limit, MemberMap, Context
    ),
    {reply, #{mentions => Entries, next_cursor => NextCursor}, State}.

-spec resolve_channel_mentions(map(), guild_state()) -> guild_reply(map()).
resolve_channel_mentions(#{channel_ids := ChannelIds}, State) ->
    Data = guild_members_common:guild_data(State),
    Channels =
        case guild_id(State, Data) of
            undefined -> [];
            GuildId -> resolve_channel_mentions_for_guild(ChannelIds, GuildId, Data)
        end,
    {reply, #{channels => Channels}, State}.

-spec request_mention_context(map(), guild_state()) -> map().
request_mention_context(Request, State) ->
    #{
        channel_id := ChannelId,
        author_id := AuthorId,
        mention_everyone := MentionEveryone,
        mention_here := MentionHere,
        role_ids := RoleIds,
        user_ids := DirectUserIds
    } = Request,
    build_mention_context(
        ChannelId, AuthorId, MentionEveryone, MentionHere, RoleIds, DirectUserIds, State
    ).

-spec drop_candidates_after_cursor(term(), [user_id()]) -> [user_id()].
drop_candidates_after_cursor(undefined, CandidateUserIds) ->
    CandidateUserIds;
drop_candidates_after_cursor(Cursor, CandidateUserIds) ->
    lists:dropwhile(fun(UserId) -> UserId =< Cursor end, CandidateUserIds).

-spec build_mention_context(
    channel_id(), user_id(), boolean(), boolean(), term(), term(), guild_state()
) -> map().
build_mention_context(
    ChannelId, AuthorId, MentionEveryone, MentionHere, RoleIds, DirectUserIds, State
) ->
    Sessions = maps:get(sessions, State, #{}),
    RoleIdSet = gb_sets:from_list(guild_members_common:normalize_int_list(RoleIds)),
    DirectUserIdSet = gb_sets:from_list(guild_members_common:normalize_int_list(DirectUserIds)),
    #{
        channel_id => ChannelId,
        author_id => AuthorId,
        mention_everyone => MentionEveryone,
        mention_here => MentionHere,
        has_role_mentions => not gb_sets:is_empty(RoleIdSet),
        has_direct_mentions => not gb_sets:is_empty(DirectUserIdSet),
        role_id_set => RoleIdSet,
        direct_user_id_set => DirectUserIdSet,
        connected_user_ids => guild_members_common:build_connected_user_ids(
            MentionHere, Sessions
        ),
        thread_universe => guild_thread_push:mention_universe(ChannelId, State),
        state => State
    }.

-spec thread_universe_members([member()], map()) -> [member()].
thread_universe_members(Members, #{thread_universe := Universe}) when is_map(Universe) ->
    [
        Member
     || Member <- Members,
        maps:is_key(guild_members_common:member_user_id(Member), Universe)
    ];
thread_universe_members(Members, _Context) ->
    Members.

-spec thread_here_set(gb_sets:set(user_id()), map()) -> gb_sets:set(user_id()).
thread_here_set(HereSet, #{thread_universe := Universe}) when is_map(Universe) ->
    gb_sets:filter(fun(UserId) -> maps:is_key(UserId, Universe) end, HereSet);
thread_here_set(HereSet, _Context) ->
    HereSet.

-spec thread_recipients([user_id()], map()) -> [user_id()].
thread_recipients(UserIds, #{
    thread_universe := Universe, channel_id := ChannelId, state := State
}) when
    is_map(Universe)
->
    Recipient = guild_thread_gate:recipient_active(ChannelId, State),
    [UserId || UserId <- UserIds, Recipient(UserId)];
thread_recipients(UserIds, _Context) ->
    UserIds.

-spec thread_recipient_set(gb_sets:set(user_id()), map()) -> gb_sets:set(user_id()).
thread_recipient_set(UserIdSet, #{thread_universe := Universe} = Context) when
    is_map(Universe)
->
    gb_sets:from_list(thread_recipients(gb_sets:to_list(UserIdSet), Context));
thread_recipient_set(UserIdSet, _Context) ->
    UserIdSet.

-spec resolve_mentions([member()], map()) -> [user_id()].
resolve_mentions(Members, Context) ->
    #{author_id := AuthorId} = Context,
    lists:filtermap(
        fun(Member) ->
            resolve_member_mention(Member, AuthorId, Context)
        end,
        Members
    ).

-spec resolve_member_mention(member(), user_id(), map()) -> {true, user_id()} | false.
resolve_member_mention(Member, AuthorId, Context) ->
    case guild_members_common:member_user_id(Member) of
        undefined -> false;
        AuthorId -> false;
        UserId -> check_member_mention(UserId, Member, Context)
    end.

-spec check_member_mention(user_id(), member(), map()) -> {true, user_id()} | false.
check_member_mention(UserId, Member, Context) ->
    case guild_members_common:is_member_bot(Member) of
        true -> false;
        false -> maybe_visible_member_mention(UserId, Member, Context)
    end.

-spec maybe_visible_member_mention(user_id(), member(), map()) -> {true, user_id()} | false.
maybe_visible_member_mention(UserId, Member, Context) ->
    #{channel_id := ChannelId, state := State} = Context,
    ShouldMention = guild_members_common:check_should_mention(UserId, Member, Context),
    CanView = guild_members_common:member_can_view_channel(UserId, ChannelId, Member, State),
    case ShouldMention andalso CanView of
        true -> {true, UserId};
        false -> false
    end.

-spec resolve_mentions_for_ids([user_id()], #{user_id() => member()}, map()) -> [user_id()].
resolve_mentions_for_ids(CandidateUserIds, MemberMap, Context) ->
    #{author_id := AuthorId} = Context,
    lists:filtermap(
        fun(UserId) ->
            resolve_candidate_mention(UserId, AuthorId, MemberMap, Context)
        end,
        lists:usort(CandidateUserIds)
    ).

-spec resolve_candidate_mention(user_id(), user_id(), #{user_id() => member()}, map()) ->
    {true, user_id()} | false.
resolve_candidate_mention(UserId, UserId, _MemberMap, _Context) ->
    false;
resolve_candidate_mention(UserId, _AuthorId, MemberMap, Context) ->
    check_candidate_mention(UserId, MemberMap, Context).

-spec check_candidate_mention(user_id(), #{user_id() => member()}, map()) ->
    {true, user_id()} | false.
check_candidate_mention(UserId, MemberMap, Context) ->
    case maps:get(UserId, MemberMap, undefined) of
        undefined -> false;
        Member -> check_member_mention(UserId, Member, Context)
    end.

-spec candidate_user_ids(map()) -> [user_id()].
candidate_user_ids(Context) ->
    #{
        mention_here := MentionHere,
        has_role_mentions := HasRoleMentions,
        has_direct_mentions := HasDirectMentions,
        role_id_set := RoleIdSet,
        direct_user_id_set := DirectUserIdSet,
        connected_user_ids := ConnectedUserIds,
        state := State
    } = Context,
    HereSet =
        case MentionHere of
            true -> thread_here_set(ConnectedUserIds, Context);
            false -> gb_sets:empty()
        end,
    RoleUsersSet =
        case HasRoleMentions of
            true ->
                RoleIds = gb_sets:to_list(RoleIdSet),
                RoleUserIds = guild_members_roles:user_ids_for_any_role(RoleIds, State),
                gb_sets:from_list(thread_recipients(RoleUserIds, Context));
            false ->
                gb_sets:empty()
        end,
    DirectSet =
        case HasDirectMentions of
            true -> thread_recipient_set(DirectUserIdSet, Context);
            false -> gb_sets:empty()
        end,
    gb_sets:to_list(gb_sets:union(HereSet, gb_sets:union(RoleUsersSet, DirectSet))).

-spec paged_candidates(map(), map()) -> [user_id()].
paged_candidates(
    #{mention_everyone := true, thread_universe := Universe} = Context, MemberMap
) when
    is_map(Universe)
->
    lists:usort(
        [UserId || UserId <- maps:keys(Universe), maps:is_key(UserId, MemberMap)] ++
            candidate_user_ids(Context)
    );
paged_candidates(#{mention_everyone := true}, MemberMap) ->
    lists:sort(maps:keys(MemberMap));
paged_candidates(Context, _MemberMap) ->
    lists:sort(candidate_user_ids(Context)).

-spec everyone_can_view(map(), role_id(), integer()) -> boolean().
everyone_can_view(Channel, GuildId, BasePerms) ->
    case permission_bits:has(BasePerms, constants:administrator_permission()) of
        true ->
            true;
        false ->
            Perms = apply_everyone_overwrites(BasePerms, Channel, GuildId),
            permission_bits:has(Perms, constants:view_channel_permission())
    end.

-spec build_channel_mention(map()) -> {true, map()} | false.
build_channel_mention(Channel) ->
    Id = snowflake_id:parse_optional(maps:get(<<"id">>, Channel, undefined)),
    Name = maps:get(<<"name">>, Channel, undefined),
    Type = guild_data_normalize_schema:int(maps:get(<<"type">>, Channel, undefined)),
    case {Id, Name, Type} of
        {I, N, T} when is_integer(I), is_binary(N), is_integer(T) ->
            {true, #{id => I, name => N, type => T}};
        _ ->
            false
    end.

-spec resolve_channel_mentions_for_guild(term(), role_id(), map()) -> [map()].
resolve_channel_mentions_for_guild(ChannelIds, GuildId, Data) ->
    Roles = guild_data_index:role_index(Data),
    BasePerms = role_permissions_for_id(Roles, GuildId),
    ChannelIndex = guild_data_index:channel_index(Data),
    lists:filtermap(
        fun(ChannelId) ->
            maybe_channel_mention(ChannelId, ChannelIndex, GuildId, BasePerms)
        end,
        guild_members_common:unique_preserve(parse_channel_ids(ChannelIds))
    ).

-spec parse_channel_ids(term()) -> [integer()].
parse_channel_ids(ChannelIds) when is_list(ChannelIds) ->
    lists:filtermap(fun snowflake_id:filter/1, ChannelIds);
parse_channel_ids(_) ->
    [].

-spec maybe_channel_mention(integer(), map(), role_id(), integer()) -> {true, map()} | false.
maybe_channel_mention(ChannelId, ChannelIndex, GuildId, BasePerms) ->
    case maps:get(ChannelId, ChannelIndex, undefined) of
        Channel when is_map(Channel) ->
            maybe_channel_mention_for_channel(Channel, GuildId, BasePerms);
        _ ->
            false
    end.

-spec maybe_channel_mention_for_channel(map(), role_id(), integer()) -> {true, map()} | false.
maybe_channel_mention_for_channel(#{<<"type">> := Type}, _GuildId, _BasePerms) when
    Type =:= 15; Type =:= 16
->
    false;
maybe_channel_mention_for_channel(Channel, GuildId, BasePerms) ->
    case everyone_can_view(Channel, GuildId, BasePerms) of
        true -> build_channel_mention(Channel);
        false -> false
    end.

-spec role_permissions_for_id(map(), role_id()) -> integer().
role_permissions_for_id(Roles, GuildId) ->
    case guild_permissions:find_role_by_id(GuildId, Roles) of
        undefined -> 0;
        Role -> permission_bits:parse(maps:get(<<"permissions">>, Role, undefined))
    end.

-spec apply_everyone_overwrites(integer(), map(), role_id()) -> integer().
apply_everyone_overwrites(BasePerms, Channel, GuildId) ->
    Overwrites = maps:get(<<"permission_overwrites">>, Channel, []),
    lists:foldl(
        fun(Overwrite, Acc) -> apply_everyone_overwrite(Overwrite, GuildId, Acc) end,
        BasePerms,
        map_utils:ensure_list(Overwrites)
    ).

-spec apply_everyone_overwrite(term(), role_id(), integer()) -> integer().
apply_everyone_overwrite(Overwrite, GuildId, Acc) when is_map(Overwrite) ->
    case
        {
            overwrite_id(Overwrite),
            overwrite_type(Overwrite),
            overwrite_allow(Overwrite),
            overwrite_deny(Overwrite)
        }
    of
        {GuildId, 0, Allow, Deny} when is_integer(Allow), is_integer(Deny) ->
            permission_bits:apply_allow_deny(Acc, Allow, Deny);
        _ ->
            Acc
    end;
apply_everyone_overwrite(_, _, Acc) ->
    Acc.

-spec guild_id(guild_state(), map()) -> role_id() | undefined.
guild_id(State, Data) ->
    Guild = map_utils:ensure_map(maps:get(<<"guild">>, Data, #{})),
    snowflake_id:first([
        maps:get(id, State, undefined),
        maps:get(<<"id">>, State, undefined),
        maps:get(<<"id">>, Guild, undefined)
    ]).

-spec overwrite_id(map()) -> integer() | undefined.
overwrite_id(Overwrite) ->
    snowflake_id:parse_optional(maps:get(<<"id">>, Overwrite, undefined)).

-spec overwrite_type(map()) -> integer() | undefined.
overwrite_type(Overwrite) ->
    guild_data_normalize_schema:int(maps:get(<<"type">>, Overwrite, undefined)).

-spec overwrite_allow(map()) -> integer() | undefined.
overwrite_allow(Overwrite) ->
    permission_bits:parse_optional(maps:get(<<"allow">>, Overwrite, undefined)).

-spec overwrite_deny(map()) -> integer() | undefined.
overwrite_deny(Overwrite) ->
    permission_bits:parse_optional(maps:get(<<"deny">>, Overwrite, undefined)).
