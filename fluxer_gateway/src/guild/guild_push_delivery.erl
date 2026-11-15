%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_push_delivery).
-typing([eqwalizer]).

-export([compact/3, legacy/3]).

-type event_data() :: map().
-type guild_state() :: map().
-type guild_id() :: integer().
-type user_id() :: integer().
-type grace_sessions() :: guild_push_engagement:grace_sessions().
-type grace_hold() :: guild_push_engagement:grace_hold().
-type member_source() :: {ets, ets:tid()} | {map, map()}.
-type forum_request() :: #{
    message_data := event_data(),
    guild_id := guild_id(),
    channel_id := integer(),
    source := member_source(),
    format_data := fun(() -> map()),
    session_eligibility := map(),
    hold := grace_hold(),
    state := guild_state()
}.
-type legacy_request() :: #{
    message_data := event_data(),
    guild_id := guild_id(),
    members := map(),
    candidate_user_ids := [user_id()],
    channel_id := integer(),
    session_eligibility := map(),
    hold := grace_hold(),
    data := map(),
    state := guild_state()
}.

-spec compact(map(), integer(), map()) ->
    ok.
compact(MessageData, GuildId, State) ->
    case guild_dispatch_config:should_send_push_notifications(State) of
        false -> ok;
        true -> send_compact_push_notifications(MessageData, GuildId, State)
    end.

-spec send_compact_push_notifications(event_data(), guild_id(), guild_state()) -> ok.
send_compact_push_notifications(MessageData, GuildId, State) ->
    ChannelIdBin = maps:get(<<"channel_id">>, MessageData, undefined),
    case guild_dispatch_decorate:parse_snowflake(<<"channel_id">>, ChannelIdBin) of
        undefined -> ok;
        ChannelId -> scan_and_send_compact_push(MessageData, GuildId, ChannelId, State)
    end.

-spec scan_and_send_compact_push(event_data(), guild_id(), integer(), guild_state()) -> ok.
scan_and_send_compact_push(MessageData, GuildId, ChannelId, State) ->
    Context = guild_push_scan:context(MessageData, ChannelId, State),
    MembersTab = eqwalizer:dynamic_cast(maps:get(members_ets, State)),
    case guild_push_scan:members(MembersTab, Context, State) of
        {ok, #{eligible_user_ids := [], held_user_ids := []} = Scan} ->
            send_compact_forum_thread_created(
                MessageData, GuildId, ChannelId, MembersTab, Scan, State
            );
        {ok, Scan} ->
            ok = send_compact_scanned_push(MessageData, GuildId, Scan, State),
            send_compact_forum_thread_created(
                MessageData, GuildId, ChannelId, MembersTab, Scan, State
            );
        {error, table_unavailable} ->
            scan_table_unavailable(GuildId)
    end.

-spec send_compact_forum_thread_created(
    event_data(), guild_id(), integer(), ets:tid(), map(), guild_state()
) -> ok.
send_compact_forum_thread_created(MessageData, GuildId, ChannelId, MembersTab, Scan, State) ->
    send_forum_thread_created(#{
        message_data => MessageData,
        guild_id => GuildId,
        channel_id => ChannelId,
        source => {ets, MembersTab},
        format_data => fun() ->
            guild_push_message:compact_data(
                maps:get(data, State), maps:get(format_members, Scan)
            )
        end,
        session_eligibility => maps:get(session_eligibility, State, #{}),
        hold => maps:get(grace_hold, State, none),
        state => State
    }).

-spec send_forum_thread_created(forum_request()) -> ok.
send_forum_thread_created(
    #{
        message_data := MessageData,
        channel_id := ChannelId,
        state := State
    } = Request
) ->
    Starter = guild_thread_push:forum_starter(
        MessageData, ChannelId, maps:get(data, State, #{})
    ),
    send_forum_thread_created_push(Starter, Request).

-spec forum_push_route(user_id(), map(), map(), grace_sessions()) ->
    eligible_user_ids | held_user_ids | skip.
forum_push_route(UserId, Skip, _SessionEligibility, _HeldSessions) when
    is_map_key(UserId, Skip)
->
    skip;
forum_push_route(UserId, _Skip, SessionEligibility, HeldSessions) ->
    case maps:get(UserId, SessionEligibility, true) of
        true -> eligible_user_ids;
        false -> guild_push_engagement:grace_route(UserId, HeldSessions)
    end.

-spec send_forum_thread_created_push(
    {integer(), #{integer() => map()}} | undefined, forum_request()
) -> ok.
send_forum_thread_created_push(undefined, _Request) ->
    ok;
send_forum_thread_created_push({ForumId, ThreadMembers}, #{
    message_data := MessageData,
    guild_id := GuildId,
    channel_id := ChannelId,
    source := Source,
    format_data := FormatData,
    session_eligibility := SessionEligibility,
    hold := Hold,
    state := State
}) ->
    Skip = maps:merge(
        maps:put(guild_push_candidates:author_id(MessageData), true, ThreadMembers),
        guild_push_candidates:direct_mention_ids(MessageData)
    ),
    MentionRoles = guild_push_candidates:mention_role_ids(
        maps:get(<<"mention_roles">>, MessageData, [])
    ),
    Recipient = guild_thread_gate:recipient_active(ChannelId, State),
    HeldSessions = guild_push_engagement:held_sessions(Hold),
    CanView = fun guild_permissions_check:can_view_channel/4,
    Recipients = guild_push_candidates:filtermap_members(
        fun(UserId, Member) ->
            Route = forum_push_route(UserId, Skip, SessionEligibility, HeldSessions),
            Route =/= skip andalso
                not guild_push_candidates:has_mentioned_role(Member, MentionRoles) andalso
                not guild_members_common:is_member_bot(Member) andalso
                Recipient(UserId) andalso
                CanView(UserId, ForumId, Member, State) andalso
                {true, {Route, UserId, guild_push_message:role_ids(Member)}}
        end,
        Source
    ),
    Data = FormatData(),
    case Recipients of
        [] ->
            ok;
        _ ->
            guild_push_message:send(#{
                message_data => guild_thread_push:mark_forum_thread_created(MessageData),
                guild_id => GuildId,
                eligible_user_ids => [
                    UserId
                 || {eligible_user_ids, UserId, _RoleIds} <- Recipients
                ],
                user_roles => maps:from_list([
                    {UserId, RoleIds}
                 || {_, UserId, RoleIds} <- Recipients
                ]),
                connected_users => SessionEligibility,
                data => Data,
                large_guild_meta => guild_push_message:large_guild_meta(State),
                held => {
                    [UserId || {held_user_ids, UserId, _RoleIds} <- Recipients],
                    Hold,
                    maps:get(member_presence, State, undefined),
                    maps:get(guild_pid, State, undefined)
                }
            })
    end.

-spec scan_table_unavailable(guild_id()) -> ok.
scan_table_unavailable(GuildId) ->
    guild_push_metrics:count(scan_table_unavailable),
    logger:warning(
        "guild_push_members_table_unavailable: guild_id=~p phase=scan",
        [GuildId]
    ),
    ok.

-spec send_compact_scanned_push(event_data(), guild_id(), map(), guild_state()) -> ok.
send_compact_scanned_push(MessageData, GuildId, Scan, State) ->
    FormatData = guild_push_message:compact_data(
        maps:get(data, State), maps:get(format_members, Scan)
    ),
    guild_push_message:send(#{
        message_data => MessageData,
        guild_id => GuildId,
        eligible_user_ids => lists:reverse(maps:get(eligible_user_ids, Scan)),
        user_roles => maps:get(user_roles, Scan),
        connected_users => maps:get(session_eligibility, State),
        data => FormatData,
        large_guild_meta => guild_push_message:large_guild_meta(State),
        held => {
            lists:reverse(maps:get(held_user_ids, Scan, [])),
            maps:get(grace_hold, State, none),
            maps:get(member_presence, State, undefined),
            maps:get(guild_pid, State, undefined)
        }
    }).

-spec legacy(map(), integer(), map()) -> ok.
legacy(MessageData, GuildId, State) ->
    case guild_dispatch_config:should_send_push_notifications(State) of
        false -> ok;
        true -> send_push_notifications(MessageData, GuildId, State)
    end.

-spec send_push_notifications(event_data(), guild_id(), guild_state()) -> ok.
send_push_notifications(MessageData, GuildId, State) ->
    Data = maps:get(data, State),
    Members = guild_data_index_members:member_map(Data),
    Sessions = maps:get(sessions, State, #{}),
    {SessionEligibility, Hold} = guild_push_engagement:snapshot(Sessions, State),
    CandidateUserIds = guild_push_candidates:user_ids(Members, SessionEligibility, MessageData),
    ChannelIdBin = maps:get(<<"channel_id">>, MessageData, undefined),
    case guild_dispatch_decorate:parse_snowflake(<<"channel_id">>, ChannelIdBin) of
        undefined ->
            ok;
        ChannelId ->
            ok = send_to_eligible(#{
                message_data => MessageData,
                guild_id => GuildId,
                members => Members,
                candidate_user_ids => CandidateUserIds,
                channel_id => ChannelId,
                session_eligibility => SessionEligibility,
                hold => Hold,
                data => Data,
                state => State
            }),
            send_forum_thread_created(#{
                message_data => MessageData,
                guild_id => GuildId,
                channel_id => ChannelId,
                source => {map, Members},
                format_data => fun() -> Data end,
                session_eligibility => SessionEligibility,
                hold => Hold,
                state => State
            })
    end.

-spec send_to_eligible(legacy_request()) -> ok.
send_to_eligible(
    #{
        message_data := MessageData,
        guild_id := GuildId,
        members := Members,
        candidate_user_ids := CandidateUserIds,
        channel_id := ChannelId,
        session_eligibility := SessionEligibility,
        hold := Hold,
        data := Data,
        state := State
    } = Request
) ->
    ThreadCandidates = thread_candidate_filter(
        guild_thread_push:scope(ChannelId, State), Request
    ),
    EligibleUserIds = find_eligible_users_for_push(
        Members, ThreadCandidates(CandidateUserIds), ChannelId, State
    ),
    HeldUserIds = find_eligible_users_for_push(
        Members,
        ThreadCandidates(held_candidate_user_ids(Hold, CandidateUserIds)),
        ChannelId,
        State
    ),
    case {EligibleUserIds, HeldUserIds} of
        {[], []} ->
            ok;
        _ ->
            UserRolesMap = guild_push_message:user_roles(
                Members, EligibleUserIds ++ HeldUserIds
            ),
            guild_push_message:send(#{
                message_data => MessageData,
                guild_id => GuildId,
                eligible_user_ids => EligibleUserIds,
                user_roles => UserRolesMap,
                connected_users => SessionEligibility,
                data => Data,
                large_guild_meta => guild_push_message:large_guild_meta(State),
                held => {
                    HeldUserIds,
                    Hold,
                    maps:get(member_presence, State, undefined),
                    maps:get(guild_pid, State, undefined)
                }
            })
    end.

-spec thread_candidate_filter(guild_thread_push:scope(), legacy_request()) ->
    fun(([user_id()]) -> [user_id()]).
thread_candidate_filter(undefined, _Request) ->
    fun(UserIds) -> UserIds end;
thread_candidate_filter(Scope, #{
    channel_id := ChannelId,
    members := Members,
    message_data := MessageData,
    state := State
}) ->
    Mentions0 = #{
        direct_mentions => guild_push_candidates:direct_mention_ids(MessageData),
        mention_roles => guild_push_candidates:mention_role_ids(
            maps:get(<<"mention_roles">>, MessageData, [])
        ),
        author_id => guild_push_candidates:author_id(MessageData)
    },
    Mentions = Mentions0#{
        role_adds => guild_thread_push:role_adds(
            Scope,
            Mentions0,
            {map, Members},
            guild_thread_push:role_visible(ChannelId, State)
        )
    },
    fun(UserIds) ->
        [
            UserId
         || UserId <- UserIds,
            guild_thread_push:is_candidate(
                UserId, maps:get(UserId, Members, #{}), Scope, Mentions
            )
        ]
    end.

-spec held_candidate_user_ids(grace_hold(), [user_id()]) -> [user_id()].
held_candidate_user_ids(none, _CandidateUserIds) ->
    [];
held_candidate_user_ids({Held, _RecheckAt}, CandidateUserIds) ->
    Candidates = maps:from_keys(CandidateUserIds, true),
    [UserId || UserId <- maps:keys(Held), not maps:is_key(UserId, Candidates)].

-spec find_eligible_users_for_push(map(), [user_id()], integer(), guild_state()) -> [user_id()].
find_eligible_users_for_push(Members, CandidateUserIds, ChannelId, State) ->
    ThreadRecipient = guild_thread_gate:recipient_active(ChannelId, State),
    lists:filtermap(
        fun(UserId) ->
            ThreadRecipient(UserId) andalso is_push_eligible(UserId, Members, ChannelId, State)
        end,
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
    case guild_permissions_check:can_view_channel(UserId, ChannelId, Member, State) of
        true -> {true, UserId};
        false -> false
    end.
