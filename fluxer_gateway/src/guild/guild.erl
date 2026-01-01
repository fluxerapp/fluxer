%% Copyright (C) 2026 Fluxer Contributors
%%
%% This file is part of Fluxer.
%%
%% Fluxer is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Affero General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% Fluxer is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%% GNU Affero General Public License for more details.
%%
%% You should have received a copy of the GNU Affero General Public License
%% along with Fluxer. If not, see <https://www.gnu.org/licenses/>.

-module(guild).
-behaviour(gen_server).

-export([start_link/1, update_counts/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-import(guild_permissions, [
    get_member_permissions/3,
    get_max_role_position/2,
    can_view_channel/4
]).
-import(type_conv, [to_integer/1]).
-import(guild_voice, [
    voice_state_update/2,
    get_voice_state/2,
    update_member_voice/2,
    disconnect_voice_user/2,
    disconnect_voice_user_if_in_channel/2,
    disconnect_all_voice_users_in_channel/2,
    confirm_voice_connection_from_livekit/2,
    move_member/2
]).
-import(guild_data, [
    get_guild_data/2,
    get_guild_member/2,
    list_guild_members/2,
    get_vanity_url_channel/1,
    get_first_viewable_text_channel/1
]).
-import(guild_members, [
    get_users_to_mention_by_roles/2,
    get_users_to_mention_by_user_ids/2,
    get_all_users_to_mention/2,
    resolve_all_mentions/2,
    get_members_with_role/2,
    can_manage_roles/2,
    get_assignable_roles/2,
    check_target_member/2,
    get_viewable_channels/2
]).
-import(guild_sessions, [
    handle_session_connect/3,
    handle_session_down/2,
    set_session_active_guild/3,
    set_session_passive_guild/3
]).
-import(guild_dispatch, [
    handle_dispatch/3
]).

start_link(GuildState) ->
    gen_server:start_link(?MODULE, GuildState, []).

init(GuildState) ->
    process_flag(trap_exit, true),
    StateWithVoice =
        case maps:is_key(voice_states, GuildState) of
            true -> GuildState;
            false -> maps:put(voice_states, #{}, GuildState)
        end,
    StateWithPresenceSubs = maps:put(presence_subscriptions, #{}, StateWithVoice),
    StateWithMemberListSubs = maps:put(member_list_subscriptions, #{}, StateWithPresenceSubs),
    StateWithMemberSubs = maps:put(
        member_subscriptions, guild_subscriptions:init_state(), StateWithMemberListSubs
    ),
    Data = maps:get(data, StateWithMemberSubs, #{}),
    Members = maps:get(<<"members">>, Data, []),
    MemberCount = length(Members),
    OnlineCount = count_online_members(Members),
    StateWithCounts = maps:put(member_count, MemberCount, StateWithMemberSubs),
    StateWithPresences = maps:put(presences, #{}, maps:put(online_count, OnlineCount, StateWithCounts)),
    guild_passive_sync:schedule_passive_sync(StateWithPresences),
    {ok, StateWithPresences}.

handle_call({session_connect, Request}, {CallerPid, _}, State) ->
    SessionPid = maps:get(session_pid, Request, CallerPid),
    guild_sessions:handle_session_connect(Request, SessionPid, State);
handle_call({get_counts}, _From, State) ->
    MemberCount = maps:get(member_count, State, 0),
    OnlineCount = maps:get(online_count, State, 0),
    {reply, #{member_count => MemberCount, presence_count => OnlineCount}, State};
handle_call({get_large_guild_metadata}, _From, State) ->
    MemberCount = maps:get(member_count, State, 0),
    Data = maps:get(data, State, #{}),
    Guild = maps:get(<<"guild">>, Data, #{}),
    Features = maps:get(<<"features">>, Guild, []),
    {reply, #{member_count => MemberCount, features => Features}, State};
handle_call({get_users_to_mention_by_roles, Request}, _From, State) ->
    guild_members:get_users_to_mention_by_roles(Request, State);
handle_call({get_users_to_mention_by_user_ids, Request}, _From, State) ->
    guild_members:get_users_to_mention_by_user_ids(Request, State);
handle_call({get_all_users_to_mention, Request}, _From, State) ->
    guild_members:get_all_users_to_mention(Request, State);
handle_call({resolve_all_mentions, Request}, _From, State) ->
    guild_members:resolve_all_mentions(Request, State);
handle_call({get_members_with_role, Request}, _From, State) ->
    guild_members:get_members_with_role(Request, State);
handle_call({check_permission, Request}, _From, State) ->
    #{user_id := UserId, permission := Permission, channel_id := ChannelId} = Request,
    true = is_integer(Permission),
    HasPermission =
        case owner_id(State) =:= UserId of
            true ->
                true;
            false ->
                Permissions = get_member_permissions(UserId, ChannelId, State),
                (Permissions band Permission) =:= Permission
        end,
    {reply, #{has_permission => HasPermission}, State};
handle_call({get_user_permissions, Request}, _From, State) ->
    #{user_id := UserId, channel_id := ChannelId} = Request,
    Permissions = get_member_permissions(UserId, ChannelId, State),
    {reply, #{permissions => Permissions}, State};
handle_call({can_manage_roles, Request}, _From, State) ->
    guild_members:can_manage_roles(Request, State);
handle_call({can_manage_role, Request}, _From, State) ->
    guild_members:can_manage_role(Request, State);
handle_call({get_guild_data, Request}, _From, State) ->
    guild_data:get_guild_data(Request, State);
handle_call({get_assignable_roles, Request}, _From, State) ->
    guild_members:get_assignable_roles(Request, State);
handle_call({get_user_max_role_position, Request}, _From, State) ->
    #{user_id := UserId} = Request,
    Position = get_max_role_position(UserId, State),
    {reply, #{position => Position}, State};
handle_call({check_target_member, Request}, _From, State) ->
    guild_members:check_target_member(Request, State);
handle_call({get_viewable_channels, Request}, _From, State) ->
    guild_members:get_viewable_channels(Request, State);
handle_call({get_guild_member, Request}, _From, State) ->
    guild_data:get_guild_member(Request, State);
handle_call({has_member, Request}, _From, State) ->
    guild_data:has_member(Request, State);
handle_call({list_guild_members, Request}, _From, State) ->
    guild_data:list_guild_members(Request, State);
handle_call({get_vanity_url_channel}, _From, State) ->
    guild_data:get_vanity_url_channel(State);
handle_call({get_first_viewable_text_channel}, _From, State) ->
    guild_data:get_first_viewable_text_channel(State);
handle_call({voice_state_update, Request}, _From, State) ->
    guild_voice:voice_state_update(Request, State);
handle_call({get_voice_state, Request}, _From, State) ->
    guild_voice:get_voice_state(Request, State);
handle_call({update_member_voice, Request}, _From, State) ->
    guild_voice:update_member_voice(Request, State);
handle_call({disconnect_voice_user, Request}, _From, State) ->
    guild_voice:disconnect_voice_user(Request, State);
handle_call({disconnect_voice_user_if_in_channel, Request}, _From, State) ->
    guild_voice:disconnect_voice_user_if_in_channel(Request, State);
handle_call({disconnect_all_voice_users_in_channel, Request}, _From, State) ->
    guild_voice:disconnect_all_voice_users_in_channel(Request, State);
handle_call({confirm_voice_connection_from_livekit, Request}, _From, State) ->
    guild_voice:confirm_voice_connection_from_livekit(Request, State);
handle_call({move_member, Request}, _From, State) ->
    guild_voice:move_member(Request, State);
handle_call({switch_voice_region, Request}, _From, State) ->
    guild_voice:switch_voice_region_handler(Request, State);
handle_call({get_sessions}, _From, State) ->
    {reply, State, State};
handle_call({get_category_channel_count, Request}, _From, State) ->
    #{category_id := CategoryId} = Request,
    Data = maps:get(data, State),
    Channels = maps:get(<<"channels">>, Data, []),
    Count = length([
        Ch
     || Ch <- Channels,
        map_utils:get_integer(Ch, <<"parent_id">>, undefined) =:= CategoryId
    ]),
    {reply, #{count => Count}, State};
handle_call({get_channel_count}, _From, State) ->
    Data = maps:get(data, State),
    Channels = maps:get(<<"channels">>, Data, []),
    Count = length(Channels),
    {reply, #{count => Count}, State};
handle_call({reload, NewData}, _From, State) ->
    OldData = maps:get(data, State),
    NewState0 = maps:put(data, NewData, State),

    GuildId = maps:get(id, State),
    NewGuild = maps:get(<<"guild">>, NewData, #{}),
    Sessions = maps:get(sessions, State, #{}),
    Pids = [maps:get(pid, S) || {_Sid, S} <- maps:to_list(Sessions)],
    EventData = maps:put(<<"guild_id">>, integer_to_binary(GuildId), NewGuild),
    lists:foreach(
        fun(Pid) ->
            gen_server:cast(Pid, {dispatch, guild_update, EventData})
        end,
        Pids
    ),

    NewState = cleanup_removed_member_subscriptions(OldData, NewData, NewState0),

    {reply, ok, NewState};
handle_call({dispatch, Request}, _From, State) ->
    #{event := Event, data := EventData} = Request,
    ParsedEventData =
        case is_binary(EventData) of
            true -> jsx:decode(EventData, [{return_maps, true}]);
            false -> EventData
        end,
    {noreply, NewState} = handle_dispatch(Event, ParsedEventData, State),
    StateAfterPrune = prune_invalid_member_subscriptions(NewState),
    {reply, ok, StateAfterPrune};
handle_call({terminate}, _From, State) ->
    {stop, normal, ok, State};
handle_call({lazy_subscribe, Request}, _From, State) ->
    #{session_id := SessionId, channel_id := ChannelId, ranges := Ranges} = Request,
    Sessions0 = maps:get(sessions, State, #{}),
    SessionUserId =
        case maps:get(SessionId, Sessions0, undefined) of
            #{user_id := Uid} -> Uid;
            _ -> undefined
        end,
    case is_integer(SessionUserId) andalso
             can_view_channel(SessionUserId, ChannelId, undefined, State) of
        true ->
            GuildId = maps:get(id, State),
            ListId = guild_member_list:calculate_list_id(ChannelId, State),
            {NewState, ShouldSendSync, NormalizedRanges} =
                guild_member_list:subscribe_ranges(SessionId, ListId, Ranges, State),
            case {ShouldSendSync, NormalizedRanges} of
                {true, []} ->
                    {reply, ok, NewState};
                {true, RangesToSend} ->
                    SyncResponse = guild_member_list:build_sync_response(GuildId, ListId, RangesToSend, NewState),
                    SyncResponseWithChannel = maps:put(<<"channel_id">>, integer_to_binary(ChannelId), SyncResponse),
                    Sessions = maps:get(sessions, NewState, #{}),
                    case maps:get(SessionId, Sessions, undefined) of
                        #{pid := SessionPid} when is_pid(SessionPid) ->
                            gen_server:cast(SessionPid, {dispatch, guild_member_list_update, SyncResponseWithChannel});
                        _ ->
                            ok
                    end,
                    {reply, ok, NewState};
                _ ->
                    {reply, ok, NewState}
            end;
        false ->
            {reply, ok, State}
    end;
handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast({dispatch, Request}, State) ->
    #{event := Event, data := EventData} = Request,
    ParsedEventData =
        case is_binary(EventData) of
            true -> jsx:decode(EventData, [{return_maps, true}]);
            false -> EventData
        end,
    handle_dispatch(Event, ParsedEventData, State);
handle_cast({store_pending_connection, ConnectionId, Metadata}, State) ->
    PendingConnections = maps:get(pending_voice_connections, State, #{}),
    NewPendingConnections = maps:put(ConnectionId, Metadata, PendingConnections),
    NewState = maps:put(pending_voice_connections, NewPendingConnections, State),
    {noreply, NewState};
handle_cast({add_virtual_channel_access, UserId, ChannelId}, State) ->
    NewState = guild_virtual_channel_access:add_virtual_access(UserId, ChannelId, State),
    guild_virtual_channel_access:dispatch_channel_visibility_change(
        UserId, ChannelId, add, NewState
    ),
    {noreply, NewState};
handle_cast({remove_virtual_channel_access, UserId, ChannelId}, State) ->
    guild_virtual_channel_access:dispatch_channel_visibility_change(
        UserId, ChannelId, remove, State
    ),
    NewState = guild_virtual_channel_access:remove_virtual_access(UserId, ChannelId, State),
    {noreply, NewState};
handle_cast({cleanup_virtual_access_for_user, UserId}, State) ->
    NewState = guild_voice_disconnect:cleanup_virtual_channel_access_for_user(UserId, State),
    {noreply, NewState};
handle_cast({set_session_active, SessionId}, State) ->
    GuildId = maps:get(id, State),
    NewState = set_session_active_guild(SessionId, GuildId, State),
    {noreply, NewState};
handle_cast({set_session_passive, SessionId}, State) ->
    GuildId = maps:get(id, State),
    NewState = set_session_passive_guild(SessionId, GuildId, State),
    {noreply, NewState};
handle_cast({update_member_subscriptions, SessionId, MemberIds}, State) ->
    MemberSubs = maps:get(member_subscriptions, State, guild_subscriptions:init_state()),
    Sessions = maps:get(sessions, State, #{}),
    SessionUserId =
        case maps:get(SessionId, Sessions, undefined) of
            undefined -> undefined;
            SessionData -> maps:get(user_id, SessionData, undefined)
        end,
    FilteredMemberIds = filter_member_ids_with_mutual_channels(SessionUserId, MemberIds, State),
    OldSubscriptions = guild_subscriptions:get_user_ids_for_session(SessionId, MemberSubs),
    NewMemberSubs = guild_subscriptions:update_subscriptions(SessionId, FilteredMemberIds, MemberSubs),
    NewSubscriptions = guild_subscriptions:get_user_ids_for_session(SessionId, NewMemberSubs),
    Added = sets:to_list(sets:subtract(NewSubscriptions, OldSubscriptions)),
    Removed = sets:to_list(sets:subtract(OldSubscriptions, NewSubscriptions)),
    State1 = maps:put(member_subscriptions, NewMemberSubs, State),
    State2 = lists:foldl(
        fun(UserId, Acc) ->
            StateWithPresence = guild_sessions:subscribe_to_user_presence(UserId, Acc),
            guild_presence:send_cached_presence_to_session(UserId, SessionId, StateWithPresence)
        end,
        State1,
        Added
    ),
    State3 = lists:foldl(
        fun(UserId, Acc) -> guild_sessions:unsubscribe_from_user_presence(UserId, Acc) end,
        State2,
        Removed
    ),
    {noreply, State3};
handle_cast({set_session_typing_override, SessionId, TypingFlag}, State) ->
    GuildId = maps:get(id, State),
    Sessions = maps:get(sessions, State, #{}),
    case maps:get(SessionId, Sessions, undefined) of
        undefined ->
            {noreply, State};
        SessionData ->
            NewSessionData = session_passive:set_typing_override(GuildId, TypingFlag, SessionData),
            NewSessions = maps:put(SessionId, NewSessionData, Sessions),
            NewState = maps:put(sessions, NewSessions, State),
            logger:debug("[guild] Set typing override to ~p for session ~p in guild ~p", [
                TypingFlag, SessionId, GuildId
            ]),
            {noreply, NewState}
    end;
handle_cast({send_guild_sync, SessionId}, State) ->
    GuildId = maps:get(id, State),
    Sessions = maps:get(sessions, State, #{}),
    case maps:get(SessionId, Sessions, undefined) of
        undefined ->
            logger:warning("[guild] Session ~p not found for send_guild_sync", [SessionId]),
            {noreply, State};
        SessionData ->
            case session_passive:is_guild_synced(GuildId, SessionData) of
                true ->
                    logger:debug("[guild] Guild ~p already synced for session ~p, skipping", [GuildId, SessionId]),
                    {noreply, State};
                false ->
                    UserId = maps:get(user_id, SessionData),
                    SessionPid = maps:get(pid, SessionData),
                    GuildData = guild_data:get_guild_state(UserId, State),
                    gen_server:cast(SessionPid, {dispatch, guild_sync, GuildData}),
                    NewSessionData = session_passive:mark_guild_synced(GuildId, SessionData),
                    NewSessions = maps:put(SessionId, NewSessionData, Sessions),
                    {noreply, maps:put(sessions, NewSessions, State)}
            end
    end;
handle_cast({send_members_chunk, SessionId, ChunkData}, State) ->
    GuildId = maps:get(id, State),
    Sessions = maps:get(sessions, State, #{}),
    case maps:get(SessionId, Sessions, undefined) of
        undefined ->
            logger:warning("[guild] Session ~p not found for send_members_chunk", [SessionId]),
            {noreply, State};
        SessionData ->
            SessionPid = maps:get(pid, SessionData),
            ChunkWithGuildId = maps:put(<<"guild_id">>, integer_to_binary(GuildId), ChunkData),
            gen_server:cast(SessionPid, {dispatch, guild_members_chunk, ChunkWithGuildId}),
            {noreply, State}
    end;
handle_cast(_, State) ->
    {noreply, State}.

handle_info({presence, UserId, Payload}, State) ->
    guild_presence:handle_bus_presence(UserId, Payload, State);
handle_info({'DOWN', Ref, process, _Pid, _Reason}, State) ->
    guild_sessions:handle_session_down(Ref, State);
handle_info(passive_sync, State) ->
    guild_passive_sync:handle_passive_sync(State);
handle_info(_, State) ->
    {noreply, State}.

filter_member_ids_with_mutual_channels(SessionUserId, MemberIds, State) ->
    case SessionUserId of
        undefined ->
            [];
        _ ->
            SessionChannels = guild_visibility:viewable_channel_set(SessionUserId, State),
            lists:filtermap(
                fun(MemberId) ->
                    case MemberId =:= SessionUserId of
                        true -> false;
                        false ->
                            case has_shared_channels(SessionChannels, MemberId, State) of
                                true -> {true, MemberId};
                                false -> false
                            end
                    end
                end,
                MemberIds
            )
    end.

has_shared_channels(_, MemberId, _) when not is_integer(MemberId) ->
    false;
has_shared_channels(SessionChannels, MemberId, State) ->
    CandidateChannels = guild_visibility:viewable_channel_set(MemberId, State),
    not sets:is_empty(sets:intersection(SessionChannels, CandidateChannels)).

prune_invalid_member_subscriptions(State) ->
    MemberSubs = maps:get(member_subscriptions, State, guild_subscriptions:init_state()),
    Sessions = maps:get(sessions, State, #{}),
    InvalidPairs = build_invalid_subscription_pairs(MemberSubs, Sessions, State),
    lists:foldl(
        fun({SessionId, UserId}, AccState) ->
            remove_member_subscription(SessionId, UserId, AccState)
        end,
        State,
        InvalidPairs
    ).

build_invalid_subscription_pairs(MemberSubs, Sessions, State) ->
    lists:foldl(
        fun({SessionId, SessionData}, Acc) ->
            SessionUserId = maps:get(user_id, SessionData, undefined),
            case SessionUserId of
                undefined ->
                    Acc;
                _ ->
                    SessionChannels = guild_visibility:viewable_channel_set(SessionUserId, State),
                    SubscriptionIds = guild_subscriptions:get_user_ids_for_session(SessionId, MemberSubs),
                    InvalidIds =
                        [MemberId
                         || MemberId <- sets:to_list(SubscriptionIds),
                            not has_shared_channels(SessionChannels, MemberId, State)
                        ],
                    lists:foldl(
                        fun(MemberId, Pairs) -> [{SessionId, MemberId} | Pairs] end,
                        Acc,
                        InvalidIds
                    )
            end
        end,
        [],
        maps:to_list(Sessions)
    ).

remove_member_subscription(SessionId, UserId, State) ->
    MemberSubs = maps:get(member_subscriptions, State, guild_subscriptions:init_state()),
    NewMemberSubs = guild_subscriptions:unsubscribe(SessionId, UserId, MemberSubs),
    State1 = maps:put(member_subscriptions, NewMemberSubs, State),
    guild_sessions:unsubscribe_from_user_presence(UserId, State1).

terminate(Reason, State) when is_map(State) ->
    PresenceSubs = maps:get(presence_subscriptions, State, #{}),
    lists:foreach(
        fun(UserId) ->
            presence_bus:unsubscribe(UserId)
        end,
        maps:keys(PresenceSubs)
    ),
    maybe_report_crash(Reason, State),
    ok;
terminate(Reason, State) ->
    maybe_report_crash(Reason, State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

maybe_report_crash(normal, _State) ->
    ok;
maybe_report_crash(shutdown, _State) ->
    ok;
maybe_report_crash({shutdown, _}, _State) ->
    ok;
maybe_report_crash(Reason, State) ->
    GuildId =
        case State of
            #{id := Id} ->
                integer_to_binary(Id);
            #{data := Data} when is_map(Data) ->
                case maps:get(<<"id">>, Data, undefined) of
                    undefined -> <<"unknown">>;
                    Id -> Id
                end;
            _ ->
                <<"unknown">>
        end,
    Stacktrace = iolist_to_binary(io_lib:format("~p", [Reason])),
    metrics_client:crash(GuildId, Stacktrace),
    ok.

cleanup_removed_member_subscriptions(OldData, NewData, State) ->
    OldMembers = maps:get(<<"members">>, OldData, []),
    NewMembers = maps:get(<<"members">>, NewData, []),

    OldMemberIds = sets:from_list([member_user_id(M) || M <- OldMembers]),
    NewMemberIds = sets:from_list([member_user_id(M) || M <- NewMembers]),

    RemovedIds = sets:subtract(OldMemberIds, NewMemberIds),

    PresenceSubs = maps:get(presence_subscriptions, State, #{}),
    NewPresenceSubs = lists:foldl(
        fun(UserId, Subs) ->
            case maps:is_key(UserId, Subs) of
                true ->
                    presence_bus:unsubscribe(UserId),
                    maps:remove(UserId, Subs);
                false ->
                    Subs
            end
        end,
        PresenceSubs,
        sets:to_list(RemovedIds)
    ),
    maps:put(presence_subscriptions, NewPresenceSubs, State).

member_user_id(Member) ->
    User = maps:get(<<"user">>, Member, #{}),
    map_utils:get_integer(User, <<"id">>, undefined).

owner_id(State) ->
    case resolve_data_map(State) of
        undefined ->
            0;
        Data ->
            Guild = maps:get(<<"guild">>, Data, #{}),
            to_integer(maps:get(<<"owner_id">>, Guild, <<"0">>))
    end.

resolve_data_map(State) when is_map(State) ->
    case maps:find(data, State) of
        {ok, Data} when is_map(Data) ->
            Data;
        {ok, Data} when is_map(Data) =:= false ->
            Data;
        error ->
            case State of
                #{<<"members">> := _} ->
                    State;
                _ ->
                    undefined
            end
    end;
resolve_data_map(_) ->
    undefined.

count_online_members(Members) ->
    lists:foldl(
        fun(Member, Count) ->
            Presence = maps:get(<<"presence">>, Member, <<"offline">>),
            case Presence of
                <<"offline">> -> Count;
                _ -> Count + 1
            end
        end,
        0,
        Members
    ).

update_counts(State) ->
    Data = maps:get(data, State, #{}),
    Members = maps:get(<<"members">>, Data, []),
    MemberCount = length(Members),
    OnlineCount = count_online_members(Members),
    maps:put(member_count, MemberCount, maps:put(online_count, OnlineCount, State)).
