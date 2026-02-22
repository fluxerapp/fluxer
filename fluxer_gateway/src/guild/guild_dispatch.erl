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

-module(guild_dispatch).

-export([
    handle_dispatch/3,
    extract_and_remove_session_id/1,
    decorate_member_data/3,
    extract_member_for_event/3,
    collect_and_send_push_notifications/3,
    normalize_event/1,
    is_member_list_updates_enabled/1
]).

-type guild_state() :: map().
-type event() :: atom().
-type event_data() :: map().
-type session_id() :: binary().
-type channel_id() :: integer().
-type guild_id() :: integer().
-type user_id() :: integer().
-type session_pair() :: {session_id(), map()}.

-define(GUILD_DISABLED_OP_PUSH_NOTIFICATIONS, 1 bsl 0).
-define(GUILD_DISABLED_OP_MEMBER_LIST_UPDATES, 1 bsl 6).

-spec normalize_event(term()) -> event().
normalize_event(Event) ->
    event_atoms:normalize(Event).

-spec handle_dispatch(event(), event_data(), guild_state()) -> {noreply, guild_state()}.
handle_dispatch(Event, EventData, State) ->
    case should_skip_dispatch(Event, State) of
        true ->
            {noreply, State};
        false ->
            NormalizedEvent = normalize_event(Event),
            process_dispatch(NormalizedEvent, EventData, State)
    end.

-spec should_skip_dispatch(event(), guild_state()) -> boolean().
should_skip_dispatch(guild_update, _State) ->
    false;
should_skip_dispatch(_Event, State) ->
    Data = maps:get(data, State, #{}),
    Guild = maps:get(<<"guild">>, Data, #{}),
    Features = maps:get(<<"features">>, Guild, []),
    lists:member(<<"UNAVAILABLE_FOR_EVERYONE">>, Features) orelse
        lists:member(<<"UNAVAILABLE_FOR_EVERYONE_BUT_STAFF">>, Features).

-spec process_dispatch(event(), event_data(), guild_state()) -> {noreply, guild_state()}.
process_dispatch(Event, EventData, State) ->
    GuildId = maps:get(id, State),
    {SessionIdOpt, CleanData} = extract_session_id_if_needed(Event, EventData),
    DecoratedData = maps:put(<<"guild_id">>, integer_to_binary(GuildId), CleanData),
    FinalData = decorate_member_data(Event, DecoratedData, State),
    UpdatedState = guild_state:update_state(Event, FinalData, State),
    FilterState = filter_state_for_event(Event, State, UpdatedState),
    Sessions = maps:get(sessions, UpdatedState, #{}),
    FilteredSessions = filter_sessions_for_event(
        Event, FinalData, SessionIdOpt, Sessions, FilterState
    ),
    logger:debug("process_dispatch: event=~p guild_id=~p total_sessions=~p filtered_sessions=~p",
        [Event, GuildId, map_size(Sessions), length(FilteredSessions)]),
    DispatchSuccess = dispatch_to_sessions(FilteredSessions, Event, FinalData, UpdatedState),
    track_dispatch_metrics(Event, DispatchSuccess),
    maybe_send_push_notifications(Event, FinalData, GuildId, UpdatedState),
    maybe_broadcast_member_list_update(Event, FinalData, State, UpdatedState),
    {noreply, UpdatedState}.

-spec filter_state_for_event(event(), guild_state(), guild_state()) -> guild_state().
filter_state_for_event(channel_delete, PreviousState, _UpdatedState) ->
    PreviousState;
filter_state_for_event(_Event, _PreviousState, UpdatedState) ->
    UpdatedState.

-spec extract_session_id_if_needed(event(), event_data()) ->
    {session_id() | undefined, event_data()}.
extract_session_id_if_needed(message_reaction_add, EventData) ->
    extract_and_remove_session_id(EventData);
extract_session_id_if_needed(message_reaction_remove, EventData) ->
    extract_and_remove_session_id(EventData);
extract_session_id_if_needed(_, EventData) ->
    {undefined, EventData}.

-spec filter_sessions_for_event(
    event(), event_data(), session_id() | undefined, map(), guild_state()
) ->
    [session_pair()].
filter_sessions_for_event(Event, FinalData, SessionIdOpt, Sessions, UpdatedState) ->
    case is_channel_scoped_event(Event) of
        true ->
            ChannelId = extract_channel_id(Event, FinalData),
            case is_message_access_filtered_event(Event) of
                true ->
                    MessageId = extract_message_id(FinalData),
                    guild_sessions:filter_sessions_for_message(
                        Sessions, ChannelId, MessageId, SessionIdOpt, UpdatedState
                    );
                false ->
                    guild_sessions:filter_sessions_for_channel(
                        Sessions, ChannelId, SessionIdOpt, UpdatedState
                    )
            end;
        false ->
            case is_invite_event(Event) of
                true ->
                    ChannelIdBin = maps:get(<<"channel_id">>, FinalData, undefined),
                    case parse_snowflake(<<"channel_id">>, ChannelIdBin) of
                        undefined ->
                            [];
                        ChannelId ->
                            guild_sessions:filter_sessions_for_manage_channels(
                                Sessions, ChannelId, SessionIdOpt, UpdatedState
                            )
                    end;
                false ->
                    FilteredSessions = guild_sessions:filter_sessions_exclude_session(
                        Sessions, SessionIdOpt
                    ),
                    case Event of
                        guild_member_add ->
                            [
                                {Sid, SessionData}
                             || {Sid, SessionData} <- FilteredSessions,
                                maps:get(bot, SessionData, false) =:= true
                            ];
                        _ ->
                            FilteredSessions
                    end
            end
    end.

-spec is_channel_scoped_event(event()) -> boolean().
is_channel_scoped_event(channel_create) -> true;
is_channel_scoped_event(channel_update) -> true;
is_channel_scoped_event(channel_delete) -> true;
is_channel_scoped_event(message_create) -> true;
is_channel_scoped_event(message_update) -> true;
is_channel_scoped_event(message_delete) -> true;
is_channel_scoped_event(message_delete_bulk) -> true;
is_channel_scoped_event(message_reaction_add) -> true;
is_channel_scoped_event(message_reaction_remove) -> true;
is_channel_scoped_event(message_reaction_remove_all) -> true;
is_channel_scoped_event(message_reaction_remove_emoji) -> true;
is_channel_scoped_event(typing_start) -> true;
is_channel_scoped_event(channel_pins_update) -> true;
is_channel_scoped_event(webhooks_update) -> true;
is_channel_scoped_event(_) -> false.

-spec is_invite_event(event()) -> boolean().
is_invite_event(invite_create) -> true;
is_invite_event(invite_delete) -> true;
is_invite_event(_) -> false.

-spec is_message_access_filtered_event(event()) -> boolean().
is_message_access_filtered_event(message_update) -> true;
is_message_access_filtered_event(message_delete) -> true;
is_message_access_filtered_event(message_reaction_add) -> true;
is_message_access_filtered_event(message_reaction_remove) -> true;
is_message_access_filtered_event(message_reaction_remove_all) -> true;
is_message_access_filtered_event(message_reaction_remove_emoji) -> true;
is_message_access_filtered_event(_) -> false.

-spec extract_message_id(event_data()) -> binary().
extract_message_id(EventData) ->
    RawMessageId =
        case maps:get(<<"message_id">>, EventData, undefined) of
            undefined -> maps:get(<<"id">>, EventData, undefined);
            MessageId -> MessageId
        end,
    parse_snowflake_binary(<<"message_id">>, RawMessageId).

-spec is_bulk_update_event(event()) -> boolean().
is_bulk_update_event(channel_update_bulk) -> true;
is_bulk_update_event(_) -> false.

-spec extract_channel_id(event(), event_data()) -> channel_id().
extract_channel_id(Event, FinalData) when
    Event =:= channel_create; Event =:= channel_update; Event =:= channel_delete
->
    ChannelIdBin = maps:get(<<"id">>, FinalData, undefined),
    require_snowflake(<<"id">>, ChannelIdBin);
extract_channel_id(_, FinalData) ->
    ChannelIdBin = maps:get(<<"channel_id">>, FinalData, undefined),
    require_snowflake(<<"channel_id">>, ChannelIdBin).

-spec dispatch_to_sessions([session_pair()], event(), event_data(), guild_state()) ->
    non_neg_integer().
dispatch_to_sessions(FilteredSessions, Event, FinalData, UpdatedState) ->
    GuildId = maps:get(id, UpdatedState),
    case is_bulk_update_event(Event) of
        true ->
            dispatch_bulk_update(FilteredSessions, Event, FinalData, UpdatedState);
        false ->
            dispatch_standard(FilteredSessions, Event, FinalData, GuildId, UpdatedState)
    end.

-spec dispatch_bulk_update([session_pair()], event(), event_data(), guild_state()) ->
    non_neg_integer().
dispatch_bulk_update(FilteredSessions, Event, FinalData, UpdatedState) ->
    GuildId = maps:get(id, UpdatedState),
    BulkChannels = maps:get(<<"channels">>, FinalData, []),
    SuccessCount = lists:foldl(
        fun({_Sid, SessionData}, Acc) ->
            Pid = maps:get(pid, SessionData),
            UserId = maps:get(user_id, SessionData),
            Member = guild_permissions:find_member_by_user_id(UserId, UpdatedState),
            case
                session_passive:should_receive_event(
                    Event, FinalData, GuildId, SessionData, UpdatedState
                )
            of
                false ->
                    Acc;
                true ->
                    FilteredChannels = filter_visible_channels(
                        BulkChannels, UserId, Member, UpdatedState
                    ),
                    dispatch_bulk_to_session(Pid, Event, FinalData, FilteredChannels, Acc)
            end
        end,
        0,
        FilteredSessions
    ),
    normalize_success(SuccessCount).

-spec filter_visible_channels([map()], user_id(), map() | undefined, guild_state()) -> [map()].
filter_visible_channels(Channels, UserId, Member, State) ->
    lists:filter(
        fun(Channel) ->
            ChannelIdBin = maps:get(<<"id">>, Channel, undefined),
            case {Member, parse_snowflake(<<"id">>, ChannelIdBin)} of
                {undefined, _} ->
                    false;
                {_, undefined} ->
                    false;
                {_, ChannelId} ->
                    guild_permissions:can_view_channel(UserId, ChannelId, Member, State)
            end
        end,
        Channels
    ).

-spec dispatch_bulk_to_session(pid(), event(), event_data(), [map()], non_neg_integer()) ->
    non_neg_integer().
dispatch_bulk_to_session(_, _, _, [], Acc) ->
    Acc;
dispatch_bulk_to_session(Pid, Event, FinalData, FilteredChannels, Acc) when is_pid(Pid) ->
    CustomData = maps:put(<<"channels">>, FilteredChannels, FinalData),
    try
        gen_server:cast(Pid, {dispatch, Event, CustomData}),
        Acc + 1
    catch
        _:_ -> Acc
    end;
dispatch_bulk_to_session(_, _, _, _, Acc) ->
    Acc.

-spec dispatch_standard([session_pair()], event(), event_data(), guild_id(), guild_state()) ->
    non_neg_integer().
dispatch_standard(FilteredSessions, Event, FinalData, GuildId, State) ->
    logger:debug("dispatch_standard: event=~p guild_id=~p filtered_sessions=~p member_count=~p",
        [Event, GuildId, length(FilteredSessions), maps:get(member_count, State, undefined)]),
    SuccessCount = lists:foldl(
        fun({Sid, SessionData}, Acc) ->
            Pid = maps:get(pid, SessionData),
            case
                is_pid(Pid) andalso
                    session_passive:should_receive_event(
                        Event, FinalData, GuildId, SessionData, State
                    )
            of
                true ->
                    try
                        gen_server:cast(Pid, {dispatch, Event, FinalData}),
                        Acc + 1
                    catch
                        _:_ -> Acc
                    end;
                false ->
                    logger:debug("dispatch_standard skip: sid=~p is_pid=~p passive=~p small=~p",
                        [Sid,
                         is_pid(Pid),
                         session_passive:is_passive(GuildId, SessionData),
                         session_passive:is_small_guild(State)]),
                    Acc
            end
        end,
        0,
        FilteredSessions
    ),
    normalize_success(SuccessCount).

-spec normalize_success(non_neg_integer()) -> non_neg_integer().
normalize_success(Count) when Count > 0 -> 1;
normalize_success(_) -> 0.

-spec maybe_send_push_notifications(event(), event_data(), guild_id(), guild_state()) -> ok.
maybe_send_push_notifications(message_create, FinalData, GuildId, UpdatedState) ->
    case maps:get(disable_push_notifications, UpdatedState, false) of
        true ->
            ok;
        false ->
            PushState = #{
                id => maps:get(id, UpdatedState, GuildId),
                data => maps:get(data, UpdatedState, #{}),
                sessions => maps:get(sessions, UpdatedState, #{}),
                virtual_channel_access => maps:get(virtual_channel_access, UpdatedState, #{})
            },
            spawn(fun() ->
                StartTime = erlang:monotonic_time(millisecond),
                collect_and_send_push_notifications(FinalData, GuildId, PushState),
                EndTime = erlang:monotonic_time(millisecond),
                gateway_metrics_collector:record_push_notification_time(EndTime - StartTime)
            end),
            ok
    end;
maybe_send_push_notifications(_Event, _FinalData, _GuildId, _UpdatedState) ->
    ok.

-spec maybe_broadcast_member_list_update(event(), event_data(), guild_state(), guild_state()) -> ok.
-spec maybe_broadcast_member_list_update_enabled(event(), event_data(), guild_state(), guild_state()) -> ok.
maybe_broadcast_member_list_update(Event, EventData, OldState, UpdatedState) ->
    case maybe_notify_very_large_guild_member_list(Event, EventData, UpdatedState) of
        true ->
            ok;
        false ->
            case is_member_list_updates_enabled(UpdatedState) of
                true ->
                    maybe_broadcast_member_list_update_enabled(Event, EventData, OldState, UpdatedState);
                false ->
                    ok
            end
    end.

-spec maybe_notify_very_large_guild_member_list(event(), event_data(), guild_state()) -> boolean().
maybe_notify_very_large_guild_member_list(Event, EventData, State) ->
    case {is_member_list_updates_enabled(State),
        maps:get(very_large_guild_coordinator_pid, State, undefined),
        maps:get(very_large_guild_shard_index, State, undefined)}
    of
        {true, CoordPid, 0} when is_pid(CoordPid) ->
            Notify =
                case Event of
                    guild_member_add ->
                        UserId = extract_user_id_from_event(EventData),
                        case guild_permissions:find_member_by_user_id(UserId, State) of
                            MemberData when is_map(MemberData) -> {upsert_member, MemberData};
                            _ -> {notify_member_update, UserId}
                        end;
                    guild_member_remove ->
                        {remove_member, extract_user_id_from_event(EventData)};
                    guild_member_update ->
                        UserId = extract_user_id_from_event(EventData),
                        case guild_permissions:find_member_by_user_id(UserId, State) of
                            MemberData when is_map(MemberData) -> {upsert_member, MemberData};
                            _ -> {notify_member_update, UserId}
                        end;
                    guild_role_create ->
                        {upsert_role, maps:get(<<"role">>, EventData, #{})};
                    guild_role_update ->
                        {upsert_role, maps:get(<<"role">>, EventData, #{})};
                    guild_role_update_bulk ->
                        {roles_bulk_update, maps:get(<<"roles">>, EventData, [])};
                    guild_role_delete ->
                        {role_deleted, maps:get(<<"role_id">>, EventData, undefined)};
                    channel_create ->
                        {upsert_channel, EventData};
                    channel_update ->
                        {upsert_channel, EventData};
                    channel_delete ->
                        ChannelId = parse_snowflake(<<"id">>, maps:get(<<"id">>, EventData, undefined)),
                        case ChannelId of
                            undefined -> undefined;
                            _ -> {notify_channel_update, ChannelId}
                        end;
                    channel_update_bulk ->
                        {channels_bulk_update, maps:get(<<"channels">>, EventData, [])};
                    _ ->
                        undefined
                end,
            case Notify of
                {notify_member_update, undefined} ->
                    false;
                {remove_member, undefined} ->
                    false;
                {role_deleted, undefined} ->
                    false;
                {upsert_role, RoleData} when not is_map(RoleData) orelse map_size(RoleData) =:= 0 ->
                    false;
                undefined ->
                    false;
                _ ->
                    gen_server:cast(CoordPid, {very_large_guild_member_list_notify, Notify}),
                    true
            end;
        _ ->
            false
    end.

maybe_broadcast_member_list_update_enabled(guild_member_add, EventData, OldState, UpdatedState) ->
    broadcast_member_update(EventData, OldState, UpdatedState);
maybe_broadcast_member_list_update_enabled(guild_member_remove, EventData, OldState, UpdatedState) ->
    broadcast_member_update(EventData, OldState, UpdatedState);
maybe_broadcast_member_list_update_enabled(guild_member_update, EventData, OldState, UpdatedState) ->
    broadcast_member_update(EventData, OldState, UpdatedState);
maybe_broadcast_member_list_update_enabled(guild_role_create, _EventData, _OldState, UpdatedState) ->
    broadcast_all_updates(UpdatedState);
maybe_broadcast_member_list_update_enabled(guild_role_update, _EventData, _OldState, UpdatedState) ->
    broadcast_all_updates(UpdatedState);
maybe_broadcast_member_list_update_enabled(guild_role_update_bulk, _EventData, _OldState, UpdatedState) ->
    broadcast_all_updates(UpdatedState);
maybe_broadcast_member_list_update_enabled(guild_role_delete, _EventData, _OldState, UpdatedState) ->
    broadcast_all_updates(UpdatedState);
maybe_broadcast_member_list_update_enabled(channel_update, EventData, _OldState, UpdatedState) ->
    broadcast_channel_update(EventData, UpdatedState);
maybe_broadcast_member_list_update_enabled(channel_update_bulk, EventData, _OldState, UpdatedState) ->
    Channels = maps:get(<<"channels">>, EventData, []),
    lists:foreach(
        fun(Channel) -> broadcast_channel_update(Channel, UpdatedState) end,
        Channels
    ),
    gateway_metrics_collector:inc_member_list_broadcast();
maybe_broadcast_member_list_update_enabled(_Event, _FinalData, _OldState, _UpdatedState) ->
    ok.

-spec broadcast_member_update(event_data(), guild_state(), guild_state()) -> ok.
broadcast_member_update(EventData, OldState, UpdatedState) ->
    UserId = extract_user_id_from_event(EventData),
    case UserId of
        undefined ->
            ok;
        _ ->
            guild_member_list:broadcast_member_list_updates(UserId, OldState, UpdatedState),
            gateway_metrics_collector:inc_member_list_broadcast()
    end.

-spec broadcast_all_updates(guild_state()) -> ok.
broadcast_all_updates(UpdatedState) ->
    guild_member_list:broadcast_all_member_list_updates(UpdatedState),
    gateway_metrics_collector:inc_member_list_broadcast().

-spec broadcast_channel_update(event_data(), guild_state()) -> ok.
broadcast_channel_update(EventData, UpdatedState) ->
    ChannelIdBin = maps:get(<<"id">>, EventData, undefined),
    case parse_snowflake(<<"id">>, ChannelIdBin) of
        undefined ->
            ok;
        ChannelId ->
            guild_member_list:broadcast_member_list_updates_for_channel(ChannelId, UpdatedState),
            gateway_metrics_collector:inc_member_list_broadcast()
    end.

-spec extract_user_id_from_event(event_data()) -> user_id() | undefined.
extract_user_id_from_event(EventData) ->
    MUser = maps:get(<<"user">>, EventData, #{}),
    parse_snowflake(<<"user.id">>, maps:get(<<"id">>, MUser, undefined)).

-spec track_dispatch_metrics(event(), non_neg_integer()) -> ok.
track_dispatch_metrics(Event, Success) ->
    case is_channel_scoped_event(Event) of
        true ->
            gateway_metrics_collector:inc_channel_event_fanout(Event, Success);
        false ->
            track_guild_event_metrics(Event, Success)
    end.

-spec track_guild_event_metrics(event(), non_neg_integer()) -> ok.
track_guild_event_metrics(guild_create, Success) ->
    gateway_metrics_collector:inc_guild_state_change(create),
    gateway_metrics_collector:inc_guild_event_dispatched(guild_create, Success);
track_guild_event_metrics(guild_delete, Success) ->
    gateway_metrics_collector:inc_guild_state_change(delete),
    gateway_metrics_collector:inc_guild_event_dispatched(guild_delete, Success);
track_guild_event_metrics(guild_update, Success) ->
    gateway_metrics_collector:inc_guild_state_change(update),
    gateway_metrics_collector:inc_guild_event_dispatched(guild_update, Success);
track_guild_event_metrics(Event, Success) ->
    gateway_metrics_collector:inc_guild_event_dispatched(Event, Success).

-spec extract_and_remove_session_id(event_data()) -> {session_id() | undefined, event_data()}.
extract_and_remove_session_id(Data) ->
    case maps:get(<<"session_id">>, Data, undefined) of
        undefined -> {undefined, Data};
        SessionId -> {SessionId, maps:remove(<<"session_id">>, Data)}
    end.

-spec decorate_member_data(event(), event_data(), guild_state()) -> event_data().
decorate_member_data(Event, Data, State) ->
    case extract_member_for_event(Event, Data, State) of
        undefined -> Data;
        MemberData -> add_member_to_data(Event, Data, MemberData)
    end.

-spec add_member_to_data(event(), event_data(), map()) -> event_data().
add_member_to_data(Event, Data, MemberData) ->
    case is_message_event(Event) of
        true ->
            CleanMemberData = maps:remove(<<"user">>, MemberData),
            maps:put(<<"member">>, CleanMemberData, Data);
        false ->
            case is_user_event(Event) of
                true ->
                    maps:put(<<"member">>, MemberData, Data);
                false ->
                    Data
            end
    end.

-spec is_message_event(event()) -> boolean().
is_message_event(message_create) -> true;
is_message_event(message_update) -> true;
is_message_event(message_delete) -> true;
is_message_event(message_delete_bulk) -> true;
is_message_event(_) -> false.

-spec is_user_event(event()) -> boolean().
is_user_event(typing_start) -> true;
is_user_event(message_reaction_add) -> true;
is_user_event(message_reaction_remove) -> true;
is_user_event(_) -> false.

-spec extract_member_for_event(event(), event_data(), guild_state()) -> map() | undefined.
extract_member_for_event(Event, Data, State) ->
    UserId = extract_user_id_for_event(Event, Data),
    case UserId of
        undefined -> undefined;
        Id -> guild_permissions:find_member_by_user_id(Id, State)
    end.

-spec extract_user_id_for_event(event(), event_data()) -> user_id() | undefined.
extract_user_id_for_event(Event, Data) ->
    case is_message_event(Event) of
        true ->
            extract_author_id(Data);
        false ->
            case is_user_event(Event) of
                true -> extract_user_id_field(Data);
                false -> undefined
            end
    end.

-spec extract_author_id(event_data()) -> user_id() | undefined.
extract_author_id(Data) ->
    AuthorId = maps:get(<<"id">>, maps:get(<<"author">>, Data, #{}), undefined),
    RawAuthorId =
        case AuthorId of
            undefined -> maps:get(<<"author_id">>, Data, undefined);
            _ -> AuthorId
        end,
    case RawAuthorId of
        undefined ->
            undefined;
        _ ->
            case validation:validate_snowflake(<<"author_id">>, RawAuthorId) of
                {ok, Id} -> Id;
                {error, _, _} -> undefined
            end
    end.

-spec extract_user_id_field(event_data()) -> user_id() | undefined.
extract_user_id_field(Data) ->
    UserId = maps:get(<<"user_id">>, Data, undefined),
    case UserId of
        undefined ->
            undefined;
        _ ->
            case validation:validate_snowflake(<<"user_id">>, UserId) of
                {ok, Id} -> Id;
                {error, _, _} -> undefined
            end
    end.

-spec collect_and_send_push_notifications(event_data(), guild_id(), guild_state()) -> ok.
collect_and_send_push_notifications(MessageData, GuildId, State) ->
    case should_send_push_notifications(State) of
        false -> ok;
        true -> send_push_notifications(MessageData, GuildId, State)
    end.

-spec should_send_push_notifications(guild_state()) -> boolean().
should_send_push_notifications(State) ->
    not is_guild_operation_disabled(State, ?GUILD_DISABLED_OP_PUSH_NOTIFICATIONS).

-spec is_member_list_updates_enabled(guild_state()) -> boolean().
is_member_list_updates_enabled(State) ->
    case maps:get(disable_member_list_updates, State, false) of
        true -> false;
        false -> not is_guild_operation_disabled(State, ?GUILD_DISABLED_OP_MEMBER_LIST_UPDATES)
    end.

-spec is_guild_operation_disabled(guild_state(), integer()) -> boolean().
is_guild_operation_disabled(State, OperationMask) ->
    Data = maps:get(data, State, #{}),
    Guild = maps:get(<<"guild">>, Data, #{}),
    DisabledOperationsBin = maps:get(<<"disabled_operations">>, Guild, undefined),
    DisabledOperations = parse_integer(DisabledOperationsBin, 0),
    (DisabledOperations band OperationMask) =:= OperationMask.

-spec send_push_notifications(event_data(), guild_id(), guild_state()) -> ok.
send_push_notifications(MessageData, GuildId, State) ->
    Data = maps:get(data, State),
    Members = guild_data_index:member_map(Data),
    Sessions = maps:get(sessions, State, #{}),
    SessionEligibility = build_push_session_eligibility(Sessions),
    CandidateUserIds = push_candidate_user_ids(Members, SessionEligibility),
    ChannelIdBin = maps:get(<<"channel_id">>, MessageData, undefined),
    case parse_snowflake(<<"channel_id">>, ChannelIdBin) of
        undefined ->
            ok;
        ChannelId ->
            case find_eligible_users_for_push(Members, CandidateUserIds, ChannelId, State) of
                [] ->
                    ok;
                EligibleUserIds ->
                    UserRolesMap = build_user_roles_map(Members, EligibleUserIds),
                    send_push_to_eligible_users(MessageData, GuildId, EligibleUserIds, UserRolesMap, Data)
            end
    end.

-spec push_candidate_user_ids(map(), map()) -> [user_id()].
push_candidate_user_ids(Members, SessionEligibility) ->
    NoSessionUserIds = lists:filter(
        fun(UserId) -> not maps:is_key(UserId, SessionEligibility) end,
        maps:keys(Members)
    ),
    EligibleSessionUserIds = maps:fold(
        fun(UserId, IsEligible, Acc) ->
            case IsEligible of
                true -> [UserId | Acc];
                false -> Acc
            end
        end,
        [],
        SessionEligibility
    ),
    lists:usort(NoSessionUserIds ++ EligibleSessionUserIds).

-spec find_eligible_users_for_push(map(), [user_id()], channel_id(), guild_state()) -> [user_id()].
find_eligible_users_for_push(Members, CandidateUserIds, ChannelId, State) ->
    lists:filtermap(
        fun(UserId) ->
            case maps:get(UserId, Members, undefined) of
                undefined ->
                    false;
                Member ->
                    case guild_permissions:can_view_channel(UserId, ChannelId, Member, State) of
                        true -> {true, UserId};
                        false -> false
                    end
            end
        end,
        CandidateUserIds
    ).

-spec build_push_session_eligibility(map()) -> #{user_id() => boolean()}.
build_push_session_eligibility(Sessions) ->
    SessionStats = maps:fold(
        fun(_Sid, Session, Acc) ->
            case maps:get(user_id, Session, undefined) of
                UserId when is_integer(UserId) ->
                    PrevStats = maps:get(
                        UserId,
                        Acc,
                        #{count => 0, has_mobile => false, all_afk => true}
                    ),
                    NewStats = #{
                        count => maps:get(count, PrevStats, 0) + 1,
                        has_mobile =>
                            maps:get(has_mobile, PrevStats, false) orelse
                                maps:get(mobile, Session, false),
                        all_afk =>
                            maps:get(all_afk, PrevStats, true) andalso
                                maps:get(afk, Session, false)
                    },
                    maps:put(UserId, NewStats, Acc);
                _ ->
                    Acc
            end
        end,
        #{},
        Sessions
    ),
    maps:map(
        fun(_UserId, Stats) ->
            (not maps:get(has_mobile, Stats, false)) andalso maps:get(all_afk, Stats, false)
        end,
        SessionStats
    ).

-spec send_push_to_eligible_users(event_data(), guild_id(), [user_id()], map(), map()) -> ok.
send_push_to_eligible_users(MessageData, GuildId, EligibleUserIds, UserRolesMap, Data) ->
    Guild = maps:get(<<"guild">>, Data),
    AuthorIdBin = maps:get(<<"id">>, maps:get(<<"author">>, MessageData, #{}), undefined),
    case parse_snowflake(<<"author.id">>, AuthorIdBin) of
        undefined ->
            ok;
        AuthorId ->
            DefaultMessageNotifications = maps:get(<<"default_message_notifications">>, Guild, 0),
            GuildName = maps:get(<<"name">>, Guild, <<"Unknown">>),
            ChannelIdBin = maps:get(<<"channel_id">>, MessageData),
            ChannelName = find_channel_name(ChannelIdBin, Data),
            gateway_metrics_collector:inc_push_notification_sent(),
            push:handle_message_create(#{
                message_data => MessageData,
                user_ids => EligibleUserIds,
                guild_id => GuildId,
                author_id => AuthorId,
                guild_default_notifications => DefaultMessageNotifications,
                guild_name => GuildName,
                channel_name => ChannelName,
                user_roles => UserRolesMap
            })
    end.

-spec find_channel_name(binary(), map()) -> binary().
find_channel_name(ChannelIdBin, Data) ->
    case parse_snowflake(<<"channel_id">>, ChannelIdBin) of
        undefined ->
            <<"unknown">>;
        ChannelId ->
            Channels = guild_data_index:channel_index(Data),
            case maps:get(ChannelId, Channels, undefined) of
                undefined -> <<"unknown">>;
                Channel -> maps:get(<<"name">>, Channel, <<"unknown">>)
            end
    end.

-spec build_user_roles_map(map(), [user_id()]) -> #{user_id() => [integer()]}.
build_user_roles_map(Members, EligibleUserIds) ->
    lists:foldl(
        fun(UserId, Acc) ->
            case maps:get(UserId, Members, undefined) of
                undefined ->
                    Acc;
                Member ->
                    Roles = extract_role_ids(Member),
                    maps:put(UserId, Roles, Acc)
            end
        end,
        #{},
        EligibleUserIds
    ).

-spec extract_role_ids(map()) -> [integer()].
extract_role_ids(Member) ->
    Roles = maps:get(<<"roles">>, Member, []),
    lists:foldl(
        fun(Role, Acc) ->
            case validation:validate_snowflake(<<"role">>, Role) of
                {ok, RoleId} -> [RoleId | Acc];
                _ -> Acc
            end
        end,
        [],
        Roles
    ).

-spec parse_snowflake(binary(), term()) -> integer() | undefined.
parse_snowflake(FieldName, Value) ->
    case validation:validate_snowflake(FieldName, Value) of
        {ok, Id} -> Id;
        {error, _, _} -> undefined
    end.

-spec require_snowflake(binary(), term()) -> integer().
require_snowflake(FieldName, Value) ->
    validation:snowflake_or_throw(FieldName, Value).

-spec parse_snowflake_binary(binary(), term()) -> binary().
parse_snowflake_binary(FieldName, Value) ->
    integer_to_binary(require_snowflake(FieldName, Value)).

-spec parse_integer(term(), integer()) -> integer().
parse_integer(undefined, Default) ->
    Default;
parse_integer(Value, _Default) when is_integer(Value) ->
    Value;
parse_integer(Value, Default) when is_binary(Value) ->
    try binary_to_integer(Value) of
        Parsed -> Parsed
    catch
        error:badarg -> Default
    end;
parse_integer(_, Default) ->
    Default.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

is_channel_scoped_event_test() ->
    ?assertEqual(true, is_channel_scoped_event(message_create)),
    ?assertEqual(true, is_channel_scoped_event(channel_update)),
    ?assertEqual(true, is_channel_scoped_event(typing_start)),
    ?assertEqual(false, is_channel_scoped_event(guild_update)),
    ?assertEqual(false, is_channel_scoped_event(guild_member_add)).

is_invite_event_test() ->
    ?assertEqual(true, is_invite_event(invite_create)),
    ?assertEqual(true, is_invite_event(invite_delete)),
    ?assertEqual(false, is_invite_event(message_create)).

is_bulk_update_event_test() ->
    ?assertEqual(true, is_bulk_update_event(channel_update_bulk)),
    ?assertEqual(false, is_bulk_update_event(channel_update)).

extract_and_remove_session_id_present_test() ->
    Data = #{<<"session_id">> => <<"abc123">>, <<"other">> => <<"value">>},
    {SessionId, CleanData} = extract_and_remove_session_id(Data),
    ?assertEqual(<<"abc123">>, SessionId),
    ?assertEqual(#{<<"other">> => <<"value">>}, CleanData).

extract_and_remove_session_id_absent_test() ->
    Data = #{<<"other">> => <<"value">>},
    {SessionId, CleanData} = extract_and_remove_session_id(Data),
    ?assertEqual(undefined, SessionId),
    ?assertEqual(Data, CleanData).

is_message_event_test() ->
    ?assertEqual(true, is_message_event(message_create)),
    ?assertEqual(true, is_message_event(message_update)),
    ?assertEqual(true, is_message_event(message_delete)),
    ?assertEqual(true, is_message_event(message_delete_bulk)),
    ?assertEqual(false, is_message_event(typing_start)).

is_user_event_test() ->
    ?assertEqual(true, is_user_event(typing_start)),
    ?assertEqual(true, is_user_event(message_reaction_add)),
    ?assertEqual(false, is_user_event(message_reaction_remove_all)),
    ?assertEqual(false, is_user_event(message_reaction_remove_emoji)),
    ?assertEqual(false, is_user_event(message_create)).

normalize_success_test() ->
    ?assertEqual(1, normalize_success(5)),
    ?assertEqual(1, normalize_success(1)),
    ?assertEqual(0, normalize_success(0)).

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

extract_session_id_if_needed_reaction_test() ->
    Data = #{<<"session_id">> => <<"sid">>, <<"emoji">> => #{}},
    {SessionId, CleanData} = extract_session_id_if_needed(message_reaction_add, Data),
    ?assertEqual(<<"sid">>, SessionId),
    ?assertNot(maps:is_key(<<"session_id">>, CleanData)).

extract_session_id_if_needed_other_test() ->
    Data = #{<<"session_id">> => <<"sid">>, <<"content">> => <<"hi">>},
    {SessionId, CleanData} = extract_session_id_if_needed(message_create, Data),
    ?assertEqual(undefined, SessionId),
    ?assertEqual(Data, CleanData).

decorate_member_data_message_delete_author_id_test() ->
    Member = #{<<"user">> => #{<<"id">> => <<"123">>}, <<"roles">> => []},
    State = #{data => #{<<"members">> => [Member]}},
    Data = #{<<"author_id">> => <<"123">>},
    Decorated = decorate_member_data(message_delete, Data, State),
    ?assert(maps:is_key(<<"member">>, Decorated)),
    ?assertEqual(false, maps:is_key(<<"user">>, maps:get(<<"member">>, Decorated))).

is_message_access_filtered_event_test() ->
    ?assertEqual(true, is_message_access_filtered_event(message_update)),
    ?assertEqual(true, is_message_access_filtered_event(message_delete)),
    ?assertEqual(true, is_message_access_filtered_event(message_reaction_add)),
    ?assertEqual(true, is_message_access_filtered_event(message_reaction_remove)),
    ?assertEqual(true, is_message_access_filtered_event(message_reaction_remove_all)),
    ?assertEqual(true, is_message_access_filtered_event(message_reaction_remove_emoji)),
    ?assertEqual(false, is_message_access_filtered_event(message_create)),
    ?assertEqual(false, is_message_access_filtered_event(message_delete_bulk)),
    ?assertEqual(false, is_message_access_filtered_event(typing_start)),
    ?assertEqual(false, is_message_access_filtered_event(channel_create)).

extract_message_id_from_id_field_test() ->
    Data = #{<<"id">> => <<"12345">>, <<"channel_id">> => <<"100">>},
    ?assertEqual(<<"12345">>, extract_message_id(Data)).

extract_message_id_from_message_id_field_test() ->
    Data = #{<<"message_id">> => <<"67890">>, <<"channel_id">> => <<"100">>},
    ?assertEqual(<<"67890">>, extract_message_id(Data)).

extract_message_id_prefers_message_id_test() ->
    Data = #{<<"id">> => <<"12345">>, <<"message_id">> => <<"67890">>},
    ?assertEqual(<<"67890">>, extract_message_id(Data)).

extract_channel_id_channel_delete_uses_id_field_test() ->
    Data = #{<<"id">> => <<"42">>},
    ?assertEqual(42, extract_channel_id(channel_delete, Data)).

channel_delete_dispatch_filters_by_pre_delete_visibility_test() ->
    Parent = self(),
    VisiblePid = start_dispatch_capture(visible, Parent),
    HiddenPid = start_dispatch_capture(hidden, Parent),
    try
        State = build_channel_delete_dispatch_state(VisiblePid, HiddenPid),
        HiddenMember = guild_permissions:find_member_by_user_id(1001, State),
        VisibleMember = guild_permissions:find_member_by_user_id(1002, State),
        ?assertEqual(false, guild_permissions:can_view_channel(1001, 10, HiddenMember, State)),
        ?assertEqual(true, guild_permissions:can_view_channel(1002, 10, VisibleMember, State)),
        {noreply, _} = handle_dispatch(channel_delete, #{<<"id">> => <<"10">>}, State),
        receive
            {visible, {dispatch, channel_delete, Payload}} ->
                ?assertEqual(<<"10">>, maps:get(<<"id">>, Payload));
            {visible, Other} ->
                ?assert(false, {unexpected_visible_message, Other})
        after 1000 ->
            ?assert(false, visible_channel_delete_not_dispatched)
        end,
        receive
            {hidden, {dispatch, channel_delete, _Payload}} ->
                ?assert(false, hidden_user_received_channel_delete)
        after 200 ->
            ok
        end
    after
        VisiblePid ! stop,
        HiddenPid ! stop
    end.

-spec disabled_operations_state(integer() | binary()) -> guild_state().
disabled_operations_state(Value) ->
    #{data => #{<<"guild">> => #{<<"disabled_operations">> => Value}}}.

should_send_push_notifications_respects_flag_test() ->
    ?assertEqual(true, should_send_push_notifications(disabled_operations_state(0))),
    ?assertEqual(
        false,
        should_send_push_notifications(disabled_operations_state(?GUILD_DISABLED_OP_PUSH_NOTIFICATIONS))
    ).

member_list_updates_enabled_respects_flag_test() ->
    ?assertEqual(true, is_member_list_updates_enabled(disabled_operations_state(<<"0">>))),
    ?assertEqual(
        false,
        is_member_list_updates_enabled(#{disable_member_list_updates => true, data => #{<<"guild">> => #{}}})
    ),
    ?assertEqual(
        false,
        is_member_list_updates_enabled(
            disabled_operations_state(integer_to_binary(?GUILD_DISABLED_OP_MEMBER_LIST_UPDATES))
        )
    ).

build_push_session_eligibility_test() ->
    Sessions = #{
        <<"s1">> => #{user_id => 1, mobile => false, afk => true},
        <<"s2">> => #{user_id => 1, mobile => false, afk => true},
        <<"s3">> => #{user_id => 2, mobile => true, afk => true},
        <<"s4">> => #{user_id => 3, mobile => false, afk => false}
    },
    Eligibility = build_push_session_eligibility(Sessions),
    ?assertEqual(true, maps:get(1, Eligibility)),
    ?assertEqual(false, maps:get(2, Eligibility)),
    ?assertEqual(false, maps:get(3, Eligibility)).

push_candidate_user_ids_prefers_sessionless_and_eligible_sessions_test() ->
    Members = #{
        1 => #{},
        2 => #{},
        3 => #{},
        4 => #{}
    },
    SessionEligibility = #{
        1 => false,
        2 => true
    },
    CandidateUserIds = push_candidate_user_ids(Members, SessionEligibility),
    ?assertEqual([2, 3, 4], CandidateUserIds).

build_user_roles_map_uses_member_map_test() ->
    Members = #{
        1 => #{<<"roles">> => [<<"10">>, <<"11">>]},
        2 => #{<<"roles">> => [<<"20">>]}
    },
    Result = build_user_roles_map(Members, [2, 1]),
    ?assertEqual([10, 11], lists:sort(maps:get(1, Result))),
    ?assertEqual([20], maps:get(2, Result)).

channel_create_notifies_vlg_member_list_test() ->
    Parent = self(),
    CoordPid = spawn(fun() -> vlg_coord_capture_loop(Parent) end),
    EventData = #{<<"id">> => <<"500">>, <<"name">> => <<"new-channel">>},
    State = build_vlg_state(CoordPid),
    ?assertEqual(true, maybe_notify_very_large_guild_member_list(channel_create, EventData, State)),
    receive
        {vlg_notify, {upsert_channel, Received}} ->
            ?assertEqual(EventData, Received)
    after 1000 ->
        ?assert(false, channel_create_vlg_notification_not_received)
    end,
    CoordPid ! stop.

channel_delete_notifies_vlg_member_list_test() ->
    Parent = self(),
    CoordPid = spawn(fun() -> vlg_coord_capture_loop(Parent) end),
    EventData = #{<<"id">> => <<"600">>, <<"name">> => <<"deleted-channel">>},
    State = build_vlg_state(CoordPid),
    ?assertEqual(true, maybe_notify_very_large_guild_member_list(channel_delete, EventData, State)),
    receive
        {vlg_notify, {notify_channel_update, ChannelId}} ->
            ?assertEqual(600, ChannelId)
    after 1000 ->
        ?assert(false, channel_delete_vlg_notification_not_received)
    end,
    CoordPid ! stop.

-spec build_vlg_state(pid()) -> guild_state().
build_vlg_state(CoordPid) ->
    #{
        id => 1,
        very_large_guild_coordinator_pid => CoordPid,
        very_large_guild_shard_index => 0,
        data => #{
            <<"guild">> => #{<<"disabled_operations">> => <<"0">>}
        },
        disable_member_list_updates => false
    }.

-spec vlg_coord_capture_loop(pid()) -> ok.
vlg_coord_capture_loop(Parent) ->
    receive
        stop ->
            ok;
        {'$gen_cast', {very_large_guild_member_list_notify, Notify}} ->
            Parent ! {vlg_notify, Notify},
            vlg_coord_capture_loop(Parent);
        _Other ->
            vlg_coord_capture_loop(Parent)
    end.

-spec start_dispatch_capture(atom(), pid()) -> pid().
start_dispatch_capture(Tag, Parent) ->
    spawn(fun() -> dispatch_capture_loop(Tag, Parent) end).

-spec dispatch_capture_loop(atom(), pid()) -> ok.
dispatch_capture_loop(Tag, Parent) ->
    receive
        stop ->
            ok;
        {'$gen_cast', Msg} ->
            Parent ! {Tag, Msg},
            dispatch_capture_loop(Tag, Parent);
        _Other ->
            dispatch_capture_loop(Tag, Parent)
    end.

-spec build_channel_delete_dispatch_state(pid(), pid()) -> guild_state().
build_channel_delete_dispatch_state(VisiblePid, HiddenPid) ->
    GuildId = 1,
    GuildIdBin = integer_to_binary(GuildId),
    ViewPermission = constants:view_channel_permission(),
    ViewPermissionBin = integer_to_binary(ViewPermission),
    #{
        id => GuildId,
        member_count => 100,
        sessions => #{
            <<"visible">> => #{
                user_id => 1002,
                pid => VisiblePid,
                active_guilds => sets:from_list([GuildId])
            },
            <<"hidden">> => #{
                user_id => 1001,
                pid => HiddenPid,
                active_guilds => sets:from_list([GuildId])
            }
        },
        data => #{
            <<"guild">> => #{
                <<"id">> => GuildIdBin,
                <<"owner_id">> => <<"999">>,
                <<"features">> => []
            },
            <<"roles">> => [
                #{<<"id">> => GuildIdBin, <<"permissions">> => ViewPermissionBin},
                #{<<"id">> => <<"200">>, <<"permissions">> => <<"0">>}
            ],
            <<"members">> => [
                #{<<"user">> => #{<<"id">> => <<"1001">>}, <<"roles">> => [<<"200">>]},
                #{<<"user">> => #{<<"id">> => <<"1002">>}, <<"roles">> => []}
            ],
            <<"channels">> => [
                #{
                    <<"id">> => <<"10">>,
                    <<"permission_overwrites">> => [
                        #{
                            <<"id">> => <<"200">>,
                            <<"type">> => 0,
                            <<"allow">> => <<"0">>,
                            <<"deny">> => ViewPermissionBin
                        }
                    ]
                }
            ]
        }
    }.

should_skip_dispatch_guild_update_never_skipped_test() ->
    State = #{
        data => #{
            <<"guild">> => #{<<"features">> => [<<"UNAVAILABLE_FOR_EVERYONE">>]}
        }
    },
    ?assertEqual(false, should_skip_dispatch(guild_update, State)).

should_skip_dispatch_unavailable_for_everyone_test() ->
    State = #{
        data => #{
            <<"guild">> => #{<<"features">> => [<<"UNAVAILABLE_FOR_EVERYONE">>]}
        }
    },
    ?assertEqual(true, should_skip_dispatch(message_create, State)).

should_skip_dispatch_unavailable_for_everyone_but_staff_test() ->
    State = #{
        data => #{
            <<"guild">> => #{<<"features">> => [<<"UNAVAILABLE_FOR_EVERYONE_BUT_STAFF">>]}
        }
    },
    ?assertEqual(true, should_skip_dispatch(message_create, State)).

should_skip_dispatch_normal_guild_test() ->
    State = #{
        data => #{
            <<"guild">> => #{<<"features">> => []}
        }
    },
    ?assertEqual(false, should_skip_dispatch(message_create, State)).

should_skip_dispatch_no_features_test() ->
    State = #{data => #{<<"guild">> => #{}}},
    ?assertEqual(false, should_skip_dispatch(message_create, State)).

filter_sessions_for_event_guild_wide_goes_to_all_sessions_test() ->
    S1 = #{session_id => <<"s1">>, user_id => 10, pid => self()},
    S2 = #{session_id => <<"s2">>, user_id => 11, pid => self()},
    Sessions = #{<<"s1">> => S1, <<"s2">> => S2},
    State = #{sessions => Sessions, data => #{<<"members">> => #{}}},
    Result = filter_sessions_for_event(guild_update, #{}, undefined, Sessions, State),
    ?assertEqual(2, length(Result)).

filter_sessions_for_event_guild_member_add_bots_only_test() ->
    S1 = #{session_id => <<"s1">>, user_id => 10, pid => self(), bot => false},
    S2 = #{session_id => <<"s2">>, user_id => 11, pid => self(), bot => true},
    Sessions = #{<<"s1">> => S1, <<"s2">> => S2},
    State = #{sessions => Sessions, data => #{<<"members">> => #{}}},
    Result = filter_sessions_for_event(guild_member_add, #{}, undefined, Sessions, State),
    ?assertEqual([{<<"s2">>, S2}], Result).

extract_channel_id_message_create_uses_channel_id_field_test() ->
    Data = #{<<"channel_id">> => <<"42">>},
    ?assertEqual(42, extract_channel_id(message_create, Data)).

extract_channel_id_channel_create_uses_id_field_test() ->
    Data = #{<<"id">> => <<"42">>},
    ?assertEqual(42, extract_channel_id(channel_create, Data)).

extract_channel_id_channel_update_uses_id_field_test() ->
    Data = #{<<"id">> => <<"42">>},
    ?assertEqual(42, extract_channel_id(channel_update, Data)).

parse_integer_undefined_returns_default_test() ->
    ?assertEqual(42, parse_integer(undefined, 42)).

parse_integer_integer_test() ->
    ?assertEqual(7, parse_integer(7, 0)).

parse_integer_valid_binary_test() ->
    ?assertEqual(123, parse_integer(<<"123">>, 0)).

parse_integer_invalid_binary_test() ->
    ?assertEqual(0, parse_integer(<<"abc">>, 0)).

parse_integer_other_type_test() ->
    ?assertEqual(5, parse_integer(3.14, 5)).

is_guild_operation_disabled_test() ->
    State = disabled_operations_state(3),
    ?assertEqual(true, is_guild_operation_disabled(State, 1)),
    ?assertEqual(true, is_guild_operation_disabled(State, 2)),
    ?assertEqual(true, is_guild_operation_disabled(State, 3)),
    ?assertEqual(false, is_guild_operation_disabled(State, 4)).

is_guild_operation_disabled_binary_test() ->
    State = disabled_operations_state(<<"5">>),
    ?assertEqual(true, is_guild_operation_disabled(State, 1)),
    ?assertEqual(true, is_guild_operation_disabled(State, 4)),
    ?assertEqual(false, is_guild_operation_disabled(State, 2)).

extract_session_id_if_needed_reaction_remove_test() ->
    Data = #{<<"session_id">> => <<"sid">>, <<"emoji">> => #{}},
    {SessionId, CleanData} = extract_session_id_if_needed(message_reaction_remove, Data),
    ?assertEqual(<<"sid">>, SessionId),
    ?assertNot(maps:is_key(<<"session_id">>, CleanData)).

decorate_member_data_typing_start_test() ->
    Member = #{<<"user">> => #{<<"id">> => <<"456">>}, <<"roles">> => []},
    State = #{data => #{<<"members">> => [Member]}},
    Data = #{<<"user_id">> => <<"456">>},
    Decorated = decorate_member_data(typing_start, Data, State),
    ?assert(maps:is_key(<<"member">>, Decorated)),
    ?assert(maps:is_key(<<"user">>, maps:get(<<"member">>, Decorated))).

decorate_member_data_guild_event_no_decoration_test() ->
    State = #{data => #{<<"members">> => []}},
    Data = #{<<"name">> => <<"test">>},
    Decorated = decorate_member_data(guild_update, Data, State),
    ?assertEqual(false, maps:is_key(<<"member">>, Decorated)).

filter_visible_channels_test() ->
    GuildId = 42,
    UserId = 10,
    ViewPerm = constants:view_channel_permission(),
    Member = #{<<"user">> => #{<<"id">> => integer_to_binary(UserId)}, <<"roles">> => []},
    State = #{
        id => GuildId,
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"999">>},
            <<"roles">> => [
                #{<<"id">> => integer_to_binary(GuildId), <<"permissions">> => integer_to_binary(ViewPerm)}
            ],
            <<"members">> => [Member],
            <<"channels">> => [
                #{<<"id">> => <<"100">>, <<"permission_overwrites">> => []},
                #{
                    <<"id">> => <<"101">>,
                    <<"permission_overwrites">> => [
                        #{
                            <<"id">> => integer_to_binary(GuildId),
                            <<"type">> => 0,
                            <<"allow">> => <<"0">>,
                            <<"deny">> => integer_to_binary(ViewPerm)
                        }
                    ]
                }
            ]
        }
    },
    Channels = [
        #{<<"id">> => <<"100">>},
        #{<<"id">> => <<"101">>}
    ],
    Result = filter_visible_channels(Channels, UserId, Member, State),
    ?assertEqual(1, length(Result)),
    ?assertEqual(<<"100">>, maps:get(<<"id">>, hd(Result))).

filter_visible_channels_undefined_member_test() ->
    State = #{data => #{<<"members">> => []}},
    Channels = [#{<<"id">> => <<"100">>}],
    Result = filter_visible_channels(Channels, 10, undefined, State),
    ?assertEqual([], Result).

extract_user_id_from_event_test() ->
    EventData = #{<<"user">> => #{<<"id">> => <<"42">>}},
    ?assertEqual(42, extract_user_id_from_event(EventData)).

extract_user_id_from_event_missing_test() ->
    ?assertEqual(undefined, extract_user_id_from_event(#{})).

extract_user_id_from_event_invalid_test() ->
    EventData = #{<<"user">> => #{<<"id">> => <<"invalid">>}},
    ?assertEqual(undefined, extract_user_id_from_event(EventData)).

find_channel_name_uses_index_test() ->
    Data = #{
        <<"channels">> => [
            #{<<"id">> => <<"100">>, <<"name">> => <<"general">>}
        ],
        <<"channel_index">> => #{100 => #{<<"id">> => <<"100">>, <<"name">> => <<"general">>}}
    },
    ?assertEqual(<<"general">>, find_channel_name(<<"100">>, Data)).

find_channel_name_invalid_id_test() ->
    Data = #{<<"channels">> => []},
    ?assertEqual(<<"unknown">>, find_channel_name(<<"invalid">>, Data)).

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
