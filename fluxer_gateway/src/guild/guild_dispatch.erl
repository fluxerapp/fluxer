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
    normalize_event/1
]).

-import(guild_permissions, [find_member_by_user_id/2]).
-import(guild_state, [update_state/3]).
-import(guild_sessions, [
    filter_sessions_for_channel/4,
    filter_sessions_for_manage_channels/4,
    filter_sessions_exclude_session/2
]).
-import(session_passive, [should_receive_event/5]).

normalize_event(Event) when is_atom(Event) -> Event;
normalize_event(<<"MESSAGE_CREATE">>) -> message_create;
normalize_event(<<"MESSAGE_UPDATE">>) -> message_update;
normalize_event(<<"MESSAGE_DELETE">>) -> message_delete;
normalize_event(<<"MESSAGE_DELETE_BULK">>) -> message_delete_bulk;
normalize_event(<<"MESSAGE_REACTION_ADD">>) -> message_reaction_add;
normalize_event(<<"MESSAGE_REACTION_REMOVE">>) -> message_reaction_remove;
normalize_event(<<"MESSAGE_REACTION_REMOVE_ALL">>) -> message_reaction_remove_all;
normalize_event(<<"MESSAGE_REACTION_REMOVE_EMOJI">>) -> message_reaction_remove_emoji;
normalize_event(<<"CHANNEL_CREATE">>) -> channel_create;
normalize_event(<<"CHANNEL_UPDATE">>) -> channel_update;
normalize_event(<<"CHANNEL_UPDATE_BULK">>) -> channel_update_bulk;
normalize_event(<<"CHANNEL_DELETE">>) -> channel_delete;
normalize_event(<<"CHANNEL_PINS_UPDATE">>) -> channel_pins_update;
normalize_event(<<"TYPING_START">>) -> typing_start;
normalize_event(<<"INVITE_CREATE">>) -> invite_create;
normalize_event(<<"INVITE_DELETE">>) -> invite_delete;
normalize_event(<<"GUILD_UPDATE">>) -> guild_update;
normalize_event(EventBinary) when is_binary(EventBinary) -> EventBinary.

handle_dispatch(Event, EventData, State) ->
    case should_skip_dispatch(Event, State) of
        true ->
            {noreply, State};
        false ->
            NormalizedEvent = normalize_event(Event),
            process_dispatch(NormalizedEvent, EventData, State)
    end.

should_skip_dispatch(guild_update, _State) ->
    false;
should_skip_dispatch(_Event, State) ->
    Data = maps:get(data, State),
    Guild = maps:get(<<"guild">>, Data),
    Features = maps:get(<<"features">>, Guild, []),
    lists:member(<<"UNAVAILABLE_FOR_EVERYONE">>, Features) orelse
        lists:member(<<"UNAVAILABLE_FOR_EVERYONE_BUT_STAFF">>, Features).

process_dispatch(Event, EventData, State) ->
    GuildId = maps:get(id, State),

    {SessionIdOpt, CleanData} = extract_session_id_if_needed(Event, EventData),
    DecoratedData = maps:put(<<"guild_id">>, integer_to_binary(GuildId), CleanData),
    FinalData = decorate_member_data(Event, DecoratedData, State),

    UpdatedState = update_state(Event, FinalData, State),
    Sessions = maps:get(sessions, UpdatedState, #{}),

    FilteredSessions = filter_sessions_for_event(
        Event, FinalData, SessionIdOpt, Sessions, UpdatedState
    ),
    dispatch_to_sessions(FilteredSessions, Event, FinalData, UpdatedState),

    maybe_send_push_notifications(Event, FinalData, GuildId, UpdatedState),
    maybe_broadcast_member_list_update(Event, FinalData, State, UpdatedState),

    {noreply, UpdatedState}.

extract_session_id_if_needed(Event, EventData) ->
    case Event of
        message_reaction_add -> extract_and_remove_session_id(EventData);
        message_reaction_remove -> extract_and_remove_session_id(EventData);
        _ -> {undefined, EventData}
    end.

filter_sessions_for_event(Event, FinalData, SessionIdOpt, Sessions, UpdatedState) ->
    case is_channel_scoped_event(Event) of
        true ->
            ChannelId = extract_channel_id(Event, FinalData),
            filter_sessions_for_channel(Sessions, ChannelId, SessionIdOpt, UpdatedState);
        false ->
            case is_invite_event(Event) of
                true ->
                    ChannelIdBin = maps:get(<<"channel_id">>, FinalData, <<"0">>),
                    ChannelId = validation:snowflake_or_default(<<"channel_id">>, ChannelIdBin, 0),
                    filter_sessions_for_manage_channels(
                        Sessions, ChannelId, SessionIdOpt, UpdatedState
                    );
                false ->
                    case is_bulk_update_event(Event) of
                        true ->
                            filter_sessions_exclude_session(Sessions, SessionIdOpt);
                        false ->
                            filter_sessions_exclude_session(Sessions, SessionIdOpt)
                    end
            end
    end.

is_channel_scoped_event(channel_create) -> true;
is_channel_scoped_event(channel_update) -> true;
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
is_channel_scoped_event(_) -> false.

is_invite_event(invite_create) -> true;
is_invite_event(invite_delete) -> true;
is_invite_event(_) -> false.

is_bulk_update_event(channel_update_bulk) -> true;
is_bulk_update_event(_) -> false.

extract_channel_id(Event, FinalData) ->
    case Event of
        channel_create ->
            ChannelIdBin = maps:get(<<"id">>, FinalData, <<"0">>),
            validation:snowflake_or_default(<<"id">>, ChannelIdBin, 0);
        channel_update ->
            ChannelIdBin = maps:get(<<"id">>, FinalData, <<"0">>),
            validation:snowflake_or_default(<<"id">>, ChannelIdBin, 0);
        _ ->
            ChannelIdBin = maps:get(<<"channel_id">>, FinalData, <<"0">>),
            validation:snowflake_or_default(<<"channel_id">>, ChannelIdBin, 0)
    end.

dispatch_to_sessions(FilteredSessions, Event, FinalData, UpdatedState) ->
    GuildId = maps:get(id, UpdatedState),
    case is_bulk_update_event(Event) of
        true ->
            dispatch_bulk_update(FilteredSessions, Event, FinalData, UpdatedState);
        false ->
            dispatch_standard(FilteredSessions, Event, FinalData, GuildId, UpdatedState)
    end.

dispatch_bulk_update(FilteredSessions, Event, FinalData, UpdatedState) ->
    GuildId = maps:get(id, UpdatedState),
    BulkChannels = maps:get(<<"channels">>, FinalData, []),
    lists:foreach(
        fun({_Sid, SessionData}) ->
            Pid = maps:get(pid, SessionData),
            UserId = maps:get(user_id, SessionData),
            Member = find_member_by_user_id(UserId, UpdatedState),

            case should_receive_event(Event, FinalData, GuildId, SessionData, UpdatedState) of
                false ->
                    ok;
                true ->
                    FilteredChannels = lists:filter(
                        fun(Channel) ->
                            ChannelIdBin = maps:get(<<"id">>, Channel, <<"0">>),
                            ChannelId = validation:snowflake_or_default(<<"id">>, ChannelIdBin, 0),
                            case Member of
                                undefined ->
                                    false;
                                _ ->
                                    guild_permissions:can_view_channel(
                                        UserId, ChannelId, Member, UpdatedState
                                    )
                            end
                        end,
                        BulkChannels
                    ),

                    case FilteredChannels of
                        [] ->
                            ok;
                        _ when is_pid(Pid) ->
                            CustomData = maps:put(<<"channels">>, FilteredChannels, FinalData),
                            gen_server:cast(Pid, {dispatch, Event, CustomData})
                    end
            end
        end,
        FilteredSessions
    ).

dispatch_standard(FilteredSessions, Event, FinalData, GuildId, State) ->
    lists:foreach(
        fun({_Sid, SessionData}) ->
            Pid = maps:get(pid, SessionData),
            case is_pid(Pid) andalso should_receive_event(Event, FinalData, GuildId, SessionData, State) of
                true ->
                    gen_server:cast(Pid, {dispatch, Event, FinalData});
                false ->
                    ok
            end
        end,
        FilteredSessions
    ).

maybe_send_push_notifications(message_create, FinalData, GuildId, UpdatedState) ->
    spawn(fun() ->
        collect_and_send_push_notifications(FinalData, GuildId, UpdatedState)
    end);
maybe_send_push_notifications(_Event, _FinalData, _GuildId, _UpdatedState) ->
    ok.

maybe_broadcast_member_list_update(guild_member_add, EventData, OldState, UpdatedState) ->
    UserId = extract_user_id_from_event(EventData),
    guild_member_list:broadcast_member_list_updates(UserId, OldState, UpdatedState);
maybe_broadcast_member_list_update(guild_member_remove, EventData, OldState, UpdatedState) ->
    UserId = extract_user_id_from_event(EventData),
    guild_member_list:broadcast_member_list_updates(UserId, OldState, UpdatedState);
maybe_broadcast_member_list_update(guild_member_update, EventData, OldState, UpdatedState) ->
    UserId = extract_user_id_from_event(EventData),
    guild_member_list:broadcast_member_list_updates(UserId, OldState, UpdatedState);
maybe_broadcast_member_list_update(guild_role_create, _EventData, _OldState, UpdatedState) ->
    guild_member_list:broadcast_all_member_list_updates(UpdatedState);
maybe_broadcast_member_list_update(guild_role_update, _EventData, _OldState, UpdatedState) ->
    guild_member_list:broadcast_all_member_list_updates(UpdatedState);
maybe_broadcast_member_list_update(guild_role_update_bulk, _EventData, _OldState, UpdatedState) ->
    guild_member_list:broadcast_all_member_list_updates(UpdatedState);
maybe_broadcast_member_list_update(guild_role_delete, _EventData, _OldState, UpdatedState) ->
    guild_member_list:broadcast_all_member_list_updates(UpdatedState);
maybe_broadcast_member_list_update(channel_update, EventData, _OldState, UpdatedState) ->
    ChannelIdBin = maps:get(<<"id">>, EventData, <<"0">>),
    ChannelId = validation:snowflake_or_default(<<"id">>, ChannelIdBin, 0),
    guild_member_list:broadcast_member_list_updates_for_channel(ChannelId, UpdatedState);
maybe_broadcast_member_list_update(channel_update_bulk, EventData, _OldState, UpdatedState) ->
    Channels = maps:get(<<"channels">>, EventData, []),
    lists:foreach(
        fun(Channel) ->
            ChannelIdBin = maps:get(<<"id">>, Channel, <<"0">>),
            ChannelId = validation:snowflake_or_default(<<"id">>, ChannelIdBin, 0),
            guild_member_list:broadcast_member_list_updates_for_channel(ChannelId, UpdatedState)
        end,
        Channels
    );
maybe_broadcast_member_list_update(_Event, _FinalData, _OldState, _UpdatedState) ->
    ok.

extract_user_id_from_event(EventData) ->
    MUser = maps:get(<<"user">>, EventData, #{}),
    utils:binary_to_integer_safe(maps:get(<<"id">>, MUser, <<"0">>)).

extract_and_remove_session_id(Data) ->
    case maps:get(<<"session_id">>, Data, undefined) of
        undefined -> {undefined, Data};
        SessionId -> {SessionId, maps:remove(<<"session_id">>, Data)}
    end.

decorate_member_data(Event, Data, State) ->
    case extract_member_for_event(Event, Data, State) of
        undefined ->
            Data;
        MemberData ->
            add_member_to_data(Event, Data, MemberData)
    end.

add_member_to_data(Event, Data, MemberData) ->
    case is_message_event(Event) of
        true ->
            case maps:is_key(<<"author">>, Data) of
                true ->
                    CleanMemberData = maps:remove(<<"user">>, MemberData),
                    maps:put(<<"member">>, CleanMemberData, Data);
                false ->
                    Data
            end;
        false ->
            case is_user_event(Event) of
                true ->
                    case maps:is_key(<<"user_id">>, Data) of
                        true -> maps:put(<<"member">>, MemberData, Data);
                        false -> Data
                    end;
                false ->
                    Data
            end
    end.

is_message_event(message_create) -> true;
is_message_event(message_update) -> true;
is_message_event(_) -> false.

is_user_event(typing_start) -> true;
is_user_event(message_reaction_add) -> true;
is_user_event(message_reaction_remove) -> true;
is_user_event(_) -> false.

extract_member_for_event(Event, Data, State) ->
    UserId = extract_user_id_for_event(Event, Data),
    case UserId of
        undefined -> undefined;
        Id -> find_member_by_user_id(Id, State)
    end.

extract_user_id_for_event(Event, Data) ->
    case is_message_event(Event) of
        true ->
            AuthorId = maps:get(<<"id">>, maps:get(<<"author">>, Data, #{}), undefined),
            case AuthorId of
                undefined ->
                    undefined;
                _ ->
                    case validation:validate_snowflake(<<"author.id">>, AuthorId) of
                        {ok, Id} ->
                            Id;
                        {error, _, Reason} ->
                            logger:warning("[guild_dispatch] Invalid field: ~p", [Reason]),
                            undefined
                    end
            end;
        false ->
            case is_user_event(Event) of
                true ->
                    UserId = maps:get(<<"user_id">>, Data, undefined),
                    case UserId of
                        undefined ->
                            undefined;
                        _ ->
                            case validation:validate_snowflake(<<"user_id">>, UserId) of
                                {ok, Id} ->
                                    Id;
                                {error, _, Reason} ->
                                    logger:warning("[guild_dispatch] Invalid field: ~p", [Reason]),
                                    undefined
                            end
                    end;
                false ->
                    undefined
            end
    end.

collect_and_send_push_notifications(MessageData, GuildId, State) ->
    case should_send_push_notifications(State) of
        false ->
            ok;
        true ->
            send_push_notifications(MessageData, GuildId, State)
    end.

should_send_push_notifications(State) ->
    Data = maps:get(data, State),
    Guild = maps:get(<<"guild">>, Data),
    DisabledOperationsBin = maps:get(<<"disabled_operations">>, Guild, <<"0">>),
    DisabledOperations = validation:snowflake_or_default(
        <<"disabled_operations">>, DisabledOperationsBin, 0
    ),
    (DisabledOperations band 1) =:= 0.

send_push_notifications(MessageData, GuildId, State) ->
    Data = maps:get(data, State),
    Members = maps:get(<<"members">>, Data, []),
    Sessions = maps:get(sessions, State, #{}),
    ChannelIdBin = maps:get(<<"channel_id">>, MessageData),
    ChannelId = validation:snowflake_or_default(<<"channel_id">>, ChannelIdBin, 0),

    case find_eligible_users_for_push(Members, Sessions, ChannelId, State) of
        [] ->
            ok;
        EligibleUserIds ->
            UserRolesMap = build_user_roles_map(Members, EligibleUserIds),
            send_push_to_eligible_users(MessageData, GuildId, EligibleUserIds, UserRolesMap, Data)
    end.

find_eligible_users_for_push(Members, Sessions, ChannelId, State) ->
    lists:filtermap(
        fun(Member) ->
            is_user_eligible_for_push(Member, Sessions, ChannelId, State)
        end,
        Members
    ).

is_user_eligible_for_push(Member, Sessions, ChannelId, State) ->
    MUser = maps:get(<<"user">>, Member, #{}),
    UserIdBin = maps:get(<<"id">>, MUser, <<"0">>),
    UserId = validation:snowflake_or_default(<<"user.id">>, UserIdBin, 0),

    case guild_permissions:can_view_channel(UserId, ChannelId, Member, State) of
        false ->
            false;
        true ->
            check_user_session_eligibility(UserId, Sessions)
    end.

check_user_session_eligibility(UserId, Sessions) ->
    UserSessions = maps:filter(
        fun(_Sid, Session) ->
            maps:get(user_id, Session) =:= UserId
        end,
        Sessions
    ),

    case map_size(UserSessions) of
        0 ->
            {true, UserId};
        _ ->
            HasMobile = lists:any(
                fun(Session) -> maps:get(mobile, Session, false) end,
                maps:values(UserSessions)
            ),
            AllAfk = lists:all(
                fun(Session) -> maps:get(afk, Session, false) end,
                maps:values(UserSessions)
            ),
            case (not HasMobile) andalso AllAfk of
                true -> {true, UserId};
                false -> false
            end
    end.

send_push_to_eligible_users(MessageData, GuildId, EligibleUserIds, UserRolesMap, Data) ->
    Guild = maps:get(<<"guild">>, Data),
    AuthorIdBin = maps:get(<<"id">>, maps:get(<<"author">>, MessageData, #{}), <<"0">>),
    AuthorId = validation:snowflake_or_default(<<"author.id">>, AuthorIdBin, 0),
    DefaultMessageNotifications = maps:get(<<"default_message_notifications">>, Guild, 0),
    GuildName = maps:get(<<"name">>, Guild, <<"Unknown">>),

    ChannelIdBin = maps:get(<<"channel_id">>, MessageData),
    ChannelName = find_channel_name(ChannelIdBin, Data),

    push:handle_message_create(#{
        message_data => MessageData,
        user_ids => EligibleUserIds,
        guild_id => GuildId,
        author_id => AuthorId,
        guild_default_notifications => DefaultMessageNotifications,
        guild_name => GuildName,
        channel_name => ChannelName,
        user_roles => UserRolesMap
    }).

find_channel_name(ChannelIdBin, Data) ->
    Channels = maps:get(<<"channels">>, Data, []),
    lists:foldl(
        fun(Channel, Acc) ->
            case maps:get(<<"id">>, Channel, <<"">>) of
                ChannelIdBin -> maps:get(<<"name">>, Channel, <<"unknown">>);
                _ -> Acc
            end
        end,
        <<"unknown">>,
        Channels
    ).

build_user_roles_map(Members, EligibleUserIds) ->
    EligibleSet = sets:from_list(EligibleUserIds),
    lists:foldl(
        fun(Member, Acc) ->
            case get_member_user_id(Member) of
                0 -> Acc;
                UserId ->
                    case sets:is_element(UserId, EligibleSet) of
                        true ->
                            Roles = extract_role_ids(Member),
                            maps:put(UserId, Roles, Acc);
                        false ->
                            Acc
                    end
            end
        end,
        #{},
        Members
    ).

get_member_user_id(Member) ->
    User = maps:get(<<"user">>, Member, #{}),
    case maps:get(<<"id">>, User, undefined) of
        undefined ->
            0;
        Id ->
            validation:snowflake_or_default(<<"member.user.id">>, Id, 0)
    end.

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
