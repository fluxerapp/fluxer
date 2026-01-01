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

-module(session_dispatch).

-export([
    handle_dispatch/3,
    flush_all_pending_presences/1
]).

handle_dispatch(Event, Data, State) ->
    case should_ignore_event(Event, State) of
        true ->
            {noreply, State};
        false ->
            case should_buffer_presence(Event, Data, State) of
                true ->
                    {noreply, buffer_presence(Event, Data, State)};
                false ->
                    Seq = maps:get(seq, State),
                    Buffer = maps:get(buffer, State),
                    SocketPid = maps:get(socket_pid, State, undefined),

                    NewSeq = Seq + 1,
                    Request = #{event => Event, data => Data, seq => NewSeq},

                    NewBuffer =
                        case Event of
                            message_reaction_add ->
                                Buffer;
                            message_reaction_remove ->
                                Buffer;
                            _ ->
                                Buffer ++ [Request]
                        end,

                    case SocketPid of
                        undefined ->
                            ok;
                        Pid when is_pid(Pid) ->
                            case erlang:is_process_alive(Pid) of
                                true ->
                                    Pid ! {dispatch, Event, Data, NewSeq},
                                    ok;
                                false ->
                                    ok
                            end
                    end,

                    StateWithChannels = update_channels_map(Event, Data, State),
                    StateWithRelationships0 = update_relationships_map(
                        Event, Data, StateWithChannels
                    ),
                    StateAfterMain = maps:merge(StateWithRelationships0, #{
                        seq => NewSeq, buffer => NewBuffer
                    }),
                    StateWithPending = maybe_flush_pending_presences(Event, Data, StateAfterMain),
                    FinalState = sync_presence_targets(StateWithPending),
                    {noreply, FinalState}
            end
    end.

should_buffer_presence(presence_update, Data, State) ->
    case maps:get(suppress_presence_updates, State, true) of
        true ->
            true;
        false ->
            HasGuildId =
                is_map(Data) andalso (maps:get(<<"guild_id">>, Data, undefined) =/= undefined),
            case HasGuildId of
                true ->
                    false;
                false ->
                    UserId = presence_user_id(Data),
                    Relationships = maps:get(relationships, State, #{}),
                    case UserId of
                        undefined ->
                            false;
                        _ ->
                            IsRelationship = relationship_allows_presence(UserId, Relationships),
                            IsGroupDmRecipient = is_group_dm_recipient(UserId, State),
                            not (IsRelationship orelse IsGroupDmRecipient)
                    end
            end
    end;
should_buffer_presence(_, _, _) ->
    false.

relationship_allows_presence(UserId, Relationships) when
    is_integer(UserId), is_map(Relationships)
->
    case maps:get(UserId, Relationships, 0) of
        1 -> true;
        3 -> true;
        _ -> false
    end;
relationship_allows_presence(_, _) ->
    false.

is_group_dm_recipient(UserId, State) ->
    GroupDmRecipients = presence_targets:group_dm_recipients_from_state(State),
    lists:any(
        fun({_ChannelId, Recipients}) ->
            maps:is_key(UserId, Recipients)
        end,
        maps:to_list(GroupDmRecipients)
    ).

buffer_presence(Event, Data, State) ->
    Pending = maps:get(pending_presences, State, []),
    UserId = presence_user_id(Data),
    maps:put(
        pending_presences, Pending ++ [#{event => Event, data => Data, user_id => UserId}], State
    ).

maybe_flush_pending_presences(relationship_add, Data, State) ->
    maybe_flush_relationship_pending_presences(Data, State);
maybe_flush_pending_presences(relationship_update, Data, State) ->
    maybe_flush_relationship_pending_presences(Data, State);
maybe_flush_pending_presences(_, _, State) ->
    State.

maybe_flush_relationship_pending_presences(Data, State) when is_map(Data) ->
    case maps:get(<<"type">>, Data, 0) of
        1 ->
            flush_pending_presences(relationship_target_id(Data), State);
        3 ->
            flush_pending_presences(relationship_target_id(Data), State);
        _ ->
            State
    end;
maybe_flush_relationship_pending_presences(_Data, State) ->
    State.

flush_pending_presences(undefined, State) ->
    State;
flush_pending_presences(UserId, State) ->
    Pending = maps:get(pending_presences, State, []),
    {ToSend, Remaining} =
        lists:partition(fun(P) -> maps:get(user_id, P, undefined) =:= UserId end, Pending),
    FlushedState =
        lists:foldl(
            fun(P, AccState) ->
                dispatch_presence_now(P, AccState)
            end,
            State,
            ToSend
        ),
    maps:put(pending_presences, Remaining, FlushedState).

dispatch_presence_now(P, State) ->
    Event = maps:get(event, P),
    Data = maps:get(data, P),
    Seq = maps:get(seq, State),
    Buffer = maps:get(buffer, State),
    SocketPid = maps:get(socket_pid, State, undefined),

    NewSeq = Seq + 1,
    Request = #{event => Event, data => Data, seq => NewSeq},
    NewBuffer = Buffer ++ [Request],

    case SocketPid of
        undefined ->
            ok;
        Pid when is_pid(Pid) ->
            case erlang:is_process_alive(Pid) of
                true ->
                    Pid ! {dispatch, Event, Data, NewSeq},
                    ok;
                false ->
                    ok
            end
    end,

    maps:merge(State, #{seq => NewSeq, buffer => NewBuffer}).

presence_user_id(Data) when is_map(Data) ->
    User = maps:get(<<"user">>, Data, #{}),
    map_utils:get_integer(User, <<"id">>, undefined);
presence_user_id(_) ->
    undefined.

relationship_target_id(Data) when is_map(Data) ->
    type_conv:extract_id(Data, <<"id">>).

flush_all_pending_presences(State) ->
    Pending = maps:get(pending_presences, State, []),
    FlushedState =
        lists:foldl(
            fun(P, AccState) ->
                dispatch_presence_now(P, AccState)
            end,
            State,
            Pending
        ),
    maps:put(pending_presences, [], FlushedState).

should_ignore_event(Event, State) ->
    IgnoredEvents = maps:get(ignored_events, State, #{}),
    case event_name(Event) of
        undefined ->
            false;
        EventName ->
            maps:is_key(EventName, IgnoredEvents)
    end.

event_name(Event) when is_binary(Event) ->
    Event;
event_name(Event) when is_atom(Event) ->
    try constants:dispatch_event_atom(Event) of
        Name when is_binary(Name) ->
            Name
    catch
        _:_ ->
            undefined
    end;
event_name(_) ->
    undefined.

update_channels_map(channel_create, Data, State) when is_map(Data) ->
    case maps:get(<<"type">>, Data, undefined) of
        1 ->
            add_channel_to_state(Data, State);
        3 ->
            add_channel_to_state(Data, State);
        _ ->
            State
    end;
update_channels_map(channel_update, Data, State) when is_map(Data) ->
    case maps:get(<<"type">>, Data, undefined) of
        1 ->
            add_channel_to_state(Data, State);
        3 ->
            add_channel_to_state(Data, State);
        _ ->
            State
    end;
update_channels_map(channel_delete, Data, State) when is_map(Data) ->
    case maps:get(<<"id">>, Data, undefined) of
        undefined ->
            State;
        ChannelIdBin ->
            case validation:validate_snowflake(<<"id">>, ChannelIdBin) of
                {ok, ChannelId} ->
                    Channels = maps:get(channels, State, #{}),
                    NewChannels = maps:remove(ChannelId, Channels),
                    maps:put(channels, NewChannels, State);
                {error, _, _} ->
                    State
            end
    end;
update_channels_map(channel_recipient_add, Data, State) when is_map(Data) ->
    update_recipient_membership(add, Data, State);
update_channels_map(channel_recipient_remove, Data, State) when is_map(Data) ->
    update_recipient_membership(remove, Data, State);
update_channels_map(_Event, _Data, State) ->
    State.

add_channel_to_state(Data, State) ->
    case maps:get(<<"id">>, Data, undefined) of
        undefined ->
            State;
        ChannelIdBin ->
            case validation:validate_snowflake(<<"id">>, ChannelIdBin) of
                {ok, ChannelId} ->
                    Channels = maps:get(channels, State, #{}),
                    NewChannels = maps:put(ChannelId, Data, Channels),
                    UserId = maps:get(user_id, State),
                    logger:info(
                        "[session_dispatch] Added/updated channel ~p for user ~p, type: ~p",
                        [ChannelId, UserId, maps:get(<<"type">>, Data, 0)]
                    ),
                    maps:put(channels, NewChannels, State);
                {error, _, _} ->
                    State
            end
    end.

update_recipient_membership(Action, Data, State) ->
    ChannelIdBin = maps:get(<<"channel_id">>, Data, undefined),
    case validation:validate_snowflake(<<"channel_id">>, ChannelIdBin) of
        {ok, ChannelId} ->
            Channels = maps:get(channels, State, #{}),
            case maps:get(ChannelId, Channels, undefined) of
                undefined ->
                    State;
                Channel ->
                    case maps:get(<<"type">>, Channel, 0) of
                        3 ->
                            UserMap = maps:get(<<"user">>, Data, #{}),
                            RecipientId = type_conv:extract_id(UserMap, <<"id">>),
                            case RecipientId of
                                undefined ->
                                    State;
                                _ ->
                                    UpdatedChannel = update_channel_recipient(
                                        Channel, RecipientId, UserMap, Action
                                    ),
                                    NewChannels = maps:put(ChannelId, UpdatedChannel, Channels),
                                    maps:put(channels, NewChannels, State)
                            end;
                        _ ->
                            State
                    end
            end;
        _ ->
            State
    end.

update_channel_recipient(Channel, RecipientId, UserMap, add) ->
    RecipientIds = maps:get(<<"recipient_ids">>, Channel, []),
    Recipients = maps:get(<<"recipients">>, Channel, []),
    NewRecipientIds = add_unique_id(RecipientId, RecipientIds),
    NewRecipients = add_unique_user(UserMap, Recipients),
    Channel#{<<"recipient_ids">> => NewRecipientIds, <<"recipients">> => NewRecipients};
update_channel_recipient(Channel, RecipientId, _UserMap, remove) ->
    RecipientIds = maps:get(<<"recipient_ids">>, Channel, []),
    Recipients = maps:get(<<"recipients">>, Channel, []),
    NewRecipientIds = lists:filter(
        fun(Id) -> Id =/= integer_to_binary(RecipientId) andalso Id =/= RecipientId end,
        RecipientIds
    ),
    NewRecipients = lists:filter(
        fun(R) ->
            case type_conv:extract_id(R, <<"id">>) of
                RecipientId -> false;
                _ -> true
            end
        end,
        Recipients
    ),
    Channel#{<<"recipient_ids">> => NewRecipientIds, <<"recipients">> => NewRecipients}.

add_unique_id(Id, List) ->
    case lists:member(Id, List) orelse lists:member(integer_to_binary(Id), List) of
        true -> List;
        false -> [Id | List]
    end.

add_unique_user(UserMap, List) when is_map(UserMap) ->
    case type_conv:extract_id(UserMap, <<"id">>) of
        undefined ->
            List;
        Id ->
            case
                lists:any(
                    fun(R) -> type_conv:extract_id(R, <<"id">>) =:= Id end,
                    List
                )
            of
                true -> List;
                false -> [UserMap | List]
            end
    end.

update_relationships_map(relationship_add, Data, State) ->
    upsert_relationship(Data, State);
update_relationships_map(relationship_update, Data, State) ->
    upsert_relationship(Data, State);
update_relationships_map(relationship_remove, Data, State) ->
    case type_conv:extract_id(Data, <<"id">>) of
        undefined ->
            State;
        UserId ->
            Relationships = maps:get(relationships, State, #{}),
            NewRelationships = maps:remove(UserId, Relationships),
            maps:put(relationships, NewRelationships, State)
    end;
update_relationships_map(_, _, State) ->
    State.

upsert_relationship(Data, State) ->
    case type_conv:extract_id(Data, <<"id">>) of
        undefined ->
            State;
        UserId ->
            Type = maps:get(<<"type">>, Data, 0),
            Relationships = maps:get(relationships, State, #{}),
            NewRelationships = maps:put(UserId, Type, Relationships),
            maps:put(relationships, NewRelationships, State)
    end.

sync_presence_targets(State) ->
    PresencePid = maps:get(presence_pid, State, undefined),
    case PresencePid of
        undefined ->
            State;
        Pid when is_pid(Pid) ->
            FriendIds = presence_targets:friend_ids_from_state(State),
            GroupRecipients = presence_targets:group_dm_recipients_from_state(State),
            gen_server:cast(Pid, {sync_friends, FriendIds}),
            gen_server:cast(Pid, {sync_group_dm_recipients, GroupRecipients}),
            State
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

base_state_for_presence_buffer_test(Opts) ->
    maps:merge(
        #{
            seq => 0,
            user_id => 1,
            buffer => [],
            socket_pid => undefined,
            channels => #{},
            relationships => #{},
            suppress_presence_updates => false,
            pending_presences => [],
            presence_pid => undefined,
            ignored_events => #{}
        },
        Opts
    ).

presence_update_with_guild_id_not_buffered_test() ->
    State0 = base_state_for_presence_buffer_test(#{}),
    Presence = #{
        <<"guild_id">> => <<"123">>,
        <<"user">> => #{<<"id">> => <<"2">>},
        <<"status">> => <<"idle">>
    },
    {noreply, State1} = handle_dispatch(presence_update, Presence, State0),
    ?assertEqual([], maps:get(pending_presences, State1, [])),
    ?assertEqual(1, length(maps:get(buffer, State1, []))),
    ok.

presence_update_without_guild_id_buffered_for_non_relationship_test() ->
    State0 = base_state_for_presence_buffer_test(#{}),
    Presence = #{
        <<"user">> => #{<<"id">> => <<"2">>},
        <<"status">> => <<"online">>
    },
    {noreply, State1} = handle_dispatch(presence_update, Presence, State0),
    ?assertEqual(1, length(maps:get(pending_presences, State1, []))),
    ?assertEqual([], maps:get(buffer, State1, [])),
    ok.

presence_update_without_guild_id_not_buffered_for_relationship_test() ->
    State0 = base_state_for_presence_buffer_test(#{relationships => #{2 => 1}}),
    Presence = #{
        <<"user">> => #{<<"id">> => <<"2">>},
        <<"status">> => <<"online">>
    },
    {noreply, State1} = handle_dispatch(presence_update, Presence, State0),
    ?assertEqual([], maps:get(pending_presences, State1, [])),
    ?assertEqual(1, length(maps:get(buffer, State1, []))),
    ok.

presence_update_without_guild_id_buffered_for_outgoing_request_relationship_test() ->
    State0 = base_state_for_presence_buffer_test(#{relationships => #{2 => 4}}),
    Presence = #{
        <<"user">> => #{<<"id">> => <<"2">>},
        <<"status">> => <<"online">>
    },
    {noreply, State1} = handle_dispatch(presence_update, Presence, State0),
    ?assertEqual(1, length(maps:get(pending_presences, State1, []))),
    ?assertEqual([], maps:get(buffer, State1, [])),
    ok.

presence_update_without_guild_id_not_buffered_for_incoming_request_relationship_test() ->
    State0 = base_state_for_presence_buffer_test(#{relationships => #{2 => 3}}),
    Presence = #{
        <<"user">> => #{<<"id">> => <<"2">>},
        <<"status">> => <<"online">>
    },
    {noreply, State1} = handle_dispatch(presence_update, Presence, State0),
    ?assertEqual([], maps:get(pending_presences, State1, [])),
    ?assertEqual(1, length(maps:get(buffer, State1, []))),
    ok.

-endif.
