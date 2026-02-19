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

-module(guild_passive_sync).

-export([
    schedule_passive_sync/1,
    handle_passive_sync/1,
    send_passive_updates_to_sessions/1,
    compute_delta/2,
    compute_channel_diffs/2
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(PASSIVE_SYNC_INTERVAL, 30000).

-type guild_state() :: map().
-type channel_id() :: binary().
-type last_message_id() :: binary().
-type version() :: integer().
-type voice_state() :: map().

-spec schedule_passive_sync(guild_state()) -> guild_state().
schedule_passive_sync(State) ->
    erlang:send_after(?PASSIVE_SYNC_INTERVAL, self(), passive_sync),
    State.

-spec handle_passive_sync(guild_state()) -> {noreply, guild_state()}.
handle_passive_sync(State) ->
    NewState = send_passive_updates_to_sessions(State),
    schedule_passive_sync(NewState),
    {noreply, NewState}.

-spec send_passive_updates_to_sessions(guild_state()) -> guild_state().
send_passive_updates_to_sessions(State) ->
    GuildId = maps:get(id, State),
    Sessions = maps:get(sessions, State, #{}),
    Data = maps:get(data, State, #{}),
    Channels = maps:get(<<"channels">>, Data, []),
    MemberCount = maps:get(member_count, State, undefined),
    IsLargeGuild =
        case MemberCount of
            undefined -> false;
            Count when is_integer(Count) -> Count > 250
        end,
    PassiveSessions = maps:filter(
        fun(_SessionId, SessionData) ->
            IsLargeGuild andalso session_passive:is_passive(GuildId, SessionData)
        end,
        Sessions
    ),
    case map_size(PassiveSessions) of
        0 ->
            State;
        _ ->
            process_passive_sessions(
                maps:to_list(PassiveSessions), GuildId, Channels, State
            ),
            State
    end.

-spec process_passive_sessions([{binary(), map()}], integer(), [map()], guild_state()) ->
    ok.
process_passive_sessions(PassiveSessionList, GuildId, Channels, State) ->
    lists:foreach(
        fun({SessionId, SessionData}) ->
            process_single_passive_session(
                SessionId, SessionData, GuildId, Channels, State
            )
        end,
        PassiveSessionList
    ).

-spec process_single_passive_session(binary(), map(), integer(), [map()], guild_state()) ->
    ok.
process_single_passive_session(SessionId, SessionData, GuildId, Channels, State) ->
    Pid = maps:get(pid, SessionData),
    UserId = maps:get(user_id, SessionData),
    Member = guild_permissions:find_member_by_user_id(UserId, State),
    CurrentLastMessageIds = build_last_message_ids(Channels, UserId, Member, State),
    RegState = passive_sync_registry:lookup(SessionId, GuildId),
    PreviousLastMessageIds = maps:get(previous_passive_updates, RegState, #{}),
    Delta = compute_delta(CurrentLastMessageIds, PreviousLastMessageIds),
    PreviousChannelVersions = maps:get(previous_passive_channel_versions, RegState, #{}),
    {CurrentChannelVersions, CurrentChannelsById} =
        build_viewable_channel_snapshots(Channels, UserId, Member, State),
    {CreatedChannelIds, UpdatedChannelIds, DeletedChannelIds} =
        compute_channel_diffs(CurrentChannelVersions, PreviousChannelVersions),
    CreatedChannels = [maps:get(Id, CurrentChannelsById) || Id <- CreatedChannelIds],
    UpdatedChannels = [maps:get(Id, CurrentChannelsById) || Id <- UpdatedChannelIds],
    ViewableChannels = guild_visibility:viewable_channel_set(UserId, State),
    CurrentVoiceStates = build_current_voice_state_map(ViewableChannels, State),
    PreviousVoiceStates = maps:get(previous_passive_voice_states, RegState, #{}),
    VoiceStateUpdates = compute_voice_state_updates(
        CurrentVoiceStates, PreviousVoiceStates, GuildId
    ),
    HasChannelDelta = map_size(Delta) > 0,
    HasVoiceUpdates = VoiceStateUpdates =/= [],
    HasCreatedChannels = CreatedChannels =/= [],
    HasUpdatedChannels = UpdatedChannels =/= [],
    HasDeletedChannels = DeletedChannelIds =/= [],
    ShouldSend =
        HasChannelDelta orelse HasVoiceUpdates orelse
            HasCreatedChannels orelse HasUpdatedChannels orelse HasDeletedChannels,
    case {ShouldSend, is_pid(Pid)} of
        {true, true} ->
            EventData = build_passive_event_data(
                GuildId,
                Delta,
                CreatedChannels,
                UpdatedChannels,
                DeletedChannelIds,
                VoiceStateUpdates
            ),
            gen_server:cast(Pid, {dispatch, passive_updates, EventData}),
            PreviousLastMessageIds1 = maps:without(DeletedChannelIds, PreviousLastMessageIds),
            MergedLastMessageIds = maps:merge(PreviousLastMessageIds1, Delta),
            NewRegState = #{
                previous_passive_updates => MergedLastMessageIds,
                previous_passive_channel_versions => CurrentChannelVersions,
                previous_passive_voice_states => CurrentVoiceStates
            },
            passive_sync_registry:store(SessionId, GuildId, NewRegState),
            ok;
        _ ->
            NewRegState = #{
                previous_passive_updates => PreviousLastMessageIds,
                previous_passive_channel_versions => CurrentChannelVersions,
                previous_passive_voice_states => CurrentVoiceStates
            },
            passive_sync_registry:store(SessionId, GuildId, NewRegState),
            ok
    end.

-spec build_passive_event_data(integer(), map(), [map()], [map()], [binary()], [map()]) -> map().
build_passive_event_data(
    GuildId, Delta, CreatedChannels, UpdatedChannels, DeletedChannelIds, VoiceStateUpdates
) ->
    EventDataBase = #{
        <<"guild_id">> => integer_to_binary(GuildId),
        <<"channels">> => Delta
    },
    EventData1 =
        case CreatedChannels of
            [] -> EventDataBase;
            _ -> maps:put(<<"created_channels">>, CreatedChannels, EventDataBase)
        end,
    EventData2 =
        case UpdatedChannels of
            [] -> EventData1;
            _ -> maps:put(<<"updated_channels">>, UpdatedChannels, EventData1)
        end,
    EventData3 =
        case DeletedChannelIds of
            [] -> EventData2;
            _ -> maps:put(<<"deleted_channel_ids">>, DeletedChannelIds, EventData2)
        end,
    case VoiceStateUpdates of
        [] -> EventData3;
        _ -> maps:put(<<"voice_states">>, VoiceStateUpdates, EventData3)
    end.

-spec compute_delta(#{channel_id() => last_message_id()}, #{channel_id() => last_message_id()}) ->
    #{channel_id() => last_message_id()}.
compute_delta(CurrentLastMessageIds, PreviousLastMessageIds) ->
    maps:filter(
        fun(ChannelId, CurrentValue) ->
            case maps:get(ChannelId, PreviousLastMessageIds, undefined) of
                undefined -> true;
                PreviousValue -> CurrentValue =/= PreviousValue
            end
        end,
        CurrentLastMessageIds
    ).

-spec compute_channel_diffs(#{channel_id() => version()}, #{channel_id() => version()}) ->
    {[channel_id()], [channel_id()], [channel_id()]}.
compute_channel_diffs(Current, Previous) ->
    Created =
        [Id || {Id, _} <- maps:to_list(Current), not maps:is_key(Id, Previous)],
    Updated =
        [
            Id
         || {Id, CurV} <- maps:to_list(Current),
            maps:is_key(Id, Previous) andalso maps:get(Id, Previous) =/= CurV
        ],
    Deleted =
        [Id || {Id, _} <- maps:to_list(Previous), not maps:is_key(Id, Current)],
    {Created, Updated, Deleted}.

-spec build_last_message_ids([map()], integer(), map() | undefined, guild_state()) ->
    #{channel_id() => last_message_id()}.
build_last_message_ids(Channels, UserId, Member, State) ->
    lists:foldl(
        fun(Channel, Acc) ->
            ChannelIdBin = maps:get(<<"id">>, Channel, undefined),
            LastMessageId = maps:get(<<"last_message_id">>, Channel, null),
            case {ChannelIdBin, LastMessageId} of
                {undefined, _} ->
                    Acc;
                {_, null} ->
                    Acc;
                _ ->
                    case parse_snowflake(<<"id">>, ChannelIdBin) of
                        undefined ->
                            Acc;
                        ChannelId ->
                            case Member of
                                undefined ->
                                    Acc;
                                _ ->
                                    case
                                        guild_permissions:can_view_channel(UserId, ChannelId, Member, State)
                                    of
                                        true ->
                                            maps:put(ChannelIdBin, LastMessageId, Acc);
                                        false ->
                                            Acc
                                    end
                            end
                    end
            end
        end,
        #{},
        Channels
    ).

-spec build_viewable_channel_snapshots([map()], integer(), map() | undefined, guild_state()) ->
    {#{channel_id() => version()}, #{channel_id() => map()}}.
build_viewable_channel_snapshots(Channels, UserId, Member, State) ->
    case Member of
        undefined ->
            {#{}, #{}};
        _ ->
            lists:foldl(
                fun(Channel, {VersionsAcc, ChannelsAcc}) ->
                    ChannelIdBin = maps:get(<<"id">>, Channel, undefined),
                    case ChannelIdBin of
                        undefined ->
                            {VersionsAcc, ChannelsAcc};
                        _ ->
                            case parse_snowflake(<<"id">>, ChannelIdBin) of
                                undefined ->
                                    {VersionsAcc, ChannelsAcc};
                                ChannelId ->
                                    case
                                        guild_permissions:can_view_channel(UserId, ChannelId, Member, State)
                                    of
                                        true ->
                                            Version = map_utils:get_integer(Channel, <<"version">>, 0),
                                            {
                                                maps:put(ChannelIdBin, Version, VersionsAcc),
                                                maps:put(ChannelIdBin, Channel, ChannelsAcc)
                                            };
                                        false ->
                                            {VersionsAcc, ChannelsAcc}
                                    end
                            end
                    end
                end,
                {#{}, #{}},
                Channels
            )
    end.

-spec build_current_voice_state_map(sets:set(), guild_state()) -> #{binary() => voice_state()}.
build_current_voice_state_map(ViewableChannels, State) ->
    VoiceStates = maps:get(voice_states, State, #{}),
    maps:fold(
        fun(ConnectionId, VoiceState, Acc) ->
            ChannelIdBin = maps:get(<<"channel_id">>, VoiceState, null),
            case ChannelIdBin of
                null ->
                    Acc;
                _ ->
                    case parse_snowflake(<<"channel_id">>, ChannelIdBin) of
                        undefined ->
                            Acc;
                        ChannelId ->
                            case sets:is_element(ChannelId, ViewableChannels) of
                                true -> maps:put(ConnectionId, VoiceState, Acc);
                                false -> Acc
                            end
                    end
            end
        end,
        #{},
        VoiceStates
    ).

-spec parse_snowflake(binary(), term()) -> integer() | undefined.
parse_snowflake(FieldName, Value) ->
    case validation:validate_snowflake(FieldName, Value) of
        {ok, Id} -> Id;
        {error, _, _} -> undefined
    end.

-spec compute_voice_state_updates(
    #{binary() => voice_state()}, #{binary() => voice_state()}, integer()
) ->
    [voice_state()].
compute_voice_state_updates(Current, Previous, GuildId) ->
    GuildIdBin = integer_to_binary(GuildId),
    Updated = maps:fold(
        fun(ConnectionId, VoiceState, Acc) ->
            PrevState = maps:get(ConnectionId, Previous, undefined),
            case is_voice_state_changed(VoiceState, PrevState) of
                true -> [ensure_voice_state_guild(VoiceState, GuildIdBin) | Acc];
                false -> Acc
            end
        end,
        [],
        Current
    ),
    Removed = maps:fold(
        fun(ConnectionId, PrevState, Acc) ->
            case maps:is_key(ConnectionId, Current) of
                true -> Acc;
                false -> [build_removed_voice_state(PrevState, GuildIdBin) | Acc]
            end
        end,
        [],
        Previous
    ),
    lists:reverse(Updated) ++ lists:reverse(Removed).

-spec is_voice_state_changed(voice_state(), voice_state() | undefined) -> boolean().
is_voice_state_changed(_Current, undefined) ->
    true;
is_voice_state_changed(Current, Previous) ->
    voice_state_version(Current) =/= voice_state_version(Previous).

-spec voice_state_version(voice_state()) -> integer().
voice_state_version(VoiceState) ->
    map_utils:get_integer(VoiceState, <<"version">>, 0).

-spec ensure_voice_state_guild(voice_state(), binary()) -> voice_state().
ensure_voice_state_guild(VoiceState, GuildIdBin) ->
    case maps:get(<<"guild_id">>, VoiceState, undefined) of
        undefined -> maps:put(<<"guild_id">>, GuildIdBin, VoiceState);
        _ -> VoiceState
    end.

-spec build_removed_voice_state(voice_state(), binary()) -> voice_state().
build_removed_voice_state(PrevState, GuildIdBin) ->
    PrevWithGuild = ensure_voice_state_guild(PrevState, GuildIdBin),
    maps:put(<<"channel_id">>, null, PrevWithGuild).

-ifdef(TEST).

compute_delta_empty_previous_test() ->
    Current = #{<<"1">> => <<"100">>, <<"2">> => <<"200">>},
    Previous = #{},
    Delta = compute_delta(Current, Previous),
    ?assertEqual(Current, Delta).

compute_delta_no_changes_test() ->
    Current = #{<<"1">> => <<"100">>, <<"2">> => <<"200">>},
    Previous = #{<<"1">> => <<"100">>, <<"2">> => <<"200">>},
    Delta = compute_delta(Current, Previous),
    ?assertEqual(#{}, Delta).

compute_delta_partial_changes_test() ->
    Current = #{<<"1">> => <<"101">>, <<"2">> => <<"200">>, <<"3">> => <<"300">>},
    Previous = #{<<"1">> => <<"100">>, <<"2">> => <<"200">>},
    Delta = compute_delta(Current, Previous),
    ?assertEqual(#{<<"1">> => <<"101">>, <<"3">> => <<"300">>}, Delta).

compute_delta_only_new_channels_test() ->
    Current = #{<<"1">> => <<"100">>, <<"2">> => <<"200">>, <<"3">> => <<"300">>},
    Previous = #{<<"1">> => <<"100">>, <<"2">> => <<"200">>},
    Delta = compute_delta(Current, Previous),
    ?assertEqual(#{<<"3">> => <<"300">>}, Delta).

compute_delta_ignores_removed_channels_test() ->
    Current = #{<<"1">> => <<"100">>},
    Previous = #{<<"1">> => <<"100">>, <<"2">> => <<"200">>},
    Delta = compute_delta(Current, Previous),
    ?assertEqual(#{}, Delta).

compute_channel_diffs_detects_created_updated_deleted_test() ->
    Current = #{<<"1">> => 2, <<"2">> => 1},
    Previous = #{<<"1">> => 1, <<"3">> => 9},
    {Created, Updated, Deleted} = compute_channel_diffs(Current, Previous),
    ?assertEqual([<<"2">>], lists:sort(Created)),
    ?assertEqual([<<"1">>], lists:sort(Updated)),
    ?assertEqual([<<"3">>], lists:sort(Deleted)).

compute_voice_state_updates_reports_changes_test() ->
    PrevVoiceState = #{
        <<"connection_id">> => <<"conn1">>,
        <<"channel_id">> => <<"100">>,
        <<"user_id">> => <<"200">>,
        <<"version">> => 1
    },
    CurrentVoiceState = maps:put(<<"version">>, 2, PrevVoiceState),
    Current = #{<<"conn1">> => CurrentVoiceState},
    Previous = #{<<"conn1">> => PrevVoiceState},
    Updates = compute_voice_state_updates(Current, Previous, 999),
    ?assertEqual(1, length(Updates)),
    Update = hd(Updates),
    ?assertEqual(<<"conn1">>, maps:get(<<"connection_id">>, Update)),
    ?assertEqual(integer_to_binary(999), maps:get(<<"guild_id">>, Update)).

compute_voice_state_updates_reports_removal_test() ->
    RemovedVoiceState = #{
        <<"connection_id">> => <<"conn2">>,
        <<"channel_id">> => <<"200">>,
        <<"user_id">> => <<"300">>,
        <<"version">> => 3
    },
    Current = #{},
    Previous = #{<<"conn2">> => RemovedVoiceState},
    Updates = compute_voice_state_updates(Current, Previous, 101),
    ?assertEqual(1, length(Updates)),
    Update = hd(Updates),
    ?assertEqual(null, maps:get(<<"channel_id">>, Update)),
    ?assertEqual(integer_to_binary(101), maps:get(<<"guild_id">>, Update)).

-endif.
