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

-module(session_ready).

-export([
    process_guild_state/2,
    mark_guild_unavailable/2,
    check_readiness/1,
    dispatch_ready_data/1,
    update_ready_guilds/2
]).

process_guild_state(GuildState, State) ->
    Ready = maps:get(ready, State),
    CollectedGuilds = maps:get(collected_guild_states, State),

    case Ready of
        undefined ->
            {noreply, StateAfterCreate} = session_dispatch:handle_dispatch(
                guild_create, GuildState, State
            ),
            dispatch_guild_initial_presences(GuildState, StateAfterCreate);
        _ ->
            NewCollectedGuilds = [GuildState | CollectedGuilds],
            NewState = maps:put(collected_guild_states, NewCollectedGuilds, State),
            check_readiness(update_ready_guilds(GuildState, NewState))
    end.

dispatch_guild_initial_presences(_GuildState, State) ->
    {noreply, State}.

mark_guild_unavailable(GuildId, State) ->
    CollectedGuilds = maps:get(collected_guild_states, State),
    Ready = maps:get(ready, State),

    UnavailableState = #{<<"id">> => integer_to_binary(GuildId), <<"unavailable">> => true},
    NewCollectedGuilds = [UnavailableState | CollectedGuilds],
    NewState = maps:put(collected_guild_states, NewCollectedGuilds, State),
    case Ready of
        undefined -> {noreply, NewState};
        _ -> {noreply, update_ready_guilds(UnavailableState, NewState)}
    end.

check_readiness(State) ->
    Ready = maps:get(ready, State),
    PresencePid = maps:get(presence_pid, State, undefined),
    Guilds = maps:get(guilds, State),

    case Ready of
        undefined ->
            {noreply, State};
        _ when PresencePid =/= undefined ->
            AllGuildsReady = lists:all(fun({_, V}) -> V =/= undefined end, maps:to_list(Guilds)),
            if
                AllGuildsReady -> dispatch_ready_data(State);
                true -> {noreply, State}
            end;
        _ ->
            {noreply, State}
    end.

dispatch_ready_data(State) ->
    Ready = maps:get(ready, State),
    CollectedGuilds = maps:get(collected_guild_states, State),
    CollectedSessions = maps:get(collected_sessions, State),
    CollectedPresences = collect_ready_presences(State, CollectedGuilds),
    Users = collect_ready_users(State, CollectedGuilds),
    Version = maps:get(version, State),
    UserId = maps:get(user_id, State),
    SessionId = maps:get(id, State),
    SocketPid = maps:get(socket_pid, State, undefined),
    Guilds = maps:get(guilds, State),
    IsBot = maps:get(bot, State, false),

    ReadyData =
        case Ready of
            undefined -> #{<<"guilds">> => []};
            R -> R
        end,

    ReadyDataWithStrippedRelationships = strip_user_from_relationships(ReadyData),

    ReadyDataBotStripped =
        case IsBot of
            true -> maps:put(<<"guilds">>, [], ReadyDataWithStrippedRelationships);
            false -> ReadyDataWithStrippedRelationships
        end,

    UnavailableGuilds = [
        #{<<"id">> => integer_to_binary(GuildId), <<"unavailable">> => true}
     || {GuildId, undefined} <- maps:to_list(Guilds)
    ],
    StrippedGuilds = [strip_users_from_guild_members(G) || G <- lists:reverse(CollectedGuilds)],
    AllGuildStates = StrippedGuilds ++ UnavailableGuilds,

    ReadyDataWithoutGuildIds = maps:remove(<<"guild_ids">>, ReadyDataBotStripped),

    GuildsForReady =
        case IsBot of
            true -> [];
            false -> AllGuildStates
        end,

    logger:debug(
        "[session_ready] dispatching READY for user ~p session ~p",
        [UserId, SessionId]
    ),

    FinalReadyData = maps:merge(ReadyDataWithoutGuildIds, #{
        <<"guilds">> => GuildsForReady,
        <<"sessions">> => CollectedSessions,
        <<"presences">> => CollectedPresences,
        <<"users">> => Users,
        <<"version">> => Version,
        <<"session_id">> => SessionId
    }),

    case SocketPid of
        undefined ->
            {stop, normal, State};
        Pid when is_pid(Pid) ->
            metrics_client:counter(<<"gateway.ready">>),
            StateAfterReady = dispatch_event(ready, FinalReadyData, State),
            SessionCount = length(CollectedSessions),
            GuildCount = length(GuildsForReady),
            PresenceCount = length(CollectedPresences),
            Dimensions = #{
                <<"session_id">> => SessionId,
                <<"user_id">> => integer_to_binary(UserId),
                <<"bot">> => bool_to_binary(IsBot)
            },
            metrics_client:gauge(<<"gateway.sessions.active">>, Dimensions, SessionCount),
            metrics_client:gauge(<<"gateway.guilds.active">>, Dimensions, GuildCount),
            metrics_client:gauge(<<"gateway.presences.active">>, Dimensions, PresenceCount),

            StateAfterGuildCreates =
                case IsBot of
                    true ->
                        lists:foldl(
                            fun(GuildState, AccState) ->
                                dispatch_event(guild_create, GuildState, AccState)
                            end,
                            StateAfterReady,
                            AllGuildStates
                        );
                    false ->
                        StateAfterReady
                end,

            PrivateChannels = get_private_channels(StateAfterGuildCreates),
            spawn(fun() ->
                dispatch_call_creates_for_channels(
                    PrivateChannels, SessionId, StateAfterGuildCreates
                )
            end),

            FinalState = maps:merge(StateAfterGuildCreates, #{
                ready => undefined,
                collected_guild_states => [],
                collected_sessions => []
            }),
            {noreply, FinalState}
    end.

dispatch_event(Event, Data, State) ->
    Seq = maps:get(seq, State),
    SocketPid = maps:get(socket_pid, State, undefined),
    NewSeq = Seq + 1,
    case SocketPid of
        undefined -> ok;
        Pid when is_pid(Pid) -> Pid ! {dispatch, Event, Data, NewSeq}
    end,
    maps:put(seq, NewSeq, State).

update_ready_guilds(GuildState, State) ->
    case maps:get(bot, State, false) of
        true ->
            State;
        false ->
            Ready = maps:get(ready, State),
            case is_map(Ready) of
                true ->
                    Guilds = maps:get(<<"guilds">>, Ready, []),
                    NewGuilds = Guilds ++ [GuildState],
                    NewReady = maps:put(<<"guilds">>, NewGuilds, Ready),
                    maps:put(ready, NewReady, State);
                false ->
                    State
            end
    end.

collect_ready_users(State, CollectedGuilds) ->
    case maps:get(bot, State, false) of
        true ->
            [];
        false ->
            collect_ready_users_nonbot(State, CollectedGuilds)
    end.

collect_ready_users_nonbot(State, CollectedGuilds) ->
    Ready = maps:get(ready, State, #{}),
    Relationships = map_utils:ensure_list(map_utils:get_safe(Ready, <<"relationships">>, [])),
    RelUsers = [
        user_utils:normalize_user(maps:get(<<"user">>, Rel, undefined))
     || Rel <- Relationships
    ],
    Channels = maps:get(channels, State, #{}),
    ChannelUsers = collect_channel_users(maps:values(Channels)),
    GuildUsers = collect_guild_users(CollectedGuilds),
    Users0 = [U || U <- RelUsers ++ ChannelUsers ++ GuildUsers, is_map(U)],
    dedup_users(Users0).

collect_ready_presences(State, _CollectedGuilds) ->
    CurrentUserId = maps:get(user_id, State),
    IsBot = maps:get(bot, State, false),

    {FriendIds, GdmIds} =
        case IsBot of
            true ->
                {[], []};
            false ->
                FIds = presence_targets:friend_ids_from_state(State),
                GdmMap = presence_targets:group_dm_recipients_from_state(State),
                GIds = lists:append([
                    maps:keys(Recipients)
                 || {_Cid, Recipients} <- maps:to_list(GdmMap)
                ]),
                {FIds, GIds}
        end,

    Targets = lists:usort(FriendIds ++ GdmIds) -- [CurrentUserId],
    case Targets of
        [] ->
            [];
        _ ->
            Cached = presence_cache:bulk_get(Targets),
            Visible = [P || P <- Cached, presence_visible(P)],
            dedup_presences(Visible)
    end.

presence_user_id(P) when is_map(P) ->
    User = maps:get(<<"user">>, P, #{}),
    map_utils:get_integer(User, <<"id">>, undefined);
presence_user_id(_) ->
    undefined.

presence_visible(P) ->
    Status = maps:get(<<"status">>, P, <<"offline">>),
    Status =/= <<"offline">> andalso Status =/= <<"invisible">>.

dedup_presences(Presences) ->
    Map =
        lists:foldl(
            fun(P, Acc) ->
                case presence_user_id(P) of
                    undefined -> Acc;
                    Id -> maps:put(Id, P, Acc)
                end
            end,
            #{},
            Presences
        ),
    maps:values(Map).

collect_channel_users(Channels) ->
    lists:foldl(
        fun(Channel, Acc) ->
            Type = maps:get(<<"type">>, Channel, 0),
            case Type =:= 1 orelse Type =:= 3 of
                true ->
                    RecipientsRaw = map_utils:ensure_list(maps:get(<<"recipients">>, Channel, [])),
                    Recipients = [user_utils:normalize_user(R) || R <- RecipientsRaw],
                    Recipients ++ Acc;
                false ->
                    Acc
            end
        end,
        [],
        Channels
    ).

collect_guild_users(GuildStates) ->
    lists:foldl(
        fun(GuildState, Acc) ->
            Members = map_utils:ensure_list(maps:get(<<"members">>, GuildState, [])),
            MemberUsers = [
                user_utils:normalize_user(maps:get(<<"user">>, M, undefined))
             || M <- Members
            ],
            MemberUsers ++ Acc
        end,
        [],
        ensure_list(GuildStates)
    ).

dedup_users(Users) ->
    Map =
        lists:foldl(
            fun(U, Acc) ->
                Id = maps:get(<<"id">>, U, undefined),
                case Id of
                    undefined -> Acc;
                    _ -> maps:put(Id, U, Acc)
                end
            end,
            #{},
            Users
        ),
    maps:values(Map).

ensure_list(List) when is_list(List) -> List;
ensure_list(_) -> [].

strip_users_from_guild_members(GuildState) when is_map(GuildState) ->
    case maps:get(<<"unavailable">>, GuildState, false) of
        true ->
            GuildState;
        false ->
            Members = map_utils:ensure_list(maps:get(<<"members">>, GuildState, [])),
            StrippedMembers = [strip_user_from_member(M) || M <- Members],
            maps:put(<<"members">>, StrippedMembers, GuildState)
    end;
strip_users_from_guild_members(GuildState) ->
    GuildState.

strip_user_from_member(Member) when is_map(Member) ->
    case maps:get(<<"user">>, Member, undefined) of
        undefined ->
            Member;
        User when is_map(User) ->
            UserId = maps:get(<<"id">>, User, undefined),
            maps:put(<<"user">>, #{<<"id">> => UserId}, Member);
        _ ->
            Member
    end;
strip_user_from_member(Member) ->
    Member.

strip_user_from_relationships(ReadyData) when is_map(ReadyData) ->
    Relationships = map_utils:ensure_list(maps:get(<<"relationships">>, ReadyData, [])),
    StrippedRelationships = [strip_user_from_relationship(R) || R <- Relationships],
    maps:put(<<"relationships">>, StrippedRelationships, ReadyData);
strip_user_from_relationships(ReadyData) ->
    ReadyData.

strip_user_from_relationship(Relationship) when is_map(Relationship) ->
    case maps:get(<<"user">>, Relationship, undefined) of
        undefined ->
            Relationship;
        User when is_map(User) ->
            UserId = maps:get(<<"id">>, User, undefined),
            RelWithoutUser = maps:remove(<<"user">>, Relationship),
            case maps:get(<<"id">>, RelWithoutUser, undefined) of
                undefined -> maps:put(<<"id">>, UserId, RelWithoutUser);
                _ -> RelWithoutUser
            end;
        _ ->
            Relationship
    end;
strip_user_from_relationship(Relationship) ->
    Relationship.

get_private_channels(State) ->
    Channels = maps:get(channels, State, #{}),
    maps:filter(
        fun(_ChannelId, Channel) ->
            ChannelType = maps:get(<<"type">>, Channel, 0),
            ChannelType =:= 1 orelse ChannelType =:= 3
        end,
        Channels
    ).

dispatch_call_creates_for_channels(PrivateChannels, SessionId, State) ->
    lists:foreach(
        fun({ChannelId, _Channel}) ->
            dispatch_call_create_for_channel(ChannelId, SessionId, State)
        end,
        maps:to_list(PrivateChannels)
    ).

dispatch_call_create_for_channel(ChannelId, _SessionId, State) ->
    try
        case gen_server:call(call_manager, {lookup, ChannelId}, 5000) of
            {ok, CallPid} ->
                dispatch_call_create_from_pid(CallPid, State);
            _ ->
                ok
        end
    catch
        _:_ -> ok
    end.

dispatch_call_create_from_pid(CallPid, State) ->
    case gen_server:call(CallPid, {get_state}, 5000) of
        {ok, CallData} ->
            CreatedAt = maps:get(created_at, CallData, 0),
            Now = erlang:system_time(millisecond),
            CallAge = Now - CreatedAt,
            case CallAge < 5000 of
                true ->
                    ok;
                false ->
                    ChannelIdBin = maps:get(channel_id, CallData),
                    case validation:validate_snowflake(<<"channel_id">>, ChannelIdBin) of
                        {ok, ChannelId} ->
                            SessionPid = self(),
                            gen_server:cast(SessionPid, {call_monitor, ChannelId, CallPid}),
                            dispatch_event(call_create, CallData, State),
                            SessionId = maps:get(id, State),
                            metrics_client:counter(<<"gateway.calls.total">>, #{
                                <<"channel_id">> => integer_to_binary(ChannelId),
                                <<"session_id">> => SessionId,
                                <<"status">> => <<"create">>
                            });
                        {error, _, Reason} ->
                            logger:warning("[session_ready] Invalid channel_id in call data: ~p", [
                                Reason
                            ]),
                            ok
                    end
            end;
        _ ->
            ok
    end.

-spec bool_to_binary(term()) -> binary().
bool_to_binary(true) -> <<"true">>;
bool_to_binary(false) -> <<"false">>.
