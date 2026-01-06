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

-module(presence).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(PresenceData) ->
    gen_server:start_link(?MODULE, PresenceData, []).

init(PresenceData) ->
    process_flag(trap_exit, true),
    UserId = maps:get(user_id, PresenceData),
    UserData = maps:get(user_data, PresenceData),
    Status = maps:get(status, PresenceData),
    IsBot0 = maps:get(<<"bot">>, UserData, false),
    IsBot =
        case IsBot0 of
            true -> true;
            _ -> false
        end,
    GuildIds0 = maps:get(guild_ids, PresenceData, []),
    FriendIds0 = maps:get(friend_ids, PresenceData, []),
    GroupDmRecipients0 = maps:get(group_dm_recipients, PresenceData, #{}),
    CustomStatus = maps:get(custom_status, PresenceData, null),
    FriendIds =
        case IsBot of
            true -> [];
            false -> FriendIds0
        end,
    GroupDmRecipients = normalize_group_dm_recipients(GroupDmRecipients0, UserId, IsBot),

    State = #{
        user_id => UserId,
        user_data => UserData,
        sessions => #{},
        custom_status => CustomStatus,
        status => Status,
        guild_ids => map_from_ids(GuildIds0),
        temporary_guild_ids => #{},
        friends => map_from_ids(FriendIds),
        group_dm_recipients => GroupDmRecipients,
        subscriptions => #{},
        is_bot => IsBot,
        initial_presences_sent => false,
        last_published_presence => undefined
    },

    StateWithSubs = ensure_initial_global_subscriptions(State),
    PresencePid = self(),
    spawn(fun() -> fetch_initial_presences(PresencePid, StateWithSubs) end),

    {ok, StateWithSubs}.

handle_call({session_connect, Request}, {Pid, _}, State) ->
    Result = presence_session:handle_session_connect(Request, Pid, State),
    publish_global_if_needed(Result);
handle_call({terminate_session, SessionIdHashes}, _From, State) ->
    Sessions = maps:get(sessions, State),
    SessionPids = [maps:get(pid, S) || S <- maps:values(Sessions)],
    lists:foreach(
        fun(Pid) when is_pid(Pid) ->
            gen_server:cast(Pid, {terminate, SessionIdHashes})
        end,
        SessionPids
    ),
    {reply, ok, State};
handle_call({dispatch, EventAtom, Data}, _From, State) ->
    Sessions = maps:get(sessions, State),
    UserId = maps:get(user_id, State),

    SessionPids = [maps:get(pid, S) || S <- maps:values(Sessions)],

    lists:foreach(
        fun(Pid) when is_pid(Pid) ->
            case erlang:is_process_alive(Pid) of
                true ->
                    gen_server:cast(Pid, {dispatch, EventAtom, Data});
                false ->
                    ok
            end
        end,
        SessionPids
    ),

    case EventAtom of
        user_update ->
            CurrentUserData = maps:get(user_data, State, #{}),
            case utils:check_user_data_differs(CurrentUserData, Data) of
                true ->
                    publish_user_update_to_bus(UserId, Data, State),
                    NewState = maps:put(user_data, Data, State),
                    {reply, ok, NewState};
                false ->
                    {reply, ok, State}
            end;
        user_settings_update ->
            NewState = handle_user_settings_update(Data, State),
            FinalState = force_publish_global_presence(NewState),
            {reply, ok, FinalState};
        message_create ->
            HasMobile = lists:any(
                fun(Session) ->
                    maps:get(mobile, Session, false)
                end,
                maps:values(Sessions)
            ),
            AllAfk = lists:all(
                fun(Session) ->
                    maps:get(afk, Session, false)
                end,
                maps:values(Sessions)
            ),
            ShouldSendPush =
                (map_size(Sessions) =:= 0) orelse ((not HasMobile) andalso AllAfk),
            case ShouldSendPush of
                true ->
                    AuthorIdBin = maps:get(<<"id">>, maps:get(<<"author">>, Data, #{}), <<"0">>),
                    AuthorId = validation:snowflake_or_default(AuthorIdBin, 0),
                    push:handle_message_create(#{
                        message_data => Data,
                        user_ids => [UserId],
                        guild_id => 0,
                        author_id => AuthorId
                    });
                false ->
                    ok
            end,
            {reply, ok, State};
        _ ->
            {reply, ok, State}
    end;
handle_call({join_guild, GuildId}, _From, State) ->
    handle_join_guild(GuildId, State);
handle_call({leave_guild, GuildId}, _From, State) ->
    handle_leave_guild(GuildId, State);
handle_call({add_temporary_guild, GuildId}, _From, State) ->
    {reply, JoinReply, JoinedState} = handle_join_guild(GuildId, State),
    TemporaryGuildIds = maps:get(temporary_guild_ids, JoinedState, #{}),
    NewTemporaryGuildIds = maps:put(GuildId, true, TemporaryGuildIds),
    NewState = maps:put(temporary_guild_ids, NewTemporaryGuildIds, JoinedState),
    {reply, JoinReply, NewState};
handle_call({remove_temporary_guild, GuildId}, _From, State) ->
    {reply, LeaveReply, LeftState} = handle_leave_guild(GuildId, State),
    TemporaryGuildIds = maps:get(temporary_guild_ids, LeftState, #{}),
    NewTemporaryGuildIds = maps:remove(GuildId, TemporaryGuildIds),
    NewState = maps:put(temporary_guild_ids, NewTemporaryGuildIds, LeftState),
    {reply, LeaveReply, NewState};
handle_call({terminate, SessionIdHashes}, _From, State) ->
    Sessions = maps:get(sessions, State),
    SessionPids = [maps:get(pid, S) || S <- maps:values(Sessions)],
    lists:foreach(
        fun(Pid) when is_pid(Pid) ->
            gen_server:cast(Pid, {terminate, SessionIdHashes})
        end,
        SessionPids
    ),
    {stop, normal, ok, State};
handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast({dispatch, Event, Data}, State) ->
    Sessions = maps:get(sessions, State),
    UserId = maps:get(user_id, State),

    SessionPids = [maps:get(pid, S) || S <- maps:values(Sessions)],
    lists:foreach(
        fun(Pid) when is_pid(Pid) ->
            gen_server:cast(Pid, {dispatch, Event, Data})
        end,
        SessionPids
    ),

    case Event of
        user_update ->
            CurrentUserData = maps:get(user_data, State, #{}),
            case utils:check_user_data_differs(CurrentUserData, Data) of
                true ->
                    publish_user_update_to_bus(UserId, Data, State),
                    NewState = maps:put(user_data, Data, State),
                    {noreply, NewState};
                false ->
                    {noreply, State}
            end;
        user_settings_update ->
            NewState = handle_user_settings_update(Data, State),
            FinalState = force_publish_global_presence(NewState),
            {noreply, FinalState};
        message_create ->
            HasMobile = lists:any(
                fun(Session) ->
                    maps:get(mobile, Session, false)
                end,
                maps:values(Sessions)
            ),
            AllAfk = lists:all(
                fun(Session) ->
                    maps:get(afk, Session, false)
                end,
                maps:values(Sessions)
            ),
            ShouldSendPush =
                (map_size(Sessions) =:= 0) orelse ((not HasMobile) andalso AllAfk),
            case ShouldSendPush of
                true ->
                    AuthorIdBin = maps:get(<<"id">>, maps:get(<<"author">>, Data, #{}), <<"0">>),
                    AuthorId = validation:snowflake_or_default(AuthorIdBin, 0),
                    push:handle_message_create(#{
                        message_data => Data,
                        user_ids => [UserId],
                        guild_id => 0,
                        author_id => AuthorId
                    });
                false ->
                    ok
            end,
            {noreply, State};
        _ ->
            {noreply, State}
    end;
handle_cast({presence_update, Request}, State) ->
    {UpdatedRequest, StateWithCustomStatus} = maybe_handle_custom_status(Request, State),
    Result = presence_session:handle_presence_update(UpdatedRequest, StateWithCustomStatus),
    publish_global_if_needed(Result);
handle_cast({terminate_session, SessionIdHashes}, State) ->
    Sessions = maps:get(sessions, State),
    SessionPids = [maps:get(pid, S) || S <- maps:values(Sessions)],
    lists:foreach(
        fun(Pid) when is_pid(Pid) ->
            gen_server:cast(Pid, {terminate, SessionIdHashes})
        end,
        SessionPids
    ),
    {noreply, State};
handle_cast({terminate_all_sessions}, State) ->
    Sessions = maps:get(sessions, State),
    SessionPids = [maps:get(pid, S) || S <- maps:values(Sessions)],
    lists:foreach(
        fun(Pid) when is_pid(Pid) ->
            gen_server:cast(Pid, {terminate_force})
        end,
        SessionPids
    ),
    {noreply, State};
handle_cast({sync_friends, FriendIds}, State) ->
    NewState = sync_friend_subscriptions(FriendIds, State),
    {noreply, NewState};
handle_cast({sync_group_dm_recipients, RecipientsByChannel}, State) ->
    NewState = sync_group_dm_subscriptions(RecipientsByChannel, State),
    {noreply, NewState};
handle_cast(_, State) ->
    {noreply, State}.

handle_info({presence, TargetId, Payload}, State) ->
    dispatch_global_presence(TargetId, Payload, State);
handle_info({initial_presences, Presences}, State) ->
    Sessions = maps:get(sessions, State),
    SessionPids = [maps:get(pid, S) || S <- maps:values(Sessions)],
    lists:foreach(
        fun(Pid) when is_pid(Pid) ->
            gen_server:cast(Pid, {initial_global_presences, Presences})
        end,
        SessionPids
    ),
    {noreply, State};
handle_info({'DOWN', Ref, process, _Pid, Reason}, State) ->
    handle_process_down(Ref, Reason, State);
handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, State) when not is_map(State) ->
    ok;
terminate(_Reason, State) ->
    UserId = maps:get(user_id, State),
    presence_cache:delete(UserId),
    publish_offline_on_terminate(UserId, State),
    kick_temporary_members_on_terminate(UserId, State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

kick_temporary_members_on_terminate(UserId, State) ->
    TemporaryGuildIds = maps:get(temporary_guild_ids, State, #{}),
    case map_size(TemporaryGuildIds) of
        0 ->
            ok;
        _ ->
            GuildIdsList = maps:keys(TemporaryGuildIds),
            spawn(fun() ->
                Request = #{
                    <<"type">> => <<"kick_temporary_member">>,
                    <<"user_id">> => type_conv:to_binary(UserId),
                    <<"guild_ids">> => [type_conv:to_binary(Gid) || Gid <- GuildIdsList]
                },
                case rpc_client:call(Request) of
                    {ok, _} ->
                        ok;
                    {error, Reason} ->
                        logger:warning(
                            "[presence] Failed to kick temporary member ~p from guilds ~p: ~p",
                            [UserId, GuildIdsList, Reason]
                        )
                end
            end)
    end.

publish_offline_on_terminate(UserId, State) ->
    LastPublished = maps:get(last_published_presence, State, undefined),
    WasVisible =
        case LastPublished of
            undefined ->
                false;
            #{status := Status} when
                Status =:= <<"online">>;
                Status =:= <<"idle">>;
                Status =:= <<"dnd">>
            ->
                true;
            _ ->
                false
        end,
    case WasVisible of
        true ->
            UserData = user_utils:normalize_user(maps:get(user_data, State, #{})),
            Payload = #{
                <<"user">> => UserData,
                <<"status">> => <<"offline">>,
                <<"mobile">> => false,
                <<"afk">> => false,
                <<"custom_status">> => null
            },
            presence_bus:publish(UserId, Payload);
        false ->
            ok
    end.

handle_process_down(Ref, _Reason, State) ->
    Sessions = maps:get(sessions, State),

    case presence_session:find_session_by_ref(Ref, Sessions) of
        {ok, SessionId} ->
            NewSessions = maps:remove(SessionId, Sessions),
            NewState0 = maps:put(sessions, NewSessions, State),
            NewState = publish_global_presence(NewSessions, NewState0),
            if
                map_size(NewSessions) =:= 0 ->
                    {stop, normal, NewState};
                true ->
                    presence_session:dispatch_sessions_replace(NewState),
                    {noreply, NewState}
            end;
        not_found ->
            {noreply, State}
    end.

ensure_initial_global_subscriptions(State) ->
    case maps:get(is_bot, State, false) of
        true ->
            State;
        false ->
            FriendIds = maps:keys(maps:get(friends, State, #{})),
            GroupDm = maps:get(group_dm_recipients, State, #{}),
            State1 =
                lists:foldl(
                    fun(FriendId, Acc) ->
                        ensure_subscription(FriendId, friend, undefined, Acc)
                    end,
                    State,
                    FriendIds
                ),
            lists:foldl(
                fun({ChannelId, Recipients}, AccState) ->
                    RecipientIds = maps:keys(Recipients),
                    lists:foldl(
                        fun(RId, A) -> ensure_subscription(RId, gdm, ChannelId, A) end,
                        AccState,
                        RecipientIds
                    )
                end,
                State1,
                maps:to_list(GroupDm)
            )
    end.

publish_global_if_needed({reply, Reply, NewState}) ->
    FinalState = publish_global_presence(maps:get(sessions, NewState), NewState),
    {reply, Reply, FinalState};
publish_global_if_needed({noreply, NewState}) ->
    FinalState = publish_global_presence(maps:get(sessions, NewState), NewState),
    {noreply, FinalState}.

publish_global_presence(_Sessions, State) ->
    {Payload, CurrentExternal, ExternalStatus} = build_presence_external(State),
    LastPublished = maps:get(last_published_presence, State, undefined),

    case presence_changed(LastPublished, CurrentExternal) of
        true ->
            publish_presence_payload(State, Payload, CurrentExternal, ExternalStatus);
        false ->
            State
    end.

force_publish_global_presence(State) ->
    {Payload, CurrentExternal, ExternalStatus} = build_presence_external(State),
    publish_presence_payload(State, Payload, CurrentExternal, ExternalStatus).

build_presence_external(State) ->
    Payload = build_presence_payload(State),
    ExternalStatus = maps:get(<<"status">>, Payload, <<"offline">>),
    Mobile = maps:get(<<"mobile">>, Payload, false),
    Afk = maps:get(<<"afk">>, Payload, false),
    CustomStatus = maps:get(<<"custom_status">>, Payload, null),
    CurrentExternal = #{
        status => ExternalStatus,
        mobile => Mobile,
        afk => Afk,
        custom_status => CustomStatus
    },
    {Payload, CurrentExternal, ExternalStatus}.

publish_presence_payload(State, Payload, CurrentExternal, ExternalStatus) ->
    UserId = maps:get(user_id, State),
    case ExternalStatus of
        <<"offline">> ->
            presence_cache:delete(UserId);
        _ ->
            presence_cache:put(UserId, Payload)
    end,
    presence_bus:publish(UserId, Payload),
    maps:put(last_published_presence, CurrentExternal, State).

presence_changed(undefined, _Current) ->
    true;
presence_changed(Last, Current) ->
    Last =/= Current.

publish_user_update_to_bus(UserId, UserData, State) ->
    LastPublished = maps:get(last_published_presence, State, undefined),
    WasVisible = is_last_published_visible(LastPublished),
    case WasVisible of
        true ->
            NormalizedUserData = user_utils:normalize_user(UserData),
            Payload = #{
                <<"user">> => NormalizedUserData,
                <<"user_update">> => true
            },
            presence_bus:publish(UserId, Payload);
        false ->
            ok
    end.

is_last_published_visible(undefined) ->
    false;
is_last_published_visible(#{status := Status}) when
    Status =:= <<"online">>;
    Status =:= <<"idle">>;
    Status =:= <<"dnd">>
->
    true;
is_last_published_visible(_) ->
    false.

dispatch_global_presence(TargetId, Payload, State) ->
    UserId = maps:get(user_id, State),
    case TargetId =:= UserId of
        true ->
            {noreply, State};
        false ->
            cache_if_visible(TargetId, Payload),
            Sessions = maps:get(sessions, State),
            SessionPids = [maps:get(pid, S) || S <- maps:values(Sessions)],
            lists:foreach(
                fun(Pid) when is_pid(Pid) ->
                    gen_server:cast(Pid, {dispatch, presence_update, Payload})
                end,
                SessionPids
            ),
            {noreply, State}
    end.

sync_friend_subscriptions(FriendIds, State) ->
    case maps:get(is_bot, State, false) of
        true ->
            State;
        false ->
            ExistingFriends = maps:get(friends, State, #{}),
            ExistingIds = maps:keys(ExistingFriends),
            Additions = lists:subtract(FriendIds, ExistingIds),
            Removals = lists:subtract(ExistingIds, FriendIds),
            State1 =
                lists:foldl(
                    fun(FId, Acc) ->
                        ensure_subscription(FId, friend, undefined, Acc)
                    end,
                    State,
                    Additions
                ),
            State2 =
                lists:foldl(
                    fun(FId, Acc) ->
                        remove_subscription_reason(FId, friend, undefined, Acc)
                    end,
                    State1,
                    Removals
                ),
            State3 = maps:put(friends, map_from_ids(FriendIds), State2),
            State4 = maybe_send_cached_presences(Additions, State3),
            maybe_force_offline(Removals, State4)
    end.

sync_group_dm_subscriptions(RecipientsByChannel, State) ->
    case maps:get(is_bot, State, false) of
        true ->
            State;
        false ->
            Current = maps:get(group_dm_recipients, State, #{}),
            Normalized = normalize_group_dm_recipients(
                RecipientsByChannel, maps:get(user_id, State), false
            ),
            {ToAdd, ToRemove} = diff_group_dm_recipients(Current, Normalized),
            State1 =
                lists:foldl(
                    fun({UserId, ChannelId}, Acc) ->
                        ensure_subscription(UserId, gdm, ChannelId, Acc)
                    end,
                    State,
                    ToAdd
                ),
            State2 =
                lists:foldl(
                    fun({UserId, ChannelId}, Acc) ->
                        remove_subscription_reason(UserId, gdm, ChannelId, Acc)
                    end,
                    State1,
                    ToRemove
                ),
            AddedUsers = lists:usort([UserId || {UserId, _} <- ToAdd]),
            State3 = maybe_send_cached_presences(AddedUsers, State2),
            RemovedUsers = lists:usort([UserId || {UserId, _} <- ToRemove]),
            State4 = maybe_force_offline(RemovedUsers, State3),
            maps:put(group_dm_recipients, Normalized, State4)
    end.

diff_group_dm_recipients(Old, New) ->
    OldPairs =
        lists:append(
            [
                [{UserId, ChannelId} || UserId <- maps:keys(Recipients)]
             || {ChannelId, Recipients} <- maps:to_list(Old)
            ]
        ),
    NewPairs =
        lists:append(
            [
                [{UserId, ChannelId} || UserId <- maps:keys(Recipients)]
             || {ChannelId, Recipients} <- maps:to_list(New)
            ]
        ),
    {
        lists:subtract(NewPairs, OldPairs),
        lists:subtract(OldPairs, NewPairs)
    }.

ensure_subscription(UserId, Reason, ChannelId, State) ->
    case UserId =:= maps:get(user_id, State) of
        true ->
            State;
        false ->
            Subscriptions = maps:get(subscriptions, State, #{}),
            Entry0 = maps:get(UserId, Subscriptions, #{friend => false, gdm_channels => #{}}),
            Entry1 =
                case Reason of
                    friend ->
                        Entry0#{friend => true};
                    gdm ->
                        Channels = maps:get(gdm_channels, Entry0, #{}),
                        Entry0#{gdm_channels => maps:put(ChannelId, true, Channels)}
                end,
            WasEmpty = not has_subscription(Entry0),
            NewSubscriptions = maps:put(UserId, Entry1, Subscriptions),
            case WasEmpty andalso has_subscription(Entry1) of
                true -> presence_bus:subscribe(UserId);
                false -> ok
            end,
            maps:put(subscriptions, NewSubscriptions, State)
    end.

remove_subscription_reason(UserId, Reason, ChannelId, State) ->
    Subscriptions = maps:get(subscriptions, State, #{}),
    Entry0 = maps:get(UserId, Subscriptions, #{friend => false, gdm_channels => #{}}),
    Entry1 =
        case Reason of
            friend ->
                Entry0#{friend => false};
            gdm ->
                Channels = maps:get(gdm_channels, Entry0, #{}),
                Entry0#{gdm_channels => maps:remove(ChannelId, Channels)}
        end,
    ShouldRemove = not has_subscription(Entry1),
    NewSubscriptions =
        case ShouldRemove of
            true -> maps:remove(UserId, Subscriptions);
            false -> maps:put(UserId, Entry1, Subscriptions)
        end,
    case ShouldRemove of
        true -> presence_bus:unsubscribe(UserId);
        false -> ok
    end,
    maps:put(subscriptions, NewSubscriptions, State).

has_subscription(Entry) ->
    (maps:get(friend, Entry, false) =:= true) orelse
        (map_size(maps:get(gdm_channels, Entry, #{})) > 0).

normalize_group_dm_recipients(RecipientsByChannel, UserId, IsBot) ->
    case IsBot of
        true ->
            #{};
        false ->
            maps:from_list(
                [
                    {ChannelId,
                        map_from_ids([
                            Rid
                         || Rid <- recipient_list(RecipientIds), Rid =/= UserId
                        ])}
                 || {ChannelId, RecipientIds} <- maps:to_list(RecipientsByChannel)
                ]
            )
    end.

handle_join_guild(GuildId, State) ->
    Guilds = maps:get(guild_ids, State, #{}),
    case maps:is_key(GuildId, Guilds) of
        true ->
            {reply, ok, State};
        false ->
            NewGuilds = maps:put(GuildId, true, Guilds),
            NewState = maps:put(guild_ids, NewGuilds, State),
            presence_session:notify_sessions_guild_join(GuildId, NewState),
            {reply, ok, NewState}
    end.

handle_leave_guild(GuildId, State) ->
    Guilds = maps:get(guild_ids, State, #{}),
    case maps:is_key(GuildId, Guilds) of
        false ->
            {reply, ok, State};
        true ->
            NewGuilds = maps:remove(GuildId, Guilds),
            TemporaryGuildIds = maps:get(temporary_guild_ids, State, #{}),
            NewTemporaryGuildIds = maps:remove(GuildId, TemporaryGuildIds),
            State1 = maps:put(guild_ids, NewGuilds, State),
            NewState = maps:put(temporary_guild_ids, NewTemporaryGuildIds, State1),
            presence_session:notify_sessions_guild_leave(GuildId, NewState),
            {reply, ok, NewState}
    end.

map_from_ids(Ids) when is_list(Ids) ->
    maps:from_list([{Id, true} || Id <- Ids]).

cache_if_visible(UserId, Payload) when is_integer(UserId), is_map(Payload) ->
    Status = maps:get(<<"status">>, Payload, <<"offline">>),
    case Status of
        <<"offline">> -> ok;
        <<"invisible">> -> ok;
        _ -> presence_cache:put(UserId, Payload)
    end;
cache_if_visible(_, _) ->
    ok.

build_presence_payload(State) ->
    Sessions = maps:get(sessions, State),
    Status = presence_status:get_current_status(Sessions),
    Mobile = presence_status:get_flattened_mobile(Sessions),
    Afk = presence_status:get_flattened_afk(Sessions),
    UserData = maps:get(user_data, State, #{}),
    CustomStatus = maps:get(custom_status, State, null),
    presence_payload:build(UserData, Status, Mobile, Afk, CustomStatus).

maybe_handle_custom_status(Request, State) ->
    case maps:find(<<"custom_status">>, Request) of
        error ->
            {Request, State};
        {ok, null} ->
            {maps:put(<<"custom_status">>, null, Request), maps:put(custom_status, null, State)};
        {ok, CustomStatus} when is_map(CustomStatus) ->
            PreviousCustomStatus = maps:get(custom_status, State, null),
            case
                custom_status_comparator(PreviousCustomStatus) =:=
                    custom_status_comparator(CustomStatus)
            of
                true ->
                    {maps:put(<<"custom_status">>, PreviousCustomStatus, Request), State};
                false ->
                    validate_custom_status(CustomStatus, Request, State)
            end;
        _ ->
            {Request, State}
    end.

validate_custom_status(CustomStatus, Request, State) ->
    UserId = maps:get(user_id, State),
    case custom_status_validation:validate(UserId, CustomStatus) of
        {ok, #{<<"custom_status">> := Validated}} ->
            UpdatedRequest = maps:put(<<"custom_status">>, Validated, Request),
            {UpdatedRequest, maps:put(custom_status, Validated, State)};
        {ok, _} ->
            UpdatedRequest = maps:put(<<"custom_status">>, null, Request),
            {UpdatedRequest, maps:put(custom_status, null, State)};
        {error, Reason} ->
            logger:warning(
                "[presence] Custom status validation failed for user ~p: ~p",
                [UserId, Reason]
            ),
            {Request, State}
    end.

custom_status_comparator(null) ->
    null;
custom_status_comparator(Map) when is_map(Map) ->
    #{
        <<"text">> => field_or_null(Map, <<"text">>),
        <<"expires_at">> => field_or_null(Map, <<"expires_at">>),
        <<"emoji_id">> => field_or_null(Map, <<"emoji_id">>),
        <<"emoji_name">> => field_or_null(Map, <<"emoji_name">>)
    }.

handle_user_settings_update(Data, State) ->
    case maps:find(<<"custom_status">>, Data) of
        error ->
            State;
        {ok, CustomStatus} ->
            Normalized = normalize_state_custom_status(CustomStatus),
            maps:put(custom_status, Normalized, State)
    end.

normalize_state_custom_status(null) ->
    null;
normalize_state_custom_status(Map) when is_map(Map) ->
    Map;
normalize_state_custom_status(_) ->
    null.

field_or_null(Map, Key) ->
    case maps:get(Key, Map, undefined) of
        undefined -> null;
        Value -> Value
    end.

maybe_send_cached_presences(UserIds, State) ->
    case UserIds of
        [] ->
            State;
        _ ->
            lists:foreach(
                fun(Uid) ->
                    case presence_cache:get(Uid) of
                        {ok, Presence} ->
                            notify_sessions_presence(Presence, State);
                        _ ->
                            ok
                    end
                end,
                UserIds
            ),
            State
    end.

maybe_force_offline(UserIds, State) ->
    Subscriptions = maps:get(subscriptions, State, #{}),
    lists:foldl(
        fun(Uid, Acc) ->
            case maps:is_key(Uid, Subscriptions) of
                true ->
                    Acc;
                false ->
                    presence_cache:delete(Uid),
                    Offline = #{
                        <<"user">> => #{<<"id">> => integer_to_binary(Uid)},
                        <<"status">> => <<"offline">>,
                        <<"mobile">> => false,
                        <<"afk">> => false,
                        <<"custom_status">> => null
                    },
                    notify_sessions_presence(Offline, Acc)
            end
        end,
        State,
        UserIds
    ).

notify_sessions_presence(Payload, State) ->
    Sessions = maps:get(sessions, State, #{}),
    SessionPids = [maps:get(pid, S) || S <- maps:values(Sessions)],
    lists:foreach(
        fun(Pid) when is_pid(Pid) ->
            gen_server:cast(Pid, {dispatch, presence_update, Payload})
        end,
        SessionPids
    ),
    State.

fetch_initial_presences(PresencePid, State) ->
    case maps:get(is_bot, State, false) of
        true ->
            ok;
        false ->
            FriendIds = maps:keys(maps:get(friends, State, #{})),
            GdmIds =
                lists:append([
                    maps:keys(Recipients)
                 || {_, Recipients} <- maps:to_list(
                        maps:get(group_dm_recipients, State, #{})
                    )
                ]),
            Targets = lists:usort(FriendIds ++ GdmIds),
            case Targets of
                [] ->
                    ok;
                _ ->
                    Presences = presence_cache:bulk_get(Targets),
                    Visible = [
                        P
                     || P <- Presences, maps:get(<<"status">>, P, <<"offline">>) =/= <<"offline">>
                    ],
                    case Visible of
                        [] ->
                            ok;
                        _ ->
                            PresencePid ! {initial_presences, Visible}
                    end
            end
    end.

recipient_list(Value) when is_list(Value) ->
    Value;
recipient_list(Value) when is_map(Value) ->
    maps:keys(Value);
recipient_list(_) ->
    [].

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

gdm_subscription_add_remove_test() ->
    maybe_start_presence_bus(),
    maybe_start_presence_cache(),
    BaseState = #{
        user_id => 1,
        is_bot => false,
        sessions => #{},
        user_data => #{},
        subscriptions => #{},
        friends => #{},
        group_dm_recipients => #{}
    },
    State1 = sync_group_dm_subscriptions(#{1 => [10]}, BaseState),
    Subscriptions1 = maps:get(subscriptions, State1),
    Entry1 = maps:get(10, Subscriptions1),
    GdmChannels1 = maps:get(gdm_channels, Entry1, #{}),
    ?assertEqual(true, maps:get(1, GdmChannels1)),

    State2 = sync_group_dm_subscriptions(#{}, State1),
    Subscriptions2 = maps:get(subscriptions, State2, #{}),
    ?assertEqual(false, maps:is_key(10, Subscriptions2)),
    ok.

maybe_start_presence_bus() ->
    case whereis(presence_bus) of
        undefined ->
            case presence_bus:start_link() of
                {ok, _Pid} -> ok;
                {error, {already_started, _Pid}} -> ok;
                Other -> Other
            end;
        _ ->
            ok
    end.

maybe_start_presence_cache() ->
    case whereis(presence_cache) of
        undefined ->
            case presence_cache:start_link() of
                {ok, _Pid} -> ok;
                {error, {already_started, _Pid}} -> ok;
                Other -> Other
            end;
        _ ->
            ok
    end.
-endif.
