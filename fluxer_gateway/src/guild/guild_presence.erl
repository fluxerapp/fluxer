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

-module(guild_presence).

-export([handle_bus_presence/3, send_cached_presence_to_session/3]).
-export([broadcast_presence_update/3]).

-import(guild_sessions, [handle_user_offline/2]).

-type guild_state() :: map().
-type member() :: map().
-type user_id() :: integer().

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec handle_bus_presence(user_id(), map(), guild_state()) -> {noreply, guild_state()}.
-spec send_cached_presence_to_session(user_id(), binary(), guild_state()) -> guild_state().
handle_bus_presence(UserId, Payload, State) ->
    case maps:get(<<"user_update">>, Payload, false) of
        true ->
            UserData = maps:get(<<"user">>, Payload, #{}),
            UpdatedState = handle_user_data_update(UserId, UserData, State),
            guild_member_list:broadcast_member_list_updates(UserId, State, UpdatedState),
            {noreply, UpdatedState};
        false ->
            Member = find_member_by_user_id(UserId, State),
            case Member of
                undefined ->
                    {noreply, State};
                _ ->
                    StatusBin = maps:get(<<"status">>, Payload, <<"offline">>),
                    NormalizedStatusBin = normalize_presence_status(StatusBin),
                    Status = constants:status_type_atom(NormalizedStatusBin),
                    Mobile = maps:get(<<"mobile">>, Payload, false),
                    Afk = maps:get(<<"afk">>, Payload, false),
                    logger:debug("[guild_presence] Presence update for UserId=~p, Status=~p", [UserId, Status]),
                    MemberUser = maps:get(<<"user">>, Member, #{}),
                    CustomStatus = maps:get(<<"custom_status">>, Payload, null),
                    PresenceMap = presence_payload:build(
                        MemberUser,
                        NormalizedStatusBin,
                        Mobile,
                        Afk,
                        CustomStatus
                    ),
                    Presences = maps:get(presences, State, #{}),
                    UpdatedPresences = maps:put(UserId, PresenceMap, Presences),
                    StateWithPresences = maps:put(presences, UpdatedPresences, State),
                    broadcast_presence_update(UserId, PresenceMap, StateWithPresences),
                    logger:debug("[guild_presence] Broadcasting member list updates for UserId=~p", [UserId]),
                    guild_member_list:broadcast_member_list_updates(UserId, State, StateWithPresences),
                    StateAfterOffline =
                        case Status of
                            offline ->
                                handle_user_offline(UserId, StateWithPresences);
                            _ ->
                                StateWithPresences
                        end,
                    {noreply, StateAfterOffline}
            end
    end.

-spec broadcast_presence_update(user_id(), map(), guild_state()) -> ok.
broadcast_presence_update(UserId, Payload, State) ->
    case find_member_by_user_id(UserId, State) of
        undefined ->
            ok;
            _Member ->
            GuildId = map_utils:get_integer(State, id, 0),
            PresenceUpdate = maps:put(<<"guild_id">>, integer_to_binary(GuildId), Payload),
            Sessions = maps:get(sessions, State, #{}),
            MemberSubs = maps:get(member_subscriptions, State, guild_subscriptions:init_state()),
            SubscribedSessionIds = guild_subscriptions:get_subscribed_sessions(UserId, MemberSubs),
            TargetChannels = guild_visibility:viewable_channel_set(UserId, State),
            {ValidSessionIds, InvalidSessionIds} =
                partition_subscribed_sessions(SubscribedSessionIds, Sessions, TargetChannels, UserId, State),
            StateAfterInvalidRemovals =
                lists:foldl(
                    fun(SessionId, AccState) ->
                        remove_session_member_subscription(SessionId, UserId, AccState)
                    end,
                    State,
                    sets:to_list(sets:from_list(InvalidSessionIds))
                ),
            FinalSessions = maps:get(sessions, StateAfterInvalidRemovals, #{}),
            ValidSessionSet = sets:from_list(ValidSessionIds),
            SessionsToNotify = lists:filter(
                fun({SessionId, _}) -> sets:is_element(SessionId, ValidSessionSet) end,
                maps:to_list(FinalSessions)
            ),
            lists:foreach(
                fun({_SessionId, SessionData}) ->
                    SessionPid = maps:get(pid, SessionData),
                    case is_pid(SessionPid) of
                        true ->
                            gen_server:cast(
                                SessionPid, {dispatch, presence_update, PresenceUpdate}
                            );
                        false ->
                            ok
                    end
                end,
                SessionsToNotify
            ),
            ok
    end.

normalize_presence_status(<<"invisible">>) -> <<"offline">>;
normalize_presence_status(Status) when is_binary(Status) -> Status;
normalize_presence_status(_) -> <<"offline">>.

send_cached_presence_to_session(UserId, SessionId, State) ->
    case presence_cache:get(UserId) of
        {ok, Payload} ->
            send_presence_payload_to_session(UserId, SessionId, Payload, State);
        _ ->
            State
    end.

send_presence_payload_to_session(UserId, SessionId, Payload, State) ->
    GuildId = map_utils:get_integer(State, id, 0),
    Sessions = maps:get(sessions, State, #{}),
    case maps:get(SessionId, Sessions, undefined) of
        #{pid := SessionPid} when is_pid(SessionPid) ->
            Member = find_member_by_user_id(UserId, State),
            case Member of
                undefined ->
                    State;
                _ ->
                    StatusBin = maps:get(<<"status">>, Payload, <<"offline">>),
                    Mobile = maps:get(<<"mobile">>, Payload, false),
                    Afk = maps:get(<<"afk">>, Payload, false),
                    MemberUser = maps:get(<<"user">>, Member, #{}),
                    CustomStatus = maps:get(<<"custom_status">>, Payload, null),
                    PresenceBase =
                        presence_payload:build(MemberUser, StatusBin, Mobile, Afk, CustomStatus),
                    PresenceUpdate = maps:put(<<"guild_id">>, integer_to_binary(GuildId), PresenceBase),
                    gen_server:cast(SessionPid, {dispatch, presence_update, PresenceUpdate}),
                    State
            end;
        _ ->
            State
    end.

-spec handle_user_data_update(user_id(), map(), guild_state()) -> guild_state().
handle_user_data_update(UserId, UserData, State) ->
    Data = guild_data(State),
    Members = guild_members(State),
    case find_member_by_user_id(UserId, State) of
        undefined ->
            State;
        Member ->
            CurrentUserData = maps:get(<<"user">>, Member, #{}),
            case check_user_data_differs(CurrentUserData, UserData) of
                false ->
                    State;
                true ->
                    UpdatedMembers = lists:map(
                        fun(M) ->
                            maybe_replace_member(M, UserId, UserData)
                        end,
                        Members
                    ),
                    UpdatedData = maps:put(<<"members">>, UpdatedMembers, Data),
                    UpdatedState = maps:put(data, UpdatedData, State),
                    maybe_dispatch_member_update(UserId, UpdatedState),
                    UpdatedState
            end
    end.

-spec maybe_replace_member(member(), user_id(), map()) -> member().
maybe_replace_member(Member, UserId, UserData) ->
    case member_id(Member) of
        UserId ->
            maps:put(<<"user">>, UserData, Member);
        _ ->
            Member
    end.

-spec maybe_dispatch_member_update(user_id(), guild_state()) -> ok.
maybe_dispatch_member_update(UserId, State) ->
    case find_member_by_user_id(UserId, State) of
        undefined ->
            ok;
        Member ->
            GuildId = map_utils:get_integer(State, id, 0),
            MemberUpdate = maps:put(<<"guild_id">>, integer_to_binary(GuildId), Member),
            gen_server:cast(
                self(), {dispatch, #{event => guild_member_update, data => MemberUpdate}}
            )
    end.

-spec guild_data(guild_state()) -> map().
guild_data(State) ->
    map_utils:ensure_map(map_utils:get_safe(State, data, #{})).

-spec guild_members(guild_state()) -> [map()].
guild_members(State) ->
    map_utils:ensure_list(maps:get(<<"members">>, guild_data(State), [])).

-spec member_id(map()) -> user_id() | undefined.
member_id(Member) ->
    User = map_utils:ensure_map(maps:get(<<"user">>, Member, #{})),
    map_utils:get_integer(User, <<"id">>, undefined).

partition_subscribed_sessions(SessionIds, Sessions, TargetChannels, TargetUserId, State) ->
    lists:foldl(
        fun(SessionId, {Valids, Invalids}) ->
            case maps:get(SessionId, Sessions, undefined) of
                undefined ->
                    {Valids, [SessionId | Invalids]};
                SessionData ->
                    SessionUserId = maps:get(user_id, SessionData, undefined),
                    Shared =
                        case SessionUserId of
                            undefined ->
                                false;
                            UserId when UserId =:= TargetUserId ->
                                false;
                            _ ->
                                SessionChannels = guild_visibility:viewable_channel_set(SessionUserId, State),
                                not sets:is_empty(sets:intersection(SessionChannels, TargetChannels))
                        end,
                    case Shared of
                        true -> {[SessionId | Valids], Invalids};
                        false -> {Valids, [SessionId | Invalids]}
                    end
            end
        end,
        {[], []},
        SessionIds
    ).

remove_session_member_subscription(SessionId, UserId, State) ->
    MemberSubs = maps:get(member_subscriptions, State, guild_subscriptions:init_state()),
    NewMemberSubs = guild_subscriptions:unsubscribe(SessionId, UserId, MemberSubs),
    State1 = maps:put(member_subscriptions, NewMemberSubs, State),
    guild_sessions:unsubscribe_from_user_presence(UserId, State1).

-ifdef(TEST).

handle_bus_presence_non_member_noop_test() ->
    Payload = #{<<"status">> => <<"online">>, <<"user">> => #{<<"id">> => <<"99">>}},
    State = #{data => #{<<"members">> => []}, sessions => #{}},
    {noreply, NewState} = handle_bus_presence(99, Payload, State),
    ?assertEqual(State, NewState).

handle_bus_presence_broadcasts_test() ->
    State = presence_test_state(),
    Payload = #{
        <<"status">> => <<"online">>,
        <<"mobile">> => true,
        <<"afk">> => false,
        <<"user">> => #{<<"id">> => <<"1">>, <<"username">> => <<"Alpha">>}
    },
    {noreply, _NewState} = handle_bus_presence(1, Payload, State),
    ok.

handle_bus_presence_user_update_test() ->
    State = presence_test_state(),
    UserData = #{<<"id">> => <<"1">>, <<"username">> => <<"Updated">>},
    Payload = #{<<"user">> => UserData, <<"user_update">> => true},
    {noreply, NewState} = handle_bus_presence(1, Payload, State),
    Data = maps:get(data, NewState),
    [Member | _] = maps:get(<<"members">>, Data),
    ?assertEqual(<<"Updated">>, maps:get(<<"username">>, maps:get(<<"user">>, Member))).

presence_test_state() ->
    #{
        id => 42,
        data => #{
            <<"members">> => [
                #{<<"user">> => #{<<"id">> => <<"1">>, <<"username">> => <<"Alpha">>}}
            ]
        },
        sessions => #{}
    }.

-endif.

check_user_data_differs(CurrentUserData, NewUserData) ->
    utils:check_user_data_differs(CurrentUserData, NewUserData).

find_member_by_user_id(UserId, State) ->
    guild_permissions:find_member_by_user_id(UserId, State).
