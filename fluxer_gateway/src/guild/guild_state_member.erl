%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_state_member).
-typing([eqwalizer]).

-export([
    handle_member_add/2,
    handle_member_update/2,
    handle_member_remove/2,
    sync_member/2,
    sync_member_remove/2,
    cleanup_removed_member_sessions/1,
    cleanup_removed_member_sessions/2,
    maybe_disconnect_removed_member/2,
    extract_user_id/1,
    lookup_presence/2
]).

-type guild_state() :: #{member_presence => ets:table() | map(), term() => term()}.
-type guild_data() :: map().

-type user_id() :: integer().

-spec handle_member_add(map(), map()) -> map().
handle_member_add(EventData, Data) ->
    guild_data_index_members:put_member(EventData, Data).

-spec handle_member_update(map(), map()) -> map().
handle_member_update(EventData, Data) ->
    UserId = extract_user_id(EventData),
    Members = guild_data_index_members:member_map(Data),
    case maps:is_key(UserId, Members) of
        true ->
            guild_data_index_members:put_member(EventData, Data);
        false ->
            Data
    end.

-spec handle_member_remove(map(), map()) -> map().
handle_member_remove(EventData, Data) ->
    UserId = extract_user_id(EventData),
    guild_data_index_members:remove_member(UserId, Data).

-spec sync_member(map(), #{member_presence => ets:table() | map(), term() => term()}) -> ok.
sync_member(EventData, State) ->
    UserId = extract_user_id(EventData),
    case is_integer(UserId) andalso UserId > 0 of
        true -> sync_valid_member(UserId, State);
        false -> ok
    end.

-spec sync_valid_member(user_id(), guild_state()) -> ok.
sync_valid_member(UserId, State) ->
    Data = maps:get(data, State, #{}),
    case guild_data_index_members:get_member(UserId, Data) of
        undefined -> ok;
        Member -> sync_local_nif_member(UserId, Member, State)
    end.

-spec sync_local_nif_member(user_id(), map(), guild_state()) -> ok.
sync_local_nif_member(UserId, Member, State) ->
    ok = sync_default_member_list_member(UserId, Member, State),
    guild_member_list_channel_engine:update_user_all(UserId, State).

-spec sync_default_member_list_member(user_id(), map(), guild_state()) -> ok.
sync_default_member_list_member(UserId, Member, State) ->
    case member_list_engine_ref(State) of
        undefined ->
            ok;
        Ref ->
            DisplayName = guild_member_list_common:get_member_display_name(Member),
            SortKey = guild_member_list_common:casefold_binary(DisplayName),
            RoleIds = guild_member_list_store:extract_role_ids(Member),
            ConnectedUserIds = guild_member_list_common:connected_session_user_ids(State),
            Presence = lookup_presence(maps:get(member_presence, State), UserId),
            Status = maps:get(<<"status">>, Presence, <<"offline">>),
            IsConnected = sets:is_element(UserId, ConnectedUserIds),
            NotOffline = Status =/= <<"offline">> andalso Status =/= <<"invisible">>,
            IsOnline = IsConnected andalso NotOffline,
            guild_member_list_engine:update_member(Ref, UserId, SortKey, RoleIds, IsOnline)
    end.

-spec sync_member_remove(integer() | undefined, #{
    member_presence => ets:table() | map(), term() => term()
}) -> ok.
sync_member_remove(UserId, State) ->
    case is_integer(UserId) andalso UserId > 0 of
        false ->
            ok;
        true ->
            sync_local_nif_member_remove(UserId, State)
    end.

-spec sync_local_nif_member_remove(user_id(), guild_state()) -> ok.
sync_local_nif_member_remove(UserId, State) ->
    ok = sync_default_member_list_remove(UserId, State),
    guild_member_list_channel_engine:remove_user_all(UserId, State).

-spec sync_default_member_list_remove(user_id(), guild_state()) -> ok.
sync_default_member_list_remove(UserId, State) ->
    case member_list_engine_ref(State) of
        undefined -> ok;
        Ref -> guild_member_list_engine:remove_member(Ref, UserId)
    end.

-spec cleanup_removed_member_sessions(#{
    member_presence => ets:table() | map(), term() => term()
}) -> #{member_presence => ets:table() | map(), term() => term()}.
cleanup_removed_member_sessions(State) ->
    Data = state_data(State),
    MemberUserIds = sets:from_list(guild_data_index_members:member_ids(Data)),
    Sessions = state_sessions(State),
    FilteredSessions = maps:filter(
        fun(_K, S) ->
            UserId = maps:get(user_id, S),
            sets:is_element(UserId, MemberUserIds)
        end,
        Sessions
    ),
    drop_removed_session_refs(Sessions, FilteredSessions, State#{sessions => FilteredSessions}).

-spec cleanup_removed_member_sessions(integer() | undefined, #{
    member_presence => ets:table() | map(), term() => term()
}) -> #{member_presence => ets:table() | map(), term() => term()}.
cleanup_removed_member_sessions(UserId, State) when is_integer(UserId), UserId > 0 ->
    Sessions = state_sessions(State),
    FilteredSessions = maps:filter(
        fun(_SessionId, SessionData) ->
            maps:get(user_id, SessionData, undefined) =/= UserId
        end,
        Sessions
    ),
    drop_removed_session_refs(Sessions, FilteredSessions, State#{sessions => FilteredSessions});
cleanup_removed_member_sessions(_UserId, State) ->
    cleanup_removed_member_sessions(State).

-spec drop_removed_session_refs(map(), map(), guild_state()) -> guild_state().
drop_removed_session_refs(Sessions, FilteredSessions, State) ->
    maps:fold(
        fun
            (SessionId, _Session, Acc) when is_map_key(SessionId, FilteredSessions) ->
                Acc;
            (_SessionId, Session, Acc) ->
                guild_sessions_connect:remove_session_ref(
                    maps:get(mref, Session, undefined), Acc
                )
        end,
        State,
        Sessions
    ).

-spec maybe_disconnect_removed_member(integer() | undefined, #{
    member_presence => ets:table() | map(), term() => term()
}) -> #{member_presence => ets:table() | map(), term() => term()}.
maybe_disconnect_removed_member(UserId, State) when is_integer(UserId), UserId > 0 ->
    ok = guild_voice_lifecycle:cast_disconnect_voice_user(UserId, State),
    State;
maybe_disconnect_removed_member(_, State) ->
    State.

-spec extract_user_id(map()) -> integer() | undefined.
extract_user_id(EventData) ->
    MUser = maps:get(<<"user">>, EventData, #{}),
    snowflake_id:parse_optional(maps:get(<<"id">>, MUser, undefined)).

-spec lookup_presence(ets:table() | map(), integer()) -> map().
lookup_presence(Map, UserId) when is_map(Map) ->
    maps:get(UserId, Map, guild_member_list_common:default_presence());
lookup_presence(Tab, UserId) ->
    case ets:lookup(Tab, UserId) of
        [{_, P}] -> P;
        [] -> guild_member_list_common:default_presence()
    end.

-spec member_list_engine_ref(guild_state()) -> ets:table() | undefined.
member_list_engine_ref(State) ->
    case maps:get(member_list_engine, State, undefined) of
        Ref when is_reference(Ref); is_atom(Ref) -> Ref;
        _ -> undefined
    end.

-spec state_data(guild_state()) -> guild_data().
state_data(State) ->
    case maps:get(data, State, #{}) of
        Data when is_map(Data) -> Data;
        _ -> #{}
    end.

-spec state_sessions(guild_state()) -> map().
state_sessions(State) ->
    case maps:get(sessions, State, #{}) of
        Sessions when is_map(Sessions) -> Sessions;
        _ -> #{}
    end.
