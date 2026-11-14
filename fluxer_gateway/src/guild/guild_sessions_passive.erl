%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_sessions_passive).
-typing([eqwalizer]).

-export([
    set_session_active_guild/3,
    set_session_passive_guild/3,
    handle_set_typing_override/3,
    handle_send_guild_sync/2
]).

-type guild_state() :: map().
-type session_id() :: binary().
-type guild_id() :: integer().
-export_type([guild_state/0, session_id/0, guild_id/0]).

-spec set_session_active_guild(session_id(), guild_id(), guild_state()) -> guild_state().
set_session_active_guild(SessionId, GuildId, State) ->
    case snowflake_id:parse_optional(GuildId) of
        undefined ->
            State;
        ValidGuildId ->
            set_active_for_session(SessionId, ValidGuildId, State)
    end.

-spec set_active_for_session(session_id(), guild_id(), guild_state()) -> guild_state().
set_active_for_session(SessionId, ValidGuildId, State) ->
    update_session(SessionId, State, fun(SD) ->
        session_passive:set_active(ValidGuildId, SD)
    end).

-spec set_session_passive_guild(session_id(), guild_id(), guild_state()) -> guild_state().
set_session_passive_guild(SessionId, GuildId, State) ->
    case snowflake_id:parse_optional(GuildId) of
        undefined ->
            State;
        ValidGuildId ->
            set_passive_for_session(SessionId, ValidGuildId, State)
    end.

-spec set_passive_for_session(session_id(), guild_id(), guild_state()) -> guild_state().
set_passive_for_session(SessionId, ValidGuildId, State) ->
    update_session(SessionId, State, fun(SD) ->
        NewSD = session_passive:set_passive(ValidGuildId, SD),
        guild_thread_subscriptions:clear_on_passive(
            session_passive:clear_guild_synced(ValidGuildId, NewSD)
        )
    end).

-spec handle_set_typing_override(session_id(), boolean(), guild_state()) -> guild_state().
handle_set_typing_override(SessionId, TypingFlag, State) ->
    case guild_session(SessionId, State) of
        {GuildId, Sessions, SessionData} ->
            NewSD = session_passive:set_typing_override(GuildId, TypingFlag, SessionData),
            State#{sessions => Sessions#{SessionId => NewSD}};
        undefined ->
            State
    end.

-spec handle_send_guild_sync(session_id(), guild_state()) -> guild_state().
handle_send_guild_sync(SessionId, State) ->
    case guild_session(SessionId, State) of
        {GuildId, Sessions, SessionData} ->
            maybe_dispatch_guild_sync(SessionId, SessionData, GuildId, Sessions, State);
        undefined ->
            State
    end.

-spec maybe_dispatch_guild_sync(
    session_id(), map(), guild_id(), map(), guild_state()
) -> guild_state().
maybe_dispatch_guild_sync(SessionId, SessionData, GuildId, Sessions, State) ->
    case session_passive:is_guild_synced(GuildId, SessionData) of
        true ->
            State;
        false ->
            dispatch_guild_sync(SessionId, SessionData, GuildId, Sessions, State)
    end.

-spec update_session(session_id(), guild_state(), fun((map()) -> map())) -> guild_state().
update_session(SessionId, State, Fun) ->
    Sessions = maps:get(sessions, State, #{}),
    case maps:get(SessionId, Sessions, undefined) of
        undefined -> State;
        SessionData -> State#{sessions => Sessions#{SessionId => Fun(SessionData)}}
    end.

-spec guild_session(session_id(), guild_state()) -> {guild_id(), map(), map()} | undefined.
guild_session(SessionId, State) ->
    case guild_id(State) of
        undefined ->
            undefined;
        GuildId ->
            find_session_in_guild(SessionId, GuildId, State)
    end.

-spec find_session_in_guild(session_id(), guild_id(), guild_state()) ->
    {guild_id(), map(), map()} | undefined.
find_session_in_guild(SessionId, GuildId, State) ->
    Sessions = maps:get(sessions, State, #{}),
    case maps:get(SessionId, Sessions, undefined) of
        undefined -> undefined;
        SessionData -> {GuildId, Sessions, SessionData}
    end.

-spec guild_id(guild_state()) -> guild_id() | undefined.
guild_id(State) ->
    snowflake_id:parse_optional(maps:get(id, State, undefined)).

-spec dispatch_guild_sync(session_id(), map(), guild_id(), map(), guild_state()) ->
    guild_state().
dispatch_guild_sync(SessionId, SessionData, GuildId, Sessions, State) ->
    case {session_user_id(SessionData), maps:get(pid, SessionData, undefined)} of
        {UserId, SessionPid} when is_integer(UserId), UserId > 0, is_pid(SessionPid) ->
            GuildData = guild_data:get_guild_state(
                UserId, State, guild_thread_gate:state_opts(SessionData)
            ),
            Encoded =
                {pre_encoded,
                    iolist_to_binary(
                        json:encode(guild_data_wire:payload(GuildData), fun json:encode_value/2)
                    )},
            gateway_dispatch_relay:dispatch(SessionPid, guild_sync, Encoded, GuildId),
            NewSD = session_passive:mark_guild_synced(GuildId, SessionData),
            State#{sessions => Sessions#{SessionId => NewSD}};
        _ ->
            State
    end.

-spec session_user_id(map()) -> integer() | undefined.
session_user_id(SessionData) ->
    snowflake_id:parse_optional(maps:get(user_id, SessionData, undefined)).
