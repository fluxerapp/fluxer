%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_projection).
-feature(maybe_expr, enable).
-typing([eqwalizer]).

-export([from_state/1]).

-define(VOICE_MEMBERS_TABLE_WARNED, {guild, voice_members_table_unavailable}).

-spec from_state(map()) -> map().
from_state(State) ->
    maybe
        #{members_ets := Tab} = Data ?= maps:get(data, State, #{}),
        true ?= voice_members_table_healthy(Tab),
        ok = clear_voice_members_table_warning(),
        project_voice_guild_state(Data, State)
    else
        _ -> voice_members_table_unavailable(State)
    end.

-spec voice_members_table_healthy(term()) -> boolean().
voice_members_table_healthy(Tab) when is_reference(Tab) ->
    try ets:info(eqwalizer:dynamic_cast(Tab), owner) of
        Owner when is_pid(Owner) -> Owner =:= self();
        _ -> false
    catch
        error:badarg -> false
    end;
voice_members_table_healthy(_Tab) ->
    false.

-spec project_voice_guild_state(map(), map()) -> map().
project_voice_guild_state(Data, State) ->
    Projected = maps:with(voice_guild_state_keys(), State),
    project_voice_sessions(Projected#{data => maps:with(voice_guild_data_keys(), Data)}).

-spec project_voice_sessions(map()) -> map().
project_voice_sessions(#{sessions := Sessions} = Projected) when is_map(Sessions) ->
    Projected#{sessions => maps:map(fun project_voice_session/2, Sessions)};
project_voice_sessions(Projected) ->
    Projected.

-spec project_voice_session(term(), term()) -> term().
project_voice_session(_SessionId, Session) when is_map(Session) ->
    maps:without(voice_session_drop_keys(), Session);
project_voice_session(_SessionId, Session) ->
    Session.

-spec voice_session_drop_keys() -> [atom()].
voice_session_drop_keys() ->
    [active_guilds, user_roles, mref, bot, is_staff].

-spec voice_guild_state_keys() -> [atom() | binary()].
voice_guild_state_keys() ->
    [
        id,
        <<"id">>,
        sessions,
        guild_pid,
        voice_states,
        pending_voice_connections,
        recently_disconnected_voice_states,
        e2ee_room_keys,
        virtual_channel_access,
        virtual_channel_access_pending,
        virtual_channel_access_preserve,
        virtual_channel_access_move_pending,
        virtual_channel_access_view_only,
        test_perm_fun,
        test_force_disconnect_fun,
        test_livekit_fun,
        test_permission_sync_fun
    ].

-spec voice_guild_data_keys() -> [atom() | binary()].
voice_guild_data_keys() ->
    [
        <<"id">>,
        <<"guild">>,
        <<"roles">>,
        <<"role_index">>,
        role_perms_cache,
        <<"channels">>,
        <<"channel_index">>,
        overwrite_perms_cache,
        members_ets,
        thread_gate,
        thread_store
    ].

-spec voice_members_table_unavailable(map()) -> map().
voice_members_table_unavailable(State) ->
    ok = log_voice_members_table_unavailable(maps:get(id, State, undefined)),
    State.

-spec log_voice_members_table_unavailable(term()) -> ok.
log_voice_members_table_unavailable(GuildId) ->
    case erlang:put(?VOICE_MEMBERS_TABLE_WARNED, true) of
        true ->
            ok;
        _ ->
            logger:warning("guild_voice_members_table_unavailable: guild_id=~p", [GuildId])
    end.

-spec clear_voice_members_table_warning() -> ok.
clear_voice_members_table_warning() ->
    _ = erlang:erase(?VOICE_MEMBERS_TABLE_WARNED),
    ok.
