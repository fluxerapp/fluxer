%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_connection).
-typing([eqwalizer]).

-export([
    handle_presence_connect/2,
    repair_presence_connection/1,
    handle_guild_connect/3,
    handle_guild_connect_result/4,
    handle_guild_connect_timeout/4,
    handle_call_reconnect/3
]).

-export_type([
    session_state/0,
    guild_id/0,
    channel_id/0,
    attempt/0,
    guild_connect_result/0,
    session_result/0
]).

-type session_state() :: session:session_state().
-type guild_id() :: session:guild_id().
-type channel_id() :: session:channel_id().
-type attempt() :: non_neg_integer().
-type guild_connect_result() ::
    {ok, pid(), map()}
    | {ok_unavailable, pid(), map()}
    | {ok_cached_unavailable, map()}
    | {error, term()}.
-type session_result() :: {noreply, session_state()} | {stop, normal, session_state()}.

-spec handle_presence_connect(attempt(), session_state()) ->
    session_result().
handle_presence_connect(Attempt, State) ->
    session_connection_presence:handle_presence_connect(Attempt, State).

-spec repair_presence_connection(session_state()) -> session_state().
repair_presence_connection(State) ->
    session_connection_presence:repair_presence_connection(State).

-spec handle_guild_connect(guild_id(), attempt(), session_state()) ->
    session_result().
handle_guild_connect(GuildId, Attempt, State) ->
    session_connection_guild:handle_guild_connect(GuildId, Attempt, State).

-spec handle_guild_connect_result(
    guild_id(), attempt(), guild_connect_result(), session_state()
) ->
    session_result().
handle_guild_connect_result(GuildId, Attempt, Result, State) ->
    session_connection_guild:handle_guild_connect_result(GuildId, Attempt, Result, State).

-spec handle_guild_connect_timeout(guild_id(), attempt(), reference(), session_state()) ->
    session_result().
handle_guild_connect_timeout(GuildId, Attempt, Token, State) ->
    session_connection_guild:handle_guild_connect_timeout(GuildId, Attempt, Token, State).

-spec handle_call_reconnect(channel_id(), attempt(), session_state()) ->
    {noreply, session_state()}.
handle_call_reconnect(ChannelId, Attempt, State) ->
    session_connection_retry:handle_call_reconnect(ChannelId, Attempt, State).
