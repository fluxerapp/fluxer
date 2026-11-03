%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_connection_unavailability).
-typing([eqwalizer]).

-export([
    maybe_handle_cached_unavailability/5,
    mark_cached_guild_unavailable_and_retry/3,
    maybe_build_unavailable_response_from_cache/2,
    build_initial_active_guilds/2
]).

-export_type([session_state/0, guild_id/0, attempt/0, session_result/0]).

-define(MAX_GUILD_UNAVAILABLE_RETRY_DELAY_MS, 30000).
-define(MAX_GUILD_UNAVAILABLE_BACKOFF_ATTEMPT, 5).
-define(GUILD_UNAVAILABLE_JITTER_DIVISOR, 5).

-type session_state() :: session:session_state().
-type guild_id() :: session:guild_id().
-type attempt() :: non_neg_integer().
-type session_result() :: {noreply, session_state()} | {stop, normal, session_state()}.

-spec maybe_handle_cached_unavailability(
    guild_id(), attempt(), binary(), integer(), session_state()
) ->
    session_result().
maybe_handle_cached_unavailability(GuildId, Attempt, SessionId, UserId, State) ->
    UserData = maps:get(user_data, State, #{}),
    case guild_availability:is_guild_unavailable_for_user_from_cache(GuildId, UserData) of
        true ->
            log_cached_unavailable(GuildId, UserId, Attempt),
            mark_cached_guild_unavailable_and_retry(GuildId, Attempt, State);
        false ->
            Guilds = maps:get(guilds, State, #{}),
            ResetState = State#{guilds => Guilds#{GuildId => undefined}},
            session_connection_guild:maybe_spawn_guild_connect(
                GuildId, 0, SessionId, UserId, ResetState
            )
    end.

-spec log_cached_unavailable(guild_id(), integer(), attempt()) -> ok.
log_cached_unavailable(GuildId, UserId, 0) ->
    logger:info("Guild connect cached unavailable", #{guild_id => GuildId, user_id => UserId});
log_cached_unavailable(_GuildId, _UserId, _Attempt) ->
    ok.

-spec mark_cached_guild_unavailable(guild_id(), session_state()) -> session_result().
mark_cached_guild_unavailable(GuildId, State) ->
    Guilds = maps:get(guilds, State, #{}),
    case maps:get(GuildId, Guilds, undefined) of
        cached_unavailable ->
            {noreply, State};
        _ ->
            UpdatedGuilds = Guilds#{GuildId => cached_unavailable},
            StateWithGuild = State#{guilds => UpdatedGuilds},
            UnavailableHidden =
                guild_availability:is_unavailable_hidden_enabled_from_cache(GuildId),
            {noreply, MarkedState} =
                session_ready:mark_guild_unavailable(
                    GuildId, UnavailableHidden, StateWithGuild
                ),
            session_ready:check_readiness(MarkedState)
    end.

-spec mark_cached_guild_unavailable_and_retry(guild_id(), attempt(), session_state()) ->
    session_result().
mark_cached_guild_unavailable_and_retry(GuildId, Attempt, State) ->
    case mark_cached_guild_unavailable(GuildId, State) of
        {noreply, MarkedState} ->
            schedule_cached_unavailable_retry(GuildId, Attempt, MarkedState);
        {stop, normal, _MarkedState} = Stop ->
            Stop
    end.

-spec schedule_cached_unavailable_retry(
    guild_id(), attempt(), session_state()
) -> {noreply, session_state()}.
schedule_cached_unavailable_retry(GuildId, Attempt, State) ->
    SessionId = maps:get(id, State, <<>>),
    DelayMs = cached_unavailable_retry_delay_ms(GuildId, SessionId, Attempt),
    erlang:send_after(DelayMs, self(), {guild_connect, GuildId, Attempt + 1}),
    {noreply, State}.

-spec cached_unavailable_retry_delay_ms(guild_id(), binary(), attempt()) -> non_neg_integer().
cached_unavailable_retry_delay_ms(GuildId, SessionId, Attempt) ->
    CappedAttempt = min(Attempt, ?MAX_GUILD_UNAVAILABLE_BACKOFF_ATTEMPT),
    BaseDelay = backoff_utils:calculate(CappedAttempt, ?MAX_GUILD_UNAVAILABLE_RETRY_DELAY_MS),
    case BaseDelay >= ?MAX_GUILD_UNAVAILABLE_RETRY_DELAY_MS of
        true ->
            ?MAX_GUILD_UNAVAILABLE_RETRY_DELAY_MS;
        false ->
            MaxJitter = max(1, BaseDelay div ?GUILD_UNAVAILABLE_JITTER_DIVISOR),
            Jitter = erlang:phash2({GuildId, SessionId, Attempt}, MaxJitter + 1),
            min(?MAX_GUILD_UNAVAILABLE_RETRY_DELAY_MS, BaseDelay + Jitter)
    end.

-spec maybe_build_unavailable_response_from_cache(
    guild_id(), map()
) -> {ok, map()} | not_unavailable.
maybe_build_unavailable_response_from_cache(GuildId, UserData) ->
    case guild_availability:is_guild_unavailable_for_user_from_cache(GuildId, UserData) of
        true -> {ok, build_unavailable_response(GuildId)};
        false -> not_unavailable
    end.

-spec build_unavailable_response(guild_id()) -> map().
build_unavailable_response(GuildId) ->
    Base = #{<<"id">> => integer_to_binary(GuildId), <<"unavailable">> => true},
    case guild_availability:is_unavailable_hidden_enabled_from_cache(GuildId) of
        true -> Base#{<<"unavailable_hidden">> => true};
        false -> Base
    end.

-spec build_initial_active_guilds(guild_id() | undefined, guild_id()) -> sets:set(guild_id()).
build_initial_active_guilds(undefined, _GuildId) -> sets:new();
build_initial_active_guilds(GuildId, GuildId) -> sets:from_list([GuildId]);
build_initial_active_guilds(_, _) -> sets:new().
