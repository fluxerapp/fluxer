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

-module(push).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([
    handle_message_create/1,
    sync_user_guild_settings/3,
    sync_user_blocked_ids/2,
    invalidate_user_badge_count/1
]).
-export([get_cache_stats/0]).
-export_type([state/0]).

-import(push_eligibility, [is_eligible_for_push/8]).
-import(push_cache, [
    update_lru/2,
    get_user_push_subscriptions/2,
    cache_user_subscriptions/3,
    invalidate_user_badge_count/2
]).
-import(push_sender, [send_push_notifications/8]).
-import(push_logger_filter, [install_progress_filter/0]).

-type state() :: #{
    user_guild_settings_cache := map(),
    user_guild_settings_lru := list(),
    user_guild_settings_size := non_neg_integer(),
    user_guild_settings_max_mb := non_neg_integer() | undefined,
    push_subscriptions_cache := map(),
    push_subscriptions_lru := list(),
    push_subscriptions_size := non_neg_integer(),
    push_subscriptions_max_mb := non_neg_integer() | undefined,
    blocked_ids_cache := map(),
    blocked_ids_lru := list(),
    blocked_ids_size := non_neg_integer(),
    blocked_ids_max_mb := non_neg_integer() | undefined,
    badge_counts_cache := map(),
    badge_counts_lru := list(),
    badge_counts_size := non_neg_integer(),
    badge_counts_max_mb := non_neg_integer() | undefined,
    badge_counts_ttl_seconds := non_neg_integer()
}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    install_progress_filter(),
    PushEnabled = fluxer_gateway_env:get(push_enabled),
    BaseState = #{
        user_guild_settings_cache => #{},
        user_guild_settings_lru => [],
        user_guild_settings_size => 0,
        user_guild_settings_max_mb => undefined,
        push_subscriptions_cache => #{},
        push_subscriptions_lru => [],
        push_subscriptions_size => 0,
        push_subscriptions_max_mb => undefined,
        blocked_ids_cache => #{},
        blocked_ids_lru => [],
        blocked_ids_size => 0,
        blocked_ids_max_mb => undefined,
        badge_counts_cache => #{},
        badge_counts_lru => [],
        badge_counts_size => 0,
        badge_counts_max_mb => undefined,
        badge_counts_ttl_seconds => 0
    },
    case PushEnabled of
        true ->
            UgsMaxMb = fluxer_gateway_env:get(push_user_guild_settings_cache_mb),
            PsMaxMb = fluxer_gateway_env:get(push_subscriptions_cache_mb),
            BiMaxMb = fluxer_gateway_env:get(push_blocked_ids_cache_mb),
            BcMaxMb = fluxer_gateway_env:get(push_badge_counts_cache_mb),
            BcTtl = fluxer_gateway_env:get(push_badge_counts_cache_ttl_seconds),
            {ok, BaseState#{
                user_guild_settings_max_mb := UgsMaxMb,
                push_subscriptions_max_mb := PsMaxMb,
                blocked_ids_max_mb := BiMaxMb,
                badge_counts_max_mb := BcMaxMb,
                badge_counts_ttl_seconds := BcTtl
            }};
        false ->
            {ok, BaseState}
    end.


handle_call(get_cache_stats, _From, State) ->
    #{
        user_guild_settings_cache := UgsCache,
        push_subscriptions_cache := PsCache,
        blocked_ids_cache := BiCache,
        badge_counts_cache := BcCache
    } = State,
    Stats = #{
        user_guild_settings_size => maps:size(UgsCache),
        push_subscriptions_size => maps:size(PsCache),
        blocked_ids_size => maps:size(BiCache),
        badge_counts_size => maps:size(BcCache)
    },
    {reply, {ok, Stats}, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({handle_message_create, Params}, State) ->
    ParamMap = case Params of Maps when is_map(Maps) -> Maps; _ -> #{} end,
    GuildId = maps:get(<<"guild_id">>, ParamMap, undefined),
    ChannelId = maps:get(<<"channel_id">>, ParamMap, undefined),
    MessageId = maps:get(<<"id">>, ParamMap, undefined),
    {noreply, do_handle_message_create(Params, State)};
handle_cast({sync_user_guild_settings, UserId, GuildId, UserGuildSettings}, State) ->
    #{
        user_guild_settings_cache := UgsCache,
        user_guild_settings_lru := UgsLru
    } = State,
    Key = {settings, UserId, GuildId},
    NewCache = maps:put(Key, UserGuildSettings, UgsCache),
    NewLru = update_lru(Key, UgsLru),
    {noreply, State#{
        user_guild_settings_cache := NewCache,
        user_guild_settings_lru := NewLru
    }};
handle_cast({sync_user_blocked_ids, UserId, BlockedIds}, State) ->
    #{
        blocked_ids_cache := BiCache,
        blocked_ids_lru := BiLru
    } = State,
    Key = {blocked, UserId},
    NewCache = maps:put(Key, BlockedIds, BiCache),
    NewLru = update_lru(Key, BiLru),
    {noreply, State#{
        blocked_ids_cache := NewCache,
        blocked_ids_lru := NewLru
    }};
handle_cast({cache_user_guild_settings, UserId, GuildId, Settings}, State) ->
    #{
        user_guild_settings_cache := UgsCache,
        user_guild_settings_lru := UgsLru
    } = State,
    Key = {settings, UserId, GuildId},
    NewCache = maps:put(Key, Settings, UgsCache),
    NewLru = update_lru(Key, UgsLru),
    {noreply, State#{
        user_guild_settings_cache := NewCache,
        user_guild_settings_lru := NewLru
    }};
handle_cast({invalidate_user_badge_count, UserId}, State) ->
    {noreply, invalidate_user_badge_count(UserId, State)};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, {state, UgsCache, UgsLru, UgsSize, UgsMaxMb, PsCache, PsLru, PsSize, PsMaxMb,
                      BiCache, BiLru, BiSize, BiMaxMb, BcCache, BcLru, BcSize, BcMaxMb, BcTtl}, _Extra) ->
    {ok, #{
        user_guild_settings_cache => UgsCache,
        user_guild_settings_lru => UgsLru,
        user_guild_settings_size => UgsSize,
        user_guild_settings_max_mb => UgsMaxMb,
        push_subscriptions_cache => PsCache,
        push_subscriptions_lru => PsLru,
        push_subscriptions_size => PsSize,
        push_subscriptions_max_mb => PsMaxMb,
        blocked_ids_cache => BiCache,
        blocked_ids_lru => BiLru,
        blocked_ids_size => BiSize,
        blocked_ids_max_mb => BiMaxMb,
        badge_counts_cache => BcCache,
        badge_counts_lru => BcLru,
        badge_counts_size => BcSize,
        badge_counts_max_mb => BcMaxMb,
        badge_counts_ttl_seconds => BcTtl
    }};
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_message_create(Params) ->
    PushEnabled = fluxer_gateway_env:get(push_enabled),

    case PushEnabled of
        true -> gen_server:cast(?MODULE, {handle_message_create, Params});
        false ->
            ok
    end.

sync_user_guild_settings(UserId, GuildId, UserGuildSettings) ->
    gen_server:cast(?MODULE, {sync_user_guild_settings, UserId, GuildId, UserGuildSettings}).

sync_user_blocked_ids(UserId, BlockedIds) ->
    gen_server:cast(?MODULE, {sync_user_blocked_ids, UserId, BlockedIds}).

invalidate_user_badge_count(UserId) ->
    gen_server:cast(?MODULE, {invalidate_user_badge_count, UserId}).

get_cache_stats() ->
    gen_server:call(?MODULE, get_cache_stats, 5000).

do_handle_message_create(Params, State) ->
    MessageData = maps:get(message_data, Params),
    UserIds = maps:get(user_ids, Params),
    GuildId = maps:get(guild_id, Params),
    AuthorId = maps:get(author_id, Params),
    UserRolesMap = maps:get(user_roles, Params, #{}),
    ChannelId = binary_to_integer(maps:get(<<"channel_id">>, MessageData)),
    MessageId = binary_to_integer(maps:get(<<"id">>, MessageData)),
    GuildDefaultNotifications = maps:get(guild_default_notifications, Params, 0),
    GuildName = maps:get(guild_name, Params, undefined),
    ChannelName = maps:get(channel_name, Params, undefined),
    logger:debug(
        "[push] Processing message ~p in channel ~p, guild ~p for users ~p (author ~p, defaults ~p)",
        [MessageId, ChannelId, GuildId, UserIds, AuthorId, GuildDefaultNotifications]
    ),
    EligibleUsers = lists:filter(
        fun(UserId) ->
            Eligible = is_eligible_for_push(
                UserId,
                AuthorId,
                GuildId,
                ChannelId,
                MessageData,
                GuildDefaultNotifications,
                UserRolesMap,
                State
            ),
            logger:debug("[push] User ~p eligible: ~p", [UserId, Eligible]),
            Eligible
        end,
        UserIds
    ),
    logger:debug("[push] Eligible users: ~p", [EligibleUsers]),
    case EligibleUsers of
        [] ->
            State;
        _ ->
            send_push_notifications(
                EligibleUsers,
                MessageData,
                GuildId,
                ChannelId,
                MessageId,
                GuildName,
                ChannelName,
                State
            )
    end.
