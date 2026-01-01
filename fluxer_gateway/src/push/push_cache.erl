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

-module(push_cache).

-export([update_lru/2]).
-export([get_user_push_subscriptions/2]).
-export([cache_user_subscriptions/3]).
-export([get_user_badge_count/2]).
-export([cache_user_badge_count/4]).
-export([estimate_subscriptions_size/1]).
-export([evict_if_needed/4]).
-export([invalidate_user_badge_count/2]).

update_lru(Key, Lru) ->
    NewLru = lists:delete(Key, Lru),
    [Key | NewLru].

get_user_push_subscriptions(UserId, State) ->
    Key = {subscriptions, UserId},
    PushSubscriptionsCache = maps:get(push_subscriptions_cache, State, #{}),
    case maps:get(Key, PushSubscriptionsCache, undefined) of
        undefined ->
            [];
        Subs ->
            Subs
    end.

cache_user_subscriptions(UserId, Subscriptions, State) ->
    Key = {subscriptions, UserId},

    NewSubsSize = estimate_subscriptions_size(Subscriptions),
    OldSubsSize =
        case maps:get(Key, maps:get(push_subscriptions_cache, State, #{}), undefined) of
            undefined -> 0;
            OldSubs -> estimate_subscriptions_size(OldSubs)
        end,
    SizeDelta = NewSubsSize - OldSubsSize,

    PushSubscriptionsLru = maps:get(push_subscriptions_lru, State, []),
    NewLru = update_lru(Key, PushSubscriptionsLru),

    PushSubscriptionsCache = maps:get(push_subscriptions_cache, State, #{}),
    NewCache = maps:put(Key, Subscriptions, PushSubscriptionsCache),
    PushSubscriptionsSize = maps:get(push_subscriptions_size, State, 0),
    NewSize = PushSubscriptionsSize + SizeDelta,

    MaxBytes =
        case maps:get(push_subscriptions_max_mb, State, undefined) of
            undefined -> NewSize;
            Mb -> Mb * 1024 * 1024
        end,
    {FinalCache, FinalLru, FinalSize} = evict_if_needed(
        NewCache, NewLru, NewSize, MaxBytes
    ),

    State#{
        push_subscriptions_cache => FinalCache,
        push_subscriptions_lru => FinalLru,
        push_subscriptions_size => FinalSize
    }.

get_user_badge_count(UserId, State) ->
    Key = {badge_count, UserId},
    BadgeCountsCache = maps:get(badge_counts_cache, State, #{}),
    case maps:get(Key, BadgeCountsCache, undefined) of
        undefined ->
            undefined;
        Badge ->
            Badge
    end.

cache_user_badge_count(UserId, BadgeCount, CachedAt, State) ->
    Key = {badge_count, UserId},
    NewBadge = {BadgeCount, CachedAt},

    OldBadgeSize =
        case maps:get(Key, maps:get(badge_counts_cache, State, #{}), undefined) of
            undefined -> 0;
            OldBadge -> estimate_badge_count_size(OldBadge)
        end,
    NewBadgeSize = estimate_badge_count_size(NewBadge),
    SizeDelta = NewBadgeSize - OldBadgeSize,

    BadgeCountsLru = maps:get(badge_counts_lru, State, []),
    NewLru = update_lru(Key, BadgeCountsLru),
    BadgeCountsCache = maps:get(badge_counts_cache, State, #{}),
    NewCache = maps:put(Key, NewBadge, BadgeCountsCache),
    BadgeCountsSize = maps:get(badge_counts_size, State, 0),
    NewSize = BadgeCountsSize + SizeDelta,

    MaxBytes =
        case maps:get(badge_counts_max_mb, State, undefined) of
            undefined -> NewSize;
            Mb -> Mb * 1024 * 1024
        end,
    {FinalCache, FinalLru, FinalSize} = evict_if_needed(
        NewCache, NewLru, NewSize, MaxBytes
    ),

    State#{
        badge_counts_cache => FinalCache,
        badge_counts_lru => FinalLru,
        badge_counts_size => FinalSize
    }.

estimate_subscriptions_size(Subscriptions) ->
    length(Subscriptions) * 200.

estimate_badge_count_size({_Count, _Timestamp}) ->
    64.

evict_if_needed(Cache, Lru, Size, MaxBytes) when Size > MaxBytes ->
    evict_oldest(Cache, Lru, Size, MaxBytes, lists:reverse(Lru));
evict_if_needed(Cache, Lru, Size, _MaxBytes) ->
    {Cache, Lru, Size}.

evict_oldest(Cache, Lru, Size, _MaxBytes, []) ->
    {Cache, Lru, Size};
evict_oldest(Cache, Lru, Size, MaxBytes, [OldestKey | Remaining]) ->
    case maps:get(OldestKey, Cache, undefined) of
        undefined ->
            evict_oldest(Cache, Lru, Size, MaxBytes, Remaining);
        OldSubs ->
            NewCache = maps:remove(OldestKey, Cache),
            NewSize = Size - estimate_subscriptions_size(OldSubs),
            NewLru = lists:delete(OldestKey, Lru),
            evict_if_needed(NewCache, NewLru, NewSize, MaxBytes)
    end.

invalidate_user_badge_count(UserId, State) ->
    Key = {badge_count, UserId},
    BadgeCountsCache = maps:get(badge_counts_cache, State, #{}),
    case maps:get(Key, BadgeCountsCache, undefined) of
        undefined ->
            State;
        Badge ->
            NewCache = maps:remove(Key, BadgeCountsCache),
            BadgeCountsLru = lists:delete(Key, maps:get(badge_counts_lru, State, [])),
            BadgeCountsSize = maps:get(badge_counts_size, State, 0),
            NewSize = max(0, BadgeCountsSize - estimate_badge_count_size(Badge)),
            State#{
                badge_counts_cache => NewCache,
                badge_counts_lru => BadgeCountsLru,
                badge_counts_size => NewSize
            }
    end.
