%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_availability_cache).
-typing([eqwalizer]).

-export([
    get_cached_unavailability_mode/1,
    get_cached_unavailability_entry/1,
    set_cached_unavailability_mode/2,
    set_cached_unavailability_mode/3,
    ensure_unavailability_cache_table/0
]).

-type guild_id() :: integer().
-type unavailability_mode() ::
    available
    | unavailable_for_everyone
    | unavailable_for_everyone_but_staff.
-type unavailability_cache_entry() :: {unavailability_mode(), boolean()}.

-export_type([guild_id/0, unavailability_mode/0, unavailability_cache_entry/0]).

-define(GUILD_UNAVAILABILITY_CACHE, guild_unavailability_cache).

-spec get_cached_unavailability_mode(guild_id()) -> unavailability_mode().
get_cached_unavailability_mode(GuildId) ->
    {Mode, _UnavailableHidden} = get_cached_unavailability_entry(GuildId),
    Mode.

-spec get_cached_unavailability_entry(guild_id()) -> unavailability_cache_entry().
get_cached_unavailability_entry(GuildId) ->
    ensure_unavailability_cache_table(),
    case ets:lookup(?GUILD_UNAVAILABILITY_CACHE, GuildId) of
        [{GuildId, Mode}] ->
            {normalize_unavailability_mode(Mode), false};
        [{GuildId, Mode, UnavailableHidden}] ->
            {normalize_unavailability_mode(Mode), normalize_boolean(UnavailableHidden)};
        [] ->
            {available, false}
    end.

-spec set_cached_unavailability_mode(guild_id(), unavailability_mode()) -> ok.
set_cached_unavailability_mode(GuildId, available) ->
    set_cached_unavailability_mode(GuildId, available, false);
set_cached_unavailability_mode(GuildId, Mode) ->
    set_cached_unavailability_mode(GuildId, Mode, false).

-spec set_cached_unavailability_mode(guild_id(), unavailability_mode(), boolean()) -> ok.
set_cached_unavailability_mode(GuildId, available, _UnavailableHidden) ->
    ensure_unavailability_cache_table(),
    ets:delete(?GUILD_UNAVAILABILITY_CACHE, GuildId),
    ok;
set_cached_unavailability_mode(GuildId, Mode, UnavailableHidden) ->
    ensure_unavailability_cache_table(),
    ets:insert(
        ?GUILD_UNAVAILABILITY_CACHE, {GuildId, Mode, normalize_boolean(UnavailableHidden)}
    ),
    ok.

-spec ensure_unavailability_cache_table() -> ok.
ensure_unavailability_cache_table() ->
    guild_ets_utils:ensure_table(
        ?GUILD_UNAVAILABILITY_CACHE,
        [named_table, public, set, {read_concurrency, true}]
    ).

-spec normalize_boolean(term()) -> boolean().
normalize_boolean(true) -> true;
normalize_boolean(_) -> false.

-spec normalize_unavailability_mode(term()) -> unavailability_mode().
normalize_unavailability_mode(unavailable_for_everyone) ->
    unavailable_for_everyone;
normalize_unavailability_mode(unavailable_for_everyone_but_staff) ->
    unavailable_for_everyone_but_staff;
normalize_unavailability_mode(_) ->
    available.
