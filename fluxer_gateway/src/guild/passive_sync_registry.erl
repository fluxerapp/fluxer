%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(passive_sync_registry).
-typing([eqwalizer]).

-export([
    init/0,
    store/3,
    lookup/2,
    delete/2
]).

-type session_id() :: binary().
-type guild_id() :: integer().
-export_type([session_id/0, guild_id/0, passive_state/0]).

-define(TABLE, passive_sync_registry).

-type passive_state() :: #{
    previous_passive_updates := #{binary() => binary()},
    previous_passive_channel_versions := #{binary() => integer()},
    previous_passive_voice_states := #{binary() => map()}
}.

-spec init() -> ok.
init() ->
    case ets:whereis(?TABLE) of
        undefined ->
            _ = ets:new(?TABLE, [
                named_table,
                public,
                set,
                {read_concurrency, true},
                {write_concurrency, true}
            ]),
            ok;
        _ ->
            ok
    end.

-spec store(session_id(), guild_id(), passive_state()) -> ok.
store(SessionId, GuildId, PassiveState) ->
    ensure_table(),
    Key = {SessionId, GuildId},
    ets:insert(?TABLE, {Key, PassiveState}),
    ok.

-spec lookup(session_id(), guild_id()) -> passive_state().
lookup(SessionId, GuildId) ->
    ensure_table(),
    Key = {SessionId, GuildId},
    case ets:lookup(?TABLE, Key) of
        [{Key, PassiveState}] ->
            PassiveState;
        [] ->
            #{
                previous_passive_updates => #{},
                previous_passive_channel_versions => #{},
                previous_passive_voice_states => #{}
            }
    end.

-spec delete(session_id(), guild_id()) -> ok.
delete(SessionId, GuildId) ->
    ensure_table(),
    Key = {SessionId, GuildId},
    ets:delete(?TABLE, Key),
    ok.

-spec ensure_table() -> ok.
ensure_table() ->
    case ets:whereis(?TABLE) of
        undefined -> init();
        _ -> ok
    end.
