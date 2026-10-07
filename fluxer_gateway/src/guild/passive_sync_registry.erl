%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(passive_sync_registry).
-typing([eqwalizer]).

-export([
    init/0,
    store/3,
    lookup/2,
    delete/2
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

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

-ifdef(TEST).

init_creates_table_test() ->
    cleanup_table(),
    ok = init(),
    ?assertNotEqual(undefined, ets:whereis(?TABLE)),
    cleanup_table().

init_idempotent_test() ->
    cleanup_table(),
    ok = init(),
    ok = init(),
    ?assertNotEqual(undefined, ets:whereis(?TABLE)),
    cleanup_table().

store_and_lookup_test() ->
    cleanup_table(),
    ok = init(),
    SessionId = <<"session_1">>,
    GuildId = 100,
    PassiveState = #{
        previous_passive_updates => #{<<"ch1">> => <<"msg1">>},
        previous_passive_channel_versions => #{<<"ch1">> => 5},
        previous_passive_voice_states => #{}
    },
    ok = store(SessionId, GuildId, PassiveState),
    Result = lookup(SessionId, GuildId),
    ?assertEqual(PassiveState, Result),
    cleanup_table().

lookup_missing_returns_defaults_test() ->
    cleanup_table(),
    ok = init(),
    Result = lookup(<<"nonexistent">>, 999),
    ?assertEqual(
        #{
            previous_passive_updates => #{},
            previous_passive_channel_versions => #{},
            previous_passive_voice_states => #{}
        },
        Result
    ),
    cleanup_table().

delete_removes_entry_test() ->
    cleanup_table(),
    ok = init(),
    SessionId = <<"session_1">>,
    GuildId = 100,
    PassiveState = #{
        previous_passive_updates => #{<<"ch1">> => <<"msg1">>},
        previous_passive_channel_versions => #{},
        previous_passive_voice_states => #{}
    },
    ok = store(SessionId, GuildId, PassiveState),
    ok = delete(SessionId, GuildId),
    Result = lookup(SessionId, GuildId),
    ?assertEqual(
        #{
            previous_passive_updates => #{},
            previous_passive_channel_versions => #{},
            previous_passive_voice_states => #{}
        },
        Result
    ),
    cleanup_table().

store_overwrites_existing_test() ->
    cleanup_table(),
    ok = init(),
    SessionId = <<"session_1">>,
    GuildId = 100,
    State1 = #{
        previous_passive_updates => #{<<"ch1">> => <<"msg1">>},
        previous_passive_channel_versions => #{},
        previous_passive_voice_states => #{}
    },
    State2 = #{
        previous_passive_updates => #{<<"ch1">> => <<"msg2">>},
        previous_passive_channel_versions => #{<<"ch1">> => 3},
        previous_passive_voice_states => #{}
    },
    ok = store(SessionId, GuildId, State1),
    ok = store(SessionId, GuildId, State2),
    Result = lookup(SessionId, GuildId),
    ?assertEqual(State2, Result),
    cleanup_table().

cleanup_table() ->
    case ets:whereis(?TABLE) of
        undefined ->
            ok;
        _ ->
            ets:delete(?TABLE),
            ok
    end.

-endif.
