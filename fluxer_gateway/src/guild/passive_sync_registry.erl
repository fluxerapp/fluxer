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

-module(passive_sync_registry).

-export([
    init/0,
    store/3,
    lookup/2,
    delete/2,
    delete_all_for_session/1
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(TABLE, passive_sync_registry).

-type session_id() :: binary().
-type guild_id() :: integer().
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
    ets:insert(?TABLE, {Key, PassiveState, SessionId}),
    ok.

-spec lookup(session_id(), guild_id()) -> passive_state().
lookup(SessionId, GuildId) ->
    ensure_table(),
    Key = {SessionId, GuildId},
    case ets:lookup(?TABLE, Key) of
        [{Key, PassiveState, _SessionId}] ->
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

-spec delete_all_for_session(session_id()) -> ok.
delete_all_for_session(SessionId) ->
    ensure_table(),
    ets:match_delete(?TABLE, {'_', '_', SessionId}),
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
    ?assertEqual(#{
        previous_passive_updates => #{},
        previous_passive_channel_versions => #{},
        previous_passive_voice_states => #{}
    }, Result),
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
    ?assertEqual(#{
        previous_passive_updates => #{},
        previous_passive_channel_versions => #{},
        previous_passive_voice_states => #{}
    }, Result),
    cleanup_table().

delete_all_for_session_removes_all_guilds_test() ->
    cleanup_table(),
    ok = init(),
    SessionId = <<"session_1">>,
    OtherSessionId = <<"session_2">>,
    State1 = #{
        previous_passive_updates => #{<<"ch1">> => <<"msg1">>},
        previous_passive_channel_versions => #{},
        previous_passive_voice_states => #{}
    },
    State2 = #{
        previous_passive_updates => #{<<"ch2">> => <<"msg2">>},
        previous_passive_channel_versions => #{},
        previous_passive_voice_states => #{}
    },
    OtherState = #{
        previous_passive_updates => #{<<"ch3">> => <<"msg3">>},
        previous_passive_channel_versions => #{},
        previous_passive_voice_states => #{}
    },
    ok = store(SessionId, 100, State1),
    ok = store(SessionId, 200, State2),
    ok = store(OtherSessionId, 100, OtherState),
    ok = delete_all_for_session(SessionId),
    ?assertEqual(#{
        previous_passive_updates => #{},
        previous_passive_channel_versions => #{},
        previous_passive_voice_states => #{}
    }, lookup(SessionId, 100)),
    ?assertEqual(#{
        previous_passive_updates => #{},
        previous_passive_channel_versions => #{},
        previous_passive_voice_states => #{}
    }, lookup(SessionId, 200)),
    ?assertEqual(OtherState, lookup(OtherSessionId, 100)),
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
        undefined -> ok;
        _ -> ets:delete(?TABLE), ok
    end.

-endif.
