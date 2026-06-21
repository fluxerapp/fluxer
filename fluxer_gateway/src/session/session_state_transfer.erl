%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_state_transfer).
-typing([eqwalizer]).
-behaviour(gen_server).

-export([
    start_link/0,
    push_state/3,
    pop_state/1,
    pop_state/2
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(RPC_TIMEOUT_MS, 5000).
-define(DEFAULT_STATE_TTL_MS, 120000).
-define(CLEANUP_INTERVAL_MS, 10000).

-type session_id() :: binary().
-type transferred_state() :: #{
    session_id := session_id(),
    state := map(),
    received_at := integer()
}.
-type store() :: #{session_id() => transferred_state()}.

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec push_state(node(), session_id(), map()) -> ok | {error, term()}.
push_state(TargetNode, SessionId, SessionState) ->
    try
        gen_server:call(
            {?MODULE, TargetNode},
            {receive_state, SessionId, SessionState},
            ?RPC_TIMEOUT_MS
        )
    of
        ok -> ok;
        {error, _} = Error -> Error;
        _ -> {error, unavailable}
    catch
        error:Reason -> {error, Reason};
        exit:Reason -> {error, Reason}
    end.

-spec pop_state(session_id()) -> {ok, map()} | {error, not_found}.
pop_state(SessionId) ->
    pop_state(SessionId, node()).

-spec pop_state(session_id(), node()) -> {ok, map()} | {error, not_found}.
pop_state(SessionId, TargetNode) ->
    try gen_server:call({?MODULE, TargetNode}, {pop_state, SessionId}, ?RPC_TIMEOUT_MS) of
        {ok, _State} = Result -> Result;
        _ -> {error, not_found}
    catch
        error:_Reason -> {error, not_found};
        exit:_Reason -> {error, not_found}
    end.

-spec init([]) -> {ok, store()}.
init([]) ->
    erlang:process_flag(fullsweep_after, 0),
    schedule_cleanup(),
    {ok, #{}}.

-spec handle_call(term(), gen_server:from(), store()) -> {reply, term(), store()}.
handle_call({receive_state, SessionId, SessionState}, _From, Store) when
    is_binary(SessionId), is_map(SessionState)
->
    Entry = #{
        session_id => SessionId,
        state => SessionState,
        received_at => erlang:monotonic_time(millisecond)
    },
    {reply, ok, Store#{SessionId => Entry}};
handle_call({pop_state, SessionId}, _From, Store) when is_binary(SessionId) ->
    case maps:take(SessionId, Store) of
        {#{state := State}, RemainingStore} ->
            {reply, {ok, State}, RemainingStore};
        error ->
            {reply, {error, not_found}, Store}
    end;
handle_call(_Request, _From, Store) ->
    {reply, ok, Store}.

-spec handle_cast(term(), store()) -> {noreply, store()}.
handle_cast(_Msg, Store) ->
    {noreply, Store}.

-spec handle_info(term(), store()) -> {noreply, store()}.
handle_info(cleanup_expired, Store) ->
    Now = erlang:monotonic_time(millisecond),
    Ttl = state_ttl_ms(),
    Cleaned = maps:filter(
        fun(_SessionId, #{received_at := ReceivedAt}) ->
            (Now - ReceivedAt) < Ttl
        end,
        Store
    ),
    schedule_cleanup(),
    {noreply, Cleaned};
handle_info(_Info, Store) ->
    {noreply, Store}.

-spec terminate(term(), store()) -> ok.
terminate(_Reason, _Store) ->
    ok.

-spec code_change(term(), store(), term()) -> {ok, store()}.
code_change(_OldVsn, Store, _Extra) ->
    erlang:process_flag(fullsweep_after, 0),
    erlang:garbage_collect(),
    {ok, Store}.

-spec schedule_cleanup() -> reference().
schedule_cleanup() ->
    erlang:send_after(?CLEANUP_INTERVAL_MS, self(), cleanup_expired).

-spec state_ttl_ms() -> pos_integer().
state_ttl_ms() ->
    try fluxer_gateway_env:get(session_state_transfer_ttl_ms) of
        Value when is_integer(Value), Value > 0 -> Value;
        _ -> ?DEFAULT_STATE_TTL_MS
    catch
        error:_ -> ?DEFAULT_STATE_TTL_MS;
        exit:_ -> ?DEFAULT_STATE_TTL_MS
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

receive_and_pop_state_test() ->
    SessionId = <<"test-session-1">>,
    SessionState = #{seq => 42, buffer => []},
    {ok, SessionState, Store2} = push_and_pop(SessionId, SessionState, #{}),
    ?assertEqual(#{}, Store2).

pop_state_not_found_test() ->
    Store = #{},
    {reply, {error, not_found}, Store} = handle_call(
        {pop_state, <<"nonexistent">>}, test_from(), Store
    ).

cleanup_expired_removes_old_entries_test() ->
    Now = erlang:monotonic_time(millisecond),
    Store = #{
        <<"old">> => #{
            session_id => <<"old">>,
            state => #{},
            received_at => Now - 600000
        },
        <<"fresh">> => #{
            session_id => <<"fresh">>,
            state => #{},
            received_at => Now
        }
    },
    {noreply, Cleaned} = handle_info(cleanup_expired, Store),
    ?assertEqual(false, maps:is_key(<<"old">>, Cleaned)),
    ?assertEqual(true, maps:is_key(<<"fresh">>, Cleaned)).

pop_removes_entry_so_double_pop_fails_test() ->
    SessionId = <<"double-pop">>,
    {ok, #{seq := 1}, Store2} = push_and_pop(SessionId, #{seq => 1}, #{}),
    {reply, {error, not_found}, Store2} = handle_call(
        {pop_state, SessionId}, test_from(), Store2
    ).

transfer_state_round_trip_preserves_all_fields_test() ->
    SessionId = <<"round-trip-session">>,
    TransferState = build_round_trip_state(SessionId),
    {ok, Recovered, _} = push_and_pop(SessionId, TransferState, #{}),
    assert_round_trip_fields(SessionId, Recovered).

build_round_trip_state(SessionId) ->
    #{
        id => SessionId,
        user_id => 42,
        user_data => #{<<"username">> => <<"tester">>},
        custom_status => null,
        version => 9,
        token_hash => <<"hash123">>,
        auth_session_id_hash => <<"auth_hash">>,
        properties => #{<<"os">> => <<"linux">>},
        status => online,
        afk => false,
        mobile => true,
        activities => [#{<<"name">> => <<"Fluxcap">>, <<"type">> => 0}],
        socket_pid => undefined,
        guilds => [100, 200, 300],
        active_guilds => sets:from_list([100, 300]),
        ready => #{<<"guilds">> => []},
        bot => false,
        e2ee_capable => true,
        ignored_events => [<<"TYPING_START">>],
        initial_guild_id => 100,
        debounce_reactions => true,
        channels => #{1 => #{}},
        relationships => #{10 => 1},
        seq => 15,
        ack_seq => 12,
        buffer => [#{seq => 13}, #{seq => 14}, #{seq => 15}],
        collected_guild_states => [],
        collected_sessions => [],
        collected_presences => [],
        guild_subscription_state => #{}
    }.

assert_round_trip_fields(SessionId, RS) ->
    ?assertEqual(SessionId, maps:get(id, RS)),
    ?assertEqual(42, maps:get(user_id, RS)),
    ?assertEqual(<<"hash123">>, maps:get(token_hash, RS)),
    ?assertEqual(<<"auth_hash">>, maps:get(auth_session_id_hash, RS)),
    ?assertEqual(15, maps:get(seq, RS)),
    ?assertEqual(12, maps:get(ack_seq, RS)),
    ?assertEqual([#{<<"name">> => <<"Fluxcap">>, <<"type">> => 0}], maps:get(activities, RS)),
    ?assertEqual(3, length(maps:get(buffer, RS))),
    ?assertEqual([100, 200, 300], maps:get(guilds, RS)),
    ?assert(sets:is_element(100, maps:get(active_guilds, RS))),
    ?assert(sets:is_element(300, maps:get(active_guilds, RS))),
    ?assertEqual(true, maps:get(e2ee_capable, RS)),
    ?assertEqual(true, maps:get(debounce_reactions, RS)).

push_and_pop(SessionId, SessionState, Store0) ->
    {reply, ok, Store1} = handle_call(
        {receive_state, SessionId, SessionState}, test_from(), Store0
    ),
    {reply, {ok, Recovered0}, Store2} = handle_call(
        {pop_state, SessionId}, test_from(), Store1
    ),
    {ok, ensure_test_map(Recovered0), Store2}.

cleanup_sweep_runs_periodically_test() ->
    {ok, _Store} = init([]),
    receive
        cleanup_expired -> ok
    after 100 ->
        ok
    end.

unknown_call_returns_ok_test() ->
    {reply, ok, #{}} = handle_call(unknown_request, test_from(), #{}).

unknown_cast_is_noop_test() ->
    {noreply, #{}} = handle_cast(unknown_message, #{}).

unknown_info_is_noop_test() ->
    {noreply, #{}} = handle_info(unknown_message, #{}).

-spec test_from() -> gen_server:from().
test_from() ->
    {self(), make_ref()}.

-spec ensure_test_map(term()) -> map().
ensure_test_map(V) when is_map(V) -> V.

-endif.
