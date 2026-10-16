%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_broadcaster).
-typing([eqwalizer]).
-behaviour(gen_server).

-export([start_link/1, cast_event/4, ensure/1]).

-define(MAX_MAILBOX, 500).
-define(IDLE_GC_DELAY_MS, 1000).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type broadcaster_ref() :: pid() | undefined.

-spec start_link(integer()) -> gen_server:start_ret().
start_link(GuildId) ->
    gen_server:start_link(
        ?MODULE,
        [GuildId, self()],
        [{spawn_opt, [{message_queue_data, off_heap}]}]
    ).

-spec ensure(map()) -> {pid() | undefined, map()}.
ensure(State) ->
    case maps:get(broadcaster_pid, State, undefined) of
        Pid when is_pid(Pid) -> ensure_alive(Pid, State);
        _ -> start_and_store(State)
    end.

-spec ensure_alive(pid(), map()) -> {pid() | undefined, map()}.
ensure_alive(Pid, State) ->
    case process_liveness:is_alive(Pid) of
        true -> {Pid, State};
        false -> start_and_store(State)
    end.

-spec cast_event(broadcaster_ref(), atom(), term(), [pid()]) -> boolean().
cast_event(BroadcasterPid, Event, EncodedPayload, FilteredSessionPids) ->
    maybe_cast(
        BroadcasterPid,
        {event_broadcast, Event, EncodedPayload, FilteredSessionPids}
    ).

-spec maybe_cast(term(), term()) -> boolean().
maybe_cast(BroadcasterPid, Msg) when is_pid(BroadcasterPid) ->
    case erlang:process_info(BroadcasterPid, message_queue_len) of
        {message_queue_len, Len} when Len >= ?MAX_MAILBOX ->
            false;
        _ ->
            gen_server:cast(BroadcasterPid, Msg),
            true
    end;
maybe_cast(_BroadcasterPid, _Msg) ->
    false.

-spec init([integer() | pid()]) -> {ok, map()}.
init([GuildId, GuildPid]) when is_integer(GuildId), is_pid(GuildPid) ->
    erlang:process_flag(fullsweep_after, 10),
    _ = erlang:monitor(process, GuildPid),
    {ok, #{guild_id => GuildId, guild_pid => GuildPid}}.

-spec handle_call(term(), gen_server:from(), map()) -> {reply, term(), map()}.
handle_call(_Req, _From, State) ->
    {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), map()) -> {noreply, map()}.
handle_cast({event_broadcast, Event, EncodedPayload, FilteredSessionPids}, State) ->
    maybe_dispatch_event_from_terms(Event, EncodedPayload, FilteredSessionPids, State),
    {noreply, maybe_schedule_gc(State)};
handle_cast(_Other, State) ->
    {noreply, State}.

-spec handle_info(term(), map()) ->
    {noreply, map()} | {noreply, map(), hibernate} | {stop, normal, map()}.
handle_info({'DOWN', _Ref, process, GuildPid, _Reason}, #{guild_pid := GuildPid} = State) ->
    {stop, normal, State};
handle_info(run_broadcaster_gc, State) ->
    erlang:garbage_collect(self(), [{type, major}]),
    {noreply, maps:remove(broadcaster_gc_ref, State), hibernate};
handle_info(_Msg, State) ->
    {noreply, State}.

-spec terminate(term(), map()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), map(), term()) -> {ok, map()}.
code_change(_Old, State, _Extra) ->
    erlang:garbage_collect(),
    {ok, State}.

-spec start_and_store(map()) -> {pid() | undefined, map()}.
start_and_store(State) ->
    case guild_id(State) of
        GuildId when is_integer(GuildId), GuildId > 0 ->
            do_start_and_store(GuildId, State);
        _ ->
            {undefined, State}
    end.

-spec do_start_and_store(integer(), map()) -> {pid() | undefined, map()}.
do_start_and_store(GuildId, State) ->
    case start_link(GuildId) of
        {ok, Pid} -> {Pid, State#{broadcaster_pid => Pid}};
        _ -> {undefined, State}
    end.

-spec guild_id(map()) -> integer() | undefined.
guild_id(State) ->
    snowflake_id:first([
        maps:get(guild_id, State, undefined),
        maps:get(id, State, undefined)
    ]).

-spec maybe_dispatch_event(atom(), term(), [pid()], map()) -> ok.
maybe_dispatch_event(Event, EncodedPayload, FilteredSessionPids, State) ->
    case guild_id(State) of
        GuildId when is_integer(GuildId), GuildId > 0 ->
            dispatch_event(Event, EncodedPayload, FilteredSessionPids, GuildId);
        _ ->
            ok
    end.

-spec maybe_dispatch_event_from_terms(term(), term(), term(), map()) -> ok.
maybe_dispatch_event_from_terms(Event, EncodedPayload, FilteredSessionPids, State) when
    is_atom(Event), is_list(FilteredSessionPids)
->
    maybe_dispatch_event(Event, EncodedPayload, filter_pids(FilteredSessionPids), State);
maybe_dispatch_event_from_terms(_Event, _EncodedPayload, _FilteredSessionPids, _State) ->
    ok.

-spec filter_pids([term()]) -> [pid()].
filter_pids(Values) ->
    [Value || Value <- Values, is_pid(Value)].

-spec dispatch_event(atom(), term(), [pid()], integer()) -> ok.
dispatch_event(Event, EncodedPayload, FilteredSessionPids, GuildId) ->
    try
        gateway_dispatch_relay:dispatch_many(
            FilteredSessionPids, Event, EncodedPayload, GuildId
        )
    catch
        Class:Reason:Stack ->
            logger:warning(
                "guild_broadcaster event_broadcast error: ~p:~p ~p",
                [Class, Reason, Stack]
            )
    end.

-spec maybe_schedule_gc(map()) -> map().
maybe_schedule_gc(#{broadcaster_gc_ref := Ref} = State) when is_reference(Ref) ->
    State;
maybe_schedule_gc(State) ->
    Ref = erlang:send_after(?IDLE_GC_DELAY_MS, self(), run_broadcaster_gc),
    State#{broadcaster_gc_ref => Ref}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

blocker_process(Parent) ->
    receive
        go -> ok
    after 30000 -> ok
    end,
    Parent ! done.

maybe_cast_sheds_when_mailbox_full_test() ->
    Parent = self(),
    Blocker = spawn(fun() -> blocker_process(Parent) end),
    lists:foreach(
        fun(I) -> Blocker ! {filler, I} end,
        lists:seq(1, ?MAX_MAILBOX + 10)
    ),
    ?assertEqual(false, maybe_cast(Blocker, {should_be_shed})),
    Blocker ! go,
    receive
        done -> ok
    after 1000 -> ok
    end.

event_broadcast_handler_dispatches_test() ->
    {ok, Broadcaster} = gen_server:start_link(
        ?MODULE,
        [42, self()],
        [{spawn_opt, [{message_queue_data, off_heap}]}]
    ),
    Parent = self(),
    Receiver = spawn(fun() -> event_receiver_loop(Parent) end),
    EncodedPayload = {pre_encoded, <<"test_payload">>},
    gen_server:cast(Broadcaster, {event_broadcast, typing_start, EncodedPayload, [Receiver]}),
    receive
        {received_dispatch, typing_start, {pre_encoded, <<"test_payload">>}} -> ok
    after 2000 ->
        ?assert(false)
    end,
    gen_server:stop(Broadcaster),
    Receiver ! stop.

event_broadcast_handler_survives_crash_test() ->
    {ok, Broadcaster} = gen_server:start_link(
        ?MODULE,
        [99, self()],
        [{spawn_opt, [{message_queue_data, off_heap}]}]
    ),
    BadPids = [not_a_pid],
    gen_server:cast(Broadcaster, {event_broadcast, fake_event, {pre_encoded, <<>>}, BadPids}),
    ok = gateway_retry_timer:wait(50),
    ?assert(is_process_alive(Broadcaster)),
    gen_server:stop(Broadcaster).

event_receiver_loop(Parent) ->
    receive
        stop ->
            ok;
        {'$gen_cast', {dispatch, Event, Payload}} ->
            Parent ! {received_dispatch, Event, Payload},
            event_receiver_loop(Parent);
        _Other ->
            event_receiver_loop(Parent)
    after 30000 ->
        ok
    end.

-endif.
