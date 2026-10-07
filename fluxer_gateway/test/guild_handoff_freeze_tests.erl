%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_handoff_freeze_tests).
-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(GUILD_ID, 4242).
-define(SHARD_TABLE, guild_manager_shard_table).
-define(SINK, guild_handoff_freeze_tests_sink).

transfer_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        instantiate(fun frozen_backlog_reaches_target_and_source_applies_nothing/1),
        instantiate(fun final_window_call_fails_unapplied_and_cast_is_kept/1),
        instantiate(fun route_failure_restores_routes_resumes_source_and_stops_target/1),
        instantiate(fun controller_death_before_start_resumes_source/1),
        instantiate(fun controller_death_after_start_kills_target_and_resumes_source/1),
        instantiate(fun target_death_before_commit_keeps_source/1),
        instantiate(fun sessions_stay_behind_when_not_transferred/1),
        instantiate(fun cast_during_source_terminate_reaches_target/1),
        instantiate(fun monitor_flush_in_source_terminate_loses_no_cast/1),
        instantiate(fun connect_worker_results_stay_with_the_source/1),
        instantiate(fun abort_after_forwarding_reports_the_forwarded_count/1),
        instantiate(fun late_source_stop_keeps_the_committed_target/1),
        instantiate(fun controller_death_after_commit_keeps_the_target/1),
        instantiate(fun lost_guard_aborts_before_other_nodes_route_to_the_target/1),
        instantiate(fun thread_state_survives_the_frozen_export/1),
        instantiate(fun stale_thread_sessions_do_not_break_a_sessionless_restore/1)
    ]}.

instantiate(Test) ->
    fun(Ctx) -> {timeout, 30, fun() -> Test(Ctx) end} end.

frozen_backlog_reaches_target_and_source_applies_nothing(#{src := Src, src_shard := SrcShard}) ->
    Test = self(),
    BeforeStart = fun() ->
        gen_server:cast(Src, {append, b}),
        spawn(fun() -> Test ! {call_reply, gen_server:call(Src, {append_call, c}, 30000)} end),
        wait_mailbox(Src, 2)
    end,
    Result = guild_handoff_freeze:transfer(
        ?GUILD_ID, Src, SrcShard, node(), #{before_start => BeforeStart}
    ),
    ?assertMatch({ok, #{new_pid := _}}, Result),
    {ok, #{new_pid := NewPid}} = Result,
    ?assertEqual({call_reply, {ok, NewPid}}, receive_tagged(call_reply)),
    ?assertEqual([{Src, {shutdown, handoff}, [a]}], ets:lookup(?SINK, Src)),
    ?assertEqual([a, b, c], gen_server:call(NewPid, get_log)),
    ?assertEqual(false, is_process_alive(Src)).

final_window_call_fails_unapplied_and_cast_is_kept(#{src := Src, src_shard := SrcShard}) ->
    Test = self(),
    BeforeStart = fun() ->
        gen_server:cast(Src, {append, b}),
        spawn(fun() -> Test ! {call_reply, call_or_exit(Src, {append_call, c})} end),
        wait_mailbox(Src, 2)
    end,
    Opts = #{before_start => BeforeStart, forward_rounds => 0},
    Result = guild_handoff_freeze:transfer(?GUILD_ID, Src, SrcShard, node(), Opts),
    ?assertMatch({ok, #{final := #{cast := 1, dropped_call := 1}}}, Result),
    {ok, #{new_pid := NewPid}} = Result,
    ?assertMatch({call_reply, {exit, {{shutdown, handoff}, _}}}, receive_tagged(call_reply)),
    ?assertEqual([a, b], gen_server:call(NewPid, get_log)).

route_failure_restores_routes_resumes_source_and_stops_target(#{
    src := Src, src_shard := SrcShard, dst_shard := DstShard
}) ->
    Test = self(),
    Opts = #{
        before_start => fun() -> gen_server:cast(Src, {append, b}) end,
        after_start => fun(_NewPid) -> {error, route_rejected} end,
        on_abort => fun() ->
            Test ! routes_restored,
            ok
        end
    },
    Result = guild_handoff_freeze:transfer(?GUILD_ID, Src, SrcShard, node(), Opts),
    ?assertMatch({error, #{phase := after_start, reason := route_rejected}}, Result),
    ?assertEqual(routes_restored, receive_tagged(routes_restored)),
    ?assertEqual({error, not_found}, gen_server:call(DstShard, {lookup, ?GUILD_ID})),
    ?assertEqual([a, b], gen_server:call(Src, get_log)),
    ?assertEqual(false, guild_handoff_freeze:is_frozen(Src)).

controller_death_before_start_resumes_source(#{src := Src, src_shard := SrcShard}) ->
    Opts = #{
        before_start => fun() ->
            gen_server:cast(Src, {append, b}),
            exit(self(), kill)
        end
    },
    Result = guild_handoff_freeze:transfer(?GUILD_ID, Src, SrcShard, node(), Opts),
    ?assertMatch({error, #{phase := crashed, reason := killed}}, Result),
    ?assertEqual([a, b], gen_server:call(Src, get_log, 5000)).

controller_death_after_start_kills_target_and_resumes_source(#{
    src := Src, src_shard := SrcShard, dst_shard := DstShard
}) ->
    Test = self(),
    Opts = #{
        after_start => fun(NewPid) ->
            Test ! {target, NewPid},
            exit(self(), kill)
        end,
        on_abort => fun() ->
            Test ! routes_restored,
            ok
        end
    },
    Result = guild_handoff_freeze:transfer(?GUILD_ID, Src, SrcShard, node(), Opts),
    ?assertMatch({error, #{phase := crashed, abort := #{routes := ok, target := ok}}}, Result),
    ?assertEqual(routes_restored, receive_tagged(routes_restored)),
    {target, NewPid} = receive_tagged(target),
    ?assertEqual(ok, wait_dead(NewPid)),
    ?assertEqual({error, not_found}, gen_server:call(DstShard, {lookup, ?GUILD_ID})),
    ?assertEqual([a], gen_server:call(Src, get_log, 5000)).

target_death_before_commit_keeps_source(#{src := Src, src_shard := SrcShard}) ->
    Opts = #{
        after_start => fun(NewPid) ->
            exit(NewPid, kill),
            wait_dead(NewPid)
        end
    },
    Result = guild_handoff_freeze:transfer(?GUILD_ID, Src, SrcShard, node(), Opts),
    ?assertMatch({error, #{phase := target_died, reason := killed}}, Result),
    ?assertEqual([a], gen_server:call(Src, get_log, 5000)),
    ?assertEqual(false, guild_handoff_freeze:is_frozen(Src)).

sessions_stay_behind_when_not_transferred(#{src := Src, src_shard := SrcShard}) ->
    ok = gen_server:call(Src, {put_session, <<"s1">>, #{pid => self(), user_id => 7}}),
    Opts = #{transfer_sessions => false},
    Result = guild_handoff_freeze:transfer(?GUILD_ID, Src, SrcShard, node(), Opts),
    ?assertMatch({ok, #{new_pid := _}}, Result),
    {ok, #{new_pid := NewPid}} = Result,
    ?assertEqual(#{}, gen_server:call(NewPid, get_sessions)),
    ?assertEqual([a], gen_server:call(NewPid, get_log)).

cast_during_source_terminate_reaches_target(#{src := Src, src_shard := SrcShard}) ->
    ok = gen_server:call(Src, cast_late_on_terminate),
    Result = guild_handoff_freeze:transfer(?GUILD_ID, Src, SrcShard, node(), #{}),
    ?assertMatch({ok, #{final := #{cast := 1}}}, Result),
    {ok, #{new_pid := NewPid}} = Result,
    ?assertEqual([a, late], gen_server:call(NewPid, get_log)).

monitor_flush_in_source_terminate_loses_no_cast(#{src := Src, src_shard := SrcShard}) ->
    ok = gen_server:call(Src, flush_down_on_terminate),
    Result = guild_handoff_freeze:transfer(?GUILD_ID, Src, SrcShard, node(), #{}),
    ?assertMatch({ok, #{final := #{cast := 1}}}, Result),
    {ok, #{new_pid := NewPid}} = Result,
    ?assertEqual([a, late], gen_server:call(NewPid, get_log)).

connect_worker_results_stay_with_the_source(#{src := Src, src_shard := SrcShard}) ->
    BeforeStart = fun() ->
        gen_server:cast(Src, {session_connect_worker_done, <<"s1">>, 1, {ok, #{}}, #{}}),
        gen_server:cast(
            Src, {session_connect_worker_batch_done, [{<<"s1">>, 1, {ok, #{}}, #{}}]}
        ),
        gen_server:cast(Src, {append, b}),
        wait_mailbox(Src, 3)
    end,
    Result = guild_handoff_freeze:transfer(
        ?GUILD_ID, Src, SrcShard, node(), #{before_start => BeforeStart}
    ),
    ?assertMatch({ok, #{new_pid := _}}, Result),
    {ok, #{new_pid := NewPid}} = Result,
    ?assertEqual([a, b], gen_server:call(NewPid, get_log)).

abort_after_forwarding_reports_the_forwarded_count(#{src := Src, src_shard := SrcShard}) ->
    BeforeStart = fun() ->
        gen_server:cast(Src, crash),
        wait_mailbox(Src, 1)
    end,
    Result = guild_handoff_freeze:transfer(
        ?GUILD_ID, Src, SrcShard, node(), #{before_start => BeforeStart}
    ),
    ?assertMatch(
        {error, #{phase := target_died, abort := #{forwarded := 1, thaw := ok}}}, Result
    ).

late_source_stop_keeps_the_committed_target(#{
    src := Src, src_shard := SrcShard, dst_shard := DstShard
}) ->
    Opts = #{
        stop_timeout => 100,
        after_start => fun(_NewPid) -> hold(SrcShard, 600) end
    },
    Result = guild_handoff_freeze:transfer(?GUILD_ID, Src, SrcShard, node(), Opts),
    ?assertMatch({ok, #{new_pid := _}}, Result),
    {ok, #{new_pid := NewPid}} = Result,
    ?assertEqual(ok, wait_dead(Src)),
    ?assert(is_process_alive(NewPid)),
    ?assertEqual({ok, NewPid}, gen_server:call(DstShard, {lookup, ?GUILD_ID})),
    ?assertEqual([a], gen_server:call(NewPid, get_log)).

controller_death_after_commit_keeps_the_target(#{
    src := Src, src_shard := SrcShard, dst_shard := DstShard
}) ->
    Test = self(),
    Opts = #{
        after_start => fun(NewPid) ->
            Test ! {started, self(), NewPid},
            hold(SrcShard, 1000)
        end
    },
    spawn(fun() ->
        Test ! {result, guild_handoff_freeze:transfer(?GUILD_ID, Src, SrcShard, node(), Opts)}
    end),
    {started, Controller, NewPid} = receive_tagged(started),
    ok = wait_mailbox(SrcShard, 1),
    exit(Controller, kill),
    {result, Result} = receive_tagged(result),
    ?assertMatch({error, #{phase := crashed, committed := true}}, Result),
    ?assertEqual(ok, wait_dead(Src)),
    ?assert(is_process_alive(NewPid)),
    ?assertEqual({ok, NewPid}, gen_server:call(DstShard, {lookup, ?GUILD_ID})).

lost_guard_aborts_before_other_nodes_route_to_the_target(#{
    src := Src, src_shard := SrcShard, dst_shard := DstShard
}) ->
    Test = self(),
    Opts = #{
        before_start => fun() ->
            {monitored_by, Watchers} = process_info(self(), monitored_by),
            [exit(W, kill) || W <- Watchers, W =/= Test],
            ok
        end,
        after_start => fun(_NewPid) ->
            Test ! routed,
            ok
        end
    },
    Result = guild_handoff_freeze:transfer(?GUILD_ID, Src, SrcShard, node(), Opts),
    ?assertMatch(
        {error, #{
            phase := after_start, reason := {guard_lost, killed}, abort := #{exposed := false}
        }},
        Result
    ),
    ?assertEqual({timeout, routed}, receive_tagged(routed, 200)),
    ?assertEqual({error, not_found}, gen_server:call(DstShard, {lookup, ?GUILD_ID})),
    ?assertEqual(false, guild_handoff_freeze:is_frozen(Src)),
    ?assertEqual([a], gen_server:call(Src, get_log, 5000)).

thread_state_survives_the_frozen_export(#{src := Src, src_shard := SrcShard}) ->
    ok = gen_server:call(Src, {put_session, <<"s1">>, #{pid => self(), user_id => 7}}),
    ok = gen_server:call(Src, install_threads),
    Result = guild_handoff_freeze:transfer(?GUILD_ID, Src, SrcShard, node(), #{}),
    ?assertMatch({ok, #{new_pid := _}}, Result),
    {ok, #{new_pid := NewPid}} = Result,
    ?assertEqual(ok, wait_dead(Src)),
    State = guild_init:init_base_state(gen_server:call(NewPid, get_export)),
    try
        ?assertMatch(#{<<"id">> := 10}, guild_thread_gate:thread(10, State)),
        ?assert(guild_thread_gate:active(State)),
        ?assertEqual([<<"s1">>], maps:keys(maps:get(thread_resend_pending, State))),
        ?assertEqual([<<"s1">>], maps:keys(maps:get(thread_sync_pending, State))),
        ?assertEqual([10], maps:keys(maps:get(thread_list_dirty, State))),
        ?assertEqual(
            thread_member_list_flush, receive_tagged(thread_member_list_flush, 2000)
        )
    after
        guild_thread_store:destroy(guild_thread_gate:store(State))
    end.

stale_thread_sessions_do_not_break_a_sessionless_restore(#{src := Src, src_shard := SrcShard}) ->
    ok = gen_server:call(Src, {put_session, <<"s1">>, #{pid => self(), user_id => 7}}),
    ok = gen_server:call(Src, install_threads),
    Opts = #{transfer_sessions => false},
    {ok, #{new_pid := NewPid}} =
        guild_handoff_freeze:transfer(?GUILD_ID, Src, SrcShard, node(), Opts),
    Export = gen_server:call(NewPid, get_export),
    ?assertEqual(#{}, maps:get(sessions, Export)),
    ?assertEqual([<<"s1">>], maps:get(thread_handoff_resends, Export)),
    State = guild_init:init_base_state(Export),
    try
        ?assertMatch(#{<<"id">> := 10}, guild_thread_gate:thread(10, State)),
        Resent = guild_thread_flip:resend(<<"s1">>, State),
        ?assertNot(maps:is_key(thread_resend_pending, Resent)),
        Synced = guild_thread_subscriptions:handle_tick(<<"s1">>, Resent),
        ?assertNot(maps:is_key(thread_sync_pending, Synced))
    after
        guild_thread_store:destroy(guild_thread_gate:store(State))
    end.

hold(Pid, Ms) ->
    Test = self(),
    spawn(fun() ->
        true = erlang:suspend_process(Pid),
        Test ! {held, Pid},
        timer:sleep(Ms),
        true = erlang:resume_process(Pid)
    end),
    receive
        {held, Pid} -> ok
    end.

export_drops_only_keys_the_importer_rebuilds_test() ->
    Raw = #{
        <<"guild">> => #{<<"id">> => <<"4242">>},
        <<"roles">> => [#{<<"id">> => <<"4242">>, <<"permissions">> => <<"1024">>}],
        <<"channels">> => [#{<<"id">> => <<"77">>, <<"type">> => 0}],
        <<"members">> => [
            #{<<"user">> => #{<<"id">> => <<"5">>}, <<"roles">> => []},
            #{<<"user">> => #{<<"id">> => <<"6">>}, <<"roles">> => [<<"4242">>]}
        ]
    },
    Full = guild_data_index:normalize_map(Raw),
    State = #{id => ?GUILD_ID, data => Full#{members_ets => make_ref()}, sessions => #{}},
    Slim = maps:get(data, guild_handoff:export_handoff_state(State)),
    ?assertEqual([], [K || K <- guild_handoff:derived_data_keys(), maps:is_key(K, Slim)]),
    Rebuild = fun(D) ->
        maps:remove(member_list_revision, guild_data_index:normalize_map(D))
    end,
    ?assertEqual(Rebuild(Full), Rebuild(Slim)).

export_keeps_channel_fields_that_only_the_index_holds_test() ->
    Raw = #{
        <<"guild">> => #{<<"id">> => <<"4242">>},
        <<"channels">> => [
            #{<<"id">> => <<"77">>, <<"type">> => 0, <<"last_message_id">> => <<"10">>}
        ]
    },
    Full = guild_state_channels:handle_message_create(
        #{<<"channel_id">> => <<"77">>, <<"id">> => <<"900">>},
        guild_data_index:normalize_map(Raw)
    ),
    State = #{id => ?GUILD_ID, data => Full, sessions => #{}},
    Slim = maps:get(data, guild_handoff:export_handoff_state(State)),
    ?assertMatch({rebuilt, _}, bounded_rebuild(Slim)),
    {rebuilt, Rebuilt} = bounded_rebuild(Slim),
    [Channel] = guild_data_index:channel_list(Rebuilt),
    ?assertEqual(900, maps:get(<<"last_message_id">>, Channel)).

bounded_rebuild(Data) ->
    {Pid, Ref} = spawn_monitor(fun() ->
        process_flag(max_heap_size, #{size => 4000000, kill => true, error_logger => false}),
        exit({rebuilt, guild_data_index:normalize_map(Data)})
    end),
    receive
        {'DOWN', Ref, process, Pid, Reason} -> Reason
    after 10000 ->
        exit(Pid, kill),
        timeout
    end.

setup() ->
    ets:new(?SHARD_TABLE, [named_table, public, set]),
    ets:new(?SINK, [named_table, public, set]),
    {ok, SrcShard} = gen_server:start(?MODULE, shard, []),
    {ok, DstShard} = gen_server:start(?MODULE, shard, []),
    Index = guild_manager_shards:select_shard(?GUILD_ID, 1),
    ets:insert(?SHARD_TABLE, [{shard_count, 1}, {{shard_pid, Index}, DstShard}]),
    {ok, Src} = gen_server:call(
        SrcShard, {start_transferred, ?GUILD_ID, guild_export([a])}
    ),
    #{src => Src, src_shard => SrcShard, dst_shard => DstShard}.

cleanup(#{src_shard := SrcShard, dst_shard := DstShard}) ->
    Guilds = lists:append([gen_server:call(S, all_guilds) || S <- [SrcShard, DstShard]]),
    [exit(G, kill) || G <- Guilds],
    [gen_server:stop(S) || S <- [SrcShard, DstShard]],
    ets:delete(?SHARD_TABLE),
    ets:delete(?SINK),
    flush().

guild_export(Log) ->
    #{
        id => ?GUILD_ID,
        data => #{<<"log">> => Log},
        sessions => #{},
        voice_states => #{}
    }.

init(shard) ->
    {ok, #{role => shard, guilds => #{}}};
init({guild, Export}) ->
    process_flag(trap_exit, true),
    {ok, Export#{role => guild}}.

handle_call({start_transferred, GuildId, Export}, _From, #{role := shard} = State) ->
    {ok, Pid} = gen_server:start(?MODULE, {guild, Export}, []),
    {reply, {ok, Pid}, put_guild(GuildId, Pid, State)};
handle_call({stop_guild, GuildId}, From, #{role := shard} = State) ->
    handle_call({stop_guild, GuildId, normal}, From, State);
handle_call({stop_guild, GuildId, Reason}, _From, #{role := shard, guilds := Guilds} = State) ->
    case maps:get(GuildId, Guilds, undefined) of
        Pid when is_pid(Pid) -> stop_quietly(Pid, Reason);
        undefined -> ok
    end,
    {reply, ok, State#{guilds => maps:remove(GuildId, Guilds)}};
handle_call({lookup, GuildId}, _From, #{role := shard, guilds := Guilds} = State) ->
    case maps:get(GuildId, Guilds, undefined) of
        Pid when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true -> {reply, {ok, Pid}, State};
                false -> {reply, {error, not_found}, State}
            end;
        undefined ->
            {reply, {error, not_found}, State}
    end;
handle_call(all_guilds, _From, #{role := shard, guilds := Guilds} = State) ->
    {reply, maps:values(Guilds), State};
handle_call({get_guild_id}, _From, #{role := guild, id := Id} = State) ->
    {reply, Id, State};
handle_call({put_session, Id, Session}, _From, #{role := guild, sessions := Sessions} = State) ->
    {reply, ok, State#{sessions => Sessions#{Id => Session}}};
handle_call(flush_down_on_terminate, _From, #{role := guild} = State) ->
    Helper = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    {reply, ok, State#{flush_down => {Helper, erlang:monitor(process, Helper)}}};
handle_call(cast_late_on_terminate, _From, #{role := guild} = State) ->
    {reply, ok, State#{cast_late => true}};
handle_call(install_threads, _From, #{role := guild, data := Data} = State) ->
    Tab = guild_thread_store:new(),
    ok = guild_thread_store:put_thread(Tab, #{<<"id">> => 10, <<"parent_id">> => 1}),
    {reply, ok, State#{
        data => Data#{thread_store => Tab, thread_gate => #{active => true, version => 1}},
        thread_load => #{status => ready, queue => [], queued => 0, attempt => 0},
        thread_resend_pending => #{<<"s1">> => true},
        thread_sync_pending => #{<<"s1">> => scheduled},
        thread_list_dirty => #{10 => true}
    }};
handle_call(get_export, _From, #{role := guild} = State) ->
    {reply, maps:remove(role, State), State};
handle_call(get_sessions, _From, #{role := guild, sessions := Sessions} = State) ->
    {reply, Sessions, State};
handle_call(get_log, _From, #{role := guild} = State) ->
    {reply, log(State), State};
handle_call({append_call, Item}, _From, #{role := guild} = State) ->
    {reply, {ok, self()}, append(Item, State)}.

handle_cast({append, Item}, #{role := guild} = State) ->
    {noreply, append(Item, State)};
handle_cast(crash, #{role := guild} = State) ->
    {stop, crashed, State}.

handle_info({'EXIT', _Pid, Reason}, #{role := guild} = State) ->
    {stop, Reason, State};
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(Reason, #{role := guild} = State) ->
    ets:insert(?SINK, {self(), Reason, log(State)}),
    case maps:get(cast_late, State, false) of
        true ->
            gen_server:cast(self(), {append, late}),
            timer:sleep(200);
        false ->
            ok
    end,
    case maps:get(flush_down, State, undefined) of
        {Helper, Ref} ->
            exit(Helper, kill),
            timer:sleep(100),
            erlang:demonitor(Ref, [flush]),
            gen_server:cast(self(), {append, late}),
            timer:sleep(200);
        undefined ->
            ok
    end;
terminate(_Reason, _State) ->
    ok.

put_guild(GuildId, Pid, #{guilds := Guilds} = State) ->
    State#{guilds => Guilds#{GuildId => Pid}}.

log(#{data := Data}) ->
    maps:get(<<"log">>, Data).

append(Item, #{data := Data} = State) ->
    State#{data => Data#{<<"log">> => log(State) ++ [Item]}}.

call_or_exit(Pid, Request) ->
    try
        gen_server:call(Pid, Request, 30000)
    catch
        exit:Reason -> {exit, Reason}
    end.

stop_quietly(Pid, Reason) ->
    try
        gen_server:stop(Pid, Reason, 5000)
    catch
        exit:_ -> ok
    end.

wait_mailbox(Pid, N) ->
    case process_info(Pid, message_queue_len) of
        {message_queue_len, Len} when Len >= N ->
            ok;
        _ ->
            timer:sleep(5),
            wait_mailbox(Pid, N)
    end.

wait_dead(Pid) ->
    Ref = erlang:monitor(process, Pid),
    receive
        {'DOWN', Ref, process, Pid, _} -> ok
    after 5000 -> still_alive
    end.

receive_tagged(Tag) ->
    receive_tagged(Tag, 10000).

receive_tagged(Tag, Timeout) ->
    receive
        Msg when element(1, Msg) =:= Tag -> Msg;
        Tag -> Tag
    after Timeout -> {timeout, Tag}
    end.

flush() ->
    receive
        _ -> flush()
    after 0 -> ok
    end.
