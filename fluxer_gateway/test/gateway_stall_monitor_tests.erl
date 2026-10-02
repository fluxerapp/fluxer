%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_stall_monitor_tests).

-include_lib("eunit/include/eunit.hrl").

stall_monitor_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun installs_and_releases_system_monitor/1,
        fun counts_forwarded_events_and_names_the_process/1,
        fun leaves_a_foreign_system_monitor_alone/1,
        fun records_late_timer_ticks/1
    ]}.

setup() ->
    _ = erlang:system_monitor(undefined),
    application:unset_env(fluxer_gateway, stall_monitor_enabled),
    ok.

cleanup(_) ->
    case whereis(gateway_stall_monitor) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid)
    end,
    _ = erlang:system_monitor(undefined),
    ok.

installs_and_releases_system_monitor(_) ->
    fun() ->
        {ok, Pid} = gateway_stall_monitor:start_link(),
        unlink(Pid),
        {Pid, Opts} = erlang:system_monitor(),
        ?assertEqual(200, proplists:get_value(long_schedule, Opts)),
        ?assertEqual(200, proplists:get_value(long_gc, Opts)),
        ?assertMatch(#{status := installed}, gateway_stall_monitor:snapshot()),
        ok = gen_server:stop(Pid),
        ?assertEqual(undefined, erlang:system_monitor())
    end.

counts_forwarded_events_and_names_the_process(_) ->
    fun() ->
        {ok, Pid} = gateway_stall_monitor:start_link(),
        unlink(Pid),
        Busy = spawn(fun() ->
            receive
                stop -> ok
            end
        end),
        Info = [{timeout, 1500}, {in, {guild, handle_info, 2}}, {out, {guild, handle_info, 2}}],
        Pid ! {monitor, Busy, long_schedule, Info},
        Pid ! {monitor, Busy, long_gc, [{timeout, 300}, {heap_size, 10}, {old_heap_size, 0}]},
        #{stats := Stats, recent := Recent} = gateway_stall_monitor:snapshot(),
        ?assertEqual(
            #{count => 1, severe => 1, max_ms => 1500}, maps:get(long_schedule, Stats)
        ),
        ?assertEqual(#{count => 1, severe => 0, max_ms => 300}, maps:get(long_gc, Stats)),
        [GcEvent, SchedEvent] = Recent,
        ?assertMatch(#{kind := long_gc, ms := 300, info := #{heap_size := 10}}, GcEvent),
        ?assertMatch(
            #{kind := long_schedule, who := Busy, info := #{in := {guild, handle_info, 2}}},
            SchedEvent
        ),
        ?assertMatch(#{who_info := #{guild_id := undefined}}, SchedEvent),
        Busy ! stop
    end.

leaves_a_foreign_system_monitor_alone(_) ->
    fun() ->
        Foreign = spawn(fun() ->
            receive
                stop -> ok
            end
        end),
        _ = erlang:system_monitor(Foreign, [{long_gc, 500}]),
        {ok, Pid} = gateway_stall_monitor:start_link(),
        unlink(Pid),
        ?assertMatch(#{status := not_owner}, gateway_stall_monitor:snapshot()),
        ok = gen_server:stop(Pid),
        ?assertMatch({Foreign, _}, erlang:system_monitor()),
        Foreign ! stop
    end.

records_late_timer_ticks(_) ->
    fun() ->
        {ok, Pid} = gateway_stall_monitor:start_link(),
        unlink(Pid),
        _ = sys:replace_state(Pid, fun(S) -> S#{tick_due := maps:get(tick_due, S) - 1300} end),
        Pid ! tick,
        #{stats := Stats} = gateway_stall_monitor:snapshot(),
        #{count := Count, max_ms := Max} = maps:get(timer_late, Stats),
        ?assert(Count >= 1),
        ?assert(Max >= 1200)
    end.
