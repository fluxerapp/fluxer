%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(metrics_handler).
-typing([eqwalizer]).

-export([init/2]).

-spec init(cowboy_req:req(), term()) -> {ok, cowboy_req:req(), term()}.
init(Req0, State) ->
    case is_loopback_request(Req0) of
        false ->
            Req = cowboy_req:reply(
                403,
                #{<<"content-type">> => <<"text/plain">>},
                <<"FORBIDDEN">>,
                Req0
            ),
            {ok, Req, State};
        true ->
            Body = render_metrics(),
            Req = cowboy_req:reply(
                200,
                #{<<"content-type">> => <<"text/plain; version=0.0.4; charset=utf-8">>},
                Body,
                Req0
            ),
            {ok, Req, State}
    end.

-spec is_loopback_request(cowboy_req:req()) -> boolean().
is_loopback_request(Req) ->
    case cowboy_req:peer(Req) of
        {{127, 0, 0, 1}, _Port} ->
            true;
        {{0, 0, 0, 0, 0, 0, 0, 1}, _Port} ->
            true;
        _ ->
            false
    end.

-spec render_metrics() -> iolist().
render_metrics() ->
    [
        render_gateway_gauges(),
        render_cluster_counters(),
        render_process_counts(),
        render_push_outbox_stats(safe_apply_map(fun push_outbox:stats/0)),
        render_vm_metrics()
    ].

-spec render_gateway_gauges() -> iolist().
render_gateway_gauges() ->
    Sessions = safe_apply_int(fun session_manager:session_count/0),
    Guilds = safe_apply_int(fun guild_manager:local_guild_count/0),
    VoiceCounts = safe_apply_map(fun voice_state_counts_cache:get_local_counts/0),
    CallIds = safe_apply_list(fun call_manager:local_call_ids/0),
    PushActive = safe_apply_int(fun push_worker_pool:active_count/0),
    ConcurrentSessions = safe_apply_int(fun gateway_concurrency:session_start_count/0),
    ConcurrentGuilds = safe_apply_int(fun gateway_concurrency:guild_start_count/0),
    [
        format_metric(
            <<"fluxer_gateway_sessions_total">>,
            <<"gauge">>,
            <<"Connected WebSocket sessions">>,
            integer_to_binary(Sessions)
        ),
        format_metric(
            <<"fluxer_gateway_guilds_total">>,
            <<"gauge">>,
            <<"Locally loaded guilds">>,
            integer_to_binary(Guilds)
        ),
        render_voice_metrics(VoiceCounts),
        format_metric(
            <<"fluxer_gateway_calls_total">>,
            <<"gauge">>,
            <<"Active voice calls">>,
            integer_to_binary(length(CallIds))
        ),
        format_metric(
            <<"fluxer_gateway_push_workers_active">>,
            <<"gauge">>,
            <<"Active push notification workers">>,
            integer_to_binary(PushActive)
        ),
        format_metric(
            <<"fluxer_gateway_concurrent_session_starts">>,
            <<"gauge">>,
            <<"In-flight session start operations">>,
            integer_to_binary(ConcurrentSessions)
        ),
        format_metric(
            <<"fluxer_gateway_concurrent_guild_starts">>,
            <<"gauge">>,
            <<"In-flight guild start operations">>,
            integer_to_binary(ConcurrentGuilds)
        )
    ].

-spec safe_apply_int(fun(() -> term())) -> integer().
safe_apply_int(Fun) ->
    case shard_utils:safe_apply(Fun, 0) of
        N when is_integer(N) -> N;
        _ -> 0
    end.

-spec safe_apply_map(fun(() -> term())) -> map().
safe_apply_map(Fun) ->
    case shard_utils:safe_apply(Fun, #{}) of
        M when is_map(M) -> M;
        _ -> #{}
    end.

-spec safe_apply_list(fun(() -> term())) -> list().
safe_apply_list(Fun) ->
    case shard_utils:safe_apply(Fun, []) of
        L when is_list(L) -> L;
        _ -> []
    end.

-spec render_voice_metrics(map()) -> iolist().
render_voice_metrics(Counts) when map_size(Counts) =:= 0 ->
    format_metric(
        <<"fluxer_gateway_voice_connections_total">>,
        <<"gauge">>,
        <<"Total voice connections">>,
        <<"0">>
    );
render_voice_metrics(Counts) ->
    Total = maps:get(<<"total_voice_states">>, Counts, 0),
    Regions = maps:get(<<"regions">>, Counts, []),
    Servers = maps:get(<<"servers">>, Counts, []),
    [
        format_metric(
            <<"fluxer_gateway_voice_connections_total">>,
            <<"gauge">>,
            <<"Total voice connections">>,
            integer_to_binary(Total)
        ),
        format_labeled_series(
            <<"fluxer_gateway_voice_connections">>,
            <<"gauge">>,
            <<"Voice connections by dimension">>,
            render_voice_region_labels(Regions) ++ render_voice_server_labels(Servers)
        )
    ].

-spec render_voice_region_labels([map()]) -> [{binary(), binary()}].
render_voice_region_labels(Regions) ->
    [
        {
            <<"region=\"", (maps:get(<<"region_id">>, R, <<>>))/binary, "\"">>,
            integer_to_binary(maps:get(<<"voice_state_count">>, R, 0))
        }
     || R <- Regions, is_map(R)
    ].

-spec render_voice_server_labels([map()]) -> [{binary(), binary()}].
render_voice_server_labels(Servers) ->
    [
        {
            <<"server=\"", (maps:get(<<"server_id">>, S, <<>>))/binary, "\"">>,
            integer_to_binary(maps:get(<<"voice_state_count">>, S, 0))
        }
     || S <- Servers, is_map(S)
    ].

-spec render_cluster_counters() -> iolist().
render_cluster_counters() ->
    Snapshot = safe_apply_map(fun gateway_cluster_metrics:snapshot/0),
    case map_size(Snapshot) of
        0 ->
            [];
        _ ->
            MemberCount = maps:get(<<"gateway_cluster_member_count">>, Snapshot, 0),
            DiscoveryFailures = maps:get(
                <<"gateway_cluster_discovery_resolve_failures_total">>, Snapshot, 0
            ),
            MembershipTransitions = maps:get(
                <<"gateway_cluster_membership_transitions_total">>, Snapshot, #{}
            ),
            OwnerResolutions = maps:get(
                <<"gateway_node_router_owner_resolutions_total">>, Snapshot, #{}
            ),
            MembershipUp = maps:get(<<"up">>, MembershipTransitions, 0),
            MembershipDown = maps:get(<<"down">>, MembershipTransitions, 0),
            OwnerSelf = maps:get(<<"self">>, OwnerResolutions, 0),
            OwnerPeer = maps:get(<<"peer">>, OwnerResolutions, 0),
            [
                format_metric(
                    <<"fluxer_gateway_cluster_member_count">>,
                    <<"gauge">>,
                    <<"Alive cluster members">>,
                    integer_to_binary(MemberCount)
                ),
                format_metric(
                    <<"fluxer_gateway_cluster_discovery_resolve_failures_total">>,
                    <<"counter">>,
                    <<"DNS discovery resolve failures">>,
                    integer_to_binary(DiscoveryFailures)
                ),
                format_labeled_series(
                    <<"fluxer_gateway_cluster_membership_transitions_total">>,
                    <<"counter">>,
                    <<"Cluster membership transitions">>,
                    [
                        {<<"direction=\"up\"">>, integer_to_binary(MembershipUp)},
                        {<<"direction=\"down\"">>, integer_to_binary(MembershipDown)}
                    ]
                ),
                format_labeled_series(
                    <<"fluxer_gateway_cluster_owner_resolutions_total">>,
                    <<"counter">>,
                    <<"Owner resolution outcomes">>,
                    [
                        {<<"result=\"self\"">>, integer_to_binary(OwnerSelf)},
                        {<<"result=\"peer\"">>, integer_to_binary(OwnerPeer)}
                    ]
                )
            ]
    end.

-spec render_process_counts() -> iolist().
render_process_counts() ->
    Prefixes = [guild, session, presence, call, voice],
    Lines = lists:filtermap(fun render_process_count_line/1, Prefixes),
    format_labeled_series(
        <<"fluxer_gateway_processes">>,
        <<"gauge">>,
        <<"Registered processes by type">>,
        Lines
    ).

-spec render_process_count_line(atom()) -> {true, {binary(), binary()}} | false.
render_process_count_line(Prefix) ->
    Count = safe_apply_int(fun() -> count_registry_prefix(Prefix) end),
    PrefixBin = atom_to_binary(Prefix, utf8),
    {true, {<<"type=\"", PrefixBin/binary, "\"">>, integer_to_binary(Count)}}.

-spec count_registry_prefix(atom()) -> non_neg_integer().
count_registry_prefix(Prefix) ->
    try
        ets:foldl(
            fun
                ({{P, _Id}, Pid}, Acc) when P =:= Prefix, is_pid(Pid) -> Acc + 1;
                (_, Acc) -> Acc
            end,
            0,
            process_registry_table
        )
    catch
        error:badarg -> 0
    end.

-spec render_push_outbox_stats(map()) -> iolist().
render_push_outbox_stats(Stats) when map_size(Stats) =:= 0 ->
    [];
render_push_outbox_stats(Stats) ->
    [render_push_outbox_queue_stats(Stats), render_push_outbox_dropped(Stats)].

-spec render_push_outbox_queue_stats(map()) -> iolist().
render_push_outbox_queue_stats(Stats) ->
    [
        format_metric(
            <<"fluxer_gateway_push_outbox_depth">>,
            <<"gauge">>,
            <<"Push jobs queued in the outbox">>,
            gate_counter(depth, Stats)
        ),
        format_metric(
            <<"fluxer_gateway_push_outbox_inflight">>,
            <<"gauge">>,
            <<"Push job requests awaiting a reply">>,
            gate_counter(inflight, Stats)
        ),
        format_metric(
            <<"fluxer_gateway_push_outbox_delivered_total">>,
            <<"counter">>,
            <<"Push jobs acknowledged by the push service">>,
            gate_counter(delivered, Stats)
        ),
        format_metric(
            <<"fluxer_gateway_push_outbox_retries_total">>,
            <<"counter">>,
            <<"Push job requests scheduled for retry">>,
            gate_counter(retries, Stats)
        ),
        format_metric(
            <<"fluxer_gateway_push_outbox_sheds_total">>,
            <<"counter">>,
            <<"Earliest queued push jobs shed at outbox capacity">>,
            gate_counter(sheds, Stats)
        ),
        format_metric(
            <<"fluxer_gateway_push_outbox_truncations_total">>,
            <<"counter">>,
            <<"Queued recipients dropped because they read the channel">>,
            gate_counter(truncations, Stats)
        ),
        format_metric(
            <<"fluxer_gateway_push_outbox_skipped_active_total">>,
            <<"counter">>,
            <<"Queued recipients skipped because they became active">>,
            gate_counter(skipped_active, Stats)
        )
    ].

-spec render_push_outbox_dropped(map()) -> iolist().
render_push_outbox_dropped(Stats) ->
    format_labeled_series(
        <<"fluxer_gateway_push_outbox_dropped_total">>,
        <<"counter">>,
        <<"Push jobs dropped undelivered by kind and reason">>,
        [
            {push_outbox_dropped_label(Kind, Reason), integer_to_binary(Count)}
         || {{Kind, Reason}, Count} <- lists:sort(maps:to_list(maps:get(dropped, Stats, #{}))),
            is_atom(Kind),
            is_atom(Reason),
            is_integer(Count)
        ]
    ).

-spec push_outbox_dropped_label(atom(), atom()) -> binary().
push_outbox_dropped_label(Kind, Reason) ->
    <<"kind=\"", (atom_to_binary(Kind))/binary, "\",reason=\"", (atom_to_binary(Reason))/binary,
        "\"">>.

-spec gate_counter(atom(), map()) -> binary().
gate_counter(Key, Counters) ->
    integer_to_binary(maps:get(Key, Counters, 0)).

-spec render_vm_metrics() -> iolist().
render_vm_metrics() ->
    Memory = safe_apply_list(fun erlang:memory/0),
    ProcessCount = safe_apply_int(fun() -> erlang:system_info(process_count) end),
    PortCount = safe_apply_int(fun() -> erlang:system_info(port_count) end),
    AtomCount = safe_apply_int(fun() -> erlang:system_info(atom_count) end),
    SchedulerCount = safe_apply_int(fun() -> erlang:system_info(schedulers_online) end),
    [
        render_memory_metrics(Memory),
        format_metric(
            <<"erlang_vm_process_count">>,
            <<"gauge">>,
            <<"Erlang VM process count">>,
            integer_to_binary(ProcessCount)
        ),
        format_metric(
            <<"erlang_vm_port_count">>,
            <<"gauge">>,
            <<"Erlang VM port count">>,
            integer_to_binary(PortCount)
        ),
        format_metric(
            <<"erlang_vm_atom_count">>,
            <<"gauge">>,
            <<"Erlang VM atom count">>,
            integer_to_binary(AtomCount)
        ),
        format_metric(
            <<"erlang_vm_scheduler_count">>,
            <<"gauge">>,
            <<"Erlang VM online schedulers">>,
            integer_to_binary(SchedulerCount)
        )
    ].

-spec render_memory_metrics(list()) -> iolist().
render_memory_metrics([]) ->
    [];
render_memory_metrics(Memory) ->
    Types = [total, processes, binary, ets, atom],
    Lines = [
        {
            <<"type=\"", (atom_to_binary(Type, utf8))/binary, "\"">>,
            integer_to_binary(proplists:get_value(Type, Memory, 0))
        }
     || Type <- Types
    ],
    format_labeled_series(
        <<"erlang_vm_memory_bytes">>,
        <<"gauge">>,
        <<"Erlang VM memory usage in bytes">>,
        Lines
    ).

-spec format_metric(binary(), binary(), binary(), binary()) -> iolist().
format_metric(Name, Type, Help, Value) ->
    [
        <<"# HELP ">>,
        Name,
        <<" ">>,
        Help,
        <<"\n">>,
        <<"# TYPE ">>,
        Name,
        <<" ">>,
        Type,
        <<"\n">>,
        Name,
        <<" ">>,
        Value,
        <<"\n">>
    ].

-spec format_labeled_series(binary(), binary(), binary(), [{binary(), binary()}]) -> iolist().
format_labeled_series(_Name, _Type, _Help, []) ->
    [];
format_labeled_series(Name, Type, Help, LabelValues) ->
    [
        <<"# HELP ">>,
        Name,
        <<" ">>,
        Help,
        <<"\n">>,
        <<"# TYPE ">>,
        Name,
        <<" ">>,
        Type,
        <<"\n">>,
        [
            [Name, <<"{">>, Label, <<"} ">>, Value, <<"\n">>]
         || {Label, Value} <- LabelValues
        ]
    ].

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

push_outbox_drops_render_one_series_per_kind_and_reason_test() ->
    Rendered = iolist_to_binary(
        render_push_outbox_dropped(#{
            dropped => #{{message, expired} => 3, {clear, outbox_unavailable} => 1}
        })
    ),
    ?assertNotEqual(
        nomatch,
        binary:match(
            Rendered,
            <<"fluxer_gateway_push_outbox_dropped_total{kind=\"message\",reason=\"expired\"} 3\n">>
        )
    ),
    ?assertNotEqual(
        nomatch,
        binary:match(
            Rendered,
            <<"fluxer_gateway_push_outbox_dropped_total{kind=\"clear\",reason=\"outbox_unavailable\"} 1\n">>
        )
    ).

push_outbox_without_drops_renders_no_dropped_series_test() ->
    ?assertEqual([], render_push_outbox_dropped(#{dropped => #{}})).

-endif.
