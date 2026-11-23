// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/api/metrics as api_metrics
import fluxer_gateway/cluster/dist_metrics
import fluxer_gateway/cluster/metrics as cluster_metrics
import fluxer_gateway/gateway/socket_metrics
import fluxer_gateway/guild/metrics as guild_metrics
import fluxer_gateway/nats/metrics as nats_metrics
import fluxer_gateway/presence/counters as presence_counters
import fluxer_gateway/rpc/metrics as rpc_metrics
import fluxer_gateway/session/session_metrics
import fluxer_gateway/telemetry/metrics.{
  type HistogramSeries, type Kind, type Sample, Counter, Gauge, Histogram,
}
import fluxer_gateway/telemetry/native_metrics
import fluxer_gateway/telemetry/process_registry
import fluxer_gateway/telemetry/resident
import fluxer_gateway/telemetry/vm
import fluxer_gateway/telemetry/watchdog
import fluxer_gateway/voice/metrics as voice_metrics
import gleam/int
import gleam/list
import gleam/result
import gleam/set.{type Set}
import gleam/string
import gleam/string_tree.{type StringTree}

type Shape {
  Plain
  Fixed(label: String, values: List(String))
  Open
}

type Spec {
  Spec(name: String, kind: Kind, help: String, shape: Shape)
}

pub fn current_samples() -> List(Sample) {
  list.append(process_registry.samples(), metrics.read_all())
}

pub fn render() -> String {
  render_without(set.new())
}

pub fn render_without(taken: Set(String)) -> String {
  let kinds = metrics.registry_samples()
  let samples = current_samples()
  let specs = catalogue(samples)
  let known = list.map(specs, fn(spec) { spec.name }) |> set.from_list
  [
    list.map(specs, fn(spec) { #(spec.name, render_spec(spec, samples)) }),
    render_vm(),
    render_runtime(),
    list.map(native_metrics.samples(), fn(sample) {
      #(sample.name, render_native(sample))
    }),
    render_others(samples, kinds, known),
    render_histograms(metrics.histograms()),
  ]
  |> list.flatten
  |> list.filter_map(fn(entry) {
    let #(name, text) = entry
    case set.contains(taken, name) {
      True -> Error(Nil)
      False -> Ok(text)
    }
  })
  |> string_tree.concat
  |> string_tree.to_string
}

fn catalogue(samples: List(Sample)) -> List(Spec) {
  let push_outbox = case
    list.any(samples, fn(sample) {
      string.starts_with(sample.name, "fluxer_gateway_push_outbox_")
    })
  {
    True -> push_outbox_specs()
    False -> []
  }
  let sockets = case sampled(samples, socket_metrics.connections) {
    True -> socket_specs()
    False -> []
  }
  let sessions = case sampled(samples, session_metrics.detached) {
    True -> session_specs()
    False -> []
  }
  let guild = case sampled(samples, guild_metrics.members_held) {
    True -> guild_specs()
    False -> []
  }
  list.flatten([
    gateway_specs(),
    cluster_specs(),
    [
      Spec(
        "fluxer_gateway_processes",
        Gauge,
        "Registered processes by type",
        Fixed(
          "type",
          list.map(process_registry.kinds, process_registry.kind_name),
        ),
      ),
    ],
    channel_threads_specs(),
    visitor_specs(),
    guild,
    outbound_specs(),
    push_outbox,
    sockets,
    sessions,
    present(presence_specs(), samples),
    present(voice_specs(), samples),
  ])
}

fn sampled(samples: List(Sample), name: String) -> Bool {
  list.any(samples, fn(sample) { sample.name == name })
}

fn socket_specs() -> List(Spec) {
  let states = socket_metrics.connection_states()
  list.flatten([
    list.map(socket_metrics.counter_families(), fn(entry) {
      let #(family, _) = entry
      Spec(
        family.name,
        Counter,
        family.help,
        Fixed(family.label, family.values),
      )
    }),
    [
      Spec(
        socket_metrics.send_failures,
        Counter,
        "Frames the transport refused to send, such as on a send timeout",
        Plain,
      ),
      Spec(states.name, Gauge, states.help, Fixed(states.label, states.values)),
      Spec(
        socket_metrics.process_memory,
        Gauge,
        "Memory held by WebSocket connection processes in bytes",
        Plain,
      ),
      Spec(
        socket_metrics.mailbox_messages,
        Gauge,
        "Messages waiting in WebSocket connection mailboxes",
        Plain,
      ),
      Spec(
        socket_metrics.mailbox_max_messages,
        Gauge,
        "Longest WebSocket connection mailbox",
        Plain,
      ),
    ],
  ])
}

fn session_specs() -> List(Spec) {
  let paths = Fixed("path", session_metrics.replay_paths)
  [
    Spec(
      session_metrics.detached,
      Gauge,
      "Sessions on this node waiting for a socket to resume",
      Plain,
    ),
    Spec(
      session_metrics.process_memory,
      Gauge,
      "Memory held by session processes on this node in bytes",
      Plain,
    ),
    Spec(
      session_metrics.mailbox_messages,
      Gauge,
      "Messages waiting in session process mailboxes",
      Plain,
    ),
    Spec(
      session_metrics.mailbox_max_messages,
      Gauge,
      "Longest session process mailbox",
      Plain,
    ),
    Spec(
      session_metrics.replays,
      Counter,
      "Replays of buffered dispatches to a socket by path",
      paths,
    ),
    Spec(
      session_metrics.replayed_dispatches,
      Counter,
      "Buffered dispatches replayed to a socket by path",
      paths,
    ),
  ]
}

fn present(specs: List(Spec), samples: List(Sample)) -> List(Spec) {
  let names = list.map(samples, fn(sample) { sample.name }) |> set.from_list
  list.filter(specs, fn(spec) { set.contains(names, spec.name) })
}

fn presence_specs() -> List(Spec) {
  [
    Spec(
      presence_counters.inputs_metric,
      Counter,
      "Inputs handled by local presence processes by kind",
      Fixed("kind", presence_counters.input_kinds),
    ),
    Spec(
      presence_counters.published_metric,
      Counter,
      "Presence events published by local presence processes",
      Plain,
    ),
    Spec(
      presence_counters.deliveries_metric,
      Counter,
      "Presence event deliveries by target",
      Fixed("target", presence_counters.delivery_targets),
    ),
  ]
}

fn voice_specs() -> List(Spec) {
  [
    Spec(
      voice_metrics.transitions_metric,
      Counter,
      "Voice state joins, leaves, moves and updates",
      Open,
    ),
    Spec(
      voice_metrics.guild_requests_metric,
      Counter,
      "Guild voice requests handled by kind",
      Fixed("request", voice_metrics.guild_requests),
    ),
    Spec(
      voice_metrics.server_updates_metric,
      Counter,
      "VOICE_SERVER_UPDATE dispatches sent",
      Fixed("scope", voice_metrics.scopes),
    ),
    Spec(
      voice_metrics.guild_errors_metric,
      Counter,
      "Guild voice errors sent to sessions",
      Plain,
    ),
    Spec(
      voice_metrics.calls_started_metric,
      Counter,
      "Calls started on this node",
      Plain,
    ),
    Spec(
      voice_metrics.calls_ended_metric,
      Counter,
      "Calls ended on this node",
      Plain,
    ),
    Spec(
      voice_metrics.token_results_metric,
      Counter,
      "Voice token fetches finished by job and outcome",
      Open,
    ),
  ]
}

fn gateway_specs() -> List(Spec) {
  [
    Spec(
      "fluxer_gateway_sessions_total",
      Gauge,
      "Connected WebSocket sessions",
      Plain,
    ),
    Spec(
      "fluxer_gateway_session_resumes_total",
      Counter,
      "Successful WebSocket session resumes",
      Plain,
    ),
    Spec("fluxer_gateway_guilds_total", Gauge, "Locally loaded guilds", Plain),
    Spec(
      "fluxer_gateway_websocket_dispatches_total",
      Counter,
      "WebSocket dispatch frames handed to the transport, including replays",
      Plain,
    ),
    Spec(
      "fluxer_gateway_websocket_dispatch_drops_total",
      Counter,
      "WebSocket dispatches dropped due to encoding or compression errors",
      Plain,
    ),
    Spec(
      "fluxer_gateway_voice_connections_total",
      Gauge,
      "Total voice connections",
      Plain,
    ),
    Spec(
      "fluxer_gateway_voice_connections",
      Gauge,
      "Voice connections by dimension",
      Open,
    ),
    Spec("fluxer_gateway_calls_total", Gauge, "Active voice calls", Plain),
    Spec(
      "fluxer_gateway_push_workers_active",
      Gauge,
      "Active push notification workers",
      Plain,
    ),
    Spec(
      "fluxer_gateway_concurrent_session_starts",
      Gauge,
      "In-flight session start operations",
      Plain,
    ),
    Spec(
      "fluxer_gateway_queued_session_starts",
      Gauge,
      "Session starts waiting for admission",
      Plain,
    ),
    Spec(
      "fluxer_gateway_concurrent_guild_starts",
      Gauge,
      "In-flight guild start operations",
      Plain,
    ),
  ]
}

fn cluster_specs() -> List(Spec) {
  [
    Spec(
      "fluxer_gateway_cluster_member_count",
      Gauge,
      "Alive cluster members",
      Plain,
    ),
    Spec(
      "fluxer_gateway_cluster_discovery_resolve_failures_total",
      Counter,
      "DNS discovery resolve failures",
      Plain,
    ),
    Spec(
      "fluxer_gateway_cluster_membership_transitions_total",
      Counter,
      "Cluster membership transitions",
      Fixed("direction", ["up", "down"]),
    ),
    Spec(
      "fluxer_gateway_cluster_owner_resolutions_total",
      Counter,
      "Owner resolution outcomes",
      Fixed("result", ["self", "peer"]),
    ),
    Spec(
      cluster_metrics.beat_acks,
      Counter,
      "Bouncer beat acknowledgements by whether the held digest matched",
      Fixed("outcome", ["matched", "mismatched"]),
    ),
    Spec(
      "fluxer_gateway_watchdog_actions_total",
      Counter,
      "Mailbox watchdog actions taken by action",
      Fixed("action", watchdog.action_names),
    ),
    Spec(
      "fluxer_gateway_watchdog_mailbox_max_messages",
      Gauge,
      "Longest mailbox the watchdog saw on its last check by holder kind",
      Fixed("kind", watchdog.holder_kinds),
    ),
  ]
}

const histogram_help = [
  #(
    "fluxer_gateway_bouncer_beat_rtt_ms",
    "Round trip from a worker beat to its bouncer acknowledgement in milliseconds",
  ),
  #(
    "fluxer_gateway_bouncer_probe_ms",
    "Lease probe round duration by verdict in milliseconds",
  ),
  #(
    "fluxer_gateway_bouncer_close_ms",
    "Bucket close request until the bouncer marks it moving, by outcome, in milliseconds",
  ),
  #(
    "fluxer_gateway_handoff_precopy_duration_ms",
    "Handoff precopy duration by entity kind and outcome in milliseconds",
  ),
  #(
    "fluxer_gateway_scheduler_stall_duration_ms",
    "Scheduler stall duration by kind in milliseconds",
  ),
  #(
    "fluxer_gateway_websocket_identify_ready_ms",
    "IDENTIFY to READY on a WebSocket, including time held, in milliseconds",
  ),
  #(
    "fluxer_gateway_websocket_resume_ms",
    "RESUME to RESUMED on a WebSocket in milliseconds",
  ),
  #(
    "fluxer_gateway_session_move_ms",
    "Session move duration by outcome in milliseconds",
  ),
  #(
    "fluxer_gateway_guild_load_ms",
    "Guild load from the API by outcome in milliseconds",
  ),
  #(
    "fluxer_gateway_guild_dispatch_ms",
    "Time to handle one guild event by event family in milliseconds",
  ),
  #(
    "fluxer_gateway_guild_dispatch_fanout_sessions",
    "Sessions each guild event was delivered to",
  ),
  #(
    "fluxer_gateway_guild_member_list_ms",
    "Member list subscribe handling time in milliseconds",
  ),
  #(
    "fluxer_gateway_guild_lazy_request_ms",
    "Lazy request handling time in the guild process in milliseconds",
  ),
  #(
    "fluxer_gateway_guild_join_wait_ms",
    "Time a join waited at the join gate by outcome in milliseconds",
  ),
  #(
    "fluxer_gateway_guild_join_batch_ms",
    "Join gate batch handling time in milliseconds",
  ),
  #(
    "fluxer_gateway_presence_start_ms",
    "Presence start or lookup on session connect by outcome in milliseconds",
  ),
  #(
    "fluxer_gateway_presence_lookup_ms",
    "Presence registry lookup by outcome in milliseconds",
  ),
  #(
    "fluxer_gateway_voice_token_turnaround_ms",
    "Voice join request to token by job in milliseconds",
  ),
  #(
    "fluxer_gateway_voice_call_create_ms",
    "Call creation by outcome in milliseconds",
  ),
  #(
    "fluxer_gateway_voice_call_lookup_ms",
    "Call lookup by outcome in milliseconds",
  ),
  #(
    "fluxer_gateway_voice_counts_sync_ms",
    "Voice counts rebuild duration in milliseconds",
  ),
  #(
    "fluxer_gateway_api_request_duration_ms",
    "Internal API HTTP attempt duration by request type in milliseconds",
  ),
  #(
    "fluxer_gateway_nats_rpc_duration_ms",
    "NATS RPC handling time by method family in milliseconds",
  ),
  #(
    "fluxer_gateway_push_job_wait_ms",
    "Time a push job waited for a worker in milliseconds",
  ),
  #(
    "fluxer_gateway_push_job_duration_ms",
    "Push worker run time in milliseconds",
  ),
  #(
    "fluxer_gateway_push_outbox_attempt_duration_ms",
    "Push outbox NATS request duration by job kind in milliseconds",
  ),
  #(
    "fluxer_gateway_cluster_down_duration_ms",
    "Time a cluster peer stayed down in milliseconds",
  ),
]

fn channel_threads_specs() -> List(Spec) {
  [
    Spec(
      "fluxer_gateway_thread_store_threads",
      Gauge,
      "Active threads held in guild thread stores on this node",
      Plain,
    ),
    Spec(
      "fluxer_gateway_thread_dispatch_total",
      Counter,
      "Thread-gated events delivered to sessions by event and variant",
      Open,
    ),
    Spec(
      "fluxer_gateway_thread_list_sync_total",
      Counter,
      "THREAD_LIST_SYNC sends by reason",
      Fixed("reason", ["full", "access"]),
    ),
    Spec(
      "fluxer_gateway_thread_collection_fetch_total",
      Counter,
      "Thread collection fetches by result",
      Fixed("result", ["ok", "error", "refetch"]),
    ),
    Spec(
      "fluxer_gateway_thread_permission_unknown_parent_total",
      Counter,
      "Thread permission checks whose parent channel was unknown",
      Plain,
    ),
  ]
}

fn visitor_specs() -> List(Spec) {
  [
    Spec(
      "fluxer_gateway_guild_visitor_sessions",
      Gauge,
      "Live guild visitor sessions on this node",
      Plain,
    ),
    Spec(
      "fluxer_gateway_guild_visitor_admitted_total",
      Counter,
      "Guild visitor sessions admitted",
      Plain,
    ),
    Spec(
      "fluxer_gateway_guild_visitor_converted_total",
      Counter,
      "Guild visitor sessions converted to member sessions",
      Plain,
    ),
    Spec(
      "fluxer_gateway_guild_visitor_rejected_total",
      Counter,
      "Guild visitor connects rejected by reason",
      Fixed("reason", ["disabled", "age_restricted", "unavailable", "busy"]),
    ),
    Spec(
      "fluxer_gateway_guild_visitor_evicted_total",
      Counter,
      "Guild visitor sessions evicted by reason",
      Fixed("reason", [
        "busy", "banned", "disabled", "age_restricted", "removed", "error",
        "unavailable",
      ]),
    ),
  ]
}

fn guild_specs() -> List(Spec) {
  [
    Spec(
      guild_metrics.starts,
      Counter,
      "Guild processes started on this node by origin",
      Fixed("origin", guild_metrics.origins),
    ),
    Spec(
      guild_metrics.start_failures,
      Counter,
      "Guild starts that failed by reason",
      Fixed("reason", guild_metrics.failure_reasons),
    ),
    Spec(
      guild_metrics.crash_restarts,
      Counter,
      "Guild starts that rejoin sessions left by a crashed guild process",
      Plain,
    ),
    Spec(
      guild_metrics.stops,
      Counter,
      "Guild processes stopped by reason",
      Fixed("reason", guild_metrics.stop_reasons),
    ),
    Spec(
      guild_metrics.loading_joins_expired,
      Counter,
      "Joins that expired while their guild was still loading",
      Plain,
    ),
    Spec(
      guild_metrics.events,
      Counter,
      "Guild events handled by event family",
      Open,
    ),
    Spec(
      guild_metrics.deliveries,
      Counter,
      "Session deliveries of guild dispatches by event family",
      Open,
    ),
    Spec(
      guild_metrics.member_list_ops,
      Counter,
      "Member list subscription operations by op",
      Fixed("op", guild_metrics.member_list_op_names),
    ),
    Spec(
      guild_metrics.member_list_ranges,
      Counter,
      "Member list ranges requested in subscriptions",
      Plain,
    ),
    Spec(
      guild_metrics.joins_admitted,
      Counter,
      "Joins admitted by the guild join gate by class",
      Fixed("class", guild_metrics.join_classes),
    ),
    Spec(
      guild_metrics.members_held,
      Gauge,
      "Guild members held by guild processes on this node",
      Plain,
    ),
    Spec(
      guild_metrics.members_online,
      Gauge,
      "Online guild members counted by guild processes on this node",
      Plain,
    ),
    Spec(
      guild_metrics.sessions_held,
      Gauge,
      "Session memberships held by guild processes on this node",
      Plain,
    ),
    Spec(
      guild_metrics.in_mode,
      Gauge,
      "Guild processes outside the serving mode by mode",
      Fixed("mode", guild_metrics.modes),
    ),
  ]
}

fn outbound_specs() -> List(Spec) {
  let direction = Fixed("direction", ["in", "out"])
  [
    Spec(
      api_metrics.requests,
      Counter,
      "Internal API HTTP attempts by request type and outcome",
      Open,
    ),
    Spec(
      api_metrics.retries,
      Counter,
      "Internal API requests retried by request type",
      Open,
    ),
    Spec(
      api_metrics.in_flight,
      Gauge,
      "Internal API HTTP requests in flight",
      Plain,
    ),
    Spec(
      api_metrics.breaker_state,
      Gauge,
      "Internal API circuit breaker state, 0 closed, 1 open, 2 half open",
      Plain,
    ),
    Spec(
      api_metrics.breaker_transitions,
      Counter,
      "Internal API circuit breaker transitions by new state",
      Fixed("state", api_metrics.breaker_states),
    ),
    Spec(
      rpc_metrics.requests,
      Counter,
      "NATS RPC requests handled by method and outcome",
      Open,
    ),
    Spec(rpc_metrics.handlers, Gauge, "NATS RPC handlers running", Plain),
    Spec(
      nats_metrics.connections,
      Gauge,
      "NATS pool connection slots by state",
      Fixed("state", nats_metrics.connection_states),
    ),
    Spec(
      nats_metrics.connection_events,
      Counter,
      "NATS pool connection slot events",
      Fixed("event", nats_metrics.events),
    ),
    Spec(
      nats_metrics.subscriptions,
      Gauge,
      "NATS subscriptions held by the pool",
      Plain,
    ),
    Spec(
      nats_metrics.publishes,
      Counter,
      "NATS publishes by kind and outcome",
      Open,
    ),
    Spec(
      nats_metrics.messages,
      Counter,
      "NATS messages by direction",
      direction,
    ),
    Spec(
      nats_metrics.bytes,
      Counter,
      "NATS payload and wire bytes by direction",
      direction,
    ),
    Spec(
      "fluxer_gateway_push_server_received_total",
      Counter,
      "Push work received by the push server by kind",
      Fixed("kind", ["message_create", "clear"]),
    ),
    Spec(
      "fluxer_gateway_push_pending_jobs",
      Gauge,
      "Message creates waiting for a push worker",
      Plain,
    ),
    Spec(
      "fluxer_gateway_push_outbox_attempt_results_total",
      Counter,
      "Push job NATS requests by job kind and result",
      Open,
    ),
  ]
}

fn push_outbox_specs() -> List(Spec) {
  let by_class = Fixed("class", ["direct", "bulk"])
  [
    Spec(
      "fluxer_gateway_push_outbox_depth",
      Gauge,
      "Push jobs queued in the outbox",
      by_class,
    ),
    Spec(
      "fluxer_gateway_push_outbox_inflight",
      Gauge,
      "Push job requests awaiting a reply",
      by_class,
    ),
    Spec(
      "fluxer_gateway_push_outbox_unknown",
      Gauge,
      "Push job requests whose outcome is unknown",
      by_class,
    ),
    Spec(
      "fluxer_gateway_push_outbox_delivered_total",
      Counter,
      "Push jobs acknowledged by the push service",
      by_class,
    ),
    Spec(
      "fluxer_gateway_push_outbox_retries_total",
      Counter,
      "Push job requests scheduled for retry",
      by_class,
    ),
    Spec(
      "fluxer_gateway_push_outbox_sheds_total",
      Counter,
      "Earliest queued push jobs shed at outbox capacity",
      by_class,
    ),
    Spec(
      "fluxer_gateway_push_outbox_truncations_total",
      Counter,
      "Queued recipients dropped because they read the channel",
      by_class,
    ),
    Spec(
      "fluxer_gateway_push_outbox_skipped_active_total",
      Counter,
      "Queued recipients skipped because they became active",
      by_class,
    ),
    Spec(
      "fluxer_gateway_push_outbox_followup_clears_total",
      Counter,
      "Clears queued after an in-flight push job whose recipient read the channel",
      by_class,
    ),
    Spec(
      "fluxer_gateway_push_outbox_dropped_total",
      Counter,
      "Push jobs dropped undelivered by kind and reason",
      Open,
    ),
  ]
}

fn render_spec(spec: Spec, samples: List(Sample)) -> StringTree {
  case spec.shape {
    Plain ->
      family(spec.name, kind_name(spec.kind), spec.help, [
        #([], plain_value(spec.name, samples)),
      ])
    Fixed(label, values) ->
      family(
        spec.name,
        kind_name(spec.kind),
        spec.help,
        list.map(values, fn(value) {
          let labels = [#(label, value)]
          #(labels, metrics.value(samples, spec.name, labels))
        }),
      )
    Open ->
      family(
        spec.name,
        kind_name(spec.kind),
        spec.help,
        labelled_series(samples, spec.name),
      )
  }
}

fn plain_value(name: String, samples: List(Sample)) -> Int {
  let value = metrics.value(samples, name, [])
  case name {
    "fluxer_gateway_guild_visitor_sessions" -> int.max(0, value)
    _ -> value
  }
}

fn labelled_series(
  samples: List(Sample),
  name: String,
) -> List(#(List(#(String, String)), Int)) {
  samples
  |> list.filter(fn(sample) { sample.name == name && sample.labels != [] })
  |> list.map(fn(sample) { metrics.normalise(sample.labels) })
  |> list.unique
  |> list.map(fn(labels) { #(labels, metrics.value(samples, name, labels)) })
}

fn render_vm() -> List(#(String, StringTree)) {
  let queues = vm.run_queues()
  let wall = vm.scheduler_wall_time()
  let activity = vm.activity()
  let #(terms, term_bytes) = vm.persistent_terms()
  let by_class = fn(read: fn(vm.WallTime) -> Int) {
    list.map(wall, fn(entry) {
      #([#("type", vm.class_name(entry.class))], read(entry))
    })
  }
  let plain = fn(name, kind, help, value) {
    named_family(name, kind, help, [#([], value)])
  }
  [
    named_family(
      "erlang_vm_memory_bytes",
      "gauge",
      "Erlang VM memory usage in bytes",
      list.map(vm.memory_by_type(), fn(entry) {
        #([#("type", entry.0)], entry.1)
      }),
    ),
    plain(
      "erlang_vm_process_count",
      "gauge",
      "Erlang VM process count",
      vm.process_count(),
    ),
    plain(
      "erlang_vm_process_limit",
      "gauge",
      "Erlang VM process limit",
      vm.process_limit(),
    ),
    plain(
      "erlang_vm_port_count",
      "gauge",
      "Erlang VM port count",
      vm.port_count(),
    ),
    plain(
      "erlang_vm_port_limit",
      "gauge",
      "Erlang VM port limit",
      vm.port_limit(),
    ),
    plain(
      "erlang_vm_atom_count",
      "gauge",
      "Erlang VM atom count",
      vm.atom_count(),
    ),
    plain(
      "erlang_vm_atom_limit",
      "gauge",
      "Erlang VM atom limit",
      vm.atom_limit(),
    ),
    plain(
      "erlang_vm_ets_table_count",
      "gauge",
      "Erlang VM ETS tables",
      vm.ets_count(),
    ),
    plain(
      "erlang_vm_ets_table_limit",
      "gauge",
      "Erlang VM ETS table limit",
      vm.ets_limit(),
    ),
    plain(
      "erlang_vm_scheduler_count",
      "gauge",
      "Erlang VM online schedulers",
      vm.schedulers_online(),
    ),
    named_family(
      "erlang_vm_dirty_scheduler_count",
      "gauge",
      "Erlang VM online dirty schedulers by type",
      [
        #([#("type", "dirty_cpu")], vm.dirty_cpu_schedulers_online()),
        #([#("type", "dirty_io")], vm.dirty_io_schedulers()),
      ],
    ),
    named_family(
      "erlang_vm_run_queue_length",
      "gauge",
      "Processes and dirty NIF calls waiting to run by scheduler type",
      [
        #([#("type", "normal")], queues.normal),
        #([#("type", "dirty_cpu")], queues.dirty_cpu),
        #([#("type", "dirty_io")], queues.dirty_io),
      ],
    ),
    named_family(
      "erlang_vm_scheduler_active_time_total",
      "counter",
      "Scheduler busy time by scheduler type, in the same unit as erlang_vm_scheduler_wall_time_total",
      by_class(fn(entry) { entry.active }),
    ),
    named_family(
      "erlang_vm_scheduler_wall_time_total",
      "counter",
      "Scheduler wall time by scheduler type, divide the active time rate by this rate for utilisation",
      by_class(fn(entry) { entry.total }),
    ),
    plain(
      "erlang_vm_reductions_total",
      "counter",
      "Erlang VM reductions",
      activity.reductions,
    ),
    plain(
      "erlang_vm_context_switches_total",
      "counter",
      "Erlang VM context switches",
      activity.context_switches,
    ),
    plain(
      "erlang_vm_gc_runs_total",
      "counter",
      "Erlang VM garbage collections",
      activity.gc_runs,
    ),
    plain(
      "erlang_vm_gc_words_reclaimed_total",
      "counter",
      "Erlang VM words reclaimed by garbage collection",
      activity.gc_words_reclaimed,
    ),
    named_family(
      "erlang_vm_io_bytes_total",
      "counter",
      "Bytes through Erlang VM ports by direction",
      [
        #([#("direction", "in")], activity.io_in_bytes),
        #([#("direction", "out")], activity.io_out_bytes),
      ],
    ),
    plain(
      "erlang_vm_uptime_seconds",
      "gauge",
      "Erlang VM uptime in seconds",
      vm.uptime_seconds(),
    ),
    plain(
      "erlang_vm_persistent_term_count",
      "gauge",
      "Terms in persistent_term storage",
      terms,
    ),
    plain(
      "erlang_vm_persistent_term_bytes",
      "gauge",
      "Memory used by persistent_term storage in bytes",
      term_bytes,
    ),
  ]
}

fn render_runtime() -> List(#(String, StringTree)) {
  let traffic = dist_metrics.traffic()
  let tables = resident.tables()
  let singletons = resident.singletons()
  let per_table = fn(read: fn(resident.Held) -> Int) {
    list.map(tables, fn(held) { #([#("table", held.name)], read(held)) })
  }
  let per_process = fn(read: fn(resident.Singleton) -> Int) {
    list.map(singletons, fn(held) { #([#("process", held.name)], read(held)) })
  }
  [
    named_family(
      "fluxer_gateway_cluster_connected_nodes",
      "gauge",
      "Erlang distribution connections by node visibility",
      [
        #([#("visibility", "visible")], traffic.visible_nodes),
        #([#("visibility", "hidden")], traffic.hidden_nodes),
      ],
    ),
    named_family(
      "fluxer_gateway_cluster_dist_bytes_total",
      "counter",
      "Erlang distribution bytes over all peer connections by direction",
      [
        #([#("direction", "sent")], traffic.sent),
        #([#("direction", "received")], traffic.received),
      ],
    ),
    named_family(
      "fluxer_gateway_cluster_dist_queue_bytes",
      "gauge",
      "Bytes queued on Erlang distribution ports waiting to be sent",
      [#([], traffic.queued)],
    ),
    named_family(
      "fluxer_gateway_ets_table_rows",
      "gauge",
      "Rows held in named gateway ETS tables",
      per_table(fn(held) { held.count }),
    ),
    named_family(
      "fluxer_gateway_ets_table_bytes",
      "gauge",
      "Memory held by named gateway ETS tables in bytes",
      per_table(fn(held) { held.bytes }),
    ),
    named_family(
      "fluxer_gateway_singleton_mailbox_messages",
      "gauge",
      "Messages queued in registered gateway processes sharing a name",
      per_process(fn(held) { held.queued }),
    ),
    named_family(
      "fluxer_gateway_singleton_mailbox_max_messages",
      "gauge",
      "Longest mailbox among registered gateway processes sharing a name",
      per_process(fn(held) { held.longest }),
    ),
    named_family(
      "fluxer_gateway_singleton_memory_bytes",
      "gauge",
      "Memory held by registered gateway processes sharing a name in bytes",
      per_process(fn(held) { held.bytes }),
    ),
  ]
}

fn named_family(
  name: String,
  kind: String,
  help: String,
  series: List(#(List(#(String, String)), Int)),
) -> #(String, StringTree) {
  #(name, family(name, kind, help, series))
}

fn render_native(sample: Sample) -> StringTree {
  family(sample.name, "gauge", "Rust NIF runtime metric " <> sample.name, [
    #([], sample.value),
  ])
}

fn render_others(
  samples: List(Sample),
  kinds: List(#(Kind, Sample)),
  known: Set(String),
) -> List(#(String, StringTree)) {
  samples
  |> list.map(fn(sample) { sample.name })
  |> list.unique
  |> list.filter(fn(name) { !set.contains(known, name) })
  |> list.map(fn(name) {
    let series =
      samples
      |> list.filter(fn(sample) { sample.name == name })
      |> list.map(fn(sample) { metrics.normalise(sample.labels) })
      |> list.unique
      |> list.map(fn(labels) { #(labels, metrics.value(samples, name, labels)) })
    named_family(name, kind_name(registry_kind(kinds, name)), name, series)
  })
}

fn registry_kind(kinds: List(#(Kind, Sample)), name: String) -> Kind {
  let found =
    list.find(kinds, fn(entry) {
      let #(_, sample) = entry
      sample.name == name
    })
  case found {
    Ok(#(kind, _)) -> kind
    Error(Nil) -> Gauge
  }
}

fn render_histograms(
  series: List(HistogramSeries),
) -> List(#(String, StringTree)) {
  series
  |> list.map(fn(entry) { entry.name })
  |> list.unique
  |> list.map(fn(name) {
    let lines =
      series
      |> list.filter(fn(entry) { entry.name == name })
      |> list.map(histogram_lines)
    let help = list.key_find(histogram_help, name) |> result.unwrap(name)
    #(name, string_tree.concat([header(name, "histogram", help), ..lines]))
  })
}

fn histogram_lines(series: HistogramSeries) -> StringTree {
  let #(_, bucket_lines) =
    list.zip(metrics.bucket_bounds_ms, series.buckets)
    |> list.map_fold(0, fn(running, pair) {
      let #(bound, count) = pair
      let total = running + count
      #(
        total,
        line(
          series.name <> "_bucket",
          list.append(series.labels, [#("le", int.to_string(bound))]),
          total,
        ),
      )
    })
  string_tree.concat(
    list.flatten([
      bucket_lines,
      [
        line(
          series.name <> "_bucket",
          list.append(series.labels, [#("le", "+Inf")]),
          series.count,
        ),
        line(series.name <> "_sum", series.labels, series.sum),
        line(series.name <> "_count", series.labels, series.count),
      ],
    ]),
  )
}

fn family(
  name: String,
  kind: String,
  help: String,
  series: List(#(List(#(String, String)), Int)),
) -> StringTree {
  case series {
    [] -> string_tree.new()
    _ ->
      string_tree.concat([
        header(name, kind, help),
        ..list.map(series, fn(entry) {
          let #(labels, value) = entry
          line(name, labels, value)
        })
      ])
  }
}

fn header(name: String, kind: String, help: String) -> StringTree {
  string_tree.from_strings([
    "# HELP ", name, " ", help, "\n", "# TYPE ", name, " ", kind, "\n",
  ])
}

fn line(
  name: String,
  labels: List(#(String, String)),
  value: Int,
) -> StringTree {
  string_tree.from_strings([
    name,
    label_text(labels),
    " ",
    int.to_string(value),
    "\n",
  ])
}

fn label_text(labels: List(#(String, String))) -> String {
  case labels {
    [] -> ""
    _ -> {
      let pairs =
        labels
        |> list.map(fn(label) {
          let #(key, value) = label
          key <> "=\"" <> escape(value) <> "\""
        })
        |> string.join(",")
      "{" <> pairs <> "}"
    }
  }
}

fn escape(value: String) -> String {
  value
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
}

fn kind_name(kind: Kind) -> String {
  case kind {
    Counter -> "counter"
    Gauge -> "gauge"
    Histogram -> "histogram"
  }
}
