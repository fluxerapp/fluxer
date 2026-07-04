# Gateway

`fluxer_gateway` is the Erlang OTP WebSocket gateway for Fluxer. Clients connect over WebSocket and interact with per-user Session processes, which coordinate with per-guild Guild processes, the Presence subsystem, and a NATS-backed inter-node RPC layer.

## Documents

| Document | What it covers |
|---|---|
| [Architecture Overview](architecture-overview.md) | Top-level role of the gateway, subsystem inventory, `GATEWAY_ROLE` env var, sharding model, node routing |
| [Opcodes and Close Codes](opcodes-and-close-codes.md) | All gateway opcodes with direction and description; all close codes with resumability |
| [Events](events.md) | All dispatched event names grouped by category, with visibility rules |
| [OTP Supervision Tree](otp-supervision-tree.md) | Supervisor strategy, all children in startup order, role-gating, `guild_ets_owner` constraint |
| [WebSocket Handler](websocket-handler.md) | Cowboy callback chain, opcode table, close code table, encoding, compression, rate limiting, IP extraction |
| [Session Lifecycle](session-lifecycle.md) | Session creation, state fields, Resume flow, event buffer, drain, presence attachment, state serialization |
| [Guild gen_server](guild-gen-server.md) | Init pipeline, `handle_call` routing, sync vs async session connect, dispatch, hibernate, termination |
| [Event Dispatch Pipeline](event-dispatch-pipeline.md) | `handle_dispatch/3` entry, all pipeline steps, session filtering, pre-encoded optimisation, push, member-list broadcast |
| [Presence Subsystem](presence-subsystem.md) | `presence` gen_server state, `presence_bus` fan-out, `presence_cache`, `presence_manager`, push buffer |
| [Push Notifications](push-notifications.md) | `push` gen_server init, eligibility checks, dispatcher pool, APNs, FCM, ETS cache, rendezvous routing, VAPID keys |
| [Voice](voice.md) | `guild_voice_server`, connection lifecycle, E2EE, reconciliation, channel move, permission checks, DM voice |
| [Calls](calls.md) | `call` gen_server state, init pipeline, ringing lifecycle, join paths, confirm flow, idle timeout, handoff |
| [Permissions](permissions.md) | Bitfield model, `can_view_channel`, overwrite merge order, permission cache, `guild_visibility` |
| [Clustering and NATS RPC](clustering-nats-rpc.md) | Cluster discovery, rendezvous hashing, `gateway_node_router`, NATS RPC, publish pool, cluster handoff |
| [Hot Patching](hot-patching.md) | Hotpatch reconciler, startup reconcile loop, bundle verification, module loading, audit trail, readiness gate |
| [Telemetry](telemetry.md) | Health endpoints, runtime probe, process watchdog, `process_registry`, `gateway_timings`, Prometheus metrics, periodic GC |
| [Shared Utilities](shared-utilities.md) | `constants`, `bitset`, `permission_bits`, `snowflake_id`, `event_atoms`, `backoff_utils`, `limited_deque`, validation |

## Quick orientation

**Starting a new feature** > read [Architecture Overview](architecture-overview.md) then the document for the subsystem you are changing.

**Tracing an inbound event** > [WebSocket Handler](websocket-handler.md) → [Session Lifecycle](session-lifecycle.md) → [Guild gen_server](guild-gen-server.md) → [Event Dispatch Pipeline](event-dispatch-pipeline.md).

**Debugging a permission issue** > [Permissions](permissions.md) covers the full bitfield pipeline; [Shared Utilities](shared-utilities.md) covers `bitset` and `permission_bits` APIs.

**Working on clustering or deploys** > [Clustering and NATS RPC](clustering-nats-rpc.md) for inter-node routing and handoff; [Hot Patching](hot-patching.md) for live code loading; [Telemetry](telemetry.md) for health endpoints and the drain trigger.
