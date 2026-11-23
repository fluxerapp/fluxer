// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/endpoint_url
import fluxer_gateway/config/env.{type EnvError, type Source}
import fluxer_gateway/config/node_name
import fluxer_gateway/core/id.{type GuildId}
import fluxer_gateway/core/node
import gleam/erlang/charlist.{type Charlist}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import logging

pub type Settings {
  Settings(
    self_hosted: Bool,
    port: Int,
    bind_host: String,
    status_port: Int,
    role: node.Role,
    api_internal_url: String,
    api_rpc_endpoint: Option(String),
    rpc_auth_token: String,
    nats_core_url: String,
    nats_auth_token: String,
    nats_rpc_enabled: Bool,
    gateway_nats_rpc_max_handlers: Int,
    gateway_http_rpc_max_concurrency: Int,
    gateway_http_recovery_timeout_ms: Int,
    gateway_http_failure_threshold: Int,
    client_ip_header: String,
    trust_client_ip_header: Bool,
    media_proxy_endpoint: String,
    static_cdn_endpoint: String,
    push_enabled: Bool,
    push_enrolled_clear_notifications_enabled: Bool,
    push_outbox_request_timeout_ms: Int,
    presence_push_buffer_max_entries: Int,
    presence_push_buffer_max_bytes: Int,
    cluster_enabled: Bool,
    cluster_discovery_dns_name: Option(String),
    cluster_discovery_node_basename: Option(String),
    cluster_discovery_poll_interval_ms: Int,
    cluster_static_peers: List(String),
    pinned_guild_ids: List(GuildId),
    bouncer: Option(BouncerSettings),
    guild_visitor_sessions_max: Int,
    guild_visitor_sessions_floor: Int,
    guild_visitor_sessions_per_member: Int,
    guild_visitor_sessions_per_user: Int,
    guild_visitor_sessions_per_ip: Int,
    zstd_window_log: Int,
    identify_token_limit_per_minute: Int,
    logger_level: LogLevel,
    durations: Durations,
  )
}

pub type BouncerSettings {
  BouncerSettings(
    dns_name: String,
    basename: String,
    count: Int,
    pool: String,
    template_hash: String,
  )
}

pub type Durations {
  Durations(
    api: ApiDurations,
    app: AppDurations,
    bouncer: BouncerDurations,
    cluster: ClusterDurations,
    config: ConfigDurations,
    gateway: GatewayDurations,
    guild: GuildDurations,
    nats: NatsDurations,
    presence: PresenceDurations,
    push: PushDurations,
    rpc: RpcDurations,
    session: SessionDurations,
    telemetry: TelemetryDurations,
    threads: ThreadsDurations,
    util: UtilDurations,
    voice: VoiceDurations,
  )
}

pub type ApiDurations {
  ApiDurations(
    breaker_bucket_ms: Int,
    breaker_window_ms: Int,
    client_retry_base_ms: Int,
    client_retry_cap_ms: Int,
    http_default_recovery_timeout_ms: Int,
    http_half_open_retry_ms: Int,
    http_init_timeout_ms: Int,
    http_keep_alive_timeout_ms: Int,
    http_open_retry_floor_ms: Int,
  )
}

pub type AppDurations {
  AppDurations(
    drain_deadline_ms: Int,
    drain_frame_flush_ms: Int,
    drain_handoff_wait_ms: Int,
    drain_socket_notice_delay_ms: Int,
  )
}

pub type BouncerDurations {
  BouncerDurations(
    beat_interval_ms: Int,
    lease_ttl_ms: Int,
    skew_margin_ms: Int,
    isolation_cap_ms: Int,
    probe_timeout_ms: Int,
    unplaced_retry_ms: Int,
  )
}

pub type ClusterDurations {
  ClusterDurations(
    discovery_default_poll_interval_ms: Int,
    discovery_init_timeout_ms: Int,
    handoff_call_timeout_ms: Int,
    handoff_debounce_ms: Int,
    handoff_init_timeout_ms: Int,
    handoff_reconcile_ms: Int,
    handoff_resubscribe_ms: Int,
    handoff_worker_watch_ms: Int,
    membership_init_timeout_ms: Int,
    membership_probe_timeout_ms: Int,
    membership_tick_ms: Int,
    relay_claim_backoff_ms: Int,
    relay_claim_deadline_ms: Int,
    relay_init_timeout_ms: Int,
    router_retry_high_ms: Int,
    router_retry_low_ms: Int,
    membership_state_discovery_refresh_ms: Int,
    membership_state_flap_stable_ms: Int,
    membership_state_flap_window_ms: Int,
    membership_state_forget_after_ms: Int,
    membership_state_gone_after_ms: Int,
    membership_state_gone_reconnect_ms: Int,
    membership_state_hello_resend_ms: Int,
    membership_state_joining_stable_ms: Int,
    membership_state_probe_timeout_ms: Int,
    membership_state_publish_interval_ms: Int,
    membership_state_reconnect_cap_ms: Int,
    membership_state_reconnect_max_ms: Int,
    membership_state_reconnect_min_ms: Int,
    state_tick_margin_ms: Int,
  )
}

pub type ConfigDurations {
  ConfigDurations(
    rollout_fetch_delay_ms: Int,
    rollout_fetch_retry_ms: Int,
    rollout_init_timeout_ms: Int,
    rollout_nats_subscribe_retry_ms: Int,
    rollout_reconcile_interval_ms: Int,
  )
}

pub type GatewayDurations {
  GatewayDurations(
    abuse_protection_identify_window_secs: Int,
    ip_connections_owner_sweep_interval_ms: Int,
    limits_heartbeat_interval_ms: Int,
    limits_heartbeat_timeout_ms: Int,
    limits_resume_timeout_ms: Int,
    rate_limit_connection_window_ms: Int,
    rate_limit_forum_unreads_window_ms: Int,
    rate_limit_presence_window_ms: Int,
    rate_limit_shared_cleanup_interval_ms: Int,
    rate_limit_shared_window_ms: Int,
    rate_limit_voice_signal_window_ms: Int,
    server_init_timeout_ms: Int,
    server_registry_sweep_interval_ms: Int,
    server_request_timeout_ms: Int,
    server_send_timeout_ms: Int,
    socket_heartbeat_ack_fallback_ms: Int,
    socket_pending_identify_retry_base_ms: Int,
    socket_pending_identify_retry_jitter_ms: Int,
    socket_reconnect_linger_ms: Int,
    socket_resume_reply_timeout_ms: Int,
    start_gate_cancel_ack_ms: Int,
    start_gate_init_timeout_ms: Int,
    voice_ops_drain_interval_ms: Int,
    voice_ops_rate_window_ms: Int,
  )
}

pub type GuildDurations {
  GuildDurations(
    join_admission_baseline_window_ms: Int,
    join_admission_codel_interval_ms: Int,
    join_admission_codel_target_ms: Int,
    join_admission_rate_sample_ms: Int,
    join_admission_retry_max_ms: Int,
    join_admission_retry_min_ms: Int,
    join_admission_standing_max_age_ms: Int,
    join_admission_transit_ms: Int,
    admission_max_age_ms: Int,
    availability_recheck_interval_ms: Int,
    broadcast_init_timeout_ms: Int,
    client_overloaded_slot_ms: Int,
    client_unavailable_slot_ms: Int,
    connect_no_owner_retry_ms: Int,
    connect_too_many_sessions_retry_ms: Int,
    counts_cache_init_timeout_ms: Int,
    counts_cache_remote_timeout_ms: Int,
    counts_cache_sweep_interval_ms: Int,
    dispatch_profile_sweep_ms: Int,
    dispatch_settings_deadline_ms: Int,
    ets_owner_init_timeout_ms: Int,
    fetch_timeout_ms: Int,
    fetch_worker_stop_timeout_ms: Int,
    forum_unreads_api_timeout_ms: Int,
    freeze_ack_margin_ms: Int,
    freeze_replay_timeout_ms: Int,
    freeze_resync_timeout_ms: Int,
    freeze_timeout_ms: Int,
    grace_hold_call_timeout_ms: Int,
    grace_recheck_ms: Int,
    handoff_move_spacing_ms: Int,
    handoff_retry_delay_ms: Int,
    health_degraded_ms: Int,
    health_init_timeout_ms: Int,
    health_interval_ms: Int,
    health_probe_timeout_ms: Int,
    health_recovered_ms: Int,
    health_stale_probe_ms: Int,
    infra_forward_timeout_ms: Int,
    infra_guild_call_timeout_ms: Int,
    infra_reload_all_slack_ms: Int,
    infra_reload_all_timeout_ms: Int,
    infra_reload_timeout_ms: Int,
    infra_start_backoff_base_ms: Int,
    infra_start_backoff_jitter_ms: Int,
    infra_start_backoff_max_ms: Int,
    infra_start_lookup_timeout_ms: Int,
    maintenance_count_refresh_interval_ms: Int,
    manager_describe_timeout_ms: Int,
    manager_guild_shutdown_ms: Int,
    manager_incumbent_lookup_timeout_ms: Int,
    manager_overloaded_retry_ms: Int,
    manager_unavailable_retry_ms: Int,
    members_filter_bot_full_list_window_ms: Int,
    members_filter_guild_window_ms: Int,
    move_freeze_ask_timeout_ms: Int,
    online_viewer_count_ttl_ms: Int,
    passive_sync_interval_ms: Int,
    permissions_batch_timeout_ms: Int,
    profiles_invalidation_ttl_ms: Int,
    profiles_profile_ttl_ms: Int,
    reconcile_confirm_delay_ms: Int,
    reconcile_interval_ms: Int,
    rows_large_flush_delay_ms: Int,
    rows_small_flush_delay_ms: Int,
    send_claim_backoff_ms: Int,
    send_claim_deadline_ms: Int,
    server_auto_stop_grace_ms: Int,
    server_idle_start_grace_ms: Int,
    server_stale_perms_release_ms: Int,
    server_down_jitter_ms: Int,
    server_forward_grace_ms: Int,
    server_init_timeout_ms: Int,
    server_seal_wait_ms: Int,
    server_staged_ttl_ms: Int,
    server_successor_timeout_ms: Int,
    shard_api_unavailable_retry_ms: Int,
    shard_gate_full_retry_ms: Int,
    shard_init_timeout_ms: Int,
    shard_reload_timeout_ms: Int,
    shard_retire_timeout_ms: Int,
    shard_stop_timeout_ms: Int,
    shard_unavailable_retry_ms: Int,
    transfer_awaiting_replay_ms: Int,
    transfer_freeze_deadline_ms: Int,
    transfer_moved_timeout_ms: Int,
  )
}

pub type NatsDurations {
  NatsDurations(
    connection_handshake_slack_ms: Int,
    connection_ping_interval_ms: Int,
    connection_pong_timeout_ms: Int,
    connection_send_timeout_ms: Int,
    connection_waiter_grace_ms: Int,
    pool_breaker_cooldown_ms: Int,
    pool_call_timeout_ms: Int,
    pool_connect_slack_ms: Int,
    pool_connect_timeout_ms: Int,
    pool_init_timeout_ms: Int,
    pool_reconnect_base_ms: Int,
    pool_reconnect_max_ms: Int,
    pool_subscribe_retry_base_ms: Int,
    pool_subscribe_retry_max_ms: Int,
    pool_subscribe_timeout_ms: Int,
  )
}

pub type PresenceDurations {
  PresenceDurations(
    cache_remote_read_timeout_ms: Int,
    client_connect_timeout_ms: Int,
    client_retry_after_ms: Int,
    client_start_timeout_ms: Int,
    fanout_drain_linger_ms: Int,
    fanout_drain_retry_ms: Int,
    fanout_drain_start_timeout_ms: Int,
    fanout_init_timeout_ms: Int,
    fanout_relay_tick_ms: Int,
    manager_check_interval_ms: Int,
    manager_init_timeout_ms: Int,
    manager_move_spread_ms: Int,
    manager_topology_settle_ms: Int,
    owner_init_timeout_ms: Int,
    registry_digest_interval_ms: Int,
    registry_grave_ttl_ms: Int,
    registry_init_timeout_ms: Int,
    registry_lost_owner_grace_ms: Int,
    registry_sweep_interval_ms: Int,
    route_handoff_window_ms: Int,
    route_rebuild_interval_ms: Int,
    route_unreachable_retry_ms: Int,
    rpc_lookup_timeout_ms: Int,
    server_expiry_jitter_ms: Int,
    server_forward_grace_ms: Int,
    server_handoff_timeout_ms: Int,
    server_init_timeout_ms: Int,
    server_kick_timeout_ms: Int,
    server_seal_wait_ms: Int,
    server_shed_tick_ms: Int,
    server_terminate_timeout_ms: Int,
    server_validation_grace_ms: Int,
    server_validation_timeout_ms: Int,
    state_doomed_for_ms: Int,
    state_idle_check_ms: Int,
    state_max_move_backoff_ms: Int,
    state_move_busy_retry_ms: Int,
    state_noconnection_grace_ms: Int,
  )
}

pub type PushDurations {
  PushDurations(
    fetch_max_chunk_ms: Int,
    fetch_result_slack_ms: Int,
    outbox_enqueue_timeout_ms: Int,
    outbox_gauge_interval_ms: Int,
    outbox_init_timeout_ms: Int,
    outbox_prune_interval_ms: Int,
    permit_give_back_timeout_ms: Int,
    permit_guild_wide_max_wait_ms: Int,
    permit_init_timeout_ms: Int,
    permit_keeper_init_timeout_ms: Int,
    permit_release_timeout_ms: Int,
    permit_settings_fetch_max_wait_ms: Int,
    permit_tick_ms: Int,
    read_state_fetch_timeout_ms: Int,
    read_state_outbox_max_age_ms: Int,
    recipients_blocked_timeout_ms: Int,
    recipients_known_blocked_deadline_ms: Int,
    recipients_result_slack_ms: Int,
    rpc_multicall_timeout_ms: Int,
    server_evict_interval_ms: Int,
    server_init_timeout_ms: Int,
    server_job_deadline_ms: Int,
    server_pending_drain_interval_ms: Int,
    server_pending_max_age_ms: Int,
    server_utilization_warn_interval_ms: Int,
    settings_cache_blocked_ids_ttl_seconds: Int,
    settings_cache_failure_entry_ms: Int,
    settings_cache_reservation_ttl_ms: Int,
    settings_cache_settings_ttl_ms: Int,
    outbox_state_bulk_age_ms: Int,
    outbox_state_direct_age_ms: Int,
    outbox_state_drain_grace_ms: Int,
    outbox_state_drain_window_ms: Int,
    outbox_state_in_flight_floor_ms: Int,
    outbox_state_reads_max_age_ms: Int,
    outbox_state_retry_base_ms: Int,
    outbox_state_retry_max_ms: Int,
    outbox_state_slot_deadline_ms: Int,
    throttle_bucket_ms: Int,
  )
}

pub type RpcDurations {
  RpcDurations(server_init_timeout_ms: Int, server_subscribe_retry_ms: Int)
}

pub type SessionDurations {
  SessionDurations(
    calls_get_state_timeout_ms: Int,
    calls_unavailable_retry_ms: Int,
    guilds_after_ready_join_ms: Int,
    guilds_busy_retry_ms: Int,
    guilds_down_reconnect_ms: Int,
    guilds_first_join_floor_ms: Int,
    guilds_join_grace_ms: Int,
    join_budget_rate_sample_ms: Int,
    join_budget_retry_max_ms: Int,
    join_budget_retry_min_ms: Int,
    links_backoff_base_ms: Int,
    links_backoff_cap_ms: Int,
    links_base_sleep_ms: Int,
    links_cap_sleep_ms: Int,
    links_repair_interval_ms: Int,
    links_slow_retry_ms: Int,
    manager_start_timeout: Int,
    manager_unavailable_retry_ms: Int,
    member_requests_limit_cleanup_interval_ms: Int,
    member_requests_limit_window_ms: Int,
    presence_link_connect_timeout_ms: Int,
    reactions_interval_ms: Int,
    router_init_timeout_ms: Int,
    server_init_timeout_ms: Int,
    server_offline_grace_ms: Int,
    server_presence_enable_ms: Int,
    server_ready_deadline_ms: Int,
    server_rebind_ack_wait_ms: Int,
    server_move_relay_ms: Int,
    server_repair_interval_ms: Int,
    shard_fetch_timeout_ms: Int,
    shard_init_timeout_ms: Int,
    shard_permit_wait_ms: Int,
    shard_remote_resume_timeout_ms: Int,
    shard_unavailable_retry_ms: Int,
    transfer_cleanup_interval_ms: Int,
    transfer_hand_off_timeout_ms: Int,
    transfer_init_timeout_ms: Int,
    transfer_locate_timeout_ms: Int,
    transfer_push_timeout_ms: Int,
    transfer_ttl_ms: Int,
    voice_counts_timeout_ms: Int,
  )
}

pub type TelemetryDurations {
  TelemetryDurations(
    periodic_gc_gc_interval_ms: Int,
    periodic_gc_graceful_stop_wait_ms: Int,
    periodic_gc_health_interval_ms: Int,
    periodic_gc_init_timeout_ms: Int,
    periodic_gc_registry_sweep_interval_ms: Int,
    rpc_memory_rpc_timeout_ms: Int,
    rpc_node_rpc_timeout_ms: Int,
    stall_monitor_init_timeout_ms: Int,
    stall_monitor_late_tick_ms: Int,
    stall_monitor_log_interval_ms: Int,
    stall_monitor_long_gc_ms: Int,
    stall_monitor_long_schedule_ms: Int,
    stall_monitor_severe_ms: Int,
    stall_monitor_tick_ms: Int,
    timings_remote_pod_lookup_timeout_ms: Int,
    watchdog_check_interval_ms: Int,
    watchdog_init_timeout_ms: Int,
  )
}

pub type ThreadsDurations {
  ThreadsDurations(
    runtime_first_retry_ms: Int,
    runtime_later_retry_ms: Int,
    runtime_second_retry_ms: Int,
    store_handoff_max_age_ms: Int,
    subscriptions_list_flush_ms: Int,
    subscriptions_sync_jitter_ms: Int,
  )
}

pub type UtilDurations {
  UtilDurations(
    liveness_remote_timeout_ms: Int,
    table_await_poll_ms: Int,
    table_keeper_ready_ms: Int,
  )
}

pub type VoiceDurations {
  VoiceDurations(
    call_server_init_timeout_ms: Int,
    connection_pending_ttl_ms: Int,
    counts_collect_timeout_ms: Int,
    counts_init_timeout_ms: Int,
    counts_sync_interval_ms: Int,
    dm_legacy_call_check_interval_ms: Int,
    dm_server_init_timeout_ms: Int,
    guild_handler_fetch_ttl_ms: Int,
    guild_handler_handshake_timeout_ms: Int,
    guild_handler_p2p_check_interval_ms: Int,
    guild_handler_relay_lifetime_ms: Int,
    guild_handler_sweep_interval_ms: Int,
    guild_rpc_recent_restore_ms: Int,
    lifecycle_force_disconnect_retry_ms: Int,
    lifecycle_recent_ttl_ms: Int,
    lifecycle_sweep_recent_ttl_ms: Int,
    manager_adopt_timeout_ms: Int,
    manager_create_timeout_ms: Int,
    manager_export_timeout_ms: Int,
    manager_init_timeout_ms: Int,
    manager_list_timeout_ms: Int,
    manager_lookup_timeout_ms: Int,
    permission_sync_init_timeout_ms: Int,
    permission_sync_retry_base_ms: Int,
    permission_sync_retry_jitter_ms: Int,
    permission_sync_retry_max_ms: Int,
    region_pending_ttl_ms: Int,
    rpc_call_timeout_ms: Int,
    server_ask_timeout_ms: Int,
    server_call_timeout_ms: Int,
    server_forward_grace_ms: Int,
    server_handoff_timeout_ms: Int,
    server_handshake_timeout_ms: Int,
    server_idle_timeout_ms: Int,
    server_pending_timeout_ms: Int,
    server_rebind_timeout_ms: Int,
    server_retry_sleep_ms: Int,
    server_ring_timeout_ms: Int,
  )
}

pub fn default_durations() -> Durations {
  Durations(
    api: ApiDurations(
      breaker_bucket_ms: 1000,
      breaker_window_ms: 10_000,
      client_retry_base_ms: 250,
      client_retry_cap_ms: 15_000,
      http_default_recovery_timeout_ms: 5000,
      http_half_open_retry_ms: 1000,
      http_init_timeout_ms: 5000,
      http_keep_alive_timeout_ms: 4000,
      http_open_retry_floor_ms: 100,
    ),
    app: AppDurations(
      drain_deadline_ms: 600_000,
      drain_frame_flush_ms: 500,
      drain_handoff_wait_ms: 15_000,
      drain_socket_notice_delay_ms: 5000,
    ),
    bouncer: BouncerDurations(
      beat_interval_ms: 1000,
      lease_ttl_ms: 6000,
      skew_margin_ms: 1000,
      isolation_cap_ms: 120_000,
      probe_timeout_ms: 500,
      unplaced_retry_ms: 50,
    ),
    cluster: ClusterDurations(
      discovery_default_poll_interval_ms: 5000,
      discovery_init_timeout_ms: 5000,
      handoff_call_timeout_ms: 5000,
      handoff_debounce_ms: 2000,
      handoff_init_timeout_ms: 5000,
      handoff_reconcile_ms: 60_000,
      handoff_resubscribe_ms: 500,
      handoff_worker_watch_ms: 90_000,
      membership_init_timeout_ms: 5000,
      membership_probe_timeout_ms: 2000,
      membership_tick_ms: 500,
      relay_claim_backoff_ms: 1,
      relay_claim_deadline_ms: 5000,
      relay_init_timeout_ms: 1000,
      router_retry_high_ms: 2500,
      router_retry_low_ms: 500,
      membership_state_discovery_refresh_ms: 10_000,
      membership_state_flap_stable_ms: 120_000,
      membership_state_flap_window_ms: 300_000,
      membership_state_forget_after_ms: 600_000,
      membership_state_gone_after_ms: 30_000,
      membership_state_gone_reconnect_ms: 10_000,
      membership_state_hello_resend_ms: 3000,
      membership_state_joining_stable_ms: 10_000,
      membership_state_probe_timeout_ms: 2000,
      membership_state_publish_interval_ms: 10_000,
      membership_state_reconnect_cap_ms: 5000,
      membership_state_reconnect_max_ms: 2000,
      membership_state_reconnect_min_ms: 1000,
      state_tick_margin_ms: 1000,
    ),
    config: ConfigDurations(
      rollout_fetch_delay_ms: 2000,
      rollout_fetch_retry_ms: 10_000,
      rollout_init_timeout_ms: 5000,
      rollout_nats_subscribe_retry_ms: 2000,
      rollout_reconcile_interval_ms: 30_000,
    ),
    gateway: GatewayDurations(
      abuse_protection_identify_window_secs: 60,
      ip_connections_owner_sweep_interval_ms: 60_000,
      limits_heartbeat_interval_ms: 41_250,
      limits_heartbeat_timeout_ms: 45_000,
      limits_resume_timeout_ms: 60_000,
      rate_limit_connection_window_ms: 60_000,
      rate_limit_forum_unreads_window_ms: 5000,
      rate_limit_presence_window_ms: 20_000,
      rate_limit_shared_cleanup_interval_ms: 120_000,
      rate_limit_shared_window_ms: 60_000,
      rate_limit_voice_signal_window_ms: 10_000,
      server_init_timeout_ms: 1000,
      server_registry_sweep_interval_ms: 60_000,
      server_request_timeout_ms: 5000,
      server_send_timeout_ms: 30_000,
      socket_heartbeat_ack_fallback_ms: 5000,
      socket_pending_identify_retry_base_ms: 1000,
      socket_pending_identify_retry_jitter_ms: 1000,
      socket_reconnect_linger_ms: 1000,
      socket_resume_reply_timeout_ms: 15_000,
      start_gate_cancel_ack_ms: 1000,
      start_gate_init_timeout_ms: 5000,
      voice_ops_drain_interval_ms: 500,
      voice_ops_rate_window_ms: 1000,
    ),
    guild: GuildDurations(
      join_admission_baseline_window_ms: 10_000,
      join_admission_codel_interval_ms: 500,
      join_admission_codel_target_ms: 100,
      join_admission_rate_sample_ms: 1000,
      join_admission_retry_max_ms: 15_000,
      join_admission_retry_min_ms: 250,
      join_admission_standing_max_age_ms: 2000,
      join_admission_transit_ms: 50,
      admission_max_age_ms: 60_000,
      availability_recheck_interval_ms: 30_000,
      broadcast_init_timeout_ms: 5000,
      client_overloaded_slot_ms: 500,
      client_unavailable_slot_ms: 1000,
      connect_no_owner_retry_ms: 1000,
      connect_too_many_sessions_retry_ms: 5000,
      counts_cache_init_timeout_ms: 5000,
      counts_cache_remote_timeout_ms: 1000,
      counts_cache_sweep_interval_ms: 60_000,
      dispatch_profile_sweep_ms: 60_000,
      dispatch_settings_deadline_ms: 30_000,
      ets_owner_init_timeout_ms: 5000,
      fetch_timeout_ms: 120_000,
      fetch_worker_stop_timeout_ms: 5000,
      forum_unreads_api_timeout_ms: 2000,
      freeze_ack_margin_ms: 5000,
      freeze_replay_timeout_ms: 5000,
      freeze_resync_timeout_ms: 140_000,
      freeze_timeout_ms: 10_000,
      grace_hold_call_timeout_ms: 5000,
      grace_recheck_ms: 7000,
      handoff_move_spacing_ms: 50,
      handoff_retry_delay_ms: 100,
      health_degraded_ms: 2000,
      health_init_timeout_ms: 5000,
      health_interval_ms: 250,
      health_probe_timeout_ms: 5000,
      health_recovered_ms: 500,
      health_stale_probe_ms: 6000,
      infra_forward_timeout_ms: 15_000,
      infra_guild_call_timeout_ms: 4000,
      infra_reload_all_slack_ms: 1000,
      infra_reload_all_timeout_ms: 15_000,
      infra_reload_timeout_ms: 20_000,
      infra_start_backoff_base_ms: 250,
      infra_start_backoff_jitter_ms: 100,
      infra_start_backoff_max_ms: 2000,
      infra_start_lookup_timeout_ms: 8000,
      maintenance_count_refresh_interval_ms: 30_000,
      manager_describe_timeout_ms: 100,
      manager_guild_shutdown_ms: 130_000,
      manager_incumbent_lookup_timeout_ms: 1000,
      manager_overloaded_retry_ms: 500,
      manager_unavailable_retry_ms: 1000,
      members_filter_bot_full_list_window_ms: 30_000,
      members_filter_guild_window_ms: 10_000,
      move_freeze_ask_timeout_ms: 10_000,
      online_viewer_count_ttl_ms: 5000,
      passive_sync_interval_ms: 30_000,
      permissions_batch_timeout_ms: 5000,
      profiles_invalidation_ttl_ms: 60_000,
      profiles_profile_ttl_ms: 600_000,
      reconcile_confirm_delay_ms: 1500,
      reconcile_interval_ms: 30_000,
      rows_large_flush_delay_ms: 1000,
      rows_small_flush_delay_ms: 250,
      send_claim_backoff_ms: 1,
      send_claim_deadline_ms: 250,
      server_auto_stop_grace_ms: 30_000,
      server_idle_start_grace_ms: 600_000,
      server_stale_perms_release_ms: 5000,
      server_down_jitter_ms: 1000,
      server_forward_grace_ms: 10_000,
      server_init_timeout_ms: 120_000,
      server_seal_wait_ms: 500,
      server_staged_ttl_ms: 90_000,
      server_successor_timeout_ms: 2000,
      shard_api_unavailable_retry_ms: 5000,
      shard_gate_full_retry_ms: 500,
      shard_init_timeout_ms: 5000,
      shard_reload_timeout_ms: 20_000,
      shard_retire_timeout_ms: 5000,
      shard_stop_timeout_ms: 5000,
      shard_unavailable_retry_ms: 1000,
      transfer_awaiting_replay_ms: 30_000,
      transfer_freeze_deadline_ms: 170_000,
      transfer_moved_timeout_ms: 20_000,
    ),
    nats: NatsDurations(
      connection_handshake_slack_ms: 1000,
      connection_ping_interval_ms: 15_000,
      connection_pong_timeout_ms: 5000,
      connection_send_timeout_ms: 5000,
      connection_waiter_grace_ms: 1000,
      pool_breaker_cooldown_ms: 15_000,
      pool_call_timeout_ms: 5000,
      pool_connect_slack_ms: 1000,
      pool_connect_timeout_ms: 10_000,
      pool_init_timeout_ms: 5000,
      pool_reconnect_base_ms: 2000,
      pool_reconnect_max_ms: 30_000,
      pool_subscribe_retry_base_ms: 250,
      pool_subscribe_retry_max_ms: 5000,
      pool_subscribe_timeout_ms: 5000,
    ),
    presence: PresenceDurations(
      cache_remote_read_timeout_ms: 1000,
      client_connect_timeout_ms: 10_000,
      client_retry_after_ms: 1000,
      client_start_timeout_ms: 2000,
      fanout_drain_linger_ms: 200,
      fanout_drain_retry_ms: 50,
      fanout_drain_start_timeout_ms: 1000,
      fanout_init_timeout_ms: 5000,
      fanout_relay_tick_ms: 200,
      manager_check_interval_ms: 10_000,
      manager_init_timeout_ms: 5000,
      manager_move_spread_ms: 2000,
      manager_topology_settle_ms: 1000,
      owner_init_timeout_ms: 5000,
      registry_digest_interval_ms: 30_000,
      registry_grave_ttl_ms: 300_000,
      registry_init_timeout_ms: 5000,
      registry_lost_owner_grace_ms: 15_000,
      registry_sweep_interval_ms: 60_000,
      route_handoff_window_ms: 120_000,
      route_rebuild_interval_ms: 10_000,
      route_unreachable_retry_ms: 1000,
      rpc_lookup_timeout_ms: 2000,
      server_expiry_jitter_ms: 5000,
      server_forward_grace_ms: 10_000,
      server_handoff_timeout_ms: 5000,
      server_init_timeout_ms: 5000,
      server_kick_timeout_ms: 10_000,
      server_seal_wait_ms: 500,
      server_shed_tick_ms: 200,
      server_terminate_timeout_ms: 5000,
      server_validation_grace_ms: 1000,
      server_validation_timeout_ms: 5000,
      state_doomed_for_ms: 60_000,
      state_idle_check_ms: 30_000,
      state_max_move_backoff_ms: 30_000,
      state_move_busy_retry_ms: 250,
      state_noconnection_grace_ms: 15_000,
    ),
    push: PushDurations(
      fetch_max_chunk_ms: 10_000,
      fetch_result_slack_ms: 1000,
      outbox_enqueue_timeout_ms: 1000,
      outbox_gauge_interval_ms: 1000,
      outbox_init_timeout_ms: 1000,
      outbox_prune_interval_ms: 30_000,
      permit_give_back_timeout_ms: 1000,
      permit_guild_wide_max_wait_ms: 60_000,
      permit_init_timeout_ms: 5000,
      permit_keeper_init_timeout_ms: 1000,
      permit_release_timeout_ms: 5000,
      permit_settings_fetch_max_wait_ms: 10_000,
      permit_tick_ms: 1000,
      read_state_fetch_timeout_ms: 2000,
      read_state_outbox_max_age_ms: 300_000,
      recipients_blocked_timeout_ms: 10_000,
      recipients_known_blocked_deadline_ms: 30_000,
      recipients_result_slack_ms: 1000,
      rpc_multicall_timeout_ms: 5000,
      server_evict_interval_ms: 60_000,
      server_init_timeout_ms: 1000,
      server_job_deadline_ms: 300_000,
      server_pending_drain_interval_ms: 50,
      server_pending_max_age_ms: 30_000,
      server_utilization_warn_interval_ms: 10_000,
      settings_cache_blocked_ids_ttl_seconds: 300,
      settings_cache_failure_entry_ms: 1000,
      settings_cache_reservation_ttl_ms: 120_000,
      settings_cache_settings_ttl_ms: 600_000,
      outbox_state_bulk_age_ms: 30_000,
      outbox_state_direct_age_ms: 300_000,
      outbox_state_drain_grace_ms: 5000,
      outbox_state_drain_window_ms: 20_000,
      outbox_state_in_flight_floor_ms: 5000,
      outbox_state_reads_max_age_ms: 300_000,
      outbox_state_retry_base_ms: 1000,
      outbox_state_retry_max_ms: 30_000,
      outbox_state_slot_deadline_ms: 5000,
      throttle_bucket_ms: 5000,
    ),
    rpc: RpcDurations(
      server_init_timeout_ms: 5000,
      server_subscribe_retry_ms: 2000,
    ),
    session: SessionDurations(
      calls_get_state_timeout_ms: 5000,
      calls_unavailable_retry_ms: 1000,
      guilds_after_ready_join_ms: 15_000,
      guilds_busy_retry_ms: 50,
      guilds_down_reconnect_ms: 5500,
      guilds_first_join_floor_ms: 2000,
      guilds_join_grace_ms: 1000,
      join_budget_rate_sample_ms: 1000,
      join_budget_retry_max_ms: 15_000,
      join_budget_retry_min_ms: 250,
      links_backoff_base_ms: 1000,
      links_backoff_cap_ms: 30_000,
      links_base_sleep_ms: 250,
      links_cap_sleep_ms: 15_000,
      links_repair_interval_ms: 30_000,
      links_slow_retry_ms: 60_000,
      manager_start_timeout: 20_000,
      manager_unavailable_retry_ms: 1000,
      member_requests_limit_cleanup_interval_ms: 60_000,
      member_requests_limit_window_ms: 10_000,
      presence_link_connect_timeout_ms: 10_000,
      reactions_interval_ms: 650,
      router_init_timeout_ms: 5000,
      server_init_timeout_ms: 5000,
      server_offline_grace_ms: 5000,
      server_presence_enable_ms: 10_000,
      server_ready_deadline_ms: 5000,
      server_rebind_ack_wait_ms: 2000,
      server_move_relay_ms: 10_000,
      server_repair_interval_ms: 60_000,
      shard_fetch_timeout_ms: 10_000,
      shard_init_timeout_ms: 5000,
      shard_permit_wait_ms: 5000,
      shard_remote_resume_timeout_ms: 10_000,
      shard_unavailable_retry_ms: 1000,
      transfer_cleanup_interval_ms: 10_000,
      transfer_hand_off_timeout_ms: 8000,
      transfer_init_timeout_ms: 5000,
      transfer_locate_timeout_ms: 1000,
      transfer_push_timeout_ms: 5000,
      transfer_ttl_ms: 120_000,
      voice_counts_timeout_ms: 2000,
    ),
    telemetry: TelemetryDurations(
      periodic_gc_gc_interval_ms: 10_000,
      periodic_gc_graceful_stop_wait_ms: 2000,
      periodic_gc_health_interval_ms: 30_000,
      periodic_gc_init_timeout_ms: 1000,
      periodic_gc_registry_sweep_interval_ms: 300_000,
      rpc_memory_rpc_timeout_ms: 5000,
      rpc_node_rpc_timeout_ms: 10_000,
      stall_monitor_init_timeout_ms: 1000,
      stall_monitor_late_tick_ms: 250,
      stall_monitor_log_interval_ms: 10_000,
      stall_monitor_long_gc_ms: 200,
      stall_monitor_long_schedule_ms: 200,
      stall_monitor_severe_ms: 1000,
      stall_monitor_tick_ms: 100,
      timings_remote_pod_lookup_timeout_ms: 100,
      watchdog_check_interval_ms: 10_000,
      watchdog_init_timeout_ms: 1000,
    ),
    threads: ThreadsDurations(
      runtime_first_retry_ms: 30_000,
      runtime_later_retry_ms: 600_000,
      runtime_second_retry_ms: 120_000,
      store_handoff_max_age_ms: 60_000,
      subscriptions_list_flush_ms: 250,
      subscriptions_sync_jitter_ms: 5000,
    ),
    util: UtilDurations(
      liveness_remote_timeout_ms: 1000,
      table_await_poll_ms: 1,
      table_keeper_ready_ms: 1000,
    ),
    voice: VoiceDurations(
      call_server_init_timeout_ms: 5000,
      connection_pending_ttl_ms: 30_000,
      counts_collect_timeout_ms: 500,
      counts_init_timeout_ms: 5000,
      counts_sync_interval_ms: 30_000,
      dm_legacy_call_check_interval_ms: 2000,
      dm_server_init_timeout_ms: 5000,
      guild_handler_fetch_ttl_ms: 60_000,
      guild_handler_handshake_timeout_ms: 5000,
      guild_handler_p2p_check_interval_ms: 60_000,
      guild_handler_relay_lifetime_ms: 180_000,
      guild_handler_sweep_interval_ms: 10_000,
      guild_rpc_recent_restore_ms: 60_000,
      lifecycle_force_disconnect_retry_ms: 500,
      lifecycle_recent_ttl_ms: 60_000,
      lifecycle_sweep_recent_ttl_ms: 120_000,
      manager_adopt_timeout_ms: 5000,
      manager_create_timeout_ms: 10_000,
      manager_export_timeout_ms: 5000,
      manager_init_timeout_ms: 5000,
      manager_list_timeout_ms: 5000,
      manager_lookup_timeout_ms: 2000,
      permission_sync_init_timeout_ms: 5000,
      permission_sync_retry_base_ms: 250,
      permission_sync_retry_jitter_ms: 100,
      permission_sync_retry_max_ms: 2000,
      region_pending_ttl_ms: 30_000,
      rpc_call_timeout_ms: 2000,
      server_ask_timeout_ms: 5000,
      server_call_timeout_ms: 5000,
      server_forward_grace_ms: 10_000,
      server_handoff_timeout_ms: 30_000,
      server_handshake_timeout_ms: 5000,
      server_idle_timeout_ms: 120_000,
      server_pending_timeout_ms: 30_000,
      server_rebind_timeout_ms: 30_000,
      server_retry_sleep_ms: 300,
      server_ring_timeout_ms: 30_000,
    ),
  )
}

pub type LogLevel {
  Debug
  Info
  Notice
  Warning
  Error
  Critical
  Alert
  Emergency
}

pub type SettingsError {
  EnvVar(EnvError)
  InvalidStaticPeer(peer: String)
  InvalidPinnedGuildId(token: String)
  MissingBouncerDnsName
  InvalidBindHost(text: String)
}

type Key {
  FluxerGatewaySettings
}

type Stored

@external(erlang, "persistent_term", "put")
fn put_term(key: Key, value: Option(Settings)) -> Stored

@external(erlang, "persistent_term", "get")
fn get_term(key: Key, default: Option(Settings)) -> Option(Settings)

const max_static_peers = 256

const default_bind_host = "0.0.0.0"

type IpAddress

type ParseError

@external(erlang, "inet", "parse_address")
fn parse_address(text: Charlist) -> Result(IpAddress, ParseError)

const default_media_proxy_endpoint = "http://localhost:8088/media"

const default_static_cdn_endpoint = "http://localhost:8088"

fn load() -> Settings {
  read(env.system())
  |> result.map_error(log_invalid)
  |> result.lazy_unwrap(defaults)
  |> store
}

pub fn store(settings: Settings) -> Settings {
  put_term(FluxerGatewaySettings, Some(settings))
  settings
}

pub fn get() -> Settings {
  case get_term(FluxerGatewaySettings, None) {
    Some(settings) -> settings
    None -> load()
  }
}

pub fn role_runs(settings: Settings, role: node.Role) -> Bool {
  node.runs(settings.role, role)
}

pub fn defaults() -> Settings {
  Settings(
    self_hosted: False,
    port: 8771,
    bind_host: default_bind_host,
    status_port: 8082,
    role: node.AllRoles,
    api_internal_url: "http://127.0.0.1:8080",
    api_rpc_endpoint: None,
    rpc_auth_token: "",
    nats_core_url: "nats://nats:4222",
    nats_auth_token: "",
    nats_rpc_enabled: True,
    gateway_nats_rpc_max_handlers: 512,
    gateway_http_rpc_max_concurrency: 512,
    gateway_http_recovery_timeout_ms: 15_000,
    gateway_http_failure_threshold: 0,
    client_ip_header: "x-forwarded-for",
    trust_client_ip_header: False,
    media_proxy_endpoint: default_media_proxy_endpoint,
    static_cdn_endpoint: default_static_cdn_endpoint,
    push_enabled: True,
    push_enrolled_clear_notifications_enabled: True,
    push_outbox_request_timeout_ms: 100_000,
    presence_push_buffer_max_entries: 128,
    presence_push_buffer_max_bytes: 1_048_576,
    cluster_enabled: False,
    cluster_discovery_dns_name: None,
    cluster_discovery_node_basename: None,
    cluster_discovery_poll_interval_ms: 5000,
    cluster_static_peers: [],
    pinned_guild_ids: [],
    bouncer: None,
    guild_visitor_sessions_max: 10_000,
    guild_visitor_sessions_floor: 500,
    guild_visitor_sessions_per_member: 2,
    guild_visitor_sessions_per_user: 5,
    guild_visitor_sessions_per_ip: 20,
    zstd_window_log: 14,
    identify_token_limit_per_minute: 0,
    logger_level: Info,
    durations: default_durations(),
  )
}

pub fn read(source: Source) -> Result(Settings, SettingsError) {
  let d = defaults()
  let text = fn(name, default) { env.string(source, name, default:) }
  let number = fn(name, default) { env.int(source, name, default:) }
  let flag = fn(name, default) { env.bool(source, name, default:) }
  let optional = fn(name) { env.value(source, name) }
  use self_hosted <- try_env(flag("FLUXER_SELF_HOSTED", d.self_hosted))
  use port <- try_env(number("FLUXER_GATEWAY_PORT", d.port))
  use bind_host <- try_env(optional("FLUXER_GATEWAY_BIND_HOST"))
  use bind_host <- result.try(parse_bind_host(bind_host))
  use status_port <- try_env(number("FLUXER_GATEWAY_STATUS_PORT", d.status_port))
  use role <- try_env(optional("FLUXER_GATEWAY_ROLE"))
  let role = parse_role(role)
  use api_internal_url <- try_env(text(
    "FLUXER_INTERNAL_API_ENDPOINT",
    d.api_internal_url,
  ))
  use api_rpc_endpoint <- try_env(optional("FLUXER_GATEWAY_API_RPC_ENDPOINT"))
  use rpc_auth_token <- try_env(text(
    "FLUXER_GATEWAY_RPC_AUTH_TOKEN",
    d.rpc_auth_token,
  ))
  use nats_core_url <- try_env(text("FLUXER_NATS_URL", d.nats_core_url))
  use nats_auth_token <- try_env(text(
    "FLUXER_NATS_AUTH_TOKEN",
    d.nats_auth_token,
  ))
  use nats_rpc_enabled <- try_env(flag(
    "FLUXER_GATEWAY_NATS_RPC_ENABLED",
    d.nats_rpc_enabled,
  ))
  use gateway_nats_rpc_max_handlers <- try_env(number(
    "FLUXER_GATEWAY_NATS_RPC_MAX_HANDLERS",
    d.gateway_nats_rpc_max_handlers,
  ))
  use gateway_http_rpc_max_concurrency <- try_env(number(
    "FLUXER_GATEWAY_HTTP_RPC_MAX_CONCURRENCY",
    d.gateway_http_rpc_max_concurrency,
  ))
  use gateway_http_recovery_timeout_ms <- try_env(number(
    "FLUXER_GATEWAY_HTTP_RECOVERY_TIMEOUT_MS",
    d.gateway_http_recovery_timeout_ms,
  ))
  use gateway_http_failure_threshold <- try_env(number(
    "FLUXER_GATEWAY_HTTP_FAILURE_THRESHOLD",
    d.gateway_http_failure_threshold,
  ))
  use client_ip_header <- try_env(text(
    "FLUXER_CLIENT_IP_HEADER_NAME",
    d.client_ip_header,
  ))
  use trust_client_ip_header <- try_env(flag(
    "FLUXER_TRUST_CLIENT_IP_HEADER",
    d.trust_client_ip_header,
  ))
  use base_domain <- try_env(optional("FLUXER_BASE_DOMAIN"))
  use public_port <- try_env(optional("FLUXER_PUBLIC_PORT"))
  use media_proxy_endpoint <- try_env(text(
    "FLUXER_GATEWAY_MEDIA_PROXY_ENDPOINT",
    default_media_proxy_endpoint,
  ))
  use static_cdn_endpoint <- try_env(text(
    "FLUXER_GATEWAY_STATIC_CDN_ENDPOINT",
    default_static_cdn_endpoint,
  ))
  use push_enabled <- try_env(flag(
    "FLUXER_GATEWAY_PUSH_ENABLED",
    d.push_enabled,
  ))
  use push_enrolled_clear_notifications_enabled <- try_env(flag(
    "FLUXER_GATEWAY_PUSH_ENROLLED_CLEAR_NOTIFICATIONS_ENABLED",
    d.push_enrolled_clear_notifications_enabled,
  ))
  use push_outbox_request_timeout_ms <- try_env(number(
    "FLUXER_GATEWAY_PUSH_OUTBOX_REQUEST_TIMEOUT_MS",
    d.push_outbox_request_timeout_ms,
  ))
  use presence_push_buffer_max_entries <- try_env(number(
    "FLUXER_GATEWAY_PRESENCE_PUSH_BUFFER_MAX_ENTRIES",
    d.presence_push_buffer_max_entries,
  ))
  use presence_push_buffer_max_bytes <- try_env(number(
    "FLUXER_GATEWAY_PRESENCE_PUSH_BUFFER_MAX_BYTES",
    d.presence_push_buffer_max_bytes,
  ))
  use cluster_enabled <- try_env(flag(
    "FLUXER_GATEWAY_CLUSTER_ENABLED",
    d.cluster_enabled,
  ))
  use cluster_discovery_dns_name <- try_env(optional(
    "FLUXER_GATEWAY_CLUSTER_DISCOVERY_DNS_NAME",
  ))
  use cluster_discovery_node_basename <- try_env(optional(
    "FLUXER_GATEWAY_CLUSTER_DISCOVERY_NODE_BASENAME",
  ))
  use cluster_discovery_poll_interval_ms <- try_env(number(
    "FLUXER_GATEWAY_CLUSTER_DISCOVERY_POLL_INTERVAL_MS",
    d.cluster_discovery_poll_interval_ms,
  ))
  use static_peers <- try_env(optional("FLUXER_GATEWAY_CLUSTER_STATIC_PEERS"))
  use cluster_static_peers <- result.try(parse_static_peers(static_peers))
  use pinned <- try_env(optional("FLUXER_GATEWAY_PINNED_GUILD_IDS"))
  use pinned_guild_ids <- result.try(parse_pinned_guild_ids(pinned))
  use bouncer_count <- try_env(number("FLUXER_GATEWAY_BOUNCER_COUNT", 0))
  use bouncer_dns_name <- try_env(optional("FLUXER_GATEWAY_BOUNCER_DNS_NAME"))
  use bouncer_basename <- try_env(text(
    "FLUXER_GATEWAY_BOUNCER_BASENAME",
    "gateway-bouncer",
  ))
  use pool <- try_env(text("FLUXER_GATEWAY_POOL", "default"))
  use template_hash <- try_env(text("FLUXER_GATEWAY_TEMPLATE_HASH", ""))
  use bouncer <- result.try(bouncer_settings(
    bouncer_count,
    bouncer_dns_name,
    bouncer_basename,
    pool,
    template_hash,
  ))
  use guild_visitor_sessions_max <- try_env(number(
    "FLUXER_GATEWAY_GUILD_VISITOR_SESSIONS_MAX",
    d.guild_visitor_sessions_max,
  ))
  use guild_visitor_sessions_floor <- try_env(number(
    "FLUXER_GATEWAY_GUILD_VISITOR_SESSIONS_FLOOR",
    d.guild_visitor_sessions_floor,
  ))
  use guild_visitor_sessions_per_member <- try_env(number(
    "FLUXER_GATEWAY_GUILD_VISITOR_SESSIONS_PER_MEMBER",
    d.guild_visitor_sessions_per_member,
  ))
  use guild_visitor_sessions_per_user <- try_env(number(
    "FLUXER_GATEWAY_GUILD_VISITOR_SESSIONS_PER_USER",
    d.guild_visitor_sessions_per_user,
  ))
  use guild_visitor_sessions_per_ip <- try_env(number(
    "FLUXER_GATEWAY_GUILD_VISITOR_SESSIONS_PER_IP",
    d.guild_visitor_sessions_per_ip,
  ))
  use zstd_window_log <- try_env(number(
    "FLUXER_GATEWAY_ZSTD_WINDOW_LOG",
    d.zstd_window_log,
  ))
  use identify_token_limit_per_minute <- try_env(number(
    "FLUXER_GATEWAY_IDENTIFY_TOKEN_LIMIT_PER_MINUTE",
    d.identify_token_limit_per_minute,
  ))
  use drain_deadline_ms <- try_env(number(
    "FLUXER_GATEWAY_DRAIN_DEADLINE_MS",
    d.durations.app.drain_deadline_ms,
  ))
  use gateway_level <- try_env(optional("FLUXER_GATEWAY_LOGGER_LEVEL"))
  use override_level <- try_env(optional("LOGGER_LEVEL"))
  let port_number =
    option.then(public_port, fn(text) {
      endpoint_url.leading_int(text) |> option.from_result
    })
  let endpoint = fn(url) {
    endpoint_url.normalize(url, base_domain:, public_port: port_number)
  }
  let configured_level =
    gateway_level |> option.then(parse_log_level) |> option.unwrap(Info)
  Ok(Settings(
    self_hosted:,
    port:,
    bind_host:,
    status_port:,
    role:,
    api_internal_url:,
    api_rpc_endpoint:,
    rpc_auth_token:,
    nats_core_url:,
    nats_auth_token:,
    nats_rpc_enabled:,
    gateway_nats_rpc_max_handlers:,
    gateway_http_rpc_max_concurrency:,
    gateway_http_recovery_timeout_ms:,
    gateway_http_failure_threshold:,
    client_ip_header: client_ip_header |> string.trim |> string.lowercase,
    trust_client_ip_header:,
    media_proxy_endpoint: endpoint(media_proxy_endpoint),
    static_cdn_endpoint: endpoint(static_cdn_endpoint),
    push_enabled:,
    push_enrolled_clear_notifications_enabled:,
    push_outbox_request_timeout_ms:,
    presence_push_buffer_max_entries:,
    presence_push_buffer_max_bytes:,
    cluster_enabled:,
    cluster_discovery_dns_name:,
    cluster_discovery_node_basename:,
    cluster_discovery_poll_interval_ms:,
    cluster_static_peers:,
    pinned_guild_ids:,
    bouncer:,
    guild_visitor_sessions_max:,
    guild_visitor_sessions_floor:,
    guild_visitor_sessions_per_member:,
    guild_visitor_sessions_per_user:,
    guild_visitor_sessions_per_ip:,
    zstd_window_log:,
    identify_token_limit_per_minute:,
    logger_level: override_level
      |> option.then(parse_log_level)
      |> option.unwrap(configured_level),
    durations: Durations(
      ..d.durations,
      app: AppDurations(..d.durations.app, drain_deadline_ms:),
    ),
  ))
}

pub fn parse_role(value: Option(String)) -> node.Role {
  case value {
    None -> node.AllRoles
    Some(text) ->
      case node.role_from_name(string.trim(text)) {
        Ok(role) -> role
        _ -> {
          logging.log(
            logging.Warning,
            "FLUXER_GATEWAY_ROLE is not a known role, using all: " <> text,
          )
          node.AllRoles
        }
      }
  }
}

pub fn parse_bind_host(value: Option(String)) -> Result(String, SettingsError) {
  case value {
    None -> Ok(default_bind_host)
    Some(text) -> {
      let host = string.trim(text)
      parse_address(charlist.from_string(host))
      |> result.replace(host)
      |> result.replace_error(InvalidBindHost(text))
    }
  }
}

fn parse_log_level(value: String) -> Option(LogLevel) {
  case value |> string.trim |> string.lowercase {
    "debug" -> Some(Debug)
    "info" -> Some(Info)
    "notice" -> Some(Notice)
    "warning" -> Some(Warning)
    "error" -> Some(Error)
    "critical" -> Some(Critical)
    "alert" -> Some(Alert)
    "emergency" -> Some(Emergency)
    _ -> None
  }
}

pub fn log_level_name(level: LogLevel) -> String {
  case level {
    Debug -> "debug"
    Info -> "info"
    Notice -> "notice"
    Warning -> "warning"
    Error -> "error"
    Critical -> "critical"
    Alert -> "alert"
    Emergency -> "emergency"
  }
}

pub fn describe_error(error: SettingsError) -> String {
  case error {
    EnvVar(error) -> env.describe(error)
    InvalidStaticPeer(peer:) ->
      "FLUXER_GATEWAY_CLUSTER_STATIC_PEERS has an invalid node name: " <> peer
    InvalidPinnedGuildId(token:) ->
      "FLUXER_GATEWAY_PINNED_GUILD_IDS has an invalid guild id: " <> token
    MissingBouncerDnsName ->
      "FLUXER_GATEWAY_BOUNCER_COUNT is set but FLUXER_GATEWAY_BOUNCER_DNS_NAME is not"
    InvalidBindHost(text:) ->
      "FLUXER_GATEWAY_BIND_HOST is not an IP address: " <> text
  }
}

fn bouncer_settings(
  count: Int,
  dns_name: Option(String),
  basename: String,
  pool: String,
  template_hash: String,
) -> Result(Option(BouncerSettings), SettingsError) {
  case count > 0 {
    False -> Ok(None)
    True ->
      dns_name
      |> option.to_result(MissingBouncerDnsName)
      |> result.map(fn(dns_name) {
        Some(BouncerSettings(
          dns_name:,
          basename:,
          count:,
          pool:,
          template_hash:,
        ))
      })
  }
}

fn parse_static_peers(
  value: Option(String),
) -> Result(List(String), SettingsError) {
  value
  |> option.map(fn(text) { string.split(text, ",") })
  |> option.unwrap([])
  |> list.filter(fn(token) { token != "" })
  |> list.take(max_static_peers)
  |> list.try_map(fn(peer) {
    node_name.parse(peer) |> result.replace_error(InvalidStaticPeer(peer))
  })
}

fn parse_pinned_guild_ids(
  value: Option(String),
) -> Result(List(GuildId), SettingsError) {
  use ids <- result.try(
    value
    |> option.map(fn(text) { string.split(text, ",") })
    |> option.unwrap([])
    |> list.flat_map(fn(part) { string.split(part, " ") })
    |> list.filter(fn(token) { token != "" })
    |> list.try_map(parse_pinned_id),
  )
  ids
  |> list.sort(int.compare)
  |> list.unique
  |> list.try_map(fn(value) {
    id.guild_id(value)
    |> result.replace_error(InvalidPinnedGuildId(int.to_string(value)))
  })
}

fn parse_pinned_id(token: String) -> Result(Int, SettingsError) {
  int.parse(token)
  |> option.from_result
  |> option.then(fn(value) {
    case value > 0 {
      True -> Some(value)
      False -> None
    }
  })
  |> option.to_result(InvalidPinnedGuildId(token))
}

fn try_env(
  found: Result(a, EnvError),
  next: fn(a) -> Result(b, SettingsError),
) -> Result(b, SettingsError) {
  found |> result.map_error(EnvVar) |> result.try(next)
}

fn log_invalid(error: SettingsError) -> Nil {
  logging.log(
    logging.Error,
    "Gateway settings invalid, using defaults: " <> describe_error(error),
  )
}
