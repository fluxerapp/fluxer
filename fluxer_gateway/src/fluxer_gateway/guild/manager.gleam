// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/remote
import fluxer_gateway/config/durations
import fluxer_gateway/config/settings.{type Settings}
import fluxer_gateway/core/clock
import fluxer_gateway/core/error.{Unavailable}
import fluxer_gateway/core/guild.{type JoinReply, type JoinRequest}
import fluxer_gateway/core/id.{type GuildId}
import fluxer_gateway/core/node.{type Node}
import fluxer_gateway/guild/handoff
import fluxer_gateway/guild/inbox.{type Handle}
import fluxer_gateway/guild/manager/registry
import fluxer_gateway/guild/manager/shard
import fluxer_gateway/guild/metrics as guild_metrics
import fluxer_gateway/guild/names
import fluxer_gateway/guild/server
import fluxer_gateway/telemetry/memory_stats
import fluxer_gateway/telemetry/metrics
import fluxer_gateway/util/spawn
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option
import gleam/otp/factory_supervisor
import gleam/otp/static_supervisor
import gleam/otp/supervision.{type ChildSpecification}
import gleam/result

fn unavailable_retry_ms() -> Int {
  durations.guild().manager_unavailable_retry_ms
}

fn overloaded_retry_ms() -> Int {
  durations.guild().manager_overloaded_retry_ms
}

fn guild_shutdown_ms() -> Int {
  durations.guild().manager_guild_shutdown_ms
}

fn describe_timeout_ms() -> Int {
  durations.guild().manager_describe_timeout_ms
}

pub fn supervised(_settings: Settings) -> ChildSpecification(Nil) {
  metrics.register_collector("fluxer_gateway_guilds", fn() {
    [metrics.Sample("fluxer_gateway_guilds_total", [], registry.count())]
  })
  guild_metrics.register()
  memory_stats.register_describer(describe)
  int.range(
    0,
    names.shard_count,
    static_supervisor.new(static_supervisor.OneForOne)
      |> static_supervisor.restart_tolerance(intensity: 10, period: 10),
    fn(builder, index) {
      builder
      |> static_supervisor.add(
        factory_supervisor.worker_child(server.start)
        |> factory_supervisor.named(shard.factory_name(index))
        |> factory_supervisor.restart_strategy(supervision.Temporary)
        |> factory_supervisor.timeout(ms: guild_shutdown_ms())
        |> factory_supervisor.supervised,
      )
      |> static_supervisor.add(supervision.worker(fn() { shard.start(index) }))
    },
  )
  |> static_supervisor.supervised
  |> supervision.map_data(fn(_) { Nil })
}

fn describe(pid: process.Pid) -> Result(memory_stats.GuildDescription, Nil) {
  use found <- result.try(registry.handle_by_pid(pid))
  let reply = process.new_subject()
  process.send(found.control, inbox.Describe(reply))
  process.receive(reply, describe_timeout_ms())
}

fn shard_name(guild: GuildId) -> process.Name(shard.Message) {
  names.shard(names.shard_of(guild))
}

pub fn send_local(guild: GuildId, message: shard.Message) -> Result(Nil, Nil) {
  remote.send_local(shard_name(guild), message)
}

fn send_to(
  owner: Node,
  guild: GuildId,
  message: shard.Message,
) -> Result(Nil, remote.SendFailure) {
  remote.send_named(owner, shard_name(guild), message)
}

pub fn join(
  owner: Node,
  guild: GuildId,
  request: JoinRequest,
  reply: process.Subject(JoinReply),
) -> Result(Nil, remote.SendFailure) {
  send_to(owner, guild, shard.Join(guild, request, reply))
}

pub fn start_or_lookup(
  owner: Node,
  guild: GuildId,
  timeout_ms: Int,
) -> Result(Handle, shard.LookupError) {
  let unavailable =
    shard.LookupUnavailable(Unavailable(retry_after_ms: unavailable_retry_ms()))
  case ask(owner, guild, shard.StartOrLookup(guild, _), timeout_ms) {
    Ok(result) -> result
    Error(AskFailed(remote.NoSuspend)) ->
      Error(
        shard.LookupUnavailable(
          error.Overloaded(retry_after_ms: overloaded_retry_ms()),
        ),
      )
    Error(_) -> Error(unavailable)
  }
}

pub fn ask_shard(
  owner: Node,
  guild: GuildId,
  make: fn(process.Subject(reply)) -> shard.Message,
  timeout_ms: Int,
) -> Result(reply, Nil) {
  ask(owner, guild, make, timeout_ms) |> result.replace_error(Nil)
}

type AskError {
  AskFailed(remote.SendFailure)
  ShardDown
  AskTimeout
}

fn ask(
  owner: Node,
  guild: GuildId,
  make: fn(process.Subject(reply)) -> shard.Message,
  timeout_ms: Int,
) -> Result(reply, AskError) {
  let reply = process.new_subject()
  case owner == node.self() {
    False ->
      case remote.ask_named(owner, shard_name(guild), make(reply)) {
        Error(failure) -> Error(AskFailed(failure))
        Ok(monitor) -> await_reply(reply, monitor, timeout_ms)
      }
    True ->
      case process.named(shard_name(guild)) {
        Error(Nil) -> Error(ShardDown)
        Ok(pid) -> {
          let monitor = process.monitor(pid)
          process.send(process.named_subject(shard_name(guild)), make(reply))
          await_reply(reply, monitor, timeout_ms)
        }
      }
  }
}

fn await_reply(
  reply: process.Subject(reply),
  monitor: process.Monitor,
  timeout_ms: Int,
) -> Result(reply, AskError) {
  let outcome =
    process.new_selector()
    |> process.select_map(reply, Ok)
    |> process.select_specific_monitor(monitor, fn(_) { Error(ShardDown) })
    |> process.selector_receive(timeout_ms)
  process.demonitor_process(monitor)
  case outcome {
    Ok(found) -> found
    Error(Nil) -> Error(AskTimeout)
  }
}

fn guild_ids(guild_ints: List(Int)) -> List(GuildId) {
  list.filter_map(guild_ints, fn(guild) {
    id.guild_id(guild) |> result.replace_error(Nil)
  })
}

pub fn export_for(nodes: List(Node)) -> Int {
  handoff.export_for(nodes)
}

pub fn reload_local(guild_ints: List(Int), timeout_ms: Int) -> Int {
  let targets = case guild_ints {
    [] -> guild_ids(registry.guild_ints())
    _ -> guild_ids(guild_ints)
  }
  let replies = process.new_subject()
  let sent =
    list.fold(targets, 0, fn(count, guild) {
      case send_local(guild, shard.ReloadGuild(guild, replies)) {
        Ok(Nil) -> count + 1
        Error(Nil) -> count
      }
    })
  collect_reloads(replies, sent, clock.monotonic_ms() + timeout_ms, 0)
}

fn collect_reloads(
  replies: process.Subject(shard.ReloadResult),
  left: Int,
  deadline: Int,
  count: Int,
) -> Int {
  case left {
    0 -> count
    _ ->
      case
        process.receive(replies, int.max(deadline - clock.monotonic_ms(), 0))
      {
        Ok(shard.Reloaded) ->
          collect_reloads(replies, left - 1, deadline, count + 1)
        Ok(_) -> collect_reloads(replies, left - 1, deadline, count)
        Error(Nil) -> count
      }
  }
}

pub type LocalSweep {
  LocalSweep(report: shard.SweepReport, shards: Int, replied: Int)
}

pub fn sweep_ownership_local(
  force: Bool,
  dry_run: Bool,
  timeout_ms: Int,
) -> LocalSweep {
  let result = process.new_subject()
  let #(_, monitor) =
    spawn.monitored(fn() {
      process.send(result, run_sweep(force, dry_run, timeout_ms))
    })
  let swept =
    process.new_selector()
    |> process.select_map(result, Ok)
    |> process.select_specific_monitor(monitor, fn(_) { Error(Nil) })
    |> process.selector_receive(timeout_ms + sweep_margin_ms)
  process.demonitor_process(monitor)
  case swept {
    Ok(Ok(found)) -> found
    _ ->
      LocalSweep(
        report: shard.SweepReport(
          running: 0,
          unowned: 0,
          disowned: 0,
          lapsed: 0,
        ),
        shards: names.shard_count,
        replied: 0,
      )
  }
}

const sweep_margin_ms = 1000

fn run_sweep(force: Bool, dry_run: Bool, timeout_ms: Int) -> LocalSweep {
  let reply = process.new_subject()
  let asked =
    int.range(0, names.shard_count, 0, fn(asked, index) {
      case
        remote.send_local(
          names.shard(index),
          shard.SweepOwnership(force:, dry_run:, reply: option.Some(reply)),
        )
      {
        Ok(Nil) -> asked + 1
        Error(Nil) -> asked
      }
    })
  collect_sweeps(
    reply,
    asked,
    clock.deadline_in(timeout_ms),
    LocalSweep(
      report: shard.SweepReport(running: 0, unowned: 0, disowned: 0, lapsed: 0),
      shards: names.shard_count,
      replied: 0,
    ),
  )
}

fn collect_sweeps(
  reply: process.Subject(shard.SweepReport),
  left: Int,
  deadline: clock.Deadline,
  total: LocalSweep,
) -> LocalSweep {
  case left {
    0 -> total
    _ ->
      case process.receive(reply, clock.remaining_ms(deadline)) {
        Error(Nil) -> total
        Ok(found) ->
          collect_sweeps(
            reply,
            left - 1,
            deadline,
            LocalSweep(
              ..total,
              report: shard.SweepReport(
                running: total.report.running + found.running,
                unowned: total.report.unowned + found.unowned,
                disowned: total.report.disowned + found.disowned,
                lapsed: total.report.lapsed + found.lapsed,
              ),
              replied: total.replied + 1,
            ),
          )
      }
  }
}
