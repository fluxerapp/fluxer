// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/config/settings.{type Settings}
import fluxer_gateway/core/clock
import fluxer_gateway/push/metrics
import fluxer_gateway/push/named
import fluxer_gateway/push/permit/pool.{type Pool}
import fluxer_gateway/push/settings_cache
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Name, type Pid, type Subject}
import gleam/int
import gleam/list
import gleam/option.{Some}
import gleam/otp/actor
import gleam/otp/static_supervisor as supervisor
import gleam/otp/supervision
import gleam/result

fn init_timeout_ms() -> Int {
  durations.push().permit_init_timeout_ms
}

fn release_timeout_ms() -> Int {
  durations.push().permit_release_timeout_ms
}

fn give_back_timeout_ms() -> Int {
  durations.push().permit_give_back_timeout_ms
}

fn evict_interval_ms() -> Int {
  durations.push().server_evict_interval_ms
}

fn keeper_init_timeout_ms() -> Int {
  durations.push().permit_keeper_init_timeout_ms
}

const table_name = "fluxer_gateway_push_permit_rows"

pub const profiles_registry = "fluxer_gateway_push_guild_profiles"

fn tick_ms() -> Int {
  durations.push().permit_tick_ms
}

const max_waiters = 4096

fn guild_wide_max_wait_ms() -> Int {
  durations.push().permit_guild_wide_max_wait_ms
}

const settings_fetch_capacity = 64

fn settings_fetch_max_wait_ms() -> Int {
  durations.push().permit_settings_fetch_max_wait_ms
}

pub type PoolKind {
  GuildWide
  SettingsFetch
}

pub type DropReason {
  NodeQueueFull
  PermitWaitExpired
  PermitTimeout
}

pub type Reply {
  Granted(token: Int)
  Queued(token: Int)
  Dropped(token: Int, reason: pool.DropReason)
}

pub opaque type Permit {
  Held(pool: PoolKind, token: Int)
  Unmanaged
}

pub opaque type Message {
  Claim(pool: PoolKind, token: Int, holder: Pid, reply: Subject(Reply))
  Release(pool: PoolKind, token: Int, ack: Subject(Nil))
  HolderDown(process.Down)
  Tick
  TableBack
}

type KeeperMessage {
  GiveBack(to: Pid, reply: Subject(Bool))
  KeeperTableBack
  EvictCaches
}

type Claimant {
  Claimant(pid: Pid, reply: Subject(Reply))
}

type Row =
  #(#(PoolKind, Int), Pid, Subject(Reply), Bool, Int)

type State {
  State(
    self: Subject(Message),
    guild_wide: Pool(Claimant),
    settings_fetch: Pool(Claimant),
    by_pid: Dict(Pid, List(#(PoolKind, Int))),
  )
}

type TableOption {
  NamedTable
  Public
  Set
  Heir(Pid, Nil)
  Keypos(Int)
  ReadConcurrency(Bool)
  WriteConcurrency(Bool)
}

@external(erlang, "erlang", "binary_to_atom")
fn server_name_atom(name: String) -> Name(Message)

@external(erlang, "erlang", "binary_to_atom")
fn keeper_name_atom(name: String) -> Name(KeeperMessage)

@external(erlang, "erlang", "binary_to_atom")
fn to_atom(name: String) -> Atom

@external(erlang, "ets", "new")
fn ets_new(name: Atom, options: List(TableOption)) -> Atom

@external(erlang, "ets", "insert")
fn ets_insert(table: Atom, row: Row) -> Bool

@external(erlang, "ets", "delete")
fn ets_delete(table: Atom, key: #(PoolKind, Int)) -> Bool

@external(erlang, "ets", "tab2list")
fn ets_rows(table: Atom) -> List(Row)

@external(erlang, "ets", "give_away")
fn ets_give_away(table: Atom, to: Pid, data: Nil) -> Bool

@external(erlang, "ets", "setopts")
fn ets_setopts(table: Atom, option: TableOption) -> Bool

type UniqueOption {
  Positive
  Monotonic
}

@external(erlang, "erlang", "unique_integer")
fn unique_integer(options: List(UniqueOption)) -> Int

@external(erlang, "erlang", "make_ref")
fn make_ref() -> Dynamic

@external(erlang, "erlang", "system_info")
fn system_info(item: Atom) -> Int

fn server_name() -> Name(Message) {
  server_name_atom("fluxer_gateway_push_permits")
}

fn keeper_name() -> Name(KeeperMessage) {
  keeper_name_atom("fluxer_gateway_push_permit_keeper")
}

fn table() -> Atom {
  to_atom(table_name)
}

pub fn supervised(_settings: Settings) -> supervision.ChildSpecification(Nil) {
  supervision.supervisor(fn() {
    supervisor.new(supervisor.RestForOne)
    |> supervisor.restart_tolerance(intensity: 10, period: 10)
    |> supervisor.add(supervision.worker(start_keeper))
    |> supervisor.add(supervision.worker(start_server))
    |> supervisor.start
    |> result.map(fn(started) { actor.Started(pid: started.pid, data: Nil) })
  })
}

fn start_keeper() -> Result(actor.Started(Nil), actor.StartError) {
  actor.new_with_initialiser(keeper_init_timeout_ms(), fn(subject) {
    let _ = ets_new(table(), [NamedTable, Public, Set])
    let _ =
      ets_new(to_atom(profiles_registry), [
        NamedTable,
        Public,
        Set,
        Keypos(2),
        ReadConcurrency(True),
        WriteConcurrency(True),
      ])
    settings_cache.init()
    let timers = process.new_subject()
    process.send_after(timers, evict_interval_ms(), EvictCaches)
    let selector =
      process.new_selector()
      |> process.select(subject)
      |> process.select(timers)
      |> process.select_record(atom.create("ETS-TRANSFER"), 3, fn(_) {
        KeeperTableBack
      })
    actor.initialised(timers)
    |> actor.selecting(selector)
    |> actor.returning(Nil)
    |> Ok
  })
  |> actor.on_message(fn(timers, message) {
    case message {
      GiveBack(to:, reply:) -> {
        process.send(reply, ets_give_away(table(), to, Nil))
        actor.continue(timers)
      }
      KeeperTableBack -> actor.continue(timers)
      EvictCaches -> {
        settings_cache.evict()
        process.send_after(timers, evict_interval_ms(), EvictCaches)
        actor.continue(timers)
      }
    }
  })
  |> actor.named(keeper_name())
  |> actor.start
}

fn start_server() -> Result(actor.Started(Nil), actor.StartError) {
  actor.new_with_initialiser(init_timeout_ms(), fn(subject) {
    use keeper <- result.try(
      process.named(keeper_name()) |> result.replace_error("permit keeper down"),
    )
    let transfer =
      process.new_selector()
      |> process.select_record(atom.create("ETS-TRANSFER"), 3, fn(_) { Nil })
    let answer = process.new_subject()
    named.send_to(keeper, keeper_name(), GiveBack(process.self(), answer))
    use given <- result.try(
      process.receive(answer, give_back_timeout_ms())
      |> result.replace_error("permit keeper silent"),
    )
    use Nil <- result.try(case given {
      True ->
        process.selector_receive(transfer, give_back_timeout_ms())
        |> result.replace_error("permit table not transferred")
      False -> Error("permit table not owned by the keeper")
    })
    let _ = ets_setopts(table(), Heir(keeper, Nil))
    let self = process.new_subject()
    let state = restore(self)
    process.send_after(self, tick_ms(), Tick)
    let selector =
      process.new_selector()
      |> process.select(subject)
      |> process.select(self)
      |> process.select_monitors(HolderDown)
      |> process.select_record(atom.create("ETS-TRANSFER"), 3, fn(_) {
        TableBack
      })
    actor.initialised(state)
    |> actor.selecting(selector)
    |> actor.returning(Nil)
    |> Ok
  })
  |> actor.on_message(handle)
  |> actor.named(server_name())
  |> actor.start
}

fn guild_wide_config() -> pool.Config {
  pool.Config(
    capacity: 2 * system_info(atom.create("schedulers_online")),
    max_waiters:,
    max_wait_ms: Some(guild_wide_max_wait_ms()),
  )
}

fn settings_fetch_config() -> pool.Config {
  pool.Config(
    capacity: settings_fetch_capacity,
    max_waiters:,
    max_wait_ms: Some(settings_fetch_max_wait_ms()),
  )
}

fn restore(self: Subject(Message)) -> State {
  let rows = ets_rows(table())
  let state =
    State(
      self:,
      guild_wide: pool.new(guild_wide_config()),
      settings_fetch: pool.new(settings_fetch_config()),
      by_pid: dict.new(),
    )
  let state =
    list.fold(rows, state, fn(state, row) {
      let #(#(kind, token), pid, _, _, _) = row
      let state = watch(state, pid)
      State(..state, by_pid: add_owner(state.by_pid, pid, kind, token))
    })
  let #(guild_wide, wide_effects) = restore_pool(GuildWide, rows)
  let #(settings_fetch, fetch_effects) = restore_pool(SettingsFetch, rows)
  let state = State(..state, guild_wide:, settings_fetch:)
  let state = apply_effects(state, GuildWide, wide_effects)
  apply_effects(state, SettingsFetch, fetch_effects)
}

fn restore_pool(
  kind: PoolKind,
  rows: List(Row),
) -> #(Pool(Claimant), List(pool.Effect(Claimant))) {
  let holdings =
    list.filter_map(rows, fn(row) {
      case row {
        #(#(row_kind, token), pid, reply, True, _) if row_kind == kind ->
          Ok(pool.Holding(token, Claimant(pid, reply)))
        _ -> Error(Nil)
      }
    })
  let waiters =
    list.filter_map(rows, fn(row) {
      case row {
        #(#(row_kind, token), pid, reply, False, enqueued_at_ms)
          if row_kind == kind
        -> Ok(pool.Waiter(token, Claimant(pid, reply), enqueued_at_ms))
        _ -> Error(Nil)
      }
    })
  let config = case kind {
    GuildWide -> guild_wide_config()
    SettingsFetch -> settings_fetch_config()
  }
  pool.restore(config, holdings, waiters)
}

fn handle(state: State, message: Message) -> actor.Next(State, Message) {
  case message {
    Claim(pool: kind, token:, holder:, reply:) ->
      case pool.knows(pool_of(state, kind), token) {
        True -> actor.continue(state)
        False ->
          actor.continue(claim(state, kind, token, Claimant(holder, reply)))
      }
    Release(pool: kind, token:, ack:) -> {
      let _ = ets_delete(table(), #(kind, token))
      let #(updated, effects) = pool.release(pool_of(state, kind), token)
      let state = forget_token(set_pool(state, kind, updated), kind, token)
      let state = apply_effects(state, kind, effects)
      process.send(ack, Nil)
      actor.continue(state)
    }
    HolderDown(process.ProcessDown(pid:, ..)) ->
      actor.continue(holder_down(state, pid))
    HolderDown(_) -> actor.continue(state)
    Tick -> {
      process.send_after(state.self, tick_ms(), Tick)
      let now = clock.monotonic_ms()
      let #(wide, wide_drops) = pool.expire(state.guild_wide, now)
      let #(fetch, fetch_drops) = pool.expire(state.settings_fetch, now)
      let state = State(..state, guild_wide: wide, settings_fetch: fetch)
      let state = apply_effects(state, GuildWide, wide_drops)
      let state = apply_effects(state, SettingsFetch, fetch_drops)
      metrics.gauge(
        "push_permits_in_use",
        [#("pool", "guild_wide")],
        pool.in_use(state.guild_wide),
      )
      metrics.gauge(
        "push_permits_in_use",
        [#("pool", "settings_fetch")],
        pool.in_use(state.settings_fetch),
      )
      actor.continue(state)
    }
    TableBack -> actor.continue(state)
  }
}

fn claim(
  state: State,
  kind: PoolKind,
  token: Int,
  claimant: Claimant,
) -> State {
  let state = watch(state, claimant.pid)
  let #(updated, effect) =
    pool.claim(pool_of(state, kind), token, claimant, clock.monotonic_ms())
  apply_effects(set_pool(state, kind, updated), kind, [effect])
}

fn holder_down(state: State, pid: Pid) -> State {
  case dict.get(state.by_pid, pid) {
    Error(Nil) -> state
    Ok(owned) -> {
      list.each(owned, fn(entry) { ets_delete(table(), entry) })
      let state = State(..state, by_pid: dict.delete(state.by_pid, pid))
      list.fold([GuildWide, SettingsFetch], state, fn(state, kind) {
        let #(updated, effects) =
          pool.holder_down(pool_of(state, kind), fn(claimant) {
            claimant.pid == pid
          })
        apply_effects(set_pool(state, kind, updated), kind, effects)
      })
    }
  }
}

fn apply_effects(
  state: State,
  kind: PoolKind,
  effects: List(pool.Effect(Claimant)),
) -> State {
  list.fold(effects, state, fn(state, effect) {
    case effect {
      pool.Granted(token:, holder:) -> {
        let _ =
          ets_insert(table(), #(
            #(kind, token),
            holder.pid,
            holder.reply,
            True,
            clock.monotonic_ms(),
          ))
        process.send(holder.reply, Granted(token))
        State(..state, by_pid: add_owner(state.by_pid, holder.pid, kind, token))
      }
      pool.Queued(token:, holder:) -> {
        let _ =
          ets_insert(table(), #(
            #(kind, token),
            holder.pid,
            holder.reply,
            False,
            clock.monotonic_ms(),
          ))
        process.send(holder.reply, Queued(token))
        State(..state, by_pid: add_owner(state.by_pid, holder.pid, kind, token))
      }
      pool.Dropped(token:, holder:, reason:) -> {
        let _ = ets_delete(table(), #(kind, token))
        process.send(holder.reply, Dropped(token, reason))
        let counter = case reason {
          pool.QueueFull -> "push_node_permit_queue_full_total"
          pool.WaitExpired -> "push_permit_wait_expired_total"
        }
        metrics.count_labelled(counter, [#("pool", pool_label(kind))], 1)
        forget_token(state, kind, token)
      }
    }
  })
}

fn add_owner(
  owners: Dict(Pid, List(#(PoolKind, Int))),
  pid: Pid,
  kind: PoolKind,
  token: Int,
) -> Dict(Pid, List(#(PoolKind, Int))) {
  dict.upsert(owners, pid, fn(existing) {
    let rest =
      option.unwrap(existing, [])
      |> list.filter(fn(entry) { entry != #(kind, token) })
    [#(kind, token), ..rest]
  })
}

fn forget_token(state: State, kind: PoolKind, token: Int) -> State {
  State(
    ..state,
    by_pid: dict.map_values(state.by_pid, fn(_, owned) {
      list.filter(owned, fn(entry) { entry != #(kind, token) })
    }),
  )
}

fn watch(state: State, pid: Pid) -> State {
  case dict.has_key(state.by_pid, pid) {
    True -> state
    False -> {
      let _ = process.monitor(pid)
      State(..state, by_pid: dict.insert(state.by_pid, pid, []))
    }
  }
}

fn pool_of(state: State, kind: PoolKind) -> Pool(Claimant) {
  case kind {
    GuildWide -> state.guild_wide
    SettingsFetch -> state.settings_fetch
  }
}

fn set_pool(state: State, kind: PoolKind, updated: Pool(Claimant)) -> State {
  case kind {
    GuildWide -> State(..state, guild_wide: updated)
    SettingsFetch -> State(..state, settings_fetch: updated)
  }
}

fn pool_label(kind: PoolKind) -> String {
  case kind {
    GuildWide -> "guild_wide"
    SettingsFetch -> "settings_fetch"
  }
}

type Awaited {
  Answered(Reply)
  ServerDown
}

pub fn acquire(kind: PoolKind, wait_ms: Int) -> Result(Permit, DropReason) {
  let reply = process.new_subject()
  let token = unique_integer([Positive, Monotonic])
  case process.named(server_name()) {
    Error(Nil) -> {
      metrics.count_labelled(
        "push_permit_server_unavailable_total",
        [#("pool", pool_label(kind))],
        1,
      )
      Ok(Unmanaged)
    }
    Ok(server) -> {
      let monitor = send_claim(server, kind, token, reply)
      await(kind, reply, token, monitor, clock.deadline_in(wait_ms))
    }
  }
}

fn send_claim(
  server: Pid,
  kind: PoolKind,
  token: Int,
  reply: Subject(Reply),
) -> process.Monitor {
  let monitor = process.monitor(server)
  named.send_to(
    server,
    server_name(),
    Claim(pool: kind, token:, holder: process.self(), reply:),
  )
  monitor
}

fn await(
  kind: PoolKind,
  reply: Subject(Reply),
  token: Int,
  monitor: process.Monitor,
  deadline: clock.Deadline,
) -> Result(Permit, DropReason) {
  let selector =
    process.new_selector()
    |> process.select_map(reply, Answered)
    |> process.select_specific_monitor(monitor, fn(_) { ServerDown })
  case process.selector_receive(selector, clock.remaining_ms(deadline)) {
    Ok(Answered(Queued(_))) -> await(kind, reply, token, monitor, deadline)
    Ok(Answered(Granted(_))) -> {
      process.demonitor_process(monitor)
      Ok(Held(kind, token))
    }
    Ok(Answered(Dropped(_, reason))) -> {
      process.demonitor_process(monitor)
      case reason {
        pool.QueueFull -> Error(NodeQueueFull)
        pool.WaitExpired -> Error(PermitWaitExpired)
      }
    }
    Ok(ServerDown) ->
      case next_server(deadline) {
        Ok(server) ->
          await(
            kind,
            reply,
            token,
            send_claim(server, kind, token, reply),
            deadline,
          )
        Error(Nil) -> give_up(kind, token)
      }
    Error(Nil) -> {
      process.demonitor_process(monitor)
      give_up(kind, token)
    }
  }
}

fn give_up(kind: PoolKind, token: Int) -> Result(Permit, DropReason) {
  send_release(kind, token)
  Error(PermitTimeout)
}

fn next_server(deadline: clock.Deadline) -> Result(Pid, Nil) {
  case process.named(server_name()) {
    Ok(server) -> Ok(server)
    Error(Nil) ->
      case clock.remaining_ms(deadline) {
        0 -> Error(Nil)
        remaining -> {
          process.sleep(int.min(remaining, 5))
          next_server(deadline)
        }
      }
  }
}

pub fn release(permit: Permit) -> Nil {
  case permit {
    Held(pool: kind, token:) -> send_release(kind, token)
    Unmanaged -> Nil
  }
}

fn send_release(kind: PoolKind, token: Int) -> Nil {
  let deadline = clock.deadline_in(release_timeout_ms())
  case process.named(server_name()) {
    Error(Nil) -> {
      let _ =
        process.spawn_unlinked(fn() { release_until(kind, token, deadline) })
      Nil
    }
    Ok(server) -> {
      let tag = make_ref()
      let watcher =
        process.spawn_unlinked(fn() {
          watch_release(server, kind, token, tag, deadline)
        })
      named.send_to(
        server,
        server_name(),
        Release(kind, token, process.unsafely_create_subject(watcher, tag)),
      )
    }
  }
}

fn watch_release(
  server: Pid,
  kind: PoolKind,
  token: Int,
  tag: Dynamic,
  deadline: clock.Deadline,
) -> Nil {
  let monitor = process.monitor(server)
  let ack: Subject(Nil) = process.unsafely_create_subject(process.self(), tag)
  let selector =
    process.new_selector()
    |> process.select_map(ack, fn(_) { True })
    |> process.select_specific_monitor(monitor, fn(_) { False })
  case process.selector_receive(selector, clock.remaining_ms(deadline)) {
    Ok(False) -> release_until(kind, token, deadline)
    Ok(True) | Error(Nil) -> Nil
  }
}

fn release_until(kind: PoolKind, token: Int, deadline: clock.Deadline) -> Nil {
  case next_server(deadline) {
    Error(Nil) -> Nil
    Ok(server) -> {
      let ack = process.new_subject()
      let monitor = process.monitor(server)
      named.send_to(server, server_name(), Release(kind, token, ack))
      let selector =
        process.new_selector()
        |> process.select_map(ack, fn(_) { True })
        |> process.select_specific_monitor(monitor, fn(_) { False })
      case process.selector_receive(selector, clock.remaining_ms(deadline)) {
        Ok(True) -> process.demonitor_process(monitor)
        Ok(False) -> release_until(kind, token, deadline)
        Error(Nil) -> process.demonitor_process(monitor)
      }
    }
  }
}
