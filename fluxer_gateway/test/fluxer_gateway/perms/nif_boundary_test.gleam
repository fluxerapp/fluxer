// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/packed_ids
import fluxer_gateway/core/permissions
import fluxer_gateway/nif
import fluxer_gateway/perms
import fluxer_gateway/perms/audience
import fluxer_gateway/perms/config as perm_config
import fluxer_gateway/perms/event
import fluxer_gateway/perms/native
import fluxer_gateway/perms/query
import fluxer_gateway/perms/snapshot
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/list
import gleam/option.{Some}
import gleam/result
import gleam/set
import gleam/string
import support/nif_probe
import support/perms_ml_fixture as f
import support/wait

const everyone = 100

const secret_role = 200

const public = 11

const gated = 12

fn grants() -> Int {
  f.view + 2048 + f.view_members
}

fn roles() {
  [f.role(everyone, grants(), 0), f.role(secret_role, 0, 1)]
}

fn hidden() {
  f.text(gated, [
    f.deny_role(everyone, f.view),
    f.allow_role(secret_role, f.view),
  ])
}

fn opened() {
  f.text(gated, [])
}

fn roles_of(user: Int) -> List(Int) {
  case user % 2 {
    0 -> [secret_role]
    _ -> []
  }
}

fn source(total: Int) -> snapshot.Snapshot {
  int.range(
    1,
    total + 1,
    f.base(roles(), [f.text(public, []), hidden()]),
    fn(acc, user) { snapshot.add_member(acc, f.member(user, roles_of(user))) },
  )
}

fn tuned(total: Int, budget: Int, chunk: Int) -> #(perms.Perms, perms.Reader) {
  let assert Ok(Nil) = nif.load()
  let tuning =
    perm_config.default()
    |> perm_config.set_work_budget_units(budget)
    |> perm_config.set_scan_chunk(chunk)
  let #(handle, reader) = perms.new_with(f.guild(100), process.self(), tuning)
  let assert Ok(_) = perms.load(handle, source(total))
  #(handle, reader)
}

fn toggle(handle: perms.Perms, open: Bool) -> Nil {
  let channel = case open {
    True -> opened()
    False -> hidden()
  }
  let assert Ok(_) =
    perms.apply(handle, [event.channel_upsert(channel, event.Updated)])
  Nil
}

fn churn(handle: perms.Perms, rounds: Int) -> Nil {
  int.range(0, rounds, Nil, fn(_, round) { toggle(handle, round % 2 == 0) })
  toggle(handle, False)
}

fn describe(error: perms.PermsError) -> String {
  case error {
    perms.NotReady -> "not_ready"
    perms.Broken -> "broken"
    perms.Gone -> "gone"
    perms.BadInput(reason) -> "bad_input " <> reason
  }
}

fn check(
  found: Result(a, perms.PermsError),
  label: String,
  valid: fn(a) -> Bool,
) -> List(String) {
  case found {
    Ok(value) ->
      case valid(value) {
        True -> []
        False -> [label <> " " <> string.inspect(value)]
      }
    Error(error) -> [label <> " " <> describe(error)]
  }
}

fn point_reads(reader: perms.Reader, total: Int, round: Int) -> List(String) {
  let member = 2 * { 1 + round % { total / 2 } }
  let outsider = member - 1
  let gated_id = f.channel_id(gated)
  let view_bit = fn(bits) {
    int.bitwise_and(permissions.to_int(bits), f.view) == f.view
  }
  let half = total / 2
  list.flatten([
    check(
      perms.permissions(reader, f.user_id(member), perms.ChannelScope(gated_id)),
      "permissions",
      view_bit,
    ),
    check(
      perms.can_view(reader, f.user_id(member), gated_id),
      "can_view",
      fn(seen) { seen },
    ),
    check(
      perms.can_view(reader, f.user_id(outsider), gated_id),
      "outsider",
      fn(_) { True },
    ),
    check(
      perms.filter_viewers(
        reader,
        gated_id,
        packed_ids.from_list([member, outsider]),
      ),
      "filter_viewers",
      fn(kept) {
        let kept = list.sort(packed_ids.to_list(kept), int.compare)
        kept == [member] || kept == [outsider, member]
      },
    ),
    perms.viewers(reader, gated_id)
      |> result.map(fn(found) { list.length(packed_ids.to_list(found)) })
      |> check("viewers", fn(count) { count == half || count == total }),
    check(perms.audience(reader, gated_id, audience.View), "audience", fn(_) {
      True
    }),
    check(
      perms.reply_json(
        reader,
        query.UserPermissions(f.user_id(member), Some(gated_id)),
      ),
      "reply_json",
      fn(_) { True },
    ),
    check(perms.group_of(reader, f.user_id(member)), "group_of", fn(_) { True }),
  ])
}

fn hammer(
  workers: Int,
  rounds: Int,
  work: fn(Int) -> List(String),
) -> Subject(List(String)) {
  let results = process.new_subject()
  int.range(0, workers, Nil, fn(_, worker) {
    process.spawn(fn() {
      let found =
        int.range(0, rounds, [], fn(acc, round) {
          list.append(work(worker * rounds + round), acc)
        })
      process.send(results, found)
    })
    Nil
  })
  results
}

fn collect(results: Subject(List(String)), workers: Int) -> List(String) {
  int.range(0, workers, [], fn(acc, _) {
    let assert Ok(found) = process.receive(results, 120_000)
    list.append(found, acc)
  })
}

pub fn point_reads_from_many_processes_see_whole_epochs_test() {
  let total = 2000
  let #(handle, reader) = f.loaded(source(total))
  let results = hammer(32, 40, fn(round) { point_reads(reader, total, round) })
  churn(handle, 200)
  assert collect(results, 32) == []
  perms.release(handle)
}

pub fn chunked_viewers_never_mix_two_epochs_test() {
  let total = 4000
  let #(handle, reader) = tuned(total, 1000, 64)
  let before = nif_probe.dirty_calls()
  let results =
    hammer(8, 40, fn(_) {
      perms.viewers(reader, f.channel_id(gated))
      |> result.map(fn(found) { list.length(packed_ids.to_list(found)) })
      |> check("viewers", fn(count) { count == total / 2 || count == total })
    })
  let runner = process.new_subject()
  process.spawn(fn() {
    churn(handle, 400)
    process.send(runner, Nil)
  })
  let found = collect(results, 8)
  let assert Ok(Nil) = process.receive(runner, 120_000)
  assert found == []
  assert nif_probe.dirty_calls() > before
  perms.release(handle)
}

pub fn reads_over_budget_move_to_dirty_schedulers_test() {
  let total = 4000
  let #(handle, reader) = tuned(total, 1000, 4096)
  let channel = f.channel_id(gated)
  assert native.perm_viewers(nif_probe.reader_handle(reader), gated)
    == Error(native.NeedsDirty)
  let before = nif_probe.dirty_calls()
  let assert Ok(found) = perms.viewers(reader, channel)
  assert list.length(packed_ids.to_list(found)) == total / 2
  assert nif_probe.dirty_calls() > before
  toggle(handle, True)
  let assert Ok(found) = perms.viewers(reader, channel)
  assert list.length(packed_ids.to_list(found)) == total
  perms.release(handle)
}

pub fn release_under_concurrent_reads_answers_gone_test() {
  let total = 2000
  let #(handle, reader) = f.loaded(source(total))
  let results =
    hammer(24, 60, fn(round) {
      point_reads(reader, total, round)
      |> list.filter(fn(problem) { !string.ends_with(problem, " gone") })
    })
  process.sleep(5)
  perms.release(handle)
  assert collect(results, 24) == []
  assert perms.permissions(
      reader,
      f.user_id(2),
      perms.ChannelScope(f.channel_id(gated)),
    )
    == Error(perms.Gone)
  assert perms.viewers(reader, f.channel_id(gated)) == Error(perms.Gone)
  perms.release(handle)
}

pub fn owner_killed_mid_read_releases_the_index_test() {
  let total = 2000
  let handed = process.new_subject()
  let owner =
    process.spawn_unlinked(fn() {
      let #(handle, reader) = f.loaded(source(total))
      process.send(handed, #(handle, reader))
      process.sleep_forever()
    })
  let assert Ok(#(handle, reader)) = process.receive(handed, 30_000)
  let live = nif_probe.metric("perm_index_live")
  let results =
    hammer(16, 60, fn(round) {
      point_reads(reader, total, round)
      |> list.filter(fn(problem) { !string.ends_with(problem, " gone") })
    })
  process.sleep(5)
  process.kill(owner)
  assert collect(results, 16) == []
  assert wait.until(2000, fn() {
    perms.viewers(reader, f.channel_id(gated)) == Error(perms.Gone)
  })
  assert perms.apply(handle, [event.channel_upsert(opened(), event.Updated)])
    == Error(perms.Gone)
  assert nif_probe.metric("perm_index_live") <= live
}

pub fn readers_survive_a_second_handle_built_beside_them_test() {
  let total = 2000
  let #(old_handle, old_reader) = f.loaded(source(total))
  let results =
    hammer(16, 40, fn(round) {
      point_reads(old_reader, total, round)
      |> list.filter(fn(problem) { !string.ends_with(problem, " gone") })
    })
  let #(fresh, fresh_reader) = f.loaded(source(total))
  churn(fresh, 50)
  perms.release(old_handle)
  assert collect(results, 16) == []
  let seen =
    int.range(0, 50, set.new(), fn(acc, round) {
      case point_reads(fresh_reader, total, round) {
        [] -> acc
        problems -> set.union(acc, set.from_list(problems))
      }
    })
  assert set.to_list(seen) == []
  perms.release(fresh)
}
