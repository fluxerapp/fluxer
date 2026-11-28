// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/codec_range
import bouncer_proto/key
import bouncer_proto/worker.{
  type Entry, Draining, Entry, Profile, Ready, WorkerId,
}
import fluxer_gateway/cluster/lease
import fluxer_gateway/cluster/links
import fluxer_gateway/cluster/wire
import fluxer_gateway/config/build_info
import fluxer_gateway/config/settings
import fluxer_gateway/core/node
import fluxer_gateway/handoff/service
import fluxer_gateway/telemetry/metrics
import gleam/erlang/atom
import gleam/erlang/process
import gleam/list
import gleam/option.{Some}
import gleam/string

fn profile(role: String, epoch: Int) {
  Profile(role, "default", "dev", "t", epoch, [])
}

fn peer(name: String, role: String, epoch: Int, status) {
  Entry(WorkerId(atom.create(name), 1), profile(role, epoch), status, 1)
}

fn me(epoch: Int) {
  Entry(
    WorkerId(node.to_atom(node.self()), lease.incarnation()),
    profile("guilds", epoch),
    Ready,
    1,
  )
}

fn refused_count(local: Int, peer: Int) -> Int {
  metrics.value(metrics.read_all(), wire.refused_metric, [
    #("local_epoch", string.inspect(local)),
    #("peer_epoch", string.inspect(peer)),
  ])
}

pub fn a_roll_to_the_next_bridged_epoch_accepts_both_sides_test() {
  let old = me(5)
  let new = peer("wire-new@h", "guilds", 6, Ready)
  assert wire.assess(old, [old, new]) == wire.Accepted
  assert wire.assess(Entry(..new, status: Ready), [
      new,
      peer("wire-old@h", "guilds", 5, Ready),
    ])
    == wire.Accepted
}

pub fn a_gap_of_two_epochs_or_a_pre_bridge_neighbour_is_a_mismatch_test() {
  let mine = me(7)
  assert wire.assess(mine, [mine, peer("wire-a@h", "guilds", 5, Ready)])
    == wire.Mismatch(local: 7, peer: 5)
  let legacy = me(4)
  assert wire.assess(legacy, [legacy, peer("wire-b@h", "sessions", 3, Ready)])
    == wire.Mismatch(local: 4, peer: 3)
}

pub fn mismatches_count_every_role_but_only_ready_peers_test() {
  let mine = me(7)
  let peers = [
    mine,
    peer("wire-c@h", "websocket+sessions", 5, Ready),
    peer("wire-d@h", "guilds", 4, Draining),
    peer("wire-e@h", "presence", 6, Ready),
    peer("wire-f@h", "presence", 9, Ready),
  ]
  let names =
    wire.refused(mine, peers)
    |> list.map(fn(found) { atom.to_string(found.worker.node) })
  assert names == ["wire-c@h", "wire-f@h"]
}

pub fn readiness_refuses_a_mismatch_and_latches_once_accepted_test() {
  unlatch()
  let _ = links.replace([])
  assert service.wire_readiness() == wire.Pending
  let mine = me(build_info.wire_epoch)
  let stranger = peer("wire-g@h", "guilds", build_info.wire_epoch + 2, Ready)
  assert links.replace([mine, stranger])
  assert service.wire_readiness()
    == wire.Mismatch(build_info.wire_epoch, build_info.wire_epoch + 2)
  assert wire.current()
    == wire.Mismatch(build_info.wire_epoch, build_info.wire_epoch + 2)
  assert links.replace([mine])
  assert service.wire_readiness() == wire.Accepted
  assert links.replace([mine, stranger])
  assert service.wire_readiness() == wire.Accepted
  assert wire.current()
    == wire.Mismatch(build_info.wire_epoch, build_info.wire_epoch + 2)
  let _ = links.replace([])
  unlatch()
  Nil
}

pub fn a_refused_peer_is_counted_and_logged_once_test() {
  let mine = me(build_info.wire_epoch)
  let far = build_info.wire_epoch + 3
  let before = refused_count(build_info.wire_epoch, far)
  let entries = [
    mine,
    peer("wire-h@h", "guilds", far, Ready),
    peer("wire-i@h", "presence", far, Ready),
    peer("wire-j@h", "guilds", build_info.wire_epoch, Ready),
  ]
  assert wire.observe(entries) == 2
  assert wire.observe(entries) == 0
  assert refused_count(build_info.wire_epoch, far) == before + 2
}

pub fn the_old_form_goes_to_peers_below_the_bridged_epoch_test() {
  let _ = links.replace([])
  assert links.replace([
    me(build_info.wire_epoch),
    peer("wire-l@h", "guilds", 5, Ready),
    peer("wire-m@h", "guilds", 6, Ready),
  ])
  let old = node.from_atom(atom.create("wire-l@h"))
  let new = node.from_atom(atom.create("wire-m@h"))
  let unknown = node.from_atom(atom.create("wire-n@h"))
  assert !wire.speaks(old, 6)
  assert wire.speaks(new, 6)
  assert wire.speaks(node.self(), build_info.wire_epoch)
  assert wire.epoch_of(unknown) == build_info.wire_epoch
  assert wire.pid_speaks(process.self(), build_info.wire_epoch)
  let _ = links.replace([])
  Nil
}

@external(erlang, "persistent_term", "erase")
fn erase(key: atom.Atom) -> Bool

fn unlatch() -> Nil {
  let _ = erase(atom.create("fluxer_gateway_wire_accepted"))
  Nil
}

pub fn push_jobs_are_handed_only_to_a_push_peer_that_speaks_the_move_forms_test() {
  let _ = links.replace([])
  let push = codec_range.unmovable(key.push)
  let with_codecs = fn(found: Entry, codecs) {
    Entry(..found, profile: Profile(..found.profile, codecs:))
  }
  let mine = with_codecs(me(build_info.wire_epoch), [push])
  let old = with_codecs(peer("wire-p4@h", "push", 4, Ready), [push])
  let new = with_codecs(peer("wire-p5@h", "push", 5, Ready), [push])
  let guilds = peer("wire-g5@h", "guilds", 5, Ready)
  assert links.replace([mine, old, new, guilds])
  assert !service.accepts_push_jobs(mine, old)
  assert service.accepts_push_jobs(mine, new)
  assert !service.accepts_push_jobs(mine, guilds)
  assert !service.accepts_push_jobs(mine, mine)
  let _ = links.replace([])
  Nil
}

pub fn an_unknown_peer_outside_bouncer_mode_is_taken_as_the_current_epoch_test() {
  let _ = links.replace([])
  let unknown = node.from_atom(atom.create("wire-q@h"))
  assert wire.epoch_of(unknown) == build_info.wire_epoch
  assert wire.speaks(unknown, wire.move_forms_epoch)
}

pub fn an_unknown_peer_in_bouncer_mode_is_taken_as_the_floor_test() {
  let _ = links.replace([])
  let original = settings.get()
  settings.store(
    settings.Settings(
      ..original,
      bouncer: Some(settings.BouncerSettings(
        dns_name: "gateway-bouncer.test",
        basename: "gateway-bouncer",
        count: 1,
        pool: "default",
        template_hash: "abc",
      )),
    ),
  )
  let unknown = node.from_atom(atom.create("wire-r@h"))
  let found = wire.epoch_of(unknown)
  let spoken = wire.speaks(unknown, wire.guild_lost_epoch)
  settings.store(original)
  assert found == wire.floor()
  assert !spoken
}
