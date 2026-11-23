// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/presence.{Idle, Online}
import fluxer_gateway/presence/client
import fluxer_gateway/presence/registry
import fluxer_gateway/presence/remote
import fluxer_gateway/presence/route
import gleam/erlang/atom
import gleam/list
import gleam/option.{None}
import gleeunit/should
import support/presence_scenario as scn

fn ghost() -> atom.Atom {
  atom.create("presence_ghost@nowhere")
}

fn to_registry(user: Int, message: registry.RegistryMsg) -> Nil {
  let assert Ok(Nil) =
    remote.send(
      remote.self_atom(),
      route.name(route.Registry, route.shard(user)),
      message,
    )
  Nil
}

fn has_ghost(user: Int) -> Bool {
  list.contains(registry.interest(user), ghost())
}

pub fn remote_interest_survives_owned_presence_stop_test() {
  let user = 7_600_000_000_000_001
  to_registry(user, registry.InterestAdd(ghost(), [user], None))
  scn.wait_until(fn() { has_ghost(user) }, 1000) |> should.be_true
  let session = scn.fake_session(user, 1)
  let link = scn.connect(session, Online, [])
  let assert Ok(pid) = scn.presence_pid(user)
  client.session_disconnect(link)
  scn.wait_down(pid, 2000) |> should.be_true
  scn.wait_until(fn() { scn.presence_inbox(user) == Error(Nil) }, 1000)
  |> should.be_true
  scn.sync_registry(user)
  has_ghost(user) |> should.be_true
  to_registry(user, registry.InterestRemove(ghost(), [user]))
  scn.wait_until(fn() { !has_ghost(user) }, 1000) |> should.be_true
}

pub fn unreachable_interest_does_not_block_local_delivery_test() {
  let user = 7_600_000_000_000_002
  let guild = 7_600_000_000_000_902
  to_registry(user, registry.InterestAdd(ghost(), [user], None))
  scn.wait_until(fn() { has_ghost(user) }, 1000) |> should.be_true
  let observer = scn.observe(guild, [user])
  let session = scn.fake_session(user, 2)
  let link = scn.connect(session, Online, [])
  let assert Ok(_) = scn.next_presence(observer, user, 2000)
  client.session_update(link, Idle, False, False, None)
  let assert Ok(found) = scn.next_presence(observer, user, 2000)
  presence.status(found) |> should.equal(Idle)
  client.session_disconnect(link)
  to_registry(user, registry.InterestRemove(ghost(), [user]))
  scn.stop_observing(guild)
}

pub fn topology_rebuild_forgets_interest_from_departed_node_test() {
  let user = 7_600_000_000_000_003
  to_registry(user, registry.InterestAdd(ghost(), [user], None))
  scn.wait_until(fn() { has_ghost(user) }, 1000) |> should.be_true
  to_registry(user, registry.RouteBuilt)
  scn.wait_until(fn() { !has_ghost(user) }, 1000) |> should.be_true
}
