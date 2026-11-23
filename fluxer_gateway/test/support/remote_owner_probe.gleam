// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/clock
import fluxer_gateway/core/id
import fluxer_gateway/core/node
import fluxer_gateway/guild/manager as guild_manager
import fluxer_gateway/guild/manager/shard
import fluxer_gateway/guild/names
import fluxer_gateway/presence/protocol
import fluxer_gateway/presence/remote
import fluxer_gateway/presence/route
import gleam/erlang/atom
import gleam/erlang/process

pub const wait_ms = 10_000

pub fn guild_ask_ms(owner: String) -> #(Bool, Int) {
  let assert Ok(guild) = id.guild_id(4242)
  let started = clock.monotonic_ms()
  let answer =
    guild_manager.ask_shard(
      node.from_atom(atom.create(owner)),
      guild,
      shard.StartOrLookup(guild, _),
      wait_ms,
    )
  #(answer == Error(Nil), clock.monotonic_ms() - started)
}

pub fn presence_call_ms(owner: String) -> #(Bool, Int) {
  let started = clock.monotonic_ms()
  let answer =
    remote.call(
      atom.create(owner),
      route.name(route.Manager, 0),
      wait_ms,
      fn(reply) { protocol.Lookup(4242, reply) },
    )
  #(answer == Error(remote.CallUnavailable), clock.monotonic_ms() - started)
}

fn register_dying(name: process.Name(message)) -> Bool {
  let ready = process.new_subject()
  process.spawn_unlinked(fn() {
    let registered = process.register(process.self(), name)
    process.send(ready, registered == Ok(Nil))
    let _ = process.receive_forever(process.named_subject(name))
    Nil
  })
  process.receive(ready, 5000) == Ok(True)
}

pub fn shard_that_dies_on_ask() -> Bool {
  let assert Ok(guild) = id.guild_id(4242)
  register_dying(names.shard(names.shard_of(guild)))
}

pub fn manager_that_dies_on_call() -> Bool {
  register_dying(route.name(route.Manager, 0))
}
