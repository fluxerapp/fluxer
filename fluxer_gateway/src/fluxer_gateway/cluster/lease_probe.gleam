// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/worker.{type Roster}
import fluxer_gateway/cluster/lease.{type ProbeAnswer, NoBouncerMajority}
import fluxer_gateway/cluster/links
import fluxer_gateway/cluster/remote
import fluxer_gateway/config/durations
import fluxer_gateway/config/settings.{type Settings}
import fluxer_gateway/core/clock
import fluxer_gateway/core/node.{type Node}
import gleam/dict.{type Dict}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Name, type Subject}
import gleam/int
import gleam/list
import gleam/otp/actor
import gleam/otp/supervision.{type ChildSpecification}
import gleam/result
import gleam/string
import logging

pub type Probe {
  Probe(from: Atom, reply: Subject(ProbeReply))
}

pub type ProbeReply {
  ProbeReply(from: Atom, answer: ProbeAnswer)
}

@external(erlang, "erlang", "binary_to_atom")
fn probe_name_atom(name: String) -> Name(Probe)

pub fn name() -> Name(Probe) {
  probe_name_atom("fluxer_gateway_lease_probe")
}

pub fn supervised(_settings: Settings) -> ChildSpecification(Nil) {
  supervision.worker(start)
}

pub fn start() -> Result(actor.Started(Nil), actor.StartError) {
  actor.new(Nil)
  |> actor.named(name())
  |> actor.on_message(fn(state, probe: Probe) {
    process.send(
      probe.reply,
      ProbeReply(node.to_atom(node.self()), lease.probe_answer()),
    )
    actor.continue(state)
  })
  |> actor.start
  |> result.map(fn(started) { actor.Started(pid: started.pid, data: Nil) })
}

pub fn round(rosters: List(Roster)) -> Bool {
  let self = node.to_atom(node.self())
  let peers =
    rosters
    |> list.flat_map(fn(roster) { roster.members })
    |> list.map(fn(member) { member.node })
    |> list.unique
    |> list.filter(fn(peer) { peer != self })
  let reply = process.new_subject()
  list.each(peers, fn(peer) {
    process.spawn_unlinked(fn() { ask(node.from_atom(peer), self, reply) })
  })
  let answers =
    collect(
      reply,
      dict.new(),
      list.length(peers),
      clock.deadline_in(durations.bouncer().probe_timeout_ms),
    )
  let isolated = verdict(rosters, self, answers)
  case isolated {
    True -> Nil
    False ->
      logging.log(
        logging.Warning,
        "lease probe found no isolated roster majority: rosters="
          <> string.inspect(
          list.map(rosters, fn(roster) { list.length(roster.members) }),
        )
          <> " asked="
          <> int.to_string(list.length(peers))
          <> " answers="
          <> string.inspect(dict.values(answers)),
      )
  }
  isolated
}

fn ask(peer: Node, self: Atom, reply: Subject(ProbeReply)) -> Nil {
  case links.ensure(peer) {
    True -> {
      let _ = remote.deliver_named(peer, name(), Probe(self, reply))
      Nil
    }
    False -> Nil
  }
}

fn collect(
  reply: Subject(ProbeReply),
  answers: Dict(Atom, ProbeAnswer),
  waiting: Int,
  deadline: clock.Deadline,
) -> Dict(Atom, ProbeAnswer) {
  case dict.size(answers) >= waiting {
    True -> answers
    False ->
      case process.receive(reply, clock.remaining_ms(deadline)) {
        Ok(ProbeReply(from:, answer:)) ->
          collect(reply, dict.insert(answers, from, answer), waiting, deadline)
        Error(Nil) -> answers
      }
  }
}

pub fn verdict(
  rosters: List(Roster),
  self: Atom,
  answers: Dict(Atom, ProbeAnswer),
) -> Bool {
  let rosters = list.filter(rosters, fn(roster) { roster.members != [] })
  rosters != []
  && list.all(rosters, fn(roster) {
    let isolated =
      list.count(roster.members, fn(member) {
        member.node == self
        || dict.get(answers, member.node) == Ok(NoBouncerMajority)
      })
    isolated >= worker.majority_of(roster)
  })
}
