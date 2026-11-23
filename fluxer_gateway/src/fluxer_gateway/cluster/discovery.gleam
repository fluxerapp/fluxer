// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/metrics
import fluxer_gateway/cluster/remote
import fluxer_gateway/config/durations
import fluxer_gateway/config/env
import fluxer_gateway/config/node_name
import fluxer_gateway/config/settings.{type Settings}
import fluxer_gateway/core/node.{type Node}
import gleam/erlang/atom
import gleam/erlang/charlist.{type Charlist}
import gleam/erlang/process.{type Pid, type Subject}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/otp/supervision.{type ChildSpecification}
import gleam/result
import gleam/string
import logging

fn init_timeout_ms() -> Int {
  durations.cluster().discovery_init_timeout_ms
}

fn default_poll_interval_ms() -> Int {
  durations.cluster().discovery_default_poll_interval_ms
}

const default_node_basename = "fluxer_gateway"

const max_discovered_peers = 512

pub type PeersChanged {
  PeersChanged(peers: List(Node))
}

pub opaque type Message {
  Poll
  Refresh
  Subscribe(Subject(PeersChanged))
  SubscriberDown(process.Down)
}

type Source {
  StaticPeers(List(Node))
  DnsName(name: String, basename: String)
  NoSource
}

type State {
  State(
    self: Subject(Message),
    source: Source,
    poll_interval_ms: Int,
    peers: List(Node),
    subscribers: List(#(Pid, Subject(PeersChanged))),
  )
}

type Key {
  FluxerGatewayClusterPeers
}

type Stored

type IpAddress

type Family {
  Inet
}

type DnsClass {
  In
}

type RecordType {
  A
}

type ResolveFailure

@external(erlang, "persistent_term", "get")
fn get_peers(key: Key, default: List(Node)) -> List(Node)

@external(erlang, "persistent_term", "put")
fn put_peers(key: Key, value: List(Node)) -> Stored

@external(erlang, "inet", "getaddrs")
fn getaddrs(
  name: Charlist,
  family: Family,
) -> Result(List(IpAddress), ResolveFailure)

@external(erlang, "inet_res", "lookup")
fn res_lookup(
  name: Charlist,
  class: DnsClass,
  kind: RecordType,
) -> List(IpAddress)

@external(erlang, "inet", "ntoa")
fn ntoa(address: IpAddress) -> Charlist

fn name() -> process.Name(Message) {
  env.process_name("fluxer_gateway_cluster_discovery")
}

pub fn supervised(settings: Settings) -> ChildSpecification(Nil) {
  supervision.worker(fn() { start(settings) })
}

fn start(settings: Settings) -> Result(actor.Started(Nil), actor.StartError) {
  actor.new_with_initialiser(init_timeout_ms(), fn(named) {
    let self = process.new_subject()
    let state =
      State(
        self:,
        source: source_from(settings),
        poll_interval_ms: positive_or(
          settings.cluster_discovery_poll_interval_ms,
          default_poll_interval_ms(),
        ),
        peers: peers(),
        subscribers: [],
      )
    put_peers(FluxerGatewayClusterPeers, state.peers)
    process.send(self, Poll)
    let selector =
      process.new_selector()
      |> process.select(named)
      |> process.select(self)
      |> process.select_monitors(SubscriberDown)
    state
    |> actor.initialised
    |> actor.selecting(selector)
    |> Ok
  })
  |> actor.named(name())
  |> actor.on_message(handle)
  |> actor.start
}

fn peers() -> List(Node) {
  get_peers(FluxerGatewayClusterPeers, [])
}

pub fn subscribe(to subscriber: Subject(PeersChanged)) -> Result(Pid, Nil) {
  use pid <- result.try(process.named(name()))
  use Nil <- result.map(remote.send_local(name(), Subscribe(subscriber)))
  pid
}

pub fn refresh() -> Nil {
  send(Refresh)
}

fn send(message: Message) -> Nil {
  let _ = remote.send_local(name(), message)
  Nil
}

fn handle(state: State, message: Message) -> actor.Next(State, Message) {
  case message {
    Poll -> {
      let state = poll(state)
      process.send_after(state.self, state.poll_interval_ms, Poll)
      actor.continue(state)
    }
    Refresh -> actor.continue(poll(state))
    Subscribe(subscriber) -> actor.continue(add_subscriber(state, subscriber))
    SubscriberDown(process.ProcessDown(pid:, ..)) ->
      actor.continue(
        State(
          ..state,
          subscribers: list.filter(state.subscribers, fn(entry) {
            entry.0 != pid
          }),
        ),
      )
    SubscriberDown(process.PortDown(..)) -> actor.continue(state)
  }
}

fn add_subscriber(state: State, subscriber: Subject(PeersChanged)) -> State {
  case process.subject_owner(subscriber) {
    Error(Nil) -> state
    Ok(pid) ->
      case list.any(state.subscribers, fn(entry) { entry.0 == pid }) {
        True -> state
        False -> {
          let _ = process.monitor(pid)
          process.send(subscriber, PeersChanged(state.peers))
          State(..state, subscribers: [#(pid, subscriber), ..state.subscribers])
        }
      }
  }
}

fn poll(state: State) -> State {
  case state.source {
    NoSource -> state
    StaticPeers(found) -> apply(state, without_self(found))
    DnsName(dns_name, basename) ->
      case resolve(dns_name) {
        Ok(addresses) ->
          case nodes_from_addresses(addresses, basename) {
            [] -> {
              metrics.record_discovery_resolve_failure()
              logging.log(
                logging.Warning,
                "cluster discovery: DNS resolved no nodes for "
                  <> dns_name
                  <> "; retaining previous peers",
              )
              state
            }
            nodes -> apply(state, without_self(nodes))
          }
        Error(reason) -> {
          metrics.record_discovery_resolve_failure()
          logging.log(
            logging.Warning,
            "cluster discovery: DNS resolve failed for "
              <> dns_name
              <> ": "
              <> reason,
          )
          state
        }
      }
  }
}

fn apply(state: State, found: List(Node)) -> State {
  case found == state.peers {
    True -> Nil
    False -> {
      put_peers(FluxerGatewayClusterPeers, found)
      Nil
    }
  }
  list.each(state.subscribers, fn(entry) {
    process.send(entry.1, PeersChanged(found))
  })
  State(..state, peers: found)
}

fn resolve(dns_name: String) -> Result(List(String), String) {
  let host = charlist.from_string(dns_name)
  case getaddrs(host, Inet) {
    Ok(addresses) -> Ok(list.map(addresses, address_text))
    Error(native) ->
      case res_lookup(host, In, A) {
        [] -> Error(string.inspect(native))
        addresses -> Ok(list.map(addresses, address_text))
      }
  }
}

fn address_text(address: IpAddress) -> String {
  charlist.to_string(ntoa(address))
}

pub fn peers_from_addresses(
  addresses: List(String),
  basename: String,
) -> List(Node) {
  without_self(nodes_from_addresses(addresses, basename))
}

fn nodes_from_addresses(
  addresses: List(String),
  basename: String,
) -> List(Node) {
  addresses
  |> list.unique
  |> list.sort(string.compare)
  |> list.take(max_discovered_peers)
  |> list.filter_map(fn(address) {
    node_name.from_parts(base: basename, host: address)
  })
  |> list.map(fn(text) { node.from_atom(atom.create(text)) })
}

pub fn static_peers(names: List(String)) -> List(Node) {
  names
  |> list.filter_map(node_name.parse)
  |> list.map(fn(text) { node.from_atom(atom.create(text)) })
  |> without_self
}

fn without_self(peers: List(Node)) -> List(Node) {
  let self = node.self()
  peers
  |> list.filter(fn(peer) { peer != self })
  |> list.unique
  |> list.sort(fn(a, b) { string.compare(node.to_string(a), node.to_string(b)) })
}

pub fn configured(settings: Settings) -> Bool {
  source_from(settings) != NoSource
}

fn source_from(settings: Settings) -> Source {
  case static_peers(settings.cluster_static_peers) {
    [_, ..] as found -> StaticPeers(found)
    [] ->
      case non_blank(settings.cluster_discovery_dns_name) {
        Some(dns_name) ->
          DnsName(
            dns_name,
            option.unwrap(
              non_blank(settings.cluster_discovery_node_basename),
              default_node_basename,
            ),
          )
        None -> NoSource
      }
  }
}

fn non_blank(value: Option(String)) -> Option(String) {
  use text <- option.then(value)
  case string.trim(text) {
    "" -> None
    trimmed -> Some(trimmed)
  }
}

fn positive_or(value: Int, default: Int) -> Int {
  case value > 0 {
    True -> value
    False -> default
  }
}
