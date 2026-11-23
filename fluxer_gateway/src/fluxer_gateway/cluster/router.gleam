// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/key as bucket_key
import fluxer_gateway/cluster/buckets
import fluxer_gateway/cluster/lease
import fluxer_gateway/cluster/links
import fluxer_gateway/cluster/liveness
import fluxer_gateway/cluster/metrics
import fluxer_gateway/cluster/topology.{type Topology}
import fluxer_gateway/config/durations
import fluxer_gateway/config/settings
import fluxer_gateway/core/clock
import fluxer_gateway/core/id.{type ChannelId, type GuildId}
import fluxer_gateway/core/node.{type Node, type Role}
import fluxer_gateway/util/table
import gleam/erlang/process.{type Pid, type Subject}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set

pub type Owner {
  Local
  Remote(Node)
  OwnerUnavailable(retry_after_ms: Int)
}

pub type OwnerKey {
  GuildKey(GuildId)
  CallKey(ChannelId)
  PushKey(Int)
  SessionKey(String)
  PresenceKey(Int)
}

pub type TopologyChanged {
  TopologyChanged(version: Int)
}

type Key {
  FluxerGatewayClusterTopology
}

type Stored

fn retry_low_ms() -> Int {
  durations.cluster().router_retry_low_ms
}

fn retry_high_ms() -> Int {
  durations.cluster().router_retry_high_ms
}

@external(erlang, "persistent_term", "get")
fn get_topology(key: Key, default: Option(Topology)) -> Option(Topology)

@external(erlang, "persistent_term", "put")
fn put_topology(key: Key, value: Option(Topology)) -> Stored

@external(erlang, "erlang", "phash2")
fn phash2(term: String) -> Int

@external(erlang, "rand", "uniform")
fn uniform(range: Int) -> Int

fn watchers() -> table.Table {
  table.named("fluxer_gateway_topology_watchers")
}

pub fn bouncer_mode() -> Bool {
  option.is_some(settings.get().bouncer)
}

pub fn entity_key(key: OwnerKey) -> bucket_key.Key {
  case key {
    GuildKey(guild) ->
      bucket_key.Key(bucket_key.guild, id.guild_id_to_int(guild))
    CallKey(channel) ->
      bucket_key.Key(bucket_key.call, id.channel_id_to_int(channel))
    PushKey(value) -> bucket_key.Key(bucket_key.push, value)
    SessionKey(session) -> bucket_key.Key(bucket_key.session, phash2(session))
    PresenceKey(user) -> bucket_key.Key(bucket_key.presence, user)
  }
}

fn bucket_owner(entity: bucket_key.Key) -> Owner {
  case buckets.placement(entity) {
    buckets.Here(_) ->
      case lease.valid(buckets.bucket_for(entity)) {
        Ok(_) -> Local
        Error(_) -> unavailable()
      }
    buckets.There(node: found, ..) ->
      case links.is_live(found) {
        True -> Remote(found)
        False -> unavailable()
      }
    buckets.Unplaced(retry_after_ms) -> OwnerUnavailable(retry_after_ms)
  }
}

fn with_self(nodes: List(Node), role: Role) -> List(Node) {
  case
    settings.role_runs(settings.get(), role)
    && !list.contains(nodes, node.self())
  {
    True -> [node.self(), ..nodes]
    False -> nodes
  }
}

pub fn owner(key: OwnerKey, role: Role) -> Owner {
  let found = case
    bouncer_mode(),
    get_topology(FluxerGatewayClusterTopology, None)
  {
    True, _ -> bucket_owner(entity_key(key))
    False, Some(published) -> published_owner(published, key, role)
    False, None -> standalone_owner(role)
  }
  case found {
    Local -> metrics.record_owner_resolution(True)
    Remote(_) -> metrics.record_owner_resolution(False)
    OwnerUnavailable(_) -> Nil
  }
  found
}

pub fn owner_node(key: OwnerKey, role: Role) -> Result(Node, Nil) {
  case bouncer_mode(), get_topology(FluxerGatewayClusterTopology, None) {
    True, _ ->
      case buckets.placement(entity_key(key)) {
        buckets.Here(_) -> Ok(node.self())
        buckets.There(node: found, ..) -> Ok(found)
        buckets.Unplaced(_) -> Error(Nil)
      }
    False, Some(published) -> owner_in(published, key, role)
    False, None ->
      case settings.role_runs(settings.get(), role) {
        True -> Ok(node.self())
        False -> Error(Nil)
      }
  }
}

fn published_owner(published: Topology, key: OwnerKey, role: Role) -> Owner {
  case awaits_join(published, role) {
    True -> unavailable()
    False -> joined_owner(published, key, role)
  }
}

fn awaits_join(published: Topology, role: Role) -> Bool {
  case role {
    node.Guilds | node.Presence | node.Calls -> !topology.joined(published)
    _ -> False
  }
}

pub fn joined() -> Bool {
  case bouncer_mode(), get_topology(FluxerGatewayClusterTopology, None) {
    True, _ -> links.joined()
    False, Some(published) -> topology.joined(published)
    False, None -> True
  }
}

pub fn in_service() -> Bool {
  case bouncer_mode() {
    True -> links.serving(clock.monotonic_ms())
    False -> joined()
  }
}

fn joined_owner(published: Topology, key: OwnerKey, role: Role) -> Owner {
  case owner_in(published, key, role) {
    Error(Nil) -> unavailable()
    Ok(found) ->
      case found == node.self(), liveness.is_up(found) {
        True, _ -> Local
        False, True -> Remote(found)
        False, False -> unavailable()
      }
  }
}

fn owner_in(
  published: Topology,
  key: OwnerKey,
  role: Role,
) -> Result(Node, Nil) {
  let value = key_value(key)
  case role {
    node.Guilds -> topology.guild_owner(published, value)
    node.Presence | node.Push | node.Sessions ->
      topology.live_role_owner(published, role, value, liveness.is_up)
    _ -> topology.role_owner(published, role, value)
  }
}

fn standalone_owner(role: Role) -> Owner {
  case settings.role_runs(settings.get(), role) {
    True -> Local
    False -> unavailable()
  }
}

fn unavailable() -> Owner {
  OwnerUnavailable(retry_low_ms() + uniform(retry_high_ms() - retry_low_ms()))
}

fn key_value(key: OwnerKey) -> Int {
  entity_key(key).id
}

pub fn published() -> Option(Topology) {
  get_topology(FluxerGatewayClusterTopology, None)
}

pub fn nodes_with(role: Role) -> List(Node) {
  case bouncer_mode(), get_topology(FluxerGatewayClusterTopology, None) {
    True, _ -> with_self(links.live_with(role), role)
    False, Some(published) ->
      topology.nodes_running(published, role)
      |> list.filter(liveness.is_up)
    False, None ->
      case settings.role_runs(settings.get(), role) {
        True -> [node.self()]
        False -> []
      }
  }
}

pub fn reachable_with(role: Role) -> List(Node) {
  case bouncer_mode(), get_topology(FluxerGatewayClusterTopology, None) {
    True, _ -> nodes_with(role)
    False, Some(published) ->
      topology.known_running(published, role)
      |> list.filter(liveness.is_up)
    False, None -> nodes_with(role)
  }
}

pub fn expected_with(role: Role) -> List(Node) {
  case bouncer_mode(), get_topology(FluxerGatewayClusterTopology, None) {
    True, _ -> nodes_with(role)
    False, Some(published) ->
      topology.expected(published)
      |> set.to_list
      |> list.filter(fn(peer) {
        case topology.role_of(published, peer) {
          Ok(found) -> node.runs(found, role)
          Error(Nil) -> True
        }
      })
    False, None -> nodes_with(role)
  }
}

pub fn version() -> Int {
  case bouncer_mode(), get_topology(FluxerGatewayClusterTopology, None) {
    True, _ -> links.version()
    False, Some(published) -> topology.version(published)
    False, None -> 0
  }
}

pub fn members() -> List(Node) {
  case bouncer_mode(), get_topology(FluxerGatewayClusterTopology, None) {
    True, _ -> list.unique([node.self(), ..links.known()])
    False, Some(published) -> set.to_list(topology.members(published))
    False, None -> [node.self()]
  }
}

pub fn serving() -> List(Node) {
  case bouncer_mode(), get_topology(FluxerGatewayClusterTopology, None) {
    True, _ -> with_self(links.serving_with(node.Guilds), node.Guilds)
    False, Some(published) -> topology.serving(published)
    False, None -> nodes_with(node.Guilds)
  }
}

pub fn start_allowed(guild: GuildId) -> Bool {
  case bouncer_mode(), get_topology(FluxerGatewayClusterTopology, None) {
    True, _ -> {
      let entity = entity_key(GuildKey(guild))
      case buckets.placement(entity), lease.check(buckets.bucket_for(entity)) {
        buckets.Here(_), Ok(_) -> True
        _, _ -> False
      }
    }
    False, Some(published) ->
      topology.start_allowed(
        published,
        liveness.is_up,
        id.guild_id_to_int(guild),
      )
    False, None -> True
  }
}

pub fn subscribe_topology(to subscriber: Subject(TopologyChanged)) -> Nil {
  case process.subject_owner(subscriber), table.ensure_public(watchers()) {
    Ok(pid), True -> table.insert(watchers(), #(pid, subscriber))
    _, _ -> Nil
  }
}

pub fn publish(published: Topology) -> Nil {
  put_topology(FluxerGatewayClusterTopology, Some(published))
  notify_watchers()
}

pub fn notify_watchers() -> Nil {
  let changed = TopologyChanged(version())
  let rows: List(#(Pid, Subject(TopologyChanged))) = table.to_list(watchers())
  list.each(rows, fn(row) {
    let #(pid, subscriber) = row
    case process.is_alive(pid) {
      True -> process.send(subscriber, changed)
      False -> table.delete(watchers(), pid)
    }
  })
}
