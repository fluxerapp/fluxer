// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/core/presence
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/session
import fluxer_gateway/guild/inbox
import gleam/bit_array
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/result
import gleam/set.{type Set}
import gleam/string
import property/gen.{type Rng}
import support/perms_ml_harness as h

const guild = 100

const owner = 1

const admin = 8

const view = 1024

const send = 2048

const view_members = 18_014_398_509_481_984

const channels = [11, 12, 13]

const roles = [500, 501, 502, 503]

const users = [1, 2, 3, 4, 5, 6, 7, 8]

const names = [
  "alpha", "bravo", "charlie", "delta", "echo", "foxtrot", "golf", "hotel",
  "india", "juliet",
]

type Role {
  Role(id: Int, bits: Int, hoist: Bool, position: Int)
}

type Overwrite {
  Overwrite(target: Int, kind: Int, allow: Int, deny: Int)
}

type Member {
  Member(user: Int, name: String, nick: Option(String), roles: List(Int))
}

type Client {
  Client(handle: h.Client, user: Int, visible: Set(Int), list: Option(Watch))
}

type Watch {
  Watch(
    channel: Int,
    ranges: List(#(Int, Int)),
    rows: Dict(Int, String),
    counts: Option(#(Int, Int)),
    synced: Bool,
  )
}

type Model {
  Model(
    everyone: Int,
    roles: List(Role),
    channels: Dict(Int, List(Overwrite)),
    members: Dict(Int, Member),
    clients: Dict(Int, Client),
    statuses: Dict(Int, presence.Status),
    next: Int,
    log: List(String),
  )
}

fn role_json(role: Role) -> String {
  "{\"id\":\""
  <> int.to_string(role.id)
  <> "\",\"name\":\"r"
  <> int.to_string(role.id)
  <> "\",\"permissions\":\""
  <> int.to_string(role.bits)
  <> "\",\"position\":"
  <> int.to_string(role.position)
  <> ",\"hoist\":"
  <> bool_text(role.hoist)
  <> "}"
}

fn bool_text(flag: Bool) -> String {
  case flag {
    True -> "true"
    False -> "false"
  }
}

fn channel_json(channel: Int, overwrites: List(Overwrite)) -> String {
  h.channel_json(
    channel,
    0,
    "["
      <> string.join(
      list.map(overwrites, fn(found) {
        h.overwrite_json(found.target, found.kind, found.allow, found.deny)
      }),
      ",",
    )
      <> "]",
  )
}

fn member_json(member: Member) -> String {
  let nick = case member.nick {
    Some(nick) -> "\"nick\":\"" <> nick <> "\","
    None -> ""
  }
  "{\"guild_id\":\"100\","
  <> nick
  <> string.drop_start(h.member_json(member.user, member.name, member.roles), 1)
}

fn bits_choice(rng: Rng) -> #(Int, Rng) {
  gen.pick(rng, [0, 0, view, view + view_members, view_members, admin, send], 0)
}

fn everyone_choice(rng: Rng) -> #(Int, Rng) {
  gen.pick(
    rng,
    [view + send + view_members, view + send, send, view + view_members],
    view,
  )
}

fn overwrite_choice(rng: Rng, members: List(Int)) -> #(Overwrite, Rng) {
  let #(kind, rng) = gen.int(rng, 3)
  let #(target, kind, rng) = case kind {
    0 -> #(guild, 0, rng)
    1 -> {
      let #(role, rng) = gen.pick(rng, roles, 500)
      #(role, 0, rng)
    }
    _ -> {
      let #(user, rng) = gen.pick(rng, members, 2)
      #(user, 1, rng)
    }
  }
  let #(pair, rng) =
    gen.pick(
      rng,
      [
        #(view, 0),
        #(0, view),
        #(0, view_members),
        #(view_members, 0),
        #(view + view_members, 0),
        #(0, view + view_members),
      ],
      #(0, 0),
    )
  #(Overwrite(target:, kind:, allow: pair.0, deny: pair.1), rng)
}

fn overwrites_choice(rng: Rng, members: List(Int)) -> #(List(Overwrite), Rng) {
  let #(count, rng) = gen.int(rng, 4)
  let #(found, rng) = gen.list(rng, count, overwrite_choice(_, members))
  #(
    list.unique(found)
      |> list.fold([], fn(acc, item) {
        case list.any(acc, fn(seen: Overwrite) { seen.target == item.target }) {
          True -> acc
          False -> [item, ..acc]
        }
      })
      |> list.reverse,
    rng,
  )
}

fn free_position(model_roles: List(Role), rng: Rng) -> #(Int, Rng) {
  let used = list.map(model_roles, fn(role) { role.position })
  let free = list.filter(gen.span(1, 30), fn(at) { !list.contains(used, at) })
  gen.pick(rng, free, 31)
}

fn initial(rng: Rng) -> #(Model, Rng) {
  let #(everyone, rng) = everyone_choice(rng)
  let #(positions, rng) = gen.shuffle(rng, gen.span(1, 12))
  let #(model_roles, rng) =
    list.fold(list.zip(roles, positions), #([], rng), fn(state, pair) {
      let #(acc, rng) = state
      let #(bits, rng) = bits_choice(rng)
      let #(hoist, rng) = gen.bool(rng)
      #([Role(id: pair.0, bits:, hoist:, position: pair.1), ..acc], rng)
    })
  let #(count, rng) = gen.range(rng, 3, list.length(users))
  let #(member_list, rng) =
    list.fold(list.take(users, count), #([], rng), fn(state, user) {
      let #(acc, rng) = state
      let #(member, rng) = new_member(rng, user)
      #([member, ..acc], rng)
    })
  let members =
    dict.from_list(list.map(member_list, fn(member) { #(member.user, member) }))
  let #(channel_list, rng) =
    list.fold(channels, #([], rng), fn(state, channel) {
      let #(acc, rng) = state
      let #(overwrites, rng) = overwrites_choice(rng, dict.keys(members))
      #([#(channel, overwrites), ..acc], rng)
    })
  #(
    Model(
      everyone:,
      roles: list.reverse(model_roles),
      channels: dict.from_list(channel_list),
      members:,
      clients: dict.new(),
      statuses: dict.new(),
      next: 0,
      log: [],
    ),
    rng,
  )
}

fn new_member(rng: Rng, user: Int) -> #(Member, Rng) {
  let #(name, rng) = gen.pick(rng, names, "alpha")
  let #(count, rng) = gen.int(rng, 3)
  let #(picked, rng) = gen.list(rng, count, gen.pick(_, roles, 500))
  #(
    Member(
      user:,
      name: name <> int.to_string(user),
      nick: None,
      roles: list.unique(picked),
    ),
    rng,
  )
}

fn role_bits(model: Model, id: Int) -> Int {
  case list.find(model.roles, fn(role) { role.id == id }) {
    Ok(role) -> role.bits
    Error(Nil) -> 0
  }
}

fn channel_bits(model: Model, user: Int, channel: Int) -> Int {
  case dict.get(model.members, user) {
    Error(Nil) -> 0
    Ok(member) -> {
      let base =
        list.fold(member.roles, model.everyone, fn(acc, role) {
          int.bitwise_or(acc, role_bits(model, role))
        })
      case user == owner || int.bitwise_and(base, admin) != 0 {
        True -> -1
        False -> {
          let overwrites =
            dict.get(model.channels, channel) |> result.unwrap([])
          let apply = fn(bits, found: Overwrite) {
            int.bitwise_or(
              int.bitwise_and(bits, int.bitwise_not(found.deny)),
              found.allow,
            )
          }
          let bits =
            list.fold(overwrites, base, fn(bits, found) {
              case found.kind == 0 && found.target == guild {
                True -> apply(bits, found)
                False -> bits
              }
            })
          let #(allow, deny) =
            list.fold(overwrites, #(0, 0), fn(acc, found) {
              case
                found.kind == 0 && list.contains(member.roles, found.target)
              {
                True -> #(
                  int.bitwise_or(acc.0, found.allow),
                  int.bitwise_or(acc.1, found.deny),
                )
                False -> acc
              }
            })
          let bits = apply(bits, Overwrite(0, 0, allow, deny))
          list.fold(overwrites, bits, fn(bits, found) {
            case found.kind == 1 && found.target == user {
              True -> apply(bits, found)
              False -> bits
            }
          })
        }
      }
    }
  }
}

fn has(bits: Int, flag: Int) -> Bool {
  int.bitwise_and(bits, flag) == flag
}

fn can_view(model: Model, user: Int, channel: Int) -> Bool {
  has(channel_bits(model, user, channel), view)
}

fn can_list(model: Model, user: Int, channel: Int) -> Bool {
  has(channel_bits(model, user, channel), view + view_members)
}

fn visible_set(model: Model, user: Int) -> Set(Int) {
  channels
  |> list.filter(can_view(model, user, _))
  |> set.from_list
}

fn online_status(model: Model, user: Int) -> Option(String) {
  let connected =
    dict.values(model.clients) |> list.any(fn(client) { client.user == user })
  case connected, dict.get(model.statuses, user) {
    True, Ok(presence.Online) -> Some("online")
    True, Ok(presence.Idle) -> Some("idle")
    True, Ok(presence.Dnd) -> Some("dnd")
    _, _ -> None
  }
}

fn hoisted_order(model: Model) -> List(Int) {
  model.roles
  |> list.filter(fn(role) { role.hoist })
  |> list.sort(fn(a, b) { int.compare(b.position, a.position) })
  |> list.map(fn(role) { role.id })
}

fn display(member: Member) -> String {
  option.unwrap(member.nick, member.name)
}

fn model_rows(model: Model, channel: Int) -> #(List(String), #(Int, Int)) {
  let order = hoisted_order(model)
  let entries =
    dict.values(model.members)
    |> list.filter(fn(member) { can_view(model, member.user, channel) })
    |> list.map(fn(member) {
      let status = online_status(model, member.user)
      let section = case status {
        None -> #(1_000_001, "offline")
        Some(_) ->
          case
            list.index_map(order, fn(role, index) { #(role, index) })
            |> list.filter(fn(pair) { list.contains(member.roles, pair.0) })
            |> list.first
          {
            Ok(#(role, index)) -> #(index, int.to_string(role))
            Error(Nil) -> #(1_000_000, "online")
          }
      }
      #(section, display(member), member.user, option.unwrap(status, "offline"))
    })
    |> list.sort(fn(a, b) {
      int.compare(a.0.0, b.0.0)
      |> order_then(string.compare(a.1, b.1))
      |> order_then(int.compare(a.2, b.2))
    })
  let rows =
    entries
    |> list.chunk(fn(entry) { entry.0 })
    |> list.flat_map(fn(group) {
      let assert [first, ..] = group
      [
        "g:" <> first.0.1,
        ..list.map(group, fn(entry) { int.to_string(entry.2) <> ":" <> entry.3 })
      ]
    })
  let online = list.count(entries, fn(entry) { entry.3 != "offline" })
  #(rows, #(list.length(entries), online))
}

fn order_then(first, next) {
  case first {
    order.Eq -> next
    other -> other
  }
}

fn row_of(item: Dynamic) -> String {
  case h.group_ids([item]), h.item_user_ids([item]) {
    [group], _ -> "g:" <> group
    _, [user] -> {
      let status =
        decode.run(
          item,
          decode.at(["member", "presence", "status"], decode.string),
        )
        |> result.unwrap("missing")
      int.to_string(user) <> ":" <> status
    }
    _, _ -> "?"
  }
}

fn apply_op(rows: Dict(Int, String), op: h.Op) -> Dict(Int, String) {
  case op.op, op.range {
    "SYNC", [start, end] -> {
      let items = list.map(op.items, row_of)
      gen.span(start, end)
      |> list.fold(rows, fn(rows, index) {
        case list.drop(items, index - start) {
          [row, ..] -> dict.insert(rows, index, row)
          [] -> dict.delete(rows, index)
        }
      })
    }
    "INVALIDATE", [start, end] ->
      gen.span(start, end) |> list.fold(rows, dict.delete)
    "INSERT", _ -> {
      let assert [item] = op.items
      rows
      |> dict.to_list
      |> list.map(fn(pair) {
        case pair.0 >= op.index {
          True -> #(pair.0 + 1, pair.1)
          False -> pair
        }
      })
      |> dict.from_list
      |> dict.insert(op.index, row_of(item))
    }
    "UPDATE", _ -> {
      let assert [item] = op.items
      dict.insert(rows, op.index, row_of(item))
    }
    "DELETE", _ ->
      rows
      |> dict.delete(op.index)
      |> dict.to_list
      |> list.map(fn(pair) {
        case pair.0 > op.index {
          True -> #(pair.0 - 1, pair.1)
          False -> pair
        }
      })
      |> dict.from_list
    other, _ -> dict.insert(rows, -1, "unknown op " <> other)
  }
}

fn absorb(client: Client, frames: List(h.Frame)) -> Client {
  list.fold(frames, client, fn(client, frame) {
    case frame.event {
      "CHANNEL_CREATE" | "CHANNEL_UPDATE" ->
        case channel_of(frame) {
          Ok(channel) ->
            Client(..client, visible: set.insert(client.visible, channel))
          Error(Nil) -> client
        }
      "CHANNEL_DELETE" ->
        case channel_of(frame) {
          Ok(channel) ->
            Client(..client, visible: set.delete(client.visible, channel))
          Error(Nil) -> client
        }
      "GUILD_MEMBER_LIST_UPDATE" ->
        case client.list, list_channel(frame) {
          Some(watch), Ok(channel) if channel == watch.channel -> {
            let ops = h.ops(frame)
            let rows = list.fold(ops, watch.rows, apply_op)
            let synced =
              watch.synced || list.any(ops, fn(op) { op.op == "SYNC" })
            Client(
              ..client,
              list: Some(
                Watch(
                  ..watch,
                  rows:,
                  counts: Some(h.list_counts(frame)),
                  synced:,
                ),
              ),
            )
          }
          _, _ -> client
        }
      _ -> client
    }
  })
}

fn channel_of(frame: h.Frame) -> Result(Int, Nil) {
  decode.run(frame.payload, decode.at(["id"], decode.string))
  |> result.replace_error(Nil)
  |> result.try(int.parse)
}

fn list_channel(frame: h.Frame) -> Result(Int, Nil) {
  decode.run(frame.payload, decode.at(["channel_id"], decode.string))
  |> result.replace_error(Nil)
  |> result.try(int.parse)
}

fn settle(g: h.Guild, model: Model) -> Model {
  process.send(h.control(g), inbox.FlushMemberList)
  h.sync(g)
  let clients =
    dict.map_values(model.clients, fn(_, client) {
      absorb(client, drain_now(client.handle, []))
    })
  Model(..model, clients:)
}

fn viewable(g: h.Guild, user: Int) -> Set(Int) {
  let assert Ok(found) = id.user_id(user)
  case h.ask(g, inbox.ViewableChannels(found)) {
    Ok(reply) ->
      json.to_string(reply)
      |> json.parse(decode.at(
        ["channel_ids"],
        decode.list(decode.map(decode.string, int.parse)),
      ))
      |> result.unwrap([])
      |> list.filter_map(fn(x) { x })
      |> set.from_list
    Error(_) -> set.new()
  }
}

fn check(g: h.Guild, model: Model) -> Result(Nil, String) {
  use _ <- result.try(
    dict.keys(model.members)
    |> list.try_each(fn(user) {
      let expected = visible_set(model, user)
      let found = viewable(g, user)
      gen.expect(found == expected, fn() {
        "ViewableChannels for user "
        <> int.to_string(user)
        <> " gave "
        <> string.inspect(set.to_list(found))
        <> " want "
        <> string.inspect(set.to_list(expected))
      })
    }),
  )
  dict.values(model.clients)
  |> list.try_each(fn(client) {
    let expected = visible_set(model, client.user)
    use _ <- result.try(
      gen.expect(client.visible == expected, fn() {
        "client of user "
        <> int.to_string(client.user)
        <> " tracks channels "
        <> string.inspect(set.to_list(client.visible))
        <> " want "
        <> string.inspect(set.to_list(expected))
      }),
    )
    case client.list {
      Some(watch) ->
        case can_list(model, client.user, watch.channel) {
          True -> check_watch(model, client, watch)
          False -> Ok(Nil)
        }
      None -> Ok(Nil)
    }
  })
}

fn check_watch(
  model: Model,
  client: Client,
  watch: Watch,
) -> Result(Nil, String) {
  let #(rows, counts) = model_rows(model, watch.channel)
  let indexed = dict.from_list(list.index_map(rows, fn(row, i) { #(i, row) }))
  let wanted =
    watch.ranges
    |> list.flat_map(fn(range) { gen.span(range.0, range.1) })
    |> list.unique
  let seen = list.map(wanted, fn(i) { dict.get(watch.rows, i) })
  let want = list.map(wanted, fn(i) { dict.get(indexed, i) })
  use _ <- result.try(
    gen.expect(watch.synced, fn() {
      "client of user "
      <> int.to_string(client.user)
      <> " never got a SYNC for channel "
      <> int.to_string(watch.channel)
    }),
  )
  use _ <- result.try(
    gen.expect(seen == want, fn() {
      "member list of user "
      <> int.to_string(client.user)
      <> " channel "
      <> int.to_string(watch.channel)
      <> " ranges "
      <> string.inspect(watch.ranges)
      <> "\n  got  "
      <> string.inspect(list.map(seen, result.unwrap(_, "-")))
      <> "\n  want "
      <> string.inspect(list.map(want, result.unwrap(_, "-")))
    }),
  )
  gen.expect(watch.counts == Some(counts), fn() {
    "member list counts of user "
    <> int.to_string(client.user)
    <> " got "
    <> string.inspect(watch.counts)
    <> " want "
    <> string.inspect(counts)
  })
}

fn step(g: h.Guild, model: Model, rng: Rng) -> #(Model, Rng) {
  let #(kind, rng) = gen.int(rng, 10)
  let members = dict.keys(model.members)
  let slots = dict.keys(model.clients)
  let connected =
    dict.values(model.clients)
    |> list.map(fn(client) { client.user })
    |> list.unique
  case kind {
    0 -> {
      let candidates =
        list.filter(members, fn(user) {
          list.count(dict.values(model.clients), fn(client) {
            client.user == user
          })
          < 2
        })
      case candidates {
        [] -> #(model, rng)
        _ -> {
          let #(user, rng) = gen.pick(rng, candidates, owner)
          let handle = h.join(g, user)
          let client =
            Client(handle:, user:, visible: viewable(g, user), list: None)
          #(
            Model(
              ..model,
              clients: dict.insert(model.clients, model.next, client),
              statuses: case list.contains(connected, user) {
                True -> model.statuses
                False -> dict.insert(model.statuses, user, presence.Online)
              },
              next: model.next + 1,
              log: ["join " <> int.to_string(user), ..model.log],
            ),
            rng,
          )
        }
      }
    }
    1 ->
      case slots {
        [] -> #(model, rng)
        _ -> {
          let #(slot, rng) = gen.pick(rng, slots, 0)
          let assert Ok(client) = dict.get(model.clients, slot)
          h.leave(g, client.handle)
          #(
            Model(..model, clients: dict.delete(model.clients, slot), log: [
              "leave " <> int.to_string(client.user),
              ..model.log
            ]),
            rng,
          )
        }
      }
    2 ->
      case connected {
        [] -> #(model, rng)
        _ -> {
          let #(user, rng) = gen.pick(rng, connected, owner)
          let #(status, rng) =
            gen.pick(
              rng,
              [
                presence.Online,
                presence.Idle,
                presence.Dnd,
                presence.Invisible,
              ],
              presence.Online,
            )
          h.set_presence(g, h.status(user, status))
          #(
            Model(
              ..model,
              statuses: dict.insert(model.statuses, user, status),
              log: [
                "presence "
                  <> int.to_string(user)
                  <> " "
                  <> presence.status_to_wire(status),
                ..model.log
              ],
            ),
            rng,
          )
        }
      }
    3 | 4 -> {
      let #(user, rng) = gen.pick(rng, members, owner)
      let assert Ok(member) = dict.get(model.members, user)
      let #(count, rng) = gen.int(rng, 3)
      let #(picked, rng) = gen.list(rng, count, gen.pick(_, roles, 500))
      let #(nick, rng) =
        gen.pick(rng, [None, None, Some("aaa"), Some("zzz")], None)
      let member = Member(..member, roles: list.unique(picked), nick:)
      h.dispatch(g, "GUILD_MEMBER_UPDATE", member_json(member))
      #(
        Model(..model, members: dict.insert(model.members, user, member), log: [
          "member update " <> string.inspect(member),
          ..model.log
        ]),
        rng,
      )
    }
    5 -> {
      let free =
        list.filter(gen.span(2, 12), fn(u) { !list.contains(members, u) })
      let #(add, rng) = gen.bool(rng)
      let removable =
        list.filter(members, fn(u) {
          u != owner && !list.contains(connected, u)
        })
      case add, free, removable {
        True, [_, ..], _ -> {
          let #(user, rng) = gen.pick(rng, free, 9)
          let #(member, rng) = new_member(rng, user)
          h.dispatch(g, "GUILD_MEMBER_ADD", member_json(member))
          #(
            Model(
              ..model,
              members: dict.insert(model.members, user, member),
              log: ["member add " <> string.inspect(member), ..model.log],
            ),
            rng,
          )
        }
        False, _, [_, ..] -> {
          let #(user, rng) = gen.pick(rng, removable, 2)
          h.dispatch(
            g,
            "GUILD_MEMBER_REMOVE",
            "{\"guild_id\":\"100\",\"user\":{\"id\":\""
              <> int.to_string(user)
              <> "\"}}",
          )
          #(
            Model(
              ..model,
              members: dict.delete(model.members, user),
              channels: dict.map_values(model.channels, fn(_, overwrites) {
                overwrites
              }),
              log: ["member remove " <> int.to_string(user), ..model.log],
            ),
            rng,
          )
        }
        _, _, _ -> #(model, rng)
      }
    }
    6 -> {
      let #(pick, rng) = gen.int(rng, 5)
      case pick {
        0 -> {
          let #(everyone, rng) = everyone_choice(rng)
          h.dispatch(
            g,
            "GUILD_ROLE_UPDATE",
            "{\"guild_id\":\"100\",\"role\":"
              <> h.role_json(guild, everyone, 0)
              <> "}",
          )
          #(
            Model(..model, everyone:, log: [
              "everyone " <> int.to_string(everyone),
              ..model.log
            ]),
            rng,
          )
        }
        _ -> {
          let #(id, rng) = gen.pick(rng, roles, 500)
          let assert Ok(role) = list.find(model.roles, fn(r) { r.id == id })
          let #(bits, rng) = bits_choice(rng)
          let #(hoist, rng) = gen.bool(rng)
          let #(move, rng) = gen.bool(rng)
          let #(position, rng) = case move {
            True -> free_position(model.roles, rng)
            False -> #(role.position, rng)
          }
          let role = Role(..role, bits:, hoist:, position:)
          h.dispatch(
            g,
            "GUILD_ROLE_UPDATE",
            "{\"guild_id\":\"100\",\"role\":" <> role_json(role) <> "}",
          )
          #(
            Model(
              ..model,
              roles: list.map(model.roles, fn(r) {
                case r.id == id {
                  True -> role
                  False -> r
                }
              }),
              log: ["role " <> string.inspect(role), ..model.log],
            ),
            rng,
          )
        }
      }
    }
    7 -> {
      let #(channel, rng) = gen.pick(rng, channels, 11)
      let #(overwrites, rng) = overwrites_choice(rng, members)
      h.dispatch(g, "CHANNEL_UPDATE", channel_json(channel, overwrites))
      #(
        Model(
          ..model,
          channels: dict.insert(model.channels, channel, overwrites),
          log: [
            "channel "
              <> int.to_string(channel)
              <> " "
              <> string.inspect(overwrites),
            ..model.log
          ],
        ),
        rng,
      )
    }
    _ ->
      case slots {
        [] -> #(model, rng)
        _ -> {
          let #(slot, rng) = gen.pick(rng, slots, 0)
          let assert Ok(client) = dict.get(model.clients, slot)
          let user = client.user
          let #(channel, rng) = gen.pick(rng, channels, 11)
          let #(count, rng) = gen.range(rng, 1, 3)
          let #(ranges, rng) =
            gen.list(rng, count, fn(rng) {
              let #(start, rng) = gen.int(rng, 12)
              let #(span, rng) = gen.int(rng, 10)
              #(#(start, start + span), rng)
            })
          h.subscribe(g, client.handle, channel, ranges)
          let allowed = can_list(model, user, channel)
          let watch = case client.list {
            Some(watch) if watch.channel == channel -> Watch(..watch, ranges:)
            _ ->
              Watch(
                channel:,
                ranges:,
                rows: dict.new(),
                counts: None,
                synced: False,
              )
          }
          #(
            Model(
              ..model,
              clients: dict.insert(model.clients, slot, case allowed {
                True -> Client(..client, list: Some(watch))
                False -> client
              }),
              log: [
                "subscribe "
                  <> int.to_string(user)
                  <> " "
                  <> int.to_string(channel)
                  <> " "
                  <> string.inspect(ranges),
                ..model.log
              ],
            ),
            rng,
          )
        }
      }
  }
}

fn start(model: Model) -> h.Guild {
  h.start(
    guild,
    owner,
    [h.role_json(guild, model.everyone, 0), ..list.map(model.roles, role_json)],
    list.map(channels, fn(channel) {
      channel_json(
        channel,
        dict.get(model.channels, channel) |> result.unwrap([]),
      )
    }),
    list.map(dict.values(model.members), fn(member) {
      h.member_json(member.user, member.name, member.roles)
    }),
  )
}

fn run(g: h.Guild, model: Model, rng: Rng, steps: Int) -> Result(Nil, String) {
  case steps <= 0 {
    True -> Ok(Nil)
    False -> {
      let #(model, rng) = step(g, model, rng)
      let model = settle(g, model)
      case patient(g, model, 4) {
        Ok(model) -> run(g, model, rng, steps - 1)
        Error(reason) ->
          Error(
            reason
            <> "\n  after: "
            <> string.join(list.reverse(list.take(model.log, 12)), "\n    ")
            <> "\n  state: everyone "
            <> int.to_string(model.everyone)
            <> " roles "
            <> string.inspect(model.roles)
            <> "\n  channels "
            <> string.inspect(dict.to_list(model.channels))
            <> "\n  members "
            <> string.inspect(dict.values(model.members)),
          )
      }
    }
  }
}

fn patient(g: h.Guild, model: Model, tries: Int) -> Result(Model, String) {
  case check(g, model), tries {
    Ok(Nil), _ -> Ok(model)
    Error(reason), 0 -> Error(reason)
    Error(_), _ -> {
      process.sleep(150)
      patient(g, settle(g, model), tries - 1)
    }
  }
}

fn guild_case(rng: Rng) -> Result(Nil, String) {
  let #(model, rng) = initial(rng)
  let g = start(model)
  let outcome = case check(g, model) {
    Ok(Nil) -> run(g, model, rng, 40)
    Error(reason) -> Error("initial: " <> reason)
  }
  h.stop(g)
  outcome
}

pub fn guild_visibility_and_member_list_follow_the_model_test() {
  gen.check("guild model", 25, guild_case)
}

fn drain_now(client: h.Client, acc: List(h.Frame)) -> List(h.Frame) {
  case process.receive(client.inbox, 0) {
    Ok(session.GuildDispatch(_, event, payload, _)) ->
      drain_now(client, [frame(event, payload), ..acc])
    Ok(session.Dispatch(event, payload)) ->
      drain_now(client, [frame(event, payload), ..acc])
    Ok(_) -> drain_now(client, acc)
    Error(Nil) -> list.reverse(acc)
  }
}

fn frame(event: String, payload: raw_json.RawJson) -> h.Frame {
  let assert Ok(text) = bit_array.to_string(raw_json.to_bits(payload))
  h.Frame(event:, payload: h.parse(text), text:)
}
