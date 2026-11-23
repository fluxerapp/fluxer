// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/guild.{type GuildRequest}
import fluxer_gateway/core/presence.{type Status, Dnd, Idle, Online}
import fluxer_gateway/presence/client
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/list
import gleam/option.{None}
import gleeunit/should
import support/presence_scenario as scn

const wide = 150

fn observers(
  base: Int,
  count: Int,
  user: Int,
) -> List(#(Int, Subject(GuildRequest))) {
  int.range(from: 1, to: count + 1, with: [], run: list.prepend)
  |> list.reverse
  |> list.map(fn(n) { #(base + n, scn.observe(base + n, [user])) })
}

fn statuses(
  observer: Subject(GuildRequest),
  user: Int,
  count: Int,
) -> List(Status) {
  case count {
    0 -> []
    _ ->
      case scn.next_presence(observer, user, 3000) {
        Ok(seen) -> [
          presence.status(seen),
          ..statuses(observer, user, count - 1)
        ]
        Error(Nil) -> []
      }
  }
}

pub fn wide_fan_out_keeps_each_observer_in_publish_order_test() {
  let user = 7_200_000_000_000_001
  let session = scn.fake_session(user, 1)
  let link = scn.connect(session, Online, [])
  scn.sync(user)
  process.sleep(5)
  let watching = observers(7_200_000_000_100_000, wide, user)
  let published = [Dnd, Idle, Online, Dnd, Idle]
  list.each(published, fn(status) {
    client.session_update(link, status, False, False, None)
  })
  list.each(watching, fn(entry) {
    statuses(entry.1, user, list.length(published)) |> should.equal(published)
  })
  scn.sync(user)
  list.each(watching, fn(entry) {
    scn.presences(entry.1, user, 0) |> should.equal([])
  })
  client.session_disconnect(link)
  list.each(watching, fn(entry) { scn.stop_observing(entry.0) })
}

pub fn target_that_shrinks_to_nothing_and_grows_again_still_reaches_everyone_test() {
  let user = 7_200_000_000_000_002
  let session = scn.fake_session(user, 2)
  let link = scn.connect(session, Online, [])
  scn.sync(user)
  process.sleep(5)
  let first = observers(7_200_000_000_200_000, wide, user)
  client.session_update(link, Dnd, False, False, None)
  list.each(first, fn(entry) {
    statuses(entry.1, user, 1) |> should.equal([Dnd])
  })
  list.each(first, fn(entry) { scn.stop_observing(entry.0) })
  scn.sync_registry(user)
  let single = observers(7_200_000_000_300_000, 1, user)
  client.session_update(link, Idle, False, False, None)
  list.each(single, fn(entry) {
    statuses(entry.1, user, 1) |> should.equal([Idle])
  })
  scn.sync(user)
  process.sleep(5)
  let second = observers(7_200_000_000_400_000, wide, user)
  client.session_update(link, Online, False, False, None)
  list.each(single, fn(entry) {
    statuses(entry.1, user, 1) |> should.equal([Online])
  })
  list.each(second, fn(entry) {
    statuses(entry.1, user, 1) |> should.equal([Online])
  })
  scn.sync(user)
  list.each(list.flatten([first, single, second]), fn(entry) {
    scn.presences(entry.1, user, 0) |> should.equal([])
  })
  client.session_disconnect(link)
  list.each(list.append(single, second), fn(entry) {
    scn.stop_observing(entry.0)
  })
}
