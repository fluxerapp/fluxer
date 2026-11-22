// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/channel.{type Channel}
import fluxer_gateway/core/member
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/user
import fluxer_gateway/nif
import fluxer_gateway/perms
import fluxer_gateway/perms/fixture
import fluxer_gateway/perms/snapshot
import fluxer_gateway/threads/context.{type Context, Context}
import fluxer_gateway/threads/view
import gleam/erlang/process
import gleam/list
import gleam/option.{None, Some}

const view_channel = 1024

const guild = 100

const viewer = 1

const category = 10

const forum = 11

const text = 12

fn everyone(allow: Int, deny: Int) -> channel.Overwrite {
  fixture.make_overwrite(
    channel.RoleTarget(fixture.role_id(guild)),
    allow,
    deny,
  )
}

fn channels(category_deny: Int) -> List(Channel) {
  [
    fixture.make_channel(category, channel.Category, None, [
      everyone(0, category_deny),
    ]),
    fixture.make_channel(forum, channel.Forum, Some(category), [
      everyone(view_channel, 0),
    ]),
    fixture.make_channel(text, channel.Text, None, []),
  ]
}

fn context(found: List(Channel)) -> Context {
  context_with(found, [])
}

fn context_with(found: List(Channel), virtual: List(Int)) -> Context {
  let assert Ok(Nil) = nif.load()
  let guild_id = fixture.guild(guild)
  let viewer_member =
    member.new(
      user: user.new(
        id: fixture.user_id(viewer),
        bot: False,
        flags: 0,
        restricted: False,
        wire: raw_json.wrap(<<"{\"id\":\"1\"}">>),
      ),
      nick: None,
      roles: [],
      joined_at_ms: None,
      timed_out_until_ms: None,
      wire: raw_json.wrap(<<"{\"nick\":null}">>),
    )
  let source =
    list.fold(
      found,
      snapshot.new(guild_id)
        |> snapshot.set_roles([fixture.make_role(guild, view_channel)])
        |> snapshot.add_member(viewer_member),
      snapshot.add_channel,
    )
  let source = case virtual {
    [] -> source
    _ ->
      snapshot.add_virtual_access(
        source,
        fixture.user_id(viewer),
        list.map(virtual, fixture.channel_id),
      )
  }
  let #(handle, reader) = perms.new(guild_id, process.self())
  let assert Ok(_) = perms.load(handle, source)
  Context(
    guild: guild_id,
    reader:,
    member: fn(_) { Some(viewer_member) },
    channel: fn(wanted) {
      list.find(found, fn(entry) { channel.id(entry) == wanted })
      |> option.from_result
    },
    channels: found,
    passive: fn(_) { False },
    small_guild: True,
    presence: fn(_) { None },
    unavailable_for: fn(_) { False },
    history_cutoff_ms: None,
    self: process.new_subject(),
  )
}

pub fn forum_only_category_is_dropped_for_non_viewer_test() {
  let found = channels(view_channel)
  let ctx = context(found)
  assert perms.can_view(
      ctx.reader,
      fixture.user_id(viewer),
      fixture.channel_id(category),
    )
    == Ok(True)
  let kept = view.hide_for_non_viewer(ctx, fixture.user_id(viewer), found)
  assert list.length(kept) == 1
}

pub fn viewable_category_is_kept_without_its_forum_test() {
  let found = channels(0)
  let ctx = context(found)
  let kept = view.hide_for_non_viewer(ctx, fixture.user_id(viewer), found)
  assert list.length(kept) == 2
}

pub fn virtually_viewable_category_is_kept_without_its_forum_test() {
  let found = channels(view_channel)
  let ctx = context_with(found, [category])
  assert perms.has_virtual_view(
      ctx.reader,
      fixture.user_id(viewer),
      fixture.channel_id(category),
    )
    == Ok(True)
  let kept = view.hide_for_non_viewer(ctx, fixture.user_id(viewer), found)
  assert list.length(kept) == 2
}
