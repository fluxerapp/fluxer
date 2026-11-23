// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/relay
import fluxer_gateway/core/channel.{type Channel}
import fluxer_gateway/core/guild as core_guild
import fluxer_gateway/core/id.{type ChannelId, type UserId}
import fluxer_gateway/core/json_tree.{type JsonTree}
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/session.{type SessionInbox}
import fluxer_gateway/guild/data.{type GuildData}
import fluxer_gateway/guild/dispatch/model
import fluxer_gateway/guild/dispatch/send
import fluxer_gateway/guild/sessions as table
import fluxer_gateway/guild/sessions/entry.{type Entry}
import fluxer_gateway/guild/state.{type GuildState} as guild_state
import fluxer_gateway/guild/state/channels as channel_events
import fluxer_gateway/member_list
import fluxer_gateway/member_list/event as list_event
import fluxer_gateway/perms
import fluxer_gateway/perms/audience.{type Group}
import fluxer_gateway/perms/event.{type Effects} as perm_event
import fluxer_gateway/threads/context.{type Context, Context}
import fluxer_gateway/threads/payload as thread_payload
import fluxer_gateway/threads/store.{type ThreadStore}
import fluxer_gateway/threads/subscriptions as thread_subscriptions
import fluxer_gateway/threads/viewers as thread_viewers
import fluxer_gateway/voice/guild_handler
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}

type Scope {
  Skip
  Everyone(updated: Option(List(ChannelId)))
  Users(users: List(UserId))
  Created(channels: List(ChannelId))
}

type Shared {
  Shared(
    found: Scope,
    effects: Effects,
    individual: Set(UserId),
    guild: RawJson,
    old_rows: Dict(Int, Channel),
    new_rows: Dict(Int, Channel),
    old_ctx: Context,
    new_ctx: Context,
    old_categories: Dict(Int, List(ChannelId)),
    new_categories: Dict(Int, List(ChannelId)),
    old_threads: ThreadStore,
  )
}

type Cache {
  Cache(
    created: Dict(#(Int, Bool), SessionInbox),
    deleted: Dict(Int, SessionInbox),
  )
}

type Plan {
  Plan(
    messages: List(SessionInbox),
    created: List(ChannelId),
    threads: List(ChannelId),
    gained: List(ChannelId),
  )
}

type Builder {
  Builder(
    messages: List(SessionInbox),
    created: List(ChannelId),
    threads: List(ChannelId),
    cache: Cache,
  )
}

type Key =
  #(Option(Group), Bool, Bool, Bool)

type Pass {
  Pass(
    state: GuildState,
    groups: Dict(Int, Option(Group)),
    preserved: Set(UserId),
    cache: Cache,
    plans: Dict(Key, Plan),
    batches: Dict(Key, List(Entry)),
    singles: List(#(Entry, Plan)),
  )
}

pub fn dispatch(
  before: GuildState,
  state: GuildState,
  name: String,
  raw: Dynamic,
  effects: Effects,
) -> GuildState {
  case scope(before, state, name, raw), perm_event.changes(effects) {
    Skip, _ | _, [] -> state
    found, _ -> run(before, state, found, effects)
  }
}

fn scope(
  before: GuildState,
  state: GuildState,
  name: String,
  raw: Dynamic,
) -> Scope {
  case name {
    "GUILD_MEMBER_UPDATE"
    | "GUILD_ROLE_UPDATE"
    | "GUILD_ROLE_UPDATE_BULK"
    | "GUILD_ROLE_DELETE" -> Everyone(None)
    "CHANNEL_UPDATE" | "CHANNEL_UPDATE_BULK" ->
      case channel_events.channel_ids(name, raw) {
        [] -> Everyone(None)
        updated -> Everyone(Some(updated))
      }
    "CHANNEL_CREATE" -> Created(channel_events.channel_ids(name, raw))
    "GUILD_UPDATE" -> {
      let old = core_guild.owner(guild_state.facts(before))
      let new = core_guild.owner(guild_state.facts(state))
      case old == new {
        True -> Skip
        False -> Users(option.values([old, new]))
      }
    }
    _ -> Skip
  }
}

fn run(
  before: GuildState,
  state: GuildState,
  found: Scope,
  effects: Effects,
) -> GuildState {
  let old_rows = rows_by_id(guild_state.guild_data(before))
  let new_rows = rows_by_id(guild_state.guild_data(state))
  let old_ctx = indexed(guild_state.thread_context(before), old_rows)
  let new_ctx = indexed(guild_state.thread_context(state), new_rows)
  let shared =
    Shared(
      found:,
      effects:,
      individual: perm_event.individual_users(effects),
      guild: guild_state.guild_id_json(state),
      old_rows:,
      new_rows:,
      old_ctx:,
      new_ctx:,
      old_categories: thread_viewers.forum_categories(old_ctx.channels),
      new_categories: thread_viewers.forum_categories(new_ctx.channels),
      old_threads: guild_state.threads(before),
    )
  let empty =
    Pass(
      state:,
      groups: dict.new(),
      preserved: set.new(),
      cache: Cache(created: dict.new(), deleted: dict.new()),
      plans: dict.new(),
      batches: dict.new(),
      singles: [],
    )
  let pass =
    table.entries(guild_state.sessions(state))
    |> list.filter(fn(session) {
      case found {
        Users(users) -> list.contains(users, entry.user(session))
        _ -> True
      }
    })
    |> list.fold(empty, fn(pass, session) {
      plan_session(shared, pass, session)
    })
  let batches =
    dict.fold(pass.batches, [], fn(acc, key, sessions) {
      case dict.get(pass.plans, key) {
        Ok(plan) -> [#(list.reverse(sessions), plan), ..acc]
        Error(Nil) -> acc
      }
    })
  let singles =
    list.reverse(pass.singles)
    |> list.map(fn(single) { #([single.0], single.1) })
  let planned = list.append(batches, singles)
  list.each(planned, fn(batch) {
    relay.send_all(list.map(batch.0, entry.ref), { batch.1 }.messages)
  })
  let state =
    list.fold(planned, pass.state, fn(state, batch) {
      list.fold(batch.0, state, fn(state, session) {
        follow_up(state, shared, session, batch.1)
      })
    })
  set.fold(pass.preserved, state, fn(acc, user) {
    model.set_virtual_access(
      acc,
      user,
      guild_handler.virtual_channels(guild_state.voice(acc), user),
    )
  })
}

fn plan_session(shared: Shared, pass: Pass, session: Entry) -> Pass {
  let user = entry.user(session)
  let #(group, groups) = group_of(pass.state, pass.groups, user)
  let pass = Pass(..pass, groups:)
  let viewer =
    thread_viewers.is_viewer_session(
      entry.flags(session),
      entry.is_bot(session),
    )
  let connected =
    guild_handler.connected_channels(guild_state.voice(pass.state), user)
  case connected, set.contains(shared.individual, user) {
    [], False -> {
      let key = #(
        group,
        viewer,
        option.is_some(shared.old_ctx.member(user)),
        option.is_some(shared.new_ctx.member(user)),
      )
      let pass = case dict.has_key(pass.plans, key) {
        True -> pass
        False -> {
          let change = perm_event.for_user(shared.effects, user, group)
          let #(plan, pass) =
            plan_change(shared, pass, user, viewer, change, [])
          Pass(..pass, plans: dict.insert(pass.plans, key, plan))
        }
      }
      Pass(
        ..pass,
        batches: dict.upsert(pass.batches, key, fn(sessions) {
          [session, ..option.unwrap(sessions, [])]
        }),
      )
    }
    _, _ -> {
      let change = perm_event.for_user(shared.effects, user, group)
      let kept = list.filter(change.lost, list.contains(connected, _))
      let pass = preserve(pass, user, kept)
      let #(plan, pass) = plan_change(shared, pass, user, viewer, change, kept)
      Pass(..pass, singles: [#(session, plan), ..pass.singles])
    }
  }
}

fn plan_change(
  shared: Shared,
  pass: Pass,
  user: UserId,
  viewer: Bool,
  change: perm_event.UserChange,
  kept: List(ChannelId),
) -> #(Plan, Pass) {
  let #(lost, gained) = case shared.found {
    Created(channels) -> #(
      [],
      list.filter(change.gained, fn(channel_id) {
        !list.contains(channels, channel_id)
      }),
    )
    _ -> #(
      list.filter(change.lost, fn(channel_id) {
        !list.contains(kept, channel_id)
      }),
      change.gained,
    )
  }
  let lost =
    list.filter(lost, thread_viewers.channel_visible(
      shared.old_threads,
      shared.old_ctx,
      shared.old_categories,
      viewer,
      user,
      _,
    ))
    |> list.sort(id.compare_channel_id)
  let gained =
    list.filter(gained, thread_viewers.channel_visible(
      guild_state.threads(pass.state),
      shared.new_ctx,
      shared.new_categories,
      viewer,
      user,
      _,
    ))
    |> list.sort(id.compare_channel_id)
  let start = Builder(messages: [], created: [], threads: [], cache: pass.cache)
  let built = case shared.found {
    Everyone(Some(updated)) ->
      per_channel(
        start,
        shared,
        pass.state,
        viewer,
        lost,
        gained,
        list.sort(updated, id.compare_channel_id),
      )
    _ -> {
      let built =
        list.fold(lost, start, fn(acc, channel_id) {
          delete(acc, shared, pass.state, channel_id)
        })
      let built =
        list.fold(gained, built, fn(acc, channel_id) {
          create(acc, shared, pass.state, viewer, channel_id, False)
        })
      Builder(..built, threads: list.reverse(gained))
    }
  }
  #(
    Plan(
      messages: list.reverse(built.messages),
      created: list.reverse(built.created),
      threads: list.reverse(built.threads),
      gained:,
    ),
    Pass(..pass, cache: built.cache),
  )
}

fn per_channel(
  start: Builder,
  shared: Shared,
  state: GuildState,
  viewer: Bool,
  lost: List(ChannelId),
  gained: List(ChannelId),
  updated: List(ChannelId),
) -> Builder {
  let lost_set = set.from_list(lost)
  let gained_set = set.from_list(gained)
  let #(built, done) =
    list.fold(updated, #(start, set.new()), fn(acc, channel_id) {
      let #(built, done) = acc
      let is_lost = set.contains(lost_set, channel_id)
      let is_gained = set.contains(gained_set, channel_id)
      case set.contains(done, channel_id), is_lost, is_gained {
        True, _, _ | False, False, False -> acc
        False, True, _ -> #(
          delete(built, shared, state, channel_id),
          set.insert(done, channel_id),
        )
        False, False, True -> {
          let built = create(built, shared, state, viewer, channel_id, True)
          let done = set.insert(done, channel_id)
          case parent_of(shared, channel_id) {
            Some(parent) ->
              case
                set.contains(gained_set, parent) && !set.contains(done, parent)
              {
                True -> #(
                  create(built, shared, state, viewer, parent, False),
                  set.insert(done, parent),
                )
                False -> #(built, done)
              }
            None -> #(built, done)
          }
        }
      }
    })
  let built =
    list.fold(lost, built, fn(acc, channel_id) {
      case set.contains(done, channel_id) {
        True -> acc
        False -> delete(acc, shared, state, channel_id)
      }
    })
  let rest =
    list.filter(gained, fn(channel_id) { !set.contains(done, channel_id) })
  let built =
    list.fold(rest, built, fn(acc, channel_id) {
      create(acc, shared, state, viewer, channel_id, False)
    })
  Builder(..built, threads: list.append(list.reverse(rest), built.threads))
}

fn follow_up(
  state: GuildState,
  shared: Shared,
  session: Entry,
  plan: Plan,
) -> GuildState {
  let subscribed = case list.any(plan.gained, resync(shared.found, _)) {
    True ->
      member_list.subscribed_channel(
        guild_state.member_list(state),
        entry.ref(session).id,
      )
    False -> None
  }
  let state = case subscribed {
    Some(channel_id) ->
      case
        list.contains(plan.created, channel_id)
        && resync(shared.found, channel_id)
      {
        True -> list_sync(state, shared, session, channel_id)
        False -> state
      }
    None -> state
  }
  case plan.threads {
    [] -> Nil
    threads ->
      thread_subscriptions.access_gained(
        guild_state.threads(state),
        shared.new_ctx,
        entry.key(session),
        threads,
      )
  }
  state
}

fn resync(found: Scope, channel_id: ChannelId) -> Bool {
  case found {
    Everyone(None) | Users(_) -> True
    Everyone(Some(updated)) -> list.contains(updated, channel_id)
    Created(_) | Skip -> False
  }
}

fn group_of(
  state: GuildState,
  groups: Dict(Int, Option(Group)),
  user: UserId,
) -> #(Option(Group), Dict(Int, Option(Group))) {
  let key = id.user_id_to_int(user)
  case dict.get(groups, key) {
    Ok(group) -> #(group, groups)
    Error(Nil) -> {
      let group =
        perms.group_of(guild_state.perms_reader(state), user)
        |> result.unwrap(None)
      #(group, dict.insert(groups, key, group))
    }
  }
}

fn preserve(pass: Pass, user: UserId, kept: List(ChannelId)) -> Pass {
  list.fold(kept, pass, fn(acc, channel_id) {
    let voice = guild_state.voice(acc.state)
    case guild_handler.has_virtual_access(voice, user, channel_id) {
      True -> acc
      False ->
        Pass(
          ..acc,
          state: guild_state.set_voice(
            acc.state,
            guild_handler.preserve_view(voice, user, channel_id),
          ),
          preserved: set.insert(acc.preserved, user),
        )
    }
  })
}

fn indexed(ctx: Context, rows: Dict(Int, Channel)) -> Context {
  Context(..ctx, channel: fn(channel_id) {
    dict.get(rows, id.channel_id_to_int(channel_id)) |> option.from_result
  })
}

fn rows_by_id(guild_data: GuildData) -> Dict(Int, Channel) {
  list.fold(guild_data.channels, dict.new(), fn(acc, row) {
    let key = id.channel_id_to_int(channel.id(row))
    case dict.has_key(acc, key) {
      True -> acc
      False -> dict.insert(acc, key, row)
    }
  })
}

fn parent_of(shared: Shared, channel_id: ChannelId) -> Option(ChannelId) {
  dict.get(shared.new_rows, id.channel_id_to_int(channel_id))
  |> option.from_result
  |> option.then(channel.parent)
}

fn delete(
  built: Builder,
  shared: Shared,
  state: GuildState,
  channel_id: ChannelId,
) -> Builder {
  let key = id.channel_id_to_int(channel_id)
  case dict.get(shared.old_rows, key) {
    Error(Nil) -> built
    Ok(row) -> {
      let cache = built.cache
      let #(message, cache) = case dict.get(cache.deleted, key) {
        Ok(message) -> #(message, cache)
        Error(Nil) -> {
          let message =
            send.message(
              state,
              "CHANNEL_DELETE",
              delete_payload(shared.guild, row),
            )
          #(
            message,
            Cache(..cache, deleted: dict.insert(cache.deleted, key, message)),
          )
        }
      }
      Builder(..built, messages: [message, ..built.messages], cache:)
    }
  }
}

fn create(
  built: Builder,
  shared: Shared,
  state: GuildState,
  viewer: Bool,
  channel_id: ChannelId,
  threads: Bool,
) -> Builder {
  let key = id.channel_id_to_int(channel_id)
  case dict.get(shared.new_rows, key) {
    Error(Nil) -> built
    Ok(row) -> {
      let cache = built.cache
      let variant = #(key, viewer)
      let #(message, cache) = case dict.get(cache.created, variant) {
        Ok(message) -> #(message, cache)
        Error(Nil) -> {
          let message =
            send.message(
              state,
              "CHANNEL_CREATE",
              create_payload(
                shared.guild,
                guild_state.guild_data(state),
                row,
                viewer,
              ),
            )
          #(
            message,
            Cache(
              ..cache,
              created: dict.insert(cache.created, variant, message),
            ),
          )
        }
      }
      Builder(
        messages: [message, ..built.messages],
        created: [channel_id, ..built.created],
        threads: case threads {
          True -> [channel_id, ..built.threads]
          False -> built.threads
        },
        cache:,
      )
    }
  }
}

fn list_sync(
  state: GuildState,
  shared: Shared,
  session: Entry,
  channel_id: ChannelId,
) -> GuildState {
  let #(state, dispatches) =
    model.apply_member_list(
      state,
      [list_event.view_gained(entry.ref(session).id, channel_id)],
      Some(perms.delta(shared.effects)),
    )
  send.list_dispatches(state, dispatches)
  state
}

fn delete_payload(guild: RawJson, row: Channel) -> JsonTree {
  let channel_id = case
    raw_json.field(channel.raw(row), "id") |> result.map(raw_json.to_bits)
  {
    Ok(<<"\"", _:bits>> as slice) -> raw_json.wrap(slice)
    _ -> raw_json.from_json(id.json(id.channel_id_to_int(channel.id(row))))
  }
  json_tree.Object([
    #("id", json_tree.Raw(channel_id)),
    #("guild_id", json_tree.Raw(guild)),
  ])
}

fn create_payload(
  guild: RawJson,
  guild_data: GuildData,
  row: Channel,
  viewer: Bool,
) -> JsonTree {
  let wire = json_tree.Raw(data.channel_wire(guild_data, row))
  let tree = case viewer {
    True -> wire
    False ->
      json_tree.expand(wire)
      |> result.unwrap(wire)
      |> thread_payload.strip_channel_surface_tree
      |> thread_payload.mask_overwrites_tree
  }
  json_tree.set(tree, "guild_id", json_tree.Raw(guild))
  |> result.unwrap(tree)
}
