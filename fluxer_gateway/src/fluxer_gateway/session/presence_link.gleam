// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/core/error.{type Unavailable, Overloaded, Unavailable}
import fluxer_gateway/core/id.{type GuildId}
import fluxer_gateway/core/packed_ids
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/presence/client.{type PresenceLink, SessionConnect}
import fluxer_gateway/presence/protocol
import fluxer_gateway/session/links
import fluxer_gateway/session/ready
import fluxer_gateway/session/relations
import fluxer_gateway/session/state.{
  type PresenceSent, type State, PresenceConnect, PresenceConnecting,
  PresenceDown, PresenceSent, PresenceTimeout, PresenceUp, State,
} as st
import fluxer_gateway/session/visits
import gleam/erlang/process.{type Monitor}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}

const max_attempts = 25

fn connect_timeout_ms() -> Int {
  durations.session().presence_link_connect_timeout_ms
}

fn member_guilds(state: State) -> List(GuildId) {
  state.links
  |> links.guild_ids
  |> list.filter(fn(guild) {
    !visits.is_visiting(state.visits, id.guild_id_to_int(guild))
  })
}

pub fn targets(state: State) -> Set(Int) {
  relations.presence_targets(
    state.relations,
    st.user_int(state),
    st.eligible_partners(state),
  )
}

pub fn connect(state: State, attempt: Int) -> State {
  case state.presence {
    PresenceUp(_, _) -> state
    _ -> {
      let me = st.user_int(state)
      let eligible = st.eligible_partners(state)
      let friends = set.from_list(relations.friends(state.relations))
      let group_dm_recipients =
        relations.group_dm_recipients(state.relations, me, eligible)
      let request =
        SessionConnect(
          session: state.me,
          status: state.status,
          afk: state.afk,
          mobile: state.mobile,
          custom_status: state.custom_status,
          settings_invisible: state.settings_invisible,
          user: state.user,
          friends: packed(friends),
          group_dm_recipients: packed(group_dm_recipients),
          guilds: member_guilds(state),
        )
      client.session_connect(request, state.presence_replies)
      st.schedule(state, connect_timeout_ms(), PresenceTimeout(attempt))
      let sent =
        PresenceSent(
          status: state.status,
          afk: state.afk,
          mobile: state.mobile,
          custom_status: state.custom_status,
          targets: friends
            |> set.union(group_dm_recipients)
            |> set.delete(me),
        )
      State(
        ..state,
        presence: PresenceConnecting(attempt),
        presence_sent: Some(sent),
      )
    }
  }
}

fn packed(ids: Set(Int)) -> packed_ids.PackedIds {
  ids |> set.to_list |> list.sort(int.compare) |> packed_ids.from_list
}

pub type Linked {
  NowLinked(State)
  StillDown(State)
}

pub fn on_reply(
  state: State,
  result: Result(PresenceLink, Unavailable),
) -> Linked {
  case state.presence, result {
    PresenceUp(current, _), Ok(extra) -> {
      case client.same_entry(current, extra) {
        True -> Nil
        False -> client.session_disconnect(extra)
      }
      StillDown(state)
    }
    PresenceUp(_, _), Error(_) -> StillDown(state)
    PresenceConnecting(_), Ok(link) | PresenceDown, Ok(link) ->
      NowLinked(catch_up(
        State(
          ..state,
          presence: PresenceUp(link, watch(link)),
          presence_sent: None,
        ),
        state.presence_sent,
      ))
    PresenceConnecting(attempt), Error(reason) ->
      StillDown(retry(state, attempt, retry_after(reason)))
    PresenceDown, Error(_) -> StillDown(state)
  }
}

fn catch_up(state: State, sent: Option(PresenceSent)) -> State {
  case sent {
    Some(sent) -> {
      case
        sent.status == state.status
        && sent.afk == state.afk
        && sent.mobile == state.mobile
        && sent.custom_status == state.custom_status
      {
        True -> Nil
        False ->
          push_update(state, case sent.custom_status == state.custom_status {
            True -> None
            False -> Some(custom_value(state.custom_status))
          })
      }
      sync_targets(State(..state, presence_targets: sent.targets))
    }
    None -> {
      push_update(state, Some(custom_value(state.custom_status)))
      sync_targets(State(..state, presence_targets: set.new()))
    }
  }
}

fn custom_value(custom_status: Option(RawJson)) -> RawJson {
  option.unwrap(custom_status, raw_json.null())
}

fn retry_after(reason: Unavailable) -> Int {
  case reason {
    Unavailable(retry_after_ms) | Overloaded(retry_after_ms) -> retry_after_ms
  }
}

pub fn on_timeout(state: State, attempt: Int) -> State {
  case state.presence {
    PresenceConnecting(current) if current == attempt -> retry(state, attempt, 0)
    _ -> state
  }
}

fn retry(state: State, attempt: Int, hint_ms: Int) -> State {
  case attempt < max_attempts {
    True -> {
      let delay = int.max(hint_ms, links.backoff_ms(attempt))
      st.schedule(state, delay, PresenceConnect(attempt + 1))
    }
    False -> Nil
  }
  State(..state, presence: PresenceDown)
}

fn watch(link: PresenceLink) -> Option(Monitor) {
  client.link_pid(link)
  |> result.map(process.monitor)
  |> option.from_result
}

fn unwatch(monitor: Option(Monitor)) -> Nil {
  case monitor {
    Some(monitor) -> process.demonitor_process(monitor)
    None -> Nil
  }
}

pub fn rebind(
  state: State,
  to: process.Subject(protocol.Input(Monitor)),
  stub: process.Pid,
) -> State {
  case state.presence {
    PresenceUp(link, monitor) ->
      case client.link_pid(link) == Ok(stub) {
        True -> {
          unwatch(monitor)
          let link = client.rebind(link, to)
          State(..state, presence: PresenceUp(link, watch(link)))
        }
        False -> state
      }
    _ -> state
  }
}

pub fn adopt_moved(
  state: State,
  link: PresenceLink,
  targets: Set(Int),
) -> State {
  let link = client.reown(link, state.me.inbox)
  State(
    ..state,
    presence: PresenceUp(link, watch(link)),
    presence_targets: targets,
  )
}

pub fn after_move(state: State, kept: Bool) -> State {
  case kept, state.presence {
    False, PresenceUp(_, _) -> force_reconnect(state)
    _, _ -> state
  }
}

pub fn relink(state: State) -> State {
  case state.presence {
    PresenceUp(link, _) ->
      case client.is_link_alive(link) {
        True -> state
        False -> force_reconnect(state)
      }
    _ -> state
  }
}

pub fn force_reconnect(state: State) -> State {
  case state.presence {
    PresenceUp(link, monitor) -> {
      unwatch(monitor)
      client.session_disconnect(link)
    }
    _ -> Nil
  }
  st.schedule(state, 0, PresenceConnect(0))
  State(..state, presence: PresenceDown)
}

pub fn on_down(state: State, monitor: Monitor) -> Option(State) {
  case state.presence {
    PresenceUp(_, Some(watched)) if watched == monitor -> {
      st.schedule(state, 0, PresenceConnect(0))
      Some(State(..state, presence: PresenceDown))
    }
    _ -> None
  }
}

pub fn push_update(state: State, custom: Option(RawJson)) -> Nil {
  case state.presence {
    PresenceUp(link, _) ->
      client.session_update(link, state.status, state.afk, state.mobile, custom)
    _ -> Nil
  }
}

pub fn sync_targets(state: State) -> State {
  case state.presence {
    PresenceUp(link, _) -> {
      let next = targets(state)
      let added = set.difference(next, state.presence_targets)
      let removed = set.difference(state.presence_targets, next)
      case set.is_empty(added) {
        True -> Nil
        False -> client.subscribe_users(link, packed(added))
      }
      case set.is_empty(removed) {
        True -> Nil
        False -> client.unsubscribe_users(link, packed(removed))
      }
      State(..state, presence_targets: next)
    }
    _ -> state
  }
}

pub fn disconnect(state: State) -> Nil {
  case state.presence {
    PresenceUp(link, monitor) -> {
      unwatch(monitor)
      client.session_disconnect(link)
    }
    _ -> Nil
  }
}

pub fn sessions(state: State) -> RawJson {
  case state.presence {
    PresenceUp(link, _) -> client.sessions_replace(link)
    _ -> ready.no_sessions()
  }
}

pub fn is_linked(state: State) -> Bool {
  case state.presence {
    PresenceUp(_, _) -> True
    _ -> False
  }
}
