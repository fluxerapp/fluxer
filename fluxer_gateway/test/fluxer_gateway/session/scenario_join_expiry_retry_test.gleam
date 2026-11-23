// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings.{
  type Durations, Durations, SessionDurations,
}
import gleam/erlang/process
import gleam/int
import gleam/list
import support/isolation
import support/session_fake_guild as fake_guild
import support/session_harness as h

const guilds = 50

const sessions = 20

fn fast_expiries() -> Nil {
  isolation.set_durations(fn(d: Durations) {
    Durations(
      ..d,
      session: SessionDurations(
        ..d.session,
        server_ready_deadline_ms: 100,
        guilds_first_join_floor_ms: 100,
        guilds_join_grace_ms: 50,
        guilds_after_ready_join_ms: 100,
        links_base_sleep_ms: 20,
        links_cap_sleep_ms: 50,
      ),
    )
  })
}

fn drain(fake: fake_guild.FakeGuild, count: Int) -> Int {
  case fake_guild.joins(fake, 0) {
    Ok(_) -> drain(fake, count + 1)
    Error(Nil) -> count
  }
}

fn count_until(
  fakes: List(fake_guild.FakeGuild),
  target: Int,
  seen: Int,
  deadline: Int,
) -> Int {
  let seen = list.fold(fakes, seen, fn(acc, fake) { drain(fake, acc) })
  case seen >= target || h.now_ms() > deadline {
    True -> seen
    False -> {
      process.sleep(50)
      count_until(fakes, target, seen, deadline)
    }
  }
}

pub fn expiries_on_other_sessions_never_stop_a_session_from_retrying_test() {
  fast_expiries()
  let user = h.unique_user()
  let ids =
    list.map(int.range(0, guilds, [], list.prepend), fn(_) { h.unique_user() })
  let fakes = list.map(ids, fake_guild.start(_, fake_guild.Hold))
  use <- h.with_api(h.session_api(h.session_data(user, False, ids)))
  let refs =
    list.map(int.range(0, sessions, [], list.prepend), fn(n) {
      let assert Ok(ref) =
        h.start_session(
          h.identify("tok-expiry-" <> int.to_string(n)),
          h.fake_socket(),
        )
      ref
    })
  let per_session = guilds + guilds + 3
  let target = sessions * per_session
  let seen = count_until(fakes, target, 0, h.now_ms() + 20_000)
  list.each(refs, h.terminate)
  list.each(fakes, fake_guild.crash)
  assert seen >= target
}
