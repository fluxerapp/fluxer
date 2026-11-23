// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/json
import gleam/list
import gleam/option.{Some}
import support/guild_api
import support/guild_fixture as fixture
import support/guild_session as fake

pub fn start_loads_every_collection_and_joins_test() {
  use api <- guild_api.with_api
  let found =
    fixture.new(1)
    |> fixture.with_text(10)
    |> fixture.with_member(2, [])
  let handle = fixture.started(api, found)
  let session = fake.start(2)
  let reply = fake.join(session, handle)
  assert fake.is_joined(reply)
  let assert Some(created) = fake.guild_create(reply)
  assert fake.text_at(created, ["id"]) == Some(int_text(found.id))
  assert fake.channel_ids(reply) == ["10"]
  assert list.length(guild_api.collection_calls(api, "members")) == 1
  assert list.length(guild_api.collection_calls(api, "guild")) == 1
  fixture.stop(found)
}

fn int_text(value: Int) -> String {
  json.to_string(json.int(value))
}
