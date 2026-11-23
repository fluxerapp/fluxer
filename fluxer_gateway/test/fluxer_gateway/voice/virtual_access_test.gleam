// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/voice/virtual_access
import gleeunit/should

pub fn grant_and_revoke_test() {
  let assert Ok(user) = id.user_id(7)
  let assert Ok(channel) = id.channel_id(8)
  let access = virtual_access.new() |> virtual_access.grant(user, channel)
  virtual_access.has_voice_access(access, user, channel) |> should.be_true
  virtual_access.is_pending_join(access, user, channel) |> should.be_true
  let access = virtual_access.revoke(access, user, channel)
  virtual_access.has_access(access, user, channel) |> should.be_false
  virtual_access.channels(access, user) |> should.equal([])
}

pub fn transitions_test() {
  let assert Ok(user) = id.user_id(7)
  let assert Ok(channel) = id.channel_id(8)
  let access =
    virtual_access.new() |> virtual_access.mark_transition(user, channel)
  virtual_access.is_held(access, user, channel) |> should.be_true
  virtual_access.clear_transition(access, user, channel)
  |> virtual_access.is_held(user, channel)
  |> should.be_false
}
