// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/error
import fluxer_gateway/core/id
import fluxer_gateway/voice/entry
import fluxer_gateway/voice/p2p
import gleam/option.{None, Some}
import gleeunit/should
import support/voice_ids

fn sample(conn: String, p2p_on: Bool) -> entry.Entry {
  let assert Ok(user) = id.user_id(10)
  let assert Ok(channel) = id.channel_id(20)
  entry.new(
    user:,
    guild: None,
    channel:,
    connection_id: voice_ids.conn(conn),
    session_id: Some("s"),
    flags: entry.no_flags(),
    mute: False,
    deaf: False,
  )
  |> entry.set_p2p(p2p_on)
}

pub fn agreed_test() {
  p2p.agreed(Some(True), False) |> should.be_true
  p2p.agreed(Some(True), True) |> should.be_false
  p2p.agreed(None, False) |> should.be_false
  p2p.declined(Some(False)) |> should.be_true
  p2p.declined(None) |> should.be_false
}

pub fn empty_channel_initiator_test() {
  p2p.join_decision(agreed: True, states: [], pending: False)
  |> should.equal(p2p.P2p(p2p.Initiator, 1))
  p2p.join_decision(agreed: False, states: [], pending: False)
  |> should.equal(p2p.Sfu)
  p2p.join_decision(agreed: True, states: [], pending: True)
  |> should.equal(p2p.Sfu)
}

pub fn p2p_channel_requires_consent_test() {
  let states = [sample("a", True)]
  p2p.join_decision(agreed: False, states:, pending: False)
  |> should.equal(p2p.Reject(error.VoiceP2pConsentRequired))
  p2p.join_decision(agreed: True, states:, pending: False)
  |> should.equal(p2p.P2p(p2p.Joiner, 2))
}

pub fn p2p_channel_full_test() {
  let states = [
    sample("a", True),
    sample("b", True),
    sample("c", True),
    sample("d", True),
  ]
  p2p.join_decision(agreed: True, states:, pending: False)
  |> should.equal(p2p.Reject(error.VoiceChannelFull))
}

pub fn admit_test() {
  p2p.admit(p2p.P2p(p2p.Initiator, 1), p2p.SfuGrant)
  |> should.equal(p2p.AdmitSfu)
  p2p.admit(p2p.P2p(p2p.Joiner, 2), p2p.SfuGrant)
  |> should.equal(p2p.Refuse(error.VoiceP2pUnavailable))
  p2p.admit(p2p.Sfu, p2p.Declined(error.VoiceChannelFull))
  |> should.equal(p2p.Refuse(error.VoiceTokenFailed))
}

pub fn mixed_invariant_test() {
  [sample("a", True), sample("b", False)]
  |> p2p.invariant_violation(None)
  |> should.equal(Error(error.VoiceP2pConsentRequired))
}

pub fn signal_route_test() {
  let sender = sample("a", True)
  let target = sample("b", True)
  p2p.signal_route([sender, target], "s", "b")
  |> should.be_ok
  p2p.signal_route([sender], "s", "a")
  |> should.be_error
}

pub fn session_gone_follows_the_session_id_test() {
  let peer = sample("a", True)
  p2p.session_gone(peer, fn(_) { ["s"] }) |> should.be_false
  p2p.session_gone(peer, fn(_) { ["other"] }) |> should.be_true
  p2p.session_gone(peer, fn(_) { [] }) |> should.be_true
  p2p.session_gone(sample("b", False), fn(_) { [] }) |> should.be_false
}
