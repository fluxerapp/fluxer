// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/gateway/ws/fragment.{
  type Extended, Complete, Partial, TooBig,
}
import fluxer_gateway/gateway/ws/frame
import gleeunit/should

fn feed_empty(extended: Extended, remaining: Int) -> Extended {
  case extended, remaining {
    Partial(partial), n if n > 0 ->
      feed_empty(fragment.extend(partial, False, <<>>, 16_384), n - 1)
    _, _ -> extended
  }
}

pub fn joins_fragments_in_order_test() {
  let assert Partial(partial) =
    fragment.start(frame.Text, <<"he">>)
    |> fragment.extend(False, <<"ll">>, 16_384)
  fragment.extend(partial, True, <<"o">>, 16_384)
  |> should.equal(Complete(opcode: frame.Text, payload: <<"hello">>))
}

pub fn rejects_oversized_message_test() {
  fragment.start(frame.Binary, <<0:size(80)>>)
  |> fragment.extend(True, <<0:size(16)>>, 11)
  |> should.equal(TooBig)
}

pub fn accepts_message_at_size_limit_test() {
  fragment.start(frame.Binary, <<0:size(80)>>)
  |> fragment.extend(True, <<0:size(8)>>, 11)
  |> should.equal(Complete(opcode: frame.Binary, payload: <<0:size(88)>>))
}

pub fn empty_continuations_are_bounded_test() {
  Partial(fragment.start(frame.Text, <<>>))
  |> feed_empty(fragment.max_frames * 2)
  |> should.equal(TooBig)
}

pub fn accepts_message_at_frame_limit_test() {
  let assert Partial(partial) =
    Partial(fragment.start(frame.Text, <<>>))
    |> feed_empty(fragment.max_frames - 2)
  fragment.extend(partial, True, <<"ok">>, 16_384)
  |> should.equal(Complete(opcode: frame.Text, payload: <<"ok">>))
}

pub fn rejects_message_past_frame_limit_test() {
  let assert Partial(partial) =
    Partial(fragment.start(frame.Text, <<>>))
    |> feed_empty(fragment.max_frames - 1)
  fragment.extend(partial, True, <<"ok">>, 16_384)
  |> should.equal(TooBig)
}
