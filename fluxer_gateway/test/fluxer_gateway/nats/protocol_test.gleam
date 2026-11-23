// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/nats/protocol
import gleam/bytes_tree
import gleam/list
import gleam/option.{None, Some}

pub fn info_test() {
  let assert Ok(#([protocol.Info(info)], <<>>)) =
    protocol.parse(<<"INFO {\"max_payload\":2048,\"headers\":true}\r\n">>)
  assert info.max_payload == 2048
  assert info.headers
}

pub fn info_defaults_test() {
  let assert Ok(#([protocol.Info(info)], <<>>)) =
    protocol.parse(<<"INFO {\"server_id\":\"x\"}\r\n">>)
  assert info.max_payload == protocol.default_max_payload
  assert !info.headers
}

pub fn msg_without_reply_test() {
  let assert Ok(#([op], <<>>)) = protocol.parse(<<"MSG a.b 7 5\r\nhello\r\n">>)
  assert op
    == protocol.Msg(subject: "a.b", sid: 7, reply_to: None, payload: <<"hello">>)
}

pub fn msg_with_reply_test() {
  let assert Ok(#([op], <<>>)) =
    protocol.parse(<<"MSG rpc.gateway.guild.get 3 _INBOX.x.1 2\r\n{}\r\n">>)
  assert op
    == protocol.Msg(
      subject: "rpc.gateway.guild.get",
      sid: 3,
      reply_to: Some("_INBOX.x.1"),
      payload: <<"{}">>,
    )
}

pub fn partial_payload_waits_test() {
  let buffer = <<"MSG a 1 10\r\nhello">>
  let assert Ok(#([], rest)) = protocol.parse(buffer)
  assert rest == buffer
}

pub fn partial_line_waits_test() {
  let assert Ok(#([], rest)) = protocol.parse(<<"MSG a 1">>)
  assert rest == <<"MSG a 1">>
}

pub fn several_ops_test() {
  let assert Ok(#(ops, rest)) =
    protocol.parse(<<"PING\r\nPONG\r\n+OK\r\nMSG s 2 0\r\n\r\nPI">>)
  assert ops
    == [
      protocol.Ping,
      protocol.Pong,
      protocol.ServerOk,
      protocol.Msg(subject: "s", sid: 2, reply_to: None, payload: <<>>),
    ]
  assert rest == <<"PI">>
}

pub fn payload_with_crlf_inside_test() {
  let assert Ok(#([op], <<>>)) = protocol.parse(<<"MSG s 1 4\r\na\r\nb\r\n">>)
  assert op
    == protocol.Msg(subject: "s", sid: 1, reply_to: None, payload: <<"a\r\nb">>)
}

pub fn no_responders_status_test() {
  let headers = <<"NATS/1.0 503\r\n\r\n">>
  let assert Ok(#([op], <<>>)) =
    protocol.parse(<<"HMSG _INBOX.x.9 1 16 16\r\n":utf8, headers:bits, "\r\n">>)
  assert op
    == protocol.HMsg(
      subject: "_INBOX.x.9",
      sid: 1,
      reply_to: None,
      status: Some(503),
      payload: <<>>,
    )
}

pub fn hmsg_payload_test() {
  let headers = <<"NATS/1.0\r\nA: b\r\n\r\n">>
  let assert Ok(#([protocol.HMsg(status:, payload:, ..)], <<>>)) =
    protocol.parse(<<"HMSG s 4 18 20\r\n":utf8, headers:bits, "hi\r\n">>)
  assert status == None
  assert payload == <<"hi">>
}

pub fn err_test() {
  let assert Ok(#([op], <<>>)) =
    protocol.parse(<<"-ERR 'Authorization Violation'\r\n">>)
  assert op == protocol.Err("Authorization Violation")
}

pub fn unknown_op_test() {
  let assert Error(protocol.UnknownOperation(_)) =
    protocol.parse(<<"WHAT\r\n">>)
}

pub fn bad_msg_arguments_test() {
  let assert Error(protocol.BadArguments(_)) =
    protocol.parse(<<"MSG a b c\r\n">>)
}

pub fn publish_encoding_test() {
  assert bytes_tree.to_bit_array(protocol.publish("a.b", None, <<"xy">>))
    == <<"PUB a.b 2\r\nxy\r\n">>
  assert bytes_tree.to_bit_array(protocol.publish("a", Some("_INBOX.1"), <<>>))
    == <<"PUB a _INBOX.1 0\r\n\r\n">>
}

pub fn subscribe_encoding_test() {
  assert bytes_tree.to_bit_array(protocol.subscribe(
      "rpc.gateway.>",
      Some("gateway"),
      4,
    ))
    == <<"SUB rpc.gateway.> gateway 4\r\n">>
  assert bytes_tree.to_bit_array(protocol.subscribe("x", None, 2))
    == <<"SUB x 2\r\n">>
  assert bytes_tree.to_bit_array(protocol.unsubscribe(2)) == <<"UNSUB 2\r\n">>
}

pub fn valid_token_test() {
  assert protocol.valid_token("rpc.gateway.guild.>")
  assert !protocol.valid_token("")
  assert !protocol.valid_token("a b")
  assert !protocol.valid_token("a\r\n")
}

fn feed_all(
  reader: protocol.Reader,
  chunks: List(BitArray),
  acc: List(protocol.ServerOp),
) -> #(List(protocol.ServerOp), protocol.Reader) {
  case chunks {
    [] -> #(acc, reader)
    [chunk, ..rest] -> {
      let assert Ok(#(ops, next)) = protocol.feed(reader, chunk)
      feed_all(next, rest, list.append(acc, ops))
    }
  }
}

pub fn reader_joins_a_message_split_over_packets_test() {
  let #(ops, _) =
    feed_all(
      protocol.reader(),
      [
        <<"MS">>,
        <<"G a 4 1">>,
        <<"0\r\nhel">>,
        <<"lo">>,
        <<"world\r">>,
        <<"\n">>,
      ],
      [],
    )
  assert ops
    == [
      protocol.Msg(subject: "a", sid: 4, reply_to: None, payload: <<
        "helloworld",
      >>),
    ]
}

pub fn reader_waits_for_the_whole_payload_test() {
  let assert Ok(#([], reader)) =
    protocol.feed(protocol.reader(), <<"MSG a 4 10\r\nhello">>)
  let assert Ok(#([], reader)) = protocol.feed(reader, <<"wor">>)
  let assert Ok(#([op, protocol.Ping], _)) =
    protocol.feed(reader, <<"ld\r\nPING\r\n">>)
  assert op
    == protocol.Msg(subject: "a", sid: 4, reply_to: None, payload: <<
      "helloworld",
    >>)
}

pub fn reader_keeps_the_order_of_ops_test() {
  let #(ops, _) =
    feed_all(
      protocol.reader(),
      [
        <<"PING\r\nMSG s 1 2\r\nhi\r\nHMSG s 1 12 14\r\nNATS/1.0\r\n\r\nok\r">>,
        <<"\nPONG\r\n">>,
      ],
      [],
    )
  assert ops
    == [
      protocol.Ping,
      protocol.Msg(subject: "s", sid: 1, reply_to: None, payload: <<"hi">>),
      protocol.HMsg(
        subject: "s",
        sid: 1,
        reply_to: None,
        status: None,
        payload: <<"ok">>,
      ),
      protocol.Pong,
    ]
}

pub fn reader_reports_bad_lines_test() {
  let assert Ok(#([], reader)) = protocol.feed(protocol.reader(), <<"WH">>)
  assert protocol.feed(reader, <<"AT\r\n">>)
    == Error(protocol.UnknownOperation("WHAT"))
}
