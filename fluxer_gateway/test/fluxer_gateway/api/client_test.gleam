// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/api/client
import gleam/dict
import gleam/dynamic/decode
import gleam/json
import gleam/option.{None, Some}

pub fn reply_data_test() {
  let assert Ok(data) = client.reply_data(200, <<"{\"data\":{\"a\":1}}">>)
  assert decode.run(data, decode.at(["a"], decode.int)) == Ok(1)
}

pub fn reply_without_data_is_empty_object_test() {
  let assert Ok(data) = client.reply_data(204, <<"{\"other\":true}">>)
  assert decode.run(data, decode.dict(decode.string, decode.int))
    == Ok(dict.new())
}

pub fn bad_json_test() {
  assert client.reply_data(200, <<"not json">>) == Error(client.ApiBadReply)
  assert client.reply_data(200, <<"[1,2]">>) == Error(client.ApiBadReply)
}

pub fn status_errors_test() {
  assert client.reply_data(401, <<"{}">>) == Error(client.ApiHttp(401))
  assert client.reply_data(429, <<"{}">>) == Error(client.ApiHttp(429))
  assert client.reply_data(503, <<"x">>) == Error(client.ApiHttp(503))
  assert client.reply_data(403, <<"{\"code\":\"X\"}">>)
    == Error(client.ApiRejected(403, "{\"code\":\"X\"}"))
}

pub fn retryable_test() {
  assert client.is_retryable(client.ApiTimeout)
  assert client.is_retryable(client.ApiUnavailable)
  assert client.is_retryable(client.ApiHttp(502))
  assert !client.is_retryable(client.ApiHttp(429))
  assert !client.is_retryable(client.ApiRejected(400, "no"))
  assert !client.is_retryable(client.ApiBadReply)
}

pub fn next_sleep_test() {
  assert client.next_sleep(1000, Some(700)) == 700
  let first = client.next_sleep(250, None)
  assert first >= 250 && first <= 750
  assert client.next_sleep(100_000, None) <= 15_000
}

pub fn trim_trailing_slash_test() {
  assert client.trim_trailing_slash("http://api:8080//") == "http://api:8080"
  assert client.trim_trailing_slash("http://api") == "http://api"
}

pub fn client_ip_test() {
  assert client.client_ip(body([#("ip", json.string("10.0.0.1"))]))
    == "10.0.0.1"
  assert client.client_ip(body([#("ip", json.string(""))])) == "127.0.0.1"
  assert client.client_ip(body([#("ip", json.null())])) == "127.0.0.1"
  assert client.client_ip(body([])) == "127.0.0.1"
}

fn body(fields: List(#(String, json.Json))) -> BitArray {
  <<json.to_string(json.object([#("type", json.string("x")), ..fields])):utf8>>
}

pub fn reply_raw_test() {
  assert client.reply_raw(200, <<"{\"data\":{\"a\":1}}">>)
    == Ok(<<"{\"a\":1}">>)
  assert client.reply_raw(204, <<"{\"other\":true}">>) == Ok(<<"{}">>)
  assert client.reply_raw(200, <<"not json">>) == Error(client.ApiBadReply)
  assert client.reply_raw(200, <<"[1,2]">>) == Error(client.ApiBadReply)
  assert client.reply_raw(401, <<"{}">>) == Error(client.ApiHttp(401))
  assert client.reply_raw(403, <<"{\"code\":\"X\"}">>)
    == Error(client.ApiRejected(403, "{\"code\":\"X\"}"))
}
