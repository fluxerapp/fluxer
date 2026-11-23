// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/api/http
import fluxer_gateway/api/metrics as api_metrics
import fluxer_gateway/config/durations
import fluxer_gateway/config/settings.{type Settings}
import fluxer_gateway/core/clock.{type Deadline}
import fluxer_gateway/core/json_scan
import fluxer_gateway/util/decode as edge
import gleam/bit_array
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import logging

const rpc_path = "/internal/rpc"

const auth_header = "x-fluxer-rpc-auth"

const default_client_ip = "127.0.0.1"

fn retry_base_ms() -> Int {
  durations.api().client_retry_base_ms
}

fn retry_cap_ms() -> Int {
  durations.api().client_retry_cap_ms
}

const max_attempts = 5

pub type ApiError {
  ApiUnavailable
  ApiTimeout
  ApiHttp(status: Int)
  ApiRejected(status: Int, message: String)
  ApiBadReply
}

pub fn is_open() -> Bool {
  http.is_open()
}

type Failure {
  Retryable(reason: ApiError, hint: Option(Int))
  Final(reason: ApiError)
}

type Request {
  Request(kind: String, body: BitArray, ip: String)
}

pub fn call(
  kind: String,
  fields: List(#(String, Json)),
  deadline: Deadline,
) -> Result(Dynamic, ApiError) {
  attempt(request(kind, fields), deadline, reply_data)
  |> result.map_error(fn(failure) { failure.reason })
}

pub fn call_with_retry(
  kind: String,
  fields: List(#(String, Json)),
  deadline: Deadline,
) -> Result(Dynamic, ApiError) {
  retry_loop(request(kind, fields), deadline, reply_data, 1, retry_base_ms())
}

pub fn call_raw(
  kind: String,
  fields: List(#(String, Json)),
  deadline: Deadline,
) -> Result(BitArray, ApiError) {
  attempt(request(kind, fields), deadline, reply_raw)
  |> result.map_error(fn(failure) { failure.reason })
}

pub fn call_raw_with_retry(
  kind: String,
  fields: List(#(String, Json)),
  deadline: Deadline,
) -> Result(BitArray, ApiError) {
  retry_loop(request(kind, fields), deadline, reply_raw, 1, retry_base_ms())
}

fn request(kind: String, fields: List(#(String, Json))) -> Request {
  let body =
    json.object([#("type", json.string(kind)), ..fields])
    |> json.to_string
    |> bit_array.from_string
  Request(kind:, body:, ip: client_ip(body))
}

fn retry_loop(
  call: Request,
  deadline: Deadline,
  parse: fn(Int, BitArray) -> Result(a, ApiError),
  attempt_number: Int,
  previous_ms: Int,
) -> Result(a, ApiError) {
  case attempt(call, deadline, parse) {
    Ok(data) -> Ok(data)
    Error(Final(reason)) -> Error(reason)
    Error(Retryable(reason, hint)) ->
      case is_retryable(reason) {
        False -> Error(reason)
        True -> {
          let sleep = next_sleep(previous_ms, hint)
          case
            attempt_number >= max_attempts
            || sleep >= clock.remaining_ms(deadline)
          {
            True -> {
              logging.log(
                logging.Error,
                "API RPC retries exhausted: attempts="
                  <> int.to_string(attempt_number)
                  <> " reason="
                  <> describe(reason)
                  <> " request_type="
                  <> call.kind,
              )
              Error(reason)
            }
            False -> {
              logging.log(
                logging.Warning,
                "API RPC retrying: attempt="
                  <> int.to_string(attempt_number)
                  <> "/"
                  <> int.to_string(max_attempts)
                  <> " delay_ms="
                  <> int.to_string(sleep)
                  <> " reason="
                  <> describe(reason)
                  <> " request_type="
                  <> call.kind,
              )
              api_metrics.retry(call.kind)
              process.sleep(sleep)
              retry_loop(call, deadline, parse, attempt_number + 1, sleep)
            }
          }
        }
      }
  }
}

pub fn next_sleep(previous_ms: Int, hint: Option(Int)) -> Int {
  case hint {
    Some(retry_after_ms) -> int.max(retry_after_ms, 0)
    None -> {
      let high = int.max(previous_ms * 3, retry_base_ms())
      int.min(
        retry_cap_ms(),
        retry_base_ms() + int.random(high - retry_base_ms() + 1),
      )
    }
  }
}

pub fn is_retryable(reason: ApiError) -> Bool {
  case reason {
    ApiTimeout | ApiUnavailable -> True
    ApiHttp(status) -> status >= 500
    ApiRejected(..) | ApiBadReply -> False
  }
}

fn describe(reason: ApiError) -> String {
  case reason {
    ApiUnavailable -> "unavailable"
    ApiTimeout -> "timeout"
    ApiHttp(status) -> "http_" <> int.to_string(status)
    ApiRejected(status, message) ->
      "rejected_" <> int.to_string(status) <> ": " <> message
    ApiBadReply -> "bad_reply"
  }
}

fn attempt(
  call: Request,
  deadline: Deadline,
  parse: fn(Int, BitArray) -> Result(a, ApiError),
) -> Result(a, Failure) {
  let timeout_ms = clock.remaining_ms(deadline)
  case timeout_ms {
    0 -> Error(Retryable(ApiTimeout, None))
    _ -> {
      let current = settings.get()
      let started = clock.monotonic_ms()
      let sent =
        http.post(
          rpc_url(current),
          headers(current, call.ip),
          call.body,
          timeout_ms,
        )
      let parsed = case sent {
        Ok(http.Response(status, reply)) -> Ok(parse(status, reply))
        Error(failure) -> Error(failure)
      }
      api_metrics.request(
        call.kind,
        outcome(parsed),
        clock.monotonic_ms() - started,
        was_sent(sent),
      )
      case parsed {
        Ok(parsed) -> result.map_error(parsed, Retryable(_, None))
        Error(http.CircuitOpen(retry_after_ms)) ->
          Error(Retryable(ApiUnavailable, Some(retry_after_ms)))
        Error(http.Overloaded) -> Error(Retryable(ApiUnavailable, None))
        Error(http.Timeout) -> Error(Retryable(ApiTimeout, None))
        Error(http.Reset(reason)) -> {
          logging.log(
            logging.Warning,
            "API RPC connection reset: "
              <> reason
              <> " request_type="
              <> call.kind,
          )
          Error(Retryable(ApiUnavailable, None))
        }
        Error(http.Transport(reason)) -> {
          logging.log(
            logging.Warning,
            "API RPC transport error: "
              <> reason
              <> " request_type="
              <> call.kind,
          )
          Error(Final(ApiUnavailable))
        }
        Error(http.BadUrl) -> Error(Final(ApiUnavailable))
      }
    }
  }
}

fn was_sent(sent: Result(http.Response, http.HttpFailure)) -> Bool {
  case sent {
    Error(http.CircuitOpen(_)) | Error(http.Overloaded) | Error(http.BadUrl) ->
      False
    _ -> True
  }
}

fn outcome(parsed: Result(Result(a, ApiError), http.HttpFailure)) -> String {
  case parsed {
    Ok(Ok(_)) -> "ok"
    Ok(Error(ApiHttp(401))) -> "unauthorized"
    Ok(Error(ApiHttp(429))) -> "throttled"
    Ok(Error(ApiHttp(_))) -> "server_error"
    Ok(Error(ApiRejected(..))) -> "rejected"
    Ok(Error(ApiBadReply)) -> "bad_reply"
    Ok(Error(ApiTimeout)) -> "timeout"
    Ok(Error(ApiUnavailable)) -> "transport"
    Error(http.CircuitOpen(_)) -> "circuit_open"
    Error(http.Overloaded) -> "overloaded"
    Error(http.Timeout) -> "timeout"
    Error(http.Reset(_)) -> "reset"
    Error(http.Transport(_)) | Error(http.BadUrl) -> "transport"
  }
}

pub fn reply_data(status: Int, body: BitArray) -> Result(Dynamic, ApiError) {
  case status >= 200 && status < 300 {
    True ->
      case edge.decode_json(body) {
        Error(Nil) -> Error(ApiBadReply)
        Ok(decoded) -> {
          let data_decoder =
            decode.optional_field(
              "data",
              empty_object(),
              decode.dynamic,
              fn(data) { decode.success(data) },
            )
          case
            decode.run(decoded, decode.dict(decode.string, decode.dynamic)),
            decode.run(decoded, data_decoder)
          {
            Ok(_), Ok(data) -> Ok(data)
            _, _ -> Error(ApiBadReply)
          }
        }
      }
    False -> status_error(status, body)
  }
}

pub fn reply_raw(status: Int, body: BitArray) -> Result(BitArray, ApiError) {
  case status >= 200 && status < 300 {
    True ->
      case json_scan.object(body) {
        Error(Nil) -> Error(ApiBadReply)
        Ok(fields) ->
          list.key_find(fields, "data")
          |> result.unwrap(<<"{}">>)
          |> Ok
      }
    False -> status_error(status, body)
  }
}

fn status_error(status: Int, body: BitArray) -> Result(a, ApiError) {
  case status {
    401 | 429 -> Error(ApiHttp(status))
    _ if status >= 500 -> Error(ApiHttp(status))
    _ ->
      Error(ApiRejected(status, result.unwrap(bit_array.to_string(body), "")))
  }
}

fn empty_object() -> Dynamic {
  dynamic.properties([])
}

fn rpc_url(current: Settings) -> String {
  case current.api_rpc_endpoint {
    Some(endpoint) ->
      case string.trim(endpoint) {
        "" -> trim_trailing_slash(current.api_internal_url) <> rpc_path
        trimmed -> trim_trailing_slash(trimmed)
      }
    None -> trim_trailing_slash(current.api_internal_url) <> rpc_path
  }
}

pub fn trim_trailing_slash(url: String) -> String {
  case string.ends_with(url, "/") {
    True -> trim_trailing_slash(string.drop_end(url, 1))
    False -> url
  }
}

fn headers(current: Settings, ip: String) -> List(#(String, String)) {
  let ip_header = case string.lowercase(string.trim(current.client_ip_header)) {
    "" -> "x-forwarded-for"
    found -> found
  }
  [
    #("content-type", "application/json"),
    #(ip_header, ip),
    #(auth_header, current.rpc_auth_token),
  ]
}

pub fn client_ip(body: BitArray) -> String {
  case
    json_scan.field(body, "ip")
    |> result.try(fn(value) {
      json.parse_bits(value, decode.string) |> result.replace_error(Nil)
    })
  {
    Ok("") | Error(Nil) -> default_client_ip
    Ok(ip) -> ip
  }
}
