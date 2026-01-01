//// Copyright (C) 2026 Fluxer Contributors
////
//// This file is part of Fluxer.
////
//// Fluxer is free software: you can redistribute it and/or modify
//// it under the terms of the GNU Affero General Public License as published by
//// the Free Software Foundation, either version 3 of the License, or
//// (at your option) any later version.
////
//// Fluxer is distributed in the hope that it will be useful,
//// but WITHOUT ANY WARRANTY; without even the implied warranty of
//// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//// GNU Affero General Public License for more details.
////
//// You should have received a copy of the GNU Affero General Public License
//// along with Fluxer. If not, see <https://www.gnu.org/licenses/>.

import fluxer_admin/api/common.{
  type ApiError, NetworkError, NotFound, ServerError,
}
import fluxer_admin/web.{type Context}
import gleam/dynamic/decode
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/int
import gleam/json
import gleam/option.{type Option, None, Some}

pub type DataPoint {
  DataPoint(timestamp: Int, value: Float)
}

pub type QueryResponse {
  QueryResponse(metric: String, data: List(DataPoint))
}

pub type TopEntry {
  TopEntry(label: String, value: Float)
}

pub type AggregateResponse {
  AggregateResponse(
    metric: String,
    total: Float,
    breakdown: option.Option(List(TopEntry)),
  )
}

pub type TopQueryResponse {
  TopQueryResponse(metric: String, entries: List(TopEntry))
}

pub type CrashEvent {
  CrashEvent(
    id: String,
    timestamp: Int,
    guild_id: String,
    stacktrace: String,
    notified: Bool,
  )
}

pub type CrashesResponse {
  CrashesResponse(crashes: List(CrashEvent))
}

pub fn query_metrics(
  ctx: Context,
  metric: String,
  start: Option(String),
  end: Option(String),
) -> Result(QueryResponse, ApiError) {
  case ctx.metrics_endpoint {
    None -> Error(NotFound)
    Some(endpoint) -> {
      let query_params = case start, end {
        Some(s), Some(e) ->
          "?metric=" <> metric <> "&start=" <> s <> "&end=" <> e
        Some(s), None -> "?metric=" <> metric <> "&start=" <> s
        None, Some(e) -> "?metric=" <> metric <> "&end=" <> e
        None, None -> "?metric=" <> metric
      }
      let url = endpoint <> "/query" <> query_params

      let assert Ok(req) = request.to(url)
      let req = req |> request.set_method(http.Get)

      case httpc.send(req) {
        Ok(resp) if resp.status == 200 -> {
          let data_point_decoder = {
            use timestamp <- decode.field("timestamp", decode.int)
            use value <- decode.field("value", decode.float)
            decode.success(DataPoint(timestamp: timestamp, value: value))
          }

          let decoder = {
            use metric_name <- decode.field("metric", decode.string)
            use data <- decode.field("data", decode.list(data_point_decoder))
            decode.success(QueryResponse(metric: metric_name, data: data))
          }

          case json.parse(resp.body, decoder) {
            Ok(result) -> Ok(result)
            Error(_) -> Error(ServerError)
          }
        }
        Ok(_) -> Error(ServerError)
        Error(_) -> Error(NetworkError)
      }
    }
  }
}

pub fn query_aggregate(
  ctx: Context,
  metric: String,
) -> Result(AggregateResponse, ApiError) {
  query_aggregate_grouped(ctx, metric, option.None)
}

fn top_entry_decoder() -> decode.Decoder(TopEntry) {
  {
    use label <- decode.field("label", decode.string)
    use value <- decode.field("value", decode.float)
    decode.success(TopEntry(label: label, value: value))
  }
}

pub fn query_aggregate_grouped(
  ctx: Context,
  metric: String,
  group_by: option.Option(String),
) -> Result(AggregateResponse, ApiError) {
  case ctx.metrics_endpoint {
    None -> Error(NotFound)
    Some(endpoint) -> {
      let query_params = case group_by {
        option.Some(group) -> "?metric=" <> metric <> "&group_by=" <> group
        option.None -> "?metric=" <> metric
      }
      let url = endpoint <> "/query/aggregate" <> query_params

      let assert Ok(req) = request.to(url)
      let req = req |> request.set_method(http.Get)

      case httpc.send(req) {
        Ok(resp) if resp.status == 200 -> {
          let decoder = {
            use metric_name <- decode.field("metric", decode.string)
            use total <- decode.field("total", decode.float)
            use breakdown <- decode.optional_field(
              "breakdown",
              option.None,
              decode.list(top_entry_decoder()) |> decode.map(option.Some),
            )
            decode.success(AggregateResponse(
              metric: metric_name,
              total: total,
              breakdown: breakdown,
            ))
          }

          case json.parse(resp.body, decoder) {
            Ok(result) -> Ok(result)
            Error(_) -> Error(ServerError)
          }
        }
        Ok(_) -> Error(ServerError)
        Error(_) -> Error(NetworkError)
      }
    }
  }
}

pub fn query_top(
  ctx: Context,
  metric: String,
  limit: Int,
) -> Result(TopQueryResponse, ApiError) {
  case ctx.metrics_endpoint {
    None -> Error(NotFound)
    Some(endpoint) -> {
      let url =
        endpoint
        <> "/query/top?metric="
        <> metric
        <> "&limit="
        <> int.to_string(limit)

      let assert Ok(req) = request.to(url)
      let req = req |> request.set_method(http.Get)

      case httpc.send(req) {
        Ok(resp) if resp.status == 200 -> {
          let decoder = {
            use metric_name <- decode.field("metric", decode.string)
            use entries <- decode.field(
              "entries",
              decode.list(top_entry_decoder()),
            )
            decode.success(TopQueryResponse(
              metric: metric_name,
              entries: entries,
            ))
          }

          case json.parse(resp.body, decoder) {
            Ok(result) -> Ok(result)
            Error(_) -> Error(ServerError)
          }
        }
        Ok(_) -> Error(ServerError)
        Error(_) -> Error(NetworkError)
      }
    }
  }
}

pub fn query_crashes(
  ctx: Context,
  limit: Int,
) -> Result(CrashesResponse, ApiError) {
  case ctx.metrics_endpoint {
    None -> Error(NotFound)
    Some(endpoint) -> {
      let url = endpoint <> "/query/crashes?limit=" <> int.to_string(limit)

      let assert Ok(req) = request.to(url)
      let req = req |> request.set_method(http.Get)

      case httpc.send(req) {
        Ok(resp) if resp.status == 200 -> {
          let crash_decoder = {
            use id <- decode.field("id", decode.string)
            use timestamp <- decode.field("timestamp", decode.int)
            use guild_id <- decode.field("guild_id", decode.string)
            use stacktrace <- decode.field("stacktrace", decode.string)
            use notified <- decode.field("notified", decode.bool)
            decode.success(CrashEvent(
              id: id,
              timestamp: timestamp,
              guild_id: guild_id,
              stacktrace: stacktrace,
              notified: notified,
            ))
          }

          let decoder = {
            use crashes <- decode.field("crashes", decode.list(crash_decoder))
            decode.success(CrashesResponse(crashes: crashes))
          }

          case json.parse(resp.body, decoder) {
            Ok(result) -> Ok(result)
            Error(_) -> Error(ServerError)
          }
        }
        Ok(_) -> Error(ServerError)
        Error(_) -> Error(NetworkError)
      }
    }
  }
}
