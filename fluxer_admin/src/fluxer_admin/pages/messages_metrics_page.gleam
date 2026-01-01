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

import fluxer_admin/api/common
import fluxer_admin/api/metrics
import fluxer_admin/components/flash
import fluxer_admin/components/layout
import fluxer_admin/components/ui
import fluxer_admin/web.{type Context, type Session, prepend_base_path}
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import lustre/attribute as a
import lustre/element
import lustre/element/html as h
import wisp.{type Response}

pub fn view(
  ctx: Context,
  session: Session,
  current_admin: Option(common.UserLookupResult),
  flash_data: Option(flash.Flash),
) -> Response {
  let content = case ctx.metrics_endpoint {
    None -> render_not_configured()
    Some(_) -> render_dashboard(ctx)
  }

  let html =
    layout.page(
      "Messaging & API Metrics",
      "messages-metrics",
      ctx,
      session,
      current_admin,
      flash_data,
      content,
    )
  wisp.html_response(element.to_document_string(html), 200)
}

fn render_not_configured() {
  ui.stack("6", [
    ui.heading_page("Messaging & API Metrics"),
    h.div(
      [
        a.class(
          "bg-yellow-50 border border-yellow-200 rounded-lg p-6 text-center",
        ),
      ],
      [
        h.p([a.class("text-yellow-800")], [
          element.text(
            "Metrics service not configured. Set FLUXER_METRICS_HOST to enable.",
          ),
        ]),
      ],
    ),
  ])
}

fn render_dashboard(ctx: Context) {
  let messages_sent = metrics.query_aggregate(ctx, "message.send")
  let messages_edited = metrics.query_aggregate(ctx, "message.edit")
  let messages_deleted = metrics.query_aggregate(ctx, "message.delete")
  let attachments_created = metrics.query_aggregate(ctx, "attachment.created")
  let attachment_storage =
    metrics.query_aggregate(ctx, "attachment.storage.bytes")

  let proxy_endpoint = prepend_base_path(ctx, "/api/metrics")

  h.div([], [
    ui.heading_page("Messaging & API Metrics Dashboard"),
    h.div([a.class("mt-6")], [
      h.div(
        [a.class("bg-white border border-neutral-200 rounded-lg shadow-sm")],
        [
          h.div([a.class("p-6")], [
            ui.heading_section("Cumulative Totals"),
            ui.text_small_muted(
              "Lifetime totals for message and attachment activity",
            ),
            h.div(
              [
                a.class(
                  "grid grid-cols-2 md:grid-cols-3 lg:grid-cols-5 gap-4 mt-4",
                ),
              ],
              [
                render_stat_card("Messages Sent", messages_sent),
                render_stat_card("Messages Edited", messages_edited),
                render_stat_card("Messages Deleted", messages_deleted),
                render_stat_card("Attachments Created", attachments_created),
                render_storage_card(attachment_storage),
              ],
            ),
          ]),
        ],
      ),
    ]),
    h.div([a.class("mt-6")], [
      h.div(
        [a.class("bg-white border border-neutral-200 rounded-lg shadow-sm")],
        [
          h.div([a.class("p-6")], [
            ui.heading_section("Attachment Volume"),
            ui.text_small_muted(
              "Attachment bytes sent over time - useful for detecting spikes in upload activity",
            ),
            h.div([a.class("mt-4")], [
              element.element(
                "canvas",
                [a.id("attachmentBytesChart"), a.attribute("height", "250")],
                [],
              ),
            ]),
          ]),
        ],
      ),
    ]),
    h.div([a.class("mt-6")], [
      h.div(
        [a.class("bg-white border border-neutral-200 rounded-lg shadow-sm")],
        [
          h.div([a.class("p-6")], [
            ui.heading_section("Message Activity"),
            ui.text_small_muted(
              "Message send, edit, and delete rates over time - useful for detecting unusual activity spikes",
            ),
            h.div([a.class("mt-4")], [
              element.element(
                "canvas",
                [a.id("messageActivityChart"), a.attribute("height", "250")],
                [],
              ),
            ]),
          ]),
        ],
      ),
    ]),
    h.div([a.class("mt-6")], [
      h.div(
        [a.class("bg-white border border-neutral-200 rounded-lg shadow-sm")],
        [
          h.div([a.class("p-6")], [
            ui.heading_section("Reactions"),
            ui.text_small_muted("Reaction add and remove rates over time"),
            h.div([a.class("mt-4")], [
              element.element(
                "canvas",
                [a.id("reactionsChart"), a.attribute("height", "250")],
                [],
              ),
            ]),
          ]),
        ],
      ),
    ]),
    h.div([a.class("mt-6")], [
      h.div(
        [a.class("bg-white border border-neutral-200 rounded-lg shadow-sm")],
        [
          h.div([a.class("p-6")], [
            ui.heading_section("API Latency"),
            ui.text_small_muted(
              "API request latency percentiles (p50, p95, p99) over time",
            ),
            h.div([a.id("latency-stats-container"), a.class("mt-4 mb-4")], [
              h.div([a.class("grid grid-cols-1 md:grid-cols-3 gap-4")], [
                render_loading_stat_card("Current P50", "latency-p50"),
                render_loading_stat_card("Current P95", "latency-p95"),
                render_loading_stat_card("Current P99", "latency-p99"),
              ]),
            ]),
            h.div([a.class("mt-4")], [
              element.element(
                "canvas",
                [a.id("latencyChart"), a.attribute("height", "250")],
                [],
              ),
            ]),
          ]),
        ],
      ),
    ]),
    h.div([a.class("mt-6")], [
      h.div(
        [a.class("bg-white border border-neutral-200 rounded-lg shadow-sm")],
        [
          h.div([a.class("p-6")], [
            ui.heading_section("API Response Status Codes"),
            ui.text_small_muted(
              "API response status code counts (2xx, 4xx, 5xx) over time",
            ),
            h.div([a.class("mt-4")], [
              element.element(
                "canvas",
                [a.id("statusCodesChart"), a.attribute("height", "250")],
                [],
              ),
            ]),
          ]),
        ],
      ),
    ]),
    h.script([a.src("https://fluxerstatic.com/libs/chartjs/chart.min.js")], ""),
    h.script([], render_charts_script(proxy_endpoint)),
  ])
}

fn render_stat_card(
  label: String,
  result: Result(metrics.AggregateResponse, common.ApiError),
) {
  let value = case result {
    Ok(resp) -> format_number(resp.total)
    Error(_) -> "-"
  }

  h.div([a.class("bg-neutral-50 rounded-lg p-4 border border-neutral-200")], [
    h.div([a.class("text-xs text-neutral-600 uppercase tracking-wider mb-1")], [
      element.text(label),
    ]),
    h.div([a.class("text-base font-semibold text-neutral-900")], [
      element.text(value),
    ]),
  ])
}

fn render_storage_card(
  result: Result(metrics.AggregateResponse, common.ApiError),
) {
  let value = case result {
    Ok(resp) -> format_bytes(resp.total)
    Error(_) -> "-"
  }

  h.div([a.class("bg-neutral-50 rounded-lg p-4 border border-neutral-200")], [
    h.div([a.class("text-xs text-neutral-600 uppercase tracking-wider mb-1")], [
      element.text("Storage Used"),
    ]),
    h.div([a.class("text-base font-semibold text-neutral-900")], [
      element.text(value),
    ]),
  ])
}

fn render_loading_stat_card(label: String, id: String) {
  h.div([a.class("bg-neutral-50 rounded-lg p-4 border border-neutral-200")], [
    h.div([a.class("text-xs text-neutral-600 uppercase tracking-wider mb-1")], [
      element.text(label),
    ]),
    h.div([a.id(id), a.class("text-base font-semibold text-neutral-900")], [
      element.text("-"),
    ]),
  ])
}

fn format_number(n: Float) -> String {
  let int_val = float.truncate(n)
  format_int_with_commas(int_val)
}

fn format_int_with_commas(n: Int) -> String {
  let s = int.to_string(n)
  let len = string.length(s)

  case len {
    _ if len <= 3 -> s
    _ -> {
      let groups = reverse_groups(s, [])
      string.join(list.reverse(groups), ",")
    }
  }
}

fn reverse_groups(s: String, acc: List(String)) -> List(String) {
  let len = string.length(s)
  case len {
    0 -> acc
    _ if len <= 3 -> [s, ..acc]
    _ -> {
      let group = string.slice(s, len - 3, 3)
      let rest = string.slice(s, 0, len - 3)
      reverse_groups(rest, [group, ..acc])
    }
  }
}

fn format_bytes(bytes: Float) -> String {
  case bytes {
    _ if bytes <. 1024.0 -> format_number(bytes) <> " B"
    _ if bytes <. 1_048_576.0 -> {
      let kb = bytes /. 1024.0
      float_to_string_rounded(kb, 2) <> " KB"
    }
    _ if bytes <. 1_073_741_824.0 -> {
      let mb = bytes /. 1_048_576.0
      float_to_string_rounded(mb, 2) <> " MB"
    }
    _ -> {
      let gb = bytes /. 1_073_741_824.0
      float_to_string_rounded(gb, 2) <> " GB"
    }
  }
}

fn float_to_string_rounded(value: Float, decimals: Int) -> String {
  let multiplier = case decimals {
    0 -> 1.0
    1 -> 10.0
    2 -> 100.0
    3 -> 1000.0
    _ -> 100.0
  }

  let rounded = float.round(value *. multiplier) |> int.to_float
  let result = rounded /. multiplier

  case decimals {
    0 -> {
      let int_value = float.round(result)
      int.to_string(int_value)
    }
    _ -> {
      let str = float.to_string(result)
      case string.contains(str, ".") {
        True -> str
        False -> str <> ".0"
      }
    }
  }
}

fn render_charts_script(metrics_endpoint: String) -> String {
  "
  (async function() {
    const endpoint = '" <> metrics_endpoint <> "';
    if (!endpoint) return;

    const formatBytes = (bytes) => {
      if (bytes === 0) return '0 B';
      const k = 1024;
      const sizes = ['B', 'KB', 'MB', 'GB', 'TB'];
      const i = Math.floor(Math.log(bytes) / Math.log(k));
      return parseFloat((bytes / Math.pow(k, i)).toFixed(2)) + ' ' + sizes[i];
    };

    const formatMs = (ms) => {
      if (ms === null || ms === undefined) return '-';
      return ms.toFixed(2) + ' ms';
    };

    const alignData = (data, timestamps) => {
      const map = new Map(data.map(d => [d.timestamp, d.value]));
      return timestamps.map(ts => map.get(ts) ?? null);
    };

    const formatTimeLabel = (ts) => {
      const d = new Date(ts);
      return d.toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' });
    };

    const getLatestValue = (data) => {
      if (!data || data.length === 0) return null;
      const sorted = [...data].sort((a, b) => b.timestamp - a.timestamp);
      return sorted[0]?.value ?? null;
    };

    try {
      const attachmentBytesResp = await fetch(endpoint + '/query?metric=attachment.storage.bytes').then(r => r.json());

      const abTimestamps = attachmentBytesResp.data.map(d => d.timestamp).sort((a, b) => a - b);

      if (abTimestamps.length > 0) {
        new Chart(document.getElementById('attachmentBytesChart'), {
          type: 'line',
          data: {
            labels: abTimestamps.map(formatTimeLabel),
            datasets: [
              { label: 'Attachment Bytes', data: alignData(attachmentBytesResp.data, abTimestamps), borderColor: 'rgb(168, 85, 247)', backgroundColor: 'rgba(168, 85, 247, 0.1)', fill: true, tension: 0.1, spanGaps: true }
            ]
          },
          options: {
            responsive: true,
            scales: {
              y: {
                beginAtZero: true,
                title: { display: true, text: 'Bytes' },
                ticks: { callback: function(value) { return formatBytes(value); } }
              }
            },
            plugins: {
              legend: { position: 'top' },
              tooltip: { callbacks: { label: function(context) { return context.dataset.label + ': ' + formatBytes(context.raw); } } }
            }
          }
        });
      }
    } catch (e) {
      console.error('Failed to load attachment bytes chart:', e);
    }

    try {
      const [msgSendResp, msgEditResp, msgDeleteResp] = await Promise.all([
        fetch(endpoint + '/query?metric=message.send').then(r => r.json()),
        fetch(endpoint + '/query?metric=message.edit').then(r => r.json()),
        fetch(endpoint + '/query?metric=message.delete').then(r => r.json())
      ]);

      const maTimestamps = Array.from(new Set([
        ...msgSendResp.data.map(d => d.timestamp),
        ...msgEditResp.data.map(d => d.timestamp),
        ...msgDeleteResp.data.map(d => d.timestamp),
      ])).sort((a, b) => a - b);

      if (maTimestamps.length > 0) {
        new Chart(document.getElementById('messageActivityChart'), {
          type: 'line',
          data: {
            labels: maTimestamps.map(formatTimeLabel),
            datasets: [
              { label: 'Messages Sent', data: alignData(msgSendResp.data, maTimestamps), borderColor: 'rgb(34, 197, 94)', tension: 0.1, spanGaps: true },
              { label: 'Messages Edited', data: alignData(msgEditResp.data, maTimestamps), borderColor: 'rgb(59, 130, 246)', tension: 0.1, spanGaps: true },
              { label: 'Messages Deleted', data: alignData(msgDeleteResp.data, maTimestamps), borderColor: 'rgb(239, 68, 68)', tension: 0.1, spanGaps: true }
            ]
          },
          options: {
            responsive: true,
            scales: { y: { beginAtZero: true, title: { display: true, text: 'Count' } } },
            plugins: { legend: { position: 'top' } }
          }
        });
      }
    } catch (e) {
      console.error('Failed to load message activity chart:', e);
    }

    try {
      const [reactionAddResp, reactionRemoveResp] = await Promise.all([
        fetch(endpoint + '/query?metric=reaction.add').then(r => r.json()),
        fetch(endpoint + '/query?metric=reaction.remove').then(r => r.json())
      ]);

      const rxTimestamps = Array.from(new Set([
        ...reactionAddResp.data.map(d => d.timestamp),
        ...reactionRemoveResp.data.map(d => d.timestamp),
      ])).sort((a, b) => a - b);

      if (rxTimestamps.length > 0) {
        new Chart(document.getElementById('reactionsChart'), {
          type: 'line',
          data: {
            labels: rxTimestamps.map(formatTimeLabel),
            datasets: [
              { label: 'Reactions Added', data: alignData(reactionAddResp.data, rxTimestamps), borderColor: 'rgb(34, 197, 94)', tension: 0.1, spanGaps: true },
              { label: 'Reactions Removed', data: alignData(reactionRemoveResp.data, rxTimestamps), borderColor: 'rgb(239, 68, 68)', tension: 0.1, spanGaps: true }
            ]
          },
          options: {
            responsive: true,
            scales: { y: { beginAtZero: true, title: { display: true, text: 'Count' } } },
            plugins: { legend: { position: 'top' } }
          }
        });
      }
    } catch (e) {
      console.error('Failed to load reactions chart:', e);
    }

    try {
      const latencyResp = await fetch(endpoint + '/query/percentiles?metric=api.latency').then(r => r.json());

      const p50El = document.getElementById('latency-p50');
      const p95El = document.getElementById('latency-p95');
      const p99El = document.getElementById('latency-p99');

      const percentiles = latencyResp.percentiles;
      if (percentiles) {
        if (p50El) p50El.textContent = formatMs(percentiles.p50);
        if (p95El) p95El.textContent = formatMs(percentiles.p95);
        if (p99El) p99El.textContent = formatMs(percentiles.p99);

        new Chart(document.getElementById('latencyChart'), {
          type: 'bar',
          data: {
            labels: ['P50', 'P95', 'P99'],
            datasets: [
              {
                label: 'Latency',
                data: [percentiles.p50, percentiles.p95, percentiles.p99],
                backgroundColor: ['rgb(34, 197, 94)', 'rgb(251, 146, 60)', 'rgb(239, 68, 68)']
              }
            ]
          },
          options: {
            responsive: true,
            scales: {
              y: {
                beginAtZero: true,
                title: { display: true, text: 'Latency (ms)' },
                ticks: { callback: function(value) { return value + ' ms'; } }
              }
            },
            plugins: {
              legend: { display: false },
              tooltip: { callbacks: { label: function(context) { return formatMs(context.raw); } } }
            }
          }
        });
      } else {
        if (p50El) p50El.textContent = '-';
        if (p95El) p95El.textContent = '-';
        if (p99El) p99El.textContent = '-';
      }
    } catch (e) {
      console.error('Failed to load latency chart:', e);
    }

    try {
      const [status2xxResp, status4xxResp, status5xxResp] = await Promise.all([
        fetch(endpoint + '/query?metric=api.request.2xx').then(r => r.json()),
        fetch(endpoint + '/query?metric=api.request.4xx').then(r => r.json()),
        fetch(endpoint + '/query?metric=api.request.5xx').then(r => r.json())
      ]);

      const scTimestamps = Array.from(new Set([
        ...status2xxResp.data.map(d => d.timestamp),
        ...status4xxResp.data.map(d => d.timestamp),
        ...status5xxResp.data.map(d => d.timestamp),
      ])).sort((a, b) => a - b);

      if (scTimestamps.length > 0) {
        new Chart(document.getElementById('statusCodesChart'), {
          type: 'line',
          data: {
            labels: scTimestamps.map(formatTimeLabel),
            datasets: [
              { label: '2xx (Success)', data: alignData(status2xxResp.data, scTimestamps), borderColor: 'rgb(34, 197, 94)', tension: 0.1, spanGaps: true },
              { label: '4xx (Client Error)', data: alignData(status4xxResp.data, scTimestamps), borderColor: 'rgb(251, 146, 60)', tension: 0.1, spanGaps: true },
              { label: '5xx (Server Error)', data: alignData(status5xxResp.data, scTimestamps), borderColor: 'rgb(239, 68, 68)', tension: 0.1, spanGaps: true }
            ]
          },
          options: {
            responsive: true,
            scales: { y: { beginAtZero: true, title: { display: true, text: 'Request Count' } } },
            plugins: { legend: { position: 'top' } }
          }
        });
      }
    } catch (e) {
      console.error('Failed to load status codes chart:', e);
    }
  })();
  "
}
