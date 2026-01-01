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
import gleam/order
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
      "Storage & Infrastructure",
      "storage",
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
    ui.heading_page("Storage & Infrastructure"),
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
  let attachment_storage =
    metrics.query_aggregate(ctx, "attachment.storage.bytes")
  let attachments_created = metrics.query_aggregate(ctx, "attachment.created")
  let attachments_expired = metrics.query_aggregate(ctx, "attachment.expired")
  let content_type_breakdown =
    metrics.query_aggregate_grouped(
      ctx,
      "attachment.created",
      option.Some("content_type"),
    )

  let proxy_endpoint = prepend_base_path(ctx, "/api/metrics")

  h.div([], [
    ui.heading_page("Storage & Infrastructure"),
    h.div([a.class("mt-6")], [
      h.div(
        [a.class("bg-white border border-neutral-200 rounded-lg shadow-sm")],
        [
          h.div([a.class("p-6")], [
            ui.heading_section("S3/Storage Metrics"),
            ui.text_small_muted("Storage usage and attachment activity"),
            h.div([a.class("grid grid-cols-2 md:grid-cols-4 gap-4 mt-4")], [
              render_storage_card(attachment_storage),
              render_stat_card("Attachments Created", attachments_created),
              render_stat_card("Attachments Expired", attachments_expired),
              render_loading_stat_card(
                "Storage Growth (24h)",
                "storage-growth-24h",
              ),
            ]),
          ]),
        ],
      ),
    ]),
    h.div([a.class("mt-6 grid grid-cols-1 lg:grid-cols-2 gap-6")], [
      h.div(
        [a.class("bg-white border border-neutral-200 rounded-lg shadow-sm")],
        [
          h.div([a.class("p-6")], [
            ui.heading_section("Storage by Content Type"),
            ui.text_small_muted("Breakdown of attachments by MIME type"),
            h.div([a.class("mt-4 max-h-64 overflow-y-auto")], [
              render_content_type_breakdown(content_type_breakdown),
            ]),
          ]),
        ],
      ),
      h.div(
        [a.class("bg-white border border-neutral-200 rounded-lg shadow-sm")],
        [
          h.div([a.class("p-6")], [
            ui.heading_section("Storage Over Time"),
            ui.text_small_muted("Cumulative storage bytes over time"),
            h.div([a.class("mt-4")], [
              element.element(
                "canvas",
                [a.id("storageChart"), a.attribute("height", "200")],
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
            ui.heading_section("CDN & Queue Metrics"),
            ui.text_small_muted(
              "Cloudflare purge queue and asset deletion queue sizes",
            ),
            h.div([a.class("grid grid-cols-2 md:grid-cols-4 gap-4 mt-4")], [
              render_loading_stat_card(
                "Cloudflare Purge Queue",
                "redis-cloudflare-purge",
              ),
              render_loading_stat_card(
                "Asset Deletion Queue",
                "redis-asset-deletion",
              ),
              render_loading_stat_card(
                "Bulk Message Deletion",
                "redis-bulk-message-deletion",
              ),
              render_loading_stat_card(
                "Account Deletion Queue",
                "redis-account-deletion",
              ),
            ]),
          ]),
        ],
      ),
    ]),
    h.div([a.class("mt-6 grid grid-cols-1 lg:grid-cols-2 gap-6")], [
      h.div(
        [a.class("bg-white border border-neutral-200 rounded-lg shadow-sm")],
        [
          h.div([a.class("p-6")], [
            ui.heading_section("Redis Queue Depths Over Time"),
            ui.text_small_muted(
              "CDN purge and asset deletion queue sizes over time",
            ),
            h.div([a.class("mt-4")], [
              element.element(
                "canvas",
                [a.id("redisQueueChart"), a.attribute("height", "200")],
                [],
              ),
            ]),
          ]),
        ],
      ),
      h.div(
        [a.class("bg-white border border-neutral-200 rounded-lg shadow-sm")],
        [
          h.div([a.class("p-6")], [
            ui.heading_section("Upload Activity Over Time"),
            ui.text_small_muted(
              "Attachment creation and expiry rates over time",
            ),
            h.div([a.class("mt-4")], [
              element.element(
                "canvas",
                [a.id("uploadActivityChart"), a.attribute("height", "200")],
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
            ui.heading_section("Worker Job Queue Status"),
            ui.text_small_muted("Graphile worker job queue overview"),
            h.div([a.class("grid grid-cols-1 md:grid-cols-3 gap-4 mt-4")], [
              render_loading_stat_card("Total Pending", "worker-pending"),
              render_loading_stat_card("Total Running", "worker-running"),
              render_loading_stat_card("Total Failed", "worker-failed"),
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
            ui.heading_section("API Performance"),
            ui.text_small_muted("API latency and error rates"),
            h.div([a.class("grid grid-cols-1 md:grid-cols-4 gap-4 mt-4")], [
              render_loading_stat_card("P50 Latency", "api-p50"),
              render_loading_stat_card("P95 Latency", "api-p95"),
              render_loading_stat_card("P99 Latency", "api-p99"),
              render_loading_stat_card("5xx Errors (24h)", "api-5xx"),
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
      element.text("Total Storage Used"),
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

fn render_content_type_breakdown(
  result: Result(metrics.AggregateResponse, common.ApiError),
) {
  case result {
    Ok(resp) ->
      case resp.breakdown {
        Some(breakdown) -> {
          let sorted_breakdown =
            breakdown
            |> list.sort(fn(a, b) {
              case b.value, a.value {
                b_val, a_val ->
                  case float.compare(b_val, a_val) {
                    order.Gt -> order.Lt
                    order.Lt -> order.Gt
                    order.Eq -> order.Eq
                  }
              }
            })

          h.ul(
            [a.class("space-y-1 text-sm text-neutral-700")],
            list.take(sorted_breakdown, 15)
              |> list.map(render_breakdown_row),
          )
        }
        None ->
          h.div([a.class("text-neutral-500 text-sm")], [
            element.text("No content type data available"),
          ])
      }
    Error(_) ->
      h.div([a.class("text-neutral-500 text-sm")], [
        element.text("Unable to load content type data"),
      ])
  }
}

fn render_breakdown_row(entry: metrics.TopEntry) {
  h.li(
    [
      a.class(
        "flex justify-between py-1 border-b border-neutral-100 last:border-0",
      ),
    ],
    [
      h.span([a.class("text-neutral-700 font-mono text-xs")], [
        element.text(entry.label),
      ]),
      h.span([a.class("font-semibold text-neutral-900")], [
        element.text(format_number(entry.value)),
      ]),
    ],
  )
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
    _ if bytes <. 1_099_511_627_776.0 -> {
      let gb = bytes /. 1_073_741_824.0
      float_to_string_rounded(gb, 2) <> " GB"
    }
    _ -> {
      let tb = bytes /. 1_099_511_627_776.0
      float_to_string_rounded(tb, 2) <> " TB"
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

    const formatNumber = (n) => {
      if (n === null || n === undefined) return '-';
      return n.toLocaleString();
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

    const get24hGrowth = (data) => {
      if (!data || data.length < 2) return null;
      const sorted = [...data].sort((a, b) => a.timestamp - b.timestamp);
      const now = Date.now();
      const oneDayAgo = now - 24 * 60 * 60 * 1000;
      const recentPoints = sorted.filter(d => d.timestamp >= oneDayAgo);
      if (recentPoints.length < 2) return null;
      const first = recentPoints[0].value;
      const last = recentPoints[recentPoints.length - 1].value;
      return last - first;
    };

    try {
      const [assetResp, cloudflareResp, bulkMsgResp, accountResp] = await Promise.all([
        fetch(endpoint + '/query?metric=worker.redis_queue.asset_deletion').then(r => r.json()),
        fetch(endpoint + '/query?metric=worker.redis_queue.cloudflare_purge').then(r => r.json()),
        fetch(endpoint + '/query?metric=worker.redis_queue.bulk_message_deletion').then(r => r.json()),
        fetch(endpoint + '/query?metric=worker.redis_queue.account_deletion').then(r => r.json())
      ]);

      document.getElementById('redis-asset-deletion').textContent = formatNumber(getLatestValue(assetResp.data));
      document.getElementById('redis-cloudflare-purge').textContent = formatNumber(getLatestValue(cloudflareResp.data));
      document.getElementById('redis-bulk-message-deletion').textContent = formatNumber(getLatestValue(bulkMsgResp.data));
      document.getElementById('redis-account-deletion').textContent = formatNumber(getLatestValue(accountResp.data));

      const rqTimestamps = Array.from(new Set([
        ...assetResp.data.map(d => d.timestamp),
        ...cloudflareResp.data.map(d => d.timestamp),
      ])).sort((a, b) => a - b);

      if (rqTimestamps.length > 0) {
        new Chart(document.getElementById('redisQueueChart'), {
          type: 'line',
          data: {
            labels: rqTimestamps.map(formatTimeLabel),
            datasets: [
              { label: 'Cloudflare Purge', data: alignData(cloudflareResp.data, rqTimestamps), borderColor: 'rgb(251, 146, 60)', tension: 0.1, spanGaps: true },
              { label: 'Asset Deletion', data: alignData(assetResp.data, rqTimestamps), borderColor: 'rgb(239, 68, 68)', tension: 0.1, spanGaps: true }
            ]
          },
          options: {
            responsive: true,
            scales: { y: { beginAtZero: true, title: { display: true, text: 'Queue Size' } } },
            plugins: { legend: { position: 'top' } }
          }
        });
      }
    } catch (e) {
      console.error('Failed to load Redis queue stats:', e);
    }

    try {
      const [pendingResp, runningResp, failedResp] = await Promise.all([
        fetch(endpoint + '/query?metric=worker.queue.total_pending').then(r => r.json()),
        fetch(endpoint + '/query?metric=worker.queue.total_running').then(r => r.json()),
        fetch(endpoint + '/query?metric=worker.queue.total_failed').then(r => r.json())
      ]);

      document.getElementById('worker-pending').textContent = formatNumber(getLatestValue(pendingResp.data));
      document.getElementById('worker-running').textContent = formatNumber(getLatestValue(runningResp.data));
      document.getElementById('worker-failed').textContent = formatNumber(getLatestValue(failedResp.data));
    } catch (e) {
      console.error('Failed to load worker stats:', e);
    }

    try {
      const [latencyResp, status5xxResp] = await Promise.all([
        fetch(endpoint + '/query/percentiles?metric=api.latency').then(r => r.json()),
        fetch(endpoint + '/query?metric=api.request.5xx').then(r => r.json())
      ]);

      const percentiles = latencyResp.percentiles;
      document.getElementById('api-p50').textContent = percentiles ? formatMs(percentiles.p50) : '-';
      document.getElementById('api-p95').textContent = percentiles ? formatMs(percentiles.p95) : '-';
      document.getElementById('api-p99').textContent = percentiles ? formatMs(percentiles.p99) : '-';

      const now = Date.now();
      const oneDayAgo = now - 24 * 60 * 60 * 1000;
      const recent5xx = status5xxResp.data.filter(d => d.timestamp >= oneDayAgo);
      const total5xx = recent5xx.reduce((sum, d) => sum + d.value, 0);
      document.getElementById('api-5xx').textContent = formatNumber(total5xx);
    } catch (e) {
      console.error('Failed to load API stats:', e);
    }

    try {
      const storageResp = await fetch(endpoint + '/query?metric=attachment.storage.bytes').then(r => r.json());

      const growth = get24hGrowth(storageResp.data);
      const growthEl = document.getElementById('storage-growth-24h');
      if (growthEl) {
        if (growth !== null) {
          const prefix = growth >= 0 ? '+' : '';
          growthEl.textContent = prefix + formatBytes(Math.abs(growth));
          growthEl.classList.add(growth >= 0 ? 'text-green-600' : 'text-red-600');
        } else {
          growthEl.textContent = '-';
        }
      }

      const stTimestamps = storageResp.data.map(d => d.timestamp).sort((a, b) => a - b);

      if (stTimestamps.length > 0) {
        new Chart(document.getElementById('storageChart'), {
          type: 'line',
          data: {
            labels: stTimestamps.map(formatTimeLabel),
            datasets: [
              { label: 'Storage Bytes', data: alignData(storageResp.data, stTimestamps), borderColor: 'rgb(168, 85, 247)', backgroundColor: 'rgba(168, 85, 247, 0.1)', fill: true, tension: 0.1, spanGaps: true }
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
      console.error('Failed to load storage chart:', e);
    }

    try {
      const [createdResp, expiredResp] = await Promise.all([
        fetch(endpoint + '/query?metric=attachment.created').then(r => r.json()),
        fetch(endpoint + '/query?metric=attachment.expired').then(r => r.json())
      ]);

      const uaTimestamps = Array.from(new Set([
        ...createdResp.data.map(d => d.timestamp),
        ...expiredResp.data.map(d => d.timestamp),
      ])).sort((a, b) => a - b);

      if (uaTimestamps.length > 0) {
        new Chart(document.getElementById('uploadActivityChart'), {
          type: 'line',
          data: {
            labels: uaTimestamps.map(formatTimeLabel),
            datasets: [
              { label: 'Created', data: alignData(createdResp.data, uaTimestamps), borderColor: 'rgb(34, 197, 94)', tension: 0.1, spanGaps: true },
              { label: 'Expired', data: alignData(expiredResp.data, uaTimestamps), borderColor: 'rgb(239, 68, 68)', tension: 0.1, spanGaps: true }
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
      console.error('Failed to load upload activity chart:', e);
    }
  })();
  "
}
