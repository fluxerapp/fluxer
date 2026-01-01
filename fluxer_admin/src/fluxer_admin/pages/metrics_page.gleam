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
import fluxer_admin/web.{type Context, type Session, href, prepend_base_path}
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
      "Metrics Overview",
      "metrics",
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
    ui.heading_page("Metrics Overview"),
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
  let registrations = metrics.query_aggregate(ctx, "user.registration")
  let messages = metrics.query_aggregate(ctx, "message.send")
  let message_deletes = metrics.query_aggregate(ctx, "message.delete")
  let guilds_created = metrics.query_aggregate(ctx, "guild.create")
  let gateway_ready = metrics.query_aggregate(ctx, "gateway.ready")
  let attachments = metrics.query_aggregate(ctx, "attachment.created")
  let attachment_storage =
    metrics.query_aggregate(ctx, "attachment.storage.bytes")
  let reports_created = metrics.query_aggregate(ctx, "reports.iar.created")
  let reports_resolved = metrics.query_aggregate(ctx, "reports.iar.resolved")
  let age_distribution =
    metrics.query_aggregate_grouped(ctx, "user.age", option.Some("age_group"))
  let registration_by_state =
    metrics.query_aggregate_grouped(
      ctx,
      "user.registration",
      option.Some("state"),
    )
  let registration_by_country =
    metrics.query_aggregate_grouped(
      ctx,
      "user.registration",
      option.Some("country"),
    )
  let top_guilds = metrics.query_top(ctx, "guild.member_count", 6)
  let top_users = metrics.query_top(ctx, "user.guild_membership_count", 6)
  let crashes = metrics.query_crashes(ctx, 5)

  let proxy_endpoint = prepend_base_path(ctx, "/api/metrics")

  h.div([], [
    ui.flex_row_between([
      ui.heading_page("Platform Overview"),
      h.div([a.class("flex gap-2")], [
        render_quick_link(ctx, "Gateway", "/gateway"),
        render_quick_link(ctx, "Jobs", "/jobs"),
        render_quick_link(ctx, "Messaging & API", "/messages-metrics"),
      ]),
    ]),
    render_platform_health_section(ctx, proxy_endpoint),
    render_key_metrics_section(proxy_endpoint),
    h.div([a.class("mt-8")], [
      h.h2([a.class("text-base font-semibold text-neutral-900 mb-4")], [
        element.text("Activity Highlights"),
      ]),
      h.div([a.class("grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4")], [
        render_stat_card("User Registrations", registrations),
        render_stat_card("Messages Sent", messages),
        render_stat_card("Message Deletes", message_deletes),
        render_stat_card("Guilds Created", guilds_created),
        render_stat_card("Gateway Connections", gateway_ready),
        render_stat_card("Attachments Uploaded", attachments),
      ]),
    ]),
    h.div([a.class("mt-6")], [
      h.div([a.class("grid grid-cols-1 md:grid-cols-2 gap-4")], [
        render_storage_card(attachment_storage),
        render_report_rate_card(reports_created, reports_resolved),
      ]),
    ]),
    render_recent_alerts_section(crashes),
    h.div([a.class("mt-8")], [
      h.h2([a.class("text-base font-semibold text-neutral-900 mb-4")], [
        element.text("Activity Over Time"),
      ]),
      h.div([a.class("bg-white border border-neutral-200 rounded-lg p-4")], [
        element.element(
          "canvas",
          [a.id("activityChart"), a.attribute("height", "250")],
          [],
        ),
      ]),
    ]),
    h.div([a.class("mt-8 grid grid-cols-1 lg:grid-cols-3 gap-4")], [
      h.div([a.class("bg-white/70 border border-neutral-200 rounded-lg p-4")], [
        h.h3([a.class("text-base font-semibold text-neutral-900 mb-2")], [
          element.text("Age Distribution"),
        ]),
        render_age_breakdown(age_distribution),
      ]),
      h.div([a.class("bg-white/70 border border-neutral-200 rounded-lg p-4")], [
        h.h3([a.class("text-base font-semibold text-neutral-900 mb-2")], [
          element.text("Guilds With Most Members"),
        ]),
        render_top_list(top_guilds),
      ]),
      h.div([a.class("bg-white/70 border border-neutral-200 rounded-lg p-4")], [
        h.h3([a.class("text-base font-semibold text-neutral-900 mb-2")], [
          element.text("Users In Most Guilds"),
        ]),
        render_top_list(top_users),
      ]),
    ]),
    h.div([a.class("mt-8 grid grid-cols-1 lg:grid-cols-2 gap-4")], [
      h.div([a.class("bg-white/70 border border-neutral-200 rounded-lg p-4")], [
        h.h3([a.class("text-base font-semibold text-neutral-900 mb-2")], [
          element.text("Registrations by State"),
        ]),
        render_registration_breakdown(registration_by_state),
      ]),
      h.div([a.class("bg-white/70 border border-neutral-200 rounded-lg p-4")], [
        h.h3([a.class("text-base font-semibold text-neutral-900 mb-2")], [
          element.text("Registrations by Country"),
        ]),
        render_registration_breakdown(registration_by_country),
      ]),
    ]),
    h.script([a.src("https://fluxerstatic.com/libs/chartjs/chart.min.js")], ""),
    h.script([], render_dashboard_script(proxy_endpoint)),
  ])
}

fn render_quick_link(ctx: Context, label: String, path: String) {
  h.a(
    [
      href(ctx, path),
      a.class(
        "px-3 py-1.5 text-sm font-medium text-neutral-700 hover:text-neutral-900 border border-neutral-300 rounded hover:border-neutral-400 transition-colors",
      ),
    ],
    [element.text(label)],
  )
}

fn render_platform_health_section(ctx: Context, proxy_endpoint: String) {
  h.div([a.class("mt-6")], [
    h.div([a.class("bg-white border border-neutral-200 rounded-lg shadow-sm")], [
      h.div([a.class("p-6")], [
        h.div([a.class("flex items-center justify-between mb-4")], [
          ui.heading_section("Platform Health"),
          h.a(
            [
              href(ctx, "/gateway"),
              a.class(
                "text-sm text-blue-600 hover:text-blue-800 hover:underline",
              ),
            ],
            [element.text("View Gateway Details")],
          ),
        ]),
        ui.text_small_muted(
          "Real-time platform status and key health indicators",
        ),
        h.div(
          [a.class("grid grid-cols-2 md:grid-cols-3 lg:grid-cols-5 gap-4 mt-4")],
          [
            render_health_stat_card("Active Sessions", "health-sessions"),
            render_health_stat_card("Guilds in Memory", "health-guilds"),
            render_health_stat_card("API Requests/min", "health-rpm"),
            render_health_stat_card("Error Rate", "health-error-rate"),
            render_health_stat_card("Pending Jobs", "health-pending-jobs"),
          ],
        ),
      ]),
    ]),
    h.script([], render_health_script(proxy_endpoint)),
  ])
}

fn render_health_stat_card(label: String, id: String) {
  h.div([a.class("bg-neutral-50 rounded-lg p-4 border border-neutral-200")], [
    h.div([a.class("text-xs text-neutral-600 uppercase tracking-wider mb-1")], [
      element.text(label),
    ]),
    h.div([a.id(id), a.class("text-base font-semibold text-neutral-900")], [
      element.text("-"),
    ]),
  ])
}

fn render_key_metrics_section(proxy_endpoint: String) {
  h.div([a.class("mt-6")], [
    h.div([a.class("bg-white border border-neutral-200 rounded-lg shadow-sm")], [
      h.div([a.class("p-6")], [
        ui.heading_section("Key Metrics At-a-Glance"),
        ui.text_small_muted("Trend indicators showing recent activity patterns"),
        h.div([a.class("grid grid-cols-2 md:grid-cols-4 gap-4 mt-4")], [
          render_trend_card(
            "User Registrations",
            "trend-registrations",
            "trend-registrations-indicator",
          ),
          render_trend_card(
            "Message Volume",
            "trend-messages",
            "trend-messages-indicator",
          ),
          render_trend_card(
            "Storage Used",
            "trend-storage",
            "trend-storage-indicator",
          ),
          render_trend_card(
            "API Latency (P95)",
            "trend-latency",
            "trend-latency-indicator",
          ),
        ]),
      ]),
    ]),
    h.script([], render_trends_script(proxy_endpoint)),
  ])
}

fn render_trend_card(label: String, value_id: String, indicator_id: String) {
  h.div([a.class("bg-neutral-50 rounded-lg p-4 border border-neutral-200")], [
    h.div([a.class("text-xs text-neutral-600 uppercase tracking-wider mb-1")], [
      element.text(label),
    ]),
    h.div([a.class("flex items-center gap-2")], [
      h.div(
        [a.id(value_id), a.class("text-base font-semibold text-neutral-900")],
        [element.text("-")],
      ),
      h.div([a.id(indicator_id), a.class("text-xs")], []),
    ]),
  ])
}

fn render_recent_alerts_section(
  crashes: Result(metrics.CrashesResponse, common.ApiError),
) {
  h.div([a.class("mt-8")], [
    h.div([a.class("bg-white border border-neutral-200 rounded-lg shadow-sm")], [
      h.div([a.class("p-6")], [
        ui.heading_section("Recent Alerts"),
        ui.text_small_muted("Recent guild crashes and system anomalies"),
        h.div([a.class("mt-4")], [
          case crashes {
            Ok(resp) -> render_alerts_list(resp.crashes)
            Error(_) ->
              h.div([a.class("text-neutral-500 text-sm")], [
                element.text("Unable to load alert data"),
              ])
          },
        ]),
      ]),
    ]),
  ])
}

fn render_alerts_list(crashes: List(metrics.CrashEvent)) {
  case list.length(crashes) {
    0 ->
      h.div([a.class("text-green-600 text-sm py-2")], [
        element.text("No recent alerts"),
      ])
    _ -> h.div([a.class("space-y-2")], list.map(crashes, render_alert_item))
  }
}

fn render_alert_item(crash: metrics.CrashEvent) {
  let time_str = format_timestamp(crash.timestamp)
  let error_preview =
    string.slice(crash.stacktrace, 0, 60)
    <> case string.length(crash.stacktrace) > 60 {
      True -> "..."
      False -> ""
    }

  h.div(
    [
      a.class(
        "flex items-center gap-3 p-3 bg-red-50 border border-red-100 rounded-lg",
      ),
    ],
    [
      h.div(
        [
          a.class("flex-shrink-0 w-2 h-2 rounded-full bg-red-500"),
        ],
        [],
      ),
      h.div([a.class("flex-1 min-w-0")], [
        h.div([a.class("flex items-center gap-2")], [
          h.span([a.class("text-sm font-medium text-red-900")], [
            element.text("Guild crash"),
          ]),
          h.span([a.class("text-xs text-red-600")], [element.text(time_str)]),
        ]),
        h.div([a.class("text-xs text-red-700 truncate font-mono mt-1")], [
          element.text(error_preview),
        ]),
      ]),
      h.div([a.class("flex-shrink-0 text-xs text-red-600 font-mono")], [
        element.text(crash.guild_id),
      ]),
    ],
  )
}

fn render_stat_card(
  label: String,
  result: Result(metrics.AggregateResponse, common.ApiError),
) {
  let value = case result {
    Ok(resp) -> format_number(resp.total)
    Error(_) -> "-"
  }

  h.div([a.class("bg-white border border-neutral-200 rounded-lg p-4")], [
    h.p([a.class("text-sm text-neutral-500 mb-1")], [element.text(label)]),
    h.p([a.class("text-2xl font-semibold text-neutral-900")], [
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

  h.div([a.class("bg-white border border-neutral-200 rounded-lg p-4")], [
    h.p([a.class("text-sm text-neutral-500 mb-1")], [
      element.text("Data Stored"),
    ]),
    h.p([a.class("text-2xl font-semibold text-neutral-900")], [
      element.text(value),
    ]),
  ])
}

fn render_report_rate_card(
  created: Result(metrics.AggregateResponse, common.ApiError),
  resolved: Result(metrics.AggregateResponse, common.ApiError),
) {
  let rate = case created, resolved {
    Ok(created_resp), Ok(resolved_resp) -> {
      let total_created = created_resp.total
      let total_resolved = resolved_resp.total
      let percentage = case total_created >. 0.0 {
        True -> {
          let ratio = total_resolved /. total_created
          ratio *. 100.0
        }
        False -> 0.0
      }
      format_percentage(percentage)
    }
    _, _ -> "-"
  }

  h.div([a.class("bg-white border border-neutral-200 rounded-lg p-4")], [
    h.p([a.class("text-sm text-neutral-500 mb-1")], [
      element.text("Reports Resolved"),
    ]),
    h.p([a.class("text-2xl font-semibold text-neutral-900")], [
      element.text(rate),
    ]),
  ])
}

fn render_age_breakdown(
  result: Result(metrics.AggregateResponse, common.ApiError),
) {
  case result {
    Ok(resp) ->
      case resp.breakdown {
        Some(breakdown) ->
          h.ul(
            [a.class("space-y-2 text-sm text-neutral-700")],
            list.map(breakdown, render_age_row),
          )
        None ->
          h.div([a.class("text-neutral-500 text-sm")], [
            element.text("No age data available"),
          ])
      }
    Error(_) ->
      h.div([a.class("text-neutral-500 text-sm")], [
        element.text("Unable to load age data"),
      ])
  }
}

fn render_registration_breakdown(
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

          h.div([a.class("max-h-64 overflow-y-auto")], [
            h.ul(
              [a.class("space-y-1 text-sm text-neutral-700")],
              list.take(sorted_breakdown, 20)
                |> list.map(render_breakdown_row),
            ),
          ])
        }
        None ->
          h.div([a.class("text-neutral-500 text-sm")], [
            element.text("No registration data available"),
          ])
      }
    Error(_) ->
      h.div([a.class("text-neutral-500 text-sm")], [
        element.text("Unable to load registration data"),
      ])
  }
}

fn render_age_row(entry: metrics.TopEntry) {
  h.li([], [
    h.span([a.class("font-medium text-neutral-900 block")], [
      element.text(entry.label),
    ]),
    h.span([a.class("text-xs text-neutral-500")], [
      element.text(format_number(entry.value)),
    ]),
  ])
}

fn render_breakdown_row(entry: metrics.TopEntry) {
  h.li(
    [
      a.class(
        "flex justify-between py-1 border-b border-neutral-100 last:border-0",
      ),
    ],
    [
      h.span([a.class("text-neutral-700")], [element.text(entry.label)]),
      h.span([a.class("font-semibold text-neutral-900")], [
        element.text(format_number(entry.value)),
      ]),
    ],
  )
}

fn render_top_list(result: Result(metrics.TopQueryResponse, common.ApiError)) {
  case result {
    Ok(resp) ->
      case list.length(resp.entries) {
        0 ->
          h.div([a.class("text-neutral-500 text-sm")], [
            element.text("No data available"),
          ])
        _ ->
          h.div(
            [a.class("overflow-hidden rounded-lg border border-neutral-100")],
            [
              h.ul(
                [a.class("divide-y divide-neutral-100")],
                list.map(resp.entries, render_top_entry),
              ),
            ],
          )
      }
    Error(_) ->
      h.div([a.class("text-neutral-500 text-sm")], [
        element.text("Unable to load ranking"),
      ])
  }
}

fn render_top_entry(entry: metrics.TopEntry) {
  h.li([a.class("flex justify-between px-3 py-2 text-sm")], [
    h.span([a.class("text-neutral-700")], [element.text(entry.label)]),
    h.span([a.class("font-semibold text-neutral-900")], [
      element.text(format_number(entry.value)),
    ]),
  ])
}

fn render_health_script(metrics_endpoint: String) -> String {
  "
  (async function() {
    const endpoint = '" <> metrics_endpoint <> "';
    if (!endpoint) return;

    const formatNumber = (n) => {
      if (n === null || n === undefined) return '-';
      return n.toLocaleString();
    };

    const getLatestValue = (data) => {
      if (!data || data.length === 0) return null;
      const sorted = [...data].sort((a, b) => b.timestamp - a.timestamp);
      return sorted[0]?.value ?? null;
    };

    try {
      const [sessionsResp, guildsResp, status2xxResp, status4xxResp, status5xxResp, pendingResp] = await Promise.all([
        fetch(endpoint + '/query?metric=gateway.sessions.count').then(r => r.json()),
        fetch(endpoint + '/query?metric=gateway.guilds.count').then(r => r.json()),
        fetch(endpoint + '/query?metric=api.request.2xx').then(r => r.json()),
        fetch(endpoint + '/query?metric=api.request.4xx').then(r => r.json()),
        fetch(endpoint + '/query?metric=api.request.5xx').then(r => r.json()),
        fetch(endpoint + '/query?metric=worker.queue.total_pending').then(r => r.json())
      ]);

      const sessions = getLatestValue(sessionsResp.data);
      const guilds = getLatestValue(guildsResp.data);
      const pending = getLatestValue(pendingResp.data);

      document.getElementById('health-sessions').textContent = formatNumber(sessions);
      document.getElementById('health-guilds').textContent = formatNumber(guilds);
      document.getElementById('health-pending-jobs').textContent = formatNumber(pending);

      const recent2xx = status2xxResp.data.slice(-10);
      const recent4xx = status4xxResp.data.slice(-10);
      const recent5xx = status5xxResp.data.slice(-10);

      const sum2xx = recent2xx.reduce((acc, d) => acc + d.value, 0);
      const sum4xx = recent4xx.reduce((acc, d) => acc + d.value, 0);
      const sum5xx = recent5xx.reduce((acc, d) => acc + d.value, 0);
      const total = sum2xx + sum4xx + sum5xx;

      const rpm = Math.round(total / 10);
      document.getElementById('health-rpm').textContent = formatNumber(rpm);

      const errorRate = total > 0 ? ((sum4xx + sum5xx) / total * 100).toFixed(1) : '0.0';
      const errorEl = document.getElementById('health-error-rate');
      errorEl.textContent = errorRate + '%';
      if (parseFloat(errorRate) > 5) {
        errorEl.classList.add('text-red-600');
      } else if (parseFloat(errorRate) > 1) {
        errorEl.classList.add('text-yellow-600');
      } else {
        errorEl.classList.add('text-green-600');
      }
    } catch (e) {
      console.error('Failed to load health stats:', e);
    }
  })();
  "
}

fn render_trends_script(metrics_endpoint: String) -> String {
  "
  (async function() {
    const endpoint = '" <> metrics_endpoint <> "';
    if (!endpoint) return;

    const formatNumber = (n) => {
      if (n === null || n === undefined) return '-';
      return n.toLocaleString();
    };

    const formatBytes = (bytes) => {
      if (bytes === 0) return '0 B';
      const k = 1024;
      const sizes = ['B', 'KB', 'MB', 'GB', 'TB'];
      const i = Math.floor(Math.log(bytes) / Math.log(k));
      return parseFloat((bytes / Math.pow(k, i)).toFixed(2)) + ' ' + sizes[i];
    };

    const getTrend = (data, count) => {
      if (!data || data.length < 2) return { current: null, trend: 'stable' };
      const sorted = [...data].sort((a, b) => b.timestamp - a.timestamp);
      const recent = sorted.slice(0, Math.min(count, sorted.length));
      const older = sorted.slice(count, Math.min(count * 2, sorted.length));

      const recentSum = recent.reduce((acc, d) => acc + d.value, 0);
      const olderSum = older.length > 0 ? older.reduce((acc, d) => acc + d.value, 0) : recentSum;

      let trend = 'stable';
      if (olderSum > 0) {
        const change = (recentSum - olderSum) / olderSum;
        if (change > 0.1) trend = 'up';
        else if (change < -0.1) trend = 'down';
      }

      return { current: recentSum, trend };
    };

    const renderTrend = (indicator, trend, isGood) => {
      if (trend === 'up') {
        indicator.textContent = isGood ? '↑' : '↑';
        indicator.className = 'text-xs ' + (isGood ? 'text-green-600' : 'text-red-600');
      } else if (trend === 'down') {
        indicator.textContent = isGood ? '↓' : '↓';
        indicator.className = 'text-xs ' + (isGood ? 'text-red-600' : 'text-green-600');
      } else {
        indicator.textContent = '→';
        indicator.className = 'text-xs text-neutral-400';
      }
    };

    try {
      const [regResp, msgResp, storageResp, latencyResp] = await Promise.all([
        fetch(endpoint + '/query?metric=user.registration').then(r => r.json()),
        fetch(endpoint + '/query?metric=message.send').then(r => r.json()),
        fetch(endpoint + '/query?metric=attachment.storage.bytes').then(r => r.json()),
        fetch(endpoint + '/query/percentiles?metric=api.latency').then(r => r.json())
      ]);

      const regTrend = getTrend(regResp.data, 5);
      document.getElementById('trend-registrations').textContent = formatNumber(regTrend.current);
      renderTrend(document.getElementById('trend-registrations-indicator'), regTrend.trend, true);

      const msgTrend = getTrend(msgResp.data, 5);
      document.getElementById('trend-messages').textContent = formatNumber(msgTrend.current);
      renderTrend(document.getElementById('trend-messages-indicator'), msgTrend.trend, true);

      const storageSorted = [...storageResp.data].sort((a, b) => b.timestamp - a.timestamp);
      const currentStorage = storageSorted[0]?.value ?? 0;
      document.getElementById('trend-storage').textContent = formatBytes(currentStorage);
      renderTrend(document.getElementById('trend-storage-indicator'), 'stable', true);

      const currentLatency = latencyResp.percentiles?.p95 ?? 0;
      document.getElementById('trend-latency').textContent = currentLatency.toFixed(1) + ' ms';

      const latencyTrend = currentLatency > 0 ? 'stable' : 'stable';
      renderTrend(document.getElementById('trend-latency-indicator'), latencyTrend, false);
    } catch (e) {
      console.error('Failed to load trend stats:', e);
    }
  })();
  "
}

fn render_dashboard_script(metrics_endpoint: String) -> String {
  "
  (async function() {
    const endpoint = '" <> metrics_endpoint <> "';
    if (!endpoint) return;

    try {
      const [regResp, msgResp, delResp, attachResp] = await Promise.all([
        fetch(endpoint + '/query?metric=user.registration').then(r => r.json()),
        fetch(endpoint + '/query?metric=message.send').then(r => r.json()),
        fetch(endpoint + '/query?metric=message.delete').then(r => r.json()),
        fetch(endpoint + '/query?metric=attachment.created').then(r => r.json())
      ]);

      const timestamps = Array.from(new Set([
        ...regResp.data.map(d => d.timestamp),
        ...msgResp.data.map(d => d.timestamp),
        ...delResp.data.map(d => d.timestamp),
        ...attachResp.data.map(d => d.timestamp),
      ])).sort((a, b) => a - b);

      const labels = timestamps.map(ts => new Date(ts).toLocaleDateString());

      const alignData = (data) => {
        const map = new Map(data.map(d => [d.timestamp, d.value]));
        return timestamps.map(ts => map.get(ts) ?? 0);
      };

      new Chart(document.getElementById('activityChart'), {
        type: 'line',
        data: {
          labels,
          datasets: [
            {
              label: 'Registrations',
              data: alignData(regResp.data),
              borderColor: 'rgb(59, 130, 246)',
              tension: 0.1
            },
            {
              label: 'Messages Sent',
              data: alignData(msgResp.data),
              borderColor: 'rgb(34, 197, 94)',
              tension: 0.1
            },
            {
              label: 'Messages Deleted',
              data: alignData(delResp.data),
              borderColor: 'rgb(239, 68, 68)',
              tension: 0.1
            },
            {
              label: 'Attachments Created',
              data: alignData(attachResp.data),
              borderColor: 'rgb(168, 85, 247)',
              tension: 0.1
            }
          ]
        },
        options: {
          responsive: true,
          scales: {
            y: { beginAtZero: true }
          }
        }
      });
    } catch (e) {
      console.error('Failed to load chart data:', e);
    }
  })();
  "
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

fn format_timestamp(ts: Int) -> String {
  let secs = ts / 1000
  let mins = secs / 60
  let hours = mins / 60
  let days = hours / 24

  case days {
    0 -> int.to_string(hours) <> "h ago"
    1 -> "1 day ago"
    _ -> int.to_string(days) <> " days ago"
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

fn format_percentage(value: Float) -> String {
  int.to_string(float.truncate(value)) <> "%"
}
