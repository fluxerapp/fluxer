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
import fluxer_admin/api/system
import fluxer_admin/avatar
import fluxer_admin/components/flash
import fluxer_admin/components/layout
import fluxer_admin/components/ui
import fluxer_admin/web.{
  type Context, type Session, action, href, prepend_base_path,
}
import gleam/float
import gleam/int
import gleam/list
import gleam/option
import gleam/string
import lustre/attribute as a
import lustre/element
import lustre/element/html as h
import wisp.{type Request, type Response}

pub fn view(
  _req: Request,
  ctx: Context,
  session: Session,
  current_admin: option.Option(common.UserLookupResult),
  flash_data: option.Option(flash.Flash),
  admin_acls: List(String),
  result: option.Option(Int),
) -> Response {
  let node_stats_result = system.get_node_stats(ctx, session)
  let guild_stats_result = system.get_guild_memory_stats(ctx, session, 100)

  let content = case node_stats_result, guild_stats_result {
    Ok(node_stats), Ok(guild_stats) ->
      render_success(
        ctx,
        admin_acls,
        option.Some(node_stats),
        guild_stats.guilds,
        result,
      )
    _, Ok(guild_stats) ->
      render_success(ctx, admin_acls, option.None, guild_stats.guilds, result)
    _, Error(common.Unauthorized) -> render_error(ctx, "Unauthorized")
    _, Error(common.Forbidden(message)) -> render_error(ctx, message)
    _, Error(common.NotFound) -> render_error(ctx, "Not found")
    _, Error(common.ServerError) -> render_error(ctx, "Server error")
    _, Error(common.NetworkError) -> render_error(ctx, "Network error")
  }

  let html =
    layout.page(
      "Gateway",
      "gateway",
      ctx,
      session,
      current_admin,
      flash_data,
      content,
    )
  wisp.html_response(element.to_document_string(html), 200)
}

pub fn handle_action(
  req: Request,
  ctx: Context,
  session: Session,
  current_admin: option.Option(common.UserLookupResult),
  admin_acls: List(String),
) -> Response {
  let flash_data = flash.from_request(req)

  let result = case system.reload_all_guilds(ctx, session, []) {
    Ok(count) -> option.Some(count)
    Error(_) -> option.None
  }

  view(req, ctx, session, current_admin, flash_data, admin_acls, result)
}

fn render_error(_ctx: Context, message: String) {
  ui.stack("6", [
    ui.heading_page("Gateway"),
    h.div(
      [a.class("bg-red-50 border border-red-200 rounded-lg p-6 text-center")],
      [h.p([a.class("text-red-800")], [element.text(message)])],
    ),
  ])
}

fn render_success(
  ctx: Context,
  admin_acls: List(String),
  node_stats: option.Option(system.NodeStats),
  guilds: List(system.ProcessMemoryStats),
  result: option.Option(Int),
) {
  let can_reload_all =
    list.contains(admin_acls, "gateway:reload_all")
    || list.contains(admin_acls, "*")

  h.div([], [
    ui.flex_row_between([
      ui.heading_page("Gateway"),
      case can_reload_all {
        True ->
          h.form([a.method("POST"), action(ctx, "/gateway?action=reload_all")], [
            ui.button_primary("Reload All Guilds", "submit", [
              a.attribute(
                "onclick",
                "return confirm('Are you sure you want to reload all guilds in memory? This may take several minutes.');",
              ),
            ]),
          ])
        False -> element.none()
      },
    ]),
    case result {
      option.Some(count) ->
        h.div(
          [
            a.class(
              "mb-6 bg-green-50 border border-green-200 rounded-lg p-4 text-green-800",
            ),
          ],
          [
            element.text(
              "Successfully reloaded " <> int.to_string(count) <> " guilds!",
            ),
          ],
        )
      option.None -> element.none()
    },
    case node_stats {
      option.Some(stats) -> render_node_stats(ctx, stats)
      option.None -> element.none()
    },
    h.div([a.class("bg-white border border-neutral-200 rounded-lg shadow-sm")], [
      h.div([a.class("p-6 border-b border-neutral-200")], [
        ui.heading_section("Guild Memory Leaderboard (Top 100)"),
        ui.text_small_muted(
          "Guilds ranked by memory usage, showing the top 100 consumers",
        ),
      ]),
      render_guild_table(ctx, guilds),
    ]),
  ])
}

fn render_guild_table(ctx: Context, guilds: List(system.ProcessMemoryStats)) {
  case list.is_empty(guilds) {
    True ->
      h.div([a.class("p-6 text-center text-neutral-600")], [
        element.text("No guilds in memory"),
      ])
    False ->
      h.div([a.class("overflow-x-auto")], [
        h.table([a.class("w-full")], [
          h.thead([a.class("bg-neutral-50 border-b border-neutral-200")], [
            h.tr([], [
              h.th(
                [
                  a.class(
                    "px-6 py-3 text-left text-xs text-neutral-600 uppercase tracking-wider",
                  ),
                ],
                [element.text("Rank")],
              ),
              h.th(
                [
                  a.class(
                    "px-6 py-3 text-left text-xs text-neutral-600 uppercase tracking-wider",
                  ),
                ],
                [element.text("Guild")],
              ),
              h.th(
                [
                  a.class(
                    "px-6 py-3 text-right text-xs text-neutral-600 uppercase tracking-wider",
                  ),
                ],
                [element.text("RAM Usage")],
              ),
              h.th(
                [
                  a.class(
                    "px-6 py-3 text-right text-xs text-neutral-600 uppercase tracking-wider",
                  ),
                ],
                [element.text("Members")],
              ),
              h.th(
                [
                  a.class(
                    "px-6 py-3 text-right text-xs text-neutral-600 uppercase tracking-wider",
                  ),
                ],
                [element.text("Sessions")],
              ),
              h.th(
                [
                  a.class(
                    "px-6 py-3 text-right text-xs text-neutral-600 uppercase tracking-wider",
                  ),
                ],
                [element.text("Presences")],
              ),
            ]),
          ]),
          h.tbody(
            [a.class("divide-y divide-neutral-200")],
            list.index_map(guilds, fn(guild, index) {
              render_guild_row(ctx, guild, index)
            }),
          ),
        ]),
      ])
  }
}

fn render_guild_row(ctx: Context, guild: system.ProcessMemoryStats, index: Int) {
  let rank = index + 1

  h.tr([a.class("hover:bg-neutral-50 transition-colors")], [
    h.td(
      [
        a.class(
          "px-6 py-4 whitespace-nowrap text-sm font-medium text-neutral-900",
        ),
      ],
      [element.text("#" <> int.to_string(rank))],
    ),
    h.td([a.class("px-6 py-4 whitespace-nowrap")], [
      case guild.guild_id {
        option.Some(guild_id) ->
          h.a(
            [
              href(ctx, "/guilds/" <> guild_id),
              a.class("flex items-center gap-2"),
            ],
            [
              case
                avatar.get_guild_icon_url(
                  ctx.media_endpoint,
                  guild_id,
                  guild.guild_icon,
                  True,
                )
              {
                option.Some(icon_url) ->
                  h.img([
                    a.src(icon_url),
                    a.alt(guild.guild_name),
                    a.class("w-10 h-10 rounded-full"),
                  ])
                option.None ->
                  h.div(
                    [
                      a.class(
                        "w-10 h-10 rounded-full bg-neutral-200 flex items-center justify-center text-sm font-medium text-neutral-600",
                      ),
                    ],
                    [
                      element.text(
                        guild.guild_name
                        |> get_first_char,
                      ),
                    ],
                  )
              },
              h.div([], [
                h.div([a.class("text-sm font-medium text-neutral-900")], [
                  element.text(guild.guild_name),
                ]),
                h.div([a.class("text-xs text-neutral-500")], [
                  element.text(guild_id),
                ]),
              ]),
            ],
          )
        option.None ->
          h.div([a.class("flex items-center gap-2")], [
            h.div(
              [
                a.class(
                  "w-10 h-10 rounded-full bg-neutral-200 flex items-center justify-center text-sm font-medium text-neutral-600",
                ),
              ],
              [element.text("?")],
            ),
            h.span([a.class("text-sm text-neutral-600")], [
              element.text(guild.guild_name),
            ]),
          ])
      },
    ]),
    h.td(
      [
        a.class(
          "px-6 py-4 whitespace-nowrap text-sm text-neutral-900 text-right text-sm font-medium",
        ),
      ],
      [element.text(format_memory(guild.memory_mb))],
    ),
    h.td(
      [
        a.class(
          "px-6 py-4 whitespace-nowrap text-sm text-neutral-900 text-right",
        ),
      ],
      [element.text(format_number(guild.member_count))],
    ),
    h.td(
      [
        a.class(
          "px-6 py-4 whitespace-nowrap text-sm text-neutral-900 text-right",
        ),
      ],
      [element.text(format_number(guild.session_count))],
    ),
    h.td(
      [
        a.class(
          "px-6 py-4 whitespace-nowrap text-sm text-neutral-900 text-right",
        ),
      ],
      [element.text(format_number(guild.presence_count))],
    ),
  ])
}

fn get_first_char(s: String) -> String {
  case s {
    "" -> "?"
    _ -> {
      let assert Ok(first) = s |> string.first
      first
    }
  }
}

fn format_number(n: Int) -> String {
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

fn format_memory(memory_mb: Float) -> String {
  case memory_mb {
    _ if memory_mb <. 1.0 -> {
      let kb = memory_mb *. 1024.0
      float_to_string_rounded(kb, 2) <> " KB"
    }
    _ if memory_mb <. 1024.0 -> {
      float_to_string_rounded(memory_mb, 2) <> " MB"
    }
    _ -> {
      let gb = memory_mb /. 1024.0
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

fn render_node_stats(ctx: Context, stats: system.NodeStats) {
  h.div([], [
    h.div(
      [a.class("bg-white border border-neutral-200 rounded-lg shadow-sm mb-6")],
      [
        h.div([a.class("p-6")], [
          ui.heading_section("Gateway Statistics"),
          h.div(
            [
              a.class(
                "grid grid-cols-2 md:grid-cols-3 lg:grid-cols-5 gap-4 mt-4",
              ),
            ],
            [
              render_stat_card(ctx, "Sessions", format_number(stats.sessions)),
              render_stat_card(ctx, "Guilds", format_number(stats.guilds)),
              render_stat_card(ctx, "Presences", format_number(stats.presences)),
              render_stat_card(ctx, "Calls", format_number(stats.calls)),
              render_stat_card(
                ctx,
                "Total RAM",
                format_memory(int.to_float(stats.memory_total) /. 1_024_000.0),
              ),
            ],
          ),
        ]),
      ],
    ),
    render_gateway_charts(ctx),
  ])
}

fn render_gateway_charts(ctx: Context) {
  case ctx.metrics_endpoint {
    option.Some(_) -> render_gateway_charts_content(ctx)
    option.None -> element.none()
  }
}

fn render_gateway_charts_content(ctx: Context) {
  let proxy_endpoint = prepend_base_path(ctx, "/api/metrics")

  h.div([a.class("space-y-6 mb-6")], [
    h.div([a.class("bg-white border border-neutral-200 rounded-lg shadow-sm")], [
      h.div([a.class("p-6")], [
        ui.heading_section("Process Counts Over Time"),
        ui.text_small_muted(
          "Historical view of active sessions, guilds, presences, and calls",
        ),
        h.div([a.class("mt-4")], [
          element.element(
            "canvas",
            [a.id("processCountsChart"), a.attribute("height", "250")],
            [],
          ),
        ]),
      ]),
    ]),
    h.div([a.class("bg-white border border-neutral-200 rounded-lg shadow-sm")], [
      h.div([a.class("p-6")], [
        ui.heading_section("WebSocket Connection Activity"),
        ui.text_small_muted(
          "Connection and disconnection rates per reporting interval",
        ),
        h.div([a.class("mt-4")], [
          element.element(
            "canvas",
            [a.id("wsConnectionChart"), a.attribute("height", "250")],
            [],
          ),
        ]),
      ]),
    ]),
    h.div([a.class("bg-white border border-neutral-200 rounded-lg shadow-sm")], [
      h.div([a.class("p-6")], [
        ui.heading_section("Heartbeat Health"),
        ui.text_small_muted(
          "Heartbeat success and failure counts per reporting interval",
        ),
        h.div([a.class("mt-4")], [
          element.element(
            "canvas",
            [a.id("heartbeatChart"), a.attribute("height", "250")],
            [],
          ),
        ]),
      ]),
    ]),
    h.div([a.class("bg-white border border-neutral-200 rounded-lg shadow-sm")], [
      h.div([a.class("p-6")], [
        ui.heading_section("Session Resume Activity"),
        ui.text_small_muted(
          "Resume success and failure counts per reporting interval",
        ),
        h.div([a.class("mt-4")], [
          element.element(
            "canvas",
            [a.id("resumeChart"), a.attribute("height", "250")],
            [],
          ),
        ]),
      ]),
    ]),
    h.div([a.class("bg-white border border-neutral-200 rounded-lg shadow-sm")], [
      h.div([a.class("p-6")], [
        ui.heading_section("Rate Limiting Events"),
        ui.text_small_muted(
          "Identify rate limiting triggers per reporting interval",
        ),
        h.div([a.class("mt-4")], [
          element.element(
            "canvas",
            [a.id("rateLimitChart"), a.attribute("height", "250")],
            [],
          ),
        ]),
      ]),
    ]),
    h.div([a.class("bg-white border border-neutral-200 rounded-lg shadow-sm")], [
      h.div([a.class("p-6")], [
        ui.heading_section("RPC Latency"),
        ui.text_small_muted(
          "API RPC call latency percentiles (p50, p95, p99) in milliseconds",
        ),
        h.div([a.class("mt-4")], [
          element.element(
            "canvas",
            [a.id("rpcLatencyChart"), a.attribute("height", "250")],
            [],
          ),
        ]),
      ]),
    ]),
    h.div([a.class("bg-white border border-neutral-200 rounded-lg shadow-sm")], [
      h.div([a.class("p-6")], [
        ui.heading_section("Mailbox Sizes Over Time"),
        ui.text_small_muted(
          "GenServer message queue lengths - high values may indicate bottlenecks",
        ),
        h.div([a.class("mt-4")], [
          element.element(
            "canvas",
            [a.id("mailboxChart"), a.attribute("height", "250")],
            [],
          ),
        ]),
      ]),
    ]),
    h.div([a.class("bg-white border border-neutral-200 rounded-lg shadow-sm")], [
      h.div([a.class("p-6")], [
        ui.heading_section("Cache Memory Over Time"),
        ui.text_small_muted("Memory usage of presence cache and push cache"),
        h.div([a.class("mt-4")], [
          element.element(
            "canvas",
            [a.id("cacheMemoryChart"), a.attribute("height", "250")],
            [],
          ),
        ]),
      ]),
    ]),
    h.script([a.src("https://fluxerstatic.com/libs/chartjs/chart.min.js")], ""),
    h.script([], render_gateway_charts_script(proxy_endpoint)),
  ])
}

fn render_gateway_charts_script(metrics_endpoint: String) -> String {
  "
  (async function() {
    const endpoint = '" <> metrics_endpoint <> "';
    if (!endpoint) return;

    const formatBytes = (bytes) => {
      if (bytes === 0) return '0 B';
      const k = 1024;
      const sizes = ['B', 'KB', 'MB', 'GB'];
      const i = Math.floor(Math.log(bytes) / Math.log(k));
      return parseFloat((bytes / Math.pow(k, i)).toFixed(2)) + ' ' + sizes[i];
    };

    const alignData = (data, timestamps) => {
      const map = new Map(data.map(d => [d.timestamp, d.value]));
      return timestamps.map(ts => map.get(ts) ?? null);
    };

    const formatTimeLabel = (ts) => {
      const d = new Date(ts);
      return d.toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' });
    };

    try {
      const [sessionsResp, guildsResp, presencesResp, callsResp] = await Promise.all([
        fetch(endpoint + '/query?metric=gateway.sessions.count').then(r => r.json()),
        fetch(endpoint + '/query?metric=gateway.guilds.count').then(r => r.json()),
        fetch(endpoint + '/query?metric=gateway.presences.count').then(r => r.json()),
        fetch(endpoint + '/query?metric=gateway.calls.count').then(r => r.json())
      ]);

      const pcTimestamps = Array.from(new Set([
        ...sessionsResp.data.map(d => d.timestamp),
        ...guildsResp.data.map(d => d.timestamp),
        ...presencesResp.data.map(d => d.timestamp),
        ...callsResp.data.map(d => d.timestamp),
      ])).sort((a, b) => a - b);

      if (pcTimestamps.length > 0) {
        new Chart(document.getElementById('processCountsChart'), {
          type: 'line',
          data: {
            labels: pcTimestamps.map(formatTimeLabel),
            datasets: [
              { label: 'Sessions', data: alignData(sessionsResp.data, pcTimestamps), borderColor: 'rgb(59, 130, 246)', tension: 0.1, spanGaps: true },
              { label: 'Guilds', data: alignData(guildsResp.data, pcTimestamps), borderColor: 'rgb(34, 197, 94)', tension: 0.1, spanGaps: true },
              { label: 'Presences', data: alignData(presencesResp.data, pcTimestamps), borderColor: 'rgb(168, 85, 247)', tension: 0.1, spanGaps: true },
              { label: 'Calls', data: alignData(callsResp.data, pcTimestamps), borderColor: 'rgb(239, 68, 68)', tension: 0.1, spanGaps: true }
            ]
          },
          options: {
            responsive: true,
            scales: { y: { beginAtZero: true } },
            plugins: { legend: { position: 'top' } }
          }
        });
      }
    } catch (e) {
      console.error('Failed to load process counts chart:', e);
    }

    try {
      const [connResp, disconnResp] = await Promise.all([
        fetch(endpoint + '/query?metric=gateway.websocket.connections').then(r => r.json()),
        fetch(endpoint + '/query?metric=gateway.websocket.disconnections').then(r => r.json())
      ]);

      const wsTimestamps = Array.from(new Set([
        ...connResp.data.map(d => d.timestamp),
        ...disconnResp.data.map(d => d.timestamp),
      ])).sort((a, b) => a - b);

      if (wsTimestamps.length > 0) {
        new Chart(document.getElementById('wsConnectionChart'), {
          type: 'line',
          data: {
            labels: wsTimestamps.map(formatTimeLabel),
            datasets: [
              { label: 'Connections', data: alignData(connResp.data, wsTimestamps), borderColor: 'rgb(34, 197, 94)', tension: 0.1, spanGaps: true },
              { label: 'Disconnections', data: alignData(disconnResp.data, wsTimestamps), borderColor: 'rgb(239, 68, 68)', tension: 0.1, spanGaps: true }
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
      console.error('Failed to load WebSocket connection chart:', e);
    }

    try {
      const [hbSuccessResp, hbFailResp] = await Promise.all([
        fetch(endpoint + '/query?metric=gateway.heartbeat.success').then(r => r.json()),
        fetch(endpoint + '/query?metric=gateway.heartbeat.failure').then(r => r.json())
      ]);

      const hbTimestamps = Array.from(new Set([
        ...hbSuccessResp.data.map(d => d.timestamp),
        ...hbFailResp.data.map(d => d.timestamp),
      ])).sort((a, b) => a - b);

      if (hbTimestamps.length > 0) {
        new Chart(document.getElementById('heartbeatChart'), {
          type: 'line',
          data: {
            labels: hbTimestamps.map(formatTimeLabel),
            datasets: [
              { label: 'Success', data: alignData(hbSuccessResp.data, hbTimestamps), borderColor: 'rgb(34, 197, 94)', tension: 0.1, spanGaps: true },
              { label: 'Failure', data: alignData(hbFailResp.data, hbTimestamps), borderColor: 'rgb(239, 68, 68)', tension: 0.1, spanGaps: true }
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
      console.error('Failed to load heartbeat chart:', e);
    }

    try {
      const [resumeSuccessResp, resumeFailResp] = await Promise.all([
        fetch(endpoint + '/query?metric=gateway.resume.success').then(r => r.json()),
        fetch(endpoint + '/query?metric=gateway.resume.failure').then(r => r.json())
      ]);

      const resumeTimestamps = Array.from(new Set([
        ...resumeSuccessResp.data.map(d => d.timestamp),
        ...resumeFailResp.data.map(d => d.timestamp),
      ])).sort((a, b) => a - b);

      if (resumeTimestamps.length > 0) {
        new Chart(document.getElementById('resumeChart'), {
          type: 'line',
          data: {
            labels: resumeTimestamps.map(formatTimeLabel),
            datasets: [
              { label: 'Success', data: alignData(resumeSuccessResp.data, resumeTimestamps), borderColor: 'rgb(34, 197, 94)', tension: 0.1, spanGaps: true },
              { label: 'Failure', data: alignData(resumeFailResp.data, resumeTimestamps), borderColor: 'rgb(239, 68, 68)', tension: 0.1, spanGaps: true }
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
      console.error('Failed to load resume chart:', e);
    }

    try {
      const rateLimitResp = await fetch(endpoint + '/query?metric=gateway.identify.rate_limited').then(r => r.json());

      const rlTimestamps = rateLimitResp.data.map(d => d.timestamp).sort((a, b) => a - b);

      if (rlTimestamps.length > 0) {
        new Chart(document.getElementById('rateLimitChart'), {
          type: 'bar',
          data: {
            labels: rlTimestamps.map(formatTimeLabel),
            datasets: [
              { label: 'Rate Limited', data: alignData(rateLimitResp.data, rlTimestamps), backgroundColor: 'rgb(251, 146, 60)' }
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
      console.error('Failed to load rate limit chart:', e);
    }

    try {
      const [p50Resp, p95Resp, p99Resp] = await Promise.all([
        fetch(endpoint + '/query?metric=gateway.rpc.latency.p50').then(r => r.json()),
        fetch(endpoint + '/query?metric=gateway.rpc.latency.p95').then(r => r.json()),
        fetch(endpoint + '/query?metric=gateway.rpc.latency.p99').then(r => r.json())
      ]);

      const latencyTimestamps = Array.from(new Set([
        ...p50Resp.data.map(d => d.timestamp),
        ...p95Resp.data.map(d => d.timestamp),
        ...p99Resp.data.map(d => d.timestamp),
      ])).sort((a, b) => a - b);

      if (latencyTimestamps.length > 0) {
        new Chart(document.getElementById('rpcLatencyChart'), {
          type: 'line',
          data: {
            labels: latencyTimestamps.map(formatTimeLabel),
            datasets: [
              { label: 'p50', data: alignData(p50Resp.data, latencyTimestamps), borderColor: 'rgb(34, 197, 94)', tension: 0.1, spanGaps: true },
              { label: 'p95', data: alignData(p95Resp.data, latencyTimestamps), borderColor: 'rgb(251, 146, 60)', tension: 0.1, spanGaps: true },
              { label: 'p99', data: alignData(p99Resp.data, latencyTimestamps), borderColor: 'rgb(239, 68, 68)', tension: 0.1, spanGaps: true }
            ]
          },
          options: {
            responsive: true,
            scales: { y: { beginAtZero: true, title: { display: true, text: 'Latency (ms)' } } },
            plugins: { legend: { position: 'top' } }
          }
        });
      }
    } catch (e) {
      console.error('Failed to load RPC latency chart:', e);
    }

    try {
      const [smResp, gmResp, pmResp, cmResp, pushResp] = await Promise.all([
        fetch(endpoint + '/query?metric=gateway.mailbox.session_manager').then(r => r.json()),
        fetch(endpoint + '/query?metric=gateway.mailbox.guild_manager').then(r => r.json()),
        fetch(endpoint + '/query?metric=gateway.mailbox.presence_manager').then(r => r.json()),
        fetch(endpoint + '/query?metric=gateway.mailbox.call_manager').then(r => r.json()),
        fetch(endpoint + '/query?metric=gateway.mailbox.push').then(r => r.json())
      ]);

      const mbTimestamps = Array.from(new Set([
        ...smResp.data.map(d => d.timestamp),
        ...gmResp.data.map(d => d.timestamp),
        ...pmResp.data.map(d => d.timestamp),
        ...cmResp.data.map(d => d.timestamp),
        ...pushResp.data.map(d => d.timestamp),
      ])).sort((a, b) => a - b);

      if (mbTimestamps.length > 0) {
        new Chart(document.getElementById('mailboxChart'), {
          type: 'line',
          data: {
            labels: mbTimestamps.map(formatTimeLabel),
            datasets: [
              { label: 'Session Manager', data: alignData(smResp.data, mbTimestamps), borderColor: 'rgb(59, 130, 246)', tension: 0.1, spanGaps: true },
              { label: 'Guild Manager', data: alignData(gmResp.data, mbTimestamps), borderColor: 'rgb(34, 197, 94)', tension: 0.1, spanGaps: true },
              { label: 'Presence Manager', data: alignData(pmResp.data, mbTimestamps), borderColor: 'rgb(168, 85, 247)', tension: 0.1, spanGaps: true },
              { label: 'Call Manager', data: alignData(cmResp.data, mbTimestamps), borderColor: 'rgb(239, 68, 68)', tension: 0.1, spanGaps: true },
              { label: 'Push', data: alignData(pushResp.data, mbTimestamps), borderColor: 'rgb(251, 146, 60)', tension: 0.1, spanGaps: true }
            ]
          },
          options: {
            responsive: true,
            scales: { y: { beginAtZero: true, title: { display: true, text: 'Queue Length' } } },
            plugins: { legend: { position: 'top' } }
          }
        });
      }
    } catch (e) {
      console.error('Failed to load mailbox chart:', e);
    }

    try {
      const [presenceCacheResp, pushMemResp] = await Promise.all([
        fetch(endpoint + '/query?metric=gateway.memory.presence_cache').then(r => r.json()),
        fetch(endpoint + '/query?metric=gateway.memory.push').then(r => r.json())
      ]);

      const memTimestamps = Array.from(new Set([
        ...presenceCacheResp.data.map(d => d.timestamp),
        ...pushMemResp.data.map(d => d.timestamp),
      ])).sort((a, b) => a - b);

      if (memTimestamps.length > 0) {
        new Chart(document.getElementById('cacheMemoryChart'), {
          type: 'line',
          data: {
            labels: memTimestamps.map(formatTimeLabel),
            datasets: [
              { label: 'Presence Cache', data: alignData(presenceCacheResp.data, memTimestamps), borderColor: 'rgb(59, 130, 246)', tension: 0.1, spanGaps: true },
              { label: 'Push Cache', data: alignData(pushMemResp.data, memTimestamps), borderColor: 'rgb(34, 197, 94)', tension: 0.1, spanGaps: true }
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
      console.error('Failed to load cache memory chart:', e);
    }
  })();
  "
}

fn render_stat_card(_ctx: Context, label: String, value: String) {
  h.div(
    [
      a.class("bg-neutral-50 rounded-lg p-4 border border-neutral-200"),
    ],
    [
      h.div(
        [
          a.class("text-xs text-neutral-600 uppercase tracking-wider mb-1"),
        ],
        [
          element.text(label),
        ],
      ),
      h.div([a.class("text-base font-semibold text-neutral-900")], [
        element.text(value),
      ]),
    ],
  )
}
