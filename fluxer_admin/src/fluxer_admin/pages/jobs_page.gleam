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
import fluxer_admin/components/flash
import fluxer_admin/components/layout
import fluxer_admin/components/ui
import fluxer_admin/web.{type Context, type Session, prepend_base_path}
import gleam/option.{type Option, None, Some}
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
      "Jobs",
      "jobs",
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
    ui.heading_page("Jobs Dashboard"),
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
  let proxy_endpoint = prepend_base_path(ctx, "/api/metrics")

  h.div([], [
    ui.heading_page("Jobs Dashboard"),
    h.div([a.class("mt-6")], [
      h.div(
        [a.class("bg-white border border-neutral-200 rounded-lg shadow-sm")],
        [
          h.div([a.class("p-6")], [
            ui.heading_section("Queue Overview"),
            ui.text_small_muted("Current job queue totals across all workers"),
            h.div([a.id("queue-stats-container"), a.class("mt-4")], [
              h.div(
                [
                  a.class("grid grid-cols-2 md:grid-cols-5 gap-4"),
                ],
                [
                  render_loading_stat_card("Total Pending", "pending-count"),
                  render_loading_stat_card("Total Running", "running-count"),
                  render_loading_stat_card("Total Failed", "failed-count"),
                  render_loading_stat_card(
                    "Worker Utilization",
                    "concurrency-utilization",
                  ),
                  render_loading_stat_card("Avg Wait Time", "avg-wait-time"),
                ],
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
            ui.heading_section("Redis Queue Sizes"),
            ui.text_small_muted("Background job queues stored in Redis"),
            h.div(
              [
                a.id("redis-queue-container"),
                a.class("grid grid-cols-2 md:grid-cols-4 gap-4 mt-4"),
              ],
              [
                render_loading_stat_card(
                  "Asset Deletion",
                  "redis-asset-deletion",
                ),
                render_loading_stat_card(
                  "Cloudflare Purge",
                  "redis-cloudflare-purge",
                ),
                render_loading_stat_card(
                  "Bulk Message Deletion",
                  "redis-bulk-message-deletion",
                ),
                render_loading_stat_card(
                  "Account Deletion",
                  "redis-account-deletion",
                ),
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
            ui.heading_section("Per-Task Breakdown"),
            ui.text_small_muted(
              "Pending, running, success rate, and retries by task",
            ),
            h.div([a.id("per-task-container"), a.class("mt-4")], [
              h.div([a.class("text-neutral-500 text-sm")], [
                element.text("Loading..."),
              ]),
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
            ui.heading_section("Job Throughput Over Time"),
            ui.text_small_muted(
              "Jobs completed per minute (successes, errors, retries)",
            ),
            h.div([a.class("mt-4")], [
              element.element(
                "canvas",
                [a.id("jobThroughputChart"), a.attribute("height", "250")],
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
            ui.heading_section("Queue Depths Over Time"),
            ui.text_small_muted("Pending and running job counts over time"),
            h.div([a.class("mt-4")], [
              element.element(
                "canvas",
                [a.id("queueDepthsChart"), a.attribute("height", "250")],
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
            ui.heading_section("Job Latency Percentiles"),
            ui.text_small_muted("Processing time distribution (p50, p90, p99)"),
            h.div([a.class("mt-4")], [
              element.element(
                "canvas",
                [a.id("latencyPercentilesChart"), a.attribute("height", "250")],
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
            ui.heading_section("Cron Job Status"),
            ui.text_small_muted("Scheduled job execution tracking"),
            h.div([a.id("cron-status-container"), a.class("mt-4")], [
              h.div([a.class("text-neutral-500 text-sm")], [
                element.text("Loading..."),
              ]),
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
            ui.heading_section("Recent Job Errors"),
            ui.text_small_muted("Tasks with recent failures"),
            h.div([a.id("recent-errors-container"), a.class("mt-4")], [
              h.div([a.class("text-neutral-500 text-sm")], [
                element.text("Loading..."),
              ]),
            ]),
          ]),
        ],
      ),
    ]),
    h.script([a.src("https://fluxerstatic.com/libs/chartjs/chart.min.js")], ""),
    h.script([], render_jobs_script(proxy_endpoint)),
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

fn render_jobs_script(metrics_endpoint: String) -> String {
  "
  (async function() {
    const endpoint = '" <> metrics_endpoint <> "';
    if (!endpoint) return;

    const formatNumber = (n) => {
      if (n === null || n === undefined) return '-';
      return n.toLocaleString();
    };

    const formatMs = (ms) => {
      if (ms === null || ms === undefined) return '-';
      if (ms < 1000) return Math.round(ms) + 'ms';
      if (ms < 60000) return (ms / 1000).toFixed(1) + 's';
      return (ms / 60000).toFixed(1) + 'm';
    };

    const formatPercent = (n) => {
      if (n === null || n === undefined) return '-';
      return Math.round(n) + '%';
    };

    const formatTimeLabel = (ts) => {
      const d = new Date(ts);
      return d.toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' });
    };

    const formatRelativeTime = (date) => {
      if (!date) return 'Never';
      const diff = Date.now() - new Date(date).getTime();
      if (diff < 60000) return 'Just now';
      if (diff < 3600000) return Math.floor(diff / 60000) + 'm ago';
      if (diff < 86400000) return Math.floor(diff / 3600000) + 'h ago';
      return Math.floor(diff / 86400000) + 'd ago';
    };

    const alignData = (data, timestamps) => {
      const map = new Map(data.map(d => [d.timestamp, d.value]));
      return timestamps.map(ts => map.get(ts) ?? null);
    };

    const getLatestValue = (data) => {
      if (!data || data.length === 0) return null;
      const sorted = [...data].sort((a, b) => b.timestamp - a.timestamp);
      return sorted[0]?.value ?? null;
    };

    try {
      const [pendingResp, runningResp, failedResp, utilizationResp, waitTimeResp] = await Promise.all([
        fetch(endpoint + '/query?metric=worker.queue.total_pending').then(r => r.json()),
        fetch(endpoint + '/query?metric=worker.queue.total_running').then(r => r.json()),
        fetch(endpoint + '/query?metric=worker.queue.total_failed').then(r => r.json()),
        fetch(endpoint + '/query?metric=worker.concurrency.utilization_percent').then(r => r.json()),
        fetch(endpoint + '/query?metric=worker.job.avg_wait_time_ms_total').then(r => r.json())
      ]);

      document.getElementById('pending-count').textContent = formatNumber(getLatestValue(pendingResp.data));
      document.getElementById('running-count').textContent = formatNumber(getLatestValue(runningResp.data));
      document.getElementById('failed-count').textContent = formatNumber(getLatestValue(failedResp.data));
      document.getElementById('concurrency-utilization').textContent = formatPercent(getLatestValue(utilizationResp.data));
      document.getElementById('avg-wait-time').textContent = formatMs(getLatestValue(waitTimeResp.data));
    } catch (e) {
      console.error('Failed to load queue stats:', e);
    }

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
    } catch (e) {
      console.error('Failed to load Redis queue stats:', e);
    }

    try {
      const [pendingTaskResp, runningTaskResp, successTaskResp, errorTaskResp, retryTaskResp] = await Promise.all([
        fetch(endpoint + '/query/aggregate?metric=worker.queue.pending&group_by=task').then(r => r.json()),
        fetch(endpoint + '/query/aggregate?metric=worker.queue.running&group_by=task').then(r => r.json()),
        fetch(endpoint + '/query/aggregate?metric=worker.job.success&group_by=task').then(r => r.json()),
        fetch(endpoint + '/query/aggregate?metric=worker.job.error&group_by=task').then(r => r.json()),
        fetch(endpoint + '/query/aggregate?metric=worker.job.retries&group_by=task').then(r => r.json())
      ]);

      const container = document.getElementById('per-task-container');
      const pendingBreakdown = pendingTaskResp.breakdown || [];
      const runningBreakdown = runningTaskResp.breakdown || [];
      const successBreakdown = successTaskResp.breakdown || [];
      const errorBreakdown = errorTaskResp.breakdown || [];
      const retryBreakdown = retryTaskResp.breakdown || [];

      const taskSet = new Set([
        ...pendingBreakdown.map(e => e.label),
        ...runningBreakdown.map(e => e.label),
        ...successBreakdown.map(e => e.label),
        ...errorBreakdown.map(e => e.label)
      ]);

      if (taskSet.size === 0) {
        container.innerHTML = '<div class=\"text-neutral-500 text-sm\">No per-task data available</div>';
      } else {
        const tasks = Array.from(taskSet).sort();
        const pendingMap = new Map(pendingBreakdown.map(e => [e.label, e.value]));
        const runningMap = new Map(runningBreakdown.map(e => [e.label, e.value]));
        const successMap = new Map(successBreakdown.map(e => [e.label, e.value]));
        const errorMap = new Map(errorBreakdown.map(e => [e.label, e.value]));
        const retryMap = new Map(retryBreakdown.map(e => [e.label, e.value]));

        let html = '<div class=\"overflow-x-auto\"><table class=\"min-w-full divide-y divide-neutral-200\">';
        html += '<thead class=\"bg-neutral-50\"><tr>';
        html += '<th class=\"px-4 py-2 text-left text-xs font-medium text-neutral-500 uppercase\">Task</th>';
        html += '<th class=\"px-4 py-2 text-right text-xs font-medium text-neutral-500 uppercase\">Pending</th>';
        html += '<th class=\"px-4 py-2 text-right text-xs font-medium text-neutral-500 uppercase\">Running</th>';
        html += '<th class=\"px-4 py-2 text-right text-xs font-medium text-neutral-500 uppercase\">Success</th>';
        html += '<th class=\"px-4 py-2 text-right text-xs font-medium text-neutral-500 uppercase\">Errors</th>';
        html += '<th class=\"px-4 py-2 text-right text-xs font-medium text-neutral-500 uppercase\">Success Rate</th>';
        html += '<th class=\"px-4 py-2 text-right text-xs font-medium text-neutral-500 uppercase\">Retries</th>';
        html += '</tr></thead><tbody class=\"bg-white divide-y divide-neutral-200\">';

        for (const task of tasks) {
          const pending = pendingMap.get(task) ?? 0;
          const running = runningMap.get(task) ?? 0;
          const success = successMap.get(task) ?? 0;
          const errors = errorMap.get(task) ?? 0;
          const retries = retryMap.get(task) ?? 0;
          const total = success + errors;
          const successRate = total > 0 ? ((success / total) * 100).toFixed(1) + '%' : '-';
          const successRateClass = total > 0 ? (success / total >= 0.95 ? 'text-green-600' : success / total >= 0.8 ? 'text-yellow-600' : 'text-red-600') : 'text-neutral-600';

          html += '<tr class=\"hover:bg-neutral-50\">';
          html += '<td class=\"px-4 py-2 text-sm font-medium text-neutral-900\">' + task + '</td>';
          html += '<td class=\"px-4 py-2 text-sm text-neutral-600 text-right\">' + formatNumber(pending) + '</td>';
          html += '<td class=\"px-4 py-2 text-sm text-neutral-600 text-right\">' + formatNumber(running) + '</td>';
          html += '<td class=\"px-4 py-2 text-sm text-green-600 text-right\">' + formatNumber(success) + '</td>';
          html += '<td class=\"px-4 py-2 text-sm text-red-600 text-right\">' + formatNumber(errors) + '</td>';
          html += '<td class=\"px-4 py-2 text-sm font-medium text-right ' + successRateClass + '\">' + successRate + '</td>';
          html += '<td class=\"px-4 py-2 text-sm text-orange-600 text-right\">' + formatNumber(retries) + '</td>';
          html += '</tr>';
        }

        html += '</tbody></table></div>';
        container.innerHTML = html;
      }
    } catch (e) {
      console.error('Failed to load per-task stats:', e);
      document.getElementById('per-task-container').innerHTML = '<div class=\"text-red-500 text-sm\">Failed to load per-task data</div>';
    }

    try {
      const [successResp, errorResp, retryResp] = await Promise.all([
        fetch(endpoint + '/query?metric=worker.job.success').then(r => r.json()),
        fetch(endpoint + '/query?metric=worker.job.error').then(r => r.json()),
        fetch(endpoint + '/query?metric=worker.job.retry').then(r => r.json())
      ]);

      const timestamps = Array.from(new Set([
        ...successResp.data.map(d => d.timestamp),
        ...errorResp.data.map(d => d.timestamp),
        ...(retryResp.data || []).map(d => d.timestamp),
      ])).sort((a, b) => a - b);

      if (timestamps.length > 0) {
        new Chart(document.getElementById('jobThroughputChart'), {
          type: 'line',
          data: {
            labels: timestamps.map(formatTimeLabel),
            datasets: [
              { label: 'Successes', data: alignData(successResp.data, timestamps), borderColor: 'rgb(34, 197, 94)', backgroundColor: 'rgba(34, 197, 94, 0.1)', fill: true, tension: 0.1, spanGaps: true },
              { label: 'Errors', data: alignData(errorResp.data, timestamps), borderColor: 'rgb(239, 68, 68)', backgroundColor: 'rgba(239, 68, 68, 0.1)', fill: true, tension: 0.1, spanGaps: true },
              { label: 'Retries', data: alignData(retryResp.data || [], timestamps), borderColor: 'rgb(249, 115, 22)', backgroundColor: 'rgba(249, 115, 22, 0.1)', fill: true, tension: 0.1, spanGaps: true }
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
      console.error('Failed to load job throughput chart:', e);
    }

    try {
      const [pendingResp, runningResp] = await Promise.all([
        fetch(endpoint + '/query?metric=worker.queue.total_pending').then(r => r.json()),
        fetch(endpoint + '/query?metric=worker.queue.total_running').then(r => r.json())
      ]);

      const timestamps = Array.from(new Set([
        ...pendingResp.data.map(d => d.timestamp),
        ...runningResp.data.map(d => d.timestamp),
      ])).sort((a, b) => a - b);

      if (timestamps.length > 0) {
        new Chart(document.getElementById('queueDepthsChart'), {
          type: 'line',
          data: {
            labels: timestamps.map(formatTimeLabel),
            datasets: [
              { label: 'Pending', data: alignData(pendingResp.data, timestamps), borderColor: 'rgb(59, 130, 246)', backgroundColor: 'rgba(59, 130, 246, 0.1)', fill: true, tension: 0.1, spanGaps: true },
              { label: 'Running', data: alignData(runningResp.data, timestamps), borderColor: 'rgb(168, 85, 247)', backgroundColor: 'rgba(168, 85, 247, 0.1)', fill: true, tension: 0.1, spanGaps: true }
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
      console.error('Failed to load queue depths chart:', e);
    }

    try {
      const latencyResp = await fetch(endpoint + '/query/percentiles?metric=worker.job.latency').then(r => r.json());
      const percentiles = latencyResp.percentiles || {};

      if (Object.keys(percentiles).length > 0) {
        const labels = Object.keys(percentiles).sort();
        const p50Data = labels.map(task => percentiles[task]?.p50 ?? null);
        const p90Data = labels.map(task => percentiles[task]?.p90 ?? null);
        const p99Data = labels.map(task => percentiles[task]?.p99 ?? null);

        new Chart(document.getElementById('latencyPercentilesChart'), {
          type: 'bar',
          data: {
            labels: labels,
            datasets: [
              { label: 'p50', data: p50Data, backgroundColor: 'rgba(59, 130, 246, 0.8)' },
              { label: 'p90', data: p90Data, backgroundColor: 'rgba(249, 115, 22, 0.8)' },
              { label: 'p99', data: p99Data, backgroundColor: 'rgba(239, 68, 68, 0.8)' }
            ]
          },
          options: {
            responsive: true,
            scales: {
              y: {
                beginAtZero: true,
                title: { display: true, text: 'Latency (ms)' }
              }
            },
            plugins: { legend: { position: 'top' } }
          }
        });
      } else {
        document.getElementById('latencyPercentilesChart').parentElement.innerHTML = '<div class=\"text-neutral-500 text-sm text-center py-8\">No latency data available</div>';
      }
    } catch (e) {
      console.error('Failed to load latency percentiles chart:', e);
      document.getElementById('latencyPercentilesChart').parentElement.innerHTML = '<div class=\"text-neutral-500 text-sm text-center py-8\">No latency data available</div>';
    }

    try {
      const cronResp = await fetch(endpoint + '/query/aggregate?metric=worker.cron.last_run_age_ms&group_by=task').then(r => r.json());
      const overdueResp = await fetch(endpoint + '/query/aggregate?metric=worker.cron.overdue&group_by=task').then(r => r.json());

      const container = document.getElementById('cron-status-container');
      const cronBreakdown = cronResp.breakdown || [];
      const overdueBreakdown = overdueResp.breakdown || [];
      const overdueMap = new Map(overdueBreakdown.map(e => [e.label, e.value]));

      if (cronBreakdown.length === 0) {
        container.innerHTML = '<div class=\"text-neutral-500 text-sm\">No cron job data available</div>';
      } else {
        let html = '<div class=\"overflow-x-auto\"><table class=\"min-w-full divide-y divide-neutral-200\">';
        html += '<thead class=\"bg-neutral-50\"><tr>';
        html += '<th class=\"px-4 py-2 text-left text-xs font-medium text-neutral-500 uppercase\">Cron Task</th>';
        html += '<th class=\"px-4 py-2 text-right text-xs font-medium text-neutral-500 uppercase\">Last Run</th>';
        html += '<th class=\"px-4 py-2 text-center text-xs font-medium text-neutral-500 uppercase\">Status</th>';
        html += '</tr></thead><tbody class=\"bg-white divide-y divide-neutral-200\">';

        for (const cron of cronBreakdown.sort((a, b) => a.label.localeCompare(b.label))) {
          const isOverdue = overdueMap.get(cron.label) === 1;
          const statusClass = isOverdue ? 'bg-red-100 text-red-800' : 'bg-green-100 text-green-800';
          const statusText = isOverdue ? 'Overdue' : 'OK';

          html += '<tr class=\"hover:bg-neutral-50\">';
          html += '<td class=\"px-4 py-2 text-sm font-medium text-neutral-900\">' + cron.label + '</td>';
          html += '<td class=\"px-4 py-2 text-sm text-neutral-600 text-right\">' + formatMs(cron.value) + ' ago</td>';
          html += '<td class=\"px-4 py-2 text-center\"><span class=\"px-2 py-1 text-xs font-medium rounded-full ' + statusClass + '\">' + statusText + '</span></td>';
          html += '</tr>';
        }

        html += '</tbody></table></div>';
        container.innerHTML = html;
      }
    } catch (e) {
      console.error('Failed to load cron status:', e);
      document.getElementById('cron-status-container').innerHTML = '<div class=\"text-red-500 text-sm\">Failed to load cron data</div>';
    }

    try {
      const errorTaskResp = await fetch(endpoint + '/query/aggregate?metric=worker.job.error&group_by=task').then(r => r.json());
      const permFailResp = await fetch(endpoint + '/query/aggregate?metric=worker.job.permanently_failed&group_by=task').then(r => r.json());

      const container = document.getElementById('recent-errors-container');
      const errorBreakdown = errorTaskResp.breakdown || [];
      const permFailBreakdown = permFailResp.breakdown || [];
      const permFailMap = new Map(permFailBreakdown.map(e => [e.label, e.value]));

      const tasksWithErrors = errorBreakdown.filter(e => e.value > 0).sort((a, b) => b.value - a.value);

      if (tasksWithErrors.length === 0) {
        container.innerHTML = '<div class=\"text-green-600 text-sm flex items-center gap-2\"><svg xmlns=\"http://www.w3.org/2000/svg\" class=\"h-5 w-5\" viewBox=\"0 0 20 20\" fill=\"currentColor\"><path fill-rule=\"evenodd\" d=\"M10 18a8 8 0 100-16 8 8 0 000 16zm3.707-9.293a1 1 0 00-1.414-1.414L9 10.586 7.707 9.293a1 1 0 00-1.414 1.414l2 2a1 1 0 001.414 0l4-4z\" clip-rule=\"evenodd\" /></svg> No recent errors</div>';
      } else {
        let html = '<div class=\"overflow-x-auto\"><table class=\"min-w-full divide-y divide-neutral-200\">';
        html += '<thead class=\"bg-neutral-50\"><tr>';
        html += '<th class=\"px-4 py-2 text-left text-xs font-medium text-neutral-500 uppercase\">Task</th>';
        html += '<th class=\"px-4 py-2 text-right text-xs font-medium text-neutral-500 uppercase\">Total Errors</th>';
        html += '<th class=\"px-4 py-2 text-right text-xs font-medium text-neutral-500 uppercase\">Permanently Failed</th>';
        html += '</tr></thead><tbody class=\"bg-white divide-y divide-neutral-200\">';

        for (const task of tasksWithErrors.slice(0, 10)) {
          const permFailed = permFailMap.get(task.label) ?? 0;
          html += '<tr class=\"hover:bg-neutral-50\">';
          html += '<td class=\"px-4 py-2 text-sm font-medium text-neutral-900\">' + task.label + '</td>';
          html += '<td class=\"px-4 py-2 text-sm text-red-600 text-right font-medium\">' + formatNumber(task.value) + '</td>';
          html += '<td class=\"px-4 py-2 text-sm text-right ' + (permFailed > 0 ? 'text-red-800 font-bold' : 'text-neutral-600') + '\">' + formatNumber(permFailed) + '</td>';
          html += '</tr>';
        }

        html += '</tbody></table></div>';
        container.innerHTML = html;
      }
    } catch (e) {
      console.error('Failed to load recent errors:', e);
      document.getElementById('recent-errors-container').innerHTML = '<div class=\"text-red-500 text-sm\">Failed to load error data</div>';
    }
  })();
  "
}
