// SPDX-License-Identifier: AGPL-3.0-or-later

import {registerCounter, registerGauge, registerHistogram} from '@fluxer/hono/src/middleware/Metrics';

let sweepLagSeconds = 0;

export const threadsCreatedTotal = registerCounter(
	'fluxer_api_threads_created_total',
	'Threads created, by kind and channel type',
);
export const threadsArchivedTotal = registerCounter('fluxer_api_threads_archived_total', 'Threads archived, by reason');
export const threadArchiveSweepBatch = registerHistogram(
	'fluxer_api_thread_archive_sweep_batch',
	'Threads archived per auto-archive sweep tick',
	[0, 1, 10, 50, 100, 200],
);
export const threadArchiveSweepSkippedInactiveTotal = registerCounter(
	'fluxer_api_thread_archive_sweep_skipped_inactive_total',
	'Guilds skipped by the auto-archive sweep because the experiment is inactive for them',
);
registerGauge(
	'fluxer_api_thread_archive_sweep_lag_seconds',
	'Age of the oldest overdue thread seen by the last auto-archive sweep',
	() => sweepLagSeconds,
);
export const threadMemberAutojoinTotal = registerCounter(
	'fluxer_api_thread_member_autojoin_total',
	'Thread members added automatically, by source',
);
export const threadSearchIndexEnqueueCappedTotal = registerCounter(
	'fluxer_api_thread_search_index_enqueue_capped_total',
	'Guild searches whose channel index enqueue was capped',
);
export const webhookThreadIdRefusedTotal = registerCounter(
	'fluxer_api_webhook_thread_id_refused_total',
	'Webhook requests refused because thread_id is not usable outside the experiment',
);

export const threadRoleMentionCappedTotal = registerCounter(
	'fluxer_api_thread_role_mention_capped_total',
	'Thread messages whose role mentions hit the member add cap',
);

export function setThreadArchiveSweepLag(seconds: number): void {
	sweepLagSeconds = Math.max(0, seconds);
}
