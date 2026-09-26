// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/api/Logger';
import type {WorkerTaskName} from '@app/api/worker/WorkerLaneConfig';
import {WorkerQueueOverflowError} from '@app/api/worker/WorkerQueueOverflowError';
import type {WorkerService} from '@app/api/worker/WorkerService';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import {ms} from 'itty-time';

const INITIAL_SYNC_KEY = 'sync:email_domains:initialized';
const PURGE_KEY = 'sync:blocklist_feeds:purged';
const CLAIM_TTL_SECONDS = ms('6 hours') / 1000;
const FEED_TASKS = [
	'syncDisposableEmailDomains',
	'syncUrlBlocklists',
	'syncFileShaBlocklists',
] as const satisfies ReadonlyArray<WorkerTaskName>;

export async function queueBlocklistFeedStartupJobs(
	kvClient: Pick<IKVProvider, 'setnx' | 'del'>,
	workerService: Pick<WorkerService, 'addJob'>,
	enabled: boolean,
): Promise<void> {
	if (enabled) {
		if (await kvClient.setnx(INITIAL_SYNC_KEY, '1', CLAIM_TTL_SECONDS)) {
			Logger.info('Triggering initial disposable email domain sync');
			await queueJobs(workerService, ['syncDisposableEmailDomains']);
		}
		return;
	}
	const wasEnabled = (await kvClient.del(INITIAL_SYNC_KEY)) > 0;
	const claimed = await kvClient.setnx(PURGE_KEY, '1', CLAIM_TTL_SECONDS);
	if (!wasEnabled && !claimed) return;
	Logger.info('Removing blocklist feed data, blocklist feeds are disabled');
	await queueJobs(workerService, FEED_TASKS);
}

async function queueJobs(
	workerService: Pick<WorkerService, 'addJob'>,
	tasks: ReadonlyArray<WorkerTaskName>,
): Promise<void> {
	for (const task of tasks) {
		try {
			await workerService.addJob(task, {});
		} catch (error) {
			if (!(error instanceof WorkerQueueOverflowError)) {
				throw error;
			}
			Logger.warn({task}, 'Dropped blocklist feed job, jobs stream is at its limit');
		}
	}
}
