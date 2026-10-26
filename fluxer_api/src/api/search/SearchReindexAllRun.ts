// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import {seconds} from 'itty-time';

const RUN_KEY = 'index_refresh:all';
const RUN_TTL_SECONDS = seconds('1 hour');
const CLAIM_ATTEMPTS = 3;

export const INDEX_REFRESH_STATUS_TTL_SECONDS = seconds('1 hour');

export function indexRefreshStatusKey(jobId: string): string {
	return `index_refresh_status:${jobId}`;
}

export async function claimSearchReindexAll(kvClient: IKVProvider, jobId: string): Promise<string | null> {
	for (let attempt = 0; attempt < CLAIM_ATTEMPTS; attempt++) {
		if (await kvClient.acquireLock(RUN_KEY, jobId, RUN_TTL_SECONDS)) return null;
		const running = await kvClient.get(RUN_KEY);
		if (running) return running;
	}
	throw new Error('Could not claim the search reindex run');
}

export async function holdSearchReindexAll(kvClient: IKVProvider, jobId: string): Promise<boolean> {
	return (
		(await kvClient.extendLock(RUN_KEY, jobId, RUN_TTL_SECONDS)) ||
		(await kvClient.acquireLock(RUN_KEY, jobId, RUN_TTL_SECONDS))
	);
}

export async function finishSearchReindexAll(kvClient: IKVProvider, jobId: string): Promise<void> {
	await kvClient.releaseLock(RUN_KEY, jobId);
}
