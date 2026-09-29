// SPDX-License-Identifier: AGPL-3.0-or-later

import type {EventKind} from '@app/api/infrastructure/activity/Contract.generated';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';

export const ACTIVITY_SPOOL_KEY = 'activity:spool';
export const ACTIVITY_SPOOL_SIGNAL_CAP = 200_000;
export const ACTIVITY_SPOOL_MAX_AGE_MS = 7 * 24 * 60 * 60 * 1000;
const DRAIN_BATCH = 200;
export const DRAIN_BUDGET_MS = 15_000;

export interface ActivitySpoolEntry {
	kind: EventKind;
	subject: string;
	id: string;
	ttl: string;
	at_ms: number;
	data: string;
}

export interface ActivityPublishOptions {
	msgID: string;
	ttl: string;
	spooled: boolean;
}

export interface ActivityPublisher {
	publish(subject: string, payload: string, options: ActivityPublishOptions): Promise<void>;
}

type SpoolStore = Pick<IKVProvider, 'rpush' | 'llen' | 'lrange' | 'ltrim'>;

export interface ActivityDrainResult {
	published: number;
	expired: number;
	failed: number;
	remaining: number;
	error: unknown;
}

export async function spoolActivity(
	kv: SpoolStore,
	entry: ActivitySpoolEntry,
	always: boolean,
): Promise<'spooled' | 'dropped'> {
	if (!always && (await kv.llen(ACTIVITY_SPOOL_KEY)) >= ACTIVITY_SPOOL_SIGNAL_CAP) {
		return 'dropped';
	}
	await kv.rpush(ACTIVITY_SPOOL_KEY, JSON.stringify(entry));
	return 'spooled';
}

function parseJson(raw: string): unknown {
	try {
		return JSON.parse(raw);
	} catch {
		return null;
	}
}

function parseEntry(raw: string): ActivitySpoolEntry | null {
	const value = parseJson(raw) as Partial<ActivitySpoolEntry> | null;
	if (
		value &&
		typeof value.subject === 'string' &&
		typeof value.id === 'string' &&
		typeof value.ttl === 'string' &&
		typeof value.at_ms === 'number' &&
		typeof value.data === 'string'
	) {
		return value as ActivitySpoolEntry;
	}
	return null;
}

export async function drainActivitySpool(
	publisher: ActivityPublisher,
	kv: SpoolStore,
	options: {nowMs?: () => number; budgetMs?: number; keepLock?: () => Promise<boolean>} = {},
): Promise<ActivityDrainResult> {
	const now = options.nowMs ?? Date.now;
	const deadline = now() + (options.budgetMs ?? DRAIN_BUDGET_MS);
	const result: ActivityDrainResult = {published: 0, expired: 0, failed: 0, remaining: 0, error: null};
	while (now() < deadline) {
		const batch = await kv.lrange(ACTIVITY_SPOOL_KEY, 0, DRAIN_BATCH - 1);
		if (batch.length === 0) break;
		let consumed = 0;
		for (const raw of batch) {
			if (now() >= deadline) break;
			const entry = parseEntry(raw);
			if (!entry) {
				result.failed++;
				consumed++;
				continue;
			}
			if (entry.at_ms < now() - ACTIVITY_SPOOL_MAX_AGE_MS) {
				result.expired++;
				consumed++;
				continue;
			}
			try {
				await publisher.publish(entry.subject, entry.data, {msgID: entry.id, ttl: entry.ttl, spooled: true});
			} catch (error) {
				result.error = error;
				break;
			}
			result.published++;
			consumed++;
		}
		if (consumed > 0) {
			if (options.keepLock && !(await options.keepLock())) {
				result.error = new Error('activity spool drain lock lost before trim');
				break;
			}
			await kv.ltrim(ACTIVITY_SPOOL_KEY, consumed, -1);
		}
		if (result.error !== null || consumed < batch.length) break;
	}
	result.remaining = await kv.llen(ACTIVITY_SPOOL_KEY);
	return result;
}
