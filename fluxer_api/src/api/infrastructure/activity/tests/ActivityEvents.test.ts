// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	activityCountForTests,
	drainActivitySpoolNow,
	emitActivity,
	resetActivityEventsForTests,
	startActivityEvents,
} from '@app/api/infrastructure/activity/ActivityEvents';
import {workerMeta} from '@app/api/infrastructure/activity/ActivityMeta';
import {
	ACTIVITY_SPOOL_KEY,
	ACTIVITY_SPOOL_MAX_AGE_MS,
	type ActivityPublisher,
	type ActivityPublishOptions,
	type ActivitySpoolEntry,
	drainActivitySpool,
} from '@app/api/infrastructure/activity/ActivitySpool';
import {JetStreamApiError} from '@nats-io/jetstream';
import {TimeoutError} from '@nats-io/transport-node';
import {MockKVProvider} from '@pkgs/kv_client/src/testing/MockKVProvider';
import {afterEach, describe, expect, it} from 'vitest';

interface PublishCall {
	subject: string;
	payload: string;
	options: ActivityPublishOptions;
}

class FakePublisher implements ActivityPublisher {
	readonly calls: Array<PublishCall> = [];
	failures: Array<Error> = [];

	async publish(subject: string, payload: string, options: ActivityPublishOptions): Promise<void> {
		this.calls.push({subject, payload, options});
		const failure = this.failures.shift();
		if (failure) throw failure;
	}
}
const DELETED = {user_id: '1174109840998400001'};

async function spooled(kv: MockKVProvider): Promise<Array<ActivitySpoolEntry>> {
	return (await kv.lrange(ACTIVITY_SPOOL_KEY, 0, -1)).map((raw) => JSON.parse(raw) as ActivitySpoolEntry);
}

function rejection(): JetStreamApiError {
	return new JetStreamApiError({code: 10047, err_code: 10047, description: 'maximum bytes exceeded'});
}

describe('activity events', () => {
	afterEach(() => {
		resetActivityEventsForTests();
	});

	it('retries a timed out publish with the same message id', async () => {
		const publisher = new FakePublisher();
		publisher.failures = [new TimeoutError(), new TimeoutError()];
		const kv = new MockKVProvider();
		await startActivityEvents({publisher, kv});
		await emitActivity('account_deleted', DELETED.user_id, DELETED, workerMeta(), DELETED.user_id);
		expect(publisher.calls).toHaveLength(3);
		expect(new Set(publisher.calls.map((call) => call.options.msgID)).size).toBe(1);
		expect(await spooled(kv)).toEqual([]);
	});

	it('spools a fact once every retry failed, before the caller continues', async () => {
		const publisher = new FakePublisher();
		publisher.failures = [new TimeoutError(), new TimeoutError(), new TimeoutError()];
		const kv = new MockKVProvider();
		await startActivityEvents({publisher, kv});
		await emitActivity('account_deleted', DELETED.user_id, DELETED, workerMeta(), DELETED.user_id);
		const entries = await spooled(kv);
		expect(entries).toHaveLength(1);
		expect(entries[0]).toMatchObject({
			kind: 'account_deleted',
			id: `account_deleted:${DELETED.user_id}`,
			ttl: 'never',
			subject: `evt.in.account_deleted.${DELETED.user_id}`,
		});
		expect(activityCountForTests('account_deleted', 'spooled')).toBe(1);
	});

	it('drains the spool in order with the original ids, marks them spooled and drops expired entries', async () => {
		const failing = new FakePublisher();
		failing.failures = [rejection(), rejection()];
		const kv = new MockKVProvider();
		await startActivityEvents({publisher: failing, kv});
		await emitActivity('account_deleted', '1', {user_id: '1'}, workerMeta(), '1');
		await emitActivity('account_deleted', '2', {user_id: '2'}, workerMeta(), '2');
		const stale: ActivitySpoolEntry = {
			kind: 'account_deleted',
			subject: 'evt.in.account_deleted.3',
			id: 'account_deleted:3',
			ttl: 'never',
			at_ms: Date.now() - ACTIVITY_SPOOL_MAX_AGE_MS - 1000,
			data: '{}',
		};
		await kv.rpush(ACTIVITY_SPOOL_KEY, JSON.stringify(stale));
		const draining = new FakePublisher();
		await startActivityEvents({publisher: draining, kv});
		const result = await drainActivitySpoolNow();
		expect(result).toMatchObject({published: 2, expired: 1, failed: 0, remaining: 0});
		expect(draining.calls.map((call) => call.options)).toEqual([
			{msgID: 'account_deleted:1', ttl: 'never', spooled: true},
			{msgID: 'account_deleted:2', ttl: 'never', spooled: true},
		]);
	});

	it('keeps undelivered entries at the head of the spool when a drain publish fails', async () => {
		const kv = new MockKVProvider();
		const failing = new FakePublisher();
		failing.failures = [rejection(), rejection()];
		await startActivityEvents({publisher: failing, kv});
		await emitActivity('account_deleted', '1', {user_id: '1'}, workerMeta(), '1');
		await emitActivity('account_deleted', '2', {user_id: '2'}, workerMeta(), '2');
		const draining = new FakePublisher();
		draining.failures = [new TimeoutError()];
		await startActivityEvents({publisher: draining, kv});
		const result = await drainActivitySpoolNow();
		expect(result).toMatchObject({published: 0, remaining: 2});
		expect((await spooled(kv)).map((entry) => entry.id)).toEqual(['account_deleted:1', 'account_deleted:2']);
	});

	it('stops a drain at its budget inside a batch and never trims after losing the lock', async () => {
		const kv = new MockKVProvider();
		for (const id of ['1', '2', '3']) {
			const entry: ActivitySpoolEntry = {
				kind: 'account_deleted',
				subject: `evt.in.account_deleted.${id}`,
				id: `account_deleted:${id}`,
				ttl: 'never',
				at_ms: Date.now(),
				data: '{}',
			};
			await kv.rpush(ACTIVITY_SPOOL_KEY, JSON.stringify(entry));
		}
		let clock = 0;
		const slow: ActivityPublisher = {
			publish: async () => {
				clock += 600;
			},
		};
		const partial = await drainActivitySpool(slow, kv, {nowMs: () => clock, budgetMs: 1_000});
		expect(partial).toMatchObject({published: 2, remaining: 1});
		const lost = await drainActivitySpool(new FakePublisher(), kv, {keepLock: async () => false});
		expect(lost.published).toBe(1);
		expect(lost.remaining).toBe(1);
		expect((await spooled(kv)).map((entry) => entry.id)).toEqual(['account_deleted:3']);
	});
});
