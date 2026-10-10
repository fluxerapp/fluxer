// SPDX-License-Identifier: AGPL-3.0-or-later

import {emitAccountChangedIfRelevant} from '@app/api/infrastructure/activity/AccountChangeEvents';
import {
	activityCountForTests,
	drainActivitySpoolNow,
	emitActivity,
	idleActivityEvents,
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
import {MockKVProvider} from '@app/api/test/mocks/MockKVProvider';
import {JetStreamApiError} from '@nats-io/jetstream';
import {TimeoutError} from '@nats-io/transport-node';
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

const LOGIN = {user_id: '1174109840998400001', ok: true, failure: null, mfa: false, new_ip: false};
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

	it('publishes a fact with its natural id, ttl and envelope, and resolves after the ack', async () => {
		const publisher = new FakePublisher();
		const kv = new MockKVProvider();
		await startActivityEvents({publisher, kv});
		await emitActivity('account_deleted', DELETED.user_id, DELETED, workerMeta(), DELETED.user_id);
		expect(publisher.calls).toHaveLength(1);
		const [call] = publisher.calls;
		expect(call.subject).toBe(`evt.in.account_deleted.${DELETED.user_id}`);
		expect(call.options).toEqual({msgID: `account_deleted:${DELETED.user_id}`, ttl: 'never', spooled: false});
		const envelope = JSON.parse(call.payload);
		expect(envelope).toMatchObject({
			v: 1,
			id: `account_deleted:${DELETED.user_id}`,
			key: DELETED.user_id,
			kind: 'account_deleted',
			data: DELETED,
			meta: {channel: 'worker'},
		});
		expect(typeof envelope.at_ms).toBe('number');
		expect(await spooled(kv)).toEqual([]);
		expect(activityCountForTests('account_deleted', 'published')).toBe(1);
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

	it('spools a rejected publish without retrying and counts it as rejected', async () => {
		const publisher = new FakePublisher();
		publisher.failures = [rejection()];
		const kv = new MockKVProvider();
		await startActivityEvents({publisher, kv});
		await emitActivity('account_deleted', DELETED.user_id, DELETED, workerMeta(), DELETED.user_id);
		expect(publisher.calls).toHaveLength(1);
		expect(await spooled(kv)).toHaveLength(1);
		expect(activityCountForTests('account_deleted', 'rejected')).toBe(1);
	});

	it('queues signals without blocking the caller and gives each a fresh id', async () => {
		const publisher = new FakePublisher();
		const kv = new MockKVProvider();
		await startActivityEvents({publisher, kv});
		void emitActivity('login', LOGIN.user_id, LOGIN, workerMeta());
		void emitActivity('login', LOGIN.user_id, LOGIN, workerMeta());
		await new Promise((resolve) => setImmediate(resolve));
		await idleActivityEvents();
		expect(publisher.calls).toHaveLength(2);
		const ids = publisher.calls.map((call) => call.options.msgID);
		expect(ids[0]).toMatch(/^login:\d+$/);
		expect(ids[0]).not.toBe(ids[1]);
		expect(publisher.calls[0].options.ttl).toBe('3024000');
	});

	it('keys a block by the blocked user so it lands with their other signals', async () => {
		const publisher = new FakePublisher();
		const kv = new MockKVProvider();
		await startActivityEvents({publisher, kv});
		const block = {blocker_id: '1174110260428800002', blocked_id: LOGIN.user_id};
		void emitActivity('user_blocked', block.blocked_id, block, workerMeta());
		await new Promise((resolve) => setImmediate(resolve));
		await idleActivityEvents();
		expect(publisher.calls).toHaveLength(1);
		const [call] = publisher.calls;
		expect(call.subject).toBe(`evt.in.user_blocked.${LOGIN.user_id}`);
		expect(call.options.ttl).toBe('259200');
		expect(JSON.parse(call.payload)).toMatchObject({kind: 'user_blocked', key: LOGIN.user_id, data: block});
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

	it('gives two account changes written at the same version distinct ids', async () => {
		const publisher = new FakePublisher();
		await startActivityEvents({publisher, kv: new MockKVProvider()});
		const row = (flags: bigint) => ({user_id: 1174109840998400001n, version: 11, flags}) as never;
		await emitAccountChangedIfRelevant(row(0n), row(1n << 50n), 'admin');
		await emitAccountChangedIfRelevant(row(0n), row(1n << 43n), 'admin');
		await emitAccountChangedIfRelevant(row(0n), row(1n << 50n), 'admin');
		const ids = publisher.calls.map((call) => call.options.msgID);
		expect(ids).toHaveLength(3);
		expect(ids[0]).toMatch(/^account_changed:1174109840998400001:11:/u);
		expect(ids[0]).not.toBe(ids[1]);
		expect(ids[0]).toBe(ids[2]);
	});

	it('stays silent while the activity stream does not exist', async () => {
		const publisher = new FakePublisher();
		const kv = new MockKVProvider();
		await startActivityEvents({
			publisher,
			kv,
			jsm: {
				streams: {
					info: async () => {
						throw new JetStreamApiError({code: 404, err_code: 10059, description: 'stream not found'});
					},
				},
			} as never,
		});
		await emitActivity('account_deleted', DELETED.user_id, DELETED, workerMeta(), DELETED.user_id);
		expect(publisher.calls).toEqual([]);
		expect(await spooled(kv)).toEqual([]);
		expect(activityCountForTests('account_deleted', 'dropped')).toBe(1);
	});

	it('spools and counts rejected while the stream is missing when asked to', async () => {
		const publisher = new FakePublisher();
		const kv = new MockKVProvider();
		await startActivityEvents({
			publisher,
			kv,
			spoolWhileMissing: true,
			jsm: {
				streams: {
					info: async () => {
						throw new JetStreamApiError({code: 404, err_code: 10059, description: 'stream not found'});
					},
				},
			} as never,
		});
		await emitActivity('account_deleted', DELETED.user_id, DELETED, workerMeta(), DELETED.user_id);
		expect(publisher.calls).toEqual([]);
		expect((await spooled(kv)).map((entry) => entry.id)).toEqual([`account_deleted:${DELETED.user_id}`]);
		expect(activityCountForTests('account_deleted', 'rejected')).toBe(1);
		expect(await drainActivitySpoolNow()).toBeNull();
	});

	it('keeps publishing when the stream probe fails for a transient reason', async () => {
		const publisher = new FakePublisher();
		const kv = new MockKVProvider();
		await startActivityEvents({
			publisher,
			kv,
			jsm: {
				streams: {
					info: async () => {
						throw new TimeoutError();
					},
				},
			} as never,
		});
		await emitActivity('account_deleted', DELETED.user_id, DELETED, workerMeta(), DELETED.user_id);
		expect(publisher.calls.map((call) => call.options.msgID)).toEqual([`account_deleted:${DELETED.user_id}`]);
		expect(activityCountForTests('account_deleted', 'published')).toBe(1);
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
