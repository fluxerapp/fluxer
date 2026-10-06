// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type StorageChangeEvent,
	StorageChangeFeed,
	type StorageChangeSink,
} from '@app/api/infrastructure/StorageChangeFeed';
import {describe, expect, it} from 'vitest';

interface PublishCall {
	subject: string;
	body: string;
	msgID: string;
}

class ScriptedSink implements StorageChangeSink {
	readonly calls: Array<PublishCall> = [];
	closed = false;
	private readonly outcomes: Array<() => Promise<void>>;

	constructor(outcomes: Array<() => Promise<void>> = []) {
		this.outcomes = outcomes;
	}

	async publish(subject: string, body: string, msgID: string): Promise<void> {
		this.calls.push({subject, body, msgID});
		const next = this.outcomes.shift();
		if (next) await next();
	}

	async close(): Promise<void> {
		this.closed = true;
	}
}

function change(key: string, overrides: Partial<StorageChangeEvent> = {}): StorageChangeEvent {
	return {
		bucket: 'fluxer',
		key,
		op: 'put',
		size: 12,
		etag: null,
		contentType: 'image/png',
		at: '2026-09-14T12:00:00.000Z',
		...overrides,
	};
}

describe('StorageChangeFeed', () => {
	it('publishes metadata to the bucket and operation subject with a dedupe id', async () => {
		const sink = new ScriptedSink();
		const feed = new StorageChangeFeed(sink);

		feed.record(change('attachments/1/2/cat.png'));
		feed.record(change('avatars/3/abc', {bucket: 'fluxer.static', op: 'delete', etag: '"abc"'}));
		await feed.idle();

		expect(sink.calls.map(({subject, msgID}) => ({subject, msgID}))).toEqual([
			{subject: 'storage.fluxer.put', msgID: 'fluxer:attachments/1/2/cat.png:2026-09-14T12:00:00.000Z'},
			{subject: 'storage.fluxer_static.delete', msgID: 'fluxer.static:avatars/3/abc:"abc"'},
		]);
		expect(JSON.parse(sink.calls[0].body)).toEqual(change('attachments/1/2/cat.png'));
		expect(feed.stats()).toEqual({queued: 0, published: 2, dropped: 0, failed: 0});
	});

	it('does not publish before the caller has moved on', () => {
		const sink = new ScriptedSink();
		const feed = new StorageChangeFeed(sink);

		feed.record(change('attachments/1/2/cat.png'));

		expect(sink.calls).toEqual([]);
		expect(feed.stats().queued).toBe(1);
	});

	it('drops and counts events once the queue is full', async () => {
		const gate = Promise.withResolvers<void>();
		const sink = new ScriptedSink([() => gate.promise]);
		const feed = new StorageChangeFeed(sink, {capacity: 2, concurrency: 1});

		for (const key of ['a', 'b', 'c', 'd', 'e']) {
			feed.record(change(key));
		}

		expect(feed.stats()).toEqual({queued: 2, published: 0, dropped: 3, failed: 0});
		gate.resolve();
		await feed.idle();
		expect(sink.calls.map(({msgID}) => msgID.split(':')[1])).toEqual(['a', 'b']);
		expect(feed.stats()).toEqual({queued: 0, published: 2, dropped: 3, failed: 0});
	});

	it('retries a failed publish with the same dedupe id', async () => {
		const sink = new ScriptedSink([() => Promise.reject(new Error('no responders'))]);
		const feed = new StorageChangeFeed(sink, {retryDelayMs: 0});

		feed.record(change('attachments/1/2/cat.png'));
		await feed.idle();

		expect(sink.calls).toHaveLength(2);
		expect(sink.calls[0].msgID).toBe(sink.calls[1].msgID);
		expect(feed.stats()).toEqual({queued: 0, published: 1, dropped: 0, failed: 0});
	});

	it('counts an event as failed once its attempts run out and keeps going', async () => {
		const reject = () => Promise.reject(new Error('stream offline'));
		const sink = new ScriptedSink([reject, reject]);
		const feed = new StorageChangeFeed(sink, {maxAttempts: 2, retryDelayMs: 0, concurrency: 1});

		feed.record(change('lost'));
		feed.record(change('kept'));
		await feed.idle();

		expect(sink.calls.map(({msgID}) => msgID.split(':')[1])).toEqual(['lost', 'lost', 'kept']);
		expect(feed.stats()).toEqual({queued: 0, published: 1, dropped: 0, failed: 1});
	});

	it('survives a sink that throws synchronously', async () => {
		const sink: StorageChangeSink = {
			publish: () => {
				throw new Error('NATS connection is not established. Call connect() first.');
			},
			close: async () => undefined,
		};
		const feed = new StorageChangeFeed(sink, {maxAttempts: 1, retryDelayMs: 0});

		expect(() => feed.record(change('attachments/1/2/cat.png'))).not.toThrow();
		await feed.idle();

		expect(feed.stats()).toEqual({queued: 0, published: 0, dropped: 0, failed: 1});
	});

	it('flushes queued events on close, then closes the sink and drops later events', async () => {
		const sink = new ScriptedSink();
		const feed = new StorageChangeFeed(sink);

		feed.record(change('before-close'));
		await feed.close(1000);
		feed.record(change('after-close'));

		expect(sink.calls.map(({msgID}) => msgID.split(':')[1])).toEqual(['before-close']);
		expect(sink.closed).toBe(true);
		expect(feed.stats()).toEqual({queued: 0, published: 1, dropped: 1, failed: 0});
	});

	it('stops waiting for a stuck publish when close runs out of time', async () => {
		const sink = new ScriptedSink([() => new Promise<void>(() => undefined)]);
		const feed = new StorageChangeFeed(sink, {concurrency: 1});

		feed.record(change('stuck'));
		feed.record(change('waiting'));
		await new Promise((resolve) => setImmediate(resolve));
		await feed.close(10);

		expect(sink.calls.map(({msgID}) => msgID.split(':')[1])).toEqual(['stuck']);
		expect(sink.closed).toBe(true);
		expect(feed.stats()).toEqual({queued: 0, published: 0, dropped: 1, failed: 0});
	});
});
