// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import {KVCacheProvider} from '@pkgs/cache/src/providers/KVCacheProvider';
import {MockKVProvider} from '@pkgs/kv_client/src/testing/MockKVProvider';
import {describe, expect, it} from 'vitest';

const INFLIGHT_OVERFLOW_ENTRIES = 10000;
const PRODUCE_TIMEOUT_MS = 50;
const PRODUCE_TIMEOUT_MESSAGE = 'Cache produce timed out';

interface Deferred<T> {
	promise: Promise<T>;
	resolve: (value: T) => void;
	reject: (error: Error) => void;
}

function deferred<T>(): Deferred<T> {
	let resolve!: (value: T) => void;
	let reject!: (error: Error) => void;
	const promise = new Promise<T>((res, rej) => {
		resolve = res;
		reject = rej;
	});
	return {promise, resolve, reject};
}

interface GatedProduce {
	started: Deferred<void>;
	result: Deferred<string>;
}

function gatedFactory(calls: number): {produces: Array<GatedProduce>; factory: () => Promise<string>} {
	const produces = Array.from({length: calls}, () => ({started: deferred<void>(), result: deferred<string>()}));
	let next = 0;
	const factory = async () => {
		const produce = produces[next++];
		produce.started.resolve();
		return await produce.result.promise;
	};
	return {produces, factory};
}

async function fillInflight(cache: ICacheService): Promise<() => Promise<void>> {
	const fillers = Array.from({length: INFLIGHT_OVERFLOW_ENTRIES}, () => gatedFactory(1));
	const filling = fillers.map((filler, index) => cache.getOrSet(`filler:${index}`, filler.factory, 30));
	await Promise.all(fillers.map((filler) => filler.produces[0].started.promise));
	return async () => {
		for (const filler of fillers) {
			filler.produces[0].result.resolve('filler');
		}
		await Promise.all(filling);
	};
}

describe('cache invalidation during an in-flight produce', () => {
	it('does not resurrect a value deleted while the factory was running', async () => {
		const cache = new KVCacheProvider({client: new MockKVProvider()});
		const gate = deferred<string>();
		const pending = cache.getOrSet('session', async () => await gate.promise, 30);
		await cache.delete('session');
		gate.resolve('revoked-session');
		await expect(pending).resolves.toBe('revoked-session');
		expect(await cache.get('session')).toBeNull();
	});

	it('still stores the value when no invalidation happens', async () => {
		const cache = new KVCacheProvider({client: new MockKVProvider()});
		const gate = deferred<string>();
		const pending = cache.getOrSet('session', async () => await gate.promise, 30);
		gate.resolve('live-session');
		await pending;
		expect(await cache.get('session')).toBe('live-session');
	});

	it('does not resurrect a value deleted while a retried produce was running', async () => {
		const cache = new KVCacheProvider({client: new MockKVProvider()});
		const {produces, factory} = gatedFactory(2);
		const producer = cache.getOrSet('session', factory, 30);
		const joiner = cache.getOrSet('session', factory, 30);
		await produces[0].started.promise;
		produces[0].result.reject(new Error('produce failed'));
		await expect(producer).rejects.toThrow('produce failed');
		await produces[1].started.promise;
		await cache.delete('session');
		produces[1].result.resolve('fresh-after-delete');
		await expect(joiner).resolves.toBe('fresh-after-delete');
		expect(await cache.get('session')).toBeNull();
	});

	it('stores the value a retried produce built when no invalidation happens', async () => {
		const cache = new KVCacheProvider({client: new MockKVProvider()});
		const {produces, factory} = gatedFactory(2);
		const producer = cache.getOrSet('session', factory, 30);
		const joiner = cache.getOrSet('session', factory, 30);
		await produces[0].started.promise;
		produces[0].result.reject(new Error('produce failed'));
		await expect(producer).rejects.toThrow('produce failed');
		await produces[1].started.promise;
		produces[1].result.resolve('retried-session');
		await expect(joiner).resolves.toBe('retried-session');
		expect(await cache.get('session')).toBe('retried-session');
	});

	it('stores the value a retried produce built after the first produce was invalidated', async () => {
		const cache = new KVCacheProvider({client: new MockKVProvider()});
		const {produces, factory} = gatedFactory(2);
		const producer = cache.getOrSet('session', factory, 30);
		const joiner = cache.getOrSet('session', factory, 30);
		await produces[0].started.promise;
		await cache.delete('session');
		produces[0].result.reject(new Error('produce failed'));
		await expect(producer).rejects.toThrow('produce failed');
		await produces[1].started.promise;
		produces[1].result.resolve('retried-session');
		await expect(joiner).resolves.toBe('retried-session');
		expect(await cache.get('session')).toBe('retried-session');
	});

	it('does not resurrect a value deleted after a concurrent caller released its produce', async () => {
		const cache = new KVCacheProvider({client: new MockKVProvider()});
		const {produces, factory} = gatedFactory(1);
		const pending = cache.getOrSet('session', factory, 30);
		await produces[0].started.promise;
		await cache.set('session', 'served-from-cache', 30);
		await expect(cache.getOrSet('session', async () => 'unused', 30)).resolves.toBe('served-from-cache');
		await cache.delete('session');
		produces[0].result.resolve('stale-produce');
		await expect(pending).resolves.toBe('stale-produce');
		expect(await cache.get('session')).toBeNull();
	});

	it('does not resurrect a value deleted while a second overflow produce was running', async () => {
		const cache = new KVCacheProvider({client: new MockKVProvider()});
		const drain = await fillInflight(cache);
		const first = deferred<string>();
		const second = deferred<string>();
		const firstProduce = cache.getOrSet('session', async () => await first.promise, 30);
		const secondProduce = cache.getOrSet('session', async () => await second.promise, 30);
		first.resolve('first-produce');
		await expect(firstProduce).resolves.toBe('first-produce');
		await cache.delete('session');
		second.resolve('second-produce');
		await expect(secondProduce).resolves.toBe('second-produce');
		expect(await cache.get('session')).toBeNull();
		await drain();
	});

	it('does not pin a key forever when the factory never settles', async () => {
		const cache = new KVCacheProvider({client: new MockKVProvider()});
		const stuck = deferred<string>();
		const pinned = cache.getOrSet('session', async () => await stuck.promise, 30, PRODUCE_TIMEOUT_MS);
		await expect(pinned).rejects.toThrow(PRODUCE_TIMEOUT_MESSAGE);
		await expect(cache.getOrSet('session', async () => 'recovered', 30, PRODUCE_TIMEOUT_MS)).resolves.toBe('recovered');
		expect(await cache.get('session')).toBe('recovered');
	});

	it('retries once for the joiners when the producer times out', async () => {
		const cache = new KVCacheProvider({client: new MockKVProvider()});
		const {produces, factory} = gatedFactory(2);
		const producer = cache.getOrSet('session', factory, 30, PRODUCE_TIMEOUT_MS);
		const joiner = cache.getOrSet('session', factory, 30, PRODUCE_TIMEOUT_MS);
		await expect(producer).rejects.toThrow(PRODUCE_TIMEOUT_MESSAGE);
		await produces[1].started.promise;
		produces[1].result.resolve('retried-session');
		await expect(joiner).resolves.toBe('retried-session');
		expect(await cache.get('session')).toBe('retried-session');
	});

	it('stores a sibling produce that succeeded after an overflow produce timed out', async () => {
		const cache = new KVCacheProvider({client: new MockKVProvider()});
		const drain = await fillInflight(cache);
		const stuck = deferred<string>();
		const sibling = deferred<string>();
		const abandoned = cache.getOrSet('session', async () => await stuck.promise, 30, PRODUCE_TIMEOUT_MS);
		const succeeding = cache.getOrSet('session', async () => await sibling.promise, 30, PRODUCE_TIMEOUT_MS * 100);
		await expect(abandoned).rejects.toThrow(PRODUCE_TIMEOUT_MESSAGE);
		sibling.resolve('sibling-produce');
		await expect(succeeding).resolves.toBe('sibling-produce');
		expect(await cache.get('session')).toBe('sibling-produce');
		await drain();
	});

	it('keeps a later produce cacheable after an earlier one was invalidated', async () => {
		const cache = new KVCacheProvider({client: new MockKVProvider()});
		const first = deferred<string>();
		const pending = cache.getOrSet('session', async () => await first.promise, 30);
		await cache.delete('session');
		first.resolve('stale');
		await pending;
		expect(await cache.get('session')).toBeNull();
		await cache.getOrSet('session', async () => 'fresh', 30);
		expect(await cache.get('session')).toBe('fresh');
	});
});
