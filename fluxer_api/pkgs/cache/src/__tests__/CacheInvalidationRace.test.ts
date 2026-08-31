// SPDX-License-Identifier: AGPL-3.0-or-later

import {InMemoryProvider} from '@pkgs/cache/src/providers/InMemoryProvider';
import {describe, expect, it, vi} from 'vitest';

const INFLIGHT_OVERFLOW_ENTRIES = 10000;
const PRODUCE_TIMEOUT_MS = 50;
const PRODUCE_TIMEOUT_MESSAGE = 'Cache produce timed out';

function deferred<T>(): {promise: Promise<T>; resolve: (value: T) => void; reject: (error: Error) => void} {
	let resolve!: (value: T) => void;
	let reject!: (error: Error) => void;
	const promise = new Promise<T>((res, rej) => {
		resolve = res;
		reject = rej;
	});
	return {promise, resolve, reject};
}

function flush(): Promise<void> {
	return new Promise((resolve) => setTimeout(resolve, 0));
}

function settleWithin<T>(pending: Promise<T>, ms: number): Promise<T | 'pinned' | 'rejected'> {
	return Promise.race([
		pending.then(
			(value) => value,
			() => 'rejected' as const,
		),
		new Promise<'pinned'>((resolve) => setTimeout(() => resolve('pinned'), ms)),
	]);
}

function trackedProduceKeys(cache: InMemoryProvider): Array<string> {
	return [...(cache as unknown as {produceInvalidations: Map<string, unknown>}).produceInvalidations.keys()];
}

describe('cache invalidation during an in-flight produce', () => {
	it('does not resurrect a value deleted while the factory was running', async () => {
		const cache = new InMemoryProvider();
		const gate = deferred<string>();
		const pending = cache.getOrSet('session', async () => await gate.promise, 30);
		await cache.delete('session');
		gate.resolve('revoked-session');
		await expect(pending).resolves.toBe('revoked-session');
		expect(await cache.get('session')).toBeNull();
	});

	it('still stores the value when no invalidation happens', async () => {
		const cache = new InMemoryProvider();
		const gate = deferred<string>();
		const pending = cache.getOrSet('session', async () => await gate.promise, 30);
		gate.resolve('live-session');
		await pending;
		expect(await cache.get('session')).toBe('live-session');
	});

	it('does not resurrect a value deleted while a retried produce was running', async () => {
		const cache = new InMemoryProvider();
		const gates: Array<ReturnType<typeof deferred<string>>> = [];
		const factory = async () => {
			const gate = deferred<string>();
			gates.push(gate);
			return await gate.promise;
		};
		const producer = cache.getOrSet('session', factory, 30);
		const joiner = cache.getOrSet('session', factory, 30);
		await flush();
		gates[0].reject(new Error('produce failed'));
		await expect(producer).rejects.toThrow('produce failed');
		await flush();
		expect(gates).toHaveLength(2);
		await cache.delete('session');
		gates[1].resolve('fresh-after-delete');
		await expect(joiner).resolves.toBe('fresh-after-delete');
		expect(await cache.get('session')).toBeNull();
	});

	it('stores the value a retried produce built when no invalidation happens', async () => {
		const cache = new InMemoryProvider();
		const gates: Array<ReturnType<typeof deferred<string>>> = [];
		const factory = async () => {
			const gate = deferred<string>();
			gates.push(gate);
			return await gate.promise;
		};
		const producer = cache.getOrSet('session', factory, 30);
		const joiner = cache.getOrSet('session', factory, 30);
		await flush();
		gates[0].reject(new Error('produce failed'));
		await expect(producer).rejects.toThrow('produce failed');
		await flush();
		gates[1].resolve('retried-session');
		await expect(joiner).resolves.toBe('retried-session');
		expect(await cache.get('session')).toBe('retried-session');
	});

	it('stores the value a retried produce built after the first produce was invalidated', async () => {
		const cache = new InMemoryProvider();
		const gates: Array<ReturnType<typeof deferred<string>>> = [];
		const factory = async () => {
			const gate = deferred<string>();
			gates.push(gate);
			return await gate.promise;
		};
		const producer = cache.getOrSet('session', factory, 30);
		const joiner = cache.getOrSet('session', factory, 30);
		await flush();
		await cache.delete('session');
		gates[0].reject(new Error('produce failed'));
		await expect(producer).rejects.toThrow('produce failed');
		await flush();
		expect(gates).toHaveLength(2);
		gates[1].resolve('retried-session');
		await expect(joiner).resolves.toBe('retried-session');
		expect(await cache.get('session')).toBe('retried-session');
	});

	it('does not resurrect a value deleted after a concurrent caller released its produce', async () => {
		const cache = new InMemoryProvider();
		const gate = deferred<string>();
		const pending = cache.getOrSet('session', async () => await gate.promise, 30);
		await flush();
		await cache.set('session', 'served-from-cache', 30);
		await expect(cache.getOrSet('session', async () => 'unused', 30)).resolves.toBe('served-from-cache');
		await cache.delete('session');
		gate.resolve('stale-produce');
		await expect(pending).resolves.toBe('stale-produce');
		expect(await cache.get('session')).toBeNull();
	});

	it('does not resurrect a value deleted while a second overflow produce was running', async () => {
		const cache = new InMemoryProvider();
		const fillers: Array<ReturnType<typeof deferred<string>>> = [];
		const filling: Array<Promise<string>> = [];
		for (let index = 0; index < INFLIGHT_OVERFLOW_ENTRIES; index++) {
			const gate = deferred<string>();
			fillers.push(gate);
			filling.push(cache.getOrSet(`filler:${index}`, async () => await gate.promise, 30));
		}
		await flush();
		const first = deferred<string>();
		const second = deferred<string>();
		const firstProduce = cache.getOrSet('session', async () => await first.promise, 30);
		const secondProduce = cache.getOrSet('session', async () => await second.promise, 30);
		await flush();
		first.resolve('first-produce');
		await expect(firstProduce).resolves.toBe('first-produce');
		await cache.delete('session');
		second.resolve('second-produce');
		await expect(secondProduce).resolves.toBe('second-produce');
		expect(await cache.get('session')).toBeNull();
		for (const gate of fillers) {
			gate.resolve('filler');
		}
		await Promise.all(filling);
	});

	it('drops produce tracking once the last produce for a key settles', async () => {
		const cache = new InMemoryProvider();
		for (let index = 0; index < 50; index++) {
			const gate = deferred<string>();
			const pending = cache.getOrSet(`session:${index}`, async () => await gate.promise, 30);
			await cache.delete(`session:${index}`);
			gate.resolve('value');
			await pending;
		}
		const shared = deferred<string>();
		const producer = cache.getOrSet('shared', async () => await shared.promise, 30);
		const joiner = cache.getOrSet('shared', async () => 'unused', 30);
		await flush();
		await cache.delete('shared');
		shared.resolve('shared-value');
		await Promise.all([producer, joiner]);
		const failing = cache.getOrSet(
			'failing',
			async () => {
				throw new Error('produce failed');
			},
			30,
		);
		await expect(failing).rejects.toThrow('produce failed');
		expect(trackedProduceKeys(cache)).toEqual([]);
	});

	it('does not pin a key forever when the factory never settles', async () => {
		const cache = new InMemoryProvider();
		const stuck = deferred<string>();
		const pinned = cache.getOrSet('session', async () => await stuck.promise, 30, PRODUCE_TIMEOUT_MS);
		await expect(settleWithin(pinned, 500)).resolves.toBe('rejected');
		await expect(pinned).rejects.toThrow(PRODUCE_TIMEOUT_MESSAGE);
		const recovered = cache.getOrSet('session', async () => 'recovered', 30, PRODUCE_TIMEOUT_MS);
		await expect(settleWithin(recovered, 500)).resolves.toBe('recovered');
		stuck.resolve('never-settled');
		await flush();
		expect(await cache.get('session')).toBe('recovered');
	});

	it('does not store a value produced by a factory that settled after the timeout', async () => {
		const cache = new InMemoryProvider();
		const stuck = deferred<string>();
		const pending = cache.getOrSet('session', async () => await stuck.promise, 30, PRODUCE_TIMEOUT_MS);
		await expect(pending).rejects.toThrow(PRODUCE_TIMEOUT_MESSAGE);
		stuck.resolve('late-produce');
		await flush();
		expect(await cache.get('session')).toBeNull();
		expect(trackedProduceKeys(cache)).toEqual([]);
	});

	it('retries once for the joiners when the producer times out', async () => {
		const cache = new InMemoryProvider();
		const gates: Array<ReturnType<typeof deferred<string>>> = [];
		const factory = async () => {
			const gate = deferred<string>();
			gates.push(gate);
			return await gate.promise;
		};
		const producer = cache.getOrSet('session', factory, 30, PRODUCE_TIMEOUT_MS);
		const joiner = cache.getOrSet('session', factory, 30, PRODUCE_TIMEOUT_MS);
		await expect(producer).rejects.toThrow(PRODUCE_TIMEOUT_MESSAGE);
		await flush();
		expect(gates).toHaveLength(2);
		gates[1].resolve('retried-session');
		await expect(joiner).resolves.toBe('retried-session');
		expect(await cache.get('session')).toBe('retried-session');
		gates[0].resolve('abandoned-produce');
		await flush();
		expect(await cache.get('session')).toBe('retried-session');
	});

	it('releases produce tracking when the factory never settles', async () => {
		const cache = new InMemoryProvider();
		const stuck = deferred<string>();
		const pending = cache.getOrSet('session', async () => await stuck.promise, 30, PRODUCE_TIMEOUT_MS);
		await expect(pending).rejects.toThrow(PRODUCE_TIMEOUT_MESSAGE);
		await flush();
		expect(trackedProduceKeys(cache)).toEqual([]);
	});

	it('stores a sibling produce that succeeded after an overflow produce timed out', async () => {
		const cache = new InMemoryProvider();
		const fillers: Array<ReturnType<typeof deferred<string>>> = [];
		const filling: Array<Promise<string>> = [];
		for (let index = 0; index < INFLIGHT_OVERFLOW_ENTRIES; index++) {
			const gate = deferred<string>();
			fillers.push(gate);
			filling.push(cache.getOrSet(`filler:${index}`, async () => await gate.promise, 30));
		}
		await flush();
		const stuck = deferred<string>();
		const sibling = deferred<string>();
		const abandoned = cache.getOrSet('session', async () => await stuck.promise, 30, PRODUCE_TIMEOUT_MS);
		const succeeding = cache.getOrSet('session', async () => await sibling.promise, 30, PRODUCE_TIMEOUT_MS * 100);
		await expect(abandoned).rejects.toThrow(PRODUCE_TIMEOUT_MESSAGE);
		sibling.resolve('sibling-produce');
		await expect(succeeding).resolves.toBe('sibling-produce');
		expect(await cache.get('session')).toBe('sibling-produce');
		for (const gate of fillers) {
			gate.resolve('filler');
		}
		await Promise.all(filling);
	});

	it('does not hold the event loop open while a produce is in flight', async () => {
		const cache = new InMemoryProvider();
		const stuck = deferred<string>();
		const timers: Array<NodeJS.Timeout> = [];
		const scheduled = globalThis.setTimeout;
		const spy = vi.spyOn(globalThis, 'setTimeout').mockImplementation(((handler: () => void, ms?: number) => {
			const timer = scheduled(handler, ms);
			if (ms === PRODUCE_TIMEOUT_MS) {
				timers.push(timer);
			}
			return timer;
		}) as typeof globalThis.setTimeout);
		const pending = cache.getOrSet('session', async () => await stuck.promise, 30, PRODUCE_TIMEOUT_MS);
		await flush();
		spy.mockRestore();
		expect(timers).toHaveLength(1);
		expect(timers[0].hasRef()).toBe(false);
		await expect(pending).rejects.toThrow(PRODUCE_TIMEOUT_MESSAGE);
	});

	it('keeps a later produce cacheable after an earlier one was invalidated', async () => {
		const cache = new InMemoryProvider();
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
