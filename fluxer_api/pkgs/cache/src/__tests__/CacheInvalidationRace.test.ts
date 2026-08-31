// SPDX-License-Identifier: AGPL-3.0-or-later

import {InMemoryProvider} from '@pkgs/cache/src/providers/InMemoryProvider';
import {describe, expect, it} from 'vitest';

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
