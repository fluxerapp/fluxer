// SPDX-License-Identifier: AGPL-3.0-or-later

import {InMemoryProvider} from '@pkgs/cache/src/providers/InMemoryProvider';
import {KVCacheProvider} from '@pkgs/cache/src/providers/KVCacheProvider';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import {describe, expect, it, vi} from 'vitest';

function createKVCacheProvider(): {provider: KVCacheProvider; store: Map<string, string>} {
	const store = new Map<string, string>();
	const client = {
		get: async (key: string) => store.get(key) ?? null,
		set: async (key: string, value: string) => {
			store.set(key, value);
			return 'OK';
		},
		setex: async (key: string, _ttlSeconds: number, value: string) => {
			store.set(key, value);
		},
	} as unknown as IKVProvider;
	return {provider: new KVCacheProvider({client}), store};
}

function delay(ms: number): Promise<void> {
	return new Promise((resolve) => setTimeout(resolve, ms));
}

describe('ICacheService.getOrSet', () => {
	it('runs the factory once for concurrent callers on the same key', async () => {
		const cache = new InMemoryProvider();
		const factory = vi.fn(async () => {
			await delay(10);
			return 7;
		});
		const results = await Promise.all([
			cache.getOrSet('key', factory),
			cache.getOrSet('key', factory),
			cache.getOrSet('key', factory),
		]);
		expect(results).toEqual([7, 7, 7]);
		expect(factory).toHaveBeenCalledTimes(1);
		await expect(cache.get('key')).resolves.toBe(7);
	});

	it('does not coalesce concurrent callers on different keys', async () => {
		const cache = new InMemoryProvider();
		const factory = vi.fn(async () => {
			await delay(10);
			return 1;
		});
		await Promise.all([cache.getOrSet('a', factory), cache.getOrSet('b', factory)]);
		expect(factory).toHaveBeenCalledTimes(2);
	});

	it('caches a null factory result and serves it as a hit', async () => {
		const cache = new InMemoryProvider();
		const factory = vi.fn(async () => null);
		await expect(cache.getOrSet<number | null>('key', factory, 60)).resolves.toBeNull();
		await expect(cache.getOrSet<number | null>('key', factory, 60)).resolves.toBeNull();
		expect(factory).toHaveBeenCalledTimes(1);
	});

	it('serves a stored json null from the kv provider as a hit', async () => {
		const {provider, store} = createKVCacheProvider();
		const factory = vi.fn(async () => null);
		await expect(provider.getOrSet<number | null>('key', factory, 60)).resolves.toBeNull();
		expect(store.get('key')).toBe('null');
		await expect(provider.getOrSet<number | null>('key', factory, 60)).resolves.toBeNull();
		expect(factory).toHaveBeenCalledTimes(1);
	});

	it('treats an unparseable stored value as a miss', async () => {
		const {provider, store} = createKVCacheProvider();
		store.set('key', '{not json');
		const factory = vi.fn(async () => 3);
		await expect(provider.getOrSet('key', factory, 60)).resolves.toBe(3);
		expect(factory).toHaveBeenCalledTimes(1);
	});

	it('rejects every waiter and retries on the next call when the factory fails', async () => {
		const cache = new InMemoryProvider();
		const failing = vi.fn(async () => {
			await delay(10);
			throw new Error('factory failed');
		});
		const settled = await Promise.allSettled([cache.getOrSet('key', failing), cache.getOrSet('key', failing)]);
		expect(settled.map((result) => result.status)).toEqual(['rejected', 'rejected']);
		expect(failing).toHaveBeenCalledTimes(1);
		await expect(cache.exists('key')).resolves.toBe(false);
		const succeeding = vi.fn(async () => 11);
		await expect(cache.getOrSet('key', succeeding)).resolves.toBe(11);
		expect(succeeding).toHaveBeenCalledTimes(1);
	});
});
