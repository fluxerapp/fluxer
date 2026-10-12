// SPDX-License-Identifier: AGPL-3.0-or-later

import {KVCacheProvider} from '@pkgs/cache/src/providers/KVCacheProvider';
import {MockKVProvider} from '@pkgs/kv_client/src/testing/MockKVProvider';
import {describe, expect, it, vi} from 'vitest';

function delay(ms: number): Promise<void> {
	return new Promise((resolve) => setTimeout(resolve, ms));
}

describe('ICacheService.getOrSet', () => {
	it('runs the factory once for concurrent callers on the same key', async () => {
		const cache = new KVCacheProvider({client: new MockKVProvider()});
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
		const cache = new KVCacheProvider({client: new MockKVProvider()});
		const factory = vi.fn(async () => {
			await delay(10);
			return 1;
		});
		await Promise.all([cache.getOrSet('a', factory), cache.getOrSet('b', factory)]);
		expect(factory).toHaveBeenCalledTimes(2);
	});

	it('caches a null factory result and serves it as a hit', async () => {
		const cache = new KVCacheProvider({client: new MockKVProvider()});
		const factory = vi.fn(async () => null);
		await expect(cache.getOrSet<number | null>('key', factory, 60)).resolves.toBeNull();
		await expect(cache.getOrSet<number | null>('key', factory, 60)).resolves.toBeNull();
		expect(factory).toHaveBeenCalledTimes(1);
	});

	it('serves a stored json null from the kv provider as a hit', async () => {
		const client = new MockKVProvider();
		const provider = new KVCacheProvider({client});
		const factory = vi.fn(async () => null);
		await expect(provider.getOrSet<number | null>('key', factory, 60)).resolves.toBeNull();
		expect(await client.get('key')).toBe('null');
		await expect(provider.getOrSet<number | null>('key', factory, 60)).resolves.toBeNull();
		expect(factory).toHaveBeenCalledTimes(1);
	});

	it('treats an unparseable stored value as a miss', async () => {
		const client = new MockKVProvider();
		const provider = new KVCacheProvider({client});
		await client.set('key', '{not json');
		const factory = vi.fn(async () => 3);
		await expect(provider.getOrSet('key', factory, 60)).resolves.toBe(3);
		expect(factory).toHaveBeenCalledTimes(1);
	});

	it('resolves the ttl from the produced value', async ({onTestFinished}) => {
		const now = vi.spyOn(Date, 'now').mockReturnValue(1_000);
		onTestFinished(() => now.mockRestore());
		const client = new MockKVProvider();
		const provider = new KVCacheProvider({client});
		const resolver = (value: number | null) => (value === null ? 5 : 30);
		await expect(provider.getOrSet<number | null>('present', async () => 1, resolver)).resolves.toBe(1);
		await expect(provider.getOrSet<number | null>('absent', async () => null, resolver)).resolves.toBeNull();
		expect(await client.ttl('present')).toBe(30);
		expect(await client.ttl('absent')).toBe(5);
		expect(await client.get('absent')).toBe('null');
	});

	it('rejects every waiter after a single coalesced retry when the factory keeps failing', async () => {
		const cache = new KVCacheProvider({client: new MockKVProvider()});
		const failing = vi.fn(async () => {
			await delay(10);
			throw new Error('factory failed');
		});
		const settled = await Promise.allSettled([
			cache.getOrSet('key', failing),
			cache.getOrSet('key', failing),
			cache.getOrSet('key', failing),
			cache.getOrSet('key', failing),
		]);
		expect(settled.map((result) => result.status)).toEqual(['rejected', 'rejected', 'rejected', 'rejected']);
		expect(failing).toHaveBeenCalledTimes(2);
		await expect(cache.exists('key')).resolves.toBe(false);
		const succeeding = vi.fn(async () => 11);
		await expect(cache.getOrSet('key', succeeding)).resolves.toBe(11);
		expect(succeeding).toHaveBeenCalledTimes(1);
	});

	it('does not fan a transient producer failure out to the callers that joined it', async () => {
		const cache = new KVCacheProvider({client: new MockKVProvider()});
		let calls = 0;
		const factory = vi.fn(async () => {
			calls += 1;
			const attempt = calls;
			await delay(10);
			if (attempt === 1) {
				throw new Error('transient failure');
			}
			return 11;
		});
		const settled = await Promise.allSettled([
			cache.getOrSet('key', factory),
			cache.getOrSet('key', factory),
			cache.getOrSet('key', factory),
			cache.getOrSet('key', factory),
		]);
		expect(settled.map((result) => result.status)).toEqual(['rejected', 'fulfilled', 'fulfilled', 'fulfilled']);
		expect(settled.filter((result) => result.status === 'fulfilled').map((result) => result.value)).toEqual([
			11, 11, 11,
		]);
		expect(factory).toHaveBeenCalledTimes(2);
		await expect(cache.get('key')).resolves.toBe(11);
	});
});
