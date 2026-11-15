// SPDX-License-Identifier: AGPL-3.0-or-later

import {KVCacheProvider} from '@pkgs/cache/src/providers/KVCacheProvider';
import {MockKVProvider} from '@pkgs/kv_client/src/testing/MockKVProvider';
import {describe, expect, it} from 'vitest';

describe('ICacheService.getOrSet', () => {
	it('runs the factory once for concurrent callers on the same key', async () => {
		const cache = new KVCacheProvider({client: new MockKVProvider()});
		let calls = 0;
		const factory = async () => {
			calls += 1;
			return 7;
		};
		const results = await Promise.all([
			cache.getOrSet('key', factory),
			cache.getOrSet('key', factory),
			cache.getOrSet('key', factory),
		]);
		expect(results).toEqual([7, 7, 7]);
		expect(calls).toBe(1);
		await expect(cache.get('key')).resolves.toBe(7);
	});

	it('serves a stored json null from the kv provider as a hit', async () => {
		const client = new MockKVProvider();
		const provider = new KVCacheProvider({client});
		let calls = 0;
		const factory = async () => {
			calls += 1;
			return null;
		};
		await expect(provider.getOrSet<number | null>('key', factory, 60)).resolves.toBeNull();
		expect(await client.get('key')).toBe('null');
		await expect(provider.getOrSet<number | null>('key', factory, 60)).resolves.toBeNull();
		expect(calls).toBe(1);
	});

	it('treats an unparseable stored value as a miss', async () => {
		const client = new MockKVProvider();
		const provider = new KVCacheProvider({client});
		await client.set('key', '{not json');
		await expect(provider.getOrSet('key', async () => 3, 60)).resolves.toBe(3);
		await expect(provider.get('key')).resolves.toBe(3);
	});

	it('resolves the ttl from the produced value', async () => {
		const client = new MockKVProvider();
		const provider = new KVCacheProvider({client});
		const resolver = (value: number | null) => (value === null ? 5 : 30);
		await expect(provider.getOrSet<number | null>('present', async () => 1, resolver)).resolves.toBe(1);
		await expect(provider.getOrSet<number | null>('absent', async () => null, resolver)).resolves.toBeNull();
		expect(await client.ttl('present')).toBeGreaterThan(5);
		expect(await client.ttl('absent')).toBeLessThanOrEqual(5);
		expect(await client.get('absent')).toBe('null');
	});

	it('rejects every waiter after a single coalesced retry when the factory keeps failing', async () => {
		const cache = new KVCacheProvider({client: new MockKVProvider()});
		let calls = 0;
		const failing = async (): Promise<number> => {
			calls += 1;
			throw new Error('factory failed');
		};
		const settled = await Promise.allSettled([
			cache.getOrSet('key', failing),
			cache.getOrSet('key', failing),
			cache.getOrSet('key', failing),
			cache.getOrSet('key', failing),
		]);
		expect(settled.map((result) => result.status)).toEqual(['rejected', 'rejected', 'rejected', 'rejected']);
		expect(calls).toBe(2);
		await expect(cache.exists('key')).resolves.toBe(false);
		await expect(cache.getOrSet('key', async () => 11)).resolves.toBe(11);
	});

	it('does not fan a transient producer failure out to the callers that joined it', async () => {
		const cache = new KVCacheProvider({client: new MockKVProvider()});
		let calls = 0;
		const factory = async () => {
			calls += 1;
			if (calls === 1) {
				throw new Error('transient failure');
			}
			return 11;
		};
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
		expect(calls).toBe(2);
		await expect(cache.get('key')).resolves.toBe(11);
	});
});
