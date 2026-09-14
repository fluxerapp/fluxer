// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IpInfoCache} from '@pkgs/geoip/src/IpInfoService';
import {createTieredIpInfoCache} from '@pkgs/geoip/src/TieredIpInfoCache';
import {describe, expect, it} from 'vitest';

interface RecordedSet {
	key: string;
	value: unknown;
	ttlSeconds: number | undefined;
}

interface RecordingCache {
	cache: IpInfoCache;
	store: Map<string, unknown>;
	sets: Array<RecordedSet>;
}

function createRecordingCache(): RecordingCache {
	const store = new Map<string, unknown>();
	const sets: Array<RecordedSet> = [];
	return {
		store,
		sets,
		cache: {
			async get<T>(key: string): Promise<T | null> {
				return (store.get(key) as T | undefined) ?? null;
			},
			async set<T>(key: string, value: T, ttlSeconds?: number): Promise<void> {
				store.set(key, value);
				sets.push({key, value, ttlSeconds});
			},
		},
	};
}

describe('TieredIpInfoCache', () => {
	it('clamps the hot TTL to the requested TTL and passes the raw TTL to the cold tier', async () => {
		const hot = createRecordingCache();
		const cold = createRecordingCache();
		const tiered = createTieredIpInfoCache({hot: hot.cache, cold: cold.cache});

		await tiered.set('a', {available: false}, 60);

		expect(hot.sets).toEqual([{key: 'a', value: {available: false}, ttlSeconds: 60}]);
		expect(cold.sets).toEqual([{key: 'a', value: {available: false}, ttlSeconds: 60}]);
	});

	it('caps the hot TTL at the configured hot window', async () => {
		const hot = createRecordingCache();
		const cold = createRecordingCache();
		const tiered = createTieredIpInfoCache({hot: hot.cache, cold: cold.cache});

		await tiered.set('a', {available: true}, 100000);

		expect(hot.sets[0]?.ttlSeconds).toBe(600);
		expect(cold.sets[0]?.ttlSeconds).toBe(100000);
	});

	it('uses the hot window when no TTL is supplied', async () => {
		const hot = createRecordingCache();
		const cold = createRecordingCache();
		const tiered = createTieredIpInfoCache({hot: hot.cache, cold: cold.cache});

		await tiered.set('a', {available: true});

		expect(hot.sets[0]?.ttlSeconds).toBe(600);
		expect(cold.sets[0]?.ttlSeconds).toBeUndefined();
	});

	it('skips the cold write when skipColdWrite matches', async () => {
		const hot = createRecordingCache();
		const cold = createRecordingCache();
		const tiered = createTieredIpInfoCache({
			hot: hot.cache,
			cold: cold.cache,
			skipColdWrite: (value) => (value as {available?: unknown}).available === false,
		});

		await tiered.set('a', {available: false}, 60);
		await tiered.set('b', {available: true}, 60);

		expect(hot.sets.map((entry) => entry.key)).toEqual(['a', 'b']);
		expect(cold.sets.map((entry) => entry.key)).toEqual(['b']);
	});

	it('promotes a cold hit into the hot tier', async () => {
		const hot = createRecordingCache();
		const cold = createRecordingCache();
		cold.store.set('a', {available: true});
		const tiered = createTieredIpInfoCache({hot: hot.cache, cold: cold.cache});

		const hit = await tiered.get('a');

		expect(hit).toEqual({available: true});
		expect(hot.sets).toEqual([{key: 'a', value: {available: true}, ttlSeconds: 600}]);
	});

	it('never promotes a cold hit that skipColdWrite matches', async () => {
		const hot = createRecordingCache();
		const cold = createRecordingCache();
		cold.store.set('a', {available: false});
		const tiered = createTieredIpInfoCache({
			hot: hot.cache,
			cold: cold.cache,
			skipColdWrite: (value) => (value as {available?: unknown}).available === false,
		});

		const hit = await tiered.get('a');

		expect(hit).toEqual({available: false});
		expect(hot.sets).toEqual([]);
	});

	it('never writes a zero TTL', async () => {
		const hot = createRecordingCache();
		const cold = createRecordingCache();
		const tiered = createTieredIpInfoCache({hot: hot.cache, cold: cold.cache});

		await tiered.set('a', {available: false}, 60);
		await tiered.set('b', {available: true}, 100000);
		await tiered.set('c', {available: true});
		cold.store.set('d', {available: true});
		await tiered.get('d');

		for (const entry of [...hot.sets, ...cold.sets]) {
			expect(entry.ttlSeconds === undefined || entry.ttlSeconds > 0).toBe(true);
		}
	});
});
