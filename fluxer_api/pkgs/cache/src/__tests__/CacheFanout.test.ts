// SPDX-License-Identifier: AGPL-3.0-or-later

import {KVCacheProvider} from '@pkgs/cache/src/providers/KVCacheProvider';
import type {IKVPipeline, IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import {computeHashSlot} from '@pkgs/kv_client/src/KVHashSlots';
import {describe, expect, it} from 'vitest';

const MAX_CONCURRENT_ROUND_TRIPS = 16;

interface RecordingProvider {
	client: IKVProvider;
	batches: Array<Array<string>>;
	peakInFlight: number;
}

function createRecordingProvider(clustered: boolean): RecordingProvider {
	const recorder: RecordingProvider = {
		client: {} as IKVProvider,
		batches: [],
		peakInFlight: 0,
	};
	let inFlight = 0;
	const trackRoundTrip = async (keys: Array<string>): Promise<void> => {
		recorder.batches.push(keys);
		inFlight += 1;
		recorder.peakInFlight = Math.max(recorder.peakInFlight, inFlight);
		await new Promise((resolve) => setTimeout(resolve, 0));
		inFlight -= 1;
	};
	recorder.client = {
		isClustered: () => clustered,
		set: async (key: string) => {
			await trackRoundTrip([key]);
			return 'OK';
		},
		setex: async (key: string) => {
			await trackRoundTrip([key]);
		},
		pipeline: () => {
			const keys: Array<string> = [];
			const batch = {
				set: (key: string) => {
					keys.push(key);
					return batch;
				},
				setex: (key: string) => {
					keys.push(key);
					return batch;
				},
				exec: async () => {
					await trackRoundTrip(keys);
					return [];
				},
			} as unknown as IKVPipeline;
			return batch;
		},
	} as unknown as IKVProvider;
	return recorder;
}

function createEntries(count: number): Array<{key: string; value: number; ttlSeconds: number}> {
	return Array.from({length: count}, (_unused, index) => ({
		key: `cache:entry:${index}`,
		value: index,
		ttlSeconds: 60,
	}));
}

describe('KVCacheProvider multi entry write fan out', () => {
	it('writes every entry in one round trip outside cluster mode', async () => {
		const recorder = createRecordingProvider(false);
		const provider = new KVCacheProvider({client: recorder.client});

		await provider.mset(createEntries(1000));

		expect(recorder.batches.map((keys) => keys.length)).toEqual([1000]);
		expect(recorder.peakInFlight).toBe(1);
	});

	it('surfaces a failed command inside a batched write', async () => {
		const client = {
			isClustered: () => false,
			pipeline: () => {
				const batch = {
					set: () => batch,
					setex: () => batch,
					exec: async () => [[new Error('write rejected'), null]],
				} as unknown as IKVPipeline;
				return batch;
			},
		} as unknown as IKVProvider;
		const provider = new KVCacheProvider({client});

		await expect(provider.mset(createEntries(2))).rejects.toThrow('write rejected');
	});

	it('bounds concurrent round trips when entries span hash slots', async () => {
		const recorder = createRecordingProvider(true);
		const provider = new KVCacheProvider({client: recorder.client});

		await provider.mset(createEntries(1000));

		expect(recorder.peakInFlight).toBeLessThanOrEqual(MAX_CONCURRENT_ROUND_TRIPS);
		expect(recorder.batches.filter((keys) => new Set(keys.map(computeHashSlot)).size > 1)).toEqual([]);
		expect(recorder.batches.flat().length).toBe(1000);
	});
});
