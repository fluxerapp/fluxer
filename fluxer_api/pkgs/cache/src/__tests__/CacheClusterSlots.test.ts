// SPDX-License-Identifier: AGPL-3.0-or-later

import {KVCacheProvider} from '@pkgs/cache/src/providers/KVCacheProvider';
import type {IKVPipeline, IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import {computeHashSlot} from '@pkgs/kv_client/src/KVHashSlots';
import {describe, expect, it} from 'vitest';

function createRecordingProvider(): {
	client: IKVProvider;
	commands: Array<Array<string>>;
} {
	const commands: Array<Array<string>> = [];
	const client = {
		set: async (key: string) => {
			commands.push([key]);
			return 'OK';
		},
		setex: async (key: string) => {
			commands.push([key]);
		},
		isClustered: () => true,
		pipeline: () => {
			const keys: Array<string> = [];
			commands.push(keys);
			const batch = {
				set: (key: string) => {
					keys.push(key);
					return batch;
				},
				setex: (key: string) => {
					keys.push(key);
					return batch;
				},
				exec: async () => [],
			} as unknown as IKVPipeline;
			return batch;
		},
	} as unknown as IKVProvider;
	return {client, commands};
}

describe('KVCacheProvider cluster hash slots', () => {
	it('keeps a multi entry write off batched commands that span hash slots', async () => {
		const {client, commands} = createRecordingProvider();
		const provider = new KVCacheProvider({client});

		expect(computeHashSlot('cache:alpha')).not.toBe(computeHashSlot('cache:beta'));

		await provider.mset([
			{key: 'cache:alpha', value: 1, ttlSeconds: 60},
			{key: 'cache:beta', value: 2},
		]);

		expect(commands.flat().sort()).toEqual(['cache:alpha', 'cache:beta']);
		expect(commands.filter((keys) => new Set(keys.map(computeHashSlot)).size > 1)).toEqual([]);
	});
});
