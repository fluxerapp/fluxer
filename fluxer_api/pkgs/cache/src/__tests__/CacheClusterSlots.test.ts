// SPDX-License-Identifier: AGPL-3.0-or-later

import {KVCacheProvider} from '@pkgs/cache/src/providers/KVCacheProvider';
import type {IKVPipeline, IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import {describe, expect, it} from 'vitest';

function hashSlot(key: string): number {
	let hashed = key;
	const start = key.indexOf('{');
	if (start !== -1) {
		const end = key.indexOf('}', start + 1);
		if (end > start + 1) {
			hashed = key.slice(start + 1, end);
		}
	}
	let crc = 0;
	for (let index = 0; index < hashed.length; index += 1) {
		crc ^= (hashed.charCodeAt(index) & 0xff) << 8;
		for (let bit = 0; bit < 8; bit += 1) {
			crc = (crc & 0x8000) === 0 ? (crc << 1) & 0xffff : ((crc << 1) ^ 0x1021) & 0xffff;
		}
	}
	return crc % 16384;
}

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

		expect(hashSlot('cache:alpha')).not.toBe(hashSlot('cache:beta'));

		await provider.mset([
			{key: 'cache:alpha', value: 1, ttlSeconds: 60},
			{key: 'cache:beta', value: 2},
		]);

		expect(commands.flat().sort()).toEqual(['cache:alpha', 'cache:beta']);
		expect(commands.filter((keys) => new Set(keys.map(hashSlot)).size > 1)).toEqual([]);
	});
});
