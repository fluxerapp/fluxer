// SPDX-License-Identifier: AGPL-3.0-or-later

import {KVClient} from '@pkgs/kv_client/src/KVClient';
import {beforeEach, describe, expect, it, vi} from 'vitest';

const {commands, store} = vi.hoisted(() => ({
	commands: [] as Array<{name: string; keys: Array<string>}>,
	store: new Map<string, string>(),
}));

vi.mock('ioredis', () => {
	class MockRedis {
		async get(key: string): Promise<string | null> {
			commands.push({name: 'get', keys: [key]});
			return store.get(key) ?? null;
		}

		async set(key: string, value: string): Promise<string> {
			commands.push({name: 'set', keys: [key]});
			store.set(key, value);
			return 'OK';
		}

		async del(...keys: Array<string>): Promise<number> {
			commands.push({name: 'del', keys});
			return keys.filter((key) => store.delete(key)).length;
		}

		async mget(...keys: Array<string>): Promise<Array<string | null>> {
			commands.push({name: 'mget', keys});
			return keys.map((key) => store.get(key) ?? null);
		}

		async mset(...args: Array<string>): Promise<string> {
			const keys: Array<string> = [];
			for (let index = 0; index + 1 < args.length; index += 2) {
				keys.push(args[index]);
				store.set(args[index], args[index + 1]);
			}
			commands.push({name: 'mset', keys});
			return 'OK';
		}
	}
	return {default: MockRedis, Cluster: MockRedis};
});

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

function crossSlotCommands(): Array<{name: string; keys: Array<string>}> {
	return commands.filter((command) => new Set(command.keys.map(hashSlot)).size > 1);
}

function createClient(): KVClient {
	return new KVClient('redis://127.0.0.1:6379');
}

describe('KVClient cluster hash slots', () => {
	beforeEach(() => {
		commands.length = 0;
		store.clear();
	});

	it('reads several keys without a command spanning hash slots', async () => {
		expect(hashSlot('slot:alpha')).not.toBe(hashSlot('slot:beta'));
		const client = createClient();
		await client.set('slot:alpha', 'one');

		await expect(client.mget('slot:alpha', 'slot:beta')).resolves.toEqual(['one', null]);
		expect(crossSlotCommands()).toEqual([]);
	});

	it('writes several keys without a command spanning hash slots', async () => {
		expect(hashSlot('slot:alpha')).not.toBe(hashSlot('slot:beta'));
		const client = createClient();

		await client.mset('slot:alpha', 'one', 'slot:beta', 'two');

		expect(store.get('slot:alpha')).toBe('one');
		expect(store.get('slot:beta')).toBe('two');
		expect(crossSlotCommands()).toEqual([]);
	});

	it('deletes several keys without a command spanning hash slots', async () => {
		expect(hashSlot('slot:alpha')).not.toBe(hashSlot('slot:beta'));
		const client = createClient();
		await client.set('slot:alpha', 'one');
		await client.set('slot:beta', 'two');

		await expect(client.del('slot:alpha', 'slot:beta', 'slot:gamma')).resolves.toBe(2);
		expect(store.size).toBe(0);
		expect(crossSlotCommands()).toEqual([]);
	});
});
