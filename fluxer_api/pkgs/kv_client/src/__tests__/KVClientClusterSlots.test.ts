// SPDX-License-Identifier: AGPL-3.0-or-later

import {KVClient} from '@pkgs/kv_client/src/KVClient';
import {computeHashSlot} from '@pkgs/kv_client/src/KVHashSlots';
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

function crossSlotCommands(): Array<{name: string; keys: Array<string>}> {
	return commands.filter((command) => new Set(command.keys.map(computeHashSlot)).size > 1);
}

function createClusteredClient(): KVClient {
	return new KVClient({url: 'redis://127.0.0.1:6379', mode: 'cluster'});
}

function createStandaloneClient(): KVClient {
	return new KVClient({url: 'redis://127.0.0.1:6379', mode: 'standalone'});
}

describe('KVClient cluster hash slots', () => {
	beforeEach(() => {
		commands.length = 0;
		store.clear();
	});

	it('reads several keys without a command spanning hash slots', async () => {
		expect(computeHashSlot('slot:alpha')).not.toBe(computeHashSlot('slot:beta'));
		const client = createClusteredClient();
		await client.set('slot:alpha', 'one');

		await expect(client.mget('slot:alpha', 'slot:beta')).resolves.toEqual(['one', null]);
		expect(crossSlotCommands()).toEqual([]);
	});

	it('writes several keys without a command spanning hash slots', async () => {
		expect(computeHashSlot('slot:alpha')).not.toBe(computeHashSlot('slot:beta'));
		const client = createClusteredClient();

		await client.mset('slot:alpha', 'one', 'slot:beta', 'two');

		expect(store.get('slot:alpha')).toBe('one');
		expect(store.get('slot:beta')).toBe('two');
		expect(crossSlotCommands()).toEqual([]);
	});

	it('deletes several keys without a command spanning hash slots', async () => {
		expect(computeHashSlot('slot:alpha')).not.toBe(computeHashSlot('slot:beta'));
		const client = createClusteredClient();
		await client.set('slot:alpha', 'one');
		await client.set('slot:beta', 'two');

		await expect(client.del('slot:alpha', 'slot:beta', 'slot:gamma')).resolves.toBe(2);
		expect(store.size).toBe(0);
		expect(crossSlotCommands()).toEqual([]);
	});

	it('keeps multi key commands whole outside cluster mode', async () => {
		const client = createStandaloneClient();

		await client.mset('slot:alpha', 'one', 'slot:beta', 'two');
		await expect(client.mget('slot:alpha', 'slot:beta')).resolves.toEqual(['one', 'two']);
		await expect(client.del('slot:alpha', 'slot:beta')).resolves.toBe(2);

		expect(commands).toEqual([
			{name: 'mset', keys: ['slot:alpha', 'slot:beta']},
			{name: 'mget', keys: ['slot:alpha', 'slot:beta']},
			{name: 'del', keys: ['slot:alpha', 'slot:beta']},
		]);
	});
});
