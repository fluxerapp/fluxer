// SPDX-License-Identifier: AGPL-3.0-or-later

import {KVClient} from '@pkgs/kv_client/src/KVClient';
import {computeHashSlot} from '@pkgs/kv_client/src/KVHashSlots';
import {beforeEach, describe, expect, it, vi} from 'vitest';

const MAX_CONCURRENT_ROUND_TRIPS = 16;

const {commands, store, tracker} = vi.hoisted(() => ({
	commands: [] as Array<{name: string; keys: Array<string>}>,
	store: new Map<string, string>(),
	tracker: {inFlight: 0, peakInFlight: 0},
}));

vi.mock('ioredis', () => {
	const trackRoundTrip = async (name: string, keys: Array<string>): Promise<void> => {
		commands.push({name, keys});
		tracker.inFlight += 1;
		tracker.peakInFlight = Math.max(tracker.peakInFlight, tracker.inFlight);
		await new Promise((resolve) => setTimeout(resolve, 0));
		tracker.inFlight -= 1;
	};
	class MockRedis {
		async mget(...keys: Array<string>): Promise<Array<string | null>> {
			await trackRoundTrip('mget', keys);
			return keys.map((key) => store.get(key) ?? null);
		}

		async mset(...args: Array<string>): Promise<string> {
			const keys: Array<string> = [];
			for (let index = 0; index + 1 < args.length; index += 2) {
				keys.push(args[index]);
				store.set(args[index], args[index + 1]);
			}
			await trackRoundTrip('mset', keys);
			return 'OK';
		}

		async del(...keys: Array<string>): Promise<number> {
			await trackRoundTrip('del', keys);
			return keys.length;
		}

		async get(key: string): Promise<string | null> {
			await trackRoundTrip('get', [key]);
			return store.get(key) ?? null;
		}

		async set(key: string, value: string): Promise<string> {
			await trackRoundTrip('set', [key]);
			store.set(key, value);
			return 'OK';
		}
	}
	return {default: MockRedis, Cluster: MockRedis};
});

function createKeys(count: number): Array<string> {
	return Array.from({length: count}, (_unused, index) => `fanout:key:${index}`);
}

function crossSlotCommands(): Array<{name: string; keys: Array<string>}> {
	return commands.filter((command) => new Set(command.keys.map(computeHashSlot)).size > 1);
}

describe('KVClient multi key fan out', () => {
	beforeEach(() => {
		commands.length = 0;
		store.clear();
		tracker.inFlight = 0;
		tracker.peakInFlight = 0;
	});

	it('bounds concurrent round trips for a cluster read', async () => {
		const client = new KVClient({url: 'redis://127.0.0.1:6379', mode: 'cluster'});

		const values = await client.mget(...createKeys(1000));

		expect(values.length).toBe(1000);
		expect(tracker.peakInFlight).toBeLessThanOrEqual(MAX_CONCURRENT_ROUND_TRIPS);
		expect(crossSlotCommands()).toEqual([]);
	});

	it('bounds concurrent round trips for a cluster write', async () => {
		const client = new KVClient({url: 'redis://127.0.0.1:6379', mode: 'cluster'});

		await client.mset(...createKeys(1000).flatMap((key) => [key, 'value']));

		expect(tracker.peakInFlight).toBeLessThanOrEqual(MAX_CONCURRENT_ROUND_TRIPS);
		expect(crossSlotCommands()).toEqual([]);
	});

	it('reads a thousand keys in one round trip outside cluster mode', async () => {
		const client = new KVClient({url: 'redis://127.0.0.1:6379', mode: 'standalone'});

		await client.mget(...createKeys(1000));

		expect(commands.map((command) => ({name: command.name, count: command.keys.length}))).toEqual([
			{name: 'mget', count: 1000},
		]);
		expect(tracker.peakInFlight).toBe(1);
	});

	it('keeps values ordered when a cluster read is split by slot', async () => {
		const client = new KVClient({url: 'redis://127.0.0.1:6379', mode: 'cluster'});
		await client.mset('fanout:a', 'one', 'fanout:b', 'two');

		const values = await client.mget('fanout:a', 'fanout:missing', 'fanout:b');

		expect(values).toEqual(['one', null, 'two']);
	});
});
