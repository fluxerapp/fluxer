// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IKVPipeline} from '@pkgs/kv_client/src/IKVProvider';
import {describe, expect, it} from 'vitest';
import {createChannelID, createGuildID} from '../../BrandedTypes';
import {MockKVProvider} from '../../test/mocks/MockKVProvider';
import {VOICE_OCCUPANCY_REGION_KEY_PREFIX, VOICE_OCCUPANCY_SERVER_KEY_PREFIX} from '../../voice/VoiceConstants';
import {VoiceRoomStore} from '../VoiceRoomStore';

type BatchMode = 'multi' | 'pipeline';

interface RecordedBatch {
	mode: BatchMode;
	keys: Array<string>;
}

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

class BatchRecordingKVProvider extends MockKVProvider {
	readonly batches: Array<RecordedBatch> = [];

	override pipeline(): IKVPipeline {
		return this.recordBatch('pipeline', super.pipeline());
	}

	override multi(): IKVPipeline {
		return this.recordBatch('multi', super.multi());
	}

	private recordBatch(mode: BatchMode, inner: IKVPipeline): IKVPipeline {
		const batch: RecordedBatch = {mode, keys: []};
		this.batches.push(batch);
		const recorded: IKVPipeline = {
			get: (key) => {
				batch.keys.push(key);
				inner.get(key);
				return recorded;
			},
			set: (key, value) => {
				batch.keys.push(key);
				inner.set(key, value);
				return recorded;
			},
			setex: (key, ttlSeconds, value) => {
				batch.keys.push(key);
				inner.setex(key, ttlSeconds, value);
				return recorded;
			},
			del: (key) => {
				batch.keys.push(key);
				inner.del(key);
				return recorded;
			},
			expire: (key, ttlSeconds) => {
				batch.keys.push(key);
				inner.expire(key, ttlSeconds);
				return recorded;
			},
			sadd: (key, ...members) => {
				batch.keys.push(key);
				inner.sadd(key, ...members);
				return recorded;
			},
			srem: (key, ...members) => {
				batch.keys.push(key);
				inner.srem(key, ...members);
				return recorded;
			},
			zadd: (key, score, value) => {
				batch.keys.push(key);
				inner.zadd(key, score, value);
				return recorded;
			},
			zrem: (key, ...members) => {
				batch.keys.push(key);
				inner.zrem(key, ...members);
				return recorded;
			},
			hgetall: (key) => {
				batch.keys.push(key);
				inner.hgetall(key);
				return recorded;
			},
			mset: (...args) => {
				for (let index = 0; index + 1 < args.length; index += 2) {
					batch.keys.push(args[index]);
				}
				inner.mset(...args);
				return recorded;
			},
			exec: async () => await inner.exec(),
		};
		return recorded;
	}
}

describe('VoiceRoomStore cluster hash slots', () => {
	it('keeps occupancy writes off batched commands that span hash slots', async () => {
		const kvClient = new BatchRecordingKVProvider();
		const store = new VoiceRoomStore(kvClient);
		const guildId = createGuildID(1234n);
		const channelId = createChannelID(5678n);
		const regionKey = `${VOICE_OCCUPANCY_REGION_KEY_PREFIX}:us-east`;
		const serverKey = `${VOICE_OCCUPANCY_SERVER_KEY_PREFIX}:us-east:voice-1`;
		const member = 'guild:1234:channel:5678';

		expect(hashSlot(regionKey)).not.toBe(hashSlot(serverKey));

		await store.pinRoomServer(guildId, channelId, 'us-east', 'voice-1', 'wss://voice-1.example');
		expect(await kvClient.smembers(regionKey)).toEqual([member]);
		expect(await kvClient.smembers(serverKey)).toEqual([member]);

		await store.deleteRoomServer(guildId, channelId);
		expect(await kvClient.smembers(regionKey)).toEqual([]);
		expect(await kvClient.smembers(serverKey)).toEqual([]);

		const crossSlotBatches = kvClient.batches.filter((batch) => new Set(batch.keys.map(hashSlot)).size > 1);
		expect(crossSlotBatches).toEqual([]);
	});
});
