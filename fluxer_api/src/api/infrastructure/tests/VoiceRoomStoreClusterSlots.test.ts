// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChannelID, createGuildID} from '@app/api/BrandedTypes';
import {VoiceRoomStore} from '@app/api/infrastructure/VoiceRoomStore';
import {BatchRecordingKVProvider} from '@app/api/test/mocks/BatchRecordingKVProvider';
import {VOICE_OCCUPANCY_REGION_KEY_PREFIX, VOICE_OCCUPANCY_SERVER_KEY_PREFIX} from '@app/api/voice/VoiceConstants';
import {computeHashSlot} from '@pkgs/kv_client/src/KVHashSlots';
import {describe, expect, it} from 'vitest';

describe('VoiceRoomStore cluster hash slots', () => {
	it('keeps occupancy writes off batched commands that span hash slots', async () => {
		const kvClient = new BatchRecordingKVProvider();
		const store = new VoiceRoomStore(kvClient);
		const guildId = createGuildID(1234n);
		const channelId = createChannelID(5678n);
		const regionKey = `${VOICE_OCCUPANCY_REGION_KEY_PREFIX}:us-east`;
		const serverKey = `${VOICE_OCCUPANCY_SERVER_KEY_PREFIX}:us-east:voice-1`;
		const member = 'guild:1234:channel:5678';

		expect(computeHashSlot(regionKey)).not.toBe(computeHashSlot(serverKey));

		await store.pinRoomServer(guildId, channelId, 'us-east', 'voice-1', 'wss://voice-1.example');
		expect(await kvClient.smembers(regionKey)).toEqual([member]);
		expect(await kvClient.smembers(serverKey)).toEqual([member]);

		await store.deleteRoomServer(guildId, channelId);
		expect(await kvClient.smembers(regionKey)).toEqual([]);
		expect(await kvClient.smembers(serverKey)).toEqual([]);

		expect(kvClient.crossSlotBatches()).toEqual([]);
	});
});
