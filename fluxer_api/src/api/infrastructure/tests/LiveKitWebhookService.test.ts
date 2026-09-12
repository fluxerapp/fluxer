// SPDX-License-Identifier: AGPL-3.0-or-later

import type {WebhookEvent} from 'livekit-server-sdk';
import {describe, expect, it, vi} from 'vitest';
import type {ChannelID, GuildID} from '../../BrandedTypes';
import {createChannelID, createGuildID} from '../../BrandedTypes';
import {VoiceTopology} from '../../voice/VoiceTopology';
import type {IGatewayService} from '../IGatewayService';
import type {ILiveKitService} from '../ILiveKitService';
import type {IVoiceRoomStore} from '../IVoiceRoomStore';
import {LiveKitWebhookService} from '../LiveKitWebhookService';

const GUILD_ID = createGuildID(1n);
const CHANNEL_ID = createChannelID(2n);

function roomFinished(name: string): WebhookEvent {
	return {
		event: 'room_finished',
		room: {name, sid: 'RM_test', emptyTimeout: 300, creationTime: 0n},
	} as unknown as WebhookEvent;
}

function harness(pinnedServerId: string | null) {
	const deleteRoomServer = vi.fn(async () => {});
	const disconnectAllVoiceUsersInChannel = vi.fn(async () => ({disconnectedCount: 0}));
	const voiceRoomStore = {
		getPinnedRoomServer: vi.fn(async () => (pinnedServerId ? {regionId: 'eu', serverId: pinnedServerId} : null)),
		deleteRoomServer,
	} as unknown as IVoiceRoomStore;
	const gatewayService = {disconnectAllVoiceUsersInChannel} as unknown as IGatewayService;
	const liveKitService = {} as unknown as ILiveKitService;
	const service = new LiveKitWebhookService(voiceRoomStore, gatewayService, liveKitService, new VoiceTopology());
	return {service, deleteRoomServer, disconnectAllVoiceUsersInChannel};
}

describe('LiveKitWebhookService room_finished', () => {
	it('clears the guild pin without disconnecting anybody', async () => {
		const {service, deleteRoomServer, disconnectAllVoiceUsersInChannel} = harness('eu-1');

		await service.handleRoomFinished(roomFinished(`guild_${GUILD_ID}_channel_${CHANNEL_ID}`), 'unknown-key');

		expect(deleteRoomServer).toHaveBeenCalledTimes(1);
		expect(disconnectAllVoiceUsersInChannel).not.toHaveBeenCalled();
	});

	it('clears the pin even when no server is pinned, and still disconnects nobody', async () => {
		const {service, deleteRoomServer, disconnectAllVoiceUsersInChannel} = harness(null);

		await service.handleRoomFinished(roomFinished(`guild_${GUILD_ID}_channel_${CHANNEL_ID}`), 'unknown-key');

		expect(deleteRoomServer).toHaveBeenCalledTimes(1);
		expect(disconnectAllVoiceUsersInChannel).not.toHaveBeenCalled();
	});

	it('ignores a room name it cannot parse', async () => {
		const {service, deleteRoomServer, disconnectAllVoiceUsersInChannel} = harness('eu-1');

		await service.handleRoomFinished(roomFinished('not_a_voice_room'), 'unknown-key');

		expect(deleteRoomServer).not.toHaveBeenCalled();
		expect(disconnectAllVoiceUsersInChannel).not.toHaveBeenCalled();
	});
});
