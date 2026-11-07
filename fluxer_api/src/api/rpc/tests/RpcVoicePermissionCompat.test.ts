// SPDX-License-Identifier: AGPL-3.0-or-later

import {LiveKitService} from '@app/api/infrastructure/LiveKitService';
import {RpcService} from '@app/api/rpc/RpcService';
import {VoiceService} from '@app/api/voice/VoiceService';
import {RpcRequest} from '@fluxer/schema/src/domains/rpc/RpcSchemas';
import {TrackSource} from 'livekit-server-sdk';
import {describe, expect, it} from 'vitest';

function createRpcService(participant: {
	attributes: Record<string, string>;
	tracks: Array<{sid: string; source: TrackSource}>;
}) {
	const calls: Array<Array<unknown>> = [];
	const roomServiceClient = {
		listParticipants: async () => [{identity: 'user_3_conn-1', ...participant}],
		updateParticipant: async (...args: Array<unknown>) => {
			calls.push(['updateParticipant', ...args]);
			return {identity: 'user_3_conn-1', tracks: participant.tracks};
		},
		mutePublishedTrack: async (...args: Array<unknown>) => {
			calls.push(['mutePublishedTrack', ...args]);
		},
	};
	const liveKitService = Object.create(LiveKitService.prototype) as LiveKitService;
	Reflect.set(
		liveKitService,
		'serverClients',
		new Map([
			[
				'region-1',
				new Map([
					[
						'region-1-server-1',
						{
							endpoint: 'ws://livekit.test/livekit',
							apiKey: 'test-key',
							apiSecret: 'test-secret',
							isActive: true,
							roomServiceClient,
						},
					],
				]),
			],
		]),
	);
	const voiceRoomStore = {
		getPinnedRoomServer: async () => ({regionId: 'region-1', serverId: 'region-1-server-1'}),
	};
	const voiceService = new VoiceService(
		liveKitService,
		null as never,
		null as never,
		null as never,
		voiceRoomStore as never,
		null as never,
		null as never,
	);
	const rpcService = Object.create(RpcService.prototype) as RpcService;
	Reflect.set(rpcService, 'voiceService', voiceService);
	return {rpcService, calls};
}

const oldGatewayRequest = {
	type: 'voice_update_participant_permissions',
	guild_id: '1',
	channel_id: '2',
	user_id: '3',
	connection_id: 'conn-1',
	can_speak: true,
	can_stream: false,
	can_video: true,
	deaf: false,
};

describe('RpcService voice_update_participant_permissions from an older gateway', () => {
	it('applies a permission change to the connection and keeps a server mute in place', async () => {
		const {rpcService, calls} = createRpcService({
			attributes: {server_mute: 'true'},
			tracks: [
				{sid: 'TR_mic', source: TrackSource.MICROPHONE},
				{sid: 'TR_cam', source: TrackSource.CAMERA},
				{sid: 'TR_screen', source: TrackSource.SCREEN_SHARE},
			],
		});

		const response = await rpcService.handleRpcRequest({
			request: RpcRequest.parse(oldGatewayRequest),
			requestCache: undefined as never,
		});

		expect(response).toEqual({type: 'voice_update_participant_permissions', data: {success: true}});
		expect(calls).toEqual([
			[
				'updateParticipant',
				'guild_1_channel_2',
				'user_3_conn-1',
				undefined,
				{canPublish: true, canSubscribe: true, canPublishSources: [TrackSource.CAMERA]},
			],
			['mutePublishedTrack', 'guild_1_channel_2', 'user_3_conn-1', 'TR_mic', true],
			['mutePublishedTrack', 'guild_1_channel_2', 'user_3_conn-1', 'TR_screen', true],
		]);
	});

	it('stops subscribing and publishing for a server deafened connection', async () => {
		const {rpcService, calls} = createRpcService({attributes: {}, tracks: []});

		await rpcService.handleRpcRequest({
			request: RpcRequest.parse({...oldGatewayRequest, can_stream: true, deaf: true}),
			requestCache: undefined as never,
		});

		expect(calls).toEqual([
			[
				'updateParticipant',
				'guild_1_channel_2',
				'user_3_conn-1',
				undefined,
				{
					canPublish: false,
					canSubscribe: false,
					canPublishSources: [
						TrackSource.MICROPHONE,
						TrackSource.CAMERA,
						TrackSource.SCREEN_SHARE,
						TrackSource.SCREEN_SHARE_AUDIO,
					],
				},
			],
		]);
	});
});
