// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChannelID, createGuildID, createUserID} from '@app/api/BrandedTypes';
import {computeLiveKitPublishSources, LiveKitService} from '@app/api/infrastructure/LiveKitService';
import {TrackSource} from 'livekit-server-sdk';
import {describe, expect, it} from 'vitest';

describe('LiveKitService publish permissions', () => {
	it('maps STREAM permission to LiveKit screen-share publish sources', () => {
		expect(computeLiveKitPublishSources({canSpeak: true, canStream: true, canVideo: true})).toEqual([
			TrackSource.MICROPHONE,
			TrackSource.CAMERA,
			TrackSource.SCREEN_SHARE,
			TrackSource.SCREEN_SHARE_AUDIO,
		]);
	});
	it('omits screen-share sources when STREAM is denied', () => {
		expect(computeLiveKitPublishSources({canSpeak: true, canStream: false, canVideo: false})).toEqual([
			TrackSource.MICROPHONE,
		]);
	});
});

class FakeTwirpError extends Error {
	status: number;
	code?: string;
	constructor(message: string, status: number, code?: string) {
		super(message);
		this.name = 'TwirpError';
		this.status = status;
		this.code = code;
	}
}

function createServiceWithRoomServiceClient(roomServiceClient: unknown): LiveKitService {
	const service = Object.create(LiveKitService.prototype) as LiveKitService;
	Reflect.set(
		service,
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
	return service;
}

describe('LiveKitService listParticipants', () => {
	const params = {
		guildId: createGuildID(1n),
		channelId: createChannelID(2n),
		regionId: 'region-1',
		serverId: 'region-1-server-1',
	};

	it('reports a 404 as an unreadable room instead of an empty one', async () => {
		const service = createServiceWithRoomServiceClient({
			listParticipants: async () => {
				throw new FakeTwirpError('not_found', 404, 'not_found');
			},
		});
		const result = await service.listParticipants(params);
		expect(result.status).toBe('error');
	});

	it('reports a bad_route 404 as an unreadable room instead of an empty one', async () => {
		const service = createServiceWithRoomServiceClient({
			listParticipants: async () => {
				throw new FakeTwirpError('invalid path prefix', 404, 'bad_route');
			},
		});
		const result = await service.listParticipants(params);
		expect(result.status).toBe('error');
		expect(result.status === 'error' && result.retryable).toBe(false);
	});

	it('still reports a genuinely empty room as empty', async () => {
		const service = createServiceWithRoomServiceClient({
			listParticipants: async () => [],
		});
		const result = await service.listParticipants(params);
		expect(result).toEqual({status: 'ok', participants: []});
	});
});

describe('LiveKitService updateParticipant mute', () => {
	const params = {
		userId: createUserID(3n),
		guildId: createGuildID(1n),
		channelId: createChannelID(2n),
		connectionId: 'conn-1',
		regionId: 'region-1',
		serverId: 'region-1-server-1',
	};

	function roomServiceClient(
		listedTracks: Array<{sid: string; source: TrackSource}>,
		updatedTracks: typeof listedTracks,
	) {
		const calls: Array<Array<unknown>> = [];
		const client = {
			listParticipants: async () => [{identity: 'user_3_conn-1', tracks: listedTracks}],
			updateParticipant: async (...args: Array<unknown>) => {
				calls.push(['updateParticipant', ...args]);
				return {identity: 'user_3_conn-1', tracks: updatedTracks};
			},
			mutePublishedTrack: async (...args: Array<unknown>) => {
				calls.push(['mutePublishedTrack', ...args]);
			},
		};
		return {client, calls};
	}

	it('records the mute on the participant and mutes every current microphone track', async () => {
		const {client, calls} = roomServiceClient(
			[{sid: 'TR_old', source: TrackSource.MICROPHONE}],
			[
				{sid: 'TR_old', source: TrackSource.MICROPHONE},
				{sid: 'TR_new', source: TrackSource.MICROPHONE},
			],
		);
		const service = createServiceWithRoomServiceClient(client);

		await service.updateParticipant({...params, mute: true});

		expect(calls).toEqual([
			['updateParticipant', 'guild_1_channel_2', 'user_3_conn-1', {attributes: {server_mute: 'true'}}],
			['mutePublishedTrack', 'guild_1_channel_2', 'user_3_conn-1', 'TR_old', true],
			['mutePublishedTrack', 'guild_1_channel_2', 'user_3_conn-1', 'TR_new', true],
		]);
	});

	it('clears the recorded mute on unmute', async () => {
		const {client, calls} = roomServiceClient([], []);
		const service = createServiceWithRoomServiceClient(client);

		await service.updateParticipant({...params, mute: false});

		expect(calls).toEqual([
			['updateParticipant', 'guild_1_channel_2', 'user_3_conn-1', {attributes: {server_mute: ''}}],
		]);
	});
});
