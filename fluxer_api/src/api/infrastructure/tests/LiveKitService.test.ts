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

	const allSources = [
		TrackSource.MICROPHONE,
		TrackSource.CAMERA,
		TrackSource.SCREEN_SHARE,
		TrackSource.SCREEN_SHARE_AUDIO,
	];

	function roomServiceClient(participant: {
		attributes: Record<string, string>;
		permission?: {canSubscribe: boolean; canPublishSources: Array<TrackSource>};
		tracks: Array<{sid: string; source: TrackSource}>;
		updatedTracks?: Array<{sid: string; source: TrackSource}>;
	}) {
		const calls: Array<Array<unknown>> = [];
		const client = {
			listParticipants: async () => [{identity: 'user_3_conn-1', ...participant}],
			updateParticipant: async (...args: Array<unknown>) => {
				calls.push(['updateParticipant', ...args]);
				return {identity: 'user_3_conn-1', tracks: participant.updatedTracks ?? participant.tracks};
			},
			mutePublishedTrack: async (...args: Array<unknown>) => {
				calls.push(['mutePublishedTrack', ...args]);
			},
		};
		return {client, calls};
	}

	it('records the mute, revokes the microphone, then mutes every remaining microphone track', async () => {
		const {client, calls} = roomServiceClient({
			attributes: {},
			tracks: [{sid: 'TR_old', source: TrackSource.MICROPHONE}],
			updatedTracks: [
				{sid: 'TR_old', source: TrackSource.MICROPHONE},
				{sid: 'TR_new', source: TrackSource.MICROPHONE},
			],
		});
		const service = createServiceWithRoomServiceClient(client);

		await service.updateParticipant({
			...params,
			mute: true,
			deaf: false,
			canSpeak: true,
			canStream: true,
			canVideo: true,
		});

		expect(calls).toEqual([
			['updateParticipant', 'guild_1_channel_2', 'user_3_conn-1', {attributes: {server_mute: 'true'}}],
			[
				'updateParticipant',
				'guild_1_channel_2',
				'user_3_conn-1',
				undefined,
				{
					canPublish: true,
					canSubscribe: true,
					canPublishSources: [TrackSource.CAMERA, TrackSource.SCREEN_SHARE, TrackSource.SCREEN_SHARE_AUDIO],
				},
			],
			['mutePublishedTrack', 'guild_1_channel_2', 'user_3_conn-1', 'TR_old', true],
			['mutePublishedTrack', 'guild_1_channel_2', 'user_3_conn-1', 'TR_new', true],
		]);
	});

	it('revokes the microphone even when a track mute fails', async () => {
		const {client, calls} = roomServiceClient({
			attributes: {},
			tracks: [
				{sid: 'TR_cam', source: TrackSource.CAMERA},
				{sid: 'TR_mic', source: TrackSource.MICROPHONE},
			],
		});
		const service = createServiceWithRoomServiceClient({
			...client,
			mutePublishedTrack: async (...args: Array<unknown>) => {
				calls.push(['mutePublishedTrack', ...args]);
				throw new Error('not_found');
			},
		});

		await service.updateParticipant({
			...params,
			mute: true,
			deaf: false,
			canSpeak: true,
			canStream: true,
			canVideo: false,
		});

		expect(calls).toEqual([
			['updateParticipant', 'guild_1_channel_2', 'user_3_conn-1', {attributes: {server_mute: 'true'}}],
			[
				'updateParticipant',
				'guild_1_channel_2',
				'user_3_conn-1',
				undefined,
				{
					canPublish: true,
					canSubscribe: true,
					canPublishSources: [TrackSource.SCREEN_SHARE, TrackSource.SCREEN_SHARE_AUDIO],
				},
			],
			['mutePublishedTrack', 'guild_1_channel_2', 'user_3_conn-1', 'TR_cam', true],
			['mutePublishedTrack', 'guild_1_channel_2', 'user_3_conn-1', 'TR_mic', true],
		]);
	});

	it('clears the recorded mute before restoring microphone publishing', async () => {
		const {client, calls} = roomServiceClient({
			attributes: {server_mute: 'true'},
			tracks: [{sid: 'TR_mic', source: TrackSource.MICROPHONE}],
		});
		const service = createServiceWithRoomServiceClient(client);

		await service.updateParticipant({
			...params,
			mute: false,
			deaf: false,
			canSpeak: true,
			canStream: true,
			canVideo: true,
		});

		expect(calls).toEqual([
			['updateParticipant', 'guild_1_channel_2', 'user_3_conn-1', {attributes: {server_mute: ''}}],
			[
				'updateParticipant',
				'guild_1_channel_2',
				'user_3_conn-1',
				undefined,
				{canPublish: true, canSubscribe: true, canPublishSources: allSources},
			],
			['mutePublishedTrack', 'guild_1_channel_2', 'user_3_conn-1', 'TR_mic', false],
		]);
	});

	it('leaves the microphone of a member who was never server muted alone', async () => {
		const {client, calls} = roomServiceClient({
			attributes: {},
			tracks: [{sid: 'TR_mic', source: TrackSource.MICROPHONE}],
		});
		const service = createServiceWithRoomServiceClient(client);

		await service.updateParticipant({
			...params,
			mute: false,
			deaf: false,
			canSpeak: true,
			canStream: true,
			canVideo: true,
		});

		expect(calls).toEqual([
			[
				'updateParticipant',
				'guild_1_channel_2',
				'user_3_conn-1',
				undefined,
				{canPublish: true, canSubscribe: true, canPublishSources: allSources},
			],
		]);
	});

	it('keeps the sources a muted member already has when the caller sends no permissions', async () => {
		const {client, calls} = roomServiceClient({
			attributes: {server_mute: 'true'},
			permission: {canSubscribe: true, canPublishSources: []},
			tracks: [],
		});
		const service = createServiceWithRoomServiceClient(client);

		await service.updateParticipant({...params, mute: true, deaf: false});

		expect(calls).toEqual([
			[
				'updateParticipant',
				'guild_1_channel_2',
				'user_3_conn-1',
				undefined,
				{canPublish: false, canSubscribe: true, canPublishSources: []},
			],
		]);
	});

	it('mutes camera and screen tracks a permission sync no longer allows', async () => {
		const {client, calls} = roomServiceClient({
			attributes: {},
			tracks: [
				{sid: 'TR_mic', source: TrackSource.MICROPHONE},
				{sid: 'TR_cam', source: TrackSource.CAMERA},
				{sid: 'TR_screen', source: TrackSource.SCREEN_SHARE},
			],
		});
		const service = createServiceWithRoomServiceClient(client);

		await service.updateParticipant({
			...params,
			mute: false,
			deaf: false,
			canSpeak: true,
			canStream: false,
			canVideo: false,
		});

		expect(calls).toEqual([
			[
				'updateParticipant',
				'guild_1_channel_2',
				'user_3_conn-1',
				undefined,
				{canPublish: true, canSubscribe: true, canPublishSources: [TrackSource.MICROPHONE]},
			],
			['mutePublishedTrack', 'guild_1_channel_2', 'user_3_conn-1', 'TR_cam', true],
			['mutePublishedTrack', 'guild_1_channel_2', 'user_3_conn-1', 'TR_screen', true],
		]);
	});
});

describe('LiveKitService createToken mute', () => {
	const params = {
		userId: createUserID(3n),
		guildId: createGuildID(1n),
		channelId: createChannelID(2n),
		connectionId: 'conn-1',
		tokenNonce: 'nonce-1',
		regionId: 'region-1',
		serverId: 'region-1-server-1',
	};

	async function tokenPayload(mute: boolean) {
		const service = createServiceWithRoomServiceClient({});
		const {token} = await service.createToken({...params, mute});
		return JSON.parse(Buffer.from(token.split('.')[1]!, 'base64url').toString());
	}

	async function tokenAttributes(mute: boolean): Promise<Record<string, string> | undefined> {
		return (await tokenPayload(mute)).attributes;
	}

	it('withholds microphone publishing from a token issued to a muted member', async () => {
		expect((await tokenPayload(true)).video.canPublishSources).toEqual([
			'camera',
			'screen_share',
			'screen_share_audio',
		]);
		expect((await tokenPayload(false)).video.canPublishSources).toContain('microphone');
	});

	it('records the server mute on a token issued to a muted member', async () => {
		expect(await tokenAttributes(true)).toEqual({server_mute: 'true'});
	});

	it('records no server mute on a token issued to an unmuted member', async () => {
		expect(await tokenAttributes(false)).toBeUndefined();
	});
});
