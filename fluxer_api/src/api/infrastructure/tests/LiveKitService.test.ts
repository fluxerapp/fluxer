// SPDX-License-Identifier: AGPL-3.0-or-later

import {AccessToken, TrackSource} from 'livekit-server-sdk';
import {describe, expect, it} from 'vitest';
import {createChannelID, createGuildID} from '../../BrandedTypes';
import {computeLiveKitPublishSources, LiveKitService, VOICE_TOKEN_TTL_SECONDS} from '../LiveKitService';

function decodeJwtPayload(token: string): Record<string, unknown> {
	const [, payload] = token.split('.');
	if (!payload) {
		throw new Error('JWT payload missing');
	}
	return JSON.parse(Buffer.from(payload, 'base64url').toString('utf8')) as Record<string, unknown>;
}

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
	it('serializes stream grants into LiveKit JWT video claims', async () => {
		const token = new AccessToken('test-key', 'test-secret', {identity: 'user_1_conn'});
		token.addGrant({
			roomJoin: true,
			room: 'guild_1_channel_2',
			canPublish: true,
			canSubscribe: true,
			canPublishSources: computeLiveKitPublishSources({canSpeak: true, canStream: true, canVideo: false}),
		});
		const payload = decodeJwtPayload(await token.toJwt());
		expect(payload.video).toMatchObject({
			roomJoin: true,
			room: 'guild_1_channel_2',
			canPublish: true,
			canSubscribe: true,
			canPublishSources: ['microphone', 'screen_share', 'screen_share_audio'],
		});
	});
	it('bounds voice token lifetime to the configured TTL', async () => {
		const token = new AccessToken('test-key', 'test-secret', {
			identity: 'user_1_conn',
			ttl: VOICE_TOKEN_TTL_SECONDS,
		});
		token.addGrant({roomJoin: true, room: 'guild_1_channel_2'});
		const payload = decodeJwtPayload(await token.toJwt());
		const exp = payload.exp as number;
		const nowSeconds = Math.floor(Date.now() / 1000);
		expect(exp - nowSeconds).toBeLessThanOrEqual(VOICE_TOKEN_TTL_SECONDS + 5);
		expect(exp - nowSeconds).toBeGreaterThan(0);
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
