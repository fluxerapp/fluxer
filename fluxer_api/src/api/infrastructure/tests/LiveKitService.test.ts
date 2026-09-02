// SPDX-License-Identifier: AGPL-3.0-or-later

import type {WebhookEvent} from 'livekit-server-sdk';
import {AccessToken, TrackSource} from 'livekit-server-sdk';
import {describe, expect, it, vi} from 'vitest';
import {createUserID} from '../../BrandedTypes';
import {getConfig} from '../../Config';
import type {LimitConfigService} from '../../limits/LimitConfigService';
import type {User} from '../../models/User';
import type {IUserRepository} from '../../user/IUserRepository';
import type {VoiceTopology} from '../../voice/VoiceTopology';
import type {IGatewayService} from '../IGatewayService';
import type {ILiveKitService} from '../ILiveKitService';
import type {IVoiceRoomStore} from '../IVoiceRoomStore';
import {computeLiveKitPublishSources, computeRevokedPublishGrant, VOICE_TOKEN_TTL_SECONDS} from '../LiveKitService';
import {LiveKitWebhookService} from '../LiveKitWebhookService';

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
	it('revokes only the offending source and keeps the rest of the grant', () => {
		expect(
			computeRevokedPublishGrant(
				{
					canPublish: true,
					canPublishSources: [
						TrackSource.MICROPHONE,
						TrackSource.CAMERA,
						TrackSource.SCREEN_SHARE,
						TrackSource.SCREEN_SHARE_AUDIO,
					],
				},
				TrackSource.CAMERA,
			),
		).toEqual({
			canPublish: true,
			canPublishSources: [TrackSource.MICROPHONE, TrackSource.SCREEN_SHARE, TrackSource.SCREEN_SHARE_AUDIO],
		});
	});
	it('revokes screen share audio together with screen share video', () => {
		expect(
			computeRevokedPublishGrant(
				{
					canPublish: true,
					canPublishSources: [TrackSource.MICROPHONE, TrackSource.SCREEN_SHARE, TrackSource.SCREEN_SHARE_AUDIO],
				},
				TrackSource.SCREEN_SHARE,
			),
		).toEqual({canPublish: true, canPublishSources: [TrackSource.MICROPHONE]});
	});
	it('treats an empty source list as every source and never leaves it empty', () => {
		expect(computeRevokedPublishGrant({canPublish: true, canPublishSources: []}, TrackSource.CAMERA)).toEqual({
			canPublish: true,
			canPublishSources: [TrackSource.MICROPHONE, TrackSource.SCREEN_SHARE, TrackSource.SCREEN_SHARE_AUDIO],
		});
		expect(
			computeRevokedPublishGrant({canPublish: true, canPublishSources: [TrackSource.CAMERA]}, TrackSource.CAMERA),
		).toEqual({canPublish: false, canPublishSources: []});
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

function createFreeUser(): User {
	return {
		id: createUserID(1n),
		isBot: false,
		premiumType: null,
		premiumUntil: null,
		premiumGiftExtensionEndsAt: null,
		premiumWillCancel: false,
		premiumGraceEndsAt: null,
		flags: 0n,
		premiumFlags: 0,
		traits: new Set<string>(),
	} as unknown as User;
}

function createTrackPublishedEvent(width: number, height: number): WebhookEvent {
	return {
		event: 'track_published',
		room: {name: 'guild_2_channel_3'},
		participant: {identity: 'user_1_conn'},
		track: {type: 1, source: TrackSource.CAMERA, sid: 'TR_oversized', width, height},
	} as unknown as WebhookEvent;
}

function createWebhookHarness() {
	const muteParticipantTrack = vi.fn().mockResolvedValue(true);
	const revokeParticipantPublishSource = vi.fn().mockResolvedValue(true);
	const disconnectParticipant = vi.fn().mockResolvedValue(undefined);
	const disconnectVoiceUserIfInChannel = vi.fn().mockResolvedValue(undefined);
	const service = new LiveKitWebhookService(
		{
			getPinnedRoomServer: vi.fn().mockResolvedValue({regionId: 'region-1', serverId: 'region-1-server-1'}),
		} as unknown as IVoiceRoomStore,
		{disconnectVoiceUserIfInChannel} as unknown as IGatewayService,
		{findUnique: vi.fn().mockResolvedValue(createFreeUser())} as unknown as IUserRepository,
		{muteParticipantTrack, revokeParticipantPublishSource, disconnectParticipant} as unknown as ILiveKitService,
		{
			getAllRegions: () => [],
			getServersForRegion: () => [],
			registerSubscriber: () => {},
		} as unknown as VoiceTopology,
		{getConfigSnapshot: () => null} as unknown as LimitConfigService,
	);
	return {
		service,
		muteParticipantTrack,
		revokeParticipantPublishSource,
		disconnectParticipant,
		disconnectVoiceUserIfInChannel,
	};
}

describe('LiveKit free tier video resolution enforcement', () => {
	it('mutes the oversized track and revokes its source instead of ending the call', async () => {
		const {
			service,
			muteParticipantTrack,
			revokeParticipantPublishSource,
			disconnectParticipant,
			disconnectVoiceUserIfInChannel,
		} = createWebhookHarness();

		await service.handleTrackPublished(createTrackPublishedEvent(1920, 1080), 'api-key');

		expect(muteParticipantTrack).toHaveBeenCalledTimes(1);
		expect(muteParticipantTrack).toHaveBeenCalledWith(
			expect.objectContaining({trackSid: 'TR_oversized', muted: true, regionId: 'region-1'}),
		);
		expect(revokeParticipantPublishSource).toHaveBeenCalledTimes(1);
		expect(revokeParticipantPublishSource).toHaveBeenCalledWith(
			expect.objectContaining({source: TrackSource.CAMERA, connectionId: 'conn', regionId: 'region-1'}),
		);
		expect(disconnectParticipant).not.toHaveBeenCalled();
		expect(disconnectVoiceUserIfInChannel).not.toHaveBeenCalled();
	});

	it('leaves tracks within the free tier limits alone', async () => {
		const {service, muteParticipantTrack, revokeParticipantPublishSource, disconnectParticipant} =
			createWebhookHarness();

		await service.handleTrackPublished(createTrackPublishedEvent(1280, 720), 'api-key');

		expect(muteParticipantTrack).not.toHaveBeenCalled();
		expect(revokeParticipantPublishSource).not.toHaveBeenCalled();
		expect(disconnectParticipant).not.toHaveBeenCalled();
	});

	it('does not enforce resolution limits on self-hosted instances', async () => {
		const {service, muteParticipantTrack, revokeParticipantPublishSource, disconnectParticipant} =
			createWebhookHarness();
		const config = getConfig();
		const originalSelfHosted = config.instance.selfHosted;
		config.instance.selfHosted = true;
		try {
			await service.handleTrackPublished(createTrackPublishedEvent(3840, 2160), 'api-key');
		} finally {
			config.instance.selfHosted = originalSelfHosted;
		}

		expect(muteParticipantTrack).not.toHaveBeenCalled();
		expect(revokeParticipantPublishSource).not.toHaveBeenCalled();
		expect(disconnectParticipant).not.toHaveBeenCalled();
	});
});
