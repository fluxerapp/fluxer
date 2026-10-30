// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash, createHmac} from 'node:crypto';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {ILiveKitService} from '@app/api/infrastructure/ILiveKitService';
import type {IVoiceRoomStore} from '@app/api/infrastructure/IVoiceRoomStore';
import {LiveKitWebhookService} from '@app/api/infrastructure/LiveKitWebhookService';
import type {VoiceRegionRecord, VoiceServerRecord} from '@app/api/voice/VoiceModel';
import type {VoiceTopology} from '@app/api/voice/VoiceTopology';
import type {WebhookEvent} from 'livekit-server-sdk';
import {describe, expect, it} from 'vitest';

const GUILD = '100';
const CHANNEL = '200';
const ROOM = `guild_${GUILD}_channel_${CHANNEL}`;

function server(serverId: string, apiKey: string): VoiceServerRecord {
	return {
		regionId: 'fra',
		serverId,
		endpoint: `wss://${serverId}.example.com`,
		apiKey,
		apiSecret: `${apiKey}-secret`,
		latitude: null,
		longitude: null,
		isActive: true,
		softConnectionLimit: null,
		notes: null,
		internalEndpoint: null,
		pinnedHostname: null,
		pinnedSoftLimit: false,
		draining: false,
		restrictions: {
			vipOnly: false,
			requiredGuildFeatures: new Set(),
			allowedGuildIds: new Set(),
			allowedUserIds: new Set(),
		},
		createdAt: null,
		updatedAt: null,
	};
}

function setup(servers: Array<VoiceServerRecord> = [server('pinned', 'key-pinned'), server('other', 'key-other')]) {
	const calls: Array<string> = [];
	let pinned: {regionId: string; serverId: string} | null = {regionId: 'fra', serverId: 'pinned'};
	const topology = {
		getAllRegions: () => [{id: 'fra'} as VoiceRegionRecord],
		getServersForRegion: () => servers,
		registerSubscriber: () => {},
	} as unknown as VoiceTopology;
	const roomStore = {
		getPinnedRoomServer: async () => pinned,
		deleteRoomServer: async () => {
			calls.push('delete_pin');
			pinned = null;
		},
		pinRoomServer: async () => {},
	} as unknown as IVoiceRoomStore;
	const gateway = {
		confirmVoiceConnection: async () => {
			calls.push('confirm');
			return {success: true};
		},
		disconnectVoiceUserIfInChannel: async () => {
			calls.push('disconnect');
			return {success: true};
		},
	} as unknown as IGatewayService;
	const liveKit = {
		listParticipants: async ({serverId}: {serverId: string}) => {
			calls.push(`list:${serverId}`);
			return {status: 'ok', participants: []};
		},
		disconnectParticipant: async () => calls.push('kick'),
		muteMicrophoneTrack: async () => calls.push('mute'),
	} as unknown as ILiveKitService;
	const service = new LiveKitWebhookService(roomStore, gateway, liveKit, topology);
	return {service, calls};
}

function participantEvent(event: string, metadataServer: string): WebhookEvent {
	return {
		event,
		room: {name: ROOM},
		participant: {
			identity: 'user_1_conn',
			metadata: JSON.stringify({
				user_id: '1',
				channel_id: CHANNEL,
				guild_id: GUILD,
				connection_id: 'conn',
				token_nonce: 'nonce',
				issued_at: '1',
				region_id: 'fra',
				server_id: metadataServer,
			}),
			attributes: {},
		},
	} as unknown as WebhookEvent;
}

describe('LiveKit webhooks from a server the room is not pinned to', () => {
	it('ignores forged participant events and room_finished', async () => {
		const {service, calls} = setup();
		await service.processEvent({event: participantEvent('participant_left', 'pinned'), apiKey: 'key-other'});
		await service.processEvent({event: participantEvent('participant_joined', 'pinned'), apiKey: 'key-other'});
		await service.processEvent({
			event: {event: 'room_finished', room: {name: ROOM}} as WebhookEvent,
			apiKey: 'key-other',
		});
		expect(calls).toEqual([]);
	});

	it('acts on events from the pinned server and checks presence on that server', async () => {
		const {service, calls} = setup();
		await service.processEvent({event: participantEvent('participant_joined', 'other'), apiKey: 'key-pinned'});
		await service.processEvent({event: participantEvent('participant_left', 'other'), apiKey: 'key-pinned'});
		await service.processEvent({
			event: {event: 'room_finished', room: {name: ROOM}} as WebhookEvent,
			apiKey: 'key-pinned',
		});
		expect(calls).toEqual(['confirm', 'list:pinned', 'disconnect', 'delete_pin']);
	});

	it('ignores rooms made by rexctl voice test', async () => {
		const {service, calls} = setup();
		await service.processEvent({
			event: {event: 'room_finished', room: {name: 'rexctl-test-abc'}} as WebhookEvent,
			apiKey: 'key-pinned',
		});
		expect(calls).toEqual([]);
	});
});

describe('LiveKit webhooks from servers that share an API key', () => {
	const shared = () => setup([server('pinned', 'fluxer'), server('other', 'fluxer')]);

	it('clears the voice state when a participant leaves the server that is not last in the list', async () => {
		const {service, calls} = shared();
		await service.processEvent({event: participantEvent('participant_left', 'pinned'), apiKey: 'fluxer'});
		expect(calls).toEqual(['list:pinned', 'disconnect']);
	});

	it('confirms joins on either server', async () => {
		const {service, calls} = shared();
		await service.processEvent({event: participantEvent('participant_joined', 'pinned'), apiKey: 'fluxer'});
		expect(calls).toEqual(['confirm']);
	});

	it('ignores events whose metadata points at a server the room is not pinned to', async () => {
		const {service, calls} = shared();
		await service.processEvent({event: participantEvent('participant_left', 'other'), apiKey: 'fluxer'});
		expect(calls).toEqual([]);
	});

	it('ignores metadata that points at a server with another key', async () => {
		const {service, calls} = setup([server('pinned', 'fluxer'), server('other', 'fluxer'), server('third', 'own')]);
		await service.processEvent({event: participantEvent('participant_left', 'third'), apiKey: 'fluxer'});
		expect(calls).toEqual([]);
	});

	it('clears the pin when the pinned server finishes the room', async () => {
		const {service, calls} = shared();
		await service.processEvent({event: {event: 'room_finished', room: {name: ROOM}} as WebhookEvent, apiKey: 'fluxer'});
		expect(calls).toEqual(['delete_pin']);
	});
});

describe('LiveKit webhook signatures', () => {
	function signed(body: string, offsetSeconds: number): string {
		const encode = (value: unknown) => Buffer.from(JSON.stringify(value)).toString('base64url');
		const now = Math.floor(Date.now() / 1000) + offsetSeconds;
		const claims = {
			iss: 'key-pinned',
			nbf: now - 10,
			exp: now + 60,
			sha256: createHash('sha256').update(body).digest('base64'),
		};
		const input = `${encode({alg: 'HS256', typ: 'JWT'})}.${encode(claims)}`;
		return `${input}.${createHmac('sha256', 'key-pinned-secret').update(input).digest('base64url')}`;
	}

	it('accepts a voice machine whose clock is up to a minute off', async () => {
		const {service} = setup();
		const body = JSON.stringify({event: 'room_started', id: 'EV_1', room: {name: ROOM}});
		for (const offset of [45, -45]) {
			await expect(service.verifyAndParse(body, signed(body, offset))).resolves.toMatchObject({apiKey: 'key-pinned'});
		}
		for (const offset of [180, -180]) {
			await expect(service.verifyAndParse(body, signed(body, offset))).rejects.toThrow();
		}
	});
});
