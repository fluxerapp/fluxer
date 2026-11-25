// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import type {FederationConfig} from '@app/api/federation/FederationConfig';
import {generateFederationKeyPair, signFederationBytes} from '@app/api/federation/FederationCrypto';
import {createFederationEvent} from '@app/api/federation/FederationEvent';
import type {IFederationInboxRepository} from '@app/api/federation/FederationInboxRepository';
import {FederationInboxApplicationService} from '@app/api/federation/FederationInboxApplicationService';
import type {FederationInboxRow} from '@app/api/database/types/FederationTypes';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import {describe, expect, it} from 'vitest';

const PEER_ID = 'chat-b.example.org';

function buildConfig(overrides: Partial<FederationConfig> = {}): FederationConfig {
	const peerKey = generateFederationKeyPair();
	return {
		enabled: true,
		instanceId: 'chat-a.example.org',
		keyId: 'key-1',
		privateKey: peerKey.privateKey,
		peers: [{instanceId: PEER_ID, baseUrl: 'https://chat-b.example.org', publicKey: peerKey.publicKey}],
		guildIds: new Set(['100']),
		testChannelId: '200',
		timeoutMs: 1_000,
		maxAttempts: 3,
		retryBaseMs: 1_000,
		retryMaxMs: 10_000,
		...overrides,
	};
}

class FakeInboxRepository implements IFederationInboxRepository {
	applied: Array<{origin: string; eventId: string; appliedAt: Date}> = [];
	rejected: Array<{origin: string; eventId: string; reason: string}> = [];
	readonly row: FederationInboxRow = {
		event_id: 'event-1',
		origin: PEER_ID,
		seq: 1n,
		type: 'message.create',
		payload: '{}',
		payload_hash: 'sha256:placeholder',
		prev_hash: null,
		signature: 'ed25519:placeholder',
		key_id: 'key-1',
		occurred_at: new Date(),
		received_at: new Date(),
		status: 'received',
		error_reason: null,
		applied_at: null,
	};

	async insertIfAbsent(_row: FederationInboxRow): Promise<boolean> {
		return true;
	}

	async get(): Promise<FederationInboxRow | null> {
		return this.row;
	}

	async listPendingForOrigin(): Promise<Array<FederationInboxRow>> {
		return [this.row];
	}

	async markApplied(origin: string, eventId: string, appliedAt: Date): Promise<void> {
		this.applied.push({origin, eventId, appliedAt});
	}

	async markRejected(origin: string, eventId: string, reason: string): Promise<void> {
		this.rejected.push({origin, eventId, reason});
	}
}

class FakeGateway {
	dispatched: Array<{guildId: bigint; event: string; data: unknown}> = [];

	async dispatchGuild(params: {guildId: bigint; event: string; data: unknown}): Promise<void> {
		this.dispatched.push(params);
	}
}

function buildEvent(config: FederationConfig, overrides: {type?: 'message.create'} = {}) {
	return createFederationEvent({
		eventId: 'event-1',
		origin: PEER_ID,
		seq: 1,
		type: overrides.type ?? 'message.create',
		occurredAt: new Date(),
		payload: {
			guild: {origin: PEER_ID, id: '100'},
			channel: {origin: PEER_ID, id: '300'},
			message: {
				origin: PEER_ID,
				id: '400',
				author: {handle: `ana@${PEER_ID}`, display_name: 'Ana'},
				content: 'hola',
				created_at: new Date().toISOString(),
			},
		},
		sign: (bytes) => signFederationBytes(bytes, config.privateKey!),
	});
}

function buildService(
	config: FederationConfig,
	repository: FakeInboxRepository,
	gateway: FakeGateway,
	channel: {id: bigint; guildId: bigint | null} | null = {id: 200n, guildId: 100n},
) {
	const channelRepository = {
		channelData: {
			findUnique: async () => channel,
		},
	} as unknown as ChannelRepository;
	return new FederationInboxApplicationService(
		config,
		repository,
		channelRepository,
		gateway as unknown as IGatewayService,
	);
}

describe('FederationInboxApplicationService', () => {
	it('dispatches a message create event to the configured test channel', async () => {
		const config = buildConfig();
		const repository = new FakeInboxRepository();
		const gateway = new FakeGateway();
		const service = buildService(config, repository, gateway);

		await service.apply(buildEvent(config));

		expect(gateway.dispatched).toHaveLength(1);
		expect(gateway.dispatched[0]?.guildId).toBe(100n);
		expect(repository.applied).toHaveLength(1);
		expect(repository.rejected).toHaveLength(0);
	});

	it('marks the event applied without dispatch when no test channel is configured', async () => {
		const config = buildConfig({testChannelId: null});
		const repository = new FakeInboxRepository();
		const gateway = new FakeGateway();
		const service = buildService(config, repository, gateway);

		await service.apply(buildEvent(config));

		expect(gateway.dispatched).toHaveLength(0);
		expect(repository.applied).toHaveLength(1);
	});

	it('rejects events for guilds outside the allowlist', async () => {
		const config = buildConfig({guildIds: new Set(['999'])});
		const repository = new FakeInboxRepository();
		const service = buildService(config, repository, new FakeGateway());

		await service.apply(buildEvent(config));

		expect(repository.applied).toHaveLength(0);
		expect(repository.rejected).toEqual([
			{origin: PEER_ID, eventId: 'event-1', reason: 'invalid_payload'},
		]);
	});

	it('rejects events when the test channel is missing', async () => {
		const config = buildConfig();
		const repository = new FakeInboxRepository();
		const service = buildService(config, repository, new FakeGateway(), null);

		await service.apply(buildEvent(config));

		expect(repository.rejected).toEqual([
			{origin: PEER_ID, eventId: 'event-1', reason: 'test_channel_unavailable'},
		]);
	});
});