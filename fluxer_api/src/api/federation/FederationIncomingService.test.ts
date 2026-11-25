// SPDX-License-Identifier: AGPL-3.0-or-later

import type {FederationConfig} from '@app/api/federation/FederationConfig';
import {generateFederationKeyPair, signFederationBytes} from '@app/api/federation/FederationCrypto';
import {createFederationEvent} from '@app/api/federation/FederationEvent';
import type {IFederationInboxRepository} from '@app/api/federation/FederationInboxRepository';
import {FederationIncomingService} from '@app/api/federation/FederationIncomingService';
import type {FederationInboxRow} from '@app/api/database/types/FederationTypes';
import {describe, expect, it} from 'vitest';

const PEER_ID = 'chat-b.example.org';
const SELF_ID = 'chat-a.example.org';

function buildConfig(overrides: Partial<FederationConfig> = {}): FederationConfig {
	const peerKey = generateFederationKeyPair();
	return {
		enabled: true,
		instanceId: SELF_ID,
		keyId: 'key-1',
		privateKey: peerKey.privateKey,
		peers: [{instanceId: PEER_ID, baseUrl: 'https://chat-b.example.org', publicKey: peerKey.publicKey}],
		guildIds: new Set(['100']),
		testChannelId: null,
		timeoutMs: 1_000,
		maxAttempts: 3,
		retryBaseMs: 1_000,
		retryMaxMs: 10_000,
		...overrides,
	};
}

class FakeInboxRepository implements IFederationInboxRepository {
	readonly rows = new Map<string, FederationInboxRow>();
	applied: Array<{origin: string; eventId: string; appliedAt: Date}> = [];
	rejected: Array<{origin: string; eventId: string; reason: string}> = [];

	private key(origin: string, eventId: string): string {
		return `${origin}\u0000${eventId}`;
	}

	async insertIfAbsent(row: FederationInboxRow): Promise<boolean> {
		const key = this.key(row.origin, row.event_id);
		if (this.rows.has(key)) return false;
		this.rows.set(key, {...row});
		return true;
	}

	async get(origin: string, eventId: string): Promise<FederationInboxRow | null> {
		return this.rows.get(this.key(origin, eventId)) ?? null;
	}

	async listPendingForOrigin(origin: string, limit: number): Promise<Array<FederationInboxRow>> {
		return [...this.rows.values()].filter((row) => row.origin === origin).slice(0, limit);
	}

	async markApplied(origin: string, eventId: string, appliedAt: Date): Promise<void> {
		this.applied.push({origin, eventId, appliedAt});
	}

	async markRejected(origin: string, eventId: string, reason: string): Promise<void> {
		this.rejected.push({origin, eventId, reason});
	}
}

function buildEvent(config: FederationConfig, overrides: {eventId?: string; origin?: string; seq?: number} = {}) {
	return createFederationEvent({
		eventId: overrides.eventId ?? 'event-1',
		origin: overrides.origin ?? PEER_ID,
		seq: overrides.seq ?? 1,
		type: 'message.create',
		occurredAt: new Date(),
		payload: {
			guild: {origin: PEER_ID, id: '100'},
			channel: {origin: PEER_ID, id: '200'},
			message: {
				origin: PEER_ID,
				id: '300',
				author: {handle: `ana@${PEER_ID}`, display_name: 'Ana'},
				content: 'hola',
				created_at: new Date().toISOString(),
				reply_to: null,
			},
		},
		sign: (bytes) => signFederationBytes(bytes, config.privateKey!),
	});
}

describe('FederationIncomingService', () => {
	it('persists a valid event', async () => {
		const config = buildConfig();
		const repository = new FakeInboxRepository();
		const service = new FederationIncomingService(config, repository);

		const result = await service.receive(buildEvent(config), {keyId: 'key-1', origin: PEER_ID});

		expect(result).toEqual({ok: true, eventId: 'event-1'});
		const row = repository.rows.get(`${PEER_ID}\u0000event-1`);
		expect(row?.status).toBe('received');
		expect(row?.origin).toBe(PEER_ID);
	});

	it('treats a duplicate event as accepted without inserting twice', async () => {
		const config = buildConfig();
		const repository = new FakeInboxRepository();
		const service = new FederationIncomingService(config, repository);
		const event = buildEvent(config);

		await service.receive(event, {keyId: 'key-1', origin: PEER_ID});
		const result = await service.receive(event, {keyId: 'key-1', origin: PEER_ID});

		expect(result).toEqual({ok: true, eventId: 'event-1'});
		expect(repository.rows.size).toBe(1);
	});

	it('rejects the same event id with different content', async () => {
		const config = buildConfig();
		const repository = new FakeInboxRepository();
		const service = new FederationIncomingService(config, repository);
		const event = buildEvent(config);
		await service.receive(event, {keyId: 'key-1', origin: PEER_ID});
		const row = repository.rows.get(`${PEER_ID}\u0000event-1`);
		if (!row) throw new Error('missing row');
		row.payload_hash = 'sha256:different';
		const result = await service.receive(event, {keyId: 'key-1', origin: PEER_ID});
		expect(result).toEqual({ok: false, reason: 'event_id_conflict'});
	});

	it('rejects an unknown origin', async () => {
		const config = buildConfig();
		const service = new FederationIncomingService(config, new FakeInboxRepository());

		const result = await service.receive(buildEvent(config), {keyId: 'key-1', origin: 'evil.example.org'});

		expect(result).toEqual({ok: false, reason: 'unknown_origin'});
	});

	it('rejects an event whose origin does not match the peer', async () => {
		const config = buildConfig();
		const service = new FederationIncomingService(config, new FakeInboxRepository());
		const event = buildEvent(config, {origin: 'chat-c.example.org'});

		const result = await service.receive(event, {keyId: 'key-1', origin: PEER_ID});

		expect(result).toEqual({ok: false, reason: 'origin_mismatch'});
	});

	it('rejects an invalid signature', async () => {
		const config = buildConfig();
		const service = new FederationIncomingService(config, new FakeInboxRepository());
		const event = buildEvent(config);
		const tampered = {...event, payload: {...event.payload, content: 'changed'}};

		const result = await service.receive(tampered, {keyId: 'key-1', origin: PEER_ID});

		expect(result.ok).toBe(false);
		expect(result.reason).toBe('payload_hash_mismatch');
	});

	it('rejects a replayed sequence for a new event id', async () => {
		const config = buildConfig();
		const repository = new FakeInboxRepository();
		const cursor = {
			last: 5,
			async getLastSeq() {
				return this.last;
			},
			async setLastSeq(_origin: string, seq: number) {
				this.last = seq;
			},
		};
		const service = new FederationIncomingService(config, repository, undefined, undefined, cursor);

		const result = await service.receive(buildEvent(config, {seq: 5}), {keyId: 'key-1', origin: PEER_ID});

		expect(result).toEqual({ok: false, reason: 'replayed_seq'});
	});

	it('rejects events while federation is disabled', async () => {
		const config = buildConfig({enabled: false});
		const service = new FederationIncomingService(config, new FakeInboxRepository());

		const result = await service.receive(buildEvent(config), {keyId: 'key-1', origin: PEER_ID});

		expect(result).toEqual({ok: false, reason: 'federation_disabled'});
	});
});