// SPDX-License-Identifier: AGPL-3.0-or-later

import type {FederationOutboxRow} from '@app/api/database/types/FederationTypes';
import type {FederationConfig} from '@app/api/federation/FederationConfig';
import {generateFederationKeyPair} from '@app/api/federation/FederationCrypto';
import {
	type FederationEventType,
	type SignedFederationEvent,
	verifyFederationEvent,
} from '@app/api/federation/FederationEvent';
import type {
	FederationOutboxAttempt,
	IFederationOutboxRepository,
} from '@app/api/federation/FederationOutboxRepository';
import {
	type FederationOutboxKV,
	type FederationOutboxLogger,
	FederationOutboxService,
} from '@app/api/federation/FederationOutboxService';
import type {FederationDeliveryResult, IFederationTransport} from '@app/api/federation/FederationTransport';
import type {Channel} from '@app/api/models/Channel';
import type {Message} from '@app/api/models/Message';
import type {User} from '@app/api/models/User';
import {MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {describe, expect, it} from 'vitest';

const NOW = new Date('2026-11-24T17:00:00.000Z');
const KEY_PAIR = generateFederationKeyPair();
const PEER_ID = 'chat-b.example.org';

class FakeKV implements FederationOutboxKV {
	seq = 0;
	private readonly locks = new Map<string, string>();

	async incr(): Promise<number> {
		this.seq += 1;
		return this.seq;
	}

	async acquireLock(key: string, token: string): Promise<boolean> {
		if (this.locks.has(key)) return false;
		this.locks.set(key, token);
		return true;
	}

	async releaseLock(key: string, token: string): Promise<boolean> {
		if (this.locks.get(key) !== token) return false;
		this.locks.delete(key);
		return true;
	}
}

class FakeRepository implements IFederationOutboxRepository {
	readonly rows = new Map<string, FederationOutboxRow>();

	private key(targetInstanceId: string, eventId: string): string {
		return `${targetInstanceId}\u0000${eventId}`;
	}

	async insertIfAbsent(row: FederationOutboxRow): Promise<boolean> {
		const key = this.key(row.target_instance_id, row.event_id);
		if (this.rows.has(key)) return false;
		this.rows.set(key, {...row});
		return true;
	}

	async listPendingForPeer(targetInstanceId: string, limit: number): Promise<Array<FederationOutboxRow>> {
		return [...this.rows.values()]
			.filter((row) => row.target_instance_id === targetInstanceId)
			.slice(0, limit)
			.map((row) => ({...row}));
	}

	async markAttempt(attempt: FederationOutboxAttempt): Promise<void> {
		const key = this.key(attempt.targetInstanceId, attempt.eventId);
		const row = this.rows.get(key);
		if (!row) return;
		this.rows.set(key, {
			...row,
			attempts: attempt.attempts,
			next_attempt_at: attempt.nextAttemptAt,
			last_attempt_at: attempt.lastAttemptAt,
			last_error: attempt.lastError,
		});
	}

	async delete(targetInstanceId: string, eventId: string): Promise<void> {
		this.rows.delete(this.key(targetInstanceId, eventId));
	}
}

class FakeTransport implements IFederationTransport {
	readonly sent: Array<{peerId: string; event: SignedFederationEvent}> = [];
	statusFor: (event: SignedFederationEvent) => number = () => 202;
	error: unknown = null;

	async send(peer: {instanceId: string}, event: SignedFederationEvent): Promise<FederationDeliveryResult> {
		this.sent.push({peerId: peer.instanceId, event});
		if (this.error) throw this.error;
		return {status: this.statusFor(event)};
	}
}

class FakeLogger implements FederationOutboxLogger {
	readonly warnings: Array<{obj: unknown; message?: string}> = [];

	info(): void {}

	warn(obj: unknown, message?: string): void {
		this.warnings.push({obj, message});
	}
}

function buildConfig(overrides: Partial<FederationConfig> = {}): FederationConfig {
	return {
		enabled: true,
		instanceId: 'chat-a.example.org',
		keyId: 'key-1',
		privateKey: KEY_PAIR.privateKey,
		peers: [{
			instanceId: PEER_ID, 
			baseUrl: 'https://chat-b.example.org',
			publicKey: KEY_PAIR.publicKey,
		}],
		guildIds: new Set(['100']),
		testChannelId: null,
		timeoutMs: 1_000,
		maxAttempts: 3,
		retryBaseMs: 1_000,
		retryMaxMs: 10_000,
		...overrides,
	};
}

function buildMessage(overrides: Partial<Message> = {}): Message {
	return {
		id: 300n,
		content: 'hola',
		type: MessageTypes.DEFAULT,
		attachments: [],
		stickers: [],
		embeds: [],
		reference: null,
		...overrides,
	} as unknown as Message;
}

const CHANNEL = {id: 200n, guildId: 100n} as unknown as Channel;
const USER = {id: 1n, username: 'ana', globalName: 'Ana'} as unknown as User;
const GUILD = {id: '100'} as GuildResponse;

function createService(
	options: {
		config?: Partial<FederationConfig>;
		transport?: FakeTransport;
		repository?: FakeRepository;
		kv?: FakeKV;
		logger?: FakeLogger;
		random?: () => number;
	} = {},
): {
	service: FederationOutboxService;
	config: FederationConfig;
	transport: FakeTransport;
	repository: FakeRepository;
	kv: FakeKV;
	logger: FakeLogger;
} {
	const config = buildConfig(options.config);
	const transport = options.transport ?? new FakeTransport();
	const repository = options.repository ?? new FakeRepository();
	const kv = options.kv ?? new FakeKV();
	const logger = options.logger ?? new FakeLogger();
	const service = new FederationOutboxService({
		config,
		repository,
		kv,
		transport,
		logger,
		now: () => NOW,
		random: options.random ?? (() => 0.5),
		lockToken: () => 'lock-token',
	});
	return {service, config, transport, repository, kv, logger};
}

async function enqueue(service: FederationOutboxService, message = buildMessage()): Promise<void> {
	await service.enqueueMessageCreated({message, channel: CHANNEL, guild: GUILD, user: USER});
}

function storedRow(repository: FakeRepository, eventId = '300_create'): FederationOutboxRow {
	const row = [...repository.rows.values()].find((entry) => entry.event_id === eventId);
	if (!row) throw new Error('Expected an outbox row');
	return row;
}

describe('FederationOutboxService enqueue', () => {
	it('does nothing when federation is disabled', async () => {
		const {service, repository, kv} = createService({config: {enabled: false}});

		await enqueue(service);

		expect(repository.rows.size).toBe(0);
		expect(kv.seq).toBe(0);
	});

	it('does nothing for a guild outside the allowlist', async () => {
		const {service, repository} = createService({config: {guildIds: new Set(['999'])}});

		await enqueue(service);

		expect(repository.rows.size).toBe(0);
	});

	it('does nothing for empty content or media messages', async () => {
		const {service, repository} = createService();

		await enqueue(service, buildMessage({content: '   '}));
		await enqueue(service, buildMessage({content: 'x', attachments: [{}] as unknown as Message['attachments']}));

		expect(repository.rows.size).toBe(0);
	});

	it('stores one row per peer with a deterministic event id and payload', async () => {
		const {service, repository, kv} = createService({
			config: {
				peers: [
					{instanceId: PEER_ID, baseUrl: 'https://chat-b.example.org', publicKey: KEY_PAIR.publicKey},
					{
						instanceId: 'chat-c.example.org',
						baseUrl: 'https://chat-c.example.org',
						publicKey: KEY_PAIR.publicKey,
					},
				],
			},
		});

		await enqueue(service);

		expect(kv.seq).toBe(1);
		expect(repository.rows.size).toBe(2);
		const row = storedRow(repository);
		expect(row.seq).toBe(1n);
		expect(row.origin).toBe('chat-a.example.org');
		expect(row.type).toBe('message.create');
		expect(row.attempts).toBe(0);
		expect(JSON.parse(row.payload)).toEqual({
			guild: {origin: 'chat-a.example.org', id: '100'},
			channel: {origin: 'chat-a.example.org', id: '200'},
			message: {
				origin: 'chat-a.example.org',
				id: '300',
				author: {origin: 'chat-a.example.org', id: '1', handle: 'ana@chat-a.example.org', display_name: 'Ana'},
				content: 'hola',
				created_at: expect.any(String),
				reply_to: null,
			},
		});
	});

	it('does not duplicate rows when the same message is enqueued twice', async () => {
		const {service, repository} = createService();

		await enqueue(service);
		await enqueue(service);

		expect(repository.rows.size).toBe(1);
	});
});

describe('FederationOutboxService drain', () => {
	it('signs a verifiable event and deletes the row after a 202', async () => {
		const {service, repository, transport} = createService();
		await enqueue(service);

		await service.drainPeer(PEER_ID);

		expect(transport.sent).toHaveLength(1);
		const event = transport.sent[0]!.event;
		expect(event.origin).toBe('chat-a.example.org');
		expect(event.seq).toBe(1);
		expect(event.occurred_at).toBe(NOW.toISOString());
		expect(verifyFederationEvent(event, {publicKey: KEY_PAIR.publicKey, now: NOW}).ok).toBe(true);
		expect(repository.rows.size).toBe(0);
	});

	it('keeps the row and schedules a retry after a 500', async () => {
		const {service, repository, transport} = createService();
		transport.statusFor = () => 500;
		await enqueue(service);

		await service.drainPeer(PEER_ID);

		const row = storedRow(repository);
		expect(row.attempts).toBe(1);
		expect(row.next_attempt_at.getTime()).toBeGreaterThan(NOW.getTime());
		expect(row.last_error).toContain('500');
	});

	it('drops the row after exhausting retries', async () => {
		const {service, repository, transport, logger} = createService({config: {maxAttempts: 1}});
		transport.statusFor = () => 500;
		await enqueue(service);

		await service.drainPeer(PEER_ID);

		expect(repository.rows.size).toBe(0);
		expect(
			logger.warnings.some((entry) => entry.message === 'Dropping federation event after exhausting retries'),
		).toBe(true);
	});

	it('drops the row on a non-retryable 4xx', async () => {
		const {service, repository, transport} = createService();
		transport.statusFor = () => 422;
		await enqueue(service);

		await service.drainPeer(PEER_ID);

		expect(repository.rows.size).toBe(0);
	});

	it('does not send while another worker holds the peer lock', async () => {
		const {service, transport, kv} = createService();
		await enqueue(service);
		await kv.acquireLock(`federation:outbox:lock:${PEER_ID}`, 'other', 30);

		await service.drainPeer(PEER_ID);

		expect(transport.sent).toHaveLength(0);
	});

	it('sends rows in seq order', async () => {
		const {service, repository, transport} = createService();
		const payload = JSON.stringify({message: {id: '1'}});
		await repository.insertIfAbsent({
			target_instance_id: PEER_ID,
			event_id: '300_create',
			origin: 'chat-a.example.org',
			seq: 2n,
			type: 'message.create' as FederationEventType,
			payload,
			created_at: NOW,
			next_attempt_at: NOW,
			attempts: 0,
			last_error: null,
			last_attempt_at: null,
		});
		await repository.insertIfAbsent({
			target_instance_id: PEER_ID,
			event_id: '200_create',
			origin: 'chat-a.example.org',
			seq: 1n,
			type: 'message.create' as FederationEventType,
			payload,
			created_at: NOW,
			next_attempt_at: NOW,
			attempts: 0,
			last_error: null,
			last_attempt_at: null,
		});

		await service.drainPeer(PEER_ID);

		expect(transport.sent.map((entry) => entry.event.event_id)).toEqual(['200_create', '300_create']);
	});

	it('stops draining a peer after a retryable failure to preserve order', async () => {
		const {service, repository, transport} = createService();
		const payload = JSON.stringify({message: {id: '1'}});
		for (const [seq, eventId] of [
			[1n, '200_create'],
			[2n, '300_create'],
		] as const) {
			await repository.insertIfAbsent({
				target_instance_id: PEER_ID,
				event_id: eventId,
				origin: 'chat-a.example.org',
				seq,
				type: 'message.create' as FederationEventType,
				payload,
				created_at: NOW,
				next_attempt_at: NOW,
				attempts: 0,
				last_error: null,
				last_attempt_at: null,
			});
		}
		transport.statusFor = (event) => (event.seq === 1 ? 500 : 202);

		await service.drainPeer(PEER_ID);

		expect(transport.sent.map((entry) => entry.event.seq)).toEqual([1]);
		expect(repository.rows.size).toBe(2);
	});
});
