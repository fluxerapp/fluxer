// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import type {FederationOutboxRow} from '@app/api/database/types/FederationTypes';
import type {FederationConfig, FederationPeerConfig} from '@app/api/federation/FederationConfig';
import {signFederationBytes} from '@app/api/federation/FederationCrypto';
import {
	createFederationEvent,
	type FederationEventType,
	type SignedFederationEvent,
} from '@app/api/federation/FederationEvent';
import type {IFederationOutboxRepository} from '@app/api/federation/FederationOutboxRepository';
import type {IFederationTransport} from '@app/api/federation/FederationTransport';
import {federationOutboxTotal} from '@app/api/federation/FederationMetrics';
import type {Channel} from '@app/api/models/Channel';
import type {Message} from '@app/api/models/Message';
import type {User} from '@app/api/models/User';
import {MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';

const OUTBOX_BATCH_SIZE = 200;
const OUTBOX_LOCK_TTL_SECONDS = 30;
const MIN_RETRY_DELAY_MS = 100;

export interface FederationOutboxKV {
	incr(key: string): Promise<number>;
	acquireLock(key: string, token: string, ttlSeconds: number): Promise<boolean>;
	releaseLock(key: string, token: string): Promise<boolean>;
}

export interface FederationOutboxLogger {
	info(obj: unknown, message?: string): void;
	warn(obj: unknown, message?: string): void;
}

export interface FederationOutboxServiceDeps {
	config: FederationConfig;
	repository: IFederationOutboxRepository;
	kv: FederationOutboxKV;
	transport: IFederationTransport;
	logger: FederationOutboxLogger;
	now?: () => Date;
	random?: () => number;
	lockToken?: () => string;
}

export interface EnqueueFederationMessageParams {
	message: Message;
	channel: Channel;
	guild: GuildResponse | null;
	user: User;
}

interface FederationMessagePayload {
	guild: {origin: string; id: string};
	channel: {origin: string; id: string};
	message: {
		origin: string;
		id: string;
		author: {origin: string; id: string; handle: string; display_name: string};
		content: string;
		created_at: string;
		reply_to: string | null;
	};
}

function isSuccessStatus(status: number): boolean {
	return status >= 200 && status < 300;
}

function isRetryableStatus(status: number): boolean {
	return status === 408 || status === 425 || status === 429 || status >= 500;
}

function compareSeq(a: FederationOutboxRow, b: FederationOutboxRow): number {
	if (a.seq < b.seq) return -1;
	if (a.seq > b.seq) return 1;
	return a.event_id.localeCompare(b.event_id);
}

export class FederationOutboxService {
	private readonly now: () => Date;
	private readonly random: () => number;
	private readonly lockToken: () => string;

	constructor(private readonly deps: FederationOutboxServiceDeps) {
		this.now = deps.now ?? (() => new Date());
		this.random = deps.random ?? Math.random;
		this.lockToken = deps.lockToken ?? randomUUID;
	}

	async enqueueMessageCreated(params: EnqueueFederationMessageParams): Promise<void> {
		const {config} = this.deps;
		if (!config.enabled) return;
		const {message, channel, guild, user} = params;
		if (!guild || channel.guildId === null) return;
		if (!config.guildIds.has(guild.id)) return;
		if (message.type !== MessageTypes.DEFAULT && message.type !== MessageTypes.REPLY) return;
		const content = message.content;
		if (content === null || content.trim().length === 0) return;
		if (message.attachments.length > 0 || message.stickers.length > 0 || message.embeds.length > 0) return;
		try {
			const seq = await this.deps.kv.incr(this.seqKey());
			if (!Number.isSafeInteger(seq) || seq < 0) {
				throw new Error('Federation sequence exceeded the safe integer range');
			}
			const createdAt = this.now();
			const payload: FederationMessagePayload = {
				guild: {origin: config.instanceId, id: guild.id},
				channel: {origin: config.instanceId, id: channel.id.toString()},
				message: {
					origin: config.instanceId,
					id: message.id.toString(),
					author: {
						origin: config.instanceId,
						id: user.id.toString(),
						handle: `${user.username}@${config.instanceId}`,
						display_name: user.globalName ?? user.username,
					},
					content,
					created_at: snowflakeToDate(message.id).toISOString(),
					reply_to: message.reference?.messageId?.toString() ?? null,
				},
			};
			const eventId = `${message.id.toString()}_create`;
			const serializedPayload = JSON.stringify(payload);
			const results = await Promise.allSettled(
				config.peers.map((peer) =>
					this.deps.repository.insertIfAbsent({
						target_instance_id: peer.instanceId,
						event_id: eventId,
						origin: config.instanceId,
						seq: BigInt(seq),
						type: 'message.create',
						payload: serializedPayload,
						created_at: createdAt,
						next_attempt_at: createdAt,
						attempts: 0,
						last_error: null,
						last_attempt_at: null,
					}),
				),
			);
			for (const [index, result] of results.entries()) {
				if (result.status === 'rejected') {
					this.deps.logger.warn(
						{peer: config.peers[index]?.instanceId, eventId, error: result.reason},
						'Failed to enqueue federation outbox row',
					);
				}
			}
		} catch (error) {
			this.deps.logger.warn({messageId: message.id.toString(), error}, 'Failed to enqueue federation event');
		}
	}

	async drainAllPeers(): Promise<void> {
		if (!this.deps.config.enabled) return;
		const results = await Promise.allSettled(this.deps.config.peers.map((peer) => this.drainPeer(peer.instanceId)));
		for (const [index, result] of results.entries()) {
			if (result.status === 'rejected') {
				this.deps.logger.warn(
					{peer: this.deps.config.peers[index]?.instanceId, error: result.reason},
					'Failed to drain federation outbox peer',
				);
			}
		}
	}

	async drainPeer(peerInstanceId: string): Promise<void> {
		const {config} = this.deps;
		if (!config.enabled || config.privateKey === null) return;
		const peer = config.peers.find((entry) => entry.instanceId === peerInstanceId);
		if (!peer) return;
		const lockKey = `federation:outbox:lock:${peerInstanceId}`;
		const token = this.lockToken();
		if (!(await this.deps.kv.acquireLock(lockKey, token, OUTBOX_LOCK_TTL_SECONDS))) {
			return;
		}
		try {
			await this.drainPeerLocked(peer);
		} finally {
			await this.deps.kv.releaseLock(lockKey, token).catch((error: unknown) => {
				this.deps.logger.warn({peer: peerInstanceId, error}, 'Failed to release federation outbox lock');
			});
		}
	}

	private async drainPeerLocked(peer: FederationPeerConfig): Promise<void> {
		const {config} = this.deps;
		const privateKey = config.privateKey;
		if (privateKey === null) return;
		const rows = await this.deps.repository.listPendingForPeer(peer.instanceId, OUTBOX_BATCH_SIZE);
		rows.sort(compareSeq);
		for (const row of rows) {
			const now = this.now();
			if (row.next_attempt_at.getTime() > now.getTime()) {
				return;
			}
			let event: SignedFederationEvent;
			try {
				const seq = Number(row.seq);
				if (!Number.isSafeInteger(seq) || seq < 0) {
					throw new Error('Federation outbox seq is outside the safe integer range');
				}
				event = createFederationEvent({
					eventId: row.event_id,
					origin: row.origin,
					seq,
					type: row.type as FederationEventType,
					occurredAt: now,
					payload: JSON.parse(row.payload) as Record<string, unknown>,
					prevHash: null,
					sign: (bytes) => signFederationBytes(bytes, privateKey),
				});
			} catch (error) {
				this.deps.logger.warn(
					{peer: peer.instanceId, eventId: row.event_id, error},
					'Dropping invalid federation outbox row',
				);
				await this.deps.repository.delete(peer.instanceId, row.event_id);
				federationOutboxTotal.inc('result="invalid_dropped"');
				continue;
			}
			let status: number;
			try {
				status = (await this.deps.transport.send(peer, event)).status;
			} catch (error) {
				await this.recordRetry(peer, row, error);
				return;
			}
			if (isSuccessStatus(status)) {
				await this.deps.repository.delete(peer.instanceId, row.event_id);
				federationOutboxTotal.inc('result="delivered"');
				continue;
			}
			if (isRetryableStatus(status)) {
				await this.recordRetry(peer, row, new Error(`Federation peer returned ${status}`));
				return;
			}
			this.deps.logger.warn(
				{peer: peer.instanceId, eventId: row.event_id, status},
				'Dropping federation event rejected by peer',
			);
			await this.deps.repository.delete(peer.instanceId, row.event_id);
			federationOutboxTotal.inc('result="rejected_dropped"');
		}
	}

	private async recordRetry(peer: FederationPeerConfig, row: FederationOutboxRow, error: unknown): Promise<void> {
		const attempts = row.attempts + 1;
		const now = this.now();
		const message = error instanceof Error ? error.message : String(error);
		if (attempts >= this.deps.config.maxAttempts) {
			this.deps.logger.warn(
				{peer: peer.instanceId, eventId: row.event_id, attempts, error: message},
				'Dropping federation event after exhausting retries',
			);
			await this.deps.repository.delete(peer.instanceId, row.event_id);
			federationOutboxTotal.inc('result="exhausted"');
			return;
		}
		federationOutboxTotal.inc('result="retry"');
		const delayMs = this.retryDelayMs(attempts);
		await this.deps.repository.markAttempt({
			targetInstanceId: peer.instanceId,
			eventId: row.event_id,
			attempts,
			nextAttemptAt: new Date(now.getTime() + delayMs),
			lastAttemptAt: now,
			lastError: message,
		});
	}

	private retryDelayMs(attempts: number): number {
		const exponential = Math.min(
			this.deps.config.retryMaxMs,
			this.deps.config.retryBaseMs * 2 ** Math.max(0, attempts - 1),
		);
		const jitter = 0.8 + 0.4 * this.random();
		return Math.max(MIN_RETRY_DELAY_MS, Math.round(exponential * jitter));
	}

	private seqKey(): string {
		return `federation:seq:${this.deps.config.instanceId}`;
	}
}
