// SPDX-License-Identifier: AGPL-3.0-or-later

import type {FederationConfig} from './FederationConfig';
import {type SignedFederationEvent, verifyFederationEvent} from './FederationEvent';
import type {IFederationInboxCursor} from './FederationInboxCursor';
import type {IFederationInboxRepository} from './FederationInboxRepository';
import {federationIncomingTotal} from './FederationMetrics';
import type {FederationInboxRow} from '@app/api/database/types/FederationTypes';

export interface FederationIncomingHeaders {
	keyId: string;
	origin: string;
}

export interface FederationIncomingResult {
	ok: boolean;
	reason?: string;
	eventId?: string;
}

export interface FederationIncomingLogger {
	debug(obj: unknown, message?: string): void;
}

const NOOP_LOGGER: FederationIncomingLogger = {debug() {}};
const NOOP_CURSOR: IFederationInboxCursor = {
	async getLastSeq() {
		return null;
	},
	async setLastSeq() {},
};

export interface IFederationIncomingService {
	receive(event: unknown, headers: FederationIncomingHeaders): Promise<FederationIncomingResult>;
}

export class FederationIncomingService implements IFederationIncomingService {
	constructor(
		private readonly config: FederationConfig,
		private readonly repository: IFederationInboxRepository,
		private readonly logger: FederationIncomingLogger = NOOP_LOGGER,
		private readonly onAccepted?: (event: SignedFederationEvent) => Promise<void>,
		private readonly cursor: IFederationInboxCursor = NOOP_CURSOR,
	) {}

	async receive(event: unknown, headers: FederationIncomingHeaders): Promise<FederationIncomingResult> {
		if (!this.config.enabled) {
			return {ok: false, reason: 'federation_disabled'};
		}
		const peer = this.config.peers.find((candidate) => candidate.instanceId === headers.origin);
		if (!peer) {
			return {ok: false, reason: 'unknown_origin'};
		}
		const verification = verifyFederationEvent(event, {publicKey: peer.publicKey});
		if (!verification.ok) {
			return {ok: false, reason: verification.reason};
		}
		const signedEvent = verification.event;
		if (signedEvent.origin !== peer.instanceId) {
			return {ok: false, reason: 'origin_mismatch'};
		}
		const existing = await this.repository.get(signedEvent.origin, signedEvent.event_id);
		if (existing) {
			if (existing.payload_hash !== signedEvent.payload_hash) {
				federationIncomingTotal.inc('result="event_id_conflict"');
				return {ok: false, reason: 'event_id_conflict'};
			}
			federationIncomingTotal.inc('result="duplicate"');
			return {ok: true, eventId: signedEvent.event_id};
		}
		let lastSeq: number | null;
		try {
			lastSeq = await this.cursor.getLastSeq(signedEvent.origin);
		} catch (error) {
			this.logger.debug({err: error, origin: signedEvent.origin}, 'Federation inbox cursor unavailable');
			federationIncomingTotal.inc('result="cursor_unavailable"');
			return {ok: false, reason: 'cursor_unavailable'};
		}
		if (lastSeq !== null && signedEvent.seq <= lastSeq) {
			federationIncomingTotal.inc('result="replayed_seq"');
			return {ok: false, reason: 'replayed_seq'};
		}
		const row: FederationInboxRow = {
			event_id: signedEvent.event_id,
			origin: signedEvent.origin,
			seq: BigInt(signedEvent.seq),
			type: signedEvent.type,
			payload: JSON.stringify(signedEvent.payload),
			payload_hash: signedEvent.payload_hash,
			prev_hash: signedEvent.prev_hash,
			signature: signedEvent.signature,
			key_id: headers.keyId,
			occurred_at: new Date(signedEvent.occurred_at),
			received_at: new Date(),
			status: 'received',
			error_reason: null,
			applied_at: null,
		};
		const inserted = await this.repository.insertIfAbsent(row);
		if (!inserted) {
			federationIncomingTotal.inc('result="duplicate"');
			return {ok: true, eventId: signedEvent.event_id};
		}
		await this.cursor.setLastSeq(signedEvent.origin, signedEvent.seq).catch((error: unknown) => {
			this.logger.debug({err: error, origin: signedEvent.origin}, 'Failed to update federation inbox cursor');
		});
		federationIncomingTotal.inc('result="persisted"');
		this.logger.debug(
			{
				eventId: signedEvent.event_id,
				origin: signedEvent.origin,
				inserted,
			},
			'Received federation event',
		);
		if (this.onAccepted) {
			void this.onAccepted(signedEvent).catch((error: unknown) => {
				this.logger.debug({err: error, eventId: signedEvent.event_id}, 'Federation apply callback failed');
			});
		}
		return {ok: true, eventId: signedEvent.event_id};
	}
}