// SPDX-License-Identifier: AGPL-3.0-or-later

import type {KeyObject} from 'node:crypto';
import {sha256Tagged, verifyFederationBytes} from './FederationCrypto.ts';

export const FEDERATION_PROTOCOL_VERSION = 1;
export const DEFAULT_FEDERATION_CLOCK_SKEW_MS = 5 * 60 * 1000;

const EVENT_ID_PATTERN = /^[A-Za-z0-9_-]{1,128}$/;
const ORIGIN_PATTERN = /^[a-z0-9](?:[a-z0-9._-]*[a-z0-9])?(?::[0-9]{1,5})?$/;
const TIMESTAMP_PATTERN = /^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d{1,3})?Z$/;
const HASH_PATTERN = /^sha256:[0-9a-f]{64}$/;
const SIGNATURE_PATTERN = /^ed25519:[A-Za-z0-9_-]+={0,2}$/;

export type JsonValue = string | number | boolean | null | Array<JsonValue> | {[key: string]: JsonValue};
export type FederationEventPayload = Record<string, JsonValue>;
export type FederationEventType = 'message.create' | 'message.edit' | 'message.delete';

export interface UnsignedFederationEvent {
	v: 1;
	event_id: string;
	origin: string;
	seq: number;
	type: FederationEventType;
	occurred_at: string;
	payload: FederationEventPayload;
	payload_hash: string;
	prev_hash: string | null;
}

export interface SignedFederationEvent extends UnsignedFederationEvent {
	signature: string;
}

export interface CreateFederationEventParams {
	eventId: string;
	origin: string;
	seq: number;
	type: FederationEventType;
	occurredAt: Date | string;
	payload: Record<string, unknown>;
	prevHash?: string | null;
	sign: (bytes: Uint8Array) => string;
}

export interface VerifyFederationEventOptions {
	publicKey: KeyObject;
	now?: Date;
	maxClockSkewMs?: number;
	lastSeq?: number | null;
}

export type FederationEventErrorCode =
	| 'invalid_shape'
	| 'unsupported_version'
	| 'invalid_event_id'
	| 'invalid_origin'
	| 'invalid_seq'
	| 'invalid_type'
	| 'invalid_timestamp'
	| 'invalid_payload'
	| 'invalid_payload_hash'
	| 'invalid_prev_hash'
	| 'invalid_signature';

export type FederationEventVerificationFailureReason =
	| 'invalid_event'
	| 'payload_hash_mismatch'
	| 'clock_skew'
	| 'replayed_seq'
	| 'invalid_signature';

export type FederationEventVerification =
	| {ok: true; event: SignedFederationEvent}
	| {
			ok: false;
			reason: FederationEventVerificationFailureReason;
			detail?: string;
			driftMs?: number;
			lastSeq?: number;
	  };

export class FederationEventError extends Error {
	readonly code: FederationEventErrorCode;

	constructor(code: FederationEventErrorCode, message: string) {
		super(message);
		this.name = 'FederationEventError';
		this.code = code;
	}
}

export function isFederationEventType(value: string): value is FederationEventType {
	return value === 'message.create' || value === 'message.edit' || value === 'message.delete';
}

function isPlainRecord(value: unknown): value is Record<string, unknown> {
	if (typeof value !== 'object' || value === null || Array.isArray(value)) {
		return false;
	}
	const prototype = Object.getPrototypeOf(value);
	return prototype === Object.prototype || prototype === null;
}

export function isValidFederationOrigin(value: string): boolean {
	if (value.length === 0 || value.length > 260) {
		return false;
	}
	return value === value.toLowerCase() && ORIGIN_PATTERN.test(value);
}

function canonicalizeValue(value: unknown, path: string): string {
	if (value === null) {
		return 'null';
	}
	switch (typeof value) {
		case 'boolean':
			return value ? 'true' : 'false';
		case 'number':
			if (!Number.isFinite(value)) {
				throw new FederationEventError('invalid_payload', `Non-finite number at ${path}`);
			}
			return JSON.stringify(value) as string;
		case 'string':
			return JSON.stringify(value) as string;
		case 'object': {
			if (Array.isArray(value)) {
				const members = value.map((entry, index) => canonicalizeValue(entry, `${path}[${index}]`));
				return `[${members.join(',')}]`;
			}
			if (!isPlainRecord(value)) {
				throw new FederationEventError('invalid_payload', `Non-JSON object at ${path}`);
			}
			const keys = Object.keys(value).sort();
			const members = keys.map((key) => {
				const entry = value[key];
				if (entry === undefined) {
					throw new FederationEventError('invalid_payload', `undefined value at ${path}.${key}`);
				}
				return `${JSON.stringify(key) as string}:${canonicalizeValue(entry, `${path}.${key}`)}`;
			});
			return `{${members.join(',')}}`;
		}
		default:
			throw new FederationEventError('invalid_payload', `Unsupported JSON value at ${path}`);
	}
}

export function canonicalizeJson(value: unknown): string {
	return canonicalizeValue(value, '$');
}

export function parseFederationEvent(value: unknown): SignedFederationEvent {
	if (!isPlainRecord(value)) {
		throw new FederationEventError('invalid_shape', 'Federation event must be a JSON object');
	}
	if (value.v !== FEDERATION_PROTOCOL_VERSION) {
		throw new FederationEventError('unsupported_version', `Unsupported federation protocol version: ${value.v}`);
	}
	const eventId = value.event_id;
	if (typeof eventId !== 'string' || !EVENT_ID_PATTERN.test(eventId)) {
		throw new FederationEventError('invalid_event_id', 'event_id must be a ULID-like identifier');
	}
	const origin = value.origin;
	if (typeof origin !== 'string' || !isValidFederationOrigin(origin)) {
		throw new FederationEventError('invalid_origin', 'origin must be a lowercase instance identifier');
	}
	const seq = value.seq;
	if (typeof seq !== 'number' || !Number.isSafeInteger(seq) || seq < 0) {
		throw new FederationEventError('invalid_seq', 'seq must be a non-negative safe integer');
	}
	const type = value.type;
	if (typeof type !== 'string' || !isFederationEventType(type)) {
		throw new FederationEventError('invalid_type', `Unsupported federation event type: ${String(value.type)}`);
	}
	const occurredAt = value.occurred_at;
	if (typeof occurredAt !== 'string' || !TIMESTAMP_PATTERN.test(occurredAt) || Number.isNaN(Date.parse(occurredAt))) {
		throw new FederationEventError('invalid_timestamp', 'occurred_at must be an ISO-8601 UTC timestamp');
	}
	const payload = value.payload;
	if (!isPlainRecord(payload)) {
		throw new FederationEventError('invalid_payload', 'payload must be a JSON object');
	}
	const payloadHash = value.payload_hash;
	if (typeof payloadHash !== 'string' || !HASH_PATTERN.test(payloadHash)) {
		throw new FederationEventError('invalid_payload_hash', 'payload_hash must be a sha256 digest');
	}
	const prevHash = value.prev_hash;
	if (prevHash !== null && prevHash !== undefined && (typeof prevHash !== 'string' || !HASH_PATTERN.test(prevHash))) {
		throw new FederationEventError('invalid_prev_hash', 'prev_hash must be null or a sha256 digest');
	}
	const signature = value.signature;
	if (typeof signature !== 'string' || !SIGNATURE_PATTERN.test(signature)) {
		throw new FederationEventError('invalid_signature', 'signature must be an Ed25519 signature');
	}
	return {
		v: FEDERATION_PROTOCOL_VERSION,
		event_id: eventId,
		origin,
		seq,
		type,
		occurred_at: occurredAt,
		payload: payload as FederationEventPayload,
		payload_hash: payloadHash,
		prev_hash: typeof prevHash === 'string' ? prevHash : null,
		signature,
	};
}

function toUnsignedEvent(event: UnsignedFederationEvent | SignedFederationEvent): UnsignedFederationEvent {
	return {
		v: event.v,
		event_id: event.event_id,
		origin: event.origin,
		seq: event.seq,
		type: event.type,
		occurred_at: event.occurred_at,
		payload: event.payload,
		payload_hash: event.payload_hash,
		prev_hash: event.prev_hash,
	};
}

function toSignedEvent(event: SignedFederationEvent): SignedFederationEvent {
	return {...toUnsignedEvent(event), signature: event.signature};
}

export function federationEventSigningBytes(event: UnsignedFederationEvent | SignedFederationEvent): Uint8Array {
	return Buffer.from(canonicalizeJson(toUnsignedEvent(event)), 'utf8');
}

export function federationEventHash(event: SignedFederationEvent): string {
	return sha256Tagged(canonicalizeJson(toSignedEvent(event)));
}

export function nextFederationPrevHash(previous: SignedFederationEvent | null): string | null {
	return previous ? federationEventHash(previous) : null;
}

export function createFederationEvent(params: CreateFederationEventParams): SignedFederationEvent {
	if (!isPlainRecord(params.payload)) {
		throw new FederationEventError('invalid_payload', 'payload must be a JSON object');
	}
	const occurredAt = params.occurredAt instanceof Date ? params.occurredAt.toISOString() : params.occurredAt;
	const payloadCanonical = canonicalizeJson(params.payload);
	const unsigned: UnsignedFederationEvent = {
		v: FEDERATION_PROTOCOL_VERSION,
		event_id: params.eventId,
		origin: params.origin,
		seq: params.seq,
		type: params.type,
		occurred_at: occurredAt,
		payload: params.payload as FederationEventPayload,
		payload_hash: sha256Tagged(payloadCanonical),
		prev_hash: params.prevHash ?? null,
	};
	const signature = params.sign(federationEventSigningBytes(unsigned));
	return parseFederationEvent({...unsigned, signature});
}

export function verifyFederationEvent(
	value: unknown,
	options: VerifyFederationEventOptions,
): FederationEventVerification {
	let event: SignedFederationEvent;
	try {
		event = parseFederationEvent(value);
	} catch (error) {
		return {
			ok: false,
			reason: 'invalid_event',
			detail: error instanceof Error ? error.message : String(error),
		};
	}
	let payloadCanonical: string;
	try {
		payloadCanonical = canonicalizeJson(event.payload);
	} catch (error) {
		return {
			ok: false,
			reason: 'invalid_event',
			detail: error instanceof Error ? error.message : String(error),
		};
	}
	if (sha256Tagged(payloadCanonical) !== event.payload_hash) {
		return {ok: false, reason: 'payload_hash_mismatch'};
	}
	const occurredAtMs = Date.parse(event.occurred_at);
	const nowMs = options.now?.getTime() ?? Date.now();
	const maxClockSkewMs = options.maxClockSkewMs ?? DEFAULT_FEDERATION_CLOCK_SKEW_MS;
	const driftMs = Math.abs(nowMs - occurredAtMs);
	if (driftMs > maxClockSkewMs) {
		return {ok: false, reason: 'clock_skew', driftMs};
	}
	if (options.lastSeq !== undefined && options.lastSeq !== null && event.seq <= options.lastSeq) {
		return {ok: false, reason: 'replayed_seq', lastSeq: options.lastSeq};
	}
	if (!verifyFederationBytes(federationEventSigningBytes(event), event.signature, options.publicKey)) {
		return {ok: false, reason: 'invalid_signature'};
	}
	return {ok: true, event};
}
