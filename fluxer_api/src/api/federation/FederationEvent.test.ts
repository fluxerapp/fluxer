// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type FederationKeyPair,
	generateFederationKeyPair,
	sha256Hex,
	signFederationBytes,
} from '@app/api/federation/FederationCrypto';
import {
	canonicalizeJson,
	createFederationEvent,
	DEFAULT_FEDERATION_CLOCK_SKEW_MS,
	FederationEventError,
	federationEventHash,
	nextFederationPrevHash,
	parseFederationEvent,
	type SignedFederationEvent,
	verifyFederationEvent,
} from '@app/api/federation/FederationEvent';
import {describe, expect, it} from 'vitest';

const NOW = new Date('2026-11-24T17:00:00.000Z');
const INSTANCE_A = generateFederationKeyPair();
const INSTANCE_B = generateFederationKeyPair();

const MESSAGE_PAYLOAD = {
	guild: {origin: 'chat-a.example.org', id: '100'},
	channel: {origin: 'chat-a.example.org', id: '200'},
	message: {
		origin: 'chat-a.example.org',
		id: '300',
		author: {handle: 'ana@chat-a.example.org', display_name: 'Ana'},
		content: 'hola',
		created_at: NOW.toISOString(),
		reply_to: null,
	},
};

function buildMessageEvent(params: {
	instance: FederationKeyPair;
	seq?: number;
	occurredAt?: Date | string;
	prevHash?: string | null;
	payload?: Record<string, unknown>;
}): SignedFederationEvent {
	const seq = params.seq ?? 1;
	return createFederationEvent({
		eventId: `01JTEST${seq.toString().padStart(4, '0')}`,
		origin: 'chat-a.example.org',
		seq,
		type: 'message.create',
		occurredAt: params.occurredAt ?? NOW,
		payload: params.payload ?? MESSAGE_PAYLOAD,
		prevHash: params.prevHash,
		sign: (bytes) => signFederationBytes(bytes, params.instance.privateKey),
	});
}

function parseJsonEvent(event: SignedFederationEvent): Record<string, unknown> {
	return JSON.parse(JSON.stringify(event)) as Record<string, unknown>;
}

function alterLastHexDigit(hash: string): string {
	const last = hash.at(-1);
	return `${hash.slice(0, -1)}${last === '0' ? '1' : '0'}`;
}

describe('canonicalizeJson', () => {
	it('sorts object keys recursively and preserves array order', () => {
		expect(canonicalizeJson({b: 1, a: 2})).toBe('{"a":2,"b":1}');
		expect(canonicalizeJson({b: {d: 4, c: 3}, a: [3, 1, 2]})).toBe('{"a":[3,1,2],"b":{"c":3,"d":4}}');
	});

	it('produces the same bytes for records with different insertion order', () => {
		const first = {alpha: {two: 2, one: 1}, beta: true};
		const second = {beta: true, alpha: {one: 1, two: 2}};
		expect(canonicalizeJson(first)).toBe(canonicalizeJson(second));
	});

	it('serializes strings, unicode and numbers like JSON', () => {
		const value = {a: 'ñ"\n\t', b: -0, c: 1e21, d: 0.1};
		expect(canonicalizeJson(value)).toBe('{"a":"ñ\\"\\n\\t","b":0,"c":1e+21,"d":0.1}');
	});

	it('rejects values that are not valid JSON', () => {
		expect(() => canonicalizeJson({a: undefined})).toThrow(FederationEventError);
		expect(() => canonicalizeJson({a: Number.NaN})).toThrow(FederationEventError);
		expect(() => canonicalizeJson({a: Number.POSITIVE_INFINITY})).toThrow(FederationEventError);
		expect(() => canonicalizeJson({a: 1n})).toThrow(FederationEventError);
		expect(() => canonicalizeJson({a: new Date()})).toThrow(FederationEventError);
		expect(() => canonicalizeJson({a: () => undefined})).toThrow(FederationEventError);
	});
});

describe('federated event envelope', () => {
	it('creates an event that another instance can verify', () => {
		const event = buildMessageEvent({instance: INSTANCE_A});
		const result = verifyFederationEvent(event, {publicKey: INSTANCE_A.publicKey, now: NOW});

		expect(result.ok).toBe(true);
		if (result.ok) {
			expect(result.event.origin).toBe('chat-a.example.org');
			expect(result.event.payload).toEqual(MESSAGE_PAYLOAD);
			expect(result.event.payload_hash.startsWith('sha256:')).toBe(true);
			expect(result.event.signature.startsWith('ed25519:')).toBe(true);
		}
	});

	it('verifies after a JSON transport round-trip', () => {
		const event = buildMessageEvent({instance: INSTANCE_A});
		const transported = JSON.parse(JSON.stringify(event));
		const result = verifyFederationEvent(transported, {publicKey: INSTANCE_A.publicKey, now: NOW});

		expect(result.ok).toBe(true);
	});

	it('rejects a payload altered in transit', () => {
		const event = buildMessageEvent({instance: INSTANCE_A});
		const tampered = parseJsonEvent(event);
		(tampered.payload as typeof MESSAGE_PAYLOAD).message.content = 'contenido alterado';

		expect(verifyFederationEvent(tampered, {publicKey: INSTANCE_A.publicKey, now: NOW})).toEqual({
			ok: false,
			reason: 'payload_hash_mismatch',
		});
	});

	it('rejects a payload altered in transit even when the hash is recomputed', () => {
		const event = buildMessageEvent({instance: INSTANCE_A});
		const tampered = parseJsonEvent(event);
		(tampered.payload as typeof MESSAGE_PAYLOAD).message.content = 'contenido alterado';
		tampered.payload_hash = `sha256:${sha256Hex(canonicalizeJson(tampered.payload))}`;

		expect(verifyFederationEvent(tampered, {publicKey: INSTANCE_A.publicKey, now: NOW})).toEqual({
			ok: false,
			reason: 'invalid_signature',
		});
	});

	it('rejects an event signed by a different instance', () => {
		const event = buildMessageEvent({instance: INSTANCE_A});
		const result = verifyFederationEvent(event, {publicKey: INSTANCE_B.publicKey, now: NOW});

		expect(result).toEqual({ok: false, reason: 'invalid_signature'});
	});

	it('rejects events outside the clock skew window', () => {
		const event = buildMessageEvent({instance: INSTANCE_A});
		const now = new Date(NOW.getTime() + DEFAULT_FEDERATION_CLOCK_SKEW_MS + 1);
		const result = verifyFederationEvent(event, {publicKey: INSTANCE_A.publicKey, now});

		expect(result.ok).toBe(false);
		if (!result.ok) {
			expect(result.reason).toBe('clock_skew');
			expect(result.driftMs).toBe(DEFAULT_FEDERATION_CLOCK_SKEW_MS + 1);
		}
	});

	it('rejects a sequence that does not advance the cursor', () => {
		const event = buildMessageEvent({instance: INSTANCE_A, seq: 7});
		const result = verifyFederationEvent(event, {publicKey: INSTANCE_A.publicKey, now: NOW, lastSeq: 7});

		expect(result).toEqual({ok: false, reason: 'replayed_seq', lastSeq: 7});
	});

	it('chains events with prev_hash', () => {
		const first = buildMessageEvent({instance: INSTANCE_A, seq: 1});
		const second = buildMessageEvent({
			instance: INSTANCE_A,
			seq: 2,
			prevHash: nextFederationPrevHash(first),
		});

		expect(second.prev_hash).toBe(federationEventHash(first));
		expect(verifyFederationEvent(second, {publicKey: INSTANCE_A.publicKey, now: NOW}).ok).toBe(true);

		const broken = {...second, prev_hash: alterLastHexDigit(federationEventHash(first))};
		expect(verifyFederationEvent(broken, {publicKey: INSTANCE_A.publicKey, now: NOW})).toEqual({
			ok: false,
			reason: 'invalid_signature',
		});
	});

	it('rejects malformed envelopes', () => {
		const event = buildMessageEvent({instance: INSTANCE_A});

		expect(() => parseFederationEvent(null)).toThrow(FederationEventError);
		expect(() => parseFederationEvent({...event, v: 2})).toThrow(/version/);
		expect(() => parseFederationEvent({...event, origin: 'Chat A'})).toThrow(/origin/);
		expect(() => parseFederationEvent({...event, seq: -1})).toThrow(/seq/);
		expect(() => parseFederationEvent({...event, type: 'unknown.event'})).toThrow(/type/);
		expect(() => parseFederationEvent({...event, occurred_at: 'ayer'})).toThrow(/timestamp/);
		expect(() => parseFederationEvent({...event, payload: []})).toThrow(/payload/);
		expect(() => parseFederationEvent({...event, payload_hash: 'md5:abc'})).toThrow(/payload_hash/);
		expect(() => parseFederationEvent({...event, prev_hash: 'sha256:abc'})).toThrow(/prev_hash/);
		expect(() => parseFederationEvent({...event, signature: 'rsa:abc'})).toThrow(/signature/);
	});

	it('rejects malformed events during verification without throwing', () => {
		const result = verifyFederationEvent({v: 1}, {publicKey: INSTANCE_A.publicKey, now: NOW});

		expect(result.ok).toBe(false);
		if (!result.ok) {
			expect(result.reason).toBe('invalid_event');
			expect(result.detail).toBeTruthy();
		}
	});
});
