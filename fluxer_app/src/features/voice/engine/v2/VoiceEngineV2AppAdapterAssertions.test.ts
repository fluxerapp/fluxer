// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {
	assertBoolean,
	assertDisconnectReason,
	assertNonEmptyString,
	assertNonNegativeFinite,
	assertNullableObjectLike,
	assertObjectLike,
	assertOptionalNonEmptyString,
	assertPositiveFinite,
	assertVoiceServerUpdateShape,
	hasAnyTerminalTransport,
	isMutedOrDeafened,
	isPermissionDeniedError,
	isReadyToRepublishTrack,
} from './VoiceEngineV2AppAdapterAssertions';

describe('VoiceEngineV2AppAdapterAssertions', () => {
	describe('assertNonEmptyString', () => {
		it('accepts a non-empty string', () => {
			expect(() => assertNonEmptyString('hello', 'field')).not.toThrow();
		});

		it('rejects empty strings', () => {
			expect(() => assertNonEmptyString('', 'field')).toThrow();
		});

		it('rejects non-string values', () => {
			expect(() => assertNonEmptyString(null, 'field')).toThrow();
			expect(() => assertNonEmptyString(undefined, 'field')).toThrow();
			expect(() => assertNonEmptyString(42, 'field')).toThrow();
		});
	});

	describe('assertOptionalNonEmptyString', () => {
		it('accepts null and undefined', () => {
			expect(() => assertOptionalNonEmptyString(null, 'field')).not.toThrow();
			expect(() => assertOptionalNonEmptyString(undefined, 'field')).not.toThrow();
		});

		it('accepts non-empty strings', () => {
			expect(() => assertOptionalNonEmptyString('hello', 'field')).not.toThrow();
		});

		it('rejects empty strings', () => {
			expect(() => assertOptionalNonEmptyString('', 'field')).toThrow();
		});

		it('rejects non-string values', () => {
			expect(() => assertOptionalNonEmptyString(42, 'field')).toThrow();
		});
	});

	describe('assertPositiveFinite', () => {
		it('accepts positive finite numbers', () => {
			expect(() => assertPositiveFinite(1, 'field')).not.toThrow();
			expect(() => assertPositiveFinite(0.1, 'field')).not.toThrow();
		});

		it('rejects zero, negatives, and non-finite values', () => {
			expect(() => assertPositiveFinite(0, 'field')).toThrow();
			expect(() => assertPositiveFinite(-1, 'field')).toThrow();
			expect(() => assertPositiveFinite(Number.POSITIVE_INFINITY, 'field')).toThrow();
			expect(() => assertPositiveFinite(Number.NaN, 'field')).toThrow();
			expect(() => assertPositiveFinite('1', 'field')).toThrow();
		});
	});

	describe('assertNonNegativeFinite', () => {
		it('accepts zero and positives', () => {
			expect(() => assertNonNegativeFinite(0, 'field')).not.toThrow();
			expect(() => assertNonNegativeFinite(1, 'field')).not.toThrow();
		});

		it('rejects negatives and non-finite', () => {
			expect(() => assertNonNegativeFinite(-1, 'field')).toThrow();
			expect(() => assertNonNegativeFinite(Number.NaN, 'field')).toThrow();
		});
	});

	describe('assertBoolean', () => {
		it('accepts true and false', () => {
			expect(() => assertBoolean(true, 'field')).not.toThrow();
			expect(() => assertBoolean(false, 'field')).not.toThrow();
		});

		it('rejects truthy/falsy non-booleans', () => {
			expect(() => assertBoolean(1, 'field')).toThrow();
			expect(() => assertBoolean(0, 'field')).toThrow();
			expect(() => assertBoolean(null, 'field')).toThrow();
		});
	});

	describe('assertObjectLike', () => {
		it('accepts non-null objects', () => {
			expect(() => assertObjectLike({}, 'field')).not.toThrow();
			expect(() => assertObjectLike({a: 1}, 'field')).not.toThrow();
		});

		it('rejects null and non-objects', () => {
			expect(() => assertObjectLike(null, 'field')).toThrow();
			expect(() => assertObjectLike(undefined, 'field')).toThrow();
			expect(() => assertObjectLike(42, 'field')).toThrow();
		});
	});

	describe('assertNullableObjectLike', () => {
		it('accepts null and objects', () => {
			expect(() => assertNullableObjectLike(null, 'field')).not.toThrow();
			expect(() => assertNullableObjectLike({}, 'field')).not.toThrow();
		});

		it('rejects non-object non-null values', () => {
			expect(() => assertNullableObjectLike(42, 'field')).toThrow();
			expect(() => assertNullableObjectLike('str', 'field')).toThrow();
		});
	});

	describe('isMutedOrDeafened', () => {
		it('returns true if any flag set', () => {
			expect(isMutedOrDeafened({serverMute: true, serverDeaf: false, selfDeaf: false})).toBe(true);
			expect(isMutedOrDeafened({serverMute: false, serverDeaf: true, selfDeaf: false})).toBe(true);
			expect(isMutedOrDeafened({serverMute: false, serverDeaf: false, selfDeaf: true})).toBe(true);
		});

		it('returns false when all flags are false', () => {
			expect(isMutedOrDeafened({serverMute: false, serverDeaf: false, selfDeaf: false})).toBe(false);
		});
	});

	describe('isPermissionDeniedError', () => {
		it('matches NotAllowedError and PermissionDeniedError', () => {
			const a = new Error('x');
			a.name = 'NotAllowedError';
			expect(isPermissionDeniedError(a)).toBe(true);
			const b = new Error('x');
			b.name = 'PermissionDeniedError';
			expect(isPermissionDeniedError(b)).toBe(true);
		});

		it('returns false for non-Error and other names', () => {
			expect(isPermissionDeniedError(null)).toBe(false);
			expect(isPermissionDeniedError('NotAllowedError')).toBe(false);
			const c = new Error('x');
			c.name = 'TypeError';
			expect(isPermissionDeniedError(c)).toBe(false);
		});
	});

	describe('assertVoiceServerUpdateShape', () => {
		it('accepts well-shaped voice server update', () => {
			expect(() =>
				assertVoiceServerUpdateShape(
					{
						token: 'tok',
						endpoint: 'endpoint',
						connection_id: 'cid',
						guild_id: 'gid',
						channel_id: 'chid',
					},
					'raw',
				),
			).not.toThrow();
		});

		it('rejects missing required fields', () => {
			expect(() => assertVoiceServerUpdateShape({token: 'tok', endpoint: 'ep'}, 'raw')).toThrow();
		});

		it('rejects wrong types', () => {
			expect(() => assertVoiceServerUpdateShape({token: 5, endpoint: 'ep', connection_id: 'cid'}, 'raw')).toThrow();
		});
	});

	describe('assertDisconnectReason', () => {
		it('accepts known reasons', () => {
			expect(() => assertDisconnectReason('user', 'r')).not.toThrow();
			expect(() => assertDisconnectReason('error', 'r')).not.toThrow();
			expect(() => assertDisconnectReason('server', 'r')).not.toThrow();
		});

		it('rejects unknown reasons', () => {
			expect(() => assertDisconnectReason('unknown', 'r')).toThrow();
			expect(() => assertDisconnectReason(null, 'r')).toThrow();
		});
	});

	describe('hasAnyTerminalTransport', () => {
		it('returns true when any slot is set', () => {
			const sentinel = {};
			expect(hasAnyTerminalTransport({current: {room: sentinel}, hotSwap: {}})).toBe(true);
			expect(hasAnyTerminalTransport({current: {}, hotSwap: {pendingRoom: sentinel}})).toBe(true);
			expect(hasAnyTerminalTransport({current: {}, hotSwap: {previousRoom: sentinel}})).toBe(true);
		});

		it('returns false when no transports are present', () => {
			expect(hasAnyTerminalTransport({current: {}, hotSwap: {}})).toBe(false);
		});
	});

	describe('isReadyToRepublishTrack', () => {
		it('returns true when track has a live media stream', () => {
			expect(isReadyToRepublishTrack({mediaStreamTrack: {readyState: 'live'}})).toBe(true);
		});

		it('returns false when track is null or undefined', () => {
			expect(isReadyToRepublishTrack(null)).toBe(false);
			expect(isReadyToRepublishTrack(undefined)).toBe(false);
		});

		it('returns false when media stream is missing or ended', () => {
			expect(isReadyToRepublishTrack({mediaStreamTrack: null})).toBe(false);
			expect(isReadyToRepublishTrack({mediaStreamTrack: {readyState: 'ended'}})).toBe(false);
		});
	});
});
