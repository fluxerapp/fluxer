// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	encodeRecoveryKey,
	formatRecoveryKey,
	formatRecoveryKeyInput,
	generateRecoveryKey,
	normalizeRecoveryKey,
	RECOVERY_KEY_LENGTH,
} from '@fluxer/constants/src/RecoveryKeyUtils';
import {describe, expect, it} from 'vitest';

function bytesFrom(values: Array<number>): Uint8Array {
	return Uint8Array.from(values);
}

describe('encodeRecoveryKey', () => {
	it('encodes 160 bits as 32 Crockford base32 characters', () => {
		expect(encodeRecoveryKey(new Uint8Array(20))).toBe('0'.repeat(32));
		expect(encodeRecoveryKey(new Uint8Array(20).fill(0xff))).toBe('Z'.repeat(32));
		expect(encodeRecoveryKey(bytesFrom([0x08, 0x86, 0x42, 0x98, 0xe8, ...new Array(15).fill(0)]))).toBe(
			`12345678${'0'.repeat(24)}`,
		);
	});

	it('rejects input that is not exactly 20 bytes', () => {
		expect(() => encodeRecoveryKey(new Uint8Array(19))).toThrow();
		expect(() => encodeRecoveryKey(new Uint8Array(21))).toThrow();
	});
});

describe('generateRecoveryKey', () => {
	it('returns the raw key and its display form', () => {
		const {key, formatted} = generateRecoveryKey(new Uint8Array(20).fill(0xa5));
		expect(key).toHaveLength(RECOVERY_KEY_LENGTH);
		expect(formatted).toHaveLength(39);
		expect(formatted.split('-')).toHaveLength(8);
		expect(formatted.replaceAll('-', '')).toBe(key);
		expect(normalizeRecoveryKey(formatted)).toBe(key);
	});
});

describe('normalizeRecoveryKey', () => {
	const key = 'ABCD0123EFGH4567JKMN89PQRSTVWXYZ';

	it('accepts the display form, lower case, spaces and dashes', () => {
		expect(normalizeRecoveryKey(formatRecoveryKey(key))).toBe(key);
		expect(normalizeRecoveryKey(formatRecoveryKey(key).toLowerCase())).toBe(key);
		expect(normalizeRecoveryKey(` ${formatRecoveryKey(key).replaceAll('-', ' ')} `)).toBe(key);
	});

	it('maps O to 0 and I or L to 1', () => {
		expect(normalizeRecoveryKey('OOOO-IIII-LLLL-oooo-iiii-llll-0000-1111')).toBe(
			`${'0'.repeat(4)}${'1'.repeat(8)}${'0'.repeat(4)}${'1'.repeat(8)}${'0'.repeat(4)}${'1'.repeat(4)}`,
		);
	});

	it('rejects U, other symbols and the wrong length', () => {
		expect(normalizeRecoveryKey(`U${key.slice(1)}`)).toBeNull();
		expect(normalizeRecoveryKey(`!${key.slice(1)}`)).toBeNull();
		expect(normalizeRecoveryKey(key.slice(1))).toBeNull();
		expect(normalizeRecoveryKey(`${key}0`)).toBeNull();
	});

	it('rejects characters whose uppercase form is two letters', () => {
		expect(normalizeRecoveryKey(`\uFB06${key.slice(1)}`)).toBeNull();
		expect(normalizeRecoveryKey(`\uFB05${key.slice(1)}`)).toBeNull();
	});
});

describe('formatRecoveryKeyInput', () => {
	it('groups partial input as the user types', () => {
		expect(formatRecoveryKeyInput('abcd0')).toBe('ABCD-0');
		expect(formatRecoveryKeyInput('ab-cd 01o')).toBe('ABCD-010');
		expect(formatRecoveryKeyInput('abcu')).toBe('ABC');
	});

	it('stops at the full key length', () => {
		expect(formatRecoveryKeyInput('0'.repeat(40))).toBe(formatRecoveryKey('0'.repeat(32)));
	});

	it('skips ligatures that upper-case to two letters', () => {
		expect(formatRecoveryKeyInput(`${'0'.repeat(31)}\uFB06${'1'.repeat(8)}`)).toBe(
			formatRecoveryKey(`${'0'.repeat(31)}1`),
		);
	});
});
