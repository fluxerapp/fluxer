// SPDX-License-Identifier: AGPL-3.0-or-later

import {PhonePrefixBanCache} from '@app/api/auth/PhonePrefixBanCache';
import {beforeEach, describe, expect, it} from 'vitest';

describe('PhonePrefixBanCache', () => {
	let cache: PhonePrefixBanCache;
	beforeEach(() => {
		cache = new PhonePrefixBanCache();
	});
	it('matches phones that start with a banned prefix', () => {
		cache.ban('+31970');
		expect(cache.isBlocked('+31970123456')).toBe(true);
		expect(cache.isBlocked('+3197058046507')).toBe(true);
		expect(cache.isBlocked('+3197058046509')).toBe(true);
	});
	it('does not match phones outside the banned prefix', () => {
		cache.ban('+31970');
		expect(cache.isBlocked('+15551234567')).toBe(false);
		expect(cache.isBlocked('+31612345678')).toBe(false);
		expect(cache.isBlocked('+31201234567')).toBe(false);
	});
	it('is empty by default — no prefixes block anything until ban() is called', () => {
		expect(cache.isBlocked('+31970123456')).toBe(false);
		expect(cache.isBlocked('+15551234567')).toBe(false);
	});
	it('unban() removes a prefix from the active set', () => {
		cache.ban('+31970');
		expect(cache.isBlocked('+31970123456')).toBe(true);
		cache.unban('+31970');
		expect(cache.isBlocked('+31970123456')).toBe(false);
	});
	it('supports multiple simultaneous prefixes', () => {
		cache.ban('+31970');
		cache.ban('+15550');
		expect(cache.isBlocked('+31970123456')).toBe(true);
		expect(cache.isBlocked('+15550000000')).toBe(true);
		expect(cache.isBlocked('+15551234567')).toBe(false);
	});
	it('only matches at the start of the string (prefix semantics, not substring)', () => {
		cache.ban('+31970');
		expect(cache.isBlocked('+1555031970123')).toBe(false);
	});
	it('ignores empty-string bans', () => {
		cache.ban('');
		expect(cache.isBlocked('+31970123456')).toBe(false);
		expect(cache.isBlocked('+15551234567')).toBe(false);
	});
	it('ban() is idempotent — double-banning the same prefix is a no-op', () => {
		cache.ban('+31970');
		cache.ban('+31970');
		expect(cache.snapshot().size).toBe(1);
		cache.unban('+31970');
		expect(cache.snapshot().size).toBe(0);
	});
	it('snapshot() exposes the live set for exact-membership checks', () => {
		cache.ban('+31970');
		cache.ban('+15550');
		const snapshot = cache.snapshot();
		expect(snapshot.has('+31970')).toBe(true);
		expect(snapshot.has('+15550')).toBe(true);
		expect(snapshot.has('+31971')).toBe(false);
	});
	describe('length-bucketing optimization', () => {
		it('matches against prefixes of multiple distinct lengths', () => {
			cache.ban('+1');
			cache.ban('+31970');
			cache.ban('+449876');
			expect(cache.isBlocked('+15551234567')).toBe(true);
			expect(cache.isBlocked('+31970123456')).toBe(true);
			expect(cache.isBlocked('+449876543210')).toBe(true);
			expect(cache.isBlocked('+449100000000')).toBe(false);
			expect(cache.isBlocked('+31612345678')).toBe(false);
		});
		it('returns false immediately when the phone is shorter than the shortest banned prefix', () => {
			cache.ban('+31970');
			expect(cache.isBlocked('+31')).toBe(false);
			expect(cache.isBlocked('+319')).toBe(false);
			expect(cache.isBlocked('+3197')).toBe(false);
			expect(cache.isBlocked('+31970')).toBe(true);
			expect(cache.isBlocked('+319701')).toBe(true);
		});
		it('early-exits once prefix length exceeds phone length', () => {
			cache.ban('+49123456');
			cache.ban('+12345678');
			expect(cache.isBlocked('+12345')).toBe(false);
		});
		it('rebuilds length index when unban removes the last prefix of a given length', () => {
			cache.ban('+1');
			cache.ban('+44');
			cache.ban('+31970');
			expect(cache.isBlocked('+31970123')).toBe(true);
			cache.unban('+31970');
			expect(cache.isBlocked('+31970123')).toBe(false);
			expect(cache.isBlocked('+15551234567')).toBe(true);
			expect(cache.isBlocked('+441234567890')).toBe(true);
		});
		it('handles phones that match only at a deep length', () => {
			cache.ban('+31');
			cache.ban('+31970');
			expect(cache.isBlocked('+31970123')).toBe(true);
			cache.unban('+31');
			expect(cache.isBlocked('+31970123')).toBe(true);
			expect(cache.isBlocked('+31612345678')).toBe(false);
		});
	});
});
