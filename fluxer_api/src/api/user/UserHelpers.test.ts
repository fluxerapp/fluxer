// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	canOwnerRunBots,
	checkIsPremium,
	getEffectivePremiumUntil,
	getPremiumPaymentRecoveryGraceMs,
	isSignInRefused,
	isTemporarilyBanned,
	PREMIUM_GRACE_PERIOD_MS,
} from '@app/api/user/UserHelpers';
import {PremiumFlags, UserFlags, UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {describe, expect, it} from 'vitest';

describe('checkIsPremium', () => {
	it('uses the later gift extension as the effective premium end', () => {
		const premiumUntil = new Date(Date.now() + 60_000);
		const premiumGiftExtensionEndsAt = new Date(Date.now() + 120_000);
		expect(getEffectivePremiumUntil({premiumUntil, premiumGiftExtensionEndsAt})?.toISOString()).toBe(
			premiumGiftExtensionEndsAt.toISOString(),
		);
	});
	it('treats a future gift extension as active after the subscription period ended', () => {
		const user = {
			isBot: false,
			premiumType: UserPremiumTypes.SUBSCRIPTION,
			premiumUntil: new Date(Date.now() - 60_000),
			premiumGiftExtensionEndsAt: new Date(Date.now() + 60_000),
			premiumGraceEndsAt: null,
			premiumWillCancel: false,
			flags: 0n,
			premiumFlags: 0,
		};
		expect(checkIsPremium(user)).toBe(true);
	});
	it('lets the perks-disabled flag override an active paid subscription', () => {
		const user = {
			isBot: false,
			premiumType: UserPremiumTypes.SUBSCRIPTION,
			premiumUntil: new Date(Date.now() + 60_000),
			premiumGiftExtensionEndsAt: null,
			premiumGraceEndsAt: null,
			premiumWillCancel: false,
			flags: 0n,
			premiumFlags: PremiumFlags.PERKS_DISABLED,
		};
		expect(checkIsPremium(user)).toBe(false);
	});
	it('lets the perks-disabled flag override backend premium override', () => {
		const user = {
			isBot: false,
			premiumType: UserPremiumTypes.NONE,
			premiumUntil: null,
			premiumGiftExtensionEndsAt: null,
			premiumGraceEndsAt: null,
			premiumWillCancel: false,
			flags: 0n,
			premiumFlags: PremiumFlags.ENABLED_OVERRIDE | PremiumFlags.PERKS_DISABLED,
		};
		expect(checkIsPremium(user)).toBe(false);
	});
});

describe('account standing', () => {
	const hour = 3_600_000;
	function standing(flags: bigint, tempBannedUntil: Date | null = null, deletionStartedAt: Date | null = null) {
		return {flags, tempBannedUntil, deletionStartedAt};
	}
	it('treats an active temporary ban as a refused sign-in', () => {
		const banned = standing(UserFlags.DISABLED, new Date(Date.now() + hour));
		expect(isTemporarilyBanned(banned)).toBe(true);
		expect(isSignInRefused(banned)).toBe(true);
		expect(canOwnerRunBots(banned)).toBe(false);
	});
	it('lets an expired temporary ban through', () => {
		const expired = standing(UserFlags.DISABLED, new Date(Date.now() - hour));
		expect(isTemporarilyBanned(expired)).toBe(false);
		expect(isSignInRefused(expired)).toBe(false);
		expect(canOwnerRunBots(expired)).toBe(true);
	});
	it('keeps a self-disabled account able to sign in but stops its bots', () => {
		const disabled = standing(UserFlags.DISABLED);
		expect(isSignInRefused(disabled)).toBe(false);
		expect(canOwnerRunBots(disabled)).toBe(false);
	});
	it('refuses closed accounts', () => {
		expect(isSignInRefused(standing(UserFlags.DELETED))).toBe(true);
		expect(isSignInRefused(standing(0n, null, new Date()))).toBe(true);
		expect(canOwnerRunBots(standing(UserFlags.DELETED))).toBe(false);
	});
	it('accepts an account in good standing', () => {
		expect(isSignInRefused(standing(0n))).toBe(false);
		expect(canOwnerRunBots(standing(0n))).toBe(true);
	});
});

describe('premium grace lengths', () => {
	it('maps billing cycles to payment recovery grace', () => {
		const day = 24 * 60 * 60 * 1000;
		expect(getPremiumPaymentRecoveryGraceMs('monthly')).toBe(7 * day);
		expect(getPremiumPaymentRecoveryGraceMs('yearly')).toBe(14 * day);
		expect(getPremiumPaymentRecoveryGraceMs(null)).toBe(7 * day);
		expect(getPremiumPaymentRecoveryGraceMs(undefined)).toBe(7 * day);
	});

	it('keeps the fallback grace at 3 days', () => {
		expect(PREMIUM_GRACE_PERIOD_MS).toBe(3 * 24 * 60 * 60 * 1000);
	});
});
