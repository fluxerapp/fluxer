// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	DEFERRED_PHONE_ON_COMMUNITY_JOIN,
	imposePhoneRequirements,
	PremiumFlags,
	SuspiciousActivityFlags,
	UserPremiumTypes,
} from '@fluxer/constants/src/UserConstants';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';
import type {User} from '../models/User';
import {setInjectedAccountPolicyEvaluator} from '../risk/AccountPolicyService';
import {setCachedDeferredPhoneGateEnabled} from '../risk/DeferredPhoneGateCache';
import {
	createCurrentBehaviorTestAccountPolicyEvaluator,
	TEST_POLICY_CONTACT_DOMAIN,
	TEST_POLICY_CONTACT_SUBDOMAIN,
} from '../test/AccountPolicyTestEvaluator';
import {checkIsPremium, getEffectivePremiumUntil, getEffectiveSuspiciousFlags, getRequiredActions} from './UserHelpers';

function createUser(
	overrides: Partial<Pick<User, 'email' | 'emailVerified' | 'hasVerifiedPhone' | 'suspiciousActivityFlags'>> = {},
): User {
	return {
		email: 'user@fluxer.app',
		emailVerified: false,
		hasVerifiedPhone: false,
		suspiciousActivityFlags: 0,
		...overrides,
	} as User;
}

describe('deferred phone gate marker', () => {
	beforeEach(() => {
		setInjectedAccountPolicyEvaluator(createCurrentBehaviorTestAccountPolicyEvaluator());
		setCachedDeferredPhoneGateEnabled(true);
	});
	afterEach(() => {
		setInjectedAccountPolicyEvaluator(undefined);
		setCachedDeferredPhoneGateEnabled(false);
	});
	it('does not suppress anything until a policy read has proven the gate is on', () => {
		setCachedDeferredPhoneGateEnabled(false);
		const user = createUser({
			suspiciousActivityFlags: SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE | DEFERRED_PHONE_ON_COMMUNITY_JOIN,
		});
		expect(getRequiredActions(user)).toEqual(['REQUIRE_VERIFIED_PHONE']);
	});
	it('suppresses a deferred phone requirement so the account is not locked out', () => {
		const user = createUser({
			suspiciousActivityFlags: SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE | DEFERRED_PHONE_ON_COMMUNITY_JOIN,
		});
		expect(getRequiredActions(user)).toEqual([]);
		expect(getEffectiveSuspiciousFlags(user)).toBe(0);
	});
	it('never lets an inbound-SMS requirement be suppressed, since that tier is never deferred', () => {
		const user = createUser({
			suspiciousActivityFlags:
				SuspiciousActivityFlags.REQUIRE_INBOUND_PHONE_VERIFICATION | DEFERRED_PHONE_ON_COMMUNITY_JOIN,
		});
		expect(getRequiredActions(user)).toEqual(['REQUIRE_VERIFIED_PHONE', 'REQUIRE_INBOUND_PHONE_VERIFICATION']);
	});
	it('keeps non-phone requirements active while a phone requirement is deferred', () => {
		const user = createUser({
			suspiciousActivityFlags:
				SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL |
				SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE |
				DEFERRED_PHONE_ON_COMMUNITY_JOIN,
		});
		expect(getRequiredActions(user)).toEqual(['REQUIRE_VERIFIED_EMAIL']);
		expect(getEffectiveSuspiciousFlags(user)).toBe(SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL);
	});
	it('applies the phone requirement in full once the marker is cleared', () => {
		const user = createUser({
			suspiciousActivityFlags: SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE,
		});
		expect(getRequiredActions(user)).toEqual(['REQUIRE_VERIFIED_PHONE']);
		expect(getEffectiveSuspiciousFlags(user)).toBe(SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE);
	});
	it('leaves an account carrying only the marker completely unrestricted', () => {
		const user = createUser({suspiciousActivityFlags: DEFERRED_PHONE_ON_COMMUNITY_JOIN});
		expect(getRequiredActions(user)).toEqual([]);
		expect(getEffectiveSuspiciousFlags(user)).toBe(0);
	});
	it('re-arms stored phone requirements as soon as the gate is switched off', () => {
		const user = createUser({
			suspiciousActivityFlags: SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE | DEFERRED_PHONE_ON_COMMUNITY_JOIN,
		});
		expect(getRequiredActions(user)).toEqual([]);
		setCachedDeferredPhoneGateEnabled(false);
		expect(getRequiredActions(user)).toEqual(['REQUIRE_VERIFIED_PHONE']);
	});
	it('stops suppressing once another subsystem imposes the phone requirement directly', () => {
		const deferred = SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE | DEFERRED_PHONE_ON_COMMUNITY_JOIN;
		const imposed = imposePhoneRequirements(deferred, SuspiciousActivityFlags.REQUIRE_REVERIFIED_PHONE);
		expect(imposed & DEFERRED_PHONE_ON_COMMUNITY_JOIN).toBe(0);
		const user = createUser({suspiciousActivityFlags: imposed});
		expect(getRequiredActions(user)).toEqual(['REQUIRE_REVERIFIED_PHONE']);
	});
	it('keeps the marker when a non-phone requirement is imposed', () => {
		const deferred = SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE | DEFERRED_PHONE_ON_COMMUNITY_JOIN;
		const imposed = imposePhoneRequirements(deferred, SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL);
		expect(imposed & DEFERRED_PHONE_ON_COMMUNITY_JOIN).not.toBe(0);
		expect(getRequiredActions(createUser({suspiciousActivityFlags: imposed}))).toEqual(['REQUIRE_VERIFIED_EMAIL']);
	});
	it('yields no enforceable requirement for an account without an email, so the gate must not promote it', () => {
		const user = createUser({
			email: null,
			suspiciousActivityFlags: SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE,
		});
		expect(getEffectiveSuspiciousFlags(user)).toBe(0);
	});
});

describe('getRequiredActions', () => {
	beforeEach(() => {
		setInjectedAccountPolicyEvaluator(createCurrentBehaviorTestAccountPolicyEvaluator());
	});
	afterEach(() => {
		setInjectedAccountPolicyEvaluator(undefined);
	});
	it('keeps verified-email requirements active when the account email is unverified', () => {
		const user = createUser({
			suspiciousActivityFlags: SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL,
		});
		expect(getRequiredActions(user)).toEqual(['REQUIRE_VERIFIED_EMAIL']);
		expect(getEffectiveSuspiciousFlags(user)).toBe(SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL);
	});
	it('masks verified-email requirements when the account has a verified email', () => {
		const user = createUser({
			emailVerified: true,
			suspiciousActivityFlags:
				SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL |
				SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL_OR_VERIFIED_PHONE,
		});
		expect(getRequiredActions(user)).toEqual([]);
		expect(getEffectiveSuspiciousFlags(user)).toBe(0);
	});
	it('drops weaker redundant clauses while preserving the stronger canonical action', () => {
		const user = createUser({
			suspiciousActivityFlags:
				SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL_OR_VERIFIED_PHONE |
				SuspiciousActivityFlags.REQUIRE_REVERIFIED_EMAIL_OR_VERIFIED_PHONE,
		});
		expect(getRequiredActions(user)).toEqual(['REQUIRE_REVERIFIED_EMAIL_OR_VERIFIED_PHONE']);
		expect(getEffectiveSuspiciousFlags(user)).toBe(SuspiciousActivityFlags.REQUIRE_REVERIFIED_EMAIL_OR_VERIFIED_PHONE);
	});
	it('retains incomparable combinations so the client can complete them sequentially', () => {
		const user = createUser({
			suspiciousActivityFlags:
				SuspiciousActivityFlags.REQUIRE_REVERIFIED_EMAIL | SuspiciousActivityFlags.REQUIRE_REVERIFIED_PHONE,
		});
		expect(getRequiredActions(user)).toEqual(['REQUIRE_REVERIFIED_EMAIL', 'REQUIRE_REVERIFIED_PHONE']);
		expect(getEffectiveSuspiciousFlags(user)).toBe(
			SuspiciousActivityFlags.REQUIRE_REVERIFIED_EMAIL | SuspiciousActivityFlags.REQUIRE_REVERIFIED_PHONE,
		);
	});
	it('masks verified-phone requirements after the stored phone has been removed', () => {
		const user = createUser({
			hasVerifiedPhone: true,
			suspiciousActivityFlags: SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE,
		});
		expect(getRequiredActions(user)).toEqual([]);
		expect(getEffectiveSuspiciousFlags(user)).toBe(0);
	});
	it('masks all suspicious activity requirements for policy-exempt contact domains', () => {
		const user = createUser({
			email: `builder@${TEST_POLICY_CONTACT_DOMAIN}`,
			suspiciousActivityFlags:
				SuspiciousActivityFlags.REQUIRE_REVERIFIED_EMAIL |
				SuspiciousActivityFlags.REQUIRE_REVERIFIED_PHONE |
				SuspiciousActivityFlags.REQUIRE_INBOUND_PHONE_VERIFICATION,
		});
		expect(getRequiredActions(user)).toEqual([]);
		expect(getEffectiveSuspiciousFlags(user)).toBe(0);
	});
	it('does not mask suspicious activity requirements for non-matching subdomains', () => {
		const user = createUser({
			email: `builder@${TEST_POLICY_CONTACT_SUBDOMAIN}`,
			suspiciousActivityFlags: SuspiciousActivityFlags.REQUIRE_REVERIFIED_EMAIL,
		});
		expect(getRequiredActions(user)).toEqual(['REQUIRE_REVERIFIED_EMAIL']);
		expect(getEffectiveSuspiciousFlags(user)).toBe(SuspiciousActivityFlags.REQUIRE_REVERIFIED_EMAIL);
	});
});

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
