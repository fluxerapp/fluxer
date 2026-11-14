// SPDX-License-Identifier: AGPL-3.0-or-later

import {GuildVerificationLevel} from '@fluxer/constants/src/GuildConstants';
import {
	GUILD_VERIFICATION_MIN_ACCOUNT_AGE_MS,
	GUILD_VERIFICATION_MIN_MEMBERSHIP_MS,
	GuildVerificationWaitReason,
	getGuildVerificationWait,
	isExemptFromGuildVerification,
} from '@fluxer/constants/src/GuildVerificationRules';
import {describe, expect, it} from 'vitest';

const NOW = 1_800_000_000_000;

describe('isExemptFromGuildVerification', () => {
	const base = {verificationLevel: GuildVerificationLevel.HIGH, isOwner: false, isBot: false, hasRoles: false};

	it('exempts the owner, bots, members with a role and guilds without a level', () => {
		expect(isExemptFromGuildVerification(base)).toBe(false);
		expect(isExemptFromGuildVerification({...base, isOwner: true})).toBe(true);
		expect(isExemptFromGuildVerification({...base, isBot: true})).toBe(true);
		expect(isExemptFromGuildVerification({...base, hasRoles: true})).toBe(true);
		expect(isExemptFromGuildVerification({...base, verificationLevel: GuildVerificationLevel.NONE})).toBe(true);
	});
});

describe('getGuildVerificationWait', () => {
	const fresh = {accountCreatedAt: NOW - 1000, memberJoinedAt: NOW - 1000, now: NOW};

	it('waits on account age from MEDIUM and on membership from HIGH', () => {
		expect(getGuildVerificationWait({...fresh, verificationLevel: GuildVerificationLevel.LOW})).toBeNull();
		expect(getGuildVerificationWait({...fresh, verificationLevel: GuildVerificationLevel.MEDIUM})).toEqual({
			reason: GuildVerificationWaitReason.ACCOUNT_TOO_NEW,
			endsAt: fresh.accountCreatedAt + GUILD_VERIFICATION_MIN_ACCOUNT_AGE_MS,
		});
		const oldAccount = {...fresh, accountCreatedAt: NOW - GUILD_VERIFICATION_MIN_ACCOUNT_AGE_MS};
		expect(getGuildVerificationWait({...oldAccount, verificationLevel: GuildVerificationLevel.MEDIUM})).toBeNull();
		expect(getGuildVerificationWait({...oldAccount, verificationLevel: GuildVerificationLevel.HIGH})).toEqual({
			reason: GuildVerificationWaitReason.NOT_MEMBER_LONG_ENOUGH,
			endsAt: fresh.memberJoinedAt + GUILD_VERIFICATION_MIN_MEMBERSHIP_MS,
		});
	});

	it('skips the membership wait without a join date', () => {
		const oldAccount = {accountCreatedAt: 0, memberJoinedAt: null, now: NOW};
		expect(getGuildVerificationWait({...oldAccount, verificationLevel: GuildVerificationLevel.HIGH})).toBeNull();
	});
});
