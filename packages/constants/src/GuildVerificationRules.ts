// SPDX-License-Identifier: AGPL-3.0-or-later

import {GuildVerificationLevel} from '@fluxer/constants/src/GuildConstants';
import type {ValueOf} from '@fluxer/constants/src/ValueOf';

export const GUILD_VERIFICATION_MIN_ACCOUNT_AGE_MS = 5 * 60 * 1000;
export const GUILD_VERIFICATION_MIN_MEMBERSHIP_MS = 10 * 60 * 1000;

export const GuildVerificationWaitReason = {
	ACCOUNT_TOO_NEW: 'ACCOUNT_TOO_NEW',
	NOT_MEMBER_LONG_ENOUGH: 'NOT_MEMBER_LONG_ENOUGH',
} as const;

export interface GuildVerificationWait {
	readonly reason: ValueOf<typeof GuildVerificationWaitReason>;
	readonly endsAt: number;
}

export function isExemptFromGuildVerification(input: {
	readonly verificationLevel: number;
	readonly isOwner: boolean;
	readonly isBot: boolean;
	readonly hasRoles: boolean;
}): boolean {
	return input.isOwner || input.verificationLevel === GuildVerificationLevel.NONE || input.isBot || input.hasRoles;
}

export function getGuildVerificationWait(input: {
	readonly verificationLevel: number;
	readonly accountCreatedAt: number;
	readonly memberJoinedAt: number | null;
	readonly now: number;
}): GuildVerificationWait | null {
	if (input.verificationLevel >= GuildVerificationLevel.MEDIUM) {
		const endsAt = input.accountCreatedAt + GUILD_VERIFICATION_MIN_ACCOUNT_AGE_MS;
		if (endsAt > input.now) return {reason: GuildVerificationWaitReason.ACCOUNT_TOO_NEW, endsAt};
	}
	if (input.verificationLevel >= GuildVerificationLevel.HIGH && input.memberJoinedAt != null) {
		const endsAt = input.memberJoinedAt + GUILD_VERIFICATION_MIN_MEMBERSHIP_MS;
		if (endsAt > input.now) return {reason: GuildVerificationWaitReason.NOT_MEMBER_LONG_ENOUGH, endsAt};
	}
	return null;
}
