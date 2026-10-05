// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import type {UserRow} from '@app/api/database/types/UserTypes';
import {getCachedInstancePremiumMode} from '@app/api/limits/InstancePremiumModeCache';
import type {User} from '@app/api/models/User';
import {
	PREMIUM_GRACE_PERIOD_DAYS,
	PREMIUM_PAYMENT_RECOVERY_GRACE_DAYS,
	PremiumFlags,
	UserFlags,
} from '@fluxer/constants/src/UserConstants';
import {MS_PER_DAY} from '@fluxer/date_utils/src/DateConstants';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';

export function isSsoUserWithoutPassword(user: Pick<User, 'traits' | 'passwordHash'>): boolean {
	return user.passwordHash === null && user.traits.has('sso');
}

export function isAccountClosed(user: Pick<User, 'flags' | 'deletionStartedAt'>): boolean {
	return (user.flags & UserFlags.DELETED) !== 0n || user.deletionStartedAt != null;
}

export function isTemporarilyBanned(user: Pick<User, 'flags' | 'tempBannedUntil'>, now = Date.now()): boolean {
	return (
		(user.flags & UserFlags.DISABLED) !== 0n && user.tempBannedUntil != null && user.tempBannedUntil.getTime() > now
	);
}

function isAccountDisabled(user: Pick<User, 'flags' | 'tempBannedUntil'>, now = Date.now()): boolean {
	if ((user.flags & UserFlags.DISABLED) === 0n) return false;
	return user.tempBannedUntil == null || user.tempBannedUntil.getTime() > now;
}

export function isSignInRefused(user: Pick<User, 'flags' | 'deletionStartedAt' | 'tempBannedUntil'>): boolean {
	return isAccountClosed(user) || isTemporarilyBanned(user);
}

export function canOwnerRunBots(owner: Pick<User, 'flags' | 'deletionStartedAt' | 'tempBannedUntil'>): boolean {
	return !isAccountClosed(owner) && !isAccountDisabled(owner);
}

export function isContentHidden(user: Pick<User, 'contentHiddenSince'>, messageId: bigint): boolean {
	return user.contentHiddenSince !== null && snowflakeToDate(messageId) >= user.contentHiddenSince;
}

export function isDirectDeliverySuppressed(user: Pick<User, 'isBot' | 'flags'>): boolean {
	return !user.isBot && (user.flags & UserFlags.SPAMMER) === UserFlags.SPAMMER;
}

interface PremiumCheckable {
	isBot: boolean;
	premiumType: number | null;
	premiumUntil: Date | null;
	premiumGiftExtensionEndsAt: Date | null;
	premiumWillCancel: boolean;
	premiumGraceEndsAt: Date | null;
	flags: bigint;
	premiumFlags: number;
}

export const PREMIUM_GRACE_PERIOD_MS = PREMIUM_GRACE_PERIOD_DAYS * MS_PER_DAY;

export function getPremiumPaymentRecoveryGraceMs(billingCycle: string | null | undefined): number {
	const days =
		billingCycle === 'yearly'
			? PREMIUM_PAYMENT_RECOVERY_GRACE_DAYS.yearly
			: PREMIUM_PAYMENT_RECOVERY_GRACE_DAYS.monthly;
	return days * MS_PER_DAY;
}

export function getEffectivePremiumUntil(
	user: Pick<PremiumCheckable, 'premiumUntil' | 'premiumGiftExtensionEndsAt'>,
): Date | null {
	const subUntil = user.premiumUntil?.getTime() ?? null;
	const giftUntil = user.premiumGiftExtensionEndsAt?.getTime() ?? null;
	if (subUntil == null && giftUntil == null) {
		return null;
	}
	const effectiveMs = Math.max(subUntil ?? 0, giftUntil ?? 0);
	return new Date(effectiveMs);
}

export function checkHasActivePaidPremium(
	user: Pick<PremiumCheckable, 'premiumType' | 'premiumUntil' | 'premiumGiftExtensionEndsAt' | 'premiumGraceEndsAt'>,
): boolean {
	if (user.premiumType == null || user.premiumType <= 0) {
		return false;
	}
	const effective = getEffectivePremiumUntil(user);
	if (effective == null) {
		return true;
	}
	const nowMs = Date.now();
	const untilMs = effective.getTime();
	if (nowMs <= untilMs) {
		return true;
	}
	if (user.premiumGraceEndsAt != null) {
		return nowMs <= user.premiumGraceEndsAt.getTime();
	}
	return nowMs <= untilMs + PREMIUM_GRACE_PERIOD_MS;
}

export function checkIsPremium(user: PremiumCheckable): boolean {
	if (Config.instance.selfHosted && getCachedInstancePremiumMode() === 'everyone') {
		return true;
	}
	if (user.isBot) {
		return true;
	}
	if ((user.premiumFlags & PremiumFlags.PERKS_DISABLED) !== 0) {
		return false;
	}
	if ((user.premiumFlags & PremiumFlags.ENABLED_OVERRIDE) !== 0) {
		return true;
	}
	if (checkHasActivePaidPremium(user)) {
		return true;
	}
	return false;
}

const PREMIUM_CLEAR_FIELDS = [
	'premium_type',
	'premium_since',
	'premium_until',
	'premium_gift_extension_ends_at',
	'premium_will_cancel',
	'premium_billing_cycle',
	'premium_grace_ends_at',
] as const;

type PremiumClearField = (typeof PREMIUM_CLEAR_FIELDS)[number];

export function shouldStripExpiredPremium(user: PremiumCheckable): boolean {
	if ((user.premiumType ?? 0) <= 0) {
		return false;
	}
	return !checkHasActivePaidPremium(user);
}

function mapExpiredPremiumFields<T>(mapper: (field: PremiumClearField) => T): Record<PremiumClearField, T> {
	const result = {} as Record<PremiumClearField, T>;
	for (const field of PREMIUM_CLEAR_FIELDS) {
		result[field] = mapper(field);
	}
	return result;
}

export function createPremiumClearPatch(): Partial<UserRow> {
	return mapExpiredPremiumFields(() => null) as Partial<UserRow>;
}

export function clearPerksSanitizedFlag(premiumFlags: number): number {
	return premiumFlags & ~PremiumFlags.PERKS_SANITIZED;
}

const PROFILE_SUBSTRING_EXEMPT_FLAGS = UserFlags.STAFF;

export function isProfileSubstringExempt(user: Pick<PremiumCheckable, 'flags'>): boolean {
	return (user.flags & PROFILE_SUBSTRING_EXEMPT_FLAGS) !== 0n;
}

export function isBugHunterBotUser(user: Pick<User, 'flags' | 'isBot'>): boolean {
	return user.isBot && (user.flags & UserFlags.BUG_HUNTER) !== 0n;
}
