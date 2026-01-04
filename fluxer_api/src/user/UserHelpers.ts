/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {Config} from '~/Config';
import {UserFlags} from '~/Constants';

interface PremiumCheckable {
	premiumType: number | null;
	premiumUntil: Date | null;
	premiumWillCancel: boolean;
	flags: bigint;
}

const GRACE_MS = 3 * 24 * 60 * 60 * 1000;

export function checkIsPremium(user: PremiumCheckable): boolean {
	if (Config.instance.selfHosted) {
		return true;
	}

	if ((user.flags & UserFlags.PREMIUM_ENABLED_OVERRIDE) !== 0n) {
		return true;
	}

	if (user.premiumType == null || user.premiumType <= 0) {
		return false;
	}

	if (user.premiumUntil == null) {
		return true;
	}

	const nowMs = Date.now();
	const untilMs = user.premiumUntil.getTime();

	if (user.premiumWillCancel) {
		return nowMs <= untilMs;
	}

	return nowMs <= untilMs + GRACE_MS;
}

export const PREMIUM_CLEAR_FIELDS = [
	'premium_type',
	'premium_since',
	'premium_until',
	'premium_will_cancel',
	'premium_billing_cycle',
] as const;

export type PremiumClearField = (typeof PREMIUM_CLEAR_FIELDS)[number];

export function shouldStripExpiredPremium(user: PremiumCheckable): boolean {
	if ((user.premiumType ?? 0) <= 0) {
		return false;
	}

	return !checkIsPremium(user);
}

export function mapExpiredPremiumFields<T>(mapper: (field: PremiumClearField) => T): Record<PremiumClearField, T> {
	const result = {} as Record<PremiumClearField, T>;
	for (const field of PREMIUM_CLEAR_FIELDS) {
		result[field] = mapper(field);
	}
	return result;
}
