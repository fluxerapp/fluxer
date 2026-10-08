// SPDX-License-Identifier: AGPL-3.0-or-later

import {PREMIUM_GRACE_PERIOD_DAYS} from '@fluxer/constants/src/UserConstants';
import {MS_PER_DAY} from '@fluxer/date_utils/src/DateConstants';

export function getPremiumGraceEndDate(premiumUntil: Date, premiumGraceEndsAt: Date | null | undefined): Date {
	const graceEnd = premiumGraceEndsAt ?? new Date(premiumUntil.getTime() + PREMIUM_GRACE_PERIOD_DAYS * MS_PER_DAY);
	return graceEnd.getTime() > premiumUntil.getTime() ? graceEnd : premiumUntil;
}
