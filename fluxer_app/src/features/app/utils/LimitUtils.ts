// SPDX-License-Identifier: AGPL-3.0-or-later

import type {LimitKey} from '@fluxer/constants/src/LimitConfigMetadata';

function isToggleActive(limitValue: number): boolean {
	return limitValue > 0;
}

export function isLimitToggleEnabled(limits: Partial<Record<LimitKey, number>>, feature: LimitKey): boolean {
	const value = limits[feature];
	if (value === undefined || value === null) return false;
	return isToggleActive(value);
}
