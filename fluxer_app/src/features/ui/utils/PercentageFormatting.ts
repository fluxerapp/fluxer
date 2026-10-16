// SPDX-License-Identifier: AGPL-3.0-or-later

import {getCachedNumberFormat} from '@app/features/i18n/utils/IntlCache';

export function roundPercentage(value: number): number {
	return Number.isFinite(value) ? Math.round(value) : 0;
}

export function formatRoundedPercentage(locale: string, value: number): string {
	return getCachedNumberFormat(locale, {style: 'percent', maximumFractionDigits: 0}).format(
		roundPercentage(value) / 100,
	);
}
