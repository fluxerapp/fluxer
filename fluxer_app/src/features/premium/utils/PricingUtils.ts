// SPDX-License-Identifier: AGPL-3.0-or-later

import {getCachedNumberFormat} from '@app/features/i18n/utils/IntlCache';

export function formatMinorUnitPrice(
	amountMinor: number | null | undefined,
	currency: string | null | undefined,
	locale: string,
): string | null {
	if (amountMinor == null || !currency) {
		return null;
	}
	const fractionDigits =
		getCachedNumberFormat(locale, {
			style: 'currency',
			currency,
		}).resolvedOptions().maximumFractionDigits ?? 2;
	return getCachedNumberFormat(locale, {
		style: 'currency',
		currency,
		minimumFractionDigits: fractionDigits,
		maximumFractionDigits: fractionDigits,
	}).format(amountMinor / 10 ** fractionDigits);
}
