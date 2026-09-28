// SPDX-License-Identifier: AGPL-3.0-or-later

import {getCachedNumberFormat} from '@app/features/i18n/utils/IntlCache';

const STRIPE_TWO_DECIMAL_WHOLE_UNIT_CURRENCIES = new Set(['ISK', 'HUF', 'UGX']);

export function formatMinorUnitPrice(
	amountMinor: number | null | undefined,
	currency: string | null | undefined,
	locale: string,
): string | null {
	if (amountMinor == null || !currency) {
		return null;
	}
	const wholeUnitDisplay = STRIPE_TWO_DECIMAL_WHOLE_UNIT_CURRENCIES.has(currency.toUpperCase());
	const displayFractionDigits = wholeUnitDisplay
		? 0
		: (getCachedNumberFormat(locale, {
				style: 'currency',
				currency,
			}).resolvedOptions().maximumFractionDigits ?? 2);
	const exponent = wholeUnitDisplay ? 2 : displayFractionDigits;
	return getCachedNumberFormat(locale, {
		style: 'currency',
		currency,
		minimumFractionDigits: displayFractionDigits,
		maximumFractionDigits: displayFractionDigits,
	}).format(amountMinor / 10 ** exponent);
}
