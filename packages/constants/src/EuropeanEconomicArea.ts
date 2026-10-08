// SPDX-License-Identifier: AGPL-3.0-or-later

const EU_EEA_COUNTRY_CODES = [
	'AT',
	'BE',
	'BG',
	'HR',
	'CY',
	'CZ',
	'DK',
	'EE',
	'FI',
	'FR',
	'DE',
	'GR',
	'HU',
	'IE',
	'IT',
	'LV',
	'LT',
	'LU',
	'MT',
	'NL',
	'PL',
	'PT',
	'RO',
	'SK',
	'SI',
	'ES',
	'SE',
	'IS',
	'LI',
	'NO',
	'AX',
] as const;
const EU_EEA_COUNTRY_CODE_SET: ReadonlySet<string> = new Set(EU_EEA_COUNTRY_CODES);

export function isEuEeaCountryCode(countryCode: string | null | undefined): boolean {
	return Boolean(countryCode && EU_EEA_COUNTRY_CODE_SET.has(countryCode.trim().toUpperCase()));
}
