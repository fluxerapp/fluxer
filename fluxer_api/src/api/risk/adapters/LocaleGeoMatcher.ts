// SPDX-License-Identifier: AGPL-3.0-or-later

import type {LocaleGeoMatchResult} from '@app/api/risk/RiskTypes';

const LOCALE_TO_COUNTRIES: Record<string, ReadonlyArray<string>> = {
	'en-us': ['US', 'PR', 'GU', 'VI', 'AS', 'MP'],
	'en-gb': ['GB', 'IM', 'JE', 'GG'],
	'en-au': ['AU'],
	'en-ca': ['CA'],
	'en-nz': ['NZ'],
	'en-in': ['IN'],
	'en-ie': ['IE'],
	'en-za': ['ZA'],
	'fr-fr': ['FR', 'MC'],
	'fr-ca': ['CA'],
	'fr-be': ['BE'],
	'fr-ch': ['CH'],
	'de-de': ['DE'],
	'de-at': ['AT'],
	'de-ch': ['CH'],
	'de-li': ['LI'],
	'es-es': ['ES'],
	'es-mx': ['MX'],
	'es-ar': ['AR'],
	'es-419': ['MX', 'AR', 'CL', 'CO', 'PE', 'UY', 'VE', 'BO', 'EC', 'PY', 'CR', 'PA', 'DO', 'GT', 'HN', 'NI', 'SV'],
	'pt-br': ['BR'],
	'pt-pt': ['PT'],
	'ja-jp': ['JP'],
	'ko-kr': ['KR'],
	'zh-cn': ['CN'],
	'zh-tw': ['TW'],
	'zh-hk': ['HK'],
	'zh-sg': ['SG'],
	'ru-ru': ['RU'],
	'ar-sa': ['SA'],
	'ar-eg': ['EG'],
	'ar-ae': ['AE'],
	'hi-in': ['IN'],
	'it-it': ['IT', 'SM', 'VA'],
	'nl-nl': ['NL'],
	'nl-be': ['BE'],
	'pl-pl': ['PL'],
	'tr-tr': ['TR'],
	'vi-vn': ['VN'],
	'th-th': ['TH'],
	'uk-ua': ['UA'],
	'sv-se': ['SE'],
	'da-dk': ['DK'],
	'nb-no': ['NO'],
	'nn-no': ['NO'],
	'fi-fi': ['FI'],
	'cs-cz': ['CZ'],
	'sk-sk': ['SK'],
	'hu-hu': ['HU'],
	'ro-ro': ['RO'],
	'bg-bg': ['BG'],
	'el-gr': ['GR'],
	'he-il': ['IL'],
	'id-id': ['ID'],
	'ms-my': ['MY'],
	'fil-ph': ['PH'],
	'lt-lt': ['LT'],
	'lv-lv': ['LV'],
	'et-ee': ['EE'],
	'sl-si': ['SI'],
	'hr-hr': ['HR'],
	'sr-rs': ['RS'],
	'mk-mk': ['MK'],
	'sq-al': ['AL'],
	'ka-ge': ['GE'],
	'hy-am': ['AM'],
	'az-az': ['AZ'],
	'kk-kz': ['KZ'],
	'uz-uz': ['UZ'],
	'bn-bd': ['BD'],
	'ur-pk': ['PK'],
	tr: ['TR'],
	ru: ['RU', 'BY'],
	pl: ['PL'],
	cs: ['CZ'],
	hu: ['HU'],
	ro: ['RO'],
	bg: ['BG'],
	el: ['GR', 'CY'],
	he: ['IL'],
	id: ['ID'],
	uk: ['UA'],
	sv: ['SE'],
	da: ['DK'],
	fi: ['FI'],
	sk: ['SK'],
	hr: ['HR'],
	sr: ['RS'],
	sl: ['SI'],
	lt: ['LT'],
	lv: ['LV'],
	et: ['EE'],
	sq: ['AL', 'XK'],
	ka: ['GE'],
	hy: ['AM'],
	az: ['AZ'],
	kk: ['KZ'],
	uz: ['UZ'],
	bn: ['BD', 'IN'],
	ur: ['PK'],
	hi: ['IN'],
	it: ['IT', 'SM', 'VA', 'CH'],
	nl: ['NL', 'BE', 'SR'],
	pt: ['PT', 'BR'],
	de: ['DE', 'AT', 'CH', 'LI'],
	fr: ['FR', 'BE', 'CH', 'CA', 'MC', 'LU'],
};
const SINGLE_COUNTRY_LANGUAGES: Record<string, ReadonlyArray<string>> = {
	ja: ['JP'],
	ko: ['KR'],
	th: ['TH'],
	vi: ['VN'],
	tr: ['TR'],
	pl: ['PL'],
	cs: ['CZ'],
	hu: ['HU'],
	fi: ['FI'],
	he: ['IL'],
	ro: ['RO'],
	bg: ['BG'],
	sk: ['SK'],
	lt: ['LT'],
	lv: ['LV'],
	et: ['EE'],
	ka: ['GE'],
	hy: ['AM'],
	kk: ['KZ'],
	az: ['AZ'],
	sq: ['AL', 'XK'],
};

export async function checkGeoVsLocale(args: {
	geoipCountryIso: string | null;
	registrationLocale: string | null;
	registrationTimezone: string | null;
}): Promise<LocaleGeoMatchResult> {
	const notes: Array<string> = [];
	let localeGeoMatch: boolean | null = null;
	let mismatchDetected = false;
	const country = args.geoipCountryIso?.toUpperCase() ?? null;
	const locale = args.registrationLocale?.toLowerCase() ?? null;
	if (locale && country) {
		const expected = LOCALE_TO_COUNTRIES[locale];
		if (expected) {
			localeGeoMatch = expected.includes(country);
			if (!localeGeoMatch) {
				mismatchDetected = true;
				notes.push(`locale ${locale} expects ${expected.join('/')} but GeoIP reports ${country}`);
			} else {
				notes.push(`locale ${locale} matches GeoIP country ${country}`);
			}
		} else {
			const lang = locale.split('-')[0] ?? '';
			const strict = SINGLE_COUNTRY_LANGUAGES[lang];
			if (strict && !strict.includes(country)) {
				mismatchDetected = true;
				localeGeoMatch = false;
				notes.push(`language ${lang} is nearly single-country (${strict.join('/')}) but GeoIP is ${country}`);
			} else {
				notes.push(`locale ${locale} not in lookup table; no mismatch asserted`);
			}
		}
	} else {
		notes.push('insufficient data to compare locale vs geoip');
	}
	return {
		geoipCountryIso: country,
		registrationLocale: locale,
		registrationTimezone: args.registrationTimezone ?? null,
		localeGeoMatch,
		timezoneGeoMatch: null,
		mismatchDetected,
		notes,
	};
}
