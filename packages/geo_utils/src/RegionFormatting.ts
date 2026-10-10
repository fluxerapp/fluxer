// SPDX-License-Identifier: AGPL-3.0-or-later

import {Locales} from '@fluxer/constants/src/Locales';

const displayNamesByLocale = new Map<string, Intl.DisplayNames>();

function getDisplayNames(locale?: string): Intl.DisplayNames {
	const localeCode = locale?.trim() || Locales.EN_US;
	let displayNames = displayNamesByLocale.get(localeCode);
	if (!displayNames) {
		displayNames = new Intl.DisplayNames([localeCode], {type: 'region', fallback: 'none'});
		displayNamesByLocale.set(localeCode, displayNames);
	}
	return displayNames;
}

export function getRegionDisplayName(regionCode: string, options?: {locale?: string}): string | undefined {
	const displayNames = getDisplayNames(options?.locale);
	const trimmedRegionCode = regionCode.trim();
	if (trimmedRegionCode.length !== 2) {
		return undefined;
	}
	const upperRegionCode = trimmedRegionCode.toUpperCase();
	if (!/^[A-Z]{2}$/.test(upperRegionCode)) {
		return undefined;
	}
	return displayNames.of(upperRegionCode) || undefined;
}
