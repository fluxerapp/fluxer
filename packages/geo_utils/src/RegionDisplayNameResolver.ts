// SPDX-License-Identifier: AGPL-3.0-or-later

import {Locales} from '@fluxer/constants/src/Locales';
import {normalizeRegionCode} from '@fluxer/geo_utils/src/RegionCodeValidation';

const DISPLAY_NAME_TYPE: Intl.DisplayNamesOptions['type'] = 'region';
const DISPLAY_NAME_FALLBACK: Intl.DisplayNamesOptions['fallback'] = 'none';
const displayNamesByLocale = new Map<string, Intl.DisplayNames>();

function resolveLocale(locale?: string): string {
	const trimmedLocale = locale?.trim();
	if (trimmedLocale && trimmedLocale.length > 0) {
		return trimmedLocale;
	}
	return Locales.EN_US;
}

function getDisplayNames(locale?: string): Intl.DisplayNames {
	const localeCode = resolveLocale(locale);
	const cachedDisplayNames = displayNamesByLocale.get(localeCode);
	if (cachedDisplayNames) {
		return cachedDisplayNames;
	}
	const displayNames = new Intl.DisplayNames([localeCode], {
		type: DISPLAY_NAME_TYPE,
		fallback: DISPLAY_NAME_FALLBACK,
	});
	displayNamesByLocale.set(localeCode, displayNames);
	return displayNames;
}

function resolveRegionDisplayNameFromFormatter(
	regionCode: string,
	displayNames: Intl.DisplayNames,
): string | undefined {
	const normalizedRegionCode = normalizeRegionCode(regionCode);
	if (!normalizedRegionCode) {
		return undefined;
	}
	return displayNames.of(normalizedRegionCode) ?? undefined;
}

export function resolveRegionDisplayName(regionCode: string, locale?: string): string | undefined {
	const displayNames = getDisplayNames(locale);
	return resolveRegionDisplayNameFromFormatter(regionCode, displayNames);
}
