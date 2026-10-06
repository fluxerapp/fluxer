// SPDX-License-Identifier: AGPL-3.0-or-later

import {normalizeRegionCode} from '@fluxer/geo_utils/src/RegionCodeValidation';
import {resolveRegionDisplayName} from '@fluxer/geo_utils/src/RegionDisplayNameResolver';

interface RegionDisplayNameOptions {
	locale?: string;
	fallbackToRegionCode?: boolean;
}

function applyRegionCodeFallback(
	regionCode: string,
	displayName: string | undefined,
	options?: RegionDisplayNameOptions,
): string | undefined {
	if (displayName) {
		return displayName;
	}
	if (!options?.fallbackToRegionCode) {
		return undefined;
	}
	const normalizedRegionCode = normalizeRegionCode(regionCode);
	if (normalizedRegionCode) {
		return normalizedRegionCode;
	}
	const trimmedRegionCode = regionCode.trim();
	return trimmedRegionCode.length > 0 ? trimmedRegionCode : undefined;
}

export function getRegionDisplayName(regionCode: string, options?: RegionDisplayNameOptions): string | undefined {
	const displayName = resolveRegionDisplayName(regionCode, options?.locale);
	return applyRegionCodeFallback(regionCode, displayName, options);
}
