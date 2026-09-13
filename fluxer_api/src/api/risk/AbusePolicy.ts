// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';

function normalizeCountryCode(countryCode: string | null | undefined): string | null {
	const trimmed = countryCode?.trim();
	return trimmed ? trimmed.toUpperCase() : null;
}

function configuredCountrySet(countryCodes: Array<string>): ReadonlySet<string> {
	return new Set(countryCodes.map(normalizeCountryCode).filter((code): code is string => code !== null));
}

export function countryRequiresInboundPhoneVerification(countryCode: string | null | undefined): boolean {
	const normalized = normalizeCountryCode(countryCode);
	if (!normalized) return false;
	return configuredCountrySet(Config.abusePolicy.inboundPhoneCountryCodes).has(normalized);
}

export function phoneRequiresInboundVerification(
	phone: string,
	prefixes: ReadonlyArray<string> = Config.abusePolicy.phoneVerification.inboundRequiredPrefixes,
): boolean {
	return prefixes.some((prefix) => phone.startsWith(prefix));
}
