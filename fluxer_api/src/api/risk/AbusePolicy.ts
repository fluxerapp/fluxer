// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {PHONE_REQUIREMENT_FLAGS, SuspiciousActivityFlags} from '@fluxer/constants/src/UserConstants';

const EMAIL_ONLY_EQUIVALENTS: ReadonlyArray<readonly [number, number]> = [
	[SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL_OR_VERIFIED_PHONE, SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL],
	[SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL_OR_REVERIFIED_PHONE, SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL],
	[
		SuspiciousActivityFlags.REQUIRE_REVERIFIED_EMAIL_OR_VERIFIED_PHONE,
		SuspiciousActivityFlags.REQUIRE_REVERIFIED_EMAIL,
	],
	[
		SuspiciousActivityFlags.REQUIRE_REVERIFIED_EMAIL_OR_REVERIFIED_PHONE,
		SuspiciousActivityFlags.REQUIRE_REVERIFIED_EMAIL,
	],
];

const PHONE_OFFERING_FLAGS = EMAIL_ONLY_EQUIVALENTS.reduce((mask, [either]) => mask | either, PHONE_REQUIREMENT_FLAGS);

function withoutPhoneOfferingFlags(flagBits: number): number {
	return EMAIL_ONLY_EQUIVALENTS.reduce(
		(next, [either, emailOnly]) => ((flagBits & either) !== 0 ? next | emailOnly : next),
		flagBits & ~PHONE_OFFERING_FLAGS,
	);
}

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

export function phoneFlaggingAllowedForCountry(countryCode: string | null | undefined): boolean {
	const {enabled, exemptCountryCodes} = Config.abusePolicy.phoneFlagging;
	if (!enabled) return false;
	const normalized = normalizeCountryCode(countryCode);
	if (!normalized) return true;
	return !configuredCountrySet(exemptCountryCodes).has(normalized);
}

export async function stripDisallowedPhoneFlags(
	flagBits: number,
	resolveCountryCode: () => Promise<string | null>,
): Promise<number> {
	if ((flagBits & PHONE_OFFERING_FLAGS) === 0) return flagBits;
	const {enabled, exemptCountryCodes} = Config.abusePolicy.phoneFlagging;
	if (enabled && exemptCountryCodes.length === 0) return flagBits;
	if (enabled && phoneFlaggingAllowedForCountry(await resolveCountryCode())) return flagBits;
	return withoutPhoneOfferingFlags(flagBits);
}

export function phoneRequiresInboundVerification(
	phone: string,
	prefixes: ReadonlyArray<string> = Config.abusePolicy.phoneVerification.inboundRequiredPrefixes,
): boolean {
	return prefixes.some((prefix) => phone.startsWith(prefix));
}
