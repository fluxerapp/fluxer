// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {phoneFlaggingAllowedForCountry, stripDisallowedPhoneFlags} from '@app/api/risk/AbusePolicy';
import {SuspiciousActivityFlags} from '@fluxer/constants/src/UserConstants';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

const PHONE_AND_EMAIL =
	SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL |
	SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE |
	SuspiciousActivityFlags.REQUIRE_INBOUND_PHONE_VERIFICATION;

describe('phone flagging policy', () => {
	const original = {...Config.abusePolicy.phoneFlagging};

	beforeEach(() => {
		Config.abusePolicy.phoneFlagging = {enabled: true, exemptCountryCodes: []};
	});

	afterEach(() => {
		Config.abusePolicy.phoneFlagging = original;
	});

	it('keeps phone flags by default without resolving the country', async () => {
		const resolveCountryCode = vi.fn(async () => 'NG');
		expect(await stripDisallowedPhoneFlags(PHONE_AND_EMAIL, resolveCountryCode)).toBe(PHONE_AND_EMAIL);
		expect(resolveCountryCode).not.toHaveBeenCalled();
		expect(phoneFlaggingAllowedForCountry('NG')).toBe(true);
	});

	it('strips only phone flags when disabled', async () => {
		Config.abusePolicy.phoneFlagging = {enabled: false, exemptCountryCodes: []};
		const resolveCountryCode = vi.fn(async () => 'NG');
		expect(await stripDisallowedPhoneFlags(PHONE_AND_EMAIL, resolveCountryCode)).toBe(
			SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL,
		);
		expect(resolveCountryCode).not.toHaveBeenCalled();
		expect(phoneFlaggingAllowedForCountry('NG')).toBe(false);
		expect(phoneFlaggingAllowedForCountry(null)).toBe(false);
	});

	it('strips phone flags for exempt countries only', async () => {
		Config.abusePolicy.phoneFlagging = {enabled: true, exemptCountryCodes: [' br', 'PT']};
		expect(await stripDisallowedPhoneFlags(PHONE_AND_EMAIL, async () => 'BR')).toBe(
			SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL,
		);
		expect(await stripDisallowedPhoneFlags(PHONE_AND_EMAIL, async () => 'ng')).toBe(PHONE_AND_EMAIL);
		expect(await stripDisallowedPhoneFlags(PHONE_AND_EMAIL, async () => null)).toBe(PHONE_AND_EMAIL);
		expect(phoneFlaggingAllowedForCountry('pt')).toBe(false);
		expect(phoneFlaggingAllowedForCountry('NG')).toBe(true);
	});

	it('replaces email or phone flags with their email only equivalent', async () => {
		Config.abusePolicy.phoneFlagging = {enabled: false, exemptCountryCodes: []};
		expect(
			await stripDisallowedPhoneFlags(
				SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL_OR_VERIFIED_PHONE |
					SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL_OR_REVERIFIED_PHONE,
				async () => null,
			),
		).toBe(SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL);
		expect(
			await stripDisallowedPhoneFlags(
				SuspiciousActivityFlags.REQUIRE_REVERIFIED_EMAIL_OR_VERIFIED_PHONE |
					SuspiciousActivityFlags.REQUIRE_INBOUND_PHONE_VERIFICATION,
				async () => null,
			),
		).toBe(SuspiciousActivityFlags.REQUIRE_REVERIFIED_EMAIL);
		Config.abusePolicy.phoneFlagging = {enabled: true, exemptCountryCodes: ['BR']};
		expect(
			await stripDisallowedPhoneFlags(
				SuspiciousActivityFlags.REQUIRE_REVERIFIED_EMAIL_OR_REVERIFIED_PHONE,
				async () => 'BR',
			),
		).toBe(SuspiciousActivityFlags.REQUIRE_REVERIFIED_EMAIL);
	});

	it('skips the country lookup when no phone flags are present', async () => {
		Config.abusePolicy.phoneFlagging = {enabled: true, exemptCountryCodes: ['BR']};
		const resolveCountryCode = vi.fn(async () => 'BR');
		expect(await stripDisallowedPhoneFlags(SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL, resolveCountryCode)).toBe(
			SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL,
		);
		expect(resolveCountryCode).not.toHaveBeenCalled();
	});
});
