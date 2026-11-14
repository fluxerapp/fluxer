// SPDX-License-Identifier: AGPL-3.0-or-later

import {buildAppStoreStoreKey, buildGooglePlayStoreKey, redactStoreKey} from '@app/api/store_billing/StoreBillingTypes';
import {describe, expect, it} from 'vitest';

describe('redactStoreKey', () => {
	it('replaces a Google Play purchase token with a stable digest', () => {
		const token = 'opaque-token.AO-J1OxExampleExampleExample';
		const redacted = redactStoreKey(buildGooglePlayStoreKey(token));
		expect(redacted).toMatch(/^google_play:[0-9a-f]{16}$/u);
		expect(redacted).not.toContain(token);
		expect(redactStoreKey(buildGooglePlayStoreKey(token))).toBe(redacted);
		expect(redactStoreKey(buildGooglePlayStoreKey(`${token}x`))).not.toBe(redacted);
	});

	it('keeps App Store keys as they are', () => {
		const key = buildAppStoreStoreKey('production', '2000000123456789');
		expect(redactStoreKey(key)).toBe(key);
	});
});
