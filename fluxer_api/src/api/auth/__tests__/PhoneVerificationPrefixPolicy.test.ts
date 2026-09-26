// SPDX-License-Identifier: AGPL-3.0-or-later

import {requiresInboundPhoneVerification} from '@app/api/auth/PhoneVerificationPrefixPolicy';
import {describe, expect, it} from 'vitest';

describe('PhoneVerificationPrefixPolicy', () => {
	const configuredPrefixes = ['+101', '+202', '+3034'];

	it('requires inbound verification for configured prefixes', () => {
		expect(requiresInboundPhoneVerification('+1015551234', configuredPrefixes)).toBe(true);
		expect(requiresInboundPhoneVerification('+2025551234', configuredPrefixes)).toBe(true);
		expect(requiresInboundPhoneVerification('+30345551234', configuredPrefixes)).toBe(true);
	});
	it('does not require inbound verification for prefixes outside config', () => {
		expect(requiresInboundPhoneVerification('+15551234567', configuredPrefixes)).toBe(false);
		expect(requiresInboundPhoneVerification('+4045551234', configuredPrefixes)).toBe(false);
	});
});
