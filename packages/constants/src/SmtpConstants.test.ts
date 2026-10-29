// SPDX-License-Identifier: AGPL-3.0-or-later

import {SmtpTlsModes, smtpTlsModeFromSecure} from '@fluxer/constants/src/SmtpConstants';
import {describe, expect, it} from 'vitest';

describe('smtpTlsModeFromSecure', () => {
	it('uses implicit TLS on port 465', () => {
		expect(smtpTlsModeFromSecure(true, 465)).toBe(SmtpTlsModes.IMPLICIT);
	});

	it('uses starttls on the default port when no port is set', () => {
		expect(smtpTlsModeFromSecure(true, undefined)).toBe(SmtpTlsModes.STARTTLS);
		expect(smtpTlsModeFromSecure(true, null)).toBe(SmtpTlsModes.STARTTLS);
	});

	it('uses starttls on any other port', () => {
		expect(smtpTlsModeFromSecure(true, 2525)).toBe(SmtpTlsModes.STARTTLS);
	});

	it('uses opportunistic TLS when secure is false', () => {
		expect(smtpTlsModeFromSecure(false, 465)).toBe(SmtpTlsModes.OPPORTUNISTIC);
	});

	it('leaves the mode unset when secure is unset', () => {
		expect(smtpTlsModeFromSecure(undefined, 465)).toBeNull();
	});
});
