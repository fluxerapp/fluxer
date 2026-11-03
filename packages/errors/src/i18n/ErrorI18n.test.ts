// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {SsoRequiredReasons} from '@fluxer/errors/src/domains/auth/SsoRequiredError';
import {getErrorMessageUnsafe} from '@fluxer/errors/src/i18n/ErrorI18n';
import {ERROR_I18N_LOCALE_MESSAGES} from '@fluxer/errors/src/i18n/ErrorI18nLocales';
import {describe, expect, it} from 'vitest';

describe('ErrorI18n global IP block messages', () => {
	const HOSTED = {
		ipAddress: '203.0.113.20',
		appealEmail: 'support@fluxer.com',
		expiresAt: 'Jan 12, 2027, 9:00 AM UTC',
		reason: 'none',
		product_name: 'Fluxer',
	};
	const SELF_HOSTED = {...HOSTED, appealEmail: null, product_name: 'chat.example.org'};

	it('names the instance and no mailbox in any locale when there is no appeal address', () => {
		for (const locale of Object.keys(ERROR_I18N_LOCALE_MESSAGES)) {
			for (const code of ['GLOBAL_IP_BANNED', 'GLOBAL_IP_TEMPORARILY_BANNED']) {
				const neutral = getErrorMessageUnsafe(code, locale, SELF_HOSTED);
				expect(neutral, `${locale} ${code}`).not.toMatch(/@|\bnull\b|\{/);
				expect(neutral, `${locale} ${code}`).toContain('chat.example.org');
				expect(neutral, `${locale} ${code}`).not.toContain('Fluxer');
				const hosted = getErrorMessageUnsafe(code, locale, HOSTED);
				expect(hosted, `${locale} ${code}`).toContain('support@fluxer.com');
				expect(hosted, `${locale} ${code}`).toContain('203.0.113.20');
				expect(hosted, `${locale} ${code}`).toContain('Fluxer');
			}
		}
	});
});

describe('ErrorI18n self-hosted payment error', () => {
	it('has a translated neutral variant without a support contact in every locale', () => {
		const english = getErrorMessageUnsafe('STRIPE_ERROR', 'en-US', {supportEmail: null});
		for (const locale of Object.keys(ERROR_I18N_LOCALE_MESSAGES)) {
			const hosted = getErrorMessageUnsafe('STRIPE_ERROR', locale, {supportEmail: 'support@fluxer.com'});
			const neutral = getErrorMessageUnsafe('STRIPE_ERROR', locale, {supportEmail: null});
			expect(neutral, locale).not.toBe(hosted);
			expect(neutral, locale).not.toMatch(/@|\bnull\b|\{|support/i);
			if (locale !== 'en-US' && locale !== 'en-GB') {
				expect(neutral, locale).not.toBe(english);
			}
		}
	});
});

describe('ErrorI18n single sign-on required', () => {
	it('has a translated sentence for each reason in every locale', () => {
		for (const reason of Object.values(SsoRequiredReasons)) {
			const english = getErrorMessageUnsafe(APIErrorCodes.SSO_REQUIRED, 'en-US', {reason});
			for (const locale of Object.keys(ERROR_I18N_LOCALE_MESSAGES)) {
				const message = getErrorMessageUnsafe(APIErrorCodes.SSO_REQUIRED, locale, {reason});
				expect(message, `${locale} ${reason}`).not.toMatch(/\{|invalid request/i);
				if (locale !== 'en-US' && locale !== 'en-GB') {
					expect(message, `${locale} ${reason}`).not.toBe(english);
				}
			}
		}
	});
});
