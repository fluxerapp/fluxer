// SPDX-License-Identifier: AGPL-3.0-or-later

import {getErrorMessageResult, getErrorMessageUnsafe} from '@fluxer/errors/src/i18n/ErrorI18n';
import {ERROR_I18N_LOCALE_MESSAGES} from '@fluxer/errors/src/i18n/ErrorI18nLocales';
import type {ErrorI18nKey} from '@fluxer/errors/src/i18n/ErrorI18nMessages';
import {beforeEach, describe, expect, it, type MockInstance, vi} from 'vitest';

describe('ErrorI18n', () => {
	let consoleWarnSpy: MockInstance;
	beforeEach(() => {
		consoleWarnSpy = vi.spyOn(console, 'warn').mockImplementation(() => {});
		consoleWarnSpy.mockClear();
	});
	describe('constructor and initialization', () => {
		it('handles missing default bundle gracefully', () => {
			const message = getErrorMessageUnsafe('nonexistent.key', 'en-US', undefined, 'Fallback message');
			expect(message).toBe('Fallback message');
		});
	});
	describe('getMessage() - basic retrieval', () => {
		it('returns key when translation missing', () => {
			const message = getErrorMessageUnsafe('nonexistent.key', 'en-US');
			expect(message).toBe('nonexistent.key');
			expect(consoleWarnSpy).toHaveBeenCalledWith(
				'Missing translation for error message: nonexistent.key (locale: en-US)',
			);
		});
	});
	describe('getMessage() - locale handling', () => {
		it('falls back to en-US for unsupported locales', () => {
			const message = getErrorMessageUnsafe('rate_limits.rate_limited', 'de-DE');
			expect(message).toBe("You're being rate limited.");
			expect(consoleWarnSpy).toHaveBeenCalledWith('Unsupported locale, falling back to en-US: de-DE');
		});
		it('handles null locale by defaulting to en-US', () => {
			const message = getErrorMessageUnsafe('rate_limits.rate_limited', null);
			expect(message).toBe("You're being rate limited.");
		});
		it('handles undefined locale by defaulting to en-US', () => {
			const message = getErrorMessageUnsafe('rate_limits.rate_limited', undefined);
			expect(message).toBe("You're being rate limited.");
		});
	});
	describe('getMessageResult()', () => {
		it('returns error result for missing template', () => {
			const result = getErrorMessageResult('missing.key' as ErrorI18nKey, 'en-US');
			expect(result.ok).toBe(false);
			if (!result.ok) {
				expect(result.error.kind).toBe('missing-template');
			}
		});
	});
	describe('global IP block messages', () => {
		const HOSTED = {ipAddress: '203.0.113.20', appealEmail: 'support@fluxer.com', product_name: 'Fluxer'};
		const SELF_HOSTED = {ipAddress: '203.0.113.20', appealEmail: null, product_name: 'Example Chat'};

		it('names the instance and no mailbox in any locale when there is no appeal address', () => {
			for (const locale of Object.keys(ERROR_I18N_LOCALE_MESSAGES)) {
				for (const code of ['GLOBAL_IP_BANNED', 'GLOBAL_IP_TEMPORARILY_BANNED']) {
					const neutral = getErrorMessageUnsafe(code, locale, SELF_HOSTED);
					expect(neutral, `${locale} ${code}`).toContain('203.0.113.20');
					expect(neutral, `${locale} ${code}`).not.toMatch(/@|\bnull\b|\{/);
					expect(neutral, `${locale} ${code}`).toContain('Example Chat');
					expect(neutral, `${locale} ${code}`).not.toContain('Fluxer');
					const hosted = getErrorMessageUnsafe(code, locale, HOSTED);
					expect(hosted, `${locale} ${code}`).toContain('support@fluxer.com');
					expect(hosted, `${locale} ${code}`).toContain('Fluxer');
				}
			}
		});
	});
	describe('payment processing error', () => {
		const HOSTED = {supportEmail: 'support@fluxer.com'};
		const SELF_HOSTED = {supportEmail: null};
		const ENGLISH_NEUTRAL =
			'Payment processing encountered an error. Please try again or contact the administrators of this instance.';

		it('has a translated neutral variant in every locale', () => {
			for (const locale of Object.keys(ERROR_I18N_LOCALE_MESSAGES)) {
				const hosted = getErrorMessageUnsafe('STRIPE_ERROR', locale, HOSTED);
				const neutral = getErrorMessageUnsafe('STRIPE_ERROR', locale, SELF_HOSTED);
				expect(neutral, locale).not.toBe(hosted);
				expect(neutral, locale).not.toMatch(/@|\bnull\b|\{|support/i);
				if (locale !== 'en-GB') {
					expect(neutral, locale).not.toBe(ENGLISH_NEUTRAL);
				}
			}
			expect(consoleWarnSpy).not.toHaveBeenCalled();
		});
	});
});
