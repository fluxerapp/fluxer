// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	getErrorMessage,
	getErrorMessageResult,
	getErrorMessageUnsafe,
	hasErrorLocale,
} from '@fluxer/errors/src/i18n/ErrorI18n';
import type {ErrorI18nKey} from '@fluxer/errors/src/i18n/ErrorI18nTypes.generated';
import {beforeEach, describe, expect, it, type MockInstance, vi} from 'vitest';

describe('ErrorI18n', () => {
	let consoleWarnSpy: MockInstance;
	beforeEach(() => {
		consoleWarnSpy = vi.spyOn(console, 'warn').mockImplementation(() => {});
		consoleWarnSpy.mockClear();
	});
	describe('constructor and initialization', () => {
		it('loads the default TypeScript catalog', () => {
			const message = getErrorMessage('rate_limits.rate_limited', 'en-US');
			expect(message).toBe("You're being rate limited.");
		});
		it('initializes internal state correctly', () => {
			expect(hasErrorLocale('en-US')).toBe(true);
		});
		it('handles missing default bundle gracefully', () => {
			const message = getErrorMessageUnsafe('nonexistent.key', 'en-US', undefined, 'Fallback message');
			expect(message).toBe('Fallback message');
		});
	});
	describe('getMessage() - basic retrieval', () => {
		it('returns message for valid key in default locale', () => {
			const message = getErrorMessage('rate_limits.rate_limited', 'en-US');
			expect(message).toBe("You're being rate limited.");
		});
		it('maps API error codes to localized messages', () => {
			const message = getErrorMessageUnsafe('INVALID_FORM_BODY', 'en-US');
			expect(message).toBe('Invalid form body.');
			expect(consoleWarnSpy).not.toHaveBeenCalled();
		});
		it('returns message for valid key in supported locale', () => {
			const message = getErrorMessage('rate_limits.rate_limited', 'fr');
			expect(message).toBe('Tu es soumis à une limitation de débit.');
		});
		it('returns key when translation missing', () => {
			const message = getErrorMessageUnsafe('nonexistent.key', 'en-US');
			expect(message).toBe('nonexistent.key');
			expect(consoleWarnSpy).toHaveBeenCalledWith(
				'Missing translation for error message: nonexistent.key (locale: en-US)',
			);
		});
		it('returns fallbackMessage when provided and key missing', () => {
			const message = getErrorMessageUnsafe('nonexistent.key', 'en-US', undefined, 'Custom fallback');
			expect(message).toBe('Custom fallback');
		});
	});
	describe('getMessage() - locale handling', () => {
		it('normalizes en-GB locale to en-US', () => {
			const message = getErrorMessage('rate_limits.rate_limited', 'en-GB');
			expect(message).toBe("You're being rate limited.");
		});
		it('normalizes en-CA locale to en-US', () => {
			const message = getErrorMessage('rate_limits.rate_limited', 'en-CA');
			expect(message).toBe("You're being rate limited.");
		});
		it('falls back to en-US for unsupported locales', () => {
			const message = getErrorMessage('rate_limits.rate_limited', 'de-DE');
			expect(message).toBe("You're being rate limited.");
			expect(consoleWarnSpy).toHaveBeenCalledWith('Unsupported locale, falling back to en-US: de-DE');
		});
		it('handles null locale by defaulting to en-US', () => {
			const message = getErrorMessage('rate_limits.rate_limited', null);
			expect(message).toBe("You're being rate limited.");
		});
		it('handles undefined locale by defaulting to en-US', () => {
			const message = getErrorMessage('rate_limits.rate_limited', undefined);
			expect(message).toBe("You're being rate limited.");
		});
		it('loads locale on-demand when first accessed', () => {
			expect(hasErrorLocale('fr')).toBe(true);
			const message = getErrorMessage('account.suspended_permanently', 'fr');
			expect(message).toBe('Ce compte a été suspendu définitivement.');
		});
	});
	describe('getMessage() - variable interpolation', () => {
		it('interpolates simple {variable} placeholders', () => {
			const message = getErrorMessage('channels_and_guilds.invalid_channel_id', 'en-US', {
				channelId: '123456789',
			});
			expect(message).toBe('Invalid channel ID: 123456789.');
		});
		it('handles MessageFormat plural syntax', () => {
			const message = getErrorMessage('rate_limits.username_changed_too_often', 'en-US', {
				minutes: 1,
			});
			expect(message).toBe("You've changed your username too often recently. Please try again in 1 minute.");
		});
		it('handles MessageFormat plural syntax for multiple values', () => {
			const message = getErrorMessage('rate_limits.username_changed_too_often', 'en-US', {
				minutes: 5,
			});
			expect(message).toBe("You've changed your username too often recently. Please try again in 5 minutes.");
		});
		it('falls back to simple interpolation on MessageFormat failure', () => {
			const message = getErrorMessage('channels_and_guilds.invalid_channel_id', 'en-US', {
				channelId: 'test-channel',
			});
			expect(message).toBe('Invalid channel ID: test-channel.');
		});
		it('returns raw message when no variables provided', () => {
			const message = getErrorMessage('rate_limits.rate_limited', 'en-US');
			expect(message).toBe("You're being rate limited.");
		});
		it('handles complex nested error keys', () => {
			const message = getErrorMessage('roles.invalid_role_id', 'en-US', {roleId: '999'});
			expect(message).toBe('Invalid role ID: 999.');
		});
	});
	describe('getMessage() - edge cases', () => {
		it('returns key when source message does not exist', () => {
			const message = getErrorMessageUnsafe('completely.made.up.key', 'en-US');
			expect(message).toBe('completely.made.up.key');
		});
		it('uses fallback when both key and fallbackMessage provided', () => {
			const message = getErrorMessageUnsafe('missing.key', 'en-US', {}, 'Fallback used');
			expect(message).toBe('Fallback used');
		});
		it('returns source message when locale translation missing but source exists', () => {
			const message = getErrorMessage('rate_limits.rate_limited', 'xx-XX');
			expect(message).toBe("You're being rate limited.");
			expect(consoleWarnSpy).toHaveBeenCalledWith('Unsupported locale, falling back to en-US: xx-XX');
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
	describe('phone rejection messages', () => {
		it.each([
			[
				'PHONE_COUNTRY_NOT_SUPPORTED',
				"We don't send verification texts to this country. Use a mobile number from another country, or email support@fluxer.app and a person will review your account.",
			],
			[
				'PHONE_INBOUND_VERIFICATION_REQUIRED',
				'This number is verified by texting us instead of us texting you. Start phone verification again to get the code and the number to text.',
			],
			[
				'PHONE_LOOKUP_UNAVAILABLE',
				'Our phone number check is down right now, so we stopped before sending your code. This is on us, not your number. Wait a few minutes and try the same number again.',
			],
			[
				'PHONE_NUMBER_NOT_IN_SERVICE',
				"Your carrier says this number isn't in service. Check the number and try again, or email support@fluxer.app if it's correct.",
			],
			[
				'PHONE_NUMBER_NOT_MOBILE',
				"This isn't a mobile number, so it can't receive our text. Use a mobile number, or email support@fluxer.app if you think that's wrong.",
			],
			[
				'PHONE_VERIFICATION_NEEDS_REVIEW',
				"We couldn't verify this number automatically. Email support@fluxer.app and a person will review your account.",
			],
		])('resolves %s to its own message', (code, expected) => {
			const message = getErrorMessageUnsafe(code, 'en-US');
			expect(message).toBe(expected);
			expect(consoleWarnSpy).not.toHaveBeenCalled();
		});
		it.each([
			'PHONE_COUNTRY_NOT_SUPPORTED',
			'PHONE_NUMBER_NOT_IN_SERVICE',
			'PHONE_NUMBER_NOT_MOBILE',
			'PHONE_VERIFICATION_NEEDS_REVIEW',
		])('routes %s to support', (code) => {
			expect(getErrorMessageUnsafe(code, 'en-US')).toContain('support@fluxer.app');
		});
		it('blames us for a lookup outage and invites the same number again', () => {
			const message = getErrorMessage('phone.lookup_unavailable', 'en-US');
			expect(message).toContain('This is on us, not your number.');
			expect(message).toContain('try the same number again');
			expect(message.toLowerCase()).not.toContain('invalid');
		});
		it('leaves the two fraud reasons indistinguishable', () => {
			expect(getErrorMessageUnsafe('PHONE_VERIFICATION_NEEDS_REVIEW', 'en-US')).toBe(
				getErrorMessage('phone.verification_needs_review', 'en-US'),
			);
		});
		it('falls back to the source message when the locale has no catalog', () => {
			expect(getErrorMessage('phone.lookup_unavailable', 'zz-ZZ')).toBe(
				getErrorMessage('phone.lookup_unavailable', 'en-US'),
			);
		});

		it('serves a real translation for a locale that has one', () => {
			for (const locale of ['fr', 'de', 'ja', 'pt-BR']) {
				expect(getErrorMessage('phone.lookup_unavailable', locale)).not.toBe(
					getErrorMessage('phone.lookup_unavailable', 'en-US'),
				);
			}
		});
	});
});
