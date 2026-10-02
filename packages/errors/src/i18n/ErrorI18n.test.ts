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
			expect(message).toBe('Vous avez atteint la limite de requêtes.');
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
	describe('account limited message', () => {
		it('resolves ACCOUNT_LIMITED to its own message', () => {
			expect(getErrorMessageUnsafe('ACCOUNT_LIMITED', 'en-US')).toBe(
				'Messaging is paused on your account. Check your email for a quick step to continue.',
			);
			expect(consoleWarnSpy).not.toHaveBeenCalled();
		});
		it('falls back to the source message when the locale has no catalog', () => {
			expect(getErrorMessage('account.limited', 'zz-ZZ')).toBe(getErrorMessage('account.limited', 'en-US'));
		});
	});
});
