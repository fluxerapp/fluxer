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
		it('loads the default TypeScript catalog', () => {
			const message = getErrorMessageUnsafe('rate_limits.rate_limited', 'en-US');
			expect(message).toBe("You're being rate limited.");
		});
		it('handles missing default bundle gracefully', () => {
			const message = getErrorMessageUnsafe('nonexistent.key', 'en-US', undefined, 'Fallback message');
			expect(message).toBe('Fallback message');
		});
	});
	describe('getMessage() - basic retrieval', () => {
		it('returns message for valid key in default locale', () => {
			const message = getErrorMessageUnsafe('rate_limits.rate_limited', 'en-US');
			expect(message).toBe("You're being rate limited.");
		});
		it('maps API error codes to localized messages', () => {
			const message = getErrorMessageUnsafe('INVALID_FORM_BODY', 'en-US');
			expect(message).toBe('Invalid form body.');
			expect(consoleWarnSpy).not.toHaveBeenCalled();
		});
		it('returns message for valid key in supported locale', () => {
			const message = getErrorMessageUnsafe('rate_limits.rate_limited', 'fr');
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
			const message = getErrorMessageUnsafe('rate_limits.rate_limited', 'en-GB');
			expect(message).toBe("You're being rate limited.");
		});
		it('normalizes en-CA locale to en-US', () => {
			const message = getErrorMessageUnsafe('rate_limits.rate_limited', 'en-CA');
			expect(message).toBe("You're being rate limited.");
		});
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
		it('loads locale on-demand when first accessed', () => {
			const message = getErrorMessageUnsafe('account.suspended_permanently', 'fr');
			expect(message).toBe('Ce compte a été suspendu définitivement.');
		});
	});
	describe('getMessage() - variable interpolation', () => {
		it('interpolates simple {variable} placeholders', () => {
			const message = getErrorMessageUnsafe('channels_and_guilds.invalid_channel_id', 'en-US', {
				channelId: '123456789',
			});
			expect(message).toBe('Invalid channel ID: 123456789.');
		});
		it('handles MessageFormat plural syntax', () => {
			const message = getErrorMessageUnsafe('rate_limits.username_changed_too_often', 'en-US', {
				minutes: 1,
			});
			expect(message).toBe("You've changed your username too often recently. Please try again in 1 minute.");
		});
		it('handles MessageFormat plural syntax for multiple values', () => {
			const message = getErrorMessageUnsafe('rate_limits.username_changed_too_often', 'en-US', {
				minutes: 5,
			});
			expect(message).toBe("You've changed your username too often recently. Please try again in 5 minutes.");
		});
		it('falls back to simple interpolation on MessageFormat failure', () => {
			const message = getErrorMessageUnsafe('channels_and_guilds.invalid_channel_id', 'en-US', {
				channelId: 'test-channel',
			});
			expect(message).toBe('Invalid channel ID: test-channel.');
		});
		it('returns raw message when no variables provided', () => {
			const message = getErrorMessageUnsafe('rate_limits.rate_limited', 'en-US');
			expect(message).toBe("You're being rate limited.");
		});
		it('handles complex nested error keys', () => {
			const message = getErrorMessageUnsafe('roles.invalid_role_id', 'en-US', {roleId: '999'});
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
			const message = getErrorMessageUnsafe('rate_limits.rate_limited', 'xx-XX');
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
			expect(getErrorMessageUnsafe('account.limited', 'zz-ZZ')).toBe(getErrorMessageUnsafe('account.limited', 'en-US'));
		});
	});
	describe('global IP block messages', () => {
		const HOSTED = {ipAddress: '203.0.113.20', appealEmail: 'support@fluxer.com', product_name: 'Fluxer'};
		const SELF_HOSTED = {ipAddress: '203.0.113.20', appealEmail: null, product_name: 'Example Chat'};

		it('names the appeal address when there is one', () => {
			expect(getErrorMessageUnsafe('GLOBAL_IP_BANNED', 'en-US', HOSTED)).toBe(
				'Your IP address 203.0.113.20 has been permanently blocked from the Fluxer API by platform administrators. If you believe this is a mistake, contact support@fluxer.com to appeal. Include this IP address in your appeal.',
			);
			expect(getErrorMessageUnsafe('GLOBAL_IP_TEMPORARILY_BANNED', 'en-US', HOSTED)).toBe(
				'Your IP address 203.0.113.20 has been temporarily blocked from the Fluxer API. The block lifts on its own when it expires. If you think this is a mistake, contact support@fluxer.com and include this IP address.',
			);
		});
		it('points at the instance administrators when there is no appeal address', () => {
			expect(getErrorMessageUnsafe('GLOBAL_IP_BANNED', 'en-US', SELF_HOSTED)).toBe(
				'Your IP address 203.0.113.20 has been permanently blocked from the Example Chat API by platform administrators. If you believe this is a mistake, contact the administrators of this instance to appeal. Include this IP address in your appeal.',
			);
			expect(getErrorMessageUnsafe('GLOBAL_IP_TEMPORARILY_BANNED', 'en-US', SELF_HOSTED)).toBe(
				'Your IP address 203.0.113.20 has been temporarily blocked from the Example Chat API. The block lifts on its own when it expires. If you think this is a mistake, contact the administrators of this instance and include this IP address.',
			);
		});
		it.each([
			['de', 'die Administratoren dieser Instanz'],
			['pt-BR', 'os administradores desta instância'],
			['ja', 'このインスタンスの管理者'],
		])('renders both variants in %s', (locale, administrators) => {
			for (const code of ['GLOBAL_IP_BANNED', 'GLOBAL_IP_TEMPORARILY_BANNED']) {
				const hosted = getErrorMessageUnsafe(code, locale, HOSTED);
				const neutral = getErrorMessageUnsafe(code, locale, SELF_HOSTED);
				expect(hosted).toContain('support@fluxer.com');
				expect(hosted).toContain('203.0.113.20');
				expect(hosted).not.toContain(administrators);
				expect(neutral).toContain(administrators);
				expect(neutral).toContain('203.0.113.20');
				expect(neutral).not.toContain('@');
				expect(neutral).not.toMatch(/\bnull\b/);
				expect(neutral).not.toContain('{');
			}
			expect(consoleWarnSpy).not.toHaveBeenCalled();
		});
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

		it('keeps the hosted text and names the instance administrators on a self-hosted instance', () => {
			expect(getErrorMessageUnsafe('STRIPE_ERROR', 'en-US', HOSTED)).toBe(
				'Payment processing encountered an error. Please try again or contact support.',
			);
			expect(getErrorMessageUnsafe('STRIPE_ERROR', 'en-US', SELF_HOSTED)).toBe(ENGLISH_NEUTRAL);
		});
		it.each([
			['de', 'kontaktiere den Support', 'die Administratoren dieser Instanz'],
			['pt-BR', 'com o suporte', 'os administradores desta instância'],
			['ja', 'サポートにお問い合わせください', 'このインスタンスの管理者'],
		])('renders both variants in %s', (locale, support, administrators) => {
			const hosted = getErrorMessageUnsafe('STRIPE_ERROR', locale, HOSTED);
			const neutral = getErrorMessageUnsafe('STRIPE_ERROR', locale, SELF_HOSTED);
			expect(hosted).toContain(support);
			expect(hosted).not.toContain(administrators);
			expect(neutral).toContain(administrators);
			expect(neutral).not.toContain(support);
			expect(consoleWarnSpy).not.toHaveBeenCalled();
		});
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
