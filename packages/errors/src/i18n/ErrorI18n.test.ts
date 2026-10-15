// SPDX-License-Identifier: AGPL-3.0-or-later

import {getErrorMessageUnsafe} from '@fluxer/errors/src/i18n/ErrorI18n';
import {ERROR_I18N_LOCALE_MESSAGES} from '@fluxer/errors/src/i18n/ErrorI18nLocales';
import {beforeEach, describe, expect, it, type MockInstance, vi} from 'vitest';

describe('ErrorI18n', () => {
	let consoleWarnSpy: MockInstance;
	beforeEach(() => {
		consoleWarnSpy = vi.spyOn(console, 'warn').mockImplementation(() => {});
		consoleWarnSpy.mockClear();
	});
	describe('global IP block messages', () => {
		const HOSTED = {
			ipAddress: '203.0.113.20',
			appealEmail: 'support@fluxer.com',
			expiresAt: 'Jan 12, 2027, 9:00 AM UTC',
			reason: 'none',
			product_name: 'Fluxer',
		};
		const SELF_HOSTED = {...HOSTED, appealEmail: null, product_name: 'chat.example.org'};
		const SHARED_NETWORK =
			'If you share this network, for example at school, at work, on public Wi-Fi, through your mobile carrier or a VPN, this block may have nothing to do with you. The quickest fix is to switch networks, for example to mobile data instead of Wi-Fi, or to turn your VPN on or off.';

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

		it('leads with the shared-network advice and never blames the reader', () => {
			const permanent = getErrorMessageUnsafe('GLOBAL_IP_BANNED', 'en-US', HOSTED);
			expect(permanent).toBe(
				`Fluxer blocked access from this network. ${SHARED_NETWORK} If you think this block is a mistake, email support@fluxer.com and include your IP address, 203.0.113.20.`,
			);
			expect(getErrorMessageUnsafe('GLOBAL_IP_TEMPORARILY_BANNED', 'en-US', SELF_HOSTED)).toBe(
				`chat.example.org blocked access from this network. The block ends on Jan 12, 2027, 9:00 AM UTC. ${SHARED_NETWORK}`,
			);
			expect(getErrorMessageUnsafe('GLOBAL_IP_BANNED', 'en-US', {...HOSTED, reason: 'shown'})).toMatch(
				/^Fluxer blocked access from this network because of activity that broke its rules\. /,
			);
			for (const message of [permanent, getErrorMessageUnsafe('GLOBAL_IP_TEMPORARILY_BANNED', 'en-US', HOSTED)]) {
				expect(message).not.toMatch(/platform admin|sign in|\byou did\b/i);
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
