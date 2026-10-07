// SPDX-License-Identifier: AGPL-3.0-or-later

import {getConfig} from '@app/api/Config';
import {ErrorI18nService} from '@app/api/infrastructure/ErrorI18nService';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {StripeError} from '@fluxer/errors/src/domains/payment/StripeError';
import {afterEach, describe, expect, it} from 'vitest';

describe('ErrorI18nService', () => {
	const originalSelfHosted = getConfig().instance.selfHosted;
	const service = new ErrorI18nService();

	function renderStripeError(locale: string): string {
		const error = new StripeError('Stripe checkout session missing url');
		return service.getMessage(error.code, locale, error.messageVariables, error.message);
	}

	afterEach(() => {
		getConfig().instance.selfHosted = originalSelfHosted;
	});

	it('tells a hosted user to contact support when payment processing fails', () => {
		getConfig().instance.selfHosted = false;
		expect(renderStripeError('en-US')).toBe(
			'Payment processing encountered an error. Please try again or contact support.',
		);
	});

	it('points a self-hosted user at the instance administrators when payment processing fails', () => {
		getConfig().instance.selfHosted = true;
		expect(renderStripeError('en-US')).toBe(
			'Payment processing encountered an error. Please try again or contact the administrators of this instance.',
		);
		expect(renderStripeError('de')).toBe(
			'Bei der Zahlungsabwicklung ist ein Fehler aufgetreten. Versuch es noch mal oder kontaktiere die Administratoren dieser Instanz.',
		);
	});

	it('lets a variable passed by the error win over the instance default', () => {
		getConfig().instance.selfHosted = true;
		expect(service.getMessage(APIErrorCodes.STRIPE_ERROR, 'en-US', {supportEmail: 'billing@example.com'})).toBe(
			'Payment processing encountered an error. Please try again or contact support.',
		);
	});

	it('leaves messages without the variable unchanged', () => {
		getConfig().instance.selfHosted = true;
		expect(service.getMessage(APIErrorCodes.RATE_LIMITED, 'en-US')).toBe("You're being rate limited.");
	});
});
