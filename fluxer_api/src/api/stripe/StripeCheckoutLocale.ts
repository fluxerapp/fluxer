// SPDX-License-Identifier: AGPL-3.0-or-later

import type Stripe from 'stripe';

type CheckoutSessionLocale = NonNullable<Stripe.Checkout.SessionCreateParams['locale']>;

const STRIPE_CHECKOUT_LOCALES: Record<string, CheckoutSessionLocale> = {
	bg: 'bg',
	cs: 'cs',
	da: 'da',
	de: 'de',
	el: 'el',
	'en-GB': 'en-GB',
	'en-US': 'en',
	'es-419': 'es-419',
	'es-ES': 'es',
	fi: 'fi',
	fr: 'fr',
	hr: 'hr',
	hu: 'hu',
	id: 'id',
	it: 'it',
	ja: 'ja',
	ko: 'ko',
	lt: 'lt',
	nl: 'nl',
	no: 'nb',
	pl: 'pl',
	'pt-BR': 'pt-BR',
	ro: 'ro',
	ru: 'ru',
	'sv-SE': 'sv',
	th: 'th',
	tr: 'tr',
	vi: 'vi',
	'zh-CN': 'zh',
	'zh-TW': 'zh-TW',
};

export function getStripeCheckoutLocale(locale: string | null): CheckoutSessionLocale {
	if (!locale) {
		return 'auto';
	}
	const checkoutLocale = STRIPE_CHECKOUT_LOCALES[locale];
	return checkoutLocale ?? 'auto';
}
