// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {
	type EffectiveBillingConfig,
	getEffectiveBillingConfig,
	isStripeServiceable,
} from '@app/api/stripe/BillingConfigCache';
import {STRIPE_API_VERSION} from '@app/api/stripe/StripeApiVersion';
import Stripe from 'stripe';

let cachedClient: {secretKey: string; client: Stripe} | null = null;

function createStripeClient(secretKey: string): Stripe {
	return new Stripe(secretKey, {
		apiVersion: STRIPE_API_VERSION,
		httpClient: Config.dev.testModeEnabled
			? Stripe.createFetchHttpClient((input, init) => globalThis.fetch(input, init))
			: undefined,
	});
}

export function getStripeClient(config: EffectiveBillingConfig = getEffectiveBillingConfig()): Stripe | null {
	if (!config.secretKey || !isStripeServiceable(config)) {
		return null;
	}
	if (cachedClient === null || cachedClient.secretKey !== config.secretKey) {
		cachedClient = {secretKey: config.secretKey, client: createStripeClient(config.secretKey)};
	}
	return cachedClient.client;
}
