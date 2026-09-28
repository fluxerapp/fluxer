// SPDX-License-Identifier: AGPL-3.0-or-later

import type {BillingRepository} from '@app/api/billing/repositories/BillingRepository';
import {Logger} from '@app/api/Logger';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import {seconds} from 'itty-time';
import type Stripe from 'stripe';

export interface StripePriceSummary {
	unitAmountMinor: number | null;
}

const PRICE_CACHE_TTL_SECONDS = seconds('1 hour');
const PRICE_CACHE_PRODUCE_TIMEOUT_MS = 90000;

export async function getCachedStripePriceSummary({
	stripe,
	cacheService,
	priceId,
	mirror,
}: {
	stripe: Stripe | null;
	cacheService: ICacheService;
	priceId: string | null;
	mirror?: BillingRepository;
}): Promise<StripePriceSummary | null> {
	if (!priceId || !stripe) {
		return null;
	}
	try {
		return await cacheService.getOrSet<StripePriceSummary>(
			`stripe_price_summary:${priceId}`,
			async () => {
				const price = await stripe.prices.retrieve(priceId);
				if (mirror) {
					try {
						await mirror.prices.upsertFromStripe(price);
					} catch (mirrorErr) {
						Logger.error({mirrorErr, priceId}, 'Mirror upsert failed after Stripe price lookup');
					}
				}
				return {
					unitAmountMinor: price.unit_amount ?? null,
				};
			},
			PRICE_CACHE_TTL_SECONDS,
			PRICE_CACHE_PRODUCE_TIMEOUT_MS,
		);
	} catch (error: unknown) {
		Logger.warn({error, priceId}, 'Failed to retrieve Stripe price summary');
		return null;
	}
}
