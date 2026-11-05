// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/api/Logger';
import {
	channelPriceKey,
	channelPriceLookupKey,
	channelUnitAmount,
	lookupChannelPrice,
} from '@app/api/stripe/DistributionPricing';
import {extractId} from '@app/api/stripe/StripeUtils';
import {StripeError} from '@fluxer/errors/src/domains/payment/StripeError';
import type {
	DistributionChannelPrice,
	DistributionPricingConfig,
	DistributionRule,
} from '@fluxer/schema/src/domains/admin/InstanceDistributionPricingSchemas';
import type Stripe from 'stripe';

export interface DistributionPricingStore {
	updateDistributionPricingConfig(
		update: (current: DistributionPricingConfig) => DistributionPricingConfig,
	): Promise<DistributionPricingConfig>;
}

function channelPriceParams(
	rule: DistributionRule,
	base: Stripe.Price,
	unitAmount: number,
	lookupKey: string,
): Stripe.PriceCreateParams {
	const product = extractId(base.product);
	if (!product) throw new StripeError('Base price has no product');
	return {
		currency: base.currency,
		product,
		unit_amount: unitAmount,
		...(base.recurring
			? {recurring: {interval: base.recurring.interval, interval_count: base.recurring.interval_count}}
			: {}),
		...(base.tax_behavior ? {tax_behavior: base.tax_behavior} : {}),
		...(base.nickname ? {nickname: base.nickname} : {}),
		lookup_key: lookupKey,
		metadata: {
			distribution: rule.id,
			distribution_rule_version: String(rule.rule_version),
			distribution_rate_bps: String(rule.rate_bps),
			distribution_base_price: base.id,
		},
	};
}

export class DistributionPriceSync {
	constructor(
		private readonly stripe: Stripe,
		private readonly store: DistributionPricingStore,
	) {}

	async ensureChannelPrice(rule: DistributionRule, basePriceId: string): Promise<DistributionChannelPrice> {
		const known = lookupChannelPrice(rule, basePriceId);
		if (known) return known;
		const base = await this.stripe.prices.retrieve(basePriceId);
		if (base.unit_amount == null || base.billing_scheme === 'tiered') {
			throw new StripeError('Base price has no fixed unit amount');
		}
		const unitAmount = channelUnitAmount(base.unit_amount, base.currency, rule);
		const lookupKey = channelPriceLookupKey(rule, basePriceId);
		const price = await this.findOrCreatePrice(rule, base, unitAmount, lookupKey);
		const entry: DistributionChannelPrice = {
			price_id: price.id,
			rate_bps: rule.rate_bps,
			base_unit_amount: base.unit_amount,
			unit_amount: price.unit_amount ?? unitAmount,
		};
		const key = channelPriceKey(rule.rule_version, basePriceId);
		const stored = await this.store.updateDistributionPricingConfig((current) => ({
			rules: current.rules.map((candidate) =>
				candidate.id === rule.id && !candidate.prices[key]
					? {...candidate, prices: {...candidate.prices, [key]: entry}}
					: candidate,
			),
		}));
		const storedEntry = stored.rules.find((candidate) => candidate.id === rule.id)?.prices[key];
		Logger.info(
			{distribution: rule.id, ruleVersion: rule.rule_version, basePriceId, priceId: price.id, unitAmount},
			'Registered distribution channel price',
		);
		return storedEntry ?? entry;
	}

	private async findOrCreatePrice(
		rule: DistributionRule,
		base: Stripe.Price,
		unitAmount: number,
		lookupKey: string,
	): Promise<Stripe.Price> {
		const existing = await this.stripe.prices.list({lookup_keys: [lookupKey], limit: 1});
		const match = existing.data[0];
		if (match) {
			const matches =
				match.active &&
				match.unit_amount === unitAmount &&
				match.currency === base.currency &&
				extractId(match.product) === extractId(base.product);
			if (!matches) throw new StripeError(`Stripe price ${match.id} with lookup key ${lookupKey} does not match`);
			return match;
		}
		return this.stripe.prices.create(channelPriceParams(rule, base, unitAmount, lookupKey), {
			idempotencyKey: lookupKey,
		});
	}
}
