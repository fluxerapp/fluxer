// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	DISTRIBUTION_ID_PATTERN,
	DISTRIBUTION_RATE_BPS_MAX,
} from '@fluxer/schema/src/domains/premium/DistributionSchemas';
import {z} from 'zod';

const DISTRIBUTION_RULES_MAX = 32;
const DISTRIBUTION_PRICE_KEY_PATTERN = /^[1-9]\d{0,8}:[A-Za-z0-9_]{1,255}$/;

export const DistributionProductSchema = z.enum(['plutonium_subscription', 'plutonium_gift']);

export type DistributionProduct = z.infer<typeof DistributionProductSchema>;

const DistributionIdSchema = z
	.string()
	.regex(DISTRIBUTION_ID_PATTERN)
	.describe('Distribution id sent by clients built for that distribution, for example ios_marketplace');

const distributionRuleFields = {
	id: DistributionIdSchema,
	active: z.boolean().describe('Whether checkouts and quotes for this distribution use the channel price'),
	rate_bps: z
		.number()
		.int()
		.min(1)
		.max(DISTRIBUTION_RATE_BPS_MAX)
		.describe('Share of the channel price that goes to the distribution, in basis points. 500 means 5%'),
	products: z
		.array(DistributionProductSchema)
		.min(1)
		.max(DistributionProductSchema.options.length)
		.refine((products) => new Set(products).size === products.length, 'Products must be unique')
		.describe('Products that get the channel price'),
};

export const DistributionChannelPriceSchema = z.object({
	price_id: z.string().min(1).max(255).describe('Stripe price ID charged through the distribution'),
	rate_bps: distributionRuleFields.rate_bps,
	base_unit_amount: z.number().int().min(0).describe('Unit amount of the base price, in minor units'),
	unit_amount: z.number().int().min(0).describe('Unit amount of the channel price, in minor units'),
});

export type DistributionChannelPrice = z.infer<typeof DistributionChannelPriceSchema>;

export const DistributionRuleSchema = z.object({
	...distributionRuleFields,
	rule_version: z
		.number()
		.int()
		.min(1)
		.describe('Bumped by the server whenever rate_bps changes. Prices made under a version keep it'),
	prices: z
		.record(z.string().regex(DISTRIBUTION_PRICE_KEY_PATTERN), DistributionChannelPriceSchema)
		.default({})
		.describe(
			'Channel Stripe prices keyed by "<rule_version>:<base price ID>". Managed by the server and never removed',
		),
});

export type DistributionRule = z.infer<typeof DistributionRuleSchema>;

function uniqueRuleIds(rules: ReadonlyArray<{id: string}>): boolean {
	return new Set(rules.map((rule) => rule.id)).size === rules.length;
}

export const DistributionPricingConfigSchema = z.object({
	rules: z
		.array(DistributionRuleSchema)
		.max(DISTRIBUTION_RULES_MAX)
		.refine(uniqueRuleIds, 'Distribution ids must be unique')
		.default([]),
});

export type DistributionPricingConfig = z.infer<typeof DistributionPricingConfigSchema>;

export const DEFAULT_HOSTED_DISTRIBUTION_PRICING_CONFIG: DistributionPricingConfig = {
	rules: [
		{
			id: 'ios_marketplace',
			active: true,
			rate_bps: 500,
			products: ['plutonium_subscription', 'plutonium_gift'],
			rule_version: 1,
			prices: {},
		},
	],
};

export const DistributionPricingConfigResponse = DistributionPricingConfigSchema;

export type DistributionPricingConfigResponse = z.infer<typeof DistributionPricingConfigResponse>;

export const DistributionPricingUpdateRequest = z.object({
	rules: z
		.array(z.object(distributionRuleFields))
		.max(DISTRIBUTION_RULES_MAX)
		.refine(uniqueRuleIds, 'Distribution ids must be unique')
		.describe(
			'Full list of distribution rules. A left-out rule that already has channel prices stays stored as inactive so its subscriptions keep renewing',
		),
});

export type DistributionPricingUpdateRequest = z.infer<typeof DistributionPricingUpdateRequest>;
