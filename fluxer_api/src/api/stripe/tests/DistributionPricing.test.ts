// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	applyDistributionToQuote,
	channelUnitAmount,
	decodeDistributionPurchaseRef,
	mergeDistributionRules,
	resolveListPriceId,
} from '@app/api/stripe/DistributionPricing';
import type {
	DistributionPricingConfig,
	DistributionRule,
} from '@fluxer/schema/src/domains/admin/InstanceDistributionPricingSchemas';
import type {PriceIdsResponse} from '@fluxer/schema/src/domains/premium/PremiumSchemas';
import {describe, expect, test} from 'vitest';

const RULE: DistributionRule = {
	id: 'ios_marketplace',
	active: true,
	rate_bps: 500,
	products: ['plutonium_subscription', 'plutonium_gift'],
	rule_version: 1,
	prices: {},
};

const QUOTE: PriceIdsResponse = {
	monthly: 'price_monthly_eur',
	yearly: 'price_yearly_eur',
	gift_1_month: 'price_gift_month_eur',
	gift_1_year: 'price_gift_year_eur',
	monthly_amount_minor: 499,
	yearly_amount_minor: 4999,
	gift_1_month_amount_minor: 499,
	gift_1_year_amount_minor: 4999,
	currency: 'EUR',
	gift_currency: 'EUR',
	distribution: null,
};

describe('channelUnitAmount', () => {
	test('leaves the standard price after a 5% share, rounded up', () => {
		expect(channelUnitAmount(499, 'EUR', RULE)).toBe(526);
		expect(channelUnitAmount(4999, 'EUR', RULE)).toBe(5263);
	});

	test('rounds currencies Stripe charges in hundreds up to a multiple of 100', () => {
		expect(channelUnitAmount(79900, 'ISK', RULE)).toBe(84200);
		expect(channelUnitAmount(199900, 'HUF', RULE) % 100).toBe(0);
	});

	test('never goes below the base amount', () => {
		expect(channelUnitAmount(0, 'EUR', RULE)).toBe(0);
		expect(channelUnitAmount(1, 'EUR', {...RULE, rate_bps: 1})).toBe(2);
	});
});

describe('applyDistributionToQuote', () => {
	const config: DistributionPricingConfig = {rules: [RULE]};

	test('quotes the channel amount and reports the rule for a known distribution', () => {
		const quote = applyDistributionToQuote(QUOTE, 'ios_marketplace', config);
		expect(quote.monthly_amount_minor).toBe(526);
		expect(quote.gift_1_year_amount_minor).toBe(5263);
		expect(quote.monthly).toBe('price_monthly_eur');
		expect(quote.distribution).toEqual({id: 'ios_marketplace', rate_bps: 500, rule_version: 1});
	});

	test('falls back to the standard price for malformed, unknown or inactive values', () => {
		for (const value of [undefined, '', 'IOS_MARKETPLACE', 'ios marketplace', 'web', 'x'.repeat(40)]) {
			expect(applyDistributionToQuote(QUOTE, value, config)).toEqual(QUOTE);
		}
		expect(applyDistributionToQuote(QUOTE, 'ios_marketplace', {rules: [{...RULE, active: false}]})).toEqual(QUOTE);
	});

	test('only raises the products the rule lists and returns registered channel price IDs', () => {
		const withPrice: DistributionRule = {
			...RULE,
			products: ['plutonium_subscription'],
			prices: {'1:price_monthly_eur': {price_id: 'price_ch', rate_bps: 500, base_unit_amount: 499, unit_amount: 526}},
		};
		const quote = applyDistributionToQuote(QUOTE, 'ios_marketplace', {rules: [withPrice]});
		expect(quote.monthly).toBe('price_ch');
		expect(quote.yearly).toBe('price_yearly_eur');
		expect(quote.yearly_amount_minor).toBe(5263);
		expect(quote.gift_1_month).toBe('price_gift_month_eur');
		expect(quote.gift_1_month_amount_minor).toBe(499);
	});
});

describe('mergeDistributionRules', () => {
	const priced: DistributionRule = {
		...RULE,
		prices: {'1:price_monthly_eur': {price_id: 'price_ch', rate_bps: 500, base_unit_amount: 499, unit_amount: 526}},
	};

	test('starts a new version when the rate changes and keeps the prices', () => {
		const {prices: _prices, rule_version: _version, ...editable} = priced;
		const same = mergeDistributionRules({rules: [priced]}, [{...editable, active: false}]);
		expect(same.rules[0]).toMatchObject({rule_version: 1, active: false, prices: priced.prices});
		const raised = mergeDistributionRules({rules: [priced]}, [{...editable, rate_bps: 600}]);
		expect(raised.rules[0]).toMatchObject({rule_version: 2, rate_bps: 600, prices: priced.prices});
	});

	test('keeps a removed rule with channel prices as inactive and drops one without', () => {
		const merged = mergeDistributionRules({rules: [priced, {...RULE, id: 'other'}]}, []);
		expect(merged.rules).toEqual([{...priced, active: false}]);
	});
});

describe('resolveListPriceId', () => {
	const config: DistributionPricingConfig = {
		rules: [
			{
				...RULE,
				rule_version: 2,
				prices: {
					'1:price_monthly_old': {price_id: 'price_ch_old', rate_bps: 500, base_unit_amount: 399, unit_amount: 421},
					'2:price_monthly_eur': {price_id: 'price_ch_new', rate_bps: 500, base_unit_amount: 499, unit_amount: 526},
				},
			},
		],
	};

	test('keeps the list price inside the distribution', () => {
		expect(resolveListPriceId('price_ch_old', 'price_monthly_eur', config)).toBe('price_ch_new');
		expect(resolveListPriceId('price_ch_new', 'price_monthly_eur', config)).toBe('price_ch_new');
		expect(resolveListPriceId('price_monthly_old', 'price_monthly_eur', config)).toBe('price_monthly_eur');
		expect(resolveListPriceId(null, 'price_monthly_eur', config)).toBe('price_monthly_eur');
	});

	test('offers no list price once the rule is off, so the channel price is never dropped', () => {
		const off = {rules: config.rules.map((rule) => ({...rule, active: false}))};
		expect(resolveListPriceId('price_ch_old', 'price_monthly_eur', off)).toBeNull();
	});
});

describe('decodeDistributionPurchaseRef', () => {
	const encode = (value: unknown) => Buffer.from(JSON.stringify(value)).toString('base64url');

	test('keeps only the external purchase id', () => {
		expect(decodeDistributionPurchaseRef(encode({externalPurchaseId: 'abc-123', bundleId: 'x'}))).toBe('abc-123');
	});

	test('ignores tokens it cannot read', () => {
		expect(decodeDistributionPurchaseRef('not base64 json')).toBeNull();
		expect(decodeDistributionPurchaseRef(encode({externalPurchaseId: 'bad value with spaces'}))).toBeNull();
		expect(decodeDistributionPurchaseRef(encode(['abc']))).toBeNull();
		expect(decodeDistributionPurchaseRef(undefined)).toBeNull();
	});
});
