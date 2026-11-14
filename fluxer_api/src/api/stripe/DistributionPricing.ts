// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {stripeCurrencyExponent} from '@fluxer/number_utils/src/PriceFormatting';
import {
	DEFAULT_HOSTED_DISTRIBUTION_PRICING_CONFIG,
	type DistributionChannelPrice,
	type DistributionPricingConfig,
	type DistributionProduct,
	type DistributionRule,
} from '@fluxer/schema/src/domains/admin/InstanceDistributionPricingSchemas';
import {DISTRIBUTION_ID_PATTERN} from '@fluxer/schema/src/domains/premium/DistributionSchemas';
import type {PriceIdsResponse} from '@fluxer/schema/src/domains/premium/PremiumSchemas';

interface DistributionPrice {
	id: string;
	rateBps: number;
	ruleVersion: number;
	baseUnitAmount: number;
	unitAmount: number;
}

export interface ProductDistribution extends DistributionPrice {
	basePriceId: string;
}

export interface DistributionSale extends DistributionPrice {
	purchaseRef: string | null;
}

const DISTRIBUTION_METADATA_KEYS = [
	'distribution',
	'distribution_rate_bps',
	'distribution_rule_version',
	'distribution_base_unit_amount',
	'distribution_unit_amount',
	'distribution_extra_unit_amount',
	'distribution_purchase_ref',
] as const;

const MULTIPLE_OF_100_CURRENCIES = new Set(['HUF', 'ISK', 'TWD', 'UGX']);
const PURCHASE_REF_PATTERN = /^[A-Za-z0-9._:-]{1,128}$/;
const BPS_SCALE = 10000;

let storedConfig: DistributionPricingConfig | null = null;
let reverseIndex: {config: DistributionPricingConfig; byPriceId: Map<string, ProductDistributionRef>} | null = null;

interface ProductDistributionRef {
	id: string;
	ruleVersion: number;
	basePriceId: string;
	entry: DistributionChannelPrice;
}

export function defaultDistributionPricingConfig(): DistributionPricingConfig {
	return Config.instance.selfHosted ? {rules: []} : structuredClone(DEFAULT_HOSTED_DISTRIBUTION_PRICING_CONFIG);
}

export function setDistributionPricingConfig(config: DistributionPricingConfig | null): void {
	storedConfig = config;
}

export function getDistributionPricingConfig(): DistributionPricingConfig {
	storedConfig ??= defaultDistributionPricingConfig();
	return storedConfig;
}

export function parseDistributionId(value: string | null | undefined): string | null {
	return typeof value === 'string' && DISTRIBUTION_ID_PATTERN.test(value) ? value : null;
}

export function distributionProductOf(info: {isGift: boolean}): DistributionProduct {
	return info.isGift ? 'plutonium_gift' : 'plutonium_subscription';
}

function findDistributionRule(
	id: string | null,
	config: DistributionPricingConfig = getDistributionPricingConfig(),
): DistributionRule | null {
	if (id === null) return null;
	return config.rules.find((rule) => rule.id === id) ?? null;
}

export function resolveDistributionRule(
	id: string | null,
	product: DistributionProduct,
	config: DistributionPricingConfig = getDistributionPricingConfig(),
): DistributionRule | null {
	const rule = findDistributionRule(id, config);
	return rule?.active && rule.products.includes(product) ? rule : null;
}

function roundingStep(currency: string): number {
	const code = currency.toUpperCase();
	if (MULTIPLE_OF_100_CURRENCIES.has(code)) return 100;
	return stripeCurrencyExponent(code) === 3 ? 10 : 1;
}

export function channelUnitAmount(
	baseMinor: number,
	currency: string,
	rule: Pick<DistributionRule, 'rate_bps'>,
): number {
	const divisor = BPS_SCALE - rule.rate_bps;
	const raw = Math.floor((baseMinor * BPS_SCALE + divisor - 1) / divisor);
	const step = roundingStep(currency);
	return Math.max(baseMinor, Math.ceil(raw / step) * step);
}

export function channelPriceKey(ruleVersion: number, basePriceId: string): string {
	return `${ruleVersion}:${basePriceId}`;
}

export function channelPriceLookupKey(
	rule: Pick<DistributionRule, 'id' | 'rule_version' | 'rate_bps'>,
	basePriceId: string,
): string {
	return `dist_${rule.id}_${rule.rule_version}_${rule.rate_bps}_${basePriceId}`;
}

export function lookupChannelPrice(rule: DistributionRule, basePriceId: string): DistributionChannelPrice | null {
	return rule.prices[channelPriceKey(rule.rule_version, basePriceId)] ?? null;
}

function buildReverseIndex(config: DistributionPricingConfig): Map<string, ProductDistributionRef> {
	const byPriceId = new Map<string, ProductDistributionRef>();
	for (const rule of config.rules) {
		for (const [key, entry] of Object.entries(rule.prices)) {
			const separator = key.indexOf(':');
			byPriceId.set(entry.price_id, {
				id: rule.id,
				ruleVersion: Number(key.slice(0, separator)),
				basePriceId: key.slice(separator + 1),
				entry,
			});
		}
	}
	return byPriceId;
}

export function findChannelPrice(
	priceId: string,
	config: DistributionPricingConfig = getDistributionPricingConfig(),
): ProductDistribution | null {
	if (reverseIndex?.config !== config) {
		reverseIndex = {config, byPriceId: buildReverseIndex(config)};
	}
	const ref = reverseIndex.byPriceId.get(priceId);
	if (!ref) return null;
	return {
		id: ref.id,
		ruleVersion: ref.ruleVersion,
		rateBps: ref.entry.rate_bps,
		basePriceId: ref.basePriceId,
		baseUnitAmount: ref.entry.base_unit_amount,
		unitAmount: ref.entry.unit_amount,
	};
}

export function distributionSaleFromEntry(
	rule: Pick<DistributionRule, 'id' | 'rule_version'>,
	entry: DistributionChannelPrice,
	purchaseRef: string | null,
): DistributionSale {
	return {
		id: rule.id,
		rateBps: entry.rate_bps,
		ruleVersion: rule.rule_version,
		baseUnitAmount: entry.base_unit_amount,
		unitAmount: entry.unit_amount,
		purchaseRef,
	};
}

export function distributionMetadata(sale: DistributionPrice & {purchaseRef?: string | null}): Record<string, string> {
	return {
		distribution: sale.id,
		distribution_rate_bps: String(sale.rateBps),
		distribution_rule_version: String(sale.ruleVersion),
		distribution_base_unit_amount: String(sale.baseUnitAmount),
		distribution_unit_amount: String(sale.unitAmount),
		distribution_extra_unit_amount: String(sale.unitAmount - sale.baseUnitAmount),
		...(sale.purchaseRef ? {distribution_purchase_ref: sale.purchaseRef} : {}),
	};
}

export function pickDistributionMetadata(
	metadata: Readonly<Record<string, string>> | null | undefined,
): Record<string, string> {
	if (!metadata?.['distribution']) return {};
	const picked: Record<string, string> = {};
	for (const key of DISTRIBUTION_METADATA_KEYS) {
		const value = metadata[key];
		if (value) picked[key] = value;
	}
	return picked;
}

function metadataInt(value: string | undefined): number | null {
	return value && /^\d{1,10}$/.test(value) ? Number(value) : null;
}

export function distributionSaleFromMetadata(
	metadata: Readonly<Record<string, string>> | null | undefined,
): DistributionSale | null {
	const id = parseDistributionId(metadata?.['distribution']);
	const rateBps = metadataInt(metadata?.['distribution_rate_bps']);
	const ruleVersion = metadataInt(metadata?.['distribution_rule_version']);
	const baseUnitAmount = metadataInt(metadata?.['distribution_base_unit_amount']);
	const unitAmount = metadataInt(metadata?.['distribution_unit_amount']);
	if (!id || rateBps === null || ruleVersion === null || baseUnitAmount === null || unitAmount === null) return null;
	return {
		id,
		rateBps,
		ruleVersion,
		baseUnitAmount,
		unitAmount,
		purchaseRef: metadata?.['distribution_purchase_ref'] || null,
	};
}

export function distributionPaymentFields(sale: DistributionSale | null): {
	distribution: string | null;
	distribution_rate_bps: number | null;
	distribution_rule_version: number | null;
	distribution_base_amount_minor: number | null;
	distribution_purchase_ref: string | null;
} {
	return {
		distribution: sale?.id ?? null,
		distribution_rate_bps: sale?.rateBps ?? null,
		distribution_rule_version: sale?.ruleVersion ?? null,
		distribution_base_amount_minor: sale?.baseUnitAmount ?? null,
		distribution_purchase_ref: sale?.purchaseRef ?? null,
	};
}

export function decodeDistributionPurchaseRef(token: string | null | undefined): string | null {
	if (!token) return null;
	try {
		const payload = token.includes('.') ? (token.split('.')[1] ?? '') : token;
		const decoded: unknown = JSON.parse(Buffer.from(payload, 'base64url').toString('utf8'));
		if (typeof decoded !== 'object' || decoded === null) return null;
		const ref = (decoded as Record<string, unknown>)['externalPurchaseId'];
		return typeof ref === 'string' && PURCHASE_REF_PATTERN.test(ref) ? ref : null;
	} catch {
		return null;
	}
}

type QuoteSlot = 'monthly' | 'yearly' | 'gift_1_month' | 'gift_1_year';

const QUOTE_SLOTS: ReadonlyArray<{slot: QuoteSlot; amount: `${QuoteSlot}_amount_minor`; product: DistributionProduct}> =
	[
		{slot: 'monthly', amount: 'monthly_amount_minor', product: 'plutonium_subscription'},
		{slot: 'yearly', amount: 'yearly_amount_minor', product: 'plutonium_subscription'},
		{slot: 'gift_1_month', amount: 'gift_1_month_amount_minor', product: 'plutonium_gift'},
		{slot: 'gift_1_year', amount: 'gift_1_year_amount_minor', product: 'plutonium_gift'},
	];

export function applyDistributionToQuote(
	quote: PriceIdsResponse,
	distribution: string | null | undefined,
	config: DistributionPricingConfig = getDistributionPricingConfig(),
): PriceIdsResponse {
	const id = parseDistributionId(distribution);
	const next: PriceIdsResponse = {...quote, distribution: null};
	for (const {slot, amount, product} of QUOTE_SLOTS) {
		const rule = resolveDistributionRule(id, product, config);
		const basePriceId = quote[slot];
		if (!rule || !basePriceId) continue;
		const currency = product === 'plutonium_gift' ? quote.gift_currency : quote.currency;
		const baseAmount = quote[amount];
		next[slot] = lookupChannelPrice(rule, basePriceId)?.price_id ?? basePriceId;
		next[amount] = baseAmount == null || !currency ? null : channelUnitAmount(baseAmount, currency, rule);
		next.distribution = {id: rule.id, rate_bps: rule.rate_bps, rule_version: rule.rule_version};
	}
	return next;
}

export function mergeDistributionRules(
	current: DistributionPricingConfig,
	updates: ReadonlyArray<Omit<DistributionRule, 'rule_version' | 'prices'>>,
): DistributionPricingConfig {
	const existingById = new Map(current.rules.map((rule) => [rule.id, rule]));
	const updatedIds = new Set(updates.map((rule) => rule.id));
	const rules: Array<DistributionRule> = updates.map((next) => {
		const existing = existingById.get(next.id);
		if (!existing) return {...next, rule_version: 1, prices: {}};
		return {
			...next,
			rule_version: existing.rate_bps !== next.rate_bps ? existing.rule_version + 1 : existing.rule_version,
			prices: existing.prices,
		};
	});
	const retired = current.rules
		.filter((rule) => !updatedIds.has(rule.id) && Object.keys(rule.prices).length > 0)
		.map((rule) => ({...rule, active: false}));
	return {rules: [...rules, ...retired]};
}

export function resolveListPriceId(
	currentPriceId: string | null,
	baseListPriceId: string | null,
	config: DistributionPricingConfig = getDistributionPricingConfig(),
): string | null {
	const current = currentPriceId ? findChannelPrice(currentPriceId, config) : null;
	if (!current || !currentPriceId) return baseListPriceId;
	if (!baseListPriceId) return null;
	const rule = resolveDistributionRule(current.id, 'plutonium_subscription', config);
	const entry = rule ? lookupChannelPrice(rule, baseListPriceId) : null;
	if (entry) return entry.price_id;
	return current.basePriceId === baseListPriceId ? currentPriceId : null;
}

export function subscriptionDistributionMetadata(
	priceId: string,
	config: DistributionPricingConfig = getDistributionPricingConfig(),
): Record<string, string> {
	const distribution = findChannelPrice(priceId, config);
	if (distribution) return distributionMetadata(distribution);
	return Object.fromEntries(DISTRIBUTION_METADATA_KEYS.map((key) => [key, '']));
}
