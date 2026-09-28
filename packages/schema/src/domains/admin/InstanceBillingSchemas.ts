// SPDX-License-Identifier: AGPL-3.0-or-later

import {PremiumCurrency} from '@fluxer/schema/src/domains/premium/PremiumSchemas';
import {schemaMetadata} from '@fluxer/schema/src/SchemaMetadata';
import {z} from 'zod';

export const BILLING_MAX_CURRENCIES = 64;
export const BILLING_MAX_COUNTRY_CURRENCIES = 300;
export const BILLING_MAX_LEGACY_SLOTS = 256;
export const BILLING_MAX_LEGACY_PRICES_PER_SLOT = 32;

export const BillingCountryCodeSchema = z.string().regex(/^[A-Z]{2}$/);
export const StripePriceIdSchema = z
	.string()
	.max(255)
	.regex(/^price_[A-Za-z0-9]+$/);
export const BillingLegacyPriceSlotSchema = z.string().regex(/^(monthly|yearly|gift_1_month|gift_1_year)_[A-Z]{3}$/);
export const BillingCatalogModeSchema = z.enum(['env', 'operator']);
export type BillingCatalogMode = z.infer<typeof BillingCatalogModeSchema>;

function maxKeys(max: number) {
	return (value: Record<string, unknown>) => Object.keys(value).length <= max;
}

export const BillingPriceSetResponse = z.object({
	monthly: StripePriceIdSchema.nullable(),
	yearly: StripePriceIdSchema.nullable(),
	gift_1_month: StripePriceIdSchema.nullable(),
	gift_1_year: StripePriceIdSchema.nullable(),
});
export type BillingPriceSetResponse = z.infer<typeof BillingPriceSetResponse>;

export const BillingPriceSetUpdateRequest = z.object({
	monthly: StripePriceIdSchema.nullish(),
	yearly: StripePriceIdSchema.nullish(),
	gift_1_month: StripePriceIdSchema.nullish(),
	gift_1_year: StripePriceIdSchema.nullish(),
});
export type BillingPriceSetUpdateRequest = z.infer<typeof BillingPriceSetUpdateRequest>;

const BillingPricesSchema = <T extends z.ZodType>(entry: T) =>
	z
		.record(PremiumCurrency, entry)
		.refine(maxKeys(BILLING_MAX_CURRENCIES), {message: `At most ${BILLING_MAX_CURRENCIES} currencies`});

export const BillingCountryCurrenciesSchema = z
	.record(BillingCountryCodeSchema, PremiumCurrency)
	.refine(maxKeys(BILLING_MAX_COUNTRY_CURRENCIES), {
		message: `At most ${BILLING_MAX_COUNTRY_CURRENCIES} country mappings`,
	});

export const BillingLegacyPricesSchema = z
	.record(BillingLegacyPriceSlotSchema, z.array(StripePriceIdSchema).max(BILLING_MAX_LEGACY_PRICES_PER_SLOT))
	.refine(maxKeys(BILLING_MAX_LEGACY_SLOTS), {message: `At most ${BILLING_MAX_LEGACY_SLOTS} legacy price slots`});

export const InstanceBillingResponse = z.object({
	enabled: z.boolean().nullable(),
	effective_enabled: z.boolean(),
	stripe_secret_key_set: z.boolean(),
	stripe_webhook_secret_set: z.boolean(),
	stripe_secret_key_stored: z.boolean(),
	stripe_webhook_secret_stored: z.boolean(),
	automatic_tax: z.boolean().nullable(),
	tax_id_collection: z.boolean().nullable(),
	terms_consent_required: z.boolean().nullable(),
	effective_automatic_tax: z.boolean(),
	effective_tax_id_collection: z.boolean(),
	effective_terms_consent_required: z.boolean(),
	default_currency: PremiumCurrency.nullable(),
	prices: z.record(z.string(), BillingPriceSetResponse).nullable(),
	country_currencies: z.record(z.string(), z.string()).nullable(),
	legacy_prices: z.record(z.string(), z.array(z.string())).nullable(),
	billing_active: z.boolean(),
	stripe_serviceable: z.boolean(),
	catalog_mode: BillingCatalogModeSchema,
	webhook_url: z.string(),
});
export type InstanceBillingResponse = z.infer<typeof InstanceBillingResponse>;

export const InstanceBillingUpdateRequest = z
	.object({
		enabled: z.boolean().nullish(),
		stripe_secret_key: z.string().trim().min(1).max(4096).nullish(),
		stripe_webhook_secret: z.string().trim().min(1).max(4096).nullish(),
		automatic_tax: z.boolean().nullish(),
		tax_id_collection: z.boolean().nullish(),
		terms_consent_required: z.boolean().nullish(),
		default_currency: PremiumCurrency.nullish(),
		prices: BillingPricesSchema(BillingPriceSetUpdateRequest).nullish(),
		country_currencies: BillingCountryCurrenciesSchema.nullish(),
		legacy_prices: BillingLegacyPricesSchema.nullish(),
	})
	.register(schemaMetadata, {preserveNullFields: true});
export type InstanceBillingUpdateRequest = z.infer<typeof InstanceBillingUpdateRequest>;

export const StoredBillingConfigSchema = z.object({
	enabled: z.boolean().nullable().default(null),
	stripe_secret_key: z
		.string()
		.trim()
		.nullable()
		.default(null)
		.transform((value) => value || null),
	stripe_webhook_secret: z
		.string()
		.trim()
		.nullable()
		.default(null)
		.transform((value) => value || null),
	automatic_tax: z.boolean().nullable().default(null),
	tax_id_collection: z.boolean().nullable().default(null),
	terms_consent_required: z.boolean().nullable().default(null),
	default_currency: PremiumCurrency.nullable().default(null),
	prices: BillingPricesSchema(
		z.object({
			monthly: StripePriceIdSchema.nullable().default(null),
			yearly: StripePriceIdSchema.nullable().default(null),
			gift_1_month: StripePriceIdSchema.nullable().default(null),
			gift_1_year: StripePriceIdSchema.nullable().default(null),
		}),
	)
		.nullable()
		.default(null),
	country_currencies: BillingCountryCurrenciesSchema.nullable().default(null),
	legacy_prices: BillingLegacyPricesSchema.nullable().default(null),
});
export type StoredBillingConfig = z.output<typeof StoredBillingConfigSchema>;
