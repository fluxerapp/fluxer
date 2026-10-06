// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createStringType,
	SnowflakeStringType,
	SnowflakeType,
	withOpenApiType,
} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

const StoreProviderSchema = withOpenApiType(z.enum(['app_store', 'google_play']), 'StoreProvider');

const StoreSlotSchema = withOpenApiType(z.enum(['monthly', 'yearly', 'gift_1_month', 'gift_1_year']), 'StoreSlot');

const StorePurchaseKindSchema = withOpenApiType(z.enum(['subscription', 'gift']), 'StorePurchaseKind');

const StoreEnvironmentSchema = withOpenApiType(z.enum(['production', 'sandbox']), 'StoreEnvironment');

const StorePurchaseStateSchema = withOpenApiType(
	z.enum([
		'pending',
		'active',
		'grace',
		'billing_retry',
		'on_hold',
		'paused',
		'canceled',
		'expired',
		'revoked',
		'superseded',
		'purchased',
		'fulfilled',
		'refunded',
	]),
	'StorePurchaseState',
);

const StorePurchaseBlockedReasonSchema = withOpenApiType(
	z.enum(['lifetime', 'existing_subscription', 'purchase_disabled']),
	'StorePurchaseBlockedReason',
);

const StoreBlockingProviderSchema = withOpenApiType(
	z.enum(['stripe', 'app_store', 'google_play']),
	'StoreBlockingProvider',
);

const StoreBillingCycleSchema = withOpenApiType(z.enum(['monthly', 'yearly']), 'StoreBillingCycle');

const StoreSubscriptionSlotSchema = withOpenApiType(z.enum(['monthly', 'yearly']), 'StoreSubscriptionSlot');

export const StoreBillingAppStoreProductResponse = z.object({
	product_id: z.string().describe('App Store product identifier'),
	slot: StoreSlotSchema.describe('Fluxer product this App Store product sells'),
});

export type StoreBillingAppStoreProductResponse = z.infer<typeof StoreBillingAppStoreProductResponse>;

export const StoreBillingGooglePlayProductResponse = z.object({
	product_id: z.string().describe('Google Play product identifier'),
	base_plan_id: z.string().nullable().describe('Google Play base plan identifier, null for one-time products'),
	slot: StoreSlotSchema.describe('Fluxer product this Google Play product sells'),
});

export type StoreBillingGooglePlayProductResponse = z.infer<typeof StoreBillingGooglePlayProductResponse>;

export const StoreBillingContextResponse = z.object({
	app_account_token: z
		.string()
		.describe(
			'Lowercase UUID for this account. Pass it as appAccountToken on App Store purchases and as obfuscatedAccountId on Google Play purchases',
		),
	app_store: z
		.object({
			enabled: z.boolean().describe('Whether App Store purchases are accepted'),
			bundle_ids: z.array(z.string()).describe('App bundle identifiers whose purchases are accepted'),
			products: z.array(StoreBillingAppStoreProductResponse).describe('App Store products on sale'),
		})
		.describe('App Store purchase settings'),
	google_play: z
		.object({
			enabled: z.boolean().describe('Whether Google Play purchases are accepted'),
			package_names: z.array(z.string()).describe('App package names whose purchases are accepted'),
			products: z.array(StoreBillingGooglePlayProductResponse).describe('Google Play products on sale'),
		})
		.describe('Google Play purchase settings'),
	purchase_blocked_reason: StorePurchaseBlockedReasonSchema.nullable().describe(
		'Why this account cannot buy a subscription right now, null when it can',
	),
	blocking_provider: StoreBlockingProviderSchema.nullable().describe(
		'Provider of the active subscription that blocks a new one, null when nothing blocks',
	),
});

export type StoreBillingContextResponse = z.infer<typeof StoreBillingContextResponse>;

export const ClaimAppStoreTransactionRequest = z.object({
	signed_transaction: createStringType(1, 32768).describe(
		'JWS signed transaction from StoreKit 2 (Transaction.jwsRepresentation)',
	),
});

export type ClaimAppStoreTransactionRequest = z.infer<typeof ClaimAppStoreTransactionRequest>;

export const ClaimGooglePlayPurchaseRequest = z.object({
	purchase_token: createStringType(1, 1024).describe('Purchase token from Google Play Billing'),
	product_id: createStringType(1, 256).describe('Google Play product identifier of the purchase'),
	package_name: createStringType(1, 256)
		.optional()
		.describe('Package name of the app that made the purchase. Must be one of the accepted package names'),
});

export type ClaimGooglePlayPurchaseRequest = z.infer<typeof ClaimGooglePlayPurchaseRequest>;

export const StorePurchaseResponse = z.object({
	id: SnowflakeStringType.describe('The unique identifier (snowflake) for this store purchase'),
	provider: StoreProviderSchema.describe('Store the purchase was made in'),
	kind: StorePurchaseKindSchema.describe('Whether the purchase is a subscription or a gift'),
	slot: StoreSlotSchema.describe('Fluxer product the purchase is for'),
	product_id: z.string().describe('Store product identifier'),
	environment: StoreEnvironmentSchema.describe('Whether the purchase was a real purchase or a test purchase'),
	state: StorePurchaseStateSchema.describe('Current state of the purchase'),
	entitled: z.boolean().describe('Whether the purchase currently grants Plutonium'),
	expires_at: z.iso.datetime().nullable().describe('When the paid period ends, null for gifts'),
	entitled_until: z.iso
		.datetime()
		.nullable()
		.describe('When access from this purchase ends, including any grace period, null when not entitled'),
	will_renew: z.boolean().nullable().describe('Whether the subscription renews automatically, null for gifts'),
	gift_code: z.string().nullable().describe('Gift code minted by a gift purchase, null otherwise'),
	created_at: z.iso.datetime().describe('When Fluxer first saw the purchase'),
});

export type StorePurchaseResponse = z.infer<typeof StorePurchaseResponse>;

export const StorePurchaseClaimResponse = z.object({
	purchase: StorePurchaseResponse.describe('The claimed purchase'),
	gift_code: z.string().nullable().describe('Gift code minted by a gift purchase, null otherwise'),
});

export type StorePurchaseClaimResponse = z.infer<typeof StorePurchaseClaimResponse>;

export const StorePurchaseListResponse = z.array(StorePurchaseResponse);

export type StorePurchaseListResponse = z.infer<typeof StorePurchaseListResponse>;

export const StorePurchaseIdParam = z.object({
	purchase_id: SnowflakeType.describe('The ID of the store purchase'),
});

export type StorePurchaseIdParam = z.infer<typeof StorePurchaseIdParam>;

export const PremiumStoreSubscriptionState = z.object({
	provider: StoreProviderSchema.describe('Store that bills the subscription'),
	purchase_id: SnowflakeStringType.describe('ID of the store purchase that grants the subscription'),
	slot: StoreSubscriptionSlotSchema.describe('Fluxer subscription product'),
	billing_cycle: StoreBillingCycleSchema.describe('Billing cycle of the subscription'),
	state: StorePurchaseStateSchema.describe('Current state of the store subscription'),
	expires_at: z.iso.datetime().describe('When the paid period ends'),
	grace_ends_at: z.iso.datetime().nullable().describe('When the billing grace period ends, null outside grace'),
	will_renew: z.boolean().describe('Whether the subscription renews automatically'),
	manage_url: z.string().describe('Store page where the subscription can be managed or canceled'),
	environment: StoreEnvironmentSchema.describe('Whether the subscription is a real purchase or a test purchase'),
});

export type PremiumStoreSubscriptionState = z.infer<typeof PremiumStoreSubscriptionState>;
