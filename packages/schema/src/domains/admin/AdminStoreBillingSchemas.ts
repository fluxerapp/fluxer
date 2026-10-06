// SPDX-License-Identifier: AGPL-3.0-or-later

import {SnowflakeStringType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

export const AdminStorePurchaseResponse = z.object({
	id: SnowflakeStringType.describe('The unique identifier (snowflake) for this store purchase'),
	user_id: SnowflakeStringType.nullable().describe('ID of the user the purchase is bound to, null when unbound'),
	provider: z.enum(['app_store', 'google_play']).describe('Store the purchase was made in'),
	kind: z.enum(['subscription', 'gift']).describe('Whether the purchase is a subscription or a gift'),
	slot: z.enum(['monthly', 'yearly', 'gift_1_month', 'gift_1_year']).describe('Fluxer product the purchase is for'),
	environment: z.enum(['production', 'sandbox']).describe('Whether the purchase was real or a test purchase'),
	app_id: z.string().describe('Bundle identifier or package name of the app that made the purchase'),
	product_id: z.string().describe('Store product identifier'),
	base_plan_id: z.string().nullable().describe('Google Play base plan identifier, null otherwise'),
	latest_transaction_id: z
		.string()
		.nullable()
		.describe('Latest App Store transaction ID or Google Play order ID for the purchase'),
	ownership_type: z.string().nullable().describe('Store ownership type, such as PURCHASED or FAMILY_SHARED'),
	state: z.string().describe('Normalized state of the purchase'),
	store_state: z.string().nullable().describe('Raw state reported by the store'),
	entitled: z.boolean().describe('Whether the purchase currently grants Plutonium'),
	expires_at: z.iso.datetime().nullable().describe('When the paid period ends'),
	grace_ends_at: z.iso.datetime().nullable().describe('When the billing grace period ends'),
	auto_renew: z.boolean().nullable().describe('Whether the subscription renews automatically'),
	started_at: z.iso.datetime().nullable().describe('When the subscription or purchase started'),
	purchased_at: z.iso.datetime().nullable().describe('When the latest payment was made'),
	revoked_at: z.iso.datetime().nullable().describe('When the store revoked or refunded the purchase'),
	revocation_reason: z.string().nullable().describe('Reason the store gave for the revocation'),
	superseded: z.boolean().describe('Whether a newer purchase replaced this one'),
	acknowledged: z.boolean().describe('Whether the purchase was acknowledged with the store'),
	gift_code: z.string().nullable().describe('Gift code minted by a gift purchase'),
	bound_at: z.iso.datetime().nullable().describe('When the purchase was bound to its user'),
	last_event_at: z.iso.datetime().nullable().describe('Time of the latest store event applied to the purchase'),
	synced_at: z.iso.datetime().nullable().describe('When the purchase was last synced with the store'),
	created_at: z.iso.datetime().describe('When Fluxer first saw the purchase'),
	updated_at: z.iso.datetime().describe('When the purchase record last changed'),
});

export type AdminStorePurchaseResponse = z.infer<typeof AdminStorePurchaseResponse>;

export const AdminStorePurchaseListResponse = z.object({
	purchases: z.array(AdminStorePurchaseResponse).describe('Store purchases bound to the user'),
});

export type AdminStorePurchaseListResponse = z.infer<typeof AdminStorePurchaseListResponse>;
