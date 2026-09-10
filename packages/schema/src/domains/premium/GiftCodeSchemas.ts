// SPDX-License-Identifier: AGPL-3.0-or-later

import {UserPartialResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {createStringType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

export const CheckoutPaymentMethodEnum = z.enum(['card', 'pix', 'upi']);

export type CheckoutPaymentMethod = z.infer<typeof CheckoutPaymentMethodEnum>;

export const CreateCheckoutSessionRequest = z.object({
	price_id: createStringType().describe('The Stripe price ID for the subscription plan'),
	country_code: createStringType(2, 2)
		.optional()
		.describe(
			'Two-letter country code used for regional pricing. Only used when the server cannot geolocate the request; otherwise the request GeoIP country wins.',
		),
	client_geoip_country_code: createStringType(2, 2)
		.optional()
		.describe('Two-letter country code observed by the client GeoIP store before checkout'),
	eu_withdrawal_waiver_accepted: z
		.boolean()
		.optional()
		.describe('Whether the EU/EEA digital content withdrawal waiver was expressly accepted before checkout'),
	payment_method: CheckoutPaymentMethodEnum.optional().describe(
		'Preferred payment method. card (default) uses the account Payment Method Configuration. pix requires a BRL recurring price and enables Pix Automático. upi requires an INR recurring price and enables RBI-compliant UPI mandates.',
	),
	is_business: z
		.boolean()
		.optional()
		.describe(
			'Whether the purchase is for a business. When true, Stripe Checkout requires a billing address (needed for tax invoicing). When false or omitted, billing address collection is left to Stripe (auto).',
		),
});

export type CreateCheckoutSessionRequest = z.infer<typeof CreateCheckoutSessionRequest>;

const GiftCodeDurationTypeEnum = z.enum(['days', 'weeks', 'months', 'years']);
export const GiftCodeResponse = z.object({
	code: z.string().describe('The unique gift code string'),
	duration_type: GiftCodeDurationTypeEnum.describe('Duration unit for the gift entitlement'),
	duration_quantity: z.number().int().describe('Duration quantity for the selected duration unit'),
	redeemed: z.boolean().describe('Whether the gift code has been redeemed'),
	created_by: z
		.lazy(() => UserPartialResponse)
		.nullish()
		.describe('The user who created the gift code'),
});

export type GiftCodeResponse = z.infer<typeof GiftCodeResponse>;

export const GiftCodeMetadataResponse = z.object({
	code: z.string().describe('The unique gift code string'),
	duration_type: GiftCodeDurationTypeEnum.describe('Duration unit for the gift entitlement'),
	duration_quantity: z.number().int().describe('Duration quantity for the selected duration unit'),
	created_at: z.iso.datetime().describe('Timestamp when the gift code was created'),
	created_by: z.lazy(() => UserPartialResponse).describe('The user who created the gift code'),
	redeemed_at: z.iso.datetime().nullish().describe('Timestamp when the gift code was redeemed'),
	redeemed_by: z
		.lazy(() => UserPartialResponse)
		.nullish()
		.describe('The user who redeemed the gift code'),
});

export type GiftCodeMetadataResponse = z.infer<typeof GiftCodeMetadataResponse>;
