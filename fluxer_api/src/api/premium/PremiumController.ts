// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	PremiumStateQueryRequest,
	PremiumStateResponse,
	UpdatePremiumPerksDisabledRequest,
} from '@fluxer/schema/src/domains/premium/PremiumSchemas';
import {DefaultUserOnly, LoginRequired} from '../middleware/AuthMiddleware';
import {RateLimitMiddleware} from '../middleware/RateLimitMiddleware';
import {OpenAPI} from '../middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '../RateLimitConfig';
import type {HonoApp} from '../types/HonoEnv';
import {lookupGeoip} from '../utils/IpUtils';
import {Validator} from '../Validator';

export function PremiumController(app: HonoApp) {
	app.get(
		'/premium/state',
		RateLimitMiddleware(RateLimitConfigs.STRIPE_PREMIUM_STATE),
		LoginRequired,
		DefaultUserOnly,
		Validator('query', PremiumStateQueryRequest),
		OpenAPI({
			operationId: 'get_premium_state',
			summary: 'Get premium state',
			description:
				'Returns the authenticated user actual premium entitlement, effective perk state, and mirrored billing data. When Stripe is enabled, missing payment-method mirror data may be repaired lazily.',
			responseSchema: PremiumStateResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Premium',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const {country_code} = ctx.req.valid('query');
			const geoip = await lookupGeoip(ctx.req.raw);
			const state = await ctx.get('stripeService').getPremiumState(userId, geoip.countryCode ?? country_code);
			return ctx.json(state);
		},
	);
	app.patch(
		'/premium/perks-disabled',
		RateLimitMiddleware(RateLimitConfigs.STRIPE_PREMIUM_PERKS_DISABLED),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'set_premium_perks_disabled',
			summary: 'Set premium perks disabled',
			description:
				'Temporarily disables or restores premium perks for the authenticated user while preserving actual subscription and billing state.',
			responseSchema: PremiumStateResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Premium',
		}),
		Validator('json', UpdatePremiumPerksDisabledRequest),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const {disabled} = ctx.req.valid('json');
			const state = await ctx.get('stripeService').setPremiumPerksDisabled(userId, disabled);
			return ctx.json(state);
		},
	);
}
