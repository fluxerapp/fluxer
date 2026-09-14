// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {DonationRateLimitConfigs} from '@app/api/rate_limit_configs/DonationRateLimitConfig';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {Validator} from '@app/api/Validator';
import {
	DonationCheckoutRequest,
	DonationCheckoutResponse,
	DonationManageQuery,
	DonationRequestLinkRequest,
} from '@fluxer/schema/src/domains/donation/DonationSchemas';

export function DonationController(app: HonoApp) {
	app.post(
		'/donations/request-link',
		RateLimitMiddleware(DonationRateLimitConfigs.DONATION_REQUEST_LINK),
		OpenAPI({
			operationId: 'request_donation_magic_link',
			summary: 'Request donation management link',
			description: 'Sends a magic link email to the provided address for managing recurring donations.',
			responseSchema: null,
			statusCode: 204,
			security: [],
			tags: 'Donations',
		}),
		Validator('json', DonationRequestLinkRequest),
		async (ctx) => {
			const {email} = ctx.req.valid('json');
			await ctx.get('donationService').requestMagicLink(email, ctx.get('requestLocale') ?? null);
			return ctx.body(null, 204);
		},
	);
	app.get(
		'/donations/manage',
		RateLimitMiddleware(DonationRateLimitConfigs.DONATION_MANAGE),
		OpenAPI({
			operationId: 'manage_donation',
			summary: 'Manage donation subscription',
			description: 'Validates the magic link token and redirects to Stripe billing portal.',
			responseSchema: null,
			statusCode: 302,
			security: [],
			tags: 'Donations',
		}),
		Validator('query', DonationManageQuery),
		async (ctx) => {
			const {token} = ctx.req.valid('query');
			const {stripeCustomerId} = await ctx.get('donationService').validateMagicLinkToken(token);
			if (!stripeCustomerId) {
				return ctx.redirect(`${Config.endpoints.marketing}/donate`);
			}
			const portalUrl = await ctx.get('donationService').createDonorPortalSession(stripeCustomerId);
			return ctx.redirect(portalUrl);
		},
	);
	app.post(
		'/donations/checkout',
		RateLimitMiddleware(DonationRateLimitConfigs.DONATION_CHECKOUT),
		OpenAPI({
			operationId: 'create_donation_checkout',
			summary: 'Create donation checkout session',
			description: 'Creates a Stripe checkout session for a one-time or recurring donation.',
			responseSchema: DonationCheckoutResponse,
			statusCode: 200,
			security: [],
			tags: 'Donations',
		}),
		Validator('json', DonationCheckoutRequest),
		async (ctx) => {
			const body = ctx.req.valid('json');
			const url = await ctx.get('donationService').createDonationCheckout({
				email: body.email,
				amountCents: body.amount_cents,
				currency: body.currency,
				interval: body.interval,
				isBusiness: body.is_business,
				locale: ctx.get('requestLocale') ?? null,
			});
			return ctx.json({url});
		},
	);
}
