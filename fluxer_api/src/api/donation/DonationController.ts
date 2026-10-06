// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {DonationRateLimitConfigs} from '@app/api/rate_limit_configs/DonationRateLimitConfig';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {Validator} from '@app/api/Validator';
import {DonationMagicLinkExpiredError} from '@fluxer/errors/src/domains/donation/DonationMagicLinkExpiredError';
import {DonationMagicLinkInvalidError} from '@fluxer/errors/src/domains/donation/DonationMagicLinkInvalidError';
import {DonationMagicLinkUsedError} from '@fluxer/errors/src/domains/donation/DonationMagicLinkUsedError';
import {StripeError} from '@fluxer/errors/src/domains/payment/StripeError';
import {StripePaymentNotAvailableError} from '@fluxer/errors/src/domains/payment/StripePaymentNotAvailableError';
import {
	DonationCheckoutRequest,
	DonationCheckoutResponse,
	DonationManageQuery,
	DonationRequestLinkRequest,
} from '@fluxer/schema/src/domains/donation/DonationSchemas';

function donationManageAlertUrl(alert: string): string {
	return `${Config.endpoints.marketing}/donate/manage?alert=${alert}`;
}

function getMagicLinkAlert(error: unknown): string | null {
	if (error instanceof DonationMagicLinkExpiredError) {
		return 'link_expired';
	}
	if (error instanceof DonationMagicLinkUsedError) {
		return 'link_used';
	}
	if (error instanceof DonationMagicLinkInvalidError) {
		return 'link_invalid';
	}
	if (error instanceof StripeError || error instanceof StripePaymentNotAvailableError) {
		return 'portal_error';
	}
	return null;
}

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
			summary: 'Open donation management link',
			description:
				'Checks the magic link token without consuming it and redirects to the donation management confirmation page.',
			responseSchema: null,
			statusCode: 302,
			security: [],
			tags: 'Donations',
		}),
		Validator('query', DonationManageQuery),
		async (ctx) => {
			const {token} = ctx.req.valid('query');
			try {
				const {stripeCustomerId} = await ctx.get('donationService').validateMagicLinkToken(token);
				if (!stripeCustomerId) {
					return ctx.redirect(donationManageAlertUrl('no_customer'), 302);
				}
			} catch (error: unknown) {
				const alert = getMagicLinkAlert(error);
				if (!alert) {
					throw error;
				}
				return ctx.redirect(donationManageAlertUrl(alert), 302);
			}
			const encodedToken = encodeURIComponent(token);
			return ctx.redirect(`${Config.endpoints.marketing}/donate/manage/confirm?token=${encodedToken}`, 302);
		},
	);
	app.post(
		'/donations/manage',
		RateLimitMiddleware(DonationRateLimitConfigs.DONATION_MANAGE),
		OpenAPI({
			operationId: 'redeem_donation_magic_link',
			summary: 'Redeem donation management link',
			description: 'Consumes the magic link token and redirects to the Stripe billing portal.',
			responseSchema: null,
			statusCode: 302,
			security: [],
			tags: 'Donations',
		}),
		Validator('query', DonationManageQuery),
		async (ctx) => {
			const {token} = ctx.req.valid('query');
			try {
				const portalUrl = await ctx.get('donationService').redeemMagicLinkToken(token);
				if (!portalUrl) {
					return ctx.redirect(donationManageAlertUrl('no_customer'), 302);
				}
				return ctx.redirect(portalUrl, 302);
			} catch (error: unknown) {
				const alert = getMagicLinkAlert(error);
				if (!alert) {
					throw error;
				}
				return ctx.redirect(donationManageAlertUrl(alert), 302);
			}
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
