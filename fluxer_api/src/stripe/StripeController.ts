/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {HonoApp} from '~/App';
import {StripeWebhookSignatureMissingError} from '~/Errors';
import {DefaultUserOnly, LoginRequired} from '~/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '~/middleware/RateLimitMiddleware';
import {RateLimitConfigs} from '~/RateLimitConfig';
import {createStringType, z} from '~/Schema';
import {CreateCheckoutSessionRequest, mapGiftCodeToMetadataResponse, mapGiftCodeToResponse} from '~/stripe/StripeModel';
import {Validator} from '~/Validator';

export const StripeController = (app: HonoApp) => {
	app.post('/stripe/webhook', async (ctx) => {
		const signature = ctx.req.header('stripe-signature');
		if (!signature) {
			throw new StripeWebhookSignatureMissingError();
		}
		const body = await ctx.req.text();
		await ctx.get('stripeService').handleWebhook({body, signature});
		return ctx.json({received: true});
	});

	app.post(
		'/stripe/checkout/subscription',
		RateLimitMiddleware(RateLimitConfigs.STRIPE_CHECKOUT_SUBSCRIPTION),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', CreateCheckoutSessionRequest),
		async (ctx) => {
			const {price_id} = ctx.req.valid('json');
			const userId = ctx.get('user').id;
			const checkoutUrl = await ctx.get('stripeService').createCheckoutSession({
				userId,
				priceId: price_id,
				isGift: false,
			});
			return ctx.json({url: checkoutUrl});
		},
	);

	app.post(
		'/stripe/checkout/gift',
		RateLimitMiddleware(RateLimitConfigs.STRIPE_CHECKOUT_GIFT),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', CreateCheckoutSessionRequest),
		async (ctx) => {
			const {price_id} = ctx.req.valid('json');
			const userId = ctx.get('user').id;
			const checkoutUrl = await ctx.get('stripeService').createCheckoutSession({
				userId,
				priceId: price_id,
				isGift: true,
			});
			return ctx.json({url: checkoutUrl});
		},
	);

	app.get(
		'/gifts/:code',
		RateLimitMiddleware(RateLimitConfigs.GIFT_CODE_GET),
		Validator('param', z.object({code: createStringType()})),
		async (ctx) => {
			const {code} = ctx.req.valid('param');
			const giftCode = await ctx.get('stripeService').getGiftCode(code);
			const response = await mapGiftCodeToResponse({
				giftCode,
				userCacheService: ctx.get('userCacheService'),
				requestCache: ctx.get('requestCache'),
				includeCreator: true,
			});
			return ctx.json(response);
		},
	);

	app.post(
		'/gifts/:code/redeem',
		RateLimitMiddleware(RateLimitConfigs.GIFT_CODE_REDEEM),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', z.object({code: createStringType()})),
		async (ctx) => {
			const {code} = ctx.req.valid('param');
			const userId = ctx.get('user').id;
			await ctx.get('stripeService').redeemGiftCode(userId, code);
			return ctx.body(null, 204);
		},
	);

	app.get(
		'/users/@me/gifts',
		RateLimitMiddleware(RateLimitConfigs.GIFTS_LIST),
		LoginRequired,
		DefaultUserOnly,
		async (ctx) => {
			const userId = ctx.get('user').id;
			const gifts = await ctx.get('stripeService').getUserGifts(userId);
			const responses = await Promise.all(
				gifts.map((gift) =>
					mapGiftCodeToMetadataResponse({
						giftCode: gift,
						userCacheService: ctx.get('userCacheService'),
						requestCache: ctx.get('requestCache'),
					}),
				),
			);
			return ctx.json(responses);
		},
	);

	app.get('/premium/visionary/slots', RateLimitMiddleware(RateLimitConfigs.STRIPE_VISIONARY_SLOTS), async (ctx) => {
		const slots = await ctx.get('stripeService').getVisionarySlots();
		return ctx.json(slots);
	});

	app.get('/premium/price-ids', RateLimitMiddleware(RateLimitConfigs.STRIPE_PRICE_IDS), async (ctx) => {
		const countryCode = ctx.req.query('country_code');
		const priceIds = await ctx.get('stripeService').getPriceIds(countryCode);
		return ctx.json(priceIds);
	});

	app.post(
		'/premium/customer-portal',
		RateLimitMiddleware(RateLimitConfigs.STRIPE_CUSTOMER_PORTAL),
		LoginRequired,
		DefaultUserOnly,
		async (ctx) => {
			const userId = ctx.get('user').id;
			const url = await ctx.get('stripeService').createCustomerPortalSession(userId);
			return ctx.json({url});
		},
	);

	app.post(
		'/premium/cancel-subscription',
		RateLimitMiddleware(RateLimitConfigs.STRIPE_SUBSCRIPTION_CANCEL),
		LoginRequired,
		DefaultUserOnly,
		async (ctx) => {
			const userId = ctx.get('user').id;
			await ctx.get('stripeService').cancelSubscriptionAtPeriodEnd(userId);
			return ctx.body(null, 204);
		},
	);

	app.post(
		'/premium/reactivate-subscription',
		RateLimitMiddleware(RateLimitConfigs.STRIPE_SUBSCRIPTION_REACTIVATE),
		LoginRequired,
		DefaultUserOnly,
		async (ctx) => {
			const userId = ctx.get('user').id;
			await ctx.get('stripeService').reactivateSubscription(userId);
			return ctx.body(null, 204);
		},
	);

	app.post(
		'/premium/visionary/rejoin',
		RateLimitMiddleware(RateLimitConfigs.STRIPE_VISIONARY_REJOIN),
		LoginRequired,
		DefaultUserOnly,
		async (ctx) => {
			const userId = ctx.get('user').id;
			await ctx.get('stripeService').rejoinVisionariesGuild(userId);
			return ctx.body(null, 204);
		},
	);

	app.post(
		'/premium/operator/rejoin',
		RateLimitMiddleware(RateLimitConfigs.STRIPE_VISIONARY_REJOIN),
		LoginRequired,
		DefaultUserOnly,
		async (ctx) => {
			const userId = ctx.get('user').id;
			await ctx.get('stripeService').rejoinOperatorsGuild(userId);
			return ctx.body(null, 204);
		},
	);
};
