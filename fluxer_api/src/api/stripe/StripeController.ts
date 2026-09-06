// SPDX-License-Identifier: AGPL-3.0-or-later

import {StripeWebhookNotAvailableError} from '@fluxer/errors/src/domains/payment/StripeWebhookNotAvailableError';
import {StripeWebhookSignatureInvalidError} from '@fluxer/errors/src/domains/payment/StripeWebhookSignatureInvalidError';
import {StripeWebhookSignatureMissingError} from '@fluxer/errors/src/domains/payment/StripeWebhookSignatureMissingError';
import {GiftCodeParam, SuccessResponse} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import {
	CreateCheckoutSessionRequest,
	GiftCodeMetadataResponse,
	GiftCodeResponse,
} from '@fluxer/schema/src/domains/premium/GiftCodeSchemas';
import {
	ChangeSubscriptionRequest,
	CurrentSubscriptionPriceResponse,
	LocalizedCardPreapprovalContinueRequest,
	LocalizedCardPreapprovalContinueResponse,
	PriceIdsQueryRequest,
	PriceIdsResponse,
	SelfServeRefundEligibilityResponse,
	SelfServeRefundResponse,
	UrlResponse,
	WebhookReceivedResponse,
} from '@fluxer/schema/src/domains/premium/PremiumSchemas';
import {z} from 'zod';
import {Config} from '../Config';
import {DefaultUserOnly, LoginRequired} from '../middleware/AuthMiddleware';
import {CaptchaMiddleware} from '../middleware/CaptchaMiddleware';
import {RateLimitMiddleware} from '../middleware/RateLimitMiddleware';
import {OpenAPI} from '../middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '../RateLimitConfig';
import type {HonoApp} from '../types/HonoEnv';
import {lookupGeoip} from '../utils/IpUtils';
import {Validator} from '../Validator';
import {mapGiftCodeToMetadataResponse, mapGiftCodeToResponse} from './StripeModel';

async function getPurchaseGeoipCountryCode(request: Request): Promise<string | null> {
	const geoip = await lookupGeoip(request);
	return geoip.countryCode ?? null;
}

export function StripeController(app: HonoApp) {
	app.post(
		'/stripe/webhook',
		RateLimitMiddleware(RateLimitConfigs.STRIPE_WEBHOOK),
		OpenAPI({
			operationId: 'process_stripe_webhook',
			summary: 'Process Stripe webhook',
			description: 'Handles incoming Stripe webhook events for payment processing and subscription management.',
			responseSchema: WebhookReceivedResponse,
			statusCode: 200,
			security: [],
			tags: 'Billing',
		}),
		async (ctx) => {
			const signature = ctx.req.header('stripe-signature');
			if (!signature) {
				throw new StripeWebhookSignatureMissingError();
			}
			const stripe = ctx.get('stripeService').getStripe();
			if (!stripe || !Config.stripe.webhookSecret) {
				throw new StripeWebhookNotAvailableError();
			}
			const body = await ctx.req.text();
			try {
				stripe.webhooks.constructEvent(body, signature, Config.stripe.webhookSecret);
			} catch {
				throw new StripeWebhookSignatureInvalidError();
			}
			await ctx.get('workerService').addJob('processStripeWebhook', {body, signature});
			return ctx.json({received: true});
		},
	);
	app.post(
		'/stripe/checkout/subscription',
		RateLimitMiddleware(RateLimitConfigs.STRIPE_CHECKOUT_SUBSCRIPTION),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'create_checkout_session',
			summary: 'Create checkout session',
			description: 'Initiates a Stripe checkout session for user subscription purchases.',
			responseSchema: UrlResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Billing',
		}),
		Validator('json', CreateCheckoutSessionRequest),
		async (ctx) => {
			const {
				price_id,
				country_code,
				client_geoip_country_code,
				eu_withdrawal_waiver_accepted,
				pricing_mode,
				payment_method,
				is_business,
			} = ctx.req.valid('json');
			const userId = ctx.get('user').id;
			const checkoutUrl = await ctx.get('stripeService').createCheckoutSession({
				userId,
				priceId: price_id,
				isGift: false,
				countryCode: country_code,
				clientGeoipCountryCode: client_geoip_country_code,
				purchaseGeoipCountryCode: await getPurchaseGeoipCountryCode(ctx.req.raw),
				euWithdrawalWaiverAccepted: eu_withdrawal_waiver_accepted,
				pricingMode: pricing_mode,
				paymentMethod: payment_method,
				isBusiness: is_business,
			});
			return ctx.json({url: checkoutUrl});
		},
	);
	app.post(
		'/stripe/checkout/subscription/preapproval',
		RateLimitMiddleware(RateLimitConfigs.STRIPE_CHECKOUT_SUBSCRIPTION_PREAPPROVAL),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'create_localized_card_preapproval_session',
			summary: 'Create localized card preapproval session',
			description:
				'Initiates a Stripe Checkout setup-mode session to preapprove a local card before continuing to paid localized checkout.',
			responseSchema: UrlResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Billing',
		}),
		Validator('json', CreateCheckoutSessionRequest),
		async (ctx) => {
			const {
				price_id,
				country_code,
				client_geoip_country_code,
				eu_withdrawal_waiver_accepted,
				pricing_mode,
				is_business,
			} = ctx.req.valid('json');
			const userId = ctx.get('user').id;
			const checkoutUrl = await ctx.get('stripeService').createLocalizedCardPreapprovalSession({
				userId,
				priceId: price_id,
				countryCode: country_code,
				clientGeoipCountryCode: client_geoip_country_code,
				purchaseGeoipCountryCode: await getPurchaseGeoipCountryCode(ctx.req.raw),
				euWithdrawalWaiverAccepted: eu_withdrawal_waiver_accepted,
				pricingMode: pricing_mode,
				isBusiness: is_business,
			});
			return ctx.json({url: checkoutUrl});
		},
	);
	app.post(
		'/stripe/checkout/subscription/preapproval/continue',
		RateLimitMiddleware(RateLimitConfigs.STRIPE_CHECKOUT_SUBSCRIPTION_PREAPPROVAL_CONTINUE),
		OpenAPI({
			operationId: 'continue_localized_card_preapproval_session',
			summary: 'Continue localized card preapproval session',
			description:
				'Checks the status of a localized card preapproval flow and returns the paid Stripe Checkout URL when it is ready.',
			responseSchema: LocalizedCardPreapprovalContinueResponse,
			statusCode: 200,
			security: [],
			tags: 'Billing',
		}),
		Validator('json', LocalizedCardPreapprovalContinueRequest),
		async (ctx) => {
			const {token} = ctx.req.valid('json');
			const result = await ctx.get('stripeService').continueLocalizedCardPreapproval(token);
			return ctx.json(result);
		},
	);
	app.post(
		'/stripe/checkout/gift',
		RateLimitMiddleware(RateLimitConfigs.STRIPE_CHECKOUT_GIFT),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'create_gift_checkout_session',
			summary: 'Create gift checkout session',
			description: 'Creates a checkout session for purchasing premium gifts to send to other users.',
			responseSchema: UrlResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Billing',
		}),
		Validator('json', CreateCheckoutSessionRequest),
		async (ctx) => {
			const {
				price_id,
				country_code,
				client_geoip_country_code,
				eu_withdrawal_waiver_accepted,
				pricing_mode,
				is_business,
			} = ctx.req.valid('json');
			const userId = ctx.get('user').id;
			const checkoutUrl = await ctx.get('stripeService').createCheckoutSession({
				userId,
				priceId: price_id,
				isGift: true,
				countryCode: country_code,
				clientGeoipCountryCode: client_geoip_country_code,
				purchaseGeoipCountryCode: await getPurchaseGeoipCountryCode(ctx.req.raw),
				euWithdrawalWaiverAccepted: eu_withdrawal_waiver_accepted,
				pricingMode: pricing_mode,
				isBusiness: is_business,
			});
			return ctx.json({url: checkoutUrl});
		},
	);
	app.get(
		'/gifts/:code',
		RateLimitMiddleware(RateLimitConfigs.GIFT_CODE_GET),
		OpenAPI({
			operationId: 'get_gift_code',
			summary: 'Get gift code',
			description: 'Retrieves information about a gift code, including sender details and premium entitlements.',
			responseSchema: GiftCodeResponse,
			statusCode: 200,
			security: [],
			tags: 'Gifts',
		}),
		Validator('param', GiftCodeParam),
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
		CaptchaMiddleware,
		OpenAPI({
			operationId: 'redeem_gift_code',
			summary: 'Redeem gift code',
			description: 'Redeems a gift code for the authenticated user, applying premium benefits.',
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Gifts',
		}),
		Validator('param', GiftCodeParam),
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
		OpenAPI({
			operationId: 'list_user_gifts',
			summary: 'List user gifts',
			description: 'Lists all gift codes created by the authenticated user.',
			responseSchema: z.array(GiftCodeMetadataResponse),
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Users',
		}),
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
	app.get(
		'/premium/price-ids',
		RateLimitMiddleware(RateLimitConfigs.STRIPE_PRICE_IDS),
		Validator('query', PriceIdsQueryRequest),
		OpenAPI({
			operationId: 'get_price_ids',
			summary: 'Get Stripe price IDs',
			description: 'Retrieves Stripe price IDs for premium subscriptions based on geographic location.',
			responseSchema: PriceIdsResponse,
			statusCode: 200,
			security: [],
			tags: 'Premium',
		}),
		async (ctx) => {
			const {country_code, pricing_mode} = ctx.req.valid('query');
			const priceIds = await ctx.get('stripeService').getPriceIds(country_code, pricing_mode);
			return ctx.json(priceIds);
		},
	);
	app.get(
		'/premium/current-subscription-price',
		RateLimitMiddleware(RateLimitConfigs.STRIPE_CURRENT_SUBSCRIPTION_PRICE),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'get_current_subscription_price',
			summary: 'Get current subscription price',
			description:
				'Returns the exact price the authenticated user is being billed for their active Stripe subscription, including whether they are on a grandfathered legacy rate.',
			responseSchema: CurrentSubscriptionPriceResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Premium',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const price = await ctx.get('stripeService').getCurrentSubscriptionPrice(userId);
			return ctx.json(price);
		},
	);
	app.post(
		'/premium/customer-portal',
		RateLimitMiddleware(RateLimitConfigs.STRIPE_CUSTOMER_PORTAL),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'create_customer_portal',
			summary: 'Create customer portal',
			description:
				'Creates a session URL for the authenticated user to manage their Stripe subscription via the customer portal.',
			responseSchema: UrlResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Premium',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const url = await ctx.get('stripeService').createCustomerPortalSession(userId);
			return ctx.json({url});
		},
	);
	app.post(
		'/premium/grace/end',
		RateLimitMiddleware(RateLimitConfigs.STRIPE_PREMIUM_GRACE_END),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'end_premium_grace_period',
			summary: 'End premium grace period now',
			description:
				'Ends the post-cancel grace period immediately, downgrading the user from premium and clearing premium_since. Idempotent and safe to call when not in grace; returns success in either case. Use this when the user explicitly opts out of the 3-day recovery window.',
			responseSchema: SuccessResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Premium',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			await ctx.get('stripeService').endPremiumGracePeriod(userId);
			return ctx.json({success: true as const});
		},
	);
	app.post(
		'/users/@me/age-verification',
		RateLimitMiddleware(RateLimitConfigs.AGE_VERIFICATION),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'create_age_verification_session',
			summary: 'Create age verification session',
			description:
				'Creates a Stripe checkout session in setup mode to verify the user holds a credit card for UK adult age verification.',
			responseSchema: UrlResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Billing',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const url = await ctx.get('ageVerificationService').createVerificationSession(userId);
			return ctx.json({url});
		},
	);
	app.get(
		'/premium/refund-eligibility',
		RateLimitMiddleware(RateLimitConfigs.STRIPE_REFUND_ELIGIBILITY),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'get_self_serve_refund_eligibility',
			summary: 'Get self-serve refund eligibility',
			description:
				'Returns whether the authenticated user can self-serve refund their most recent purchase, including the refund window and cooldown timestamps.',
			responseSchema: SelfServeRefundEligibilityResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Billing',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const eligibility = await ctx.get('stripeService').getSelfServeRefundEligibility(userId);
			return ctx.json(eligibility);
		},
	);
	app.post(
		'/premium/refund-latest',
		RateLimitMiddleware(RateLimitConfigs.STRIPE_REFUND_LATEST),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'self_serve_refund_latest_purchase',
			summary: 'Refund latest purchase',
			description:
				"Refunds the authenticated user's most recent paid invoice if it is within 3 days and the user is not in a 30-day cooldown. If the invoice was tied to a subscription, the subscription is cancelled immediately.",
			responseSchema: SelfServeRefundResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Billing',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const result = await ctx.get('stripeService').refundLatestPurchase(userId);
			return ctx.json(result);
		},
	);
	app.post(
		'/premium/cancel-subscription',
		RateLimitMiddleware(RateLimitConfigs.STRIPE_SUBSCRIPTION_CANCEL),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'cancel_subscription',
			summary: 'Cancel subscription',
			description: "Cancels the authenticated user's premium subscription at the end of the current billing period.",
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Premium',
		}),
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
		OpenAPI({
			operationId: 'reactivate_subscription',
			summary: 'Reactivate subscription',
			description: 'Reactivates a previously cancelled premium subscription for the authenticated user.',
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Premium',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			await ctx.get('stripeService').reactivateSubscription(userId);
			return ctx.body(null, 204);
		},
	);
	app.post(
		'/premium/change-subscription',
		RateLimitMiddleware(RateLimitConfigs.STRIPE_SUBSCRIPTION_CHANGE),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'change_subscription_billing_cycle',
			summary: 'Change subscription billing cycle',
			description:
				'Switches the authenticated user between monthly and yearly billing for their active premium subscription.',
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Premium',
		}),
		Validator('json', ChangeSubscriptionRequest),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const {billing_cycle, effective_at} = ctx.req.valid('json');
			await ctx.get('stripeService').changeSubscriptionBillingCycle(userId, billing_cycle, effective_at);
			return ctx.body(null, 204);
		},
	);
	app.post(
		'/premium/cancel-pending-subscription-change',
		RateLimitMiddleware(RateLimitConfigs.STRIPE_SUBSCRIPTION_CHANGE),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'cancel_pending_subscription_change',
			summary: 'Cancel pending subscription billing cycle change',
			description:
				"Cancels the authenticated user's pending premium billing cycle change without cancelling the active subscription.",
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Premium',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			await ctx.get('stripeService').cancelPendingSubscriptionChange(userId);
			return ctx.body(null, 204);
		},
	);
	app.post(
		'/premium/visionary/rejoin',
		RateLimitMiddleware(RateLimitConfigs.STRIPE_VISIONARY_REJOIN),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'rejoin_visionary_guild',
			summary: 'Rejoin visionary guild',
			description: 'Adds the authenticated user back to the visionary community guild after premium re-subscription.',
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Premium',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			await ctx.get('stripeService').rejoinVisionariesGuild(userId);
			return ctx.body(null, 204);
		},
	);
}
