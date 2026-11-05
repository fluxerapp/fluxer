// SPDX-License-Identifier: AGPL-3.0-or-later

import {DefaultUserOnly, LoginRequired} from '@app/api/middleware/AuthMiddleware';
import {HostedOnlyRoute, routeAvailableWhen} from '@app/api/middleware/BillingRouteAvailability';
import {CaptchaMiddleware} from '@app/api/middleware/CaptchaMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import {
	getEffectiveBillingConfig,
	isBillingActive,
	isPremiumTieringActive,
	isStripeServiceable,
} from '@app/api/stripe/BillingConfigCache';
import {mapGiftCodeToMetadataResponse, mapGiftCodeToResponse} from '@app/api/stripe/StripeModel';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {assertTierAllows, NO_TIER_CONTEXT, tierContextFrom} from '@app/api/user/AccountTier';
import {lookupGeoip} from '@app/api/utils/IpUtils';
import {Validator} from '@app/api/Validator';
import {TierGatedActions} from '@fluxer/constants/src/AccountTierConstants';
import {StripeWebhookNotAvailableError} from '@fluxer/errors/src/domains/payment/StripeWebhookNotAvailableError';
import {StripeWebhookSignatureInvalidError} from '@fluxer/errors/src/domains/payment/StripeWebhookSignatureInvalidError';
import {StripeWebhookSignatureMissingError} from '@fluxer/errors/src/domains/payment/StripeWebhookSignatureMissingError';
import {GiftCodeParam, SuccessResponse} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import {
	CreateCheckoutSessionRequest,
	GiftCodeMetadataListResponse,
	GiftCodeResponse,
} from '@fluxer/schema/src/domains/premium/GiftCodeSchemas';
import {
	ChangeSubscriptionRequest,
	CurrentSubscriptionPriceResponse,
	PlutoniumWithdrawalIdParam,
	PlutoniumWithdrawalListResponse,
	PlutoniumWithdrawRequest,
	PriceIdsQueryRequest,
	PriceIdsResponse,
	SelfServeRefundEligibilityResponse,
	SelfServeRefundResponse,
	SwitchToListPriceResponse,
	UrlResponse,
	WebhookReceivedResponse,
} from '@fluxer/schema/src/domains/premium/PremiumSchemas';

async function getPurchaseGeoipCountryCode(request: Request): Promise<string | null> {
	const geoip = await lookupGeoip(request);
	return geoip.countryCode ?? null;
}

function isStripeWebhookAvailable(): boolean {
	const billing = getEffectiveBillingConfig();
	return billing.webhookSecret !== null && isStripeServiceable(billing);
}

const GiftRouteAvailable = routeAvailableWhen(isPremiumTieringActive);
const BillingRouteAvailable = routeAvailableWhen(() => isBillingActive());
const StripeServicingRouteAvailable = routeAvailableWhen(() => isStripeServiceable());
const StripeWebhookRouteAvailable = routeAvailableWhen(isStripeWebhookAvailable);

export function StripeController(app: HonoApp) {
	app.post(
		'/stripe/webhook',
		StripeWebhookRouteAvailable,
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
			const webhookSecret = getEffectiveBillingConfig().webhookSecret;
			if (!stripe || !webhookSecret) {
				throw new StripeWebhookNotAvailableError();
			}
			const body = await ctx.req.text();
			try {
				stripe.webhooks.constructEvent(body, signature, webhookSecret);
			} catch {
				throw new StripeWebhookSignatureInvalidError();
			}
			await ctx.get('workerService').addJob('processStripeWebhook', {body, signature});
			return ctx.json({received: true});
		},
	);
	app.post(
		'/stripe/checkout/subscription',
		BillingRouteAvailable,
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
				payment_method,
				is_business,
				cnpj,
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
				paymentMethod: payment_method,
				isBusiness: is_business,
				cnpj,
			});
			return ctx.json({url: checkoutUrl});
		},
	);
	app.post(
		'/stripe/checkout/gift',
		BillingRouteAvailable,
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
			const {price_id, country_code, client_geoip_country_code, eu_withdrawal_waiver_accepted, is_business, cnpj} =
				ctx.req.valid('json');
			const userId = ctx.get('user').id;
			const checkoutUrl = await ctx.get('stripeService').createCheckoutSession({
				userId,
				priceId: price_id,
				isGift: true,
				countryCode: country_code,
				clientGeoipCountryCode: client_geoip_country_code,
				purchaseGeoipCountryCode: await getPurchaseGeoipCountryCode(ctx.req.raw),
				euWithdrawalWaiverAccepted: eu_withdrawal_waiver_accepted,
				isBusiness: is_business,
				cnpj,
			});
			return ctx.json({url: checkoutUrl});
		},
	);
	app.get(
		'/gifts/:code',
		GiftRouteAvailable,
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
			const giftCode = await ctx.get('stripeService').gifts.getGiftCode(code);
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
		GiftRouteAvailable,
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
			assertTierAllows(ctx.get('user'), TierGatedActions.GIFT_REDEEM, NO_TIER_CONTEXT);
			await ctx.get('stripeService').gifts.redeemGiftCode(userId, code);
			return ctx.body(null, 204);
		},
	);
	app.get(
		'/users/@me/gifts',
		GiftRouteAvailable,
		RateLimitMiddleware(RateLimitConfigs.GIFTS_LIST),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'list_user_gifts',
			summary: 'List user gifts',
			description: 'Lists all gift codes created by the authenticated user.',
			responseSchema: GiftCodeMetadataListResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Users',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const gifts = await ctx.get('stripeService').gifts.getUserGifts(userId);
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
		BillingRouteAvailable,
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
			const {country_code} = ctx.req.valid('query');
			const geoipCountryCode = await getPurchaseGeoipCountryCode(ctx.req.raw);
			const priceIds = await ctx.get('stripeService').checkout.getPriceIds(geoipCountryCode ?? country_code);
			return ctx.json(priceIds);
		},
	);
	app.get(
		'/premium/current-subscription-price',
		StripeServicingRouteAvailable,
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
			const price = await ctx.get('stripeService').subscriptions.getCurrentSubscriptionPrice(userId);
			return ctx.json(price);
		},
	);
	app.post(
		'/premium/customer-portal',
		StripeServicingRouteAvailable,
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
			const url = await ctx.get('stripeService').checkout.createCustomerPortalSession(userId);
			return ctx.json({url});
		},
	);
	app.post(
		'/premium/grace/end',
		StripeServicingRouteAvailable,
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
			await ctx.get('stripeService').premium.endGracePeriod(userId);
			return ctx.json({success: true as const});
		},
	);
	app.get(
		'/premium/refund-eligibility',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.STRIPE_REFUND_ELIGIBILITY),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'get_self_serve_refund_eligibility',
			summary: 'Get self-serve refund eligibility',
			description:
				'Returns whether the authenticated user can self-serve refund their recent purchases, including the refund window and cooldown timestamps. The amount covers every payment made in the last 3 days.',
			responseSchema: SelfServeRefundEligibilityResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Billing',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const eligibility = await ctx.get('stripeService').refunds.getEligibility(userId);
			return ctx.json(eligibility);
		},
	);
	app.post(
		'/premium/refund-latest',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.STRIPE_REFUND_LATEST),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'self_serve_refund_latest_purchase',
			summary: 'Refund recent purchases',
			description:
				'Refunds every payment the authenticated user made in the last 3 days, when the most recent one is within that window and the user is not in a 30-day cooldown. A Plutonium upgrade paid shortly after the first payment is refunded together with it. If a refunded payment was tied to a subscription, the subscription is cancelled immediately. A Google Play purchase is refunded on its own. Fluxer issues a credit note for each refunded payment.',
			responseSchema: SelfServeRefundResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Billing',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const result = await ctx.get('stripeService').refunds.refundRecentPurchases(userId);
			return ctx.json(result);
		},
	);
	app.get(
		'/premium/withdrawals',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.STRIPE_REFUND_ELIGIBILITY),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'list_plutonium_withdrawals',
			summary: 'List Plutonium purchases open for withdrawal',
			description:
				'Lists the Plutonium subscriptions and gifts paid through Stripe that the authenticated user can withdraw from right now, with the amount a withdrawal refunds and when the withdrawal period ends. Consumers in the EU, the EEA and the UK get 14 days from the first payment of a subscription, a plan switch or a renewal at a new price. In the EU and the EEA the period is extended past weekends and Swedish public holidays, and in the UK it ends at midnight UK time. A purchase made with the immediate-start consent (checkout metadata withdrawal_pro_rata) refunds the payments less the share for the days of Plutonium used. An EU or EEA subscription bought without that consent also gets a new period on each renewal and a full refund, and a UK purchase without it is refunded in full. Brazilian consumers get 7 days from the first payment of a subscription or a plan switch, not from a renewal, ending at 23:59 UTC-5, with a full refund. A business purchase is not listed, and in Brazil only one made with a CNPJ.',
			responseSchema: PlutoniumWithdrawalListResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Billing',
		}),
		async (ctx) => {
			return ctx.json(await ctx.get('stripeService').refunds.getWithdrawals(ctx.get('user').id));
		},
	);
	app.post(
		'/premium/withdrawals/:purchase_id',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.STRIPE_REFUND_LATEST),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'withdraw_plutonium_purchase',
			summary: 'Withdraw from a Plutonium purchase',
			description:
				'Withdraws from a Plutonium subscription or gift paid through Stripe while its withdrawal period lasts, as the withdrawals list describes. Refunds the payments made since the withdrawal period started to the original payment method: less the share for the days of Plutonium used for a purchase made with the immediate-start consent (the amount quoted at quoted_at when it is at most one day old), and in full otherwise, including every Brazilian withdrawal. Ends a subscription immediately and emails a receipt with the time the withdrawal was received. The 3-day self-serve refund window and its 30-day cooldown do not apply. Fluxer issues a credit note for each refunded payment.',
			responseSchema: SelfServeRefundResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Billing',
		}),
		Validator('param', PlutoniumWithdrawalIdParam),
		Validator('json', PlutoniumWithdrawRequest),
		async (ctx) => {
			const {name, quoted_at} = ctx.req.valid('json');
			const result = await ctx.get('stripeService').refunds.withdraw({
				userId: ctx.get('user').id,
				purchaseId: ctx.req.valid('param').purchase_id,
				name,
				receivedAt: new Date(),
				quotedAt: quoted_at ? new Date(quoted_at) : null,
			});
			return ctx.json(result);
		},
	);
	app.post(
		'/premium/cancel-subscription',
		StripeServicingRouteAvailable,
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
			await ctx.get('stripeService').subscriptions.cancelSubscriptionAtPeriodEnd(userId, 'customer_request');
			return ctx.body(null, 204);
		},
	);
	app.post(
		'/premium/reactivate-subscription',
		StripeServicingRouteAvailable,
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
			await ctx.get('stripeService').subscriptions.reactivateSubscription(userId);
			return ctx.body(null, 204);
		},
	);
	app.post(
		'/premium/change-subscription',
		StripeServicingRouteAvailable,
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
			await ctx.get('stripeService').subscriptions.changeBillingCycle(userId, billing_cycle, effective_at);
			return ctx.body(null, 204);
		},
	);
	app.post(
		'/premium/switch-to-list-price',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.STRIPE_SUBSCRIPTION_CHANGE),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'switch_subscription_to_list_price',
			summary: 'Switch subscription to the current list price',
			description:
				"Moves the authenticated user's grandfathered premium subscription down to the current list price for the same currency and billing cycle, effective at the end of the current billing period. The target price is resolved on the server and the switch is refused unless it lowers the amount charged.",
			responseSchema: SwitchToListPriceResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Premium',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const result = await ctx.get('stripeService').subscriptions.switchToCurrentListPrice(userId);
			return ctx.json(result);
		},
	);
	app.post(
		'/premium/cancel-pending-subscription-change',
		StripeServicingRouteAvailable,
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
			await ctx.get('stripeService').subscriptions.cancelPendingSubscriptionChange(userId);
			return ctx.body(null, 204);
		},
	);
	app.post(
		'/premium/visionary/rejoin',
		HostedOnlyRoute,
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
			await ctx.get('stripeService').premium.rejoinVisionariesGuild(userId, tierContextFrom(ctx));
			return ctx.body(null, 204);
		},
	);
}
