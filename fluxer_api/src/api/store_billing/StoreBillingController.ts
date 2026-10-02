// SPDX-License-Identifier: AGPL-3.0-or-later

import {requireSudoMode} from '@app/api/auth/services/SudoVerificationService';
import {Config} from '@app/api/Config';
import {Logger} from '@app/api/Logger';
import {DefaultUserOnly, LoginRequired} from '@app/api/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {getGooglePlayPushVerifier} from '@app/api/middleware/ServiceSingletons';
import {SudoModeMiddleware} from '@app/api/middleware/SudoModeMiddleware';
import type {User} from '@app/api/models/User';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import {AppStoreJwsVerificationError, verifyNotification} from '@app/api/store_billing/app_store/AppStoreJwsVerifier';
import {parseGooglePlayPushEnvelope} from '@app/api/store_billing/google_play/GooglePlayPushVerifier';
import {
	isAppStoreConfigured,
	isGooglePlayConfigured,
	listAppStoreProducts,
	listGooglePlayProducts,
} from '@app/api/store_billing/StoreBillingConfig';
import {mapStorePurchaseToResponse} from '@app/api/store_billing/StoreBillingMappers';
import type {StoreEntitlementService} from '@app/api/store_billing/StoreEntitlementService';
import {isStripeSubscriptionActive} from '@app/api/store_billing/StoreEntitlementWriter';
import type {HonoApp, HonoEnv} from '@app/api/types/HonoEnv';
import {Validator} from '@app/api/Validator';
import {PremiumFlags, UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {AppNotFoundHandler} from '@fluxer/errors/src/domains/core/ErrorHandlers';
import {StoreBillingUnavailableError} from '@fluxer/errors/src/domains/payment/StoreBillingUnavailableError';
import {StoreNotificationUnauthorizedError} from '@fluxer/errors/src/domains/payment/StoreNotificationUnauthorizedError';
import {SudoVerificationSchema} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import {
	ClaimAppStoreTransactionRequest,
	ClaimGooglePlayPurchaseRequest,
	StoreBillingContextResponse,
	StorePurchaseClaimResponse,
	StorePurchaseIdParam,
	StorePurchaseListResponse,
} from '@fluxer/schema/src/domains/premium/StoreBillingSchemas';
import {createMiddleware} from 'hono/factory';
import {z} from 'zod';

function routeAvailableWhen(isAvailable: () => boolean) {
	return createMiddleware<HonoEnv>(async (ctx, next) => {
		if (Config.instance.selfHosted && !isAvailable()) {
			return AppNotFoundHandler(ctx);
		}
		return next();
	});
}

export const HostedOnlyRoute = routeAvailableWhen(() => false);

const AppStoreNotificationBody = z.object({signedPayload: z.string().min(1)});

async function buildStoreBillingContext(
	user: User,
	storeEntitlementService: StoreEntitlementService,
): Promise<StoreBillingContextResponse> {
	const appStoreEnabled = isAppStoreConfigured();
	const googlePlayEnabled = isGooglePlayConfigured();
	const [appAccountToken, storeEntitlement] = await Promise.all([
		storeEntitlementService.getOrCreateAccountToken(user.id),
		storeEntitlementService.getActiveStoreEntitlement(user.id),
	]);
	let purchaseBlockedReason: StoreBillingContextResponse['purchase_blocked_reason'] = null;
	let blockingProvider: StoreBillingContextResponse['blocking_provider'] = null;
	if (user.premiumType === UserPremiumTypes.LIFETIME) {
		purchaseBlockedReason = 'lifetime';
	} else if ((user.premiumFlags & PremiumFlags.PURCHASE_DISABLED) !== 0) {
		purchaseBlockedReason = 'purchase_disabled';
	} else if (storeEntitlement) {
		purchaseBlockedReason = 'existing_subscription';
		blockingProvider = storeEntitlement.provider;
	} else if (isStripeSubscriptionActive(user, new Date())) {
		purchaseBlockedReason = 'existing_subscription';
		blockingProvider = 'stripe';
	}
	return {
		app_account_token: appAccountToken,
		app_store: {
			enabled: appStoreEnabled,
			bundle_ids: appStoreEnabled ? Config.appStore.apps.map((app) => app.bundleId) : [],
			products: appStoreEnabled
				? listAppStoreProducts().map((product) => ({product_id: product.productId, slot: product.slot}))
				: [],
		},
		google_play: {
			enabled: googlePlayEnabled,
			package_names: googlePlayEnabled ? [...Config.googlePlay.packages] : [],
			products: googlePlayEnabled
				? listGooglePlayProducts().map((product) => ({
						product_id: product.productId,
						base_plan_id: product.basePlanId,
						slot: product.slot,
					}))
				: [],
		},
		purchase_blocked_reason: purchaseBlockedReason,
		blocking_provider: blockingProvider,
	};
}

export function StoreBillingController(app: HonoApp) {
	app.get(
		'/premium/store',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.STORE_BILLING_CONTEXT),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'get_store_billing_context',
			summary: 'Get in-app purchase context',
			description:
				'Returns the account token and the App Store and Google Play products the mobile apps may sell, plus the reason a new subscription is blocked, if any.',
			responseSchema: StoreBillingContextResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Premium',
		}),
		async (ctx) => {
			return ctx.json(await buildStoreBillingContext(ctx.get('user'), ctx.get('storeEntitlementService')));
		},
	);
	app.post(
		'/premium/store/app-store/transactions',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.STORE_BILLING_CLAIM_APP_STORE),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', ClaimAppStoreTransactionRequest),
		OpenAPI({
			operationId: 'claim_app_store_transaction',
			summary: 'Claim App Store transaction',
			description:
				'Verifies a StoreKit 2 signed transaction, links the purchase to the authenticated account and applies it. Calling it again for the same purchase returns the same result. Finish the transaction after a 200 or a 400 or 403 error. Leave it unfinished after a 429, a 5xx or a network failure.',
			responseSchema: StorePurchaseClaimResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Premium',
		}),
		async (ctx) => {
			const {signed_transaction} = ctx.req.valid('json');
			const result = await ctx
				.get('storeEntitlementService')
				.claimAppStoreTransaction(ctx.get('user').id, signed_transaction);
			return ctx.json({purchase: mapStorePurchaseToResponse(result.purchase), gift_code: result.giftCode});
		},
	);
	app.post(
		'/premium/store/google-play/purchases',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.STORE_BILLING_CLAIM_GOOGLE_PLAY),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', ClaimGooglePlayPurchaseRequest),
		OpenAPI({
			operationId: 'claim_google_play_purchase',
			summary: 'Claim Google Play purchase',
			description:
				'Verifies a Google Play purchase token, links the purchase to the authenticated account, acknowledges it and applies it. Calling it again for the same purchase returns the same result.',
			responseSchema: StorePurchaseClaimResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Premium',
		}),
		async (ctx) => {
			const body = ctx.req.valid('json');
			const result = await ctx.get('storeEntitlementService').claimGooglePlayPurchase(ctx.get('user').id, {
				purchaseToken: body.purchase_token,
				productId: body.product_id,
				packageName: body.package_name,
			});
			return ctx.json({purchase: mapStorePurchaseToResponse(result.purchase), gift_code: result.giftCode});
		},
	);
	app.get(
		'/premium/store/purchases',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.STORE_BILLING_PURCHASES_LIST),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'list_store_purchases',
			summary: 'List in-app purchases',
			description: 'Returns the App Store and Google Play purchases linked to the authenticated account.',
			responseSchema: StorePurchaseListResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Premium',
		}),
		async (ctx) => {
			const rows = await ctx.get('storeEntitlementService').listStorePurchases(ctx.get('user').id);
			return ctx.json(rows.map(mapStorePurchaseToResponse));
		},
	);
	app.delete(
		'/premium/store/purchases/:purchase_id',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.STORE_BILLING_PURCHASE_RELEASE),
		LoginRequired,
		DefaultUserOnly,
		SudoModeMiddleware,
		Validator('param', StorePurchaseIdParam),
		Validator('json', SudoVerificationSchema),
		OpenAPI({
			operationId: 'release_store_purchase',
			summary: 'Release in-app subscription',
			description:
				'Unlinks an App Store or Google Play subscription from the authenticated account so another account can claim it. Requires sudo mode.',
			requestSchema: SudoVerificationSchema,
			requestBodyRequired: false,
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Premium',
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const {purchase_id} = ctx.req.valid('param');
			await requireSudoMode(ctx, user, ctx.req.valid('json'));
			await ctx.get('storeEntitlementService').releaseStorePurchase(user.id, purchase_id);
			return ctx.body(null, 204);
		},
	);
	app.post(
		'/webhooks/app-store',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.STORE_BILLING_APP_STORE_WEBHOOK),
		async (ctx) => {
			if (!isAppStoreConfigured()) {
				throw new StoreBillingUnavailableError();
			}
			let signedPayload: string;
			try {
				signedPayload = AppStoreNotificationBody.parse(await ctx.req.json()).signedPayload;
			} catch {
				throw new StoreNotificationUnauthorizedError();
			}
			try {
				await verifyNotification(signedPayload);
			} catch (error) {
				if (error instanceof AppStoreJwsVerificationError) {
					Logger.warn({reason: error.reason}, 'Rejected App Store notification');
					throw new StoreNotificationUnauthorizedError();
				}
				throw error;
			}
			await ctx.get('workerService').addJob('processAppStoreNotification', {signedPayload});
			return ctx.body(null, 200);
		},
	);
	app.post(
		'/webhooks/google-play',
		HostedOnlyRoute,
		RateLimitMiddleware(RateLimitConfigs.STORE_BILLING_GOOGLE_PLAY_WEBHOOK),
		async (ctx) => {
			if (!isGooglePlayConfigured()) {
				throw new StoreBillingUnavailableError();
			}
			await getGooglePlayPushVerifier().verifyAuthorizationHeader(ctx.req.header('Authorization'));
			let body: unknown = null;
			try {
				body = await ctx.req.json();
			} catch {
				body = null;
			}
			const message = parseGooglePlayPushEnvelope(body);
			if (!message) {
				Logger.warn('Ignored a malformed Google Play push message');
				return ctx.body(null, 204);
			}
			await ctx
				.get('workerService')
				.addJob('processGooglePlayNotification', {messageId: message.messageId, data: message.data});
			return ctx.body(null, 204);
		},
	);
}
