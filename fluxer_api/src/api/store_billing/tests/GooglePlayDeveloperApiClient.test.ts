// SPDX-License-Identifier: AGPL-3.0-or-later

import {GooglePlayAccessTokenProvider} from '@app/api/store_billing/google_play/GooglePlayAccessTokenProvider';
import {GooglePlayApiError, isGooglePlayApiError} from '@app/api/store_billing/google_play/GooglePlayApiError';
import {
	GOOGLE_PLAY_API_BASE_URL,
	GooglePlayDeveloperApiClient,
} from '@app/api/store_billing/google_play/GooglePlayDeveloperApiClient';
import {
	buildGooglePlayProductSnapshot,
	buildGooglePlaySubscriptionSnapshot,
} from '@app/api/store_billing/google_play/GooglePlayPurchaseSync';
import {
	buildFakeProductPurchase,
	buildFakeSubscriptionPurchase,
	createGooglePlayDeveloperApiHandlers,
	type FakeGooglePlayDeveloperApi,
} from '@app/api/test/msw/handlers/GooglePlayDeveloperApiHandlers';
import {server} from '@app/api/test/msw/server';
import {HttpResponse, http} from 'msw';
import {beforeEach, describe, expect, it} from 'vitest';

const PACKAGE = 'com.fluxer';
const TOKEN = 'opaque.purchase-token/with+chars==';

describe('GooglePlayDeveloperApiClient', () => {
	let fake: FakeGooglePlayDeveloperApi;
	let client: GooglePlayDeveloperApiClient;

	beforeEach(() => {
		fake = createGooglePlayDeveloperApiHandlers();
		server.use(...fake.handlers);
		client = new GooglePlayDeveloperApiClient({accessTokenProvider: new GooglePlayAccessTokenProvider()});
	});

	async function captureError(promise: Promise<unknown>): Promise<GooglePlayApiError> {
		const error = await promise.catch((caught: unknown) => caught);
		expect(error).toBeInstanceOf(GooglePlayApiError);
		return error as GooglePlayApiError;
	}

	it('reads a subscription with a bearer token and an encoded purchase token', async () => {
		fake.setSubscription(PACKAGE, TOKEN, buildFakeSubscriptionPurchase({linkedPurchaseToken: 'older-token'}));
		const purchase = await client.getSubscription(PACKAGE, TOKEN);
		expect(purchase.subscriptionState).toBe('SUBSCRIPTION_STATE_ACTIVE');
		expect(purchase.linkedPurchaseToken).toBe('older-token');
		expect(purchase.lineItems?.[0]?.offerDetails?.basePlanId).toBe('monthly');
		expect(fake.spies.apiRequests).toEqual([
			{operation: 'subscriptions.get', method: 'GET', packageName: PACKAGE, authorization: 'Bearer ya29.fake-1'},
		]);
	});

	it('keeps unknown enum values and drops unknown fields', async () => {
		server.use(
			http.get(`${GOOGLE_PLAY_API_BASE_URL}/${PACKAGE}/purchases/subscriptionsv2/tokens/:token`, () =>
				HttpResponse.json({
					subscriptionState: 'SUBSCRIPTION_STATE_SOMETHING_NEW',
					acknowledgementState: 'ACKNOWLEDGEMENT_STATE_FUTURE',
					futureField: {nested: true},
					lineItems: [{productId: 'plutonium', offerPhase: {basePrice: {}}, offerDetails: {basePlanId: 'monthly'}}],
				}),
			),
		);
		const purchase = await client.getSubscription(PACKAGE, TOKEN);
		expect(purchase.subscriptionState).toBe('SUBSCRIPTION_STATE_SOMETHING_NEW');
		expect(purchase.acknowledgementState).toBe('ACKNOWLEDGEMENT_STATE_FUTURE');
		expect(purchase).not.toHaveProperty('futureField');
	});

	it('treats a malformed response as retryable', async () => {
		server.use(
			http.get(`${GOOGLE_PLAY_API_BASE_URL}/${PACKAGE}/purchases/subscriptionsv2/tokens/:token`, () =>
				HttpResponse.json({lineItems: 'nope'}),
			),
		);
		const error = await captureError(client.getSubscription(PACKAGE, TOKEN));
		expect(error.kind).toBe('retryable');
		expect(error.reason).toBe('malformed_response');
	});

	it('classifies an unknown token as invalid', async () => {
		const error = await captureError(client.getSubscription(PACKAGE, 'missing'));
		expect(error).toMatchObject({kind: 'invalid_token', status: 404, reason: 'purchaseTokenNotFound'});
	});

	it('classifies a long expired token as invalid', async () => {
		fake.setSubscription(PACKAGE, TOKEN, buildFakeSubscriptionPurchase());
		fake.markTokenGone(TOKEN);
		const error = await captureError(client.getSubscription(PACKAGE, TOKEN));
		expect(error).toMatchObject({kind: 'invalid_token', status: 410, reason: 'purchaseTokenNoLongerValid'});
	});

	it('classifies a malformed token as invalid', async () => {
		fake.failNext('subscriptions.get', {status: 400, reason: 'invalid'});
		const error = await captureError(client.getSubscription(PACKAGE, TOKEN));
		expect(error).toMatchObject({kind: 'invalid_token', status: 400});
		expect(isGooglePlayApiError(error, 'invalid_token')).toBe(true);
		expect(isGooglePlayApiError(error, 'retryable')).toBe(false);
	});

	it('classifies a token owned by another package as invalid', async () => {
		fake.setSubscription('com.other', TOKEN, buildFakeSubscriptionPurchase());
		const error = await captureError(client.getSubscription(PACKAGE, TOKEN));
		expect(error).toMatchObject({kind: 'invalid_token', status: 404});
	});

	it('classifies rate limits with Retry-After as retryable', async () => {
		fake.failNext('products.get', {status: 429, reason: 'rateLimitExceeded', retryAfter: '30'});
		const error = await captureError(client.getProduct(PACKAGE, TOKEN));
		expect(error).toMatchObject({kind: 'retryable', status: 429, reason: 'rateLimitExceeded', retryAfterMs: 30_000});
	});

	it('classifies server errors as retryable', async () => {
		fake.failNext('subscriptions.get', {status: 503});
		const error = await captureError(client.getSubscription(PACKAGE, TOKEN));
		expect(error).toMatchObject({kind: 'retryable', status: 503, retryAfterMs: null});
	});

	it('classifies a permission failure as auth', async () => {
		fake.failNext('subscriptions.get', {status: 403, reason: 'permissionDenied'});
		const error = await captureError(client.getSubscription(PACKAGE, TOKEN));
		expect(error).toMatchObject({kind: 'auth', status: 403, reason: 'permissionDenied'});
	});

	it('classifies other client errors as rejected', async () => {
		fake.failNext('orders.refund', {status: 409, reason: 'conflict'});
		const error = await captureError(client.refundOrder(PACKAGE, 'GPA.1'));
		expect(error).toMatchObject({kind: 'rejected', status: 409});
	});

	it('classifies network failures as retryable', async () => {
		server.use(
			http.get(`${GOOGLE_PLAY_API_BASE_URL}/${PACKAGE}/purchases/productsv2/tokens/:token`, () => HttpResponse.error()),
		);
		const error = await captureError(client.getProduct(PACKAGE, TOKEN));
		expect(error).toMatchObject({kind: 'retryable', status: null, reason: 'network_error'});
	});

	it('refreshes the access token once after a 401', async () => {
		fake.setSubscription(PACKAGE, TOKEN, buildFakeSubscriptionPurchase());
		await client.getSubscription(PACKAGE, TOKEN);
		fake.revokeAccessTokens();
		await client.getSubscription(PACKAGE, TOKEN);
		expect(fake.spies.apiRequests.map((request) => request.authorization)).toEqual([
			'Bearer ya29.fake-1',
			'Bearer ya29.fake-1',
			'Bearer ya29.fake-2',
		]);
	});

	it('gives up with an auth error when the refreshed token is also rejected', async () => {
		fake.failNext('subscriptions.get', {status: 401});
		fake.failNext('subscriptions.get', {status: 401});
		const error = await captureError(client.getSubscription(PACKAGE, TOKEN));
		expect(error).toMatchObject({kind: 'auth', status: 401});
		expect(fake.spies.apiRequests).toHaveLength(2);
	});

	it('never calls the API for a package that is not configured', async () => {
		const error = await captureError(client.getSubscription('com.attacker', TOKEN));
		expect(error).toMatchObject({kind: 'rejected', reason: 'package_not_configured'});
		expect(fake.spies.apiRequests).toHaveLength(0);
		expect(fake.spies.tokenAssertions).toHaveLength(0);
	});

	it('acknowledges a subscription with the account token and reports a repeat as invalid', async () => {
		fake.setSubscription(PACKAGE, TOKEN, buildFakeSubscriptionPurchase());
		await client.acknowledgeSubscription(PACKAGE, 'plutonium', TOKEN, {
			obfuscatedAccountId: '0f1e2d3c-4b5a-4968-8776-655443322110',
		});
		expect(fake.spies.acknowledged).toEqual([
			{
				packageName: PACKAGE,
				productId: 'plutonium',
				purchaseToken: TOKEN,
				body: {externalAccountIds: {obfuscatedAccountId: '0f1e2d3c-4b5a-4968-8776-655443322110'}},
			},
		]);
		expect(fake.getSubscription(TOKEN)?.acknowledgementState).toBe('ACKNOWLEDGEMENT_STATE_ACKNOWLEDGED');
		const repeat = await captureError(client.acknowledgeSubscription(PACKAGE, 'plutonium', TOKEN));
		expect(repeat).toMatchObject({kind: 'invalid_token', status: 400});
		const reread = await client.getSubscription(PACKAGE, TOKEN);
		expect(reread.acknowledgementState).toBe('ACKNOWLEDGEMENT_STATE_ACKNOWLEDGED');
	});

	it('acknowledges without a body when no account token is given', async () => {
		fake.setSubscription(PACKAGE, TOKEN, buildFakeSubscriptionPurchase());
		await client.acknowledgeSubscription(PACKAGE, 'plutonium', TOKEN);
		expect(fake.spies.acknowledged[0]?.body).toEqual({});
	});

	it('cancels a subscription to stop payments', async () => {
		fake.setSubscription(PACKAGE, TOKEN, buildFakeSubscriptionPurchase());
		await client.cancelSubscription(PACKAGE, TOKEN, 'DEVELOPER_REQUESTED_STOP_PAYMENTS');
		expect(fake.spies.canceled[0]?.body).toEqual({
			cancellationContext: {cancellationType: 'DEVELOPER_REQUESTED_STOP_PAYMENTS'},
		});
		const purchase = await client.getSubscription(PACKAGE, TOKEN);
		expect(purchase.subscriptionState).toBe('SUBSCRIPTION_STATE_CANCELED');
		expect(purchase.lineItems?.[0]?.autoRenewingPlan?.autoRenewEnabled).toBe(false);
	});

	it('revokes a subscription with a full refund', async () => {
		fake.setSubscription(PACKAGE, TOKEN, buildFakeSubscriptionPurchase());
		await client.revokeSubscription(PACKAGE, TOKEN);
		expect(fake.spies.revoked[0]?.body).toEqual({revocationContext: {fullRefund: {}}});
		expect((await client.getSubscription(PACKAGE, TOKEN)).subscriptionState).toBe('SUBSCRIPTION_STATE_EXPIRED');
	});

	it('reads and consumes a one-time product', async () => {
		fake.setProduct(PACKAGE, TOKEN, buildFakeProductPurchase());
		const before = await client.getProduct(PACKAGE, TOKEN);
		expect(before.purchaseStateContext?.purchaseState).toBe('PURCHASED');
		await client.consumeProduct(PACKAGE, 'gift_1_month', TOKEN);
		expect(fake.spies.consumed).toEqual([{packageName: PACKAGE, productId: 'gift_1_month', purchaseToken: TOKEN}]);
		const after = await client.getProduct(PACKAGE, TOKEN);
		expect(after.productLineItem?.[0]?.productOfferDetails?.consumptionState).toBe('CONSUMPTION_STATE_CONSUMED');
		expect(after.acknowledgementState).toBe('ACKNOWLEDGEMENT_STATE_ACKNOWLEDGED');
		const repeat = await captureError(client.consumeProduct(PACKAGE, 'gift_1_month', TOKEN));
		expect(repeat).toMatchObject({kind: 'invalid_token', status: 400});
	});

	it('refunds an order with revoke and lists it as voided', async () => {
		const start = new Date(Date.now() - 1000);
		fake.setSubscription(PACKAGE, TOKEN, buildFakeSubscriptionPurchase());
		await client.refundOrder(PACKAGE, 'GPA.1111-2222-3333-44444');
		expect(fake.spies.refunded).toEqual([{packageName: PACKAGE, orderId: 'GPA.1111-2222-3333-44444', revoke: true}]);
		expect((await client.getSubscription(PACKAGE, TOKEN)).subscriptionState).toBe('SUBSCRIPTION_STATE_EXPIRED');
		const page = await client.listVoidedPurchases(PACKAGE, {startTime: start});
		expect(page.nextPageToken).toBeNull();
		expect(page.voidedPurchases).toEqual([
			expect.objectContaining({purchaseToken: TOKEN, orderId: 'GPA.1111-2222-3333-44444', voidedSource: 1}),
		]);
		expect(fake.spies.voidedQueries[0]).toEqual({type: '1', startTime: String(start.getTime())});
	});

	it('pages through voided purchases', async () => {
		const now = Date.now();
		for (let index = 0; index < 3; index++) {
			fake.addVoidedPurchase(PACKAGE, {
				purchaseToken: `voided-${index}`,
				orderId: `GPA.${index}`,
				voidedTimeMillis: String(now - index * 1000),
				voidedReason: 1,
			});
		}
		fake.addVoidedPurchase('com.other', {purchaseToken: 'other', voidedTimeMillis: String(now)});
		const startTime = new Date(now - 60_000);
		const endTime = new Date(now + 60_000);
		const first = await client.listVoidedPurchases(PACKAGE, {startTime, endTime, maxResults: 2});
		expect(first.voidedPurchases.map((entry) => entry.purchaseToken)).toEqual(['voided-0', 'voided-1']);
		expect(first.nextPageToken).not.toBeNull();
		const second = await client.listVoidedPurchases(PACKAGE, {
			startTime,
			endTime,
			maxResults: 2,
			pageToken: first.nextPageToken ?? undefined,
		});
		expect(second.voidedPurchases.map((entry) => entry.purchaseToken)).toEqual(['voided-2']);
		expect(second.nextPageToken).toBeNull();
		expect(fake.spies.voidedQueries[1]).toMatchObject({
			type: '1',
			endTime: String(endTime.getTime()),
			maxResults: '2',
			token: first.nextPageToken,
		});
	});

	it('feeds the purchase sync end to end', async () => {
		fake.setSubscription(PACKAGE, TOKEN, buildFakeSubscriptionPurchase());
		fake.setProduct(PACKAGE, 'gift-token', buildFakeProductPurchase());
		const now = new Date();
		const subscription = buildGooglePlaySubscriptionSnapshot({
			packageName: PACKAGE,
			purchaseToken: TOKEN,
			purchase: await client.getSubscription(PACKAGE, TOKEN),
			now,
		});
		expect(subscription).toMatchObject({state: 'active', providerEntitled: true, slot: 'monthly', acknowledged: false});
		const gift = buildGooglePlayProductSnapshot({
			packageName: PACKAGE,
			purchaseToken: 'gift-token',
			purchase: await client.getProduct(PACKAGE, 'gift-token'),
			now,
		});
		expect(gift).toMatchObject({state: 'purchased', providerEntitled: true, slot: 'gift_1_month'});
	});
});
