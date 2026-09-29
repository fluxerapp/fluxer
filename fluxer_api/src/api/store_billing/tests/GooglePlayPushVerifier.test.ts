// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {
	decodeGooglePlayDeveloperNotification,
	GooglePlayPushVerifier,
	parseGooglePlayPushEnvelope,
} from '@app/api/store_billing/google_play/GooglePlayPushVerifier';
import {
	buildGooglePlayPushEnvelope,
	createGooglePlayDeveloperApiHandlers,
	encodeGooglePlayDeveloperNotification,
	type FakeGooglePlayDeveloperApi,
} from '@app/api/test/msw/handlers/GooglePlayDeveloperApiHandlers';
import {server} from '@app/api/test/msw/server';
import {StoreNotificationUnauthorizedError} from '@fluxer/errors/src/domains/payment/StoreNotificationUnauthorizedError';
import {SignJWT} from 'jose';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';

describe('GooglePlayPushVerifier', () => {
	let fake: FakeGooglePlayDeveloperApi;
	let verifier: GooglePlayPushVerifier;
	let savedGooglePlay: typeof Config.googlePlay;

	beforeEach(() => {
		savedGooglePlay = {...Config.googlePlay};
		fake = createGooglePlayDeveloperApiHandlers();
		server.use(...fake.handlers);
		verifier = new GooglePlayPushVerifier();
	});

	afterEach(() => {
		Object.assign(Config.googlePlay, savedGooglePlay);
	});

	async function expectRejected(header: string | null | undefined): Promise<void> {
		await expect(verifier.verifyAuthorizationHeader(header)).rejects.toBeInstanceOf(StoreNotificationUnauthorizedError);
	}

	it('accepts a token from the configured push service account', async () => {
		const token = await fake.signPushToken();
		await expect(verifier.verifyAuthorizationHeader(`Bearer ${token}`)).resolves.toEqual({
			email: 'rtdn@fluxer-test.iam.gserviceaccount.com',
			subject: '112233445566778899000',
		});
		expect(fake.spies.jwksFetches).toBe(1);
	});

	it('accepts the issuer without a scheme', async () => {
		const token = await fake.signPushToken({claims: {iss: 'accounts.google.com'}});
		await expect(verifier.verifyAuthorizationHeader(`Bearer ${token}`)).resolves.toMatchObject({
			email: 'rtdn@fluxer-test.iam.gserviceaccount.com',
		});
	});

	it('caches the key set across verifications', async () => {
		await verifier.verifyAuthorizationHeader(`Bearer ${await fake.signPushToken()}`);
		await verifier.verifyAuthorizationHeader(`Bearer ${await fake.signPushToken()}`);
		expect(fake.spies.jwksFetches).toBe(1);
	});

	it('uses an injected key set URL', async () => {
		const customFake = createGooglePlayDeveloperApiHandlers({jwksUrl: 'https://oidc.fluxer.test/certs'});
		server.use(...customFake.handlers);
		const customVerifier = new GooglePlayPushVerifier({jwksUrl: 'https://oidc.fluxer.test/certs'});
		const token = await customFake.signPushToken();
		await expect(customVerifier.verifyAuthorizationHeader(`Bearer ${token}`)).resolves.toMatchObject({
			email: 'rtdn@fluxer-test.iam.gserviceaccount.com',
		});
		expect(customFake.spies.jwksFetches).toBe(1);
		expect(fake.spies.jwksFetches).toBe(0);
	});

	it('rejects a missing or malformed authorization header', async () => {
		await expectRejected(undefined);
		await expectRejected(null);
		await expectRejected('');
		await expectRejected('Basic abc');
		await expectRejected('Bearer');
		await expectRejected(await fake.signPushToken());
	});

	it('rejects a token for another audience', async () => {
		await expectRejected(`Bearer ${await fake.signPushToken({claims: {aud: 'https://evil.test/webhooks'}})}`);
	});

	it('rejects a token from another service account', async () => {
		await expectRejected(
			`Bearer ${await fake.signPushToken({claims: {email: 'other@fluxer-test.iam.gserviceaccount.com'}})}`,
		);
	});

	it('rejects a token without an email', async () => {
		await expectRejected(`Bearer ${await fake.signPushToken({omitClaims: ['email']})}`);
	});

	it('rejects an unverified email', async () => {
		await expectRejected(`Bearer ${await fake.signPushToken({claims: {email_verified: false}})}`);
		await expectRejected(`Bearer ${await fake.signPushToken({claims: {email_verified: 'true'}})}`);
		await expectRejected(`Bearer ${await fake.signPushToken({omitClaims: ['email_verified']})}`);
	});

	it('rejects a token from another issuer', async () => {
		await expectRejected(`Bearer ${await fake.signPushToken({claims: {iss: 'https://accounts.evil.test'}})}`);
	});

	it('rejects an expired token', async () => {
		const nowSeconds = Math.floor(Date.now() / 1000);
		await expectRejected(
			`Bearer ${await fake.signPushToken({claims: {iat: nowSeconds - 7200, exp: nowSeconds - 3600}})}`,
		);
	});

	it('rejects a token without an expiry', async () => {
		await expectRejected(`Bearer ${await fake.signPushToken({omitClaims: ['exp']})}`);
	});

	it('rejects a token signed by an untrusted key', async () => {
		await expectRejected(`Bearer ${await fake.signPushToken({signer: 'untrusted'})}`);
	});

	it('rejects a token signed with an unknown key id', async () => {
		await expectRejected(`Bearer ${await fake.signPushToken({signer: 'untrusted', kid: 'unknown-key'})}`);
	});

	it('rejects a token signed with a shared secret', async () => {
		const nowSeconds = Math.floor(Date.now() / 1000);
		const token = await new SignJWT({
			email: Config.googlePlay.pushServiceAccountEmail,
			email_verified: true,
		})
			.setProtectedHeader({alg: 'HS256', kid: 'fake-google-oidc-key'})
			.setIssuer('https://accounts.google.com')
			.setAudience(Config.googlePlay.pushAudience ?? '')
			.setIssuedAt(nowSeconds)
			.setExpirationTime(nowSeconds + 600)
			.sign(new TextEncoder().encode('0123456789abcdef0123456789abcdef'));
		await expectRejected(`Bearer ${token}`);
	});

	it('rejects everything when the push audience is not configured', async () => {
		const token = await fake.signPushToken();
		Config.googlePlay.pushAudience = undefined;
		await expectRejected(`Bearer ${token}`);
	});

	it('rejects everything when the push service account is not configured', async () => {
		const token = await fake.signPushToken();
		Config.googlePlay.pushServiceAccountEmail = '';
		await expectRejected(`Bearer ${token}`);
	});

	it('rejects when the key set cannot be fetched', async () => {
		fake.failNext('jwks', {status: 500});
		await expectRejected(`Bearer ${await fake.signPushToken()}`);
	});
});

describe('parseGooglePlayPushEnvelope', () => {
	it('extracts the message id and data', () => {
		const envelope = buildGooglePlayPushEnvelope({version: '1.0', packageName: 'com.fluxer'}, '42');
		expect(parseGooglePlayPushEnvelope(envelope)).toEqual({
			messageId: '42',
			data: encodeGooglePlayDeveloperNotification({version: '1.0', packageName: 'com.fluxer'}),
			subscription: 'projects/fluxer-test/subscriptions/play-rtdn',
			publishTime: expect.any(String),
		});
	});

	it('falls back to the snake case message id', () => {
		expect(parseGooglePlayPushEnvelope({message: {data: 'e30=', message_id: '7'}})).toEqual({
			messageId: '7',
			data: 'e30=',
			subscription: null,
			publishTime: null,
		});
	});

	it('returns null for an envelope without data or id', () => {
		expect(parseGooglePlayPushEnvelope({message: {messageId: '1'}})).toBeNull();
		expect(parseGooglePlayPushEnvelope({message: {data: 'e30='}})).toBeNull();
		expect(parseGooglePlayPushEnvelope({message: {data: '', messageId: '1'}})).toBeNull();
		expect(parseGooglePlayPushEnvelope({})).toBeNull();
		expect(parseGooglePlayPushEnvelope(null)).toBeNull();
		expect(parseGooglePlayPushEnvelope('message')).toBeNull();
	});
});

describe('decodeGooglePlayDeveloperNotification', () => {
	function decode(notification: Record<string, unknown>) {
		return decodeGooglePlayDeveloperNotification(encodeGooglePlayDeveloperNotification(notification));
	}

	it('decodes a subscription notification', () => {
		expect(
			decode({
				version: '1.0',
				packageName: 'com.fluxer',
				eventTimeMillis: '1759147200000',
				subscriptionNotification: {version: '1.0', notificationType: 4, purchaseToken: 'sub-token'},
			}),
		).toEqual({
			packageName: 'com.fluxer',
			eventTime: new Date(1759147200000),
			event: {type: 'subscription', notificationType: 4, purchaseToken: 'sub-token'},
		});
	});

	it('keeps unknown subscription notification types', () => {
		expect(
			decode({
				packageName: 'com.fluxer',
				subscriptionNotification: {notificationType: 99, purchaseToken: 'sub-token', subscriptionId: 'plutonium'},
			})?.event,
		).toEqual({type: 'subscription', notificationType: 99, purchaseToken: 'sub-token'});
	});

	it('decodes a one-time product notification', () => {
		expect(
			decode({
				packageName: 'com.fluxer',
				eventTimeMillis: 1759147200000,
				oneTimeProductNotification: {version: '1.0', notificationType: 1, purchaseToken: 'otp', sku: 'gift_1_year'},
			}),
		).toEqual({
			packageName: 'com.fluxer',
			eventTime: new Date(1759147200000),
			event: {type: 'one_time_product', notificationType: 1, purchaseToken: 'otp', productId: 'gift_1_year'},
		});
	});

	it('decodes voided purchase notifications for both product types', () => {
		expect(
			decode({
				packageName: 'com.fluxer',
				voidedPurchaseNotification: {purchaseToken: 'v1', orderId: 'GPA.1', productType: 1, refundType: 1},
			})?.event,
		).toEqual({
			type: 'voided_purchase',
			purchaseToken: 'v1',
			orderId: 'GPA.1',
			productType: 'subscription',
			refundType: 'full',
		});
		expect(
			decode({
				packageName: 'com.fluxer',
				voidedPurchaseNotification: {purchaseToken: 'v2', productType: 2, refundType: 2},
			})?.event,
		).toEqual({
			type: 'voided_purchase',
			purchaseToken: 'v2',
			orderId: null,
			productType: 'one_time',
			refundType: 'quantity_based_partial',
		});
		expect(
			decode({packageName: 'com.fluxer', voidedPurchaseNotification: {purchaseToken: 'v3', productType: 9}})?.event,
		).toMatchObject({productType: null, refundType: null});
	});

	it('decodes pending refund review, test and unknown notifications', () => {
		expect(
			decode({
				packageName: 'com.fluxer',
				pendingRefundReviewNotification: {orderId: 'GPA.9', refundReason: 7, pendingRefundToken: 'x'},
			})?.event,
		).toEqual({type: 'pending_refund_review', orderId: 'GPA.9'});
		expect(decode({version: '1.0', packageName: 'com.fluxer', testNotification: {version: '1.0'}})).toEqual({
			packageName: 'com.fluxer',
			eventTime: null,
			event: {type: 'test'},
		});
		expect(decode({packageName: 'com.fluxer', somethingNewNotification: {}})?.event).toEqual({type: 'unknown'});
	});

	it('returns null for undecodable data', () => {
		expect(decodeGooglePlayDeveloperNotification('!!!')).toBeNull();
		expect(decodeGooglePlayDeveloperNotification(Buffer.from('not json').toString('base64'))).toBeNull();
		expect(decode({subscriptionNotification: {notificationType: 4, purchaseToken: 't'}})).toBeNull();
		expect(decode({packageName: 'com.fluxer', subscriptionNotification: {notificationType: 4}})).toBeNull();
		expect(decode({packageName: 'com.fluxer', eventTimeMillis: 'soon'})).toBeNull();
	});
});
