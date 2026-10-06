// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {DonationRepository} from '@app/api/donation/DonationRepository';
import {DonorMagicLinkToken} from '@app/api/donation/models/DonorMagicLinkToken';
import {
	createDonationManageBuilder,
	TEST_DONOR_EMAIL,
	TEST_INVALID_TOKEN,
	TEST_MAGIC_LINK_TOKEN,
} from '@app/api/donation/tests/DonationTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createPwnedPasswordsRangeHandler} from '@app/api/test/msw/handlers/PwnedPasswordsHandlers';
import {createStripeApiHandlers, type StripeApiHandlers} from '@app/api/test/msw/handlers/StripeApiHandlers';
import {server} from '@app/api/test/msw/server';
import {createBuilderWithoutAuth, type TestRequestBuilder} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test, vi} from 'vitest';

type DonationManageAlert = 'link_expired' | 'link_used' | 'link_invalid' | 'no_customer' | 'portal_error';

const STRIPE_PORTAL_URL_PATTERN = /^https:\/\/billing\.stripe\.com\/p\/session\//;

describe('GET and POST /donations/manage', () => {
	let harness: ApiTestHarness;
	let stripeHandlers: StripeApiHandlers;
	let donationRepository: DonationRepository;
	beforeAll(async () => {
		harness = await createApiTestHarness();
		donationRepository = new DonationRepository();
		stripeHandlers = createStripeApiHandlers();
	});
	afterAll(async () => {
		await harness.shutdown();
	});
	beforeEach(async () => {
		await harness.reset();
		stripeHandlers.reset();
		server.use(...stripeHandlers.handlers);
	});
	function createDonationRedeemBuilder(token: string): TestRequestBuilder<void> {
		return createBuilderWithoutAuth<void>(harness).post(`/donations/manage?token=${encodeURIComponent(token)}`);
	}
	async function openManageLink(token: string): Promise<string | null> {
		const {response} = await createDonationManageBuilder(harness, token).expect(302).executeWithResponse();
		return response.headers.get('location');
	}
	async function redeemManageLink(token: string): Promise<string | null> {
		const {response} = await createDonationRedeemBuilder(token).expect(302).executeWithResponse();
		return response.headers.get('location');
	}
	function confirmUrl(token: string): string {
		return `${Config.endpoints.marketing}/donate/manage/confirm?token=${encodeURIComponent(token)}`;
	}
	function alertUrl(alert: DonationManageAlert): string {
		return `${Config.endpoints.marketing}/donate/manage?alert=${alert}`;
	}
	async function isTokenUsed(token: string): Promise<boolean | undefined> {
		const tokenModel = await donationRepository.findMagicLinkToken(token);
		return tokenModel?.isUsed();
	}
	function useFailingPortalHandlers(): void {
		server.resetHandlers();
		const failingHandlers = createStripeApiHandlers({portalShouldFail: true});
		server.use(createPwnedPasswordsRangeHandler(), ...failingHandlers.handlers);
	}
	async function createDonorWithCustomerId(email: string, customerId: string): Promise<void> {
		await donationRepository.createDonor({
			email,
			stripeCustomerId: customerId,
			businessName: null,
			taxId: null,
			taxIdType: null,
			stripeSubscriptionId: 'sub_test_1',
			subscriptionAmountCents: 2500,
			subscriptionCurrency: 'usd',
			subscriptionInterval: 'month',
			subscriptionCurrentPeriodEnd: new Date(Date.now() + 30 * 24 * 60 * 60 * 1000),
		});
	}
	async function createDonorWithoutSubscription(email: string, customerId: string | null): Promise<void> {
		await donationRepository.createDonor({
			email,
			stripeCustomerId: customerId,
			businessName: null,
			taxId: null,
			taxIdType: null,
			stripeSubscriptionId: null,
			subscriptionAmountCents: null,
			subscriptionCurrency: null,
			subscriptionInterval: null,
			subscriptionCurrentPeriodEnd: null,
		});
	}
	async function createMagicLinkToken(email: string, token: string, expiresAt: Date, usedAt: Date | null) {
		const tokenModel = new DonorMagicLinkToken({
			token_: token,
			donor_email: email,
			expires_at: expiresAt,
			used_at: usedAt,
		});
		await donationRepository.createMagicLinkToken(tokenModel);
	}
	async function createValidMagicLinkToken(email: string, token: string): Promise<void> {
		await createMagicLinkToken(email, token, new Date(Date.now() + 15 * 60 * 1000), null);
	}
	async function createExpiredMagicLinkToken(email: string, token: string): Promise<void> {
		await createMagicLinkToken(email, token, new Date(Date.now() - 1000), null);
	}
	async function createUsedMagicLinkToken(email: string, token: string): Promise<void> {
		await createMagicLinkToken(email, token, new Date(Date.now() + 15 * 60 * 1000), new Date(Date.now() - 5000));
	}
	describe('GET with a valid token and customer ID', () => {
		test('redirects to the confirmation page', async () => {
			await createDonorWithCustomerId(TEST_DONOR_EMAIL, 'cus_test_valid_123');
			await createValidMagicLinkToken(TEST_DONOR_EMAIL, TEST_MAGIC_LINK_TOKEN);
			expect(await openManageLink(TEST_MAGIC_LINK_TOKEN)).toBe(confirmUrl(TEST_MAGIC_LINK_TOKEN));
		});
		test('does not open a Stripe billing portal session', async () => {
			await createDonorWithCustomerId(TEST_DONOR_EMAIL, 'cus_test_valid_123');
			await createValidMagicLinkToken(TEST_DONOR_EMAIL, TEST_MAGIC_LINK_TOKEN);
			await openManageLink(TEST_MAGIC_LINK_TOKEN);
			expect(stripeHandlers.spies.createdPortalSessions).toHaveLength(0);
		});
	});
	describe('link scanner prefetch', () => {
		test('GET does not mark token as used', async () => {
			await createDonorWithCustomerId(TEST_DONOR_EMAIL, 'cus_test_prefetch');
			await createValidMagicLinkToken(TEST_DONOR_EMAIL, TEST_MAGIC_LINK_TOKEN);
			await openManageLink(TEST_MAGIC_LINK_TOKEN);
			expect(await isTokenUsed(TEST_MAGIC_LINK_TOKEN)).toBe(false);
		});
		test('repeated GETs keep redirecting to the confirmation page', async () => {
			await createDonorWithCustomerId(TEST_DONOR_EMAIL, 'cus_test_prefetch');
			await createValidMagicLinkToken(TEST_DONOR_EMAIL, TEST_MAGIC_LINK_TOKEN);
			expect(await openManageLink(TEST_MAGIC_LINK_TOKEN)).toBe(confirmUrl(TEST_MAGIC_LINK_TOKEN));
			expect(await openManageLink(TEST_MAGIC_LINK_TOKEN)).toBe(confirmUrl(TEST_MAGIC_LINK_TOKEN));
			expect(await openManageLink(TEST_MAGIC_LINK_TOKEN)).toBe(confirmUrl(TEST_MAGIC_LINK_TOKEN));
			expect(await isTokenUsed(TEST_MAGIC_LINK_TOKEN)).toBe(false);
			expect(stripeHandlers.spies.createdPortalSessions).toHaveLength(0);
		});
		test('POST still redeems a token after repeated GETs', async () => {
			const customerId = 'cus_test_prefetch';
			await createDonorWithCustomerId(TEST_DONOR_EMAIL, customerId);
			await createValidMagicLinkToken(TEST_DONOR_EMAIL, TEST_MAGIC_LINK_TOKEN);
			await openManageLink(TEST_MAGIC_LINK_TOKEN);
			await openManageLink(TEST_MAGIC_LINK_TOKEN);
			expect(await redeemManageLink(TEST_MAGIC_LINK_TOKEN)).toMatch(STRIPE_PORTAL_URL_PATTERN);
			expect(stripeHandlers.spies.createdPortalSessions).toHaveLength(1);
			expect(stripeHandlers.spies.createdPortalSessions[0]?.customer).toBe(customerId);
			expect(await isTokenUsed(TEST_MAGIC_LINK_TOKEN)).toBe(true);
		});
	});
	describe('POST with a valid token and customer ID', () => {
		test('redirects to Stripe billing portal', async () => {
			const customerId = 'cus_test_valid_123';
			await createDonorWithCustomerId(TEST_DONOR_EMAIL, customerId);
			await createValidMagicLinkToken(TEST_DONOR_EMAIL, TEST_MAGIC_LINK_TOKEN);
			expect(await redeemManageLink(TEST_MAGIC_LINK_TOKEN)).toMatch(STRIPE_PORTAL_URL_PATTERN);
			expect(stripeHandlers.spies.createdPortalSessions).toHaveLength(1);
			expect(stripeHandlers.spies.createdPortalSessions[0]?.customer).toBe(customerId);
		});
		test('marks token as used after the portal session is created', async () => {
			await createDonorWithCustomerId(TEST_DONOR_EMAIL, 'cus_test_valid_456');
			await createValidMagicLinkToken(TEST_DONOR_EMAIL, TEST_MAGIC_LINK_TOKEN);
			await redeemManageLink(TEST_MAGIC_LINK_TOKEN);
			expect(stripeHandlers.spies.createdPortalSessions).toHaveLength(1);
			expect(await isTokenUsed(TEST_MAGIC_LINK_TOKEN)).toBe(true);
		});
		test('includes return_url in portal session', async () => {
			await createDonorWithCustomerId(TEST_DONOR_EMAIL, 'cus_test_return_url');
			await createValidMagicLinkToken(TEST_DONOR_EMAIL, TEST_MAGIC_LINK_TOKEN);
			await redeemManageLink(TEST_MAGIC_LINK_TOKEN);
			expect(stripeHandlers.spies.createdPortalSessions).toHaveLength(1);
			const portalSession = stripeHandlers.spies.createdPortalSessions[0];
			expect(portalSession?.return_url).toBe(`${Config.endpoints.marketing}/donate`);
		});
	});
	describe('valid token without customer ID', () => {
		test('redirects to no_customer alert when donor has no customer ID', async () => {
			await createDonorWithoutSubscription(TEST_DONOR_EMAIL, null);
			await createValidMagicLinkToken(TEST_DONOR_EMAIL, TEST_MAGIC_LINK_TOKEN);
			expect(await openManageLink(TEST_MAGIC_LINK_TOKEN)).toBe(alertUrl('no_customer'));
			expect(await redeemManageLink(TEST_MAGIC_LINK_TOKEN)).toBe(alertUrl('no_customer'));
			expect(stripeHandlers.spies.createdPortalSessions).toHaveLength(0);
		});
		test('redirects to no_customer alert when donor does not exist', async () => {
			await createValidMagicLinkToken(TEST_DONOR_EMAIL, TEST_MAGIC_LINK_TOKEN);
			expect(await openManageLink(TEST_MAGIC_LINK_TOKEN)).toBe(alertUrl('no_customer'));
			expect(await redeemManageLink(TEST_MAGIC_LINK_TOKEN)).toBe(alertUrl('no_customer'));
			expect(stripeHandlers.spies.createdPortalSessions).toHaveLength(0);
		});
		test('does not mark token as used when there is no customer', async () => {
			await createValidMagicLinkToken(TEST_DONOR_EMAIL, TEST_MAGIC_LINK_TOKEN);
			await openManageLink(TEST_MAGIC_LINK_TOKEN);
			await redeemManageLink(TEST_MAGIC_LINK_TOKEN);
			expect(await isTokenUsed(TEST_MAGIC_LINK_TOKEN)).toBe(false);
		});
	});
	describe('expired token handling', () => {
		test('rejects expired token', async () => {
			await createDonorWithCustomerId(TEST_DONOR_EMAIL, 'cus_test_expired');
			await createExpiredMagicLinkToken(TEST_DONOR_EMAIL, TEST_MAGIC_LINK_TOKEN);
			expect(await openManageLink(TEST_MAGIC_LINK_TOKEN)).toBe(alertUrl('link_expired'));
			expect(await redeemManageLink(TEST_MAGIC_LINK_TOKEN)).toBe(alertUrl('link_expired'));
			expect(stripeHandlers.spies.createdPortalSessions).toHaveLength(0);
		});
		test('does not mark expired token as used', async () => {
			await createDonorWithCustomerId(TEST_DONOR_EMAIL, 'cus_test');
			await createExpiredMagicLinkToken(TEST_DONOR_EMAIL, TEST_MAGIC_LINK_TOKEN);
			await openManageLink(TEST_MAGIC_LINK_TOKEN);
			await redeemManageLink(TEST_MAGIC_LINK_TOKEN);
			expect(await isTokenUsed(TEST_MAGIC_LINK_TOKEN)).toBe(false);
		});
		test('rejects token expired by exactly 1 millisecond', async () => {
			await createDonorWithCustomerId(TEST_DONOR_EMAIL, 'cus_test');
			await createMagicLinkToken(TEST_DONOR_EMAIL, TEST_MAGIC_LINK_TOKEN, new Date(Date.now() - 1), null);
			expect(await openManageLink(TEST_MAGIC_LINK_TOKEN)).toBe(alertUrl('link_expired'));
			expect(await redeemManageLink(TEST_MAGIC_LINK_TOKEN)).toBe(alertUrl('link_expired'));
			expect(stripeHandlers.spies.createdPortalSessions).toHaveLength(0);
		});
	});
	describe('used token handling', () => {
		test('rejects already-used token', async () => {
			await createDonorWithCustomerId(TEST_DONOR_EMAIL, 'cus_test_used');
			await createUsedMagicLinkToken(TEST_DONOR_EMAIL, TEST_MAGIC_LINK_TOKEN);
			expect(await openManageLink(TEST_MAGIC_LINK_TOKEN)).toBe(alertUrl('link_used'));
			expect(await redeemManageLink(TEST_MAGIC_LINK_TOKEN)).toBe(alertUrl('link_used'));
			expect(stripeHandlers.spies.createdPortalSessions).toHaveLength(0);
		});
		test('prevents reuse of token after successful redemption', async () => {
			await createDonorWithCustomerId(TEST_DONOR_EMAIL, 'cus_test_reuse');
			await createValidMagicLinkToken(TEST_DONOR_EMAIL, TEST_MAGIC_LINK_TOKEN);
			expect(await redeemManageLink(TEST_MAGIC_LINK_TOKEN)).toMatch(STRIPE_PORTAL_URL_PATTERN);
			expect(await redeemManageLink(TEST_MAGIC_LINK_TOKEN)).toBe(alertUrl('link_used'));
			expect(await openManageLink(TEST_MAGIC_LINK_TOKEN)).toBe(alertUrl('link_used'));
			expect(stripeHandlers.spies.createdPortalSessions).toHaveLength(1);
		});
	});
	describe('invalid token handling', () => {
		test('rejects non-existent token', async () => {
			expect(await openManageLink(TEST_MAGIC_LINK_TOKEN)).toBe(alertUrl('link_invalid'));
			expect(await redeemManageLink(TEST_MAGIC_LINK_TOKEN)).toBe(alertUrl('link_invalid'));
			expect(stripeHandlers.spies.createdPortalSessions).toHaveLength(0);
		});
		test('rejects token with invalid format', async () => {
			await createDonationManageBuilder(harness, TEST_INVALID_TOKEN)
				.expect(400, APIErrorCodes.INVALID_FORM_BODY)
				.execute();
			await createDonationRedeemBuilder(TEST_INVALID_TOKEN).expect(400, APIErrorCodes.INVALID_FORM_BODY).execute();
			expect(stripeHandlers.spies.createdPortalSessions).toHaveLength(0);
		});
		test('rejects empty token', async () => {
			await createDonationManageBuilder(harness, '').expect(400, APIErrorCodes.INVALID_FORM_BODY).execute();
			await createDonationRedeemBuilder('').expect(400, APIErrorCodes.INVALID_FORM_BODY).execute();
			expect(stripeHandlers.spies.createdPortalSessions).toHaveLength(0);
		});
		test('rejects token that is too short', async () => {
			const shortToken = 'a'.repeat(63);
			await createDonationManageBuilder(harness, shortToken).expect(400, APIErrorCodes.INVALID_FORM_BODY).execute();
			await createDonationRedeemBuilder(shortToken).expect(400, APIErrorCodes.INVALID_FORM_BODY).execute();
			expect(stripeHandlers.spies.createdPortalSessions).toHaveLength(0);
		});
		test('rejects token that is too long', async () => {
			const longToken = 'a'.repeat(65);
			await createDonationManageBuilder(harness, longToken).expect(400, APIErrorCodes.INVALID_FORM_BODY).execute();
			await createDonationRedeemBuilder(longToken).expect(400, APIErrorCodes.INVALID_FORM_BODY).execute();
			expect(stripeHandlers.spies.createdPortalSessions).toHaveLength(0);
		});
		test('rejects token with non-hex characters', async () => {
			const invalidToken = 'g'.repeat(64);
			expect(await openManageLink(invalidToken)).toBe(alertUrl('link_invalid'));
			expect(await redeemManageLink(invalidToken)).toBe(alertUrl('link_invalid'));
			expect(stripeHandlers.spies.createdPortalSessions).toHaveLength(0);
		});
		test('rejects token with special characters', async () => {
			const specialToken = `${'a'.repeat(62)}@!`;
			expect(await openManageLink(specialToken)).toBe(alertUrl('link_invalid'));
			expect(await redeemManageLink(specialToken)).toBe(alertUrl('link_invalid'));
			expect(stripeHandlers.spies.createdPortalSessions).toHaveLength(0);
		});
		test('rejects token with whitespace', async () => {
			const whitespaceToken = `${'a'.repeat(32)} ${'a'.repeat(31)}`;
			expect(await openManageLink(whitespaceToken)).toBe(alertUrl('link_invalid'));
			expect(await redeemManageLink(whitespaceToken)).toBe(alertUrl('link_invalid'));
			expect(stripeHandlers.spies.createdPortalSessions).toHaveLength(0);
		});
	});
	describe('missing token parameter', () => {
		test('rejects request without token query parameter', async () => {
			const getResponse = await harness.requestJson({
				path: '/donations/manage',
				method: 'GET',
			});
			expect(getResponse.status).toBe(400);
			const postResponse = await harness.requestJson({
				path: '/donations/manage',
				method: 'POST',
			});
			expect(postResponse.status).toBe(400);
			expect(stripeHandlers.spies.createdPortalSessions).toHaveLength(0);
		});
	});
	describe('Stripe portal session creation', () => {
		test('handles Stripe API failure gracefully', async () => {
			await createDonorWithCustomerId(TEST_DONOR_EMAIL, 'cus_test_stripe_fail');
			await createValidMagicLinkToken(TEST_DONOR_EMAIL, TEST_MAGIC_LINK_TOKEN);
			useFailingPortalHandlers();
			expect(await redeemManageLink(TEST_MAGIC_LINK_TOKEN)).toBe(alertUrl('portal_error'));
		});
		test('does not mark token as used when the portal session fails', async () => {
			await createDonorWithCustomerId(TEST_DONOR_EMAIL, 'cus_test_stripe_fail');
			await createValidMagicLinkToken(TEST_DONOR_EMAIL, TEST_MAGIC_LINK_TOKEN);
			useFailingPortalHandlers();
			await redeemManageLink(TEST_MAGIC_LINK_TOKEN);
			expect(await isTokenUsed(TEST_MAGIC_LINK_TOKEN)).toBe(false);
		});
		test('redeems the same token once Stripe recovers', async () => {
			const customerId = 'cus_test_stripe_retry';
			await createDonorWithCustomerId(TEST_DONOR_EMAIL, customerId);
			await createValidMagicLinkToken(TEST_DONOR_EMAIL, TEST_MAGIC_LINK_TOKEN);
			useFailingPortalHandlers();
			expect(await redeemManageLink(TEST_MAGIC_LINK_TOKEN)).toBe(alertUrl('portal_error'));
			server.resetHandlers();
			server.use(...stripeHandlers.handlers);
			expect(await openManageLink(TEST_MAGIC_LINK_TOKEN)).toBe(confirmUrl(TEST_MAGIC_LINK_TOKEN));
			expect(await redeemManageLink(TEST_MAGIC_LINK_TOKEN)).toMatch(STRIPE_PORTAL_URL_PATTERN);
			expect(stripeHandlers.spies.createdPortalSessions).toHaveLength(1);
			expect(stripeHandlers.spies.createdPortalSessions[0]?.customer).toBe(customerId);
			expect(await isTokenUsed(TEST_MAGIC_LINK_TOKEN)).toBe(true);
		});
		test('creates portal session with correct customer ID', async () => {
			const customerId = 'cus_test_specific_123';
			await createDonorWithCustomerId(TEST_DONOR_EMAIL, customerId);
			await createValidMagicLinkToken(TEST_DONOR_EMAIL, TEST_MAGIC_LINK_TOKEN);
			await redeemManageLink(TEST_MAGIC_LINK_TOKEN);
			expect(stripeHandlers.spies.createdPortalSessions).toHaveLength(1);
			const session = stripeHandlers.spies.createdPortalSessions[0];
			expect(session?.customer).toBe(customerId);
		});
	});
	describe('edge cases', () => {
		afterEach(() => {
			vi.useRealTimers();
		});
		test('handles token for donor with cancelled subscription', async () => {
			const customerId = 'cus_test_cancelled';
			await createDonorWithoutSubscription(TEST_DONOR_EMAIL, customerId);
			await createValidMagicLinkToken(TEST_DONOR_EMAIL, TEST_MAGIC_LINK_TOKEN);
			expect(await openManageLink(TEST_MAGIC_LINK_TOKEN)).toBe(confirmUrl(TEST_MAGIC_LINK_TOKEN));
			expect(await redeemManageLink(TEST_MAGIC_LINK_TOKEN)).toMatch(STRIPE_PORTAL_URL_PATTERN);
			expect(stripeHandlers.spies.createdPortalSessions).toHaveLength(1);
			expect(stripeHandlers.spies.createdPortalSessions[0]?.customer).toBe(customerId);
		});
		test('handles token at its exact expiry instant', async () => {
			const expiresAt = new Date();
			vi.useFakeTimers({toFake: ['Date']});
			vi.setSystemTime(expiresAt);
			await createDonorWithCustomerId(TEST_DONOR_EMAIL, 'cus_test');
			await createMagicLinkToken(TEST_DONOR_EMAIL, TEST_MAGIC_LINK_TOKEN, expiresAt, null);
			expect(await openManageLink(TEST_MAGIC_LINK_TOKEN)).toBe(confirmUrl(TEST_MAGIC_LINK_TOKEN));
			vi.setSystemTime(expiresAt.getTime() + 1);
			expect(await openManageLink(TEST_MAGIC_LINK_TOKEN)).toBe(alertUrl('link_expired'));
			expect(await redeemManageLink(TEST_MAGIC_LINK_TOKEN)).toBe(alertUrl('link_expired'));
			expect(await isTokenUsed(TEST_MAGIC_LINK_TOKEN)).toBe(false);
			expect(stripeHandlers.spies.createdPortalSessions).toHaveLength(0);
		});
		test('handles case-sensitive token comparison', async () => {
			const lowerToken = 'a'.repeat(64);
			const upperToken = 'A'.repeat(64);
			await createDonorWithCustomerId(TEST_DONOR_EMAIL, 'cus_test');
			await createValidMagicLinkToken(TEST_DONOR_EMAIL, lowerToken);
			expect(await openManageLink(upperToken)).toBe(alertUrl('link_invalid'));
			expect(await redeemManageLink(upperToken)).toBe(alertUrl('link_invalid'));
			expect(await isTokenUsed(lowerToken)).toBe(false);
			expect(stripeHandlers.spies.createdPortalSessions).toHaveLength(0);
		});
		test('handles multiple valid tokens for different donors', async () => {
			const token1 = 'a'.repeat(64);
			const token2 = 'b'.repeat(64);
			const email1 = 'donor1@test.com';
			const email2 = 'donor2@test.com';
			const customerId1 = 'cus_test_1';
			const customerId2 = 'cus_test_2';
			await createDonorWithCustomerId(email1, customerId1);
			await createDonorWithCustomerId(email2, customerId2);
			await createValidMagicLinkToken(email1, token1);
			await createValidMagicLinkToken(email2, token2);
			expect(await openManageLink(token1)).toBe(confirmUrl(token1));
			expect(await openManageLink(token2)).toBe(confirmUrl(token2));
			expect(await redeemManageLink(token1)).toMatch(STRIPE_PORTAL_URL_PATTERN);
			expect(stripeHandlers.spies.createdPortalSessions[0]?.customer).toBe(customerId1);
			expect(await isTokenUsed(token1)).toBe(true);
			expect(await isTokenUsed(token2)).toBe(false);
			expect(await redeemManageLink(token2)).toMatch(STRIPE_PORTAL_URL_PATTERN);
			expect(stripeHandlers.spies.createdPortalSessions[1]?.customer).toBe(customerId2);
			expect(stripeHandlers.spies.createdPortalSessions).toHaveLength(2);
		});
	});
	describe('token validation order', () => {
		test('checks token existence before expiration', async () => {
			expect(await openManageLink(TEST_MAGIC_LINK_TOKEN)).toBe(alertUrl('link_invalid'));
			expect(await redeemManageLink(TEST_MAGIC_LINK_TOKEN)).toBe(alertUrl('link_invalid'));
		});
		test('checks expiration before usage status', async () => {
			await createDonorWithCustomerId(TEST_DONOR_EMAIL, 'cus_test');
			await createMagicLinkToken(
				TEST_DONOR_EMAIL,
				TEST_MAGIC_LINK_TOKEN,
				new Date(Date.now() - 1000),
				new Date(Date.now() - 5000),
			);
			expect(await openManageLink(TEST_MAGIC_LINK_TOKEN)).toBe(alertUrl('link_expired'));
			expect(await redeemManageLink(TEST_MAGIC_LINK_TOKEN)).toBe(alertUrl('link_expired'));
		});
		test('checks usage status before customer lookup', async () => {
			await createUsedMagicLinkToken(TEST_DONOR_EMAIL, TEST_MAGIC_LINK_TOKEN);
			expect(await openManageLink(TEST_MAGIC_LINK_TOKEN)).toBe(alertUrl('link_used'));
			expect(await redeemManageLink(TEST_MAGIC_LINK_TOKEN)).toBe(alertUrl('link_used'));
		});
	});
});
