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
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

describe('GET /donations/manage', () => {
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
	async function createValidMagicLinkToken(email: string, token: string): Promise<void> {
		const expiresAt = new Date(Date.now() + 15 * 60 * 1000);
		const tokenModel = new DonorMagicLinkToken({
			token_: token,
			donor_email: email,
			expires_at: expiresAt,
			used_at: null,
		});
		await donationRepository.createMagicLinkToken(tokenModel);
	}
	async function createExpiredMagicLinkToken(email: string, token: string): Promise<void> {
		const expiresAt = new Date(Date.now() - 1000);
		const tokenModel = new DonorMagicLinkToken({
			token_: token,
			donor_email: email,
			expires_at: expiresAt,
			used_at: null,
		});
		await donationRepository.createMagicLinkToken(tokenModel);
	}
	async function createUsedMagicLinkToken(email: string, token: string): Promise<void> {
		const expiresAt = new Date(Date.now() + 15 * 60 * 1000);
		const usedAt = new Date(Date.now() - 5000);
		const tokenModel = new DonorMagicLinkToken({
			token_: token,
			donor_email: email,
			expires_at: expiresAt,
			used_at: usedAt,
		});
		await donationRepository.createMagicLinkToken(tokenModel);
	}
	describe('valid token with customer ID', () => {
		test('redirects to Stripe billing portal', async () => {
			const customerId = 'cus_test_valid_123';
			await createDonorWithCustomerId(TEST_DONOR_EMAIL, customerId);
			await createValidMagicLinkToken(TEST_DONOR_EMAIL, TEST_MAGIC_LINK_TOKEN);
			const {response} = await createDonationManageBuilder(harness, TEST_MAGIC_LINK_TOKEN).executeRaw();
			expect(response.status).toBe(302);
			expect(response.headers.get('location')).toContain('https://billing.stripe.com');
			expect(stripeHandlers.spies.createdPortalSessions).toHaveLength(1);
			expect(stripeHandlers.spies.createdPortalSessions[0]?.customer).toBe(customerId);
		});
		test('marks token as used after validation', async () => {
			const customerId = 'cus_test_valid_456';
			await createDonorWithCustomerId(TEST_DONOR_EMAIL, customerId);
			await createValidMagicLinkToken(TEST_DONOR_EMAIL, TEST_MAGIC_LINK_TOKEN);
			await createDonationManageBuilder(harness, TEST_MAGIC_LINK_TOKEN).executeRaw();
			const token = await donationRepository.findMagicLinkToken(TEST_MAGIC_LINK_TOKEN);
			expect(token?.isUsed()).toBe(true);
		});
		test('includes return_url in portal session', async () => {
			const customerId = 'cus_test_return_url';
			await createDonorWithCustomerId(TEST_DONOR_EMAIL, customerId);
			await createValidMagicLinkToken(TEST_DONOR_EMAIL, TEST_MAGIC_LINK_TOKEN);
			await createDonationManageBuilder(harness, TEST_MAGIC_LINK_TOKEN).executeRaw();
			expect(stripeHandlers.spies.createdPortalSessions).toHaveLength(1);
			const portalSession = stripeHandlers.spies.createdPortalSessions[0];
			expect(portalSession?.return_url).toBeDefined();
		});
	});
	describe('valid token without customer ID', () => {
		test('redirects to donation page when donor has no customer ID', async () => {
			await donationRepository.createDonor({
				email: TEST_DONOR_EMAIL,
				stripeCustomerId: null,
				businessName: null,
				taxId: null,
				taxIdType: null,
				stripeSubscriptionId: null,
				subscriptionAmountCents: null,
				subscriptionCurrency: null,
				subscriptionInterval: null,
				subscriptionCurrentPeriodEnd: null,
			});
			await createValidMagicLinkToken(TEST_DONOR_EMAIL, TEST_MAGIC_LINK_TOKEN);
			const {response} = await createDonationManageBuilder(harness, TEST_MAGIC_LINK_TOKEN).executeRaw();
			expect(response.status).toBe(302);
			const location = response.headers.get('location');
			expect(location).toContain(Config.endpoints.marketing);
			expect(location).toContain('/donate');
			expect(stripeHandlers.spies.createdPortalSessions).toHaveLength(0);
		});
		test('redirects to donation page when donor does not exist', async () => {
			await createValidMagicLinkToken(TEST_DONOR_EMAIL, TEST_MAGIC_LINK_TOKEN);
			const {response} = await createDonationManageBuilder(harness, TEST_MAGIC_LINK_TOKEN).executeRaw();
			expect(response.status).toBe(302);
			const location = response.headers.get('location');
			expect(location).toContain(Config.endpoints.marketing);
			expect(location).toContain('/donate');
			expect(stripeHandlers.spies.createdPortalSessions).toHaveLength(0);
		});
		test('marks token as used even when redirecting to donate page', async () => {
			await createValidMagicLinkToken(TEST_DONOR_EMAIL, TEST_MAGIC_LINK_TOKEN);
			await createDonationManageBuilder(harness, TEST_MAGIC_LINK_TOKEN).executeRaw();
			const token = await donationRepository.findMagicLinkToken(TEST_MAGIC_LINK_TOKEN);
			expect(token?.isUsed()).toBe(true);
		});
	});
	describe('expired token handling', () => {
		test('rejects expired token', async () => {
			const customerId = 'cus_test_expired';
			await createDonorWithCustomerId(TEST_DONOR_EMAIL, customerId);
			await createExpiredMagicLinkToken(TEST_DONOR_EMAIL, TEST_MAGIC_LINK_TOKEN);
			await createDonationManageBuilder(harness, TEST_MAGIC_LINK_TOKEN)
				.expect(400, APIErrorCodes.DONATION_MAGIC_LINK_EXPIRED)
				.execute();
			expect(stripeHandlers.spies.createdPortalSessions).toHaveLength(0);
		});
		test('does not mark expired token as used', async () => {
			await createDonorWithCustomerId(TEST_DONOR_EMAIL, 'cus_test');
			await createExpiredMagicLinkToken(TEST_DONOR_EMAIL, TEST_MAGIC_LINK_TOKEN);
			await createDonationManageBuilder(harness, TEST_MAGIC_LINK_TOKEN)
				.expect(400, APIErrorCodes.DONATION_MAGIC_LINK_EXPIRED)
				.execute();
			const token = await donationRepository.findMagicLinkToken(TEST_MAGIC_LINK_TOKEN);
			expect(token?.isUsed()).toBe(false);
		});
		test('rejects token expired by exactly 1 millisecond', async () => {
			const expiresAt = new Date(Date.now() - 1);
			await createDonorWithCustomerId(TEST_DONOR_EMAIL, 'cus_test');
			const tokenModel = new DonorMagicLinkToken({
				token_: TEST_MAGIC_LINK_TOKEN,
				donor_email: TEST_DONOR_EMAIL,
				expires_at: expiresAt,
				used_at: null,
			});
			await donationRepository.createMagicLinkToken(tokenModel);
			await createDonationManageBuilder(harness, TEST_MAGIC_LINK_TOKEN)
				.expect(400, APIErrorCodes.DONATION_MAGIC_LINK_EXPIRED)
				.execute();
		});
	});
	describe('used token handling', () => {
		test('rejects already-used token', async () => {
			const customerId = 'cus_test_used';
			await createDonorWithCustomerId(TEST_DONOR_EMAIL, customerId);
			await createUsedMagicLinkToken(TEST_DONOR_EMAIL, TEST_MAGIC_LINK_TOKEN);
			await createDonationManageBuilder(harness, TEST_MAGIC_LINK_TOKEN)
				.expect(400, APIErrorCodes.DONATION_MAGIC_LINK_USED)
				.execute();
			expect(stripeHandlers.spies.createdPortalSessions).toHaveLength(0);
		});
		test('prevents reuse of token after successful redemption', async () => {
			const customerId = 'cus_test_reuse';
			await createDonorWithCustomerId(TEST_DONOR_EMAIL, customerId);
			await createValidMagicLinkToken(TEST_DONOR_EMAIL, TEST_MAGIC_LINK_TOKEN);
			await createDonationManageBuilder(harness, TEST_MAGIC_LINK_TOKEN).executeRaw();
			await createDonationManageBuilder(harness, TEST_MAGIC_LINK_TOKEN)
				.expect(400, APIErrorCodes.DONATION_MAGIC_LINK_USED)
				.execute();
			expect(stripeHandlers.spies.createdPortalSessions).toHaveLength(1);
		});
	});
	describe('invalid token handling', () => {
		test('rejects non-existent token', async () => {
			await createDonationManageBuilder(harness, TEST_MAGIC_LINK_TOKEN)
				.expect(400, APIErrorCodes.DONATION_MAGIC_LINK_INVALID)
				.execute();
			expect(stripeHandlers.spies.createdPortalSessions).toHaveLength(0);
		});
		test('rejects token with invalid format', async () => {
			await createDonationManageBuilder(harness, TEST_INVALID_TOKEN).expect(400).execute();
			expect(stripeHandlers.spies.createdPortalSessions).toHaveLength(0);
		});
		test('rejects empty token', async () => {
			await createDonationManageBuilder(harness, '').expect(400).execute();
			expect(stripeHandlers.spies.createdPortalSessions).toHaveLength(0);
		});
		test('rejects token that is too short', async () => {
			const shortToken = 'a'.repeat(63);
			await createDonationManageBuilder(harness, shortToken).expect(400).execute();
			expect(stripeHandlers.spies.createdPortalSessions).toHaveLength(0);
		});
		test('rejects token that is too long', async () => {
			const longToken = 'a'.repeat(65);
			await createDonationManageBuilder(harness, longToken).expect(400).execute();
			expect(stripeHandlers.spies.createdPortalSessions).toHaveLength(0);
		});
		test('rejects token with non-hex characters', async () => {
			const invalidToken = 'g'.repeat(64);
			await createDonationManageBuilder(harness, invalidToken).expect(400).execute();
			expect(stripeHandlers.spies.createdPortalSessions).toHaveLength(0);
		});
		test('rejects token with special characters', async () => {
			const specialToken = `${'a'.repeat(62)}@!`;
			await createDonationManageBuilder(harness, specialToken).expect(400).execute();
			expect(stripeHandlers.spies.createdPortalSessions).toHaveLength(0);
		});
		test('rejects token with whitespace', async () => {
			const whitespaceToken = `${'a'.repeat(32)} ${'a'.repeat(31)}`;
			await createDonationManageBuilder(harness, whitespaceToken).expect(400).execute();
			expect(stripeHandlers.spies.createdPortalSessions).toHaveLength(0);
		});
	});
	describe('missing token parameter', () => {
		test('rejects request without token query parameter', async () => {
			const response = await harness.requestJson({
				path: '/donations/manage',
				method: 'GET',
			});
			expect(response.status).toBe(400);
			expect(stripeHandlers.spies.createdPortalSessions).toHaveLength(0);
		});
	});
	describe('Stripe portal session creation', () => {
		test('handles Stripe API failure gracefully', async () => {
			const customerId = 'cus_test_stripe_fail';
			await createDonorWithCustomerId(TEST_DONOR_EMAIL, customerId);
			await createValidMagicLinkToken(TEST_DONOR_EMAIL, TEST_MAGIC_LINK_TOKEN);
			server.resetHandlers();
			const failingHandlers = createStripeApiHandlers({portalShouldFail: true});
			server.use(createPwnedPasswordsRangeHandler(), ...failingHandlers.handlers);
			const {response} = await createDonationManageBuilder(harness, TEST_MAGIC_LINK_TOKEN).executeRaw();
			expect(response.status).toBe(400);
		});
		test('creates portal session with correct customer ID', async () => {
			const customerId = 'cus_test_specific_123';
			await createDonorWithCustomerId(TEST_DONOR_EMAIL, customerId);
			await createValidMagicLinkToken(TEST_DONOR_EMAIL, TEST_MAGIC_LINK_TOKEN);
			await createDonationManageBuilder(harness, TEST_MAGIC_LINK_TOKEN).executeRaw();
			expect(stripeHandlers.spies.createdPortalSessions).toHaveLength(1);
			const session = stripeHandlers.spies.createdPortalSessions[0];
			expect(session?.customer).toBe(customerId);
		});
	});
	describe('edge cases', () => {
		test('handles token for donor with cancelled subscription', async () => {
			await donationRepository.createDonor({
				email: TEST_DONOR_EMAIL,
				stripeCustomerId: 'cus_test_cancelled',
				businessName: null,
				taxId: null,
				taxIdType: null,
				stripeSubscriptionId: null,
				subscriptionAmountCents: null,
				subscriptionCurrency: null,
				subscriptionInterval: null,
				subscriptionCurrentPeriodEnd: null,
			});
			await createValidMagicLinkToken(TEST_DONOR_EMAIL, TEST_MAGIC_LINK_TOKEN);
			const {response} = await createDonationManageBuilder(harness, TEST_MAGIC_LINK_TOKEN).executeRaw();
			expect(response.status).toBe(302);
			expect(response.headers.get('location')).toContain('https://billing.stripe.com');
			expect(stripeHandlers.spies.createdPortalSessions).toHaveLength(1);
		});
		test('handles token expiring in exactly 0 milliseconds', async () => {
			const expiresAt = new Date(Date.now() - 1);
			await createDonorWithCustomerId(TEST_DONOR_EMAIL, 'cus_test');
			const tokenModel = new DonorMagicLinkToken({
				token_: TEST_MAGIC_LINK_TOKEN,
				donor_email: TEST_DONOR_EMAIL,
				expires_at: expiresAt,
				used_at: null,
			});
			await donationRepository.createMagicLinkToken(tokenModel);
			await createDonationManageBuilder(harness, TEST_MAGIC_LINK_TOKEN)
				.expect(400, APIErrorCodes.DONATION_MAGIC_LINK_EXPIRED)
				.execute();
		});
		test('handles case-sensitive token comparison', async () => {
			const lowerToken = 'a'.repeat(64);
			const upperToken = 'A'.repeat(64);
			await createDonorWithCustomerId(TEST_DONOR_EMAIL, 'cus_test');
			await createValidMagicLinkToken(TEST_DONOR_EMAIL, lowerToken);
			await createDonationManageBuilder(harness, upperToken)
				.expect(400, APIErrorCodes.DONATION_MAGIC_LINK_INVALID)
				.execute();
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
			const {response: response1} = await createDonationManageBuilder(harness, token1).executeRaw();
			expect(response1.status).toBe(302);
			expect(stripeHandlers.spies.createdPortalSessions[0]?.customer).toBe(customerId1);
			const {response: response2} = await createDonationManageBuilder(harness, token2).executeRaw();
			expect(response2.status).toBe(302);
			expect(stripeHandlers.spies.createdPortalSessions[1]?.customer).toBe(customerId2);
			expect(stripeHandlers.spies.createdPortalSessions).toHaveLength(2);
		});
	});
	describe('token validation order', () => {
		test('checks token existence before expiration', async () => {
			await createDonationManageBuilder(harness, TEST_MAGIC_LINK_TOKEN)
				.expect(400, APIErrorCodes.DONATION_MAGIC_LINK_INVALID)
				.execute();
		});
		test('checks expiration before usage status', async () => {
			const expiresAt = new Date(Date.now() - 1000);
			const usedAt = new Date(Date.now() - 5000);
			await createDonorWithCustomerId(TEST_DONOR_EMAIL, 'cus_test');
			const tokenModel = new DonorMagicLinkToken({
				token_: TEST_MAGIC_LINK_TOKEN,
				donor_email: TEST_DONOR_EMAIL,
				expires_at: expiresAt,
				used_at: usedAt,
			});
			await donationRepository.createMagicLinkToken(tokenModel);
			await createDonationManageBuilder(harness, TEST_MAGIC_LINK_TOKEN)
				.expect(400, APIErrorCodes.DONATION_MAGIC_LINK_EXPIRED)
				.execute();
		});
	});
});
