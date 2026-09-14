// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {DonationRepository} from '@app/api/donation/DonationRepository';
import {
	clearDonationTestEmails,
	createDonationRequestLinkBuilder,
	createUniqueEmail,
	listDonationTestEmails,
	TEST_DONOR_EMAIL,
} from '@app/api/donation/tests/DonationTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

describe('POST /donations/request-link', () => {
	let harness: ApiTestHarness;
	let donationRepository: DonationRepository;
	beforeAll(async () => {
		harness = await createApiTestHarness();
		donationRepository = new DonationRepository();
	});
	afterAll(async () => {
		await harness.shutdown();
	});
	beforeEach(async () => {
		await harness.reset();
		await clearDonationTestEmails(harness);
	});
	async function createDonor(email: string): Promise<void> {
		await donationRepository.createDonor({
			email,
			stripeCustomerId: 'cus_test_123',
			businessName: null,
			taxId: null,
			taxIdType: null,
			stripeSubscriptionId: 'sub_test_123',
			subscriptionAmountCents: 2500,
			subscriptionCurrency: 'usd',
			subscriptionInterval: 'month',
			subscriptionCurrentPeriodEnd: new Date(Date.now() + 30 * 24 * 60 * 60 * 1000),
		});
	}
	describe('email sending behaviour', () => {
		test('sends magic link email when donor exists', async () => {
			await createDonor(TEST_DONOR_EMAIL);
			await createDonationRequestLinkBuilder(harness).body({email: TEST_DONOR_EMAIL}).expect(204).execute();
			const emails = await listDonationTestEmails(harness, {recipient: TEST_DONOR_EMAIL});
			expect(emails).toHaveLength(1);
			expect(emails[0]?.type).toBe('donation_magic_link');
		});
		test('does not send email when donor does not exist', async () => {
			const unknownEmail = createUniqueEmail('unknown');
			await createDonationRequestLinkBuilder(harness).body({email: unknownEmail}).expect(204).execute();
			const emails = await listDonationTestEmails(harness, {recipient: unknownEmail});
			expect(emails).toHaveLength(0);
		});
		test('returns 204 when donor does not exist', async () => {
			await createDonationRequestLinkBuilder(harness)
				.body({email: createUniqueEmail('nonexistent')})
				.expect(204)
				.execute();
		});
		test('magic link URL points to the API endpoint', async () => {
			await createDonor(TEST_DONOR_EMAIL);
			await createDonationRequestLinkBuilder(harness).body({email: TEST_DONOR_EMAIL}).expect(204).execute();
			const emails = await listDonationTestEmails(harness, {recipient: TEST_DONOR_EMAIL});
			expect(emails).toHaveLength(1);
			const manageUrl = emails[0]?.metadata.manage_url;
			expect(manageUrl).toBeDefined();
			expect(manageUrl).toContain(`${Config.endpoints.apiPublic}/donations/manage?token=`);
		});
		test('magic link token is 64-character hex string', async () => {
			await createDonor(TEST_DONOR_EMAIL);
			await createDonationRequestLinkBuilder(harness).body({email: TEST_DONOR_EMAIL}).expect(204).execute();
			const emails = await listDonationTestEmails(harness, {recipient: TEST_DONOR_EMAIL});
			const token = emails[0]?.metadata.token;
			expect(token).toBeDefined();
			expect(token).toHaveLength(64);
			expect(token).toMatch(/^[0-9a-f]{64}$/);
		});
	});
	describe('validation', () => {
		test('rejects invalid email format', async () => {
			await createDonationRequestLinkBuilder(harness).body({email: 'not-an-email'}).expect(400).execute();
		});
		test('rejects email without @ symbol', async () => {
			await createDonationRequestLinkBuilder(harness).body({email: 'invalidemail.com'}).expect(400).execute();
		});
		test('rejects email without domain', async () => {
			await createDonationRequestLinkBuilder(harness).body({email: 'invalid@'}).expect(400).execute();
		});
		test('rejects email without local part', async () => {
			await createDonationRequestLinkBuilder(harness).body({email: '@example.com'}).expect(400).execute();
		});
		test('rejects empty email', async () => {
			await createDonationRequestLinkBuilder(harness).body({email: ''}).expect(400).execute();
		});
		test('rejects missing email field', async () => {
			await createDonationRequestLinkBuilder(harness).body({}).expect(400).execute();
		});
		test('rejects null email', async () => {
			await createDonationRequestLinkBuilder(harness).body({email: null}).expect(400).execute();
		});
		test('rejects email with spaces', async () => {
			await createDonationRequestLinkBuilder(harness)
				.body({email: 'email with spaces@example.com'})
				.expect(400)
				.execute();
		});
		test('rejects email exceeding maximum length', async () => {
			const longLocalPart = 'a'.repeat(250);
			const longEmail = `${longLocalPart}@example.com`;
			await createDonationRequestLinkBuilder(harness).body({email: longEmail}).expect(400).execute();
		});
		test('rejects email with leading/trailing whitespace', async () => {
			await createDonationRequestLinkBuilder(harness).body({email: '  test@example.com  '}).expect(400).execute();
		});
	});
	describe('idempotency', () => {
		test('multiple requests for same existing donor succeed', async () => {
			await createDonor(TEST_DONOR_EMAIL);
			await createDonationRequestLinkBuilder(harness).body({email: TEST_DONOR_EMAIL}).expect(204).execute();
			await createDonationRequestLinkBuilder(harness).body({email: TEST_DONOR_EMAIL}).expect(204).execute();
			await createDonationRequestLinkBuilder(harness).body({email: TEST_DONOR_EMAIL}).expect(204).execute();
		});
		test('generates new token on each request, invalidating previous ones', async () => {
			await createDonor(TEST_DONOR_EMAIL);
			await createDonationRequestLinkBuilder(harness).body({email: TEST_DONOR_EMAIL}).expect(204).execute();
			await createDonationRequestLinkBuilder(harness).body({email: TEST_DONOR_EMAIL}).expect(204).execute();
			const emails = await listDonationTestEmails(harness, {recipient: TEST_DONOR_EMAIL});
			expect(emails).toHaveLength(2);
			const token1 = emails[0]?.metadata.token;
			const token2 = emails[1]?.metadata.token;
			expect(token1).not.toBe(token2);
		});
	});
	describe('email format acceptance', () => {
		test('accepts various valid email formats for existing donors', async () => {
			const validEmails = [
				'simple@example.com',
				'very.common@example.com',
				'disposable.style.email.with+symbol@example.com',
				'other.email-with-hyphen@example.com',
				'user.name@example.co.uk',
				'x@example.com',
				'example-indeed@strange-example.com',
			];
			for (const email of validEmails) {
				await createDonor(email);
				await createDonationRequestLinkBuilder(harness).body({email}).expect(204).execute();
			}
		});
	});
});
