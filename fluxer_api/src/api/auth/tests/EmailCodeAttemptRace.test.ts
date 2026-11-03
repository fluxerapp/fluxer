// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	ACCOUNT_CODE_FAILURES_DAILY_CAP,
	assertEmailSendAllowed,
	CODE_FAILURES_DAILY_CAP,
	enforceLookupHardLimits,
	recordAccountCodeFailure,
	recordAddressCodeFailure,
	requesterNetworkKey,
} from '@app/api/auth/AccountBudgets';
import {normalizeEmailKey} from '@app/api/auth/AccountCodes';
import {
	clearTestEmails,
	createEmailCodeSudoBody,
	createTestAccount,
	createUniqueEmail,
	findLastTestEmail,
	listTestEmails,
} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {getKVClient} from '@app/api/middleware/ServiceRegistry';
import {
	getInstanceConfigRepository,
	getRateLimitService,
	getUserRepository,
} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {EmailCodeResendCooldownError} from '@fluxer/errors/src/domains/auth/EmailCodeResendCooldownError';
import {RateLimitError} from '@fluxer/errors/src/domains/core/RateLimitError';
import {MockKVProvider} from '@pkgs/kv_client/src/testing/MockKVProvider';
import {RateLimitService} from '@pkgs/rate_limit/src/RateLimitService';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const GUESSES = 40;
const MAX_ATTEMPTS = 5;

function wrongGuesses(code: string): Array<string> {
	return Array.from({length: GUESSES}, (_, index) =>
		((Number(code) + index + 1) % 1_000_000).toString().padStart(6, '0'),
	);
}

function countChecked(results: Array<{json?: {code?: string} | null}>): number {
	return results.filter((result) => result.json?.code === APIErrorCodes.INVALID_SIGN_IN_CODE).length;
}

describe('Email code attempt cap under concurrency', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
		await getInstanceConfigRepository().updateAuthConfig({identifier_first: true});
		await clearTestEmails(harness);
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	async function startSignIn(email: string): Promise<{ticket: string; code: string}> {
		const {ticket} = await createBuilderWithoutAuth<{ticket: string}>(harness)
			.post('/auth/identify')
			.body({email})
			.execute();
		await createBuilderWithoutAuth(harness).post('/auth/email-code/send').body({ticket}).execute();
		const code = findLastTestEmail(await listTestEmails(harness, {recipient: email}), 'sign_in_code')!.metadata[
			'code'
		]!;
		return {ticket, code};
	}

	async function createPasswordlessAccount() {
		const account = await createTestAccount(harness);
		const user = await getUserRepository().findUniqueAssert(createUserID(BigInt(account.userId)));
		await getUserRepository().patchUpsert(user.id, {password_hash: null}, user.toRow());
		return account;
	}

	it('checks at most five sign-in guesses per code even when they arrive together', async () => {
		const account = await createPasswordlessAccount();
		const {ticket, code} = await startSignIn(account.email);
		const results = await Promise.all(
			wrongGuesses(code).map((guess) =>
				createBuilderWithoutAuth<{code?: string}>(harness)
					.post('/auth/email-code/verify')
					.body({ticket, code: guess})
					.executeRaw(),
			),
		);
		expect(countChecked(results)).toBeLessThanOrEqual(MAX_ATTEMPTS);
		const last = await createBuilderWithoutAuth<{code?: string; token?: string}>(harness)
			.post('/auth/email-code/verify')
			.body({ticket, code})
			.executeRaw();
		expect(last.json?.token).toBeUndefined();
		expect(last.json?.code).toBe(APIErrorCodes.SIGN_IN_CODE_EXPIRED);
	});

	it('accepts a correct sign-in code exactly once', async () => {
		const account = await createPasswordlessAccount();
		const {ticket, code} = await startSignIn(account.email);
		const results = await Promise.all(
			Array.from({length: 4}, () =>
				createBuilderWithoutAuth<{code?: string; token?: string; ticket?: string}>(harness)
					.post('/auth/email-code/verify')
					.body({ticket, code})
					.executeRaw(),
			),
		);
		expect(results.filter((result) => result.json?.token || result.json?.ticket).length).toBe(1);
	});

	it('checks at most five claim confirmation guesses even when they arrive together', async () => {
		const account = await createBuilderWithoutAuth<{token: string}>(harness)
			.post('/auth/register')
			.body({username: 'racer', consent: true, date_of_birth: '1995-05-05'})
			.execute();
		const email = createUniqueEmail('claim-race');
		await createBuilder(harness, account.token).post('/users/@me/claim').body({email}).execute();
		const code = findLastTestEmail(await listTestEmails(harness, {recipient: email}), 'email_verification_code')!
			.metadata['code']!;
		const results = await Promise.all(
			wrongGuesses(code).map((guess) =>
				createBuilder<{code?: string}>(harness, account.token)
					.post('/users/@me/email-verification/confirm')
					.body({code: guess})
					.executeRaw(),
			),
		);
		expect(countChecked(results)).toBeLessThanOrEqual(MAX_ATTEMPTS);
		const last = await createBuilder<{code?: string; verified?: boolean}>(harness, account.token)
			.post('/users/@me/email-verification/confirm')
			.body({code})
			.executeRaw();
		expect(last.json?.code).toBe(APIErrorCodes.SIGN_IN_CODE_EXPIRED);
	});

	it('checks at most five sudo code guesses even when they arrive together', async () => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, '')
			.post(`/test/users/${account.userId}/security-flags`)
			.body({email_verified: true})
			.execute();
		const user = await getUserRepository().findUniqueAssert(createUserID(BigInt(account.userId)));
		await getUserRepository().patchUpsert(user.id, {password_hash: null}, user.toRow());
		const sudo = await createEmailCodeSudoBody(harness, account);
		const results = await Promise.all(
			wrongGuesses(sudo.email_code).map((guess) =>
				createBuilder<{code?: string}>(harness, account.token)
					.post('/users/@me/disable')
					.body({...sudo, email_code: guess})
					.executeRaw(),
			),
		);
		expect(countChecked(results)).toBeLessThanOrEqual(MAX_ATTEMPTS);
		const last = await createBuilder<{code?: string}>(harness, account.token)
			.post('/users/@me/disable')
			.body(sudo)
			.executeRaw();
		expect(last.response.status).toBe(400);
		expect(last.json?.code).toBe(APIErrorCodes.UNKNOWN_SIGN_IN_TICKET);
	});

	it('stops sudo codes once wrong sudo codes reach the account cap', async () => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, '')
			.post(`/test/users/${account.userId}/security-flags`)
			.body({email_verified: true})
			.execute();
		const user = await getUserRepository().findUniqueAssert(createUserID(BigInt(account.userId)));
		await getUserRepository().patchUpsert(user.id, {password_hash: null}, user.toRow());
		const env = {kv: getKVClient(), rateLimit: getRateLimitService(), enforce: true};
		for (let failure = 1; failure < ACCOUNT_CODE_FAILURES_DAILY_CAP; failure++) {
			await recordAccountCodeFailure(env, account.userId);
		}
		const sudo = await createEmailCodeSudoBody(harness, account);
		await createBuilder(harness, account.token)
			.post('/users/@me/disable')
			.body({...sudo, email_code: wrongGuesses(sudo.email_code)[0]})
			.expect(400, APIErrorCodes.INVALID_SIGN_IN_CODE)
			.execute();
		await createBuilder(harness, account.token)
			.post('/users/@me/sudo/email-code')
			.header('x-fluxer-test-enable-rate-limits', 'true')
			.body(null)
			.expect(429, APIErrorCodes.EMAIL_CODE_RESEND_COOLDOWN)
			.execute();
	});
});

function budgetEnv() {
	const kv = new MockKVProvider();
	return {kv, rateLimit: new RateLimitService(kv), enforce: true};
}

describe('Daily code failure caps', () => {
	async function failFrom(
		env: Parameters<typeof recordAddressCodeFailure>[0],
		emailKey: string,
		ipFor: (failure: number) => string,
	) {
		for (let failure = 0; failure < CODE_FAILURES_DAILY_CAP; failure++) {
			const clientIp = ipFor(failure);
			await assertEmailSendAllowed(env, {emailKey, requester: 'unknown', clientIp});
			await recordAddressCodeFailure(env, {emailKey, network: requesterNetworkKey(clientIp)});
		}
	}

	it('stops unknown requesters on the failing IPv4 /24 only', async () => {
		const env = budgetEnv();
		const emailKey = normalizeEmailKey('target@example.com');
		await failFrom(env, emailKey, (failure) => `203.0.113.${failure + 1}`);
		await expect(
			assertEmailSendAllowed(env, {emailKey, requester: 'unknown', clientIp: '203.0.113.77'}),
		).rejects.toBeInstanceOf(EmailCodeResendCooldownError);
		await expect(
			assertEmailSendAllowed(env, {emailKey, requester: 'unknown', clientIp: '198.51.100.4'}),
		).resolves.toBeUndefined();
		await expect(
			assertEmailSendAllowed(env, {emailKey, requester: 'known', clientIp: '203.0.113.9'}),
		).resolves.toBeUndefined();
	});

	it('stops unknown requesters on the failing IPv6 /48 only', async () => {
		const env = budgetEnv();
		const emailKey = normalizeEmailKey('target@example.com');
		await failFrom(env, emailKey, (failure) => `2001:db8:1:${failure + 1}::1`);
		await expect(
			assertEmailSendAllowed(env, {emailKey, requester: 'unknown', clientIp: '2001:db8:1:ffff::2'}),
		).rejects.toBeInstanceOf(EmailCodeResendCooldownError);
		await expect(
			assertEmailSendAllowed(env, {emailKey, requester: 'unknown', clientIp: '2001:db8:2::1'}),
		).resolves.toBeUndefined();
	});

	it('stops every code to an account once its own wrong codes reach the account cap', async () => {
		const env = budgetEnv();
		const emailKey = normalizeEmailKey('owner@example.com');
		const userId = '1234567890';
		for (let failure = 0; failure < ACCOUNT_CODE_FAILURES_DAILY_CAP; failure++) {
			await assertEmailSendAllowed(env, {emailKey, requester: 'known', clientIp: `203.0.113.${failure + 1}`, userId});
			await recordAccountCodeFailure(env, userId);
		}
		for (const requester of ['known', 'unknown'] as const) {
			await expect(
				assertEmailSendAllowed(env, {emailKey, requester, clientIp: '198.51.100.4', userId}),
			).rejects.toBeInstanceOf(EmailCodeResendCooldownError);
		}
		await expect(
			assertEmailSendAllowed(env, {emailKey, requester: 'unknown', clientIp: '198.51.100.4'}),
		).resolves.toBeUndefined();
	});

	it('caps lookups at 30 per IP and 300 per subnet', async () => {
		const env = budgetEnv();
		for (let lookup = 0; lookup < 30; lookup++) await enforceLookupHardLimits(env, '203.0.113.1');
		await expect(enforceLookupHardLimits(env, '203.0.113.1')).rejects.toBeInstanceOf(RateLimitError);
		for (let host = 2; host <= 10; host++) {
			for (let lookup = 0; lookup < 30; lookup++) await enforceLookupHardLimits(env, `203.0.113.${host}`);
		}
		await expect(enforceLookupHardLimits(env, '203.0.113.11')).rejects.toBeInstanceOf(RateLimitError);
		await expect(enforceLookupHardLimits(env, '198.51.100.1')).resolves.toBeUndefined();
	});
});
