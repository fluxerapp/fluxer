// SPDX-License-Identifier: AGPL-3.0-or-later

import {mapUserToAdminResponse} from '@app/api/admin/models/UserTypes';
import {CODE_FAILURES_DAILY_CAP, recordAddressCodeFailure, requesterNetworkKey} from '@app/api/auth/AccountBudgets';
import {normalizeEmailKey} from '@app/api/auth/AccountCodes';
import {
	createEmailCodeSudoBody,
	createTestAccount,
	createTotpSecret,
	createUniqueEmail,
	findLastTestEmail,
	generateTotpCode,
	listTestEmails,
} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {getConfig} from '@app/api/Config';
import {getKVClient} from '@app/api/middleware/ServiceRegistry';
import {
	getInstanceConfigRepository,
	getRateLimitService,
	getUserRepository,
} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {ClaimAccountNextSteps, VerificationBases} from '@fluxer/constants/src/AccountConfigConstants';
import {AccountIdentityModes} from '@fluxer/constants/src/AccountIdentityConstants';
import {AccountTiers, AccountVerificationNeeds} from '@fluxer/constants/src/AccountTierConstants';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface Me {
	email: string | null;
	verified: boolean;
	claimed: boolean;
	has_password: boolean;
	global_name: string | null;
	profile_restricted: boolean;
}

interface Registered {
	token: string;
	user_id: string;
}

describe('Claiming an account', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	async function createUnclaimed(username = 'drifter'): Promise<Registered> {
		return await createBuilderWithoutAuth<Registered>(harness)
			.post('/auth/register')
			.body({username, consent: true, date_of_birth: '1995-05-05'})
			.execute();
	}

	function claim(token: string, body: Record<string, unknown>) {
		return createBuilder<{user: Me; code_expires_at: string}>(harness, token).post('/users/@me/claim').body(body);
	}

	async function me(token: string): Promise<Me> {
		return await createBuilder<Me>(harness, token).get('/users/@me').execute();
	}

	async function lastCode(email: string): Promise<string | null> {
		return (
			findLastTestEmail(await listTestEmails(harness, {recipient: email}), 'email_verification_code')?.metadata[
				'code'
			] ?? null
		);
	}

	it('adds a pending address, then confirms it with the emailed code', async () => {
		const account = await createUnclaimed();
		const email = createUniqueEmail('claim');
		const claimed = await claim(account.token, {email}).execute();
		expect(claimed.user).toMatchObject({email, verified: false, claimed: true, has_password: false});
		const code = await lastCode(email);
		expect(code).toMatch(/^\d{6}$/);
		const confirmed = await createBuilder<Me>(harness, account.token)
			.post('/users/@me/email-verification/confirm')
			.body({code})
			.execute();
		expect(confirmed).toMatchObject({email, verified: true, claimed: true});
	});

	async function expectEmailInUse(builder: ReturnType<typeof claim>): Promise<void> {
		const {json} = await builder.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY').executeWithResponse();
		expect((json as unknown as {errors: Array<{path: string; code: string}>}).errors).toEqual([
			expect.objectContaining({path: 'email', code: ValidationErrorCodes.EMAIL_ALREADY_IN_USE}),
		]);
	}

	it('refuses an address stored on another account, confirmed or not, and registration does too', async () => {
		const owner = await createTestAccount(harness);
		const account = await createUnclaimed();
		await expectEmailInUse(claim(account.token, {email: owner.email}));
		const email = createUniqueEmail('held');
		await claim(account.token, {email}).execute();
		const late = await createUnclaimed('late');
		await expectEmailInUse(claim(late.token, {email}));
		await expectEmailInUse(
			createBuilderWithoutAuth<{user: Me; code_expires_at: string}>(harness).post('/auth/register').body({
				email,
				username: 'latecomer',
				password: 'a-strong-password-9',
				consent: true,
				date_of_birth: '1995-05-05',
			}),
		);
		expect(await me(late.token)).toMatchObject({email: null});
	});

	it('stores an address on one account only when two claims race for it', async () => {
		const email = createUniqueEmail('race');
		const racers = await Promise.all([createUnclaimed('racer_one'), createUnclaimed('racer_two')]);
		const results = await Promise.all(
			racers.map(async (racer) => (await claim(racer.token, {email}).executeRaw()).response.status),
		);
		expect(results.sort()).toEqual([HTTP_STATUS.OK, HTTP_STATUS.BAD_REQUEST]);
		const holders = (await Promise.all(racers.map((racer) => me(racer.token)))).filter((user) => user.email === email);
		expect(holders).toHaveLength(1);
	});

	it('sets the password at claim and lets the account sign in with it', async () => {
		const account = await createUnclaimed();
		const email = createUniqueEmail('claim-password');
		const claimed = await claim(account.token, {email, password: 'a-strong-password-1'}).execute();
		expect(claimed.user.has_password).toBe(true);
		const login = await createBuilderWithoutAuth<{user_id: string}>(harness)
			.post('/auth/login')
			.body({email, password: 'a-strong-password-1'})
			.execute();
		expect(login.user_id).toBe(account.user_id);
	});

	it('refuses a second claim and points unclaimed accounts at the claim route', async () => {
		const account = await createTestAccount(harness);
		await claim(account.token, {email: createUniqueEmail('again')})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.ACCOUNT_ALREADY_CLAIMED)
			.execute();
		const unclaimed = await createUnclaimed();
		await createBuilder(harness, unclaimed.token)
			.post('/users/@me/email-verification/send')
			.body(null)
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.USE_CLAIM_ROUTE)
			.execute();
	});

	it('counts a wrong claim code against the network that asked for it', async () => {
		const email = createUniqueEmail('split');
		const env = {kv: getKVClient(), rateLimit: getRateLimitService(), enforce: true};
		for (let failure = 1; failure < CODE_FAILURES_DAILY_CAP; failure++) {
			await recordAddressCodeFailure(env, {
				emailKey: normalizeEmailKey(email),
				network: requesterNetworkKey('203.0.113.9'),
			});
		}
		const account = await createUnclaimed('splitter');
		await claim(account.token, {email})
			.header('x-forwarded-for', '203.0.113.9')
			.header('x-fluxer-test-enable-rate-limits', 'true')
			.execute();
		await createBuilder(harness, account.token)
			.post('/users/@me/email-verification/confirm')
			.header('x-forwarded-for', '198.51.100.7')
			.body({code: '000000'})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_SIGN_IN_CODE)
			.execute();
		const resend = (ip: string) =>
			createBuilder(harness, account.token)
				.post('/users/@me/email-verification/send')
				.header('x-forwarded-for', ip)
				.header('x-fluxer-test-enable-rate-limits', 'true')
				.body(null);
		await resend('203.0.113.200').expect(429, APIErrorCodes.EMAIL_CODE_RESEND_COOLDOWN).execute();
		await resend('198.51.100.7').expect(HTTP_STATUS.OK).execute();
	});

	it('requires a password to claim when the instance cannot send email', async () => {
		await getInstanceConfigRepository().setInstanceIntegrationsConfig({email: {enabled: false}});
		await getInstanceConfigRepository().isEmailEnabled();
		const account = await createUnclaimed();
		await claim(account.token, {email: createUniqueEmail('no-delivery')})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.PASSWORD_REQUIRED_TO_CLAIM)
			.execute();
		const email = createUniqueEmail('no-delivery');
		const claimed = await claim(account.token, {email, password: 'a-strong-password-2'}).execute();
		expect(claimed.user).toMatchObject({email, verified: true, claimed: true});
	});
});

describe('Account limits by tier', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
		await getInstanceConfigRepository().updateAccountsConfig({interaction_tier: 'enforced'});
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	async function createUnclaimed(): Promise<Registered> {
		return await createBuilderWithoutAuth<Registered>(harness)
			.post('/auth/register')
			.body({username: 'quiet', consent: true, date_of_birth: '1995-05-05'})
			.execute();
	}

	it('lets an unclaimed account edit its profile, and others see only the username and tag', async () => {
		const account = await createUnclaimed();
		const viewer = await createTestAccount(harness);
		await createBuilder(harness, account.token)
			.patch('/users/@me')
			.body({global_name: 'Visible Only To Me', bio: 'hidden bio'})
			.execute();
		const own = await createBuilder<Me>(harness, account.token).get('/users/@me').execute();
		expect(own).toMatchObject({global_name: 'Visible Only To Me', profile_restricted: true});
		const seen = await createBuilder<{global_name: string | null; username: string}>(harness, viewer.token)
			.get(`/users/${account.user_id}`)
			.execute();
		expect(seen.global_name).toBeNull();
		expect(seen.username).toBe('quiet');
		const guild = await createBuilder<{id: string; system_channel_id: string}>(harness, viewer.token)
			.post('/guilds')
			.body({name: 'Shared'})
			.execute();
		const invite = await createBuilder<{code: string}>(harness, viewer.token)
			.post(`/channels/${guild.system_channel_id}/invites`)
			.body({})
			.execute();
		await createBuilder(harness, account.token).post(`/invites/${invite.code}`).body(null).execute();
		const profile = await createBuilder<{user_profile: {bio: string | null}}>(harness, viewer.token)
			.get(`/users/${account.user_id}/profile`)
			.execute();
		expect(profile.user_profile.bio).toBeNull();
	});

	it('refuses a bare password for an unclaimed account on email instances', async () => {
		const account = await createUnclaimed();
		await createBuilder(harness, account.token)
			.patch('/users/@me')
			.body({new_password: 'a-strong-password-3'})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.USE_CLAIM_ROUTE)
			.execute();
	});

	it('keeps the old limits for unclaimed profile edits in legacy mode', async () => {
		await getInstanceConfigRepository().updateAccountsConfig({interaction_tier: 'legacy'});
		const account = await createUnclaimed();
		await createBuilder(harness, account.token)
			.patch('/users/@me')
			.body({global_name: 'Not Yet'})
			.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
			.execute();
	});

	it('refuses invites and reports from unclaimed accounts only where they reach others', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createBuilder<{id: string; system_channel_id: string}>(harness, owner.token)
			.post('/guilds')
			.body({name: 'Tier Guild'})
			.execute();
		const unverified = await createTestAccount(harness, {skipEmailVerification: true});
		await createBuilder(harness, unverified.token)
			.post(`/channels/${guild.system_channel_id}/invites`)
			.body({})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.ACCOUNT_VERIFICATION_REQUIRED)
			.execute();
		const reporter = await createUnclaimed();
		const report = await createBuilder<{report_id: string}>(harness, reporter.token)
			.post('/reports/user')
			.body({user_id: owner.userId, category: 'harassment'})
			.execute();
		expect(report.report_id).toBeTruthy();
	});

	it('drops a second report on the same target from the same network', async () => {
		const target = await createTestAccount(harness);
		const first = await createUnclaimed();
		const second = await createUnclaimed();
		const send = (token: string) =>
			createBuilder<{report_id: string}>(harness, token)
				.post('/reports/user')
				.header('x-forwarded-for', '203.0.113.20')
				.body({user_id: target.userId, category: 'harassment'})
				.execute();
		const kept = await send(first.token);
		const dropped = await send(second.token);
		const {getReportRepository} = await import('@app/api/middleware/ServiceSingletons');
		const {createReportID} = await import('@app/api/BrandedTypes');
		expect(await getReportRepository().getReport(createReportID(BigInt(kept.report_id)))).not.toBeNull();
		expect(await getReportRepository().getReport(createReportID(BigInt(dropped.report_id)))).toBeNull();
	});
});

describe('Sudo by email code', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	async function passwordlessAccount(): Promise<{token: string; email: string; userId: string}> {
		const account = await createTestAccount(harness);
		await createBuilder(harness, '')
			.post(`/test/users/${account.userId}/security-flags`)
			.body({email_verified: true})
			.execute();
		const {getUserRepository} = await import('@app/api/middleware/ServiceSingletons');
		const {createUserID} = await import('@app/api/BrandedTypes');
		const user = await getUserRepository().findUniqueAssert(createUserID(BigInt(account.userId)));
		await getUserRepository().patchUpsert(user.id, {password_hash: null}, user.toRow());
		return {token: account.token, email: account.email, userId: account.userId};
	}

	it('asks a passwordless account with a confirmed email for an email code instead of waving it through', async () => {
		const account = await passwordlessAccount();
		const challenge = await createBuilder<{methods: {email_code: boolean; password: boolean}}>(harness, account.token)
			.post('/users/@me/disable')
			.body({})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.SUDO_MODE_REQUIRED)
			.execute();
		expect(challenge.methods).toMatchObject({email_code: true, password: false});
		const sudo = await createEmailCodeSudoBody(harness, account);
		await createBuilder(harness, account.token).post('/users/@me/disable').body(sudo).expect(204).execute();
	});

	it('asks a passwordless account for an email code even when the client sends no capabilities', async () => {
		const account = await passwordlessAccount();
		const challenge = await createBuilder<{methods: {email_code: boolean}}>(harness, account.token)
			.post('/users/@me/disable')
			.body({})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.SUDO_MODE_REQUIRED)
			.execute();
		expect(challenge.methods.email_code).toBe(true);
	});

	it('refuses a wrong email code', async () => {
		const account = await passwordlessAccount();
		const sudo = await createEmailCodeSudoBody(harness, account);
		await createBuilder(harness, account.token)
			.post('/users/@me/disable')
			.body({...sudo, email_code: sudo.email_code === '000000' ? '111111' : '000000'})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_SIGN_IN_CODE)
			.execute();
	});

	it('never accepts an email code alone from an account with two-factor authentication', async () => {
		const account = await createTestAccount(harness);
		const secret = createTotpSecret();
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/totp/enable')
			.body({secret, code: generateTotpCode(secret), password: account.password})
			.execute();
		await createBuilder(harness, account.token)
			.post('/users/@me/sudo/email-code')
			.body(null)
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.SUDO_SECOND_FACTOR_REQUIRED)
			.execute();
		await createBuilder(harness, account.token)
			.post('/users/@me/disable')
			.body({mfa_method: 'email_code', email_code_ticket: 'x', email_code: '123456'})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.SUDO_MODE_REQUIRED)
			.execute();
	});

	it('asks a passwordless account with two-factor authentication for that factor, not a password', async () => {
		const account = await passwordlessAccount();
		const secret = createTotpSecret();
		const {getUserRepository} = await import('@app/api/middleware/ServiceSingletons');
		const {createUserID} = await import('@app/api/BrandedTypes');
		const {UserAuthenticatorTypes} = await import('@fluxer/constants/src/UserConstants');
		const user = await getUserRepository().findUniqueAssert(createUserID(BigInt(account.userId)));
		await getUserRepository().patchUpsert(
			user.id,
			{totp_secret: secret, authenticator_types: new Set([UserAuthenticatorTypes.TOTP])},
			user.toRow(),
		);
		await createBuilder(harness, account.token)
			.post('/users/@me/sudo/email-code')
			.body(null)
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.SUDO_SECOND_FACTOR_REQUIRED)
			.execute();
	});

	it('still waves through an account with no email, password or passkey', async () => {
		const account = await createBuilderWithoutAuth<Registered>(harness)
			.post('/auth/register')
			.body({username: 'nothing', consent: true, date_of_birth: '1995-05-05'})
			.execute();
		await createBuilder(harness, account.token).post('/users/@me/disable').body({}).expect(204).execute();
	});
});

describe('Claim next step, tier payloads and email codes', () => {
	let harness: ApiTestHarness;
	let originalSelfHosted: boolean;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
		originalSelfHosted = getConfig().instance.selfHosted;
	});
	afterEach(() => {
		getConfig().instance.selfHosted = originalSelfHosted;
		getInstanceConfigRepository().clearCacheForTesting();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	interface TierMe {
		account_tier: string;
		verification_need: string | null;
		claimed: boolean;
	}

	interface ClaimResult {
		next_step: string;
		user: TierMe;
		code_expires_at?: string;
		resend_available_at?: string;
	}

	async function createUnclaimed(username = 'drifter'): Promise<Registered> {
		return await createBuilderWithoutAuth<Registered>(harness)
			.post('/auth/register')
			.body({username, consent: true, date_of_birth: '1995-05-05'})
			.execute();
	}

	async function createUsernameModeUnclaimed(): Promise<Registered> {
		getConfig().instance.selfHosted = true;
		await getInstanceConfigRepository().setAccountIdentityMode(AccountIdentityModes.USERNAME, 'setup');
		await getInstanceConfigRepository().setAppPublicConfig({setup: {configured: true}});
		await getInstanceConfigRepository().updateAuthConfig({instant_accounts: {enabled: true}});
		return await createBuilderWithoutAuth<Registered>(harness)
			.post('/auth/register')
			.body({global_name: 'Guest', date_of_birth: '1995-05-05'})
			.execute();
	}

	async function turnDeliveryOff(): Promise<void> {
		await getInstanceConfigRepository().setInstanceIntegrationsConfig({email: {enabled: false}});
		await getInstanceConfigRepository().isEmailEnabled();
	}

	function tierOf(token: string): Promise<TierMe> {
		return createBuilder<TierMe>(harness, token).get('/users/@me').execute();
	}

	it('reports the tier and the next need in the private user payload as the account is claimed', async () => {
		const account = await createUnclaimed();
		expect(await tierOf(account.token)).toMatchObject({
			account_tier: AccountTiers.UNCLAIMED,
			verification_need: AccountVerificationNeeds.ADD_EMAIL,
			claimed: false,
		});
		const email = createUniqueEmail('tier');
		const claimed = await createBuilder<ClaimResult>(harness, account.token)
			.post('/users/@me/claim')
			.body({email})
			.execute();
		expect(claimed.next_step).toBe(ClaimAccountNextSteps.CONFIRM_EMAIL);
		expect(claimed.code_expires_at).toBeTruthy();
		expect(claimed.resend_available_at).toBeTruthy();
		expect(claimed.user).toMatchObject({
			account_tier: AccountTiers.UNVERIFIED,
			verification_need: AccountVerificationNeeds.CONFIRM_EMAIL,
			claimed: true,
		});
		const code = findLastTestEmail(await listTestEmails(harness, {recipient: email}), 'email_verification_code')
			?.metadata['code'];
		const confirmed = await createBuilder<TierMe>(harness, account.token)
			.post('/users/@me/email-verification/confirm')
			.body({code})
			.execute();
		expect(confirmed).toMatchObject({account_tier: AccountTiers.VERIFIED, verification_need: null, claimed: true});
	});

	it('returns next_step none without code times when the instance cannot send email', async () => {
		await turnDeliveryOff();
		const account = await createUnclaimed();
		const claimed = await createBuilder<ClaimResult>(harness, account.token)
			.post('/users/@me/claim')
			.body({email: createUniqueEmail('quiet'), password: 'a-strong-password-4'})
			.execute();
		expect(claimed.next_step).toBe(ClaimAccountNextSteps.NONE);
		expect(claimed).not.toHaveProperty('code_expires_at');
		expect(claimed).not.toHaveProperty('resend_available_at');
		expect(claimed.user).toMatchObject({account_tier: AccountTiers.VERIFIED, verification_need: null});
	});

	it('returns next_step none on username instances and asks for a password before anything else', async () => {
		const account = await createUsernameModeUnclaimed();
		expect(await tierOf(account.token)).toMatchObject({
			account_tier: AccountTiers.UNCLAIMED,
			verification_need: AccountVerificationNeeds.SET_PASSWORD,
		});
		await createBuilder(harness, account.token)
			.post('/users/@me/claim')
			.body({})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.PASSWORD_REQUIRED_TO_CLAIM)
			.execute();
		const claimed = await createBuilder<ClaimResult>(harness, account.token)
			.post('/users/@me/claim')
			.body({password: 'a-strong-password-5'})
			.execute();
		expect(claimed.next_step).toBe(ClaimAccountNextSteps.NONE);
		expect(claimed).not.toHaveProperty('code_expires_at');
		expect(claimed.user).toMatchObject({account_tier: AccountTiers.VERIFIED, verification_need: null});
	});

	it('includes the tier in the admin user payload', async () => {
		const unclaimed = await createUnclaimed();
		const verified = await createTestAccount(harness);
		const users = getUserRepository();
		const unclaimedUser = await users.findUniqueAssert(createUserID(BigInt(unclaimed.user_id)));
		const verifiedUser = await users.findUniqueAssert(createUserID(BigInt(verified.userId)));
		expect(await mapUserToAdminResponse(unclaimedUser)).toMatchObject({
			account_tier: AccountTiers.UNCLAIMED,
			claimed: false,
		});
		expect(await mapUserToAdminResponse(verifiedUser)).toMatchObject({
			account_tier: AccountTiers.VERIFIED,
			claimed: true,
		});
	});

	it('refuses email code flows with EMAIL_CODES_UNAVAILABLE when the instance cannot send email', async () => {
		const account = await createTestAccount(harness);
		await turnDeliveryOff();
		const sentBefore = (await listTestEmails(harness, {recipient: account.email})).length;
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/backup-codes/challenge')
			.body({})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.EMAIL_CODES_UNAVAILABLE)
			.execute();
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/backup-codes/challenge/resend')
			.body({ticket: 'missing'})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.EMAIL_CODES_UNAVAILABLE)
			.execute();
		await createBuilder(harness, account.token)
			.post('/users/@me/email-change/start')
			.body({})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.EMAIL_CODES_UNAVAILABLE)
			.execute();
		await createBuilder(harness, account.token)
			.post('/users/@me/email-change/resend-original')
			.body({ticket: 'missing'})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.EMAIL_CODES_UNAVAILABLE)
			.execute();
		expect(await listTestEmails(harness, {recipient: account.email})).toHaveLength(sentBefore);
	});

	async function claimRefusal(token: string): Promise<string> {
		const {json} = await createBuilder(harness, token)
			.post('/users/@me/claim')
			.body({email: createUniqueEmail('bare')})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.PASSWORD_REQUIRED_TO_CLAIM)
			.executeWithResponse();
		return (json as unknown as {message: string}).message;
	}

	it('says why a claim needs a password on an email instance', async () => {
		await getInstanceConfigRepository().updateAccountsConfig({verification_basis: VerificationBases.LOCAL_CREDENTIAL});
		const account = await createUnclaimed();
		expect(await claimRefusal(account.token)).toBe(
			'Set a password to keep this account. This instance asks every account for a password or passkey.',
		);
		await turnDeliveryOff();
		expect(await claimRefusal(account.token)).toBe(
			"Set a password to keep this account. This instance can't send email codes.",
		);
	});

	it('falls back to a local credential when a confirmed email basis can no longer be delivered', async () => {
		await getInstanceConfigRepository().updateAccountsConfig({verification_basis: VerificationBases.CONFIRMED_EMAIL});
		const account = await createTestAccount(harness, {skipEmailVerification: true});
		expect(await tierOf(account.token)).toMatchObject({account_tier: AccountTiers.UNVERIFIED});
		await turnDeliveryOff();
		const discovery = await createBuilderWithoutAuth<{features: {verification_basis: string}}>(harness)
			.get('/.well-known/fluxer')
			.execute();
		expect(discovery.features.verification_basis).toBe(VerificationBases.LOCAL_CREDENTIAL);
		expect(await tierOf(account.token)).toMatchObject({account_tier: AccountTiers.VERIFIED, verification_need: null});
		const unclaimed = await createUnclaimed();
		expect(await claimRefusal(unclaimed.token)).toBe(
			"Set a password to keep this account. This instance can't send email codes.",
		);
	});

	it('answers EMAIL_CODES_UNAVAILABLE instead of already claimed for a verified account with an unconfirmed email', async () => {
		const account = await createTestAccount(harness, {skipEmailVerification: true});
		await turnDeliveryOff();
		expect(await tierOf(account.token)).toMatchObject({account_tier: AccountTiers.VERIFIED});
		await createBuilder(harness, account.token)
			.post('/users/@me/email-verification/send')
			.body(null)
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.EMAIL_CODES_UNAVAILABLE)
			.execute();
	});
});
