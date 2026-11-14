// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	ACCOUNT_CODE_FAILURES_DAILY_CAP,
	ADDRESS_CODE_FAILURES_DAILY_CAP,
	ADDRESS_CODE_FAILURES_PER_STEP,
	CODE_FAILURES_DAILY_CAP,
	enforceLookupHardLimits,
	GLOBAL_CODE_FAILURES_THRESHOLD,
	recordAccountCodeFailure,
	recordAddressCodeFailure,
	requesterNetworkKey,
} from '@app/api/auth/AccountBudgets';
import {normalizeEmailKey} from '@app/api/auth/AccountCodes';
import {
	clearTestEmails,
	createTestAccount,
	createTotpSecret,
	createUniqueEmail,
	findLastTestEmail,
	generateTotpCode,
	listTestEmails,
} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {getContentMessage} from '@app/api/content_i18n/ContentI18n';
import {getKVClient} from '@app/api/middleware/ServiceRegistry';
import {
	getInstanceConfigRepository,
	getRateLimitService,
	getUserRepository,
} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {
	CAPTCHA_TEST_HEADER,
	type CaptchaErrorBody,
	solveCaptchaChallenge,
	useCheapCaptcha,
} from '@app/api/test/CaptchaTestUtils';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface IdentifyBody {
	ticket: string;
	account: 'existing' | 'new';
	sign_in_method: 'password' | 'code' | null;
	registration: {mode: string; consent_required: boolean; requires_password: boolean} | null;
}

interface SignInBody {
	token?: string;
	user_id?: string;
	device_proof?: string;
	mfa?: boolean;
	ticket?: string;
}

const STRANGER_IP = '203.0.113.9';
const ENABLE_RATE_LIMITS = 'x-fluxer-test-enable-rate-limits';

describe('Identifier-first sign-in', () => {
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

	async function identify(email: string, ip = STRANGER_IP): Promise<IdentifyBody> {
		return await createBuilderWithoutAuth<IdentifyBody>(harness)
			.post('/auth/identify')
			.header('x-forwarded-for', ip)
			.body({email})
			.execute();
	}

	async function sendCode(ticket: string, ip = STRANGER_IP): Promise<{purpose: string}> {
		return await createBuilderWithoutAuth<{purpose: string}>(harness)
			.post('/auth/email-code/send')
			.header('x-forwarded-for', ip)
			.body({ticket})
			.execute();
	}

	async function dropPassword(account: {userId: string}): Promise<void> {
		const user = await getUserRepository().findUniqueAssert(createUserID(BigInt(account.userId)));
		await getUserRepository().patchUpsert(user.id, {password_hash: null}, user.toRow());
	}

	async function stayInsideOneLookupWindow(): Promise<void> {
		const windowMs = 10 * 60 * 1000;
		const remainingMs = windowMs - (Date.now() % windowMs);
		if (remainingMs < 15_000) await new Promise((resolve) => setTimeout(resolve, remainingMs + 100));
	}

	function lastCode(email: string, type: string, emails: Awaited<ReturnType<typeof listTestEmails>>): string {
		const record = findLastTestEmail(
			emails.filter((entry) => entry.to.toLowerCase() === email.toLowerCase()),
			type,
		);
		expect(record).not.toBeNull();
		return record!.metadata['code']!;
	}

	it('refuses the lookup while the instance keeps the older sign-in', async () => {
		await getInstanceConfigRepository().updateAuthConfig({identifier_first: false});
		await createBuilderWithoutAuth(harness)
			.post('/auth/identify')
			.body({email: createUniqueEmail('off')})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.FEATURE_TEMPORARILY_DISABLED)
			.execute();
	});

	it('answers existing and new with the same keys and no sign-in methods', async () => {
		const account = await createTestAccount(harness);
		const existing = await identify(account.email);
		const fresh = await identify(createUniqueEmail('fresh'));
		expect(existing.account).toBe('existing');
		expect(existing.registration).toBeNull();
		expect(fresh.account).toBe('new');
		expect(fresh.registration).toEqual({
			mode: 'open',
			consent_required: true,
			requires_password: false,
			instant_account_availability: 'available',
		});
		expect(Object.keys(existing).sort()).toEqual(Object.keys(fresh).sort());
		expect(existing.ticket).toHaveLength(43);
	});

	it('signs in with an emailed code, authorizes the IP and returns a device proof', async () => {
		const account = await createTestAccount(harness);
		await dropPassword(account);
		const {ticket} = await identify(account.email);
		expect((await sendCode(ticket)).purpose).toBe('sign_in');
		const emails = await listTestEmails(harness, {recipient: account.email});
		const code = lastCode(account.email, 'sign_in_code', emails);
		const signedIn = await createBuilderWithoutAuth<SignInBody>(harness)
			.post('/auth/email-code/verify')
			.header('x-forwarded-for', STRANGER_IP)
			.body({ticket, code})
			.execute();
		expect(signedIn.user_id).toBe(account.userId);
		expect(signedIn.token).toBeTruthy();
		expect(signedIn.device_proof).toBeTruthy();
		expect(await getUserRepository().checkIpAuthorized(createUserID(BigInt(account.userId)), STRANGER_IP)).toBe(true);
	});

	it('refuses a wrong code, kills a code after five tries and replaces an older code', async () => {
		const account = await createTestAccount(harness);
		await dropPassword(account);
		const {ticket} = await identify(account.email);
		await sendCode(ticket);
		const code = lastCode(account.email, 'sign_in_code', await listTestEmails(harness, {recipient: account.email}));
		const wrong = code === '000000' ? '111111' : '000000';
		for (let attempt = 0; attempt < 5; attempt++) {
			await createBuilderWithoutAuth(harness)
				.post('/auth/email-code/verify')
				.body({ticket, code: wrong})
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_SIGN_IN_CODE)
				.execute();
		}
		await createBuilderWithoutAuth(harness)
			.post('/auth/email-code/verify')
			.body({ticket, code})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.SIGN_IN_CODE_EXPIRED)
			.execute();
	});

	it('asks to wait before another code for the same ticket', async () => {
		const account = await createTestAccount(harness);
		await dropPassword(account);
		const {ticket} = await identify(account.email);
		await sendCode(ticket);
		await createBuilderWithoutAuth(harness)
			.post('/auth/email-code/send')
			.body({ticket})
			.expect(429, APIErrorCodes.EMAIL_CODE_RESEND_COOLDOWN)
			.execute();
	});

	it('rejects an unknown ticket', async () => {
		await createBuilderWithoutAuth(harness)
			.post('/auth/email-code/send')
			.body({ticket: 'a'.repeat(43)})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.UNKNOWN_SIGN_IN_TICKET)
			.execute();
	});

	it('creates the account only after the sign-up code with the terms accepted, with a confirmed email', async () => {
		const email = createUniqueEmail('signup');
		const {ticket, account} = await identify(email);
		expect(account).toBe('new');
		expect((await sendCode(ticket)).purpose).toBe('sign_up');
		const code = lastCode(email, 'sign_up_code', await listTestEmails(harness, {recipient: email}));
		for (const consent of [undefined, false]) {
			const refused = await createBuilderWithoutAuth(harness)
				.post('/auth/email-code/verify')
				.body(consent === undefined ? {ticket, code} : {ticket, code, consent})
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.executeWithResponse();
			expect(JSON.stringify(refused.json)).toContain('MUST_AGREE_TO_TOS_AND_PRIVACY_POLICY');
		}
		expect((await identify(email)).account).toBe('new');
		const created = await createBuilderWithoutAuth<SignInBody>(harness)
			.post('/auth/email-code/verify')
			.body({ticket, code, consent: true})
			.execute();
		const me = await createBuilder<{
			email: string;
			verified: boolean;
			claimed: boolean;
			has_password: boolean;
			date_of_birth_required: boolean;
			global_name: string | null;
		}>(harness, created.token!)
			.get('/users/@me')
			.execute();
		expect(me).toMatchObject({
			email,
			verified: true,
			claimed: true,
			has_password: false,
			date_of_birth_required: true,
			global_name: null,
		});
		const user = await getUserRepository().findUniqueAssert(createUserID(BigInt(created.user_id!)));
		expect(user.termsAgreedAt).not.toBeNull();
		expect(user.privacyAgreedAt).not.toBeNull();
		expect((await identify(email)).account).toBe('existing');
	});

	it('writes the sign-up code email in the language the client asked for', async () => {
		const swedish = getContentMessage('auth.unknown_location', 'sv-SE');
		expect(swedish).not.toBe(getContentMessage('auth.unknown_location', 'en-US'));
		for (const header of ['sv', 'sv-SE,sv;q=0.9,en;q=0.8']) {
			const email = createUniqueEmail('locale');
			const {ticket} = await identify(email);
			await createBuilderWithoutAuth(harness)
				.post('/auth/email-code/send')
				.header('x-forwarded-for', STRANGER_IP)
				.header('accept-language', header)
				.body({ticket})
				.execute();
			const sent = findLastTestEmail(await listTestEmails(harness, {recipient: email}), 'sign_up_code');
			expect(sent?.metadata['location']).toBe(swedish);
		}
	});

	it('hands MFA accounts a second-factor ticket after the code', async () => {
		const account = await createTestAccount(harness);
		const secret = createTotpSecret();
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/totp/enable')
			.body({secret, code: generateTotpCode(secret), password: account.password})
			.execute();
		await dropPassword(account);
		const {ticket} = await identify(account.email);
		await sendCode(ticket);
		const code = lastCode(account.email, 'sign_in_code', await listTestEmails(harness, {recipient: account.email}));
		const result = await createBuilderWithoutAuth<SignInBody>(harness)
			.post('/auth/email-code/verify')
			.body({ticket, code})
			.execute();
		expect(result.mfa).toBe(true);
		expect(result.token).toBeUndefined();
	});

	it('signs the inbox owner into the account whose claim holds the address, and confirms it', async () => {
		const email = createUniqueEmail('claimed-address');
		const squatter = await createBuilderWithoutAuth<{token: string; user_id: string}>(harness)
			.post('/auth/register')
			.body({username: 'squatter', consent: true, date_of_birth: '2000-01-01'})
			.execute();
		await createBuilder(harness, squatter.token).post('/users/@me/claim').body({email}).execute();
		const {ticket, account} = await identify(email);
		expect(account).toBe('existing');
		expect((await sendCode(ticket)).purpose).toBe('sign_in');
		const code = lastCode(email, 'sign_in_code', await listTestEmails(harness, {recipient: email}));
		const signedIn = await createBuilderWithoutAuth<SignInBody>(harness)
			.post('/auth/email-code/verify')
			.body({ticket, code})
			.execute();
		expect(signedIn.user_id).toBe(squatter.user_id);
		const me = await createBuilder<{email: string; verified: boolean}>(harness, signedIn.token!)
			.get('/users/@me')
			.execute();
		expect(me).toMatchObject({email, verified: true});
	});

	it('sends a password account to its password and never signs it in by code', async () => {
		const account = await createTestAccount(harness);
		const {ticket, sign_in_method} = await identify(account.email);
		expect(sign_in_method).toBe('password');
		await createBuilderWithoutAuth(harness)
			.post('/auth/email-code/send')
			.header('x-forwarded-for', STRANGER_IP)
			.body({ticket})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.PASSWORD_SIGN_IN_REQUIRED)
			.execute();
		expect(findLastTestEmail(await listTestEmails(harness, {recipient: account.email}), 'sign_in_code')).toBeNull();
		await createBuilderWithoutAuth(harness)
			.post('/auth/email-code/verify')
			.header('x-forwarded-for', STRANGER_IP)
			.body({ticket, code: '000000'})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.PASSWORD_SIGN_IN_REQUIRED)
			.execute();
	});

	it('refuses a code sent before the account set a password', async () => {
		const account = await createTestAccount(harness);
		await dropPassword(account);
		const {ticket, sign_in_method} = await identify(account.email);
		expect(sign_in_method).toBe('code');
		await sendCode(ticket);
		const code = lastCode(account.email, 'sign_in_code', await listTestEmails(harness, {recipient: account.email}));
		const user = await getUserRepository().findUniqueAssert(createUserID(BigInt(account.userId)));
		await getUserRepository().patchUpsert(user.id, {password_hash: 'set-later'}, user.toRow());
		await createBuilderWithoutAuth(harness)
			.post('/auth/email-code/verify')
			.body({ticket, code})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.PASSWORD_SIGN_IN_REQUIRED)
			.execute();
	});

	it('signs out other sessions and forgets networks when a code confirms an address', async () => {
		const account = await createTestAccount(harness);
		await dropPassword(account);
		await createBuilder(harness, '')
			.post(`/test/users/${account.userId}/security-flags`)
			.body({email_verified: false})
			.execute();
		const repository = getUserRepository();
		const {ticket} = await identify(account.email);
		await sendCode(ticket);
		const code = lastCode(account.email, 'sign_in_code', await listTestEmails(harness, {recipient: account.email}));
		const signedIn = await createBuilderWithoutAuth<SignInBody>(harness)
			.post('/auth/email-code/verify')
			.header('x-forwarded-for', STRANGER_IP)
			.body({ticket, code})
			.execute();
		await createBuilder(harness, account.token).get('/users/@me').expect(HTTP_STATUS.UNAUTHORIZED).execute();
		const me = await createBuilder<{verified: boolean}>(harness, signedIn.token!).get('/users/@me').execute();
		expect(me.verified).toBe(true);
		const user = await repository.findUniqueAssert(createUserID(BigInt(account.userId)));
		expect((await repository.getAuthorizedIps(user.id)).map((entry) => entry.ip)).toEqual([STRANGER_IP]);
	});

	it('leaves a passwordless squatter no session or window to enroll a factor after the owner proves the inbox', async () => {
		const email = createUniqueEmail('squatted');
		const squatter = await createBuilderWithoutAuth<{token: string; user_id: string}>(harness)
			.post('/auth/register')
			.body({username: 'squatter', consent: true, date_of_birth: '2000-01-01'})
			.execute();
		await createBuilder(harness, squatter.token).post('/users/@me/claim').body({email}).execute();
		const {ticket, sign_in_method} = await identify(email);
		expect(sign_in_method).toBe('code');
		await sendCode(ticket);
		const code = lastCode(email, 'sign_in_code', await listTestEmails(harness, {recipient: email}));
		const signedIn = await createBuilderWithoutAuth<SignInBody>(harness)
			.post('/auth/email-code/verify')
			.body({ticket, code})
			.execute();
		expect(signedIn.user_id).toBe(squatter.user_id);
		await createBuilder(harness, squatter.token).get('/users/@me').expect(HTTP_STATUS.UNAUTHORIZED).execute();
		const secret = createTotpSecret();
		await createBuilder(harness, squatter.token)
			.post('/users/@me/mfa/totp/enable')
			.body({secret, code: generateTotpCode(secret)})
			.expect(HTTP_STATUS.UNAUTHORIZED)
			.execute();
		await createBuilder(harness, squatter.token)
			.post('/users/@me/mfa/webauthn/credentials/registration-options')
			.body({})
			.expect(HTTP_STATUS.UNAUTHORIZED)
			.execute();
	});

	it('lets the inbox owner take back an address a squatter set a password on through a reset', async () => {
		const email = createUniqueEmail('squatted-password');
		const squatterPassword = 'a-strong-squatter-password-3';
		const squatter = await createBuilderWithoutAuth<{token: string; user_id: string}>(harness)
			.post('/auth/register')
			.body({username: 'squatter', consent: true, date_of_birth: '2000-01-01'})
			.execute();
		await createBuilder(harness, squatter.token)
			.post('/users/@me/claim')
			.body({email, password: squatterPassword})
			.execute();
		await createBuilderWithoutAuth(harness)
			.post('/auth/login')
			.header('x-forwarded-for', '192.0.2.77')
			.body({email, password: squatterPassword})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
		const approval = findLastTestEmail(await listTestEmails(harness, {recipient: email}), 'ip_authorization');
		const {sign_in_method} = await identify(email);
		expect(sign_in_method).toBe('password');
		await createBuilderWithoutAuth(harness).post('/auth/forgot').body({email}).expect(HTTP_STATUS.NO_CONTENT).execute();
		const token = findLastTestEmail(await listTestEmails(harness, {recipient: email}), 'password_reset')!.metadata[
			'token'
		];
		const ownerPassword = 'the-real-owner-password-4';
		const reset = await createBuilderWithoutAuth<{user_id: string; token: string}>(harness)
			.post('/auth/reset')
			.body({token, password: ownerPassword})
			.execute();
		expect(reset.user_id).toBe(squatter.user_id);
		await createBuilder(harness, squatter.token).get('/users/@me').expect(HTTP_STATUS.UNAUTHORIZED).execute();
		await createBuilderWithoutAuth(harness)
			.post('/auth/authorize-ip')
			.header('x-forwarded-for', '192.0.2.77')
			.body({token: approval!.metadata['token']})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
		await createBuilderWithoutAuth(harness)
			.post('/auth/login')
			.header('x-forwarded-for', '127.0.0.1')
			.body({email, password: squatterPassword})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});

	it('authorizes the network that passes the factor after a code', async () => {
		const account = await createTestAccount(harness);
		const secret = createTotpSecret();
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/totp/enable')
			.body({secret, code: generateTotpCode(secret), password: account.password})
			.execute();
		await dropPassword(account);
		const ip = '198.51.100.88';
		const {ticket} = await identify(account.email, ip);
		await sendCode(ticket, ip);
		const code = lastCode(account.email, 'sign_in_code', await listTestEmails(harness, {recipient: account.email}));
		const challenge = await createBuilderWithoutAuth<SignInBody>(harness)
			.post('/auth/email-code/verify')
			.header('x-forwarded-for', ip)
			.body({ticket, code})
			.execute();
		const user = await getUserRepository().findUniqueAssert(createUserID(BigInt(account.userId)));
		expect(await getUserRepository().checkIpAuthorized(user.id, ip)).toBe(false);
		await createBuilderWithoutAuth(harness)
			.post('/auth/login/mfa/totp')
			.header('x-forwarded-for', ip)
			.body({ticket: challenge.ticket, code: generateTotpCode(secret)})
			.execute();
		expect(await getUserRepository().checkIpAuthorized(user.id, ip)).toBe(true);
	});

	it('asks an unconfirmed owner with a second factor for it after the code', async () => {
		const account = await createTestAccount(harness);
		const secret = createTotpSecret();
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/totp/enable')
			.body({secret, code: generateTotpCode(secret), password: account.password})
			.execute();
		await dropPassword(account);
		await createBuilder(harness, '')
			.post(`/test/users/${account.userId}/security-flags`)
			.body({email_verified: false})
			.execute();
		const {ticket, account: state} = await identify(account.email);
		expect(state).toBe('existing');
		expect((await sendCode(ticket)).purpose).toBe('sign_in');
		const code = lastCode(account.email, 'sign_in_code', await listTestEmails(harness, {recipient: account.email}));
		const challenge = await createBuilderWithoutAuth<SignInBody>(harness)
			.post('/auth/email-code/verify')
			.body({ticket, code})
			.execute();
		expect(challenge.mfa).toBe(true);
		expect(challenge.token).toBeUndefined();
		const pending = await getUserRepository().findUniqueAssert(createUserID(BigInt(account.userId)));
		expect(pending).toMatchObject({email: account.email, emailVerified: false, totpSecret: secret});
		await createBuilder(harness, account.token).get('/users/@me').expect(HTTP_STATUS.OK).execute();
		const signedIn = await createBuilderWithoutAuth<{token: string}>(harness)
			.post('/auth/login/mfa/totp')
			.body({ticket: challenge.ticket, code: generateTotpCode(secret)})
			.execute();
		const me = await createBuilder<{email: string; mfa_enabled: boolean}>(harness, signedIn.token)
			.get('/users/@me')
			.execute();
		expect(me).toMatchObject({email: account.email, mfa_enabled: true});
	});

	it('leaves the email unconfirmed when the second factor never passes after the code', async () => {
		const account = await createTestAccount(harness);
		const secret = createTotpSecret();
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/totp/enable')
			.body({secret, code: generateTotpCode(secret), password: account.password})
			.execute();
		await dropPassword(account);
		await createBuilder(harness, '')
			.post(`/test/users/${account.userId}/security-flags`)
			.body({email_verified: false})
			.execute();
		const {ticket} = await identify(account.email);
		await sendCode(ticket);
		const code = lastCode(account.email, 'sign_in_code', await listTestEmails(harness, {recipient: account.email}));
		const challenge = await createBuilderWithoutAuth<SignInBody>(harness)
			.post('/auth/email-code/verify')
			.body({ticket, code})
			.execute();
		await createBuilderWithoutAuth(harness)
			.post('/auth/login/mfa/totp')
			.body({ticket: challenge.ticket, code: '000000'})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
		const user = await getUserRepository().findUniqueAssert(createUserID(BigInt(account.userId)));
		expect(user).toMatchObject({email: account.email, emailVerified: false, totpSecret: secret});
		expect(await getUserRepository().checkIpAuthorized(user.id, STRANGER_IP)).toBe(false);
	});

	it('asks a stranger for a captcha once one network looks up too many addresses', async () => {
		await useCheapCaptcha();
		await stayInsideOneLookupWindow();
		const lookups = async (count: number) => {
			for (let index = 0; index < count; index++) {
				await createBuilderWithoutAuth(harness)
					.post('/auth/identify')
					.header('x-forwarded-for', STRANGER_IP)
					.header('x-fluxer-test-enable-rate-limits', 'true')
					.header(CAPTCHA_TEST_HEADER, 'true')
					.body({email: createUniqueEmail(`spray-${index}`)})
					.execute();
			}
		};
		await lookups(10);
		const {json} = await createBuilderWithoutAuth<CaptchaErrorBody>(harness)
			.post('/auth/identify')
			.header('x-forwarded-for', STRANGER_IP)
			.header('x-fluxer-test-enable-rate-limits', 'true')
			.header(CAPTCHA_TEST_HEADER, 'true')
			.body({email: createUniqueEmail('spray-captcha')})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.CAPTCHA_REQUIRED)
			.executeWithResponse();
		const token = await solveCaptchaChallenge(json);
		await createBuilderWithoutAuth(harness)
			.post('/auth/identify')
			.header('x-forwarded-for', STRANGER_IP)
			.header('x-fluxer-test-enable-rate-limits', 'true')
			.header(CAPTCHA_TEST_HEADER, 'true')
			.header('x-captcha-token', token)
			.body({email: createUniqueEmail('spray-captcha')})
			.expect(HTTP_STATUS.OK)
			.execute();
	});

	it('keeps sending codes to the owner through a separate allowance while strangers spray the address', async () => {
		const account = await createTestAccount(harness);
		await dropPassword(account);
		const sprayFrom = async (ip: string, expectedStatus: number) => {
			const {ticket} = await createBuilderWithoutAuth<IdentifyBody>(harness)
				.post('/auth/identify')
				.header('x-forwarded-for', ip)
				.header('x-fluxer-test-enable-rate-limits', 'true')
				.body({email: account.email})
				.execute();
			await createBuilderWithoutAuth(harness)
				.post('/auth/email-code/send')
				.header('x-forwarded-for', ip)
				.header('x-fluxer-test-enable-rate-limits', 'true')
				.body({ticket})
				.expect(expectedStatus)
				.execute();
		};
		for (let index = 0; index < 5; index++) {
			await sprayFrom(`198.51.100.${index + 1}`, HTTP_STATUS.OK);
		}
		await sprayFrom('198.51.100.50', 429);
		await sprayFrom('127.0.0.1', HTTP_STATUS.OK);
	});

	function budgetEnv() {
		return {kv: getKVClient(), rateLimit: getRateLimitService(), enforce: true};
	}

	async function limitedIdentify(email: string, ip: string): Promise<IdentifyBody> {
		return await createBuilderWithoutAuth<IdentifyBody>(harness)
			.post('/auth/identify')
			.header('x-forwarded-for', ip)
			.header(ENABLE_RATE_LIMITS, 'true')
			.body({email})
			.execute();
	}

	async function limitedSend(ticket: string, ip: string, status: number, code?: string): Promise<void> {
		await createBuilderWithoutAuth(harness)
			.post('/auth/email-code/send')
			.header('x-forwarded-for', ip)
			.header(ENABLE_RATE_LIMITS, 'true')
			.body({ticket})
			.expect(status, code)
			.execute();
	}

	async function guessWrong(email: string, ticket: string, ip: string): Promise<void> {
		const code = lastCode(email, 'sign_in_code', await listTestEmails(harness, {recipient: email}));
		await createBuilderWithoutAuth(harness)
			.post('/auth/email-code/verify')
			.header('x-forwarded-for', ip)
			.body({ticket, code: ((Number(code) + 1) % 1_000_000).toString().padStart(6, '0')})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_SIGN_IN_CODE)
			.execute();
	}

	it('stops codes to an account once its own devices reach the wrong-code cap', async () => {
		const account = await createTestAccount(harness);
		await dropPassword(account);
		for (let failure = 1; failure < ACCOUNT_CODE_FAILURES_DAILY_CAP; failure++) {
			await recordAccountCodeFailure(budgetEnv(), account.userId);
		}
		const first = await limitedIdentify(account.email, '127.0.0.1');
		await limitedSend(first.ticket, '127.0.0.1', HTTP_STATUS.OK);
		await guessWrong(account.email, first.ticket, '127.0.0.1');
		for (const ip of ['127.0.0.1', STRANGER_IP]) {
			const {ticket} = await limitedIdentify(account.email, ip);
			await limitedSend(ticket, ip, 429, APIErrorCodes.EMAIL_CODE_RESEND_COOLDOWN);
		}
	});

	async function nearCodeFailureCap(email: string, ip: string): Promise<void> {
		const emailKey = normalizeEmailKey(email);
		for (let failure = 1; failure < CODE_FAILURES_DAILY_CAP; failure++) {
			await recordAddressCodeFailure(budgetEnv(), {emailKey, network: requesterNetworkKey(ip)});
		}
	}

	it('keeps a stranger network that guesses wrong codes from blocking the address for other networks', async () => {
		const account = await createTestAccount(harness);
		await dropPassword(account);
		await nearCodeFailureCap(account.email, STRANGER_IP);
		const first = await limitedIdentify(account.email, STRANGER_IP);
		await limitedSend(first.ticket, STRANGER_IP, HTTP_STATUS.OK);
		await guessWrong(account.email, first.ticket, STRANGER_IP);
		const sameNetwork = await limitedIdentify(account.email, '203.0.113.200');
		await limitedSend(sameNetwork.ticket, '203.0.113.200', 429, APIErrorCodes.EMAIL_CODE_RESEND_COOLDOWN);
		const otherNetwork = await limitedIdentify(account.email, '192.0.2.5');
		await limitedSend(otherNetwork.ticket, '192.0.2.5', HTTP_STATUS.OK);
	});

	it('counts a wrong code against the network that asked for it, wherever the guess comes from', async () => {
		const account = await createTestAccount(harness);
		await dropPassword(account);
		await nearCodeFailureCap(account.email, STRANGER_IP);
		const first = await limitedIdentify(account.email, STRANGER_IP);
		await limitedSend(first.ticket, STRANGER_IP, HTTP_STATUS.OK);
		await guessWrong(account.email, first.ticket, '198.51.100.7');
		const sameNetwork = await limitedIdentify(account.email, '203.0.113.200');
		await limitedSend(sameNetwork.ticket, '203.0.113.200', 429, APIErrorCodes.EMAIL_CODE_RESEND_COOLDOWN);
		const guessingNetwork = await limitedIdentify(account.email, '198.51.100.7');
		await limitedSend(guessingNetwork.ticket, '198.51.100.7', HTTP_STATUS.OK);
	});

	it('stops codes to strangers once wrong codes for the address add up across networks', async () => {
		const account = await createTestAccount(harness);
		await dropPassword(account);
		const emailKey = normalizeEmailKey(account.email);
		for (let failure = 1; failure < ADDRESS_CODE_FAILURES_DAILY_CAP; failure++) {
			await recordAddressCodeFailure(budgetEnv(), {emailKey, network: `10.${failure}.0.0/24`});
		}
		const first = await limitedIdentify(account.email, STRANGER_IP);
		await limitedSend(first.ticket, STRANGER_IP, HTTP_STATUS.OK);
		await guessWrong(account.email, first.ticket, STRANGER_IP);
		const fresh = await limitedIdentify(account.email, '192.0.2.5');
		await limitedSend(fresh.ticket, '192.0.2.5', 429, APIErrorCodes.EMAIL_CODE_RESEND_COOLDOWN);
		const owner = await limitedIdentify(account.email, '127.0.0.1');
		await limitedSend(owner.ticket, '127.0.0.1', HTTP_STATUS.OK);
	});

	async function verifyNeedsCaptcha(email: string, ticket: string, ip: string): Promise<number> {
		const code = lastCode(email, 'sign_in_code', await listTestEmails(harness, {recipient: email}));
		const {json} = await createBuilderWithoutAuth<CaptchaErrorBody>(harness)
			.post('/auth/email-code/verify')
			.header('x-forwarded-for', ip)
			.header(ENABLE_RATE_LIMITS, 'true')
			.header(CAPTCHA_TEST_HEADER, 'true')
			.body({ticket, code})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.CAPTCHA_REQUIRED)
			.executeWithResponse();
		const token = await solveCaptchaChallenge(json);
		const signedIn = await createBuilderWithoutAuth<SignInBody>(harness)
			.post('/auth/email-code/verify')
			.header('x-forwarded-for', ip)
			.header(ENABLE_RATE_LIMITS, 'true')
			.header(CAPTCHA_TEST_HEADER, 'true')
			.header('x-captcha-token', token)
			.body({ticket, code})
			.execute();
		expect(signedIn.token).toBeDefined();
		return (json.altcha_challenge!.parameters as {cost: number}).cost;
	}

	it('asks for a captcha before checking a code once wrong codes for the address add up across networks', async () => {
		await useCheapCaptcha();
		const account = await createTestAccount(harness);
		await dropPassword(account);
		const emailKey = normalizeEmailKey(account.email);
		for (let failure = 0; failure < ADDRESS_CODE_FAILURES_PER_STEP; failure++) {
			await recordAddressCodeFailure(budgetEnv(), {emailKey, network: `10.${failure}.0.0/24`});
		}
		const {ticket} = await limitedIdentify(account.email, STRANGER_IP);
		await limitedSend(ticket, STRANGER_IP, HTTP_STATUS.OK);
		await verifyNeedsCaptcha(account.email, ticket, STRANGER_IP);
	});

	it('asks every sign-in for one fixed captcha step once wrong codes pile up across the instance', async () => {
		await useCheapCaptcha();
		for (let failure = 0; failure < GLOBAL_CODE_FAILURES_THRESHOLD * 3; failure++) {
			await recordAccountCodeFailure(budgetEnv(), `${failure + 1}`);
		}
		const account = await createTestAccount(harness);
		await dropPassword(account);
		const {ticket} = await limitedIdentify(account.email, '127.0.0.1');
		await limitedSend(ticket, '127.0.0.1', HTTP_STATUS.OK);
		expect(await verifyNeedsCaptcha(account.email, ticket, '127.0.0.1')).toBe(2000);
	});

	it('refuses lookups past the hard limits while the captcha is off, and not while it is on', async () => {
		for (let lookup = 0; lookup < 30; lookup++) await enforceLookupHardLimits(budgetEnv(), STRANGER_IP);
		await getInstanceConfigRepository().updateCaptchaConfig({enabled: false});
		await createBuilderWithoutAuth(harness)
			.post('/auth/identify')
			.header('x-forwarded-for', STRANGER_IP)
			.header(ENABLE_RATE_LIMITS, 'true')
			.body({email: createUniqueEmail('hard-limit')})
			.expect(429, APIErrorCodes.RATE_LIMITED)
			.execute();
		await getInstanceConfigRepository().updateCaptchaConfig({enabled: true});
		await limitedIdentify(createUniqueEmail('hard-limit'), STRANGER_IP);
	});
});
