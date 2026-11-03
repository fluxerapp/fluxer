// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash, randomBytes} from 'node:crypto';
import {enforceInstantAccountLimits} from '@app/api/auth/AccountBudgets';
import {createUserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {getKVClient} from '@app/api/middleware/ServiceRegistry';
import {
	getInstanceConfigRepository,
	getRateLimitService,
	getUserRepository,
} from '@app/api/middleware/ServiceSingletons';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
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
import {SensitiveMediaFilterLevel} from '@fluxer/constants/src/UserConstants';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const MARKETING_KEY = 'marketing-test-key-0123456789abcdefghijkl';
const VISITOR_IP = '192.0.2.44';

interface RegisterBody {
	token: string;
	user_id: string;
}

function nonceWithHash(): {nonce: string; hash: string} {
	const nonce = randomBytes(32).toString('base64url');
	return {nonce, hash: createHash('sha256').update(nonce).digest('hex')};
}

describe('Username-only accounts', () => {
	let harness: ApiTestHarness;
	const originalTrustedCallers = Config.internal.trustedCallers;
	const originalRelax = Config.dev.relaxRegistrationRateLimits;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
		Config.internal.trustedCallers = [
			...originalTrustedCallers,
			{
				name: 'marketing',
				key: MARKETING_KEY,
				buckets: [
					RateLimitConfigs.AUTH_REGISTER_HANDOFF.bucket,
					RateLimitConfigs.AUTH_REGISTER_HANDOFF_CHALLENGE.bucket,
				],
			},
		];
	});
	afterEach(() => {
		Config.internal.trustedCallers = originalTrustedCallers;
		Config.dev.relaxRegistrationRateLimits = originalRelax;
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	async function registerInstant(ip = VISITOR_IP): Promise<RegisterBody> {
		return await createBuilderWithoutAuth<RegisterBody>(harness)
			.post('/auth/register')
			.header('x-forwarded-for', ip)
			.body({global_name: 'Newcomer', consent: true})
			.execute();
	}

	function handoff(globalName: string, nonceHash: string, extra: Record<string, unknown> = {}) {
		return createBuilderWithoutAuth<{handoff_code: string}>(harness)
			.post('/auth/register/handoff')
			.header('x-fluxer-internal-key', MARKETING_KEY)
			.header('x-fluxer-client-ip', VISITOR_IP)
			.header('x-forwarded-for', '10.9.9.9')
			.body({global_name: globalName, nonce_hash: nonceHash, ...extra});
	}

	async function createRegistrationUrlCode(approvalRequired: boolean): Promise<string> {
		const {code} = await getInstanceConfigRepository().createRegistrationUrl({
			label: null,
			createdByUserId: '1',
			expiresAt: null,
			maxUses: null,
			approvalRequired,
		});
		return code;
	}

	async function fetchFeatures(): Promise<{instant_accounts: boolean; instant_account_availability?: string}> {
		const discovery = await createBuilderWithoutAuth<{
			features: {instant_accounts: boolean; instant_account_availability?: string};
		}>(harness)
			.get('/.well-known/fluxer')
			.execute();
		return discovery.features;
	}

	function redeem(handoffCode: string, nonce: string, extra: Record<string, unknown> = {consent: true}) {
		return createBuilderWithoutAuth<{token: string; user_id: string; device_proof: string}>(harness)
			.post('/auth/register/handoff/redeem')
			.body({handoff_code: handoffCode, nonce, ...extra});
	}

	async function expectConsentRefused(builder: ReturnType<typeof createBuilderWithoutAuth>) {
		const refused = await builder.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY').executeWithResponse();
		expect(JSON.stringify(refused.json)).toContain('MUST_AGREE_TO_TOS_AND_PRIVACY_POLICY');
	}

	it('creates an unclaimed account that must enter a date of birth', async () => {
		const created = await registerInstant();
		const me = await createBuilder<{claimed: boolean; has_password: boolean; date_of_birth_required: boolean}>(
			harness,
			created.token,
		)
			.get('/users/@me')
			.execute();
		expect(me).toEqual(expect.objectContaining({claimed: false, has_password: false, date_of_birth_required: true}));
		const settings = await createBuilder<{sensitive_content_friend_dm_filter: number}>(harness, created.token)
			.get('/users/@me/settings')
			.execute();
		expect(settings.sensitive_content_friend_dm_filter).toBe(SensitiveMediaFilterLevel.BLUR);
	});

	it('refuses a username-only account without accepting the terms', async () => {
		for (const body of [{global_name: 'Newcomer'}, {global_name: 'Newcomer', consent: false}]) {
			await expectConsentRefused(createBuilderWithoutAuth(harness).post('/auth/register').body(body));
		}
		expect(await getUserRepository().findUsersByUsername('Newcomer')).toEqual([]);
	});

	it('refuses username-only accounts while the switch is off', async () => {
		await getInstanceConfigRepository().updateAuthConfig({instant_accounts: {enabled: false}});
		await createBuilderWithoutAuth(harness)
			.post('/auth/register')
			.body({global_name: 'Newcomer', consent: true})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.INSTANT_ACCOUNTS_DISABLED)
			.execute();
	});

	it('limits username-only accounts per network without the older three per hour rule', async () => {
		Config.dev.relaxRegistrationRateLimits = false;
		for (let index = 0; index < 5; index++) {
			await registerInstant();
		}
		await createBuilderWithoutAuth(harness)
			.post('/auth/register')
			.header('x-forwarded-for', VISITOR_IP)
			.body({global_name: 'Newcomer', consent: true})
			.expect(429)
			.execute();
		await registerInstant('192.0.2.99');
	});

	it('raises the captcha cost with every account from the same network', async () => {
		await useCheapCaptcha();
		const askChallenge = async () =>
			(
				await createBuilderWithoutAuth<CaptchaErrorBody>(harness)
					.post('/auth/register')
					.header('x-forwarded-for', VISITOR_IP)
					.header(CAPTCHA_TEST_HEADER, 'true')
					.body({global_name: 'Newcomer', consent: true})
					.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.CAPTCHA_REQUIRED)
					.executeWithResponse()
			).json;
		const first = await askChallenge();
		const firstCost = (first.altcha_challenge!.parameters as {cost: number}).cost;
		const token = await solveCaptchaChallenge(first);
		await createBuilderWithoutAuth(harness)
			.post('/auth/register')
			.header('x-forwarded-for', VISITOR_IP)
			.header(CAPTCHA_TEST_HEADER, 'true')
			.header('x-captcha-token', token)
			.body({global_name: 'Newcomer', consent: true})
			.expect(HTTP_STATUS.OK)
			.execute();
		const second = await askChallenge();
		expect((second.altcha_challenge!.parameters as {cost: number}).cost).toBe(firstCost * 2);
		const cheapToken = await solveCaptchaChallenge(first);
		await createBuilderWithoutAuth(harness)
			.post('/auth/register')
			.header('x-forwarded-for', VISITOR_IP)
			.header(CAPTCHA_TEST_HEADER, 'true')
			.header('x-captcha-token', cheapToken)
			.body({global_name: 'Newcomer', consent: true})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_CAPTCHA)
			.execute();
	});

	it('creates the website account only when the browser redeems it with the terms accepted', async () => {
		const {nonce, hash} = nonceWithHash();
		const {handoff_code} = await handoff('Web Visitor', hash).execute();
		expect(handoff_code).toHaveLength(43);
		expect(await getUserRepository().findUsersByUsername('Web_Visitor')).toEqual([]);
		await expectConsentRefused(redeem(handoff_code, nonce, {}));
		await expectConsentRefused(redeem(handoff_code, nonce, {consent: false}));
		const redeemed = await redeem(handoff_code, nonce).execute();
		expect(redeemed.token).toBeTruthy();
		expect(redeemed.device_proof).toBeTruthy();
		const user = await getUserRepository().findUniqueAssert(createUserID(BigInt(redeemed.user_id)));
		expect(user.username).toBe('Web_Visitor');
		expect(user.globalName).toBe('Web Visitor');
		expect(user.lastActiveIp).toBe(VISITOR_IP);
		expect(user.termsAgreedAt).not.toBeNull();
		expect(user.privacyAgreedAt).not.toBeNull();
		await redeem(handoff_code, nonce).expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.UNKNOWN_ACCOUNT_HANDOFF).execute();
	});

	it('starts every website account at the date of birth step', async () => {
		const {nonce, hash} = nonceWithHash();
		const {handoff_code} = await handoff('Web Visitor', hash).execute();
		const redeemed = await redeem(handoff_code, nonce, {consent: true}).execute();
		const me = await createBuilder<{date_of_birth_required: boolean}>(harness, redeemed.token)
			.get('/users/@me')
			.execute();
		expect(me.date_of_birth_required).toBe(true);
	});

	it('refuses a website handoff for a display name that is not allowed', async () => {
		const {hash} = nonceWithHash();
		for (const name of ['everyone', ' ']) {
			await handoff(name, hash).expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY').execute();
		}
	});

	it('keeps the code when the account cannot be made yet, and uses it up once it is', async () => {
		const {nonce, hash} = nonceWithHash();
		const {handoff_code} = await handoff('Web Visitor', hash).execute();
		Config.dev.relaxRegistrationRateLimits = false;
		const env = {kv: getKVClient(), rateLimit: getRateLimitService(), enforce: true};
		for (let attempt = 0; attempt < 5; attempt++) await enforceInstantAccountLimits(env, VISITOR_IP);
		await redeem(handoff_code, nonce).expect(429).execute();
		Config.dev.relaxRegistrationRateLimits = true;
		const redeemed = await redeem(handoff_code, nonce).execute();
		expect(redeemed.user_id).toBeDefined();
		await redeem(handoff_code, nonce).expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.UNKNOWN_ACCOUNT_HANDOFF).execute();
	});

	it('uses up the code on a wrong nonce', async () => {
		const {nonce, hash} = nonceWithHash();
		const {handoff_code} = await handoff('Web Visitor', hash).execute();
		await redeem(handoff_code, nonceWithHash().nonce)
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_ACCOUNT_HANDOFF_NONCE)
			.execute();
		await redeem(handoff_code, nonce).expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.UNKNOWN_ACCOUNT_HANDOFF).execute();
	});

	it('refuses callers without the website key', async () => {
		const {hash} = nonceWithHash();
		await createBuilderWithoutAuth(harness)
			.post('/auth/register/handoff')
			.header('x-fluxer-client-ip', VISITOR_IP)
			.body({global_name: 'Web Visitor', nonce_hash: hash})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_ACCESS)
			.execute();
	});

	it('lets a native app with no Origin header redeem the handoff on the hosted instance', async () => {
		expect(Config.instance.selfHosted).toBe(false);
		const {nonce, hash} = nonceWithHash();
		const {handoff_code} = await handoff('App Visitor', hash).execute();
		const redeemed = await createBuilderWithoutAuth<RegisterBody>(harness)
			.post('/auth/register/handoff/redeem')
			.body({handoff_code, nonce, consent: true})
			.execute();
		const user = await getUserRepository().findUniqueAssert(createUserID(BigInt(redeemed.user_id)));
		expect(user.globalName).toBe('App Visitor');
		await createBuilder(harness, redeemed.token).get('/users/@me').execute();
	});

	it('stops advertising instant accounts while sign-ups need approval', async () => {
		expect(await fetchFeatures()).toMatchObject({instant_accounts: true, instant_account_availability: 'available'});
		await getInstanceConfigRepository().setRegistrationConfig({mode: 'approval'});
		expect(await fetchFeatures()).toMatchObject({
			instant_accounts: false,
			instant_account_availability: 'approval_required',
		});
		await createBuilderWithoutAuth(harness)
			.post('/auth/register')
			.body({global_name: 'Newcomer', consent: true})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.REGISTRATION_CREDENTIALS_REQUIRED)
			.execute();
	});

	it('reports the captcha being off as the reason instant accounts are unavailable', async () => {
		await getInstanceConfigRepository().updateCaptchaConfig({provider: 'none'});
		expect(await fetchFeatures()).toMatchObject({instant_accounts: false, instant_account_availability: 'captcha_off'});
	});

	it('lets a registration URL without approval admit instant accounts while sign-ups need approval', async () => {
		await getInstanceConfigRepository().setRegistrationConfig({mode: 'approval'});
		const code = await createRegistrationUrlCode(false);
		const created = await createBuilderWithoutAuth<RegisterBody>(harness)
			.post('/auth/register')
			.body({global_name: 'Invited', consent: true, registration_url_code: code})
			.execute();
		expect(created.token).toBeTruthy();
	});

	it('takes a registration URL without approval on the website handoff while sign-ups are closed', async () => {
		await getInstanceConfigRepository().setRegistrationConfig({mode: 'closed'});
		const {nonce, hash} = nonceWithHash();
		await handoff('Web Visitor', hash).expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.REGISTRATION_CLOSED).execute();
		const approvalCode = await createRegistrationUrlCode(true);
		await handoff('Web Visitor', hash, {registration_url_code: approvalCode})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.INSTANT_ACCOUNTS_DISABLED)
			.execute();
		const code = await createRegistrationUrlCode(false);
		const {handoff_code} = await handoff('Web Visitor', hash, {registration_url_code: code}).execute();
		const redeemed = await redeem(handoff_code, nonce).execute();
		const user = await getUserRepository().findUniqueAssert(createUserID(BigInt(redeemed.user_id)));
		expect(user.globalName).toBe('Web Visitor');
	});

	it('hands the website a challenge for the visitor', async () => {
		await useCheapCaptcha();
		const challenge = await createBuilderWithoutAuth<{captcha_provider: string}>(harness)
			.get('/auth/register/handoff/challenge')
			.header('x-fluxer-internal-key', MARKETING_KEY)
			.header('x-fluxer-client-ip', VISITOR_IP)
			.header(CAPTCHA_TEST_HEADER, 'true')
			.execute();
		expect(challenge.captcha_provider).toBe('altcha');
	});
});
