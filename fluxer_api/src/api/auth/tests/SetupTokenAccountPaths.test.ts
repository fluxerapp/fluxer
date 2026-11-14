// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash, randomBytes} from 'node:crypto';
import {
	clearTestEmails,
	createUniqueEmail,
	createUniqueUsername,
	disableSso,
	enableSso,
	findLastTestEmail,
	listTestEmails,
} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {getInstanceConfigRepository, getUserRepository} from '@app/api/middleware/ServiceSingletons';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const SETUP_TOKEN = 'setup-token-for-tests-0123456789-abcdefghijklmnopqrstuvwxyz';
const MARKETING_KEY = 'marketing-test-key-0123456789abcdefghijkl';
const VISITOR_IP = '192.0.2.44';

interface GateResponse {
	user_id?: string;
	token?: string;
	ticket?: string;
	purpose?: string;
	handoff_code?: string;
	code?: string;
	errors?: Array<{path: string; code: string}>;
}

describe('Setup token on every way to create an account', () => {
	let harness: ApiTestHarness;
	const originalSelfHosted = Config.instance.selfHosted;
	const originalSetupToken = Config.instance.setup.token;
	const originalTrustedCallers = Config.internal.trustedCallers;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
		await clearTestEmails(harness);
		const repository = getInstanceConfigRepository();
		await repository.updateCaptchaConfig({enabled: true});
		await repository.updateAuthConfig({identifier_first: true, instant_accounts: {enabled: true}});
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
		Config.instance.selfHosted = originalSelfHosted;
		Config.instance.setup.token = originalSetupToken;
		Config.internal.trustedCallers = originalTrustedCallers;
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	function openSetupTokenGate(): void {
		Config.instance.selfHosted = true;
		Config.instance.setup.token = SETUP_TOKEN;
	}

	async function post(
		path: string,
		body: Record<string, unknown>,
		headers: Record<string, string> = {},
	): Promise<{status: number; json: GateResponse}> {
		const builder = createBuilderWithoutAuth<GateResponse>(harness).post(path).body(body);
		for (const [name, value] of Object.entries(headers)) builder.header(name, value);
		const {response, json} = await builder.executeRaw();
		return {status: response.status, json};
	}

	function expectSetupTokenRequired({status, json}: {status: number; json: GateResponse}): void {
		expect(status).toBe(HTTP_STATUS.BAD_REQUEST);
		expect((json.errors ?? []).map(({path, code}) => ({path, code}))).toEqual([
			{path: 'setup_token', code: ValidationErrorCodes.SETUP_TOKEN_REQUIRED},
		]);
	}

	function handoffHeaders(): Record<string, string> {
		return {
			'x-fluxer-internal-key': MARKETING_KEY,
			'x-fluxer-client-ip': VISITOR_IP,
			'x-forwarded-for': '10.9.9.9',
		};
	}

	function nonceWithHash(): {nonce: string; hash: string} {
		const nonce = randomBytes(32).toString('base64url');
		return {nonce, hash: createHash('sha256').update(nonce).digest('hex')};
	}

	async function identify(email: string): Promise<string> {
		const {status, json} = await post('/auth/identify', {email});
		expect(status).toBe(HTTP_STATUS.OK);
		return json.ticket!;
	}

	async function signUpCode(email: string): Promise<string> {
		const record = findLastTestEmail(await listTestEmails(harness, {recipient: email}), 'sign_up_code');
		expect(record).not.toBeNull();
		return record!.metadata['code']!;
	}

	function registerWithSetupToken(): Promise<{status: number; json: GateResponse}> {
		return post('/auth/register', {
			email: createUniqueEmail('setupadmin'),
			username: createUniqueUsername('setupadmin'),
			global_name: 'Setup Admin',
			date_of_birth: '2000-01-01',
			consent: true,
			setup_token: SETUP_TOKEN,
		});
	}

	async function registerAdmin(): Promise<string> {
		const {status, json} = await registerWithSetupToken();
		expect(status).toBe(HTTP_STATUS.OK);
		return json.user_id!;
	}

	async function completeSso(prefix: string): Promise<{status: number; json: GateResponse}> {
		const started = await post('/auth/sso/start', {});
		expect(started.status).toBe(HTTP_STATUS.OK);
		const {state, client_binding} = started.json as GateResponse & {state?: string; client_binding?: string};
		return post('/auth/sso/complete', {code: `${prefix}-${Date.now()}@example.com`, state, client_binding});
	}

	it('sends no sign-up code before the first admin exists', async () => {
		openSetupTokenGate();
		const email = createUniqueEmail('gatedcode');
		const ticket = await identify(email);
		expectSetupTokenRequired(await post('/auth/email-code/send', {ticket}));
		expect(await listTestEmails(harness, {recipient: email})).toEqual([]);
	});

	it('creates no account from a sign-up code that was sent before the gate applied', async () => {
		const email = createUniqueEmail('earlycode');
		const ticket = await identify(email);
		expect((await post('/auth/email-code/send', {ticket})).status).toBe(HTTP_STATUS.OK);
		const code = await signUpCode(email);
		openSetupTokenGate();
		expectSetupTokenRequired(await post('/auth/email-code/verify', {ticket, code, consent: true}));
		expect(await getUserRepository().findByEmail(email)).toBeNull();
		await expect(getInstanceConfigRepository().isAdminBootstrapped()).resolves.toBe(false);
	});

	it('creates no account without an email before the first admin exists', async () => {
		openSetupTokenGate();
		expectSetupTokenRequired(await post('/auth/register', {global_name: 'Newcomer', consent: true}));
		expect(await getUserRepository().findUsersByUsername('Newcomer')).toEqual([]);
	});

	it('starts no account from the website before the first admin exists', async () => {
		openSetupTokenGate();
		const {hash} = nonceWithHash();
		expectSetupTokenRequired(
			await post('/auth/register/handoff', {global_name: 'Newcomer', nonce_hash: hash}, handoffHeaders()),
		);
	});

	it('creates no account from a website handoff that was started before the gate applied', async () => {
		const {nonce, hash} = nonceWithHash();
		const started = await post('/auth/register/handoff', {global_name: 'Newcomer', nonce_hash: hash}, handoffHeaders());
		expect(started.status).toBe(HTTP_STATUS.OK);
		openSetupTokenGate();
		expectSetupTokenRequired(
			await post('/auth/register/handoff/redeem', {handoff_code: started.json.handoff_code, nonce, consent: true}),
		);
		expect(await getUserRepository().findUsersByUsername('Newcomer')).toEqual([]);
	});

	it('opens every way to create an account once the setup token holder is the admin', async () => {
		openSetupTokenGate();
		const adminId = await registerAdmin();
		const admin = await getUserRepository().findUniqueAssert(createUserID(BigInt(adminId)));
		expect([...admin.acls]).toEqual([AdminACLs.WILDCARD]);
		const email = createUniqueEmail('aftersetup');
		const ticket = await identify(email);
		const beforeFinish = await post('/auth/email-code/send', {ticket});
		expect(beforeFinish.status).toBe(HTTP_STATUS.FORBIDDEN);
		expect(beforeFinish.json.code).toBe(APIErrorCodes.REGISTRATION_CLOSED);
		expect((await post('/auth/register', {global_name: 'Early', consent: true})).json.code).toBe(
			APIErrorCodes.REGISTRATION_CLOSED,
		);
		await getInstanceConfigRepository().setAppPublicConfig({setup: {configured: true}});
		expect((await post('/auth/email-code/send', {ticket})).status).toBe(HTTP_STATUS.OK);
		const signedUp = await post('/auth/email-code/verify', {ticket, code: await signUpCode(email), consent: true});
		expect(signedUp.status).toBe(HTTP_STATUS.OK);
		const emailUser = await getUserRepository().findUniqueAssert(createUserID(BigInt(signedUp.json.user_id!)));
		expect([...emailUser.acls]).toEqual([]);
		const instant = await post('/auth/register', {global_name: 'Newcomer', consent: true});
		expect(instant.status).toBe(HTTP_STATUS.OK);
		const {nonce, hash} = nonceWithHash();
		const started = await post('/auth/register/handoff', {global_name: 'Visitor', nonce_hash: hash}, handoffHeaders());
		expect(started.status).toBe(HTTP_STATUS.OK);
		const redeemed = await post('/auth/register/handoff/redeem', {
			handoff_code: started.json.handoff_code,
			nonce,
			consent: true,
		});
		expect(redeemed.status).toBe(HTTP_STATUS.OK);
	});

	it('lets the setup token create only one account when two sign-ups race', async () => {
		openSetupTokenGate();
		const results = await Promise.all([registerWithSetupToken(), registerWithSetupToken()]);
		expect(results.map(({status}) => status).sort()).toEqual([HTTP_STATUS.OK, HTTP_STATUS.FORBIDDEN]);
		expect(results.find(({status}) => status === HTTP_STATUS.FORBIDDEN)?.json.code).toBe(
			APIErrorCodes.REGISTRATION_CLOSED,
		);
	});

	it('creates no SSO account until setup finishes', async () => {
		openSetupTokenGate();
		const admin = await registerWithSetupToken();
		expect(admin.status).toBe(HTTP_STATUS.OK);
		await enableSso(harness, admin.json.token!, {enforced: false});
		try {
			const beforeFinish = await completeSso('ssobeforesetup');
			expect(beforeFinish.status).toBe(HTTP_STATUS.FORBIDDEN);
			expect(beforeFinish.json.code).toBe(APIErrorCodes.REGISTRATION_CLOSED);
			await getInstanceConfigRepository().setAppPublicConfig({setup: {configured: true}});
			expect((await completeSso('ssoaftersetup')).status).toBe(HTTP_STATUS.OK);
		} finally {
			await disableSso(harness, admin.json.token!);
		}
	});
});
