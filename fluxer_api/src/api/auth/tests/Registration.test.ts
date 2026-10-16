// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import {
	createAuthHarness,
	createUniqueEmail,
	createUniqueUsername,
	fetchMe,
	type LoginSuccessResponse,
	registerUser,
	titleCaseEmail,
	type UserMeResponse,
} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {getConfig} from '@app/api/Config';
import {applySharedListUpdate, resetSharedListsForTests} from '@app/api/infrastructure/activity/SharedLists';
import {
	getInstanceConfigRepository,
	getRateLimitService,
	getUserRepository,
} from '@app/api/middleware/ServiceSingletons';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {getSameIpDecisionKey} from '@fluxer/ip_utils/src/IpAddress';
import {ms} from 'itty-time';
import {afterAll, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

function bootstrapRegistrationBody(prefix: string): Record<string, unknown> {
	return {
		email: createUniqueEmail(prefix),
		username: createUniqueUsername(prefix),
		global_name: 'Bootstrap Admin',
		date_of_birth: '2000-01-01',
		consent: true,
	};
}

function bootstrapRegistrationBodyWithDnsEmail(prefix: string): Record<string, unknown> {
	return {
		...bootstrapRegistrationBody(prefix),
		email: `${prefix}-${randomUUID()}@gmail.com`,
	};
}

async function withBootstrapAdminConfig(
	configOverrides: {
		selfHosted: boolean;
		testModeEnabled: boolean;
	},
	callback: () => Promise<void>,
): Promise<void> {
	const config = getConfig();
	const originalSelfHosted = config.instance.selfHosted;
	const originalTestModeEnabled = config.dev.testModeEnabled;
	try {
		config.instance.selfHosted = configOverrides.selfHosted;
		config.dev.testModeEnabled = configOverrides.testModeEnabled;
		await callback();
	} finally {
		config.instance.selfHosted = originalSelfHosted;
		config.dev.testModeEnabled = originalTestModeEnabled;
	}
}

const SETUP_TOKEN = 'setup-token-for-tests-0123456789-abcdefghijklmnopqrstuvwxyz';
const WRONG_SETUP_TOKEN = 'wrong-setup-token-0123456789-abcdefghijklmnopqrstuvwxyz';
const SETUP_TOKEN_HASH_KEY = 'setup_token_hash';
const TOO_MANY_REQUESTS = 429;

interface SetupTokenRegistrationResponse {
	user_id?: string;
	token?: string;
	registration_pending_approval?: boolean;
	code?: string;
	errors?: Array<{path: string; code: string}>;
}

async function withSetupTokenInstance(callback: () => Promise<void>): Promise<void> {
	const config = getConfig();
	const originalSelfHosted = config.instance.selfHosted;
	const originalSetupToken = config.instance.setup.token;
	const originalSetupConfigured = config.instance.setup.configured;
	const originalRelaxRegistrationRateLimits = config.dev.relaxRegistrationRateLimits;
	try {
		config.instance.selfHosted = true;
		config.instance.setup.token = SETUP_TOKEN;
		await callback();
	} finally {
		config.instance.selfHosted = originalSelfHosted;
		config.instance.setup.token = originalSetupToken;
		config.instance.setup.configured = originalSetupConfigured;
		config.dev.relaxRegistrationRateLimits = originalRelaxRegistrationRateLimits;
	}
}

async function expectUserACLs(userId: string, expectedACLs: Array<string>): Promise<void> {
	const user = await getUserRepository().findUniqueAssert(createUserID(BigInt(userId)));
	expect([...user.acls].sort()).toEqual([...expectedACLs].sort());
}

describe('Auth registration', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createAuthHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	it('returns token and user_id', async () => {
		const email = createUniqueEmail('register');
		const reg = await registerUser(harness, {
			email,
			username: createUniqueUsername('register'),
			global_name: 'Register User',
			password: 'a-strong-password',
			date_of_birth: '2000-01-01',
			consent: true,
		});
		expect(reg.token.length).toBeGreaterThan(0);
		expect(reg.user_id.length).toBeGreaterThan(0);
		expect(Object.keys(reg).sort()).toEqual(['token', 'user', 'user_id']);
	});
	it('does not flag accounts registered on a development instance as staff', async () => {
		expect(getConfig().nodeEnv).toBe('development');
		const reg = await registerUser(harness, {
			email: createUniqueEmail('devnotstaff'),
			username: createUniqueUsername('devnotstaff'),
			global_name: 'Dev User',
			password: 'a-strong-password',
			date_of_birth: '2000-01-01',
			consent: true,
		});
		const user = await getUserRepository().findUniqueAssert(createUserID(BigInt(reg.user_id)));
		expect(user.flags & UserFlags.STAFF).toBe(0n);
	});
	it('grants wildcard admin ACL to first accepted local dev registration', async () => {
		await getInstanceConfigRepository().updateCaptchaConfig({enabled: false});
		await withBootstrapAdminConfig({selfHosted: false, testModeEnabled: false}, async () => {
			const first = await registerUser(harness, bootstrapRegistrationBodyWithDnsEmail('localdevadminone'));
			const second = await registerUser(harness, bootstrapRegistrationBodyWithDnsEmail('localdevadmintwo'));
			await expectUserACLs(first.user_id, [AdminACLs.WILDCARD]);
			await expectUserACLs(second.user_id, []);
			await expect(getInstanceConfigRepository().isAdminBootstrapped()).resolves.toBe(true);
		});
	});
	it('does not grant local dev bootstrap admin ACL in test mode', async () => {
		await withBootstrapAdminConfig({selfHosted: false, testModeEnabled: true}, async () => {
			const account = await registerUser(harness, bootstrapRegistrationBody('testmodeadmin'));
			await expectUserACLs(account.user_id, []);
			await expect(getInstanceConfigRepository().isAdminBootstrapped()).resolves.toBe(false);
		});
	});
	it('grants wildcard admin ACL to first accepted self-hosted registration', async () => {
		await withBootstrapAdminConfig({selfHosted: true, testModeEnabled: true}, async () => {
			const first = await registerUser(harness, bootstrapRegistrationBody('selfhostadminone'));
			const second = await registerUser(harness, bootstrapRegistrationBody('selfhostadmintwo'));
			await expectUserACLs(first.user_id, [AdminACLs.WILDCARD]);
			await expectUserACLs(second.user_id, []);
			await expect(getInstanceConfigRepository().isAdminBootstrapped()).resolves.toBe(true);
		});
	});
	it('grants wildcard admin ACL to first accepted unconfigured setup registration', async () => {
		await withBootstrapAdminConfig({selfHosted: false, testModeEnabled: true}, async () => {
			await getInstanceConfigRepository().setAppPublicConfig({setup: {configured: false}});
			const first = await registerUser(harness, bootstrapRegistrationBody('setupopenadminone'));
			const second = await registerUser(harness, bootstrapRegistrationBody('setupopenadmintwo'));
			await expectUserACLs(first.user_id, [AdminACLs.WILDCARD]);
			await expectUserACLs(second.user_id, []);
			await expect(getInstanceConfigRepository().isAdminBootstrapped()).resolves.toBe(true);
		});
	});
	it('lets exactly one concurrent caller claim the admin bootstrap', async () => {
		const instanceConfigRepository = getInstanceConfigRepository();
		const claims = await Promise.all(Array.from({length: 5}, () => instanceConfigRepository.claimAdminBootstrap()));
		expect(claims.filter(Boolean)).toHaveLength(1);
		await expect(instanceConfigRepository.isAdminBootstrapped()).resolves.toBe(true);
		await expect(instanceConfigRepository.claimAdminBootstrap()).resolves.toBe(false);
	});
	it('keeps instance config closed to sessions without admin ACLs while setup is open', async () => {
		await withBootstrapAdminConfig({selfHosted: false, testModeEnabled: true}, async () => {
			const instanceConfigRepository = getInstanceConfigRepository();
			await instanceConfigRepository.setAppPublicConfig({setup: {configured: false}});
			await instanceConfigRepository.claimAdminBootstrap();
			const account = await registerUser(harness, bootstrapRegistrationBody('setupopennonadmin'));
			await expectUserACLs(account.user_id, []);
			await createBuilder(harness, account.token).get('/admin/instance/config').expect(HTTP_STATUS.FORBIDDEN).execute();
			await createBuilder(harness, account.token)
				.patch('/admin/instance/config')
				.body({app_public: {setup: {configured: true}}})
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
			await expectUserACLs(account.user_id, []);
			await expect(instanceConfigRepository.getAppPublicConfig()).resolves.toMatchObject({
				setup: {configured: false},
			});
		});
	});
	it('lets the bootstrap admin complete setup', async () => {
		await withBootstrapAdminConfig({selfHosted: false, testModeEnabled: true}, async () => {
			const instanceConfigRepository = getInstanceConfigRepository();
			await instanceConfigRepository.setAppPublicConfig({setup: {configured: false}});
			const account = await registerUser(harness, bootstrapRegistrationBody('setupopenowner'));
			await expectUserACLs(account.user_id, [AdminACLs.WILDCARD]);
			await createBuilder(harness, account.token)
				.patch('/admin/instance/config')
				.body({app_public: {setup: {configured: true}}})
				.execute();
			await expect(instanceConfigRepository.getAppPublicConfig()).resolves.toMatchObject({
				setup: {configured: true},
			});
		});
	});
	it('derives username from display name when username is omitted', async () => {
		const reg = await registerUser(harness, {
			email: createUniqueEmail('derived-username'),
			password: 'a-strong-password',
			global_name: 'Magic Tester',
			date_of_birth: '2000-01-01',
			consent: true,
		});
		const me = (await fetchMe(harness, reg.token)).json as UserMeResponse;
		expect(me.username).toBe('Magic_Tester');
	});
	it('blocks any request from an address on the shared blocked list', async () => {
		applySharedListUpdate('ip_blocked', '127.0.0.1\n');
		try {
			await createBuilderWithoutAuth(harness)
				.post('/auth/register')
				.body({
					email: createUniqueEmail('blocked-register'),
					username: createUniqueUsername('blockedregister'),
					global_name: 'Blocked Register',
					password: 'a-strong-password',
					date_of_birth: '2000-01-01',
					consent: true,
				})
				.expect(403, 'GLOBAL_IP_BANNED')
				.execute();
		} finally {
			resetSharedListsForTests();
		}
	});
	it('treats email as case-insensitive across auth flows', async () => {
		const baseEmail = 'Integration-Test-Case-Email@Example.COM';
		const password = 'a-strong-password';
		await registerUser(harness, {
			email: baseEmail,
			username: createUniqueUsername('caseuser'),
			global_name: 'Test User',
			password,
			date_of_birth: '2000-01-01',
			consent: true,
		});
		const loginEmails = [baseEmail.toLowerCase(), baseEmail.toUpperCase(), titleCaseEmail(baseEmail)];
		for (const email of loginEmails) {
			const login = await createBuilderWithoutAuth<LoginSuccessResponse>(harness)
				.post('/auth/login')
				.body({email, password})
				.execute();
			expect(login.token.length).toBeGreaterThan(0);
		}
		const duplicateJson = await createBuilderWithoutAuth<{
			code: string;
			errors: Array<{
				path: string;
				message: string;
			}>;
		}>(harness)
			.post('/auth/register')
			.body({
				email: baseEmail.toUpperCase(),
				username: createUniqueUsername('caseuser2'),
				global_name: 'Test User',
				password: 'another-strong-password',
				date_of_birth: '2000-01-01',
				consent: true,
			})
			.expect(400)
			.execute();
		expect(duplicateJson.code).toBe('INVALID_FORM_BODY');
		const emailError = duplicateJson.errors.find((e) => e.path === 'email');
		expect(emailError?.message).toBe('Email is already in use.');
		await createBuilderWithoutAuth(harness)
			.post('/auth/forgot')
			.body({email: baseEmail.toUpperCase()})
			.expect(204)
			.execute();
		const caseEmailUser = await registerUser(harness, {
			email: 'integration-case-store-email@example.com',
			username: createUniqueUsername('caseemailstored'),
			global_name: 'Stored Email',
			password: 'a-strong-password',
			date_of_birth: '2000-01-01',
			consent: true,
		});
		const me = (await fetchMe(harness, caseEmailUser.token)).json as UserMeResponse;
		expect(me.email).toBe('integration-case-store-email@example.com');
	});
});

describe('Setup token registration', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createAuthHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	async function register(
		body: Record<string, unknown>,
		ipAddress?: string,
	): Promise<{status: number; json: SetupTokenRegistrationResponse}> {
		const builder = createBuilderWithoutAuth<SetupTokenRegistrationResponse>(harness).post('/auth/register').body(body);
		if (ipAddress) builder.header('x-forwarded-for', ipAddress);
		const {response, json} = await builder.executeRaw();
		return {status: response.status, json};
	}

	function setupTokenErrors(json: SetupTokenRegistrationResponse): Array<{path: string; code: string}> {
		return (json.errors ?? []).map(({path, code}) => ({path, code}));
	}

	async function registerAdmin(prefix: string, ipAddress?: string): Promise<string> {
		const {status, json} = await register({...bootstrapRegistrationBody(prefix), setup_token: SETUP_TOKEN}, ipAddress);
		expect(status).toBe(HTTP_STATUS.OK);
		return json.user_id!;
	}

	it('refuses a registration that sends no setup token', async () => {
		await withSetupTokenInstance(async () => {
			const body = bootstrapRegistrationBody('tokenless');
			const {status, json} = await register(body);
			expect(status).toBe(HTTP_STATUS.BAD_REQUEST);
			expect(setupTokenErrors(json)).toEqual([{path: 'setup_token', code: ValidationErrorCodes.SETUP_TOKEN_REQUIRED}]);
			expect(await getUserRepository().findByEmail(body.email as string)).toBeNull();
			await expect(getInstanceConfigRepository().isAdminBootstrapped()).resolves.toBe(false);
		});
	});

	it('refuses a registration that sends a wrong setup token', async () => {
		await withSetupTokenInstance(async () => {
			const body = bootstrapRegistrationBody('wrongtoken');
			const {status, json} = await register({...body, setup_token: WRONG_SETUP_TOKEN});
			expect(status).toBe(HTTP_STATUS.BAD_REQUEST);
			expect(setupTokenErrors(json)).toEqual([{path: 'setup_token', code: ValidationErrorCodes.SETUP_TOKEN_INVALID}]);
			expect(await getUserRepository().findByEmail(body.email as string)).toBeNull();
			await expect(getInstanceConfigRepository().isAdminBootstrapped()).resolves.toBe(false);
		});
	});

	it('makes the registration that presents the setup token the admin', async () => {
		await withSetupTokenInstance(async () => {
			const userId = await registerAdmin('tokenadmin');
			await expectUserACLs(userId, [AdminACLs.WILDCARD]);
			await expect(getInstanceConfigRepository().isAdminBootstrapped()).resolves.toBe(true);
			expect((await getInstanceConfigRepository().getConfig(SETUP_TOKEN_HASH_KEY)) ?? '').toBe('');
		});
	});

	it('accepts a setup token padded with whitespace', async () => {
		await withSetupTokenInstance(async () => {
			const {status, json} = await register({
				...bootstrapRegistrationBody('paddedtoken'),
				setup_token: `  ${SETUP_TOKEN}\n`,
			});
			expect(status).toBe(HTTP_STATUS.OK);
			await expectUserACLs(json.user_id!, [AdminACLs.WILDCARD]);
		});
	});

	it('ignores the setup token once the admin exists', async () => {
		await withSetupTokenInstance(async () => {
			await registerAdmin('firstadmin');
			const tokenless = await register(bootstrapRegistrationBody('aftertokenless'));
			expect(tokenless.status).toBe(HTTP_STATUS.OK);
			await expectUserACLs(tokenless.json.user_id!, []);
			const wrong = await register({...bootstrapRegistrationBody('afterwrong'), setup_token: WRONG_SETUP_TOKEN});
			expect(wrong.status).toBe(HTTP_STATUS.OK);
			await expectUserACLs(wrong.json.user_id!, []);
			const repeated = await register({...bootstrapRegistrationBody('afterrepeat'), setup_token: SETUP_TOKEN});
			expect(repeated.status).toBe(HTTP_STATUS.OK);
			await expectUserACLs(repeated.json.user_id!, []);
		});
	});

	it('refuses an unclaimed registration that sends no setup token', async () => {
		await withSetupTokenInstance(async () => {
			const {status, json} = await register({global_name: 'Unclaimed Visitor'});
			expect(status).toBe(HTTP_STATUS.BAD_REQUEST);
			expect(setupTokenErrors(json)).toEqual([{path: 'setup_token', code: ValidationErrorCodes.SETUP_TOKEN_REQUIRED}]);
		});
	});

	it('creates a plain account for an unclaimed registration that presents the setup token', async () => {
		await withSetupTokenInstance(async () => {
			const {status, json} = await register({global_name: 'Unclaimed Operator', setup_token: SETUP_TOKEN});
			expect(status).toBe(HTTP_STATUS.OK);
			await expectUserACLs(json.user_id!, []);
			await expect(getInstanceConfigRepository().isAdminBootstrapped()).resolves.toBe(false);
			const userId = await registerAdmin('claimedoperator');
			await expectUserACLs(userId, [AdminACLs.WILDCARD]);
		});
	});

	it.each(['approval', 'closed'] as const)(
		'lets the setup token holder register as admin while registration is %s',
		async (mode) => {
			await withSetupTokenInstance(async () => {
				await getInstanceConfigRepository().setRegistrationConfig({mode});
				const {status, json} = await register({...bootstrapRegistrationBody(`mode${mode}`), setup_token: SETUP_TOKEN});
				expect(status).toBe(HTTP_STATUS.OK);
				expect(json.registration_pending_approval).toBeUndefined();
				expect(json.token).toBeTruthy();
				await expectUserACLs(json.user_id!, [AdminACLs.WILDCARD]);
				await expect(getInstanceConfigRepository().getPendingRegistrations()).resolves.toEqual([]);
			});
		},
	);

	it('reports the missing setup token before any other registration check', async () => {
		await withSetupTokenInstance(async () => {
			await getInstanceConfigRepository().setAppPublicConfig({legal: {terms_url: 'https://example.com/terms'}});
			const body = {...bootstrapRegistrationBody('noconsent'), consent: false};
			const tokenless = await register(body);
			expect(tokenless.status).toBe(HTTP_STATUS.BAD_REQUEST);
			expect(setupTokenErrors(tokenless.json)).toEqual([
				{path: 'setup_token', code: ValidationErrorCodes.SETUP_TOKEN_REQUIRED},
			]);
			const verified = await register({...body, setup_token: SETUP_TOKEN});
			expect(verified.status).toBe(HTTP_STATUS.BAD_REQUEST);
			expect(setupTokenErrors(verified.json)).toEqual([
				{path: 'consent', code: ValidationErrorCodes.MUST_AGREE_TO_TOS_AND_PRIVACY_POLICY},
			]);
			await expect(getInstanceConfigRepository().isAdminBootstrapped()).resolves.toBe(false);
		});
	});

	it('locks an address out after ten wrong setup tokens', async () => {
		await withSetupTokenInstance(async () => {
			getConfig().dev.relaxRegistrationRateLimits = false;
			const attacker = '203.0.113.10';
			const operator = '203.0.113.20';
			for (let attempt = 0; attempt < 3; attempt++) {
				const tokenless = await register(bootstrapRegistrationBody(`limitmissing${attempt}`), attacker);
				expect(setupTokenErrors(tokenless.json)).toEqual([
					{path: 'setup_token', code: ValidationErrorCodes.SETUP_TOKEN_REQUIRED},
				]);
			}
			for (let attempt = 0; attempt < 10; attempt++) {
				const wrong = await register(
					{...bootstrapRegistrationBody(`limitwrong${attempt}`), setup_token: WRONG_SETUP_TOKEN},
					attacker,
				);
				expect(wrong.status).toBe(HTTP_STATUS.BAD_REQUEST);
				expect(setupTokenErrors(wrong.json)).toEqual([
					{path: 'setup_token', code: ValidationErrorCodes.SETUP_TOKEN_INVALID},
				]);
			}
			const eleventh = await register(
				{...bootstrapRegistrationBody('limiteleventh'), setup_token: WRONG_SETUP_TOKEN},
				attacker,
			);
			expect(eleventh.status).toBe(TOO_MANY_REQUESTS);
			const lockedOut = await register(
				{...bootstrapRegistrationBody('limitlocked'), setup_token: SETUP_TOKEN},
				attacker,
			);
			expect(lockedOut.status).toBe(TOO_MANY_REQUESTS);
			await expect(getInstanceConfigRepository().isAdminBootstrapped()).resolves.toBe(false);
			const userId = await registerAdmin('limitoperator', operator);
			await expectUserACLs(userId, [AdminACLs.WILDCARD]);
		});
	});

	it('lets the setup token holder past the registration limiter of their address', async () => {
		await withSetupTokenInstance(async () => {
			getConfig().dev.relaxRegistrationRateLimits = false;
			const operator = '203.0.113.30';
			const registrationLimit = {
				identifier: `registration:ip:${getSameIpDecisionKey(operator) ?? operator}`,
				maxAttempts: 3,
				windowMs: ms('1 hour'),
			};
			for (let attempt = 0; attempt < 3; attempt++) {
				await getRateLimitService().checkLimit(registrationLimit);
			}
			const userId = await registerAdmin('limitedoperator', operator);
			await expectUserACLs(userId, [AdminACLs.WILDCARD]);
			const next = await register(bootstrapRegistrationBody('limitedmember'), operator);
			expect(next.status).toBe(TOO_MANY_REQUESTS);
		});
	});

	it('ties the admin grant to the setup token on an instance configured through the environment', async () => {
		await withSetupTokenInstance(async () => {
			getConfig().instance.setup.configured = true;
			const tokenless = await register(bootstrapRegistrationBody('envtokenless'));
			expect(tokenless.status).toBe(HTTP_STATUS.OK);
			await expectUserACLs(tokenless.json.user_id!, []);
			await expect(getInstanceConfigRepository().isAdminBootstrapped()).resolves.toBe(false);
			const wrong = await register({...bootstrapRegistrationBody('envwrong'), setup_token: WRONG_SETUP_TOKEN});
			expect(wrong.status).toBe(HTTP_STATUS.BAD_REQUEST);
			expect(setupTokenErrors(wrong.json)).toEqual([
				{path: 'setup_token', code: ValidationErrorCodes.SETUP_TOKEN_INVALID},
			]);
			const userId = await registerAdmin('envadmin');
			await expectUserACLs(userId, [AdminACLs.WILDCARD]);
			await expect(getInstanceConfigRepository().isAdminBootstrapped()).resolves.toBe(true);
		});
	});

	it('ties the admin grant to the setup token on an instance configured in storage', async () => {
		await withSetupTokenInstance(async () => {
			await getInstanceConfigRepository().setAppPublicConfig({setup: {configured: true}});
			const tokenless = await register(bootstrapRegistrationBody('storedtokenless'));
			expect(tokenless.status).toBe(HTTP_STATUS.OK);
			await expectUserACLs(tokenless.json.user_id!, []);
			const userId = await registerAdmin('storedadmin');
			await expectUserACLs(userId, [AdminACLs.WILDCARD]);
		});
	});

	it('leaves the hosted service alone even with a setup token configured', async () => {
		await withSetupTokenInstance(async () => {
			getConfig().instance.selfHosted = false;
			await getInstanceConfigRepository().setAppPublicConfig({setup: {configured: false}});
			const {status, json} = await register(bootstrapRegistrationBody('hostedtokenless'));
			expect(status).toBe(HTTP_STATUS.OK);
			await expectUserACLs(json.user_id!, [AdminACLs.WILDCARD]);
		});
	});

	it('releases the admin claim when the account cannot be created', async () => {
		await withSetupTokenInstance(async () => {
			const create = vi.spyOn(getUserRepository(), 'create').mockRejectedValueOnce(new Error('insert failed'));
			try {
				const failed = await register({...bootstrapRegistrationBody('failedadmin'), setup_token: SETUP_TOKEN});
				expect(failed.status).toBe(HTTP_STATUS.INTERNAL_SERVER_ERROR);
				expect(create).toHaveBeenCalledTimes(1);
			} finally {
				create.mockRestore();
			}
			await expect(getInstanceConfigRepository().isAdminBootstrapped()).resolves.toBe(false);
			const tokenless = await register(bootstrapRegistrationBody('failedtokenless'));
			expect(setupTokenErrors(tokenless.json)).toEqual([
				{path: 'setup_token', code: ValidationErrorCodes.SETUP_TOKEN_REQUIRED},
			]);
			const userId = await registerAdmin('retriedadmin');
			await expectUserACLs(userId, [AdminACLs.WILDCARD]);
			await expect(getInstanceConfigRepository().getConfig('admin_bootstrap_user_id')).resolves.toBe(userId);
		});
	});

	it('burns a generated setup token once the admin exists', async () => {
		await withSetupTokenInstance(async () => {
			const config = getConfig();
			const originalNodeEnv = config.nodeEnv;
			config.instance.setup.token = null;
			config.nodeEnv = 'production';
			try {
				const repository = getInstanceConfigRepository();
				const generated = await repository.createSetupTokenIfAbsent();
				expect(generated).not.toBeNull();
				const wrong = await register({...bootstrapRegistrationBody('genwrong'), setup_token: SETUP_TOKEN});
				expect(setupTokenErrors(wrong.json)).toEqual([
					{path: 'setup_token', code: ValidationErrorCodes.SETUP_TOKEN_INVALID},
				]);
				const {status, json} = await register({...bootstrapRegistrationBody('genadmin'), setup_token: generated});
				expect(status).toBe(HTTP_STATUS.OK);
				await expectUserACLs(json.user_id!, [AdminACLs.WILDCARD]);
				expect(await repository.getConfig(SETUP_TOKEN_HASH_KEY)).toBe('');
				await expect(repository.verifySetupToken(generated)).resolves.toBe(false);
			} finally {
				config.nodeEnv = originalNodeEnv;
			}
		});
	});

	it('burns the stored setup token when an admin marks setup as configured', async () => {
		await withBootstrapAdminConfig({selfHosted: false, testModeEnabled: true}, async () => {
			const repository = getInstanceConfigRepository();
			await repository.setAppPublicConfig({setup: {configured: false}});
			const account = await registerUser(harness, bootstrapRegistrationBody('configuringadmin'));
			await repository.releaseAdminBootstrap();
			const generated = await repository.createSetupTokenIfAbsent();
			await expect(repository.verifySetupToken(generated)).resolves.toBe(true);
			await createBuilder(harness, account.token)
				.patch('/admin/instance/config')
				.body({app_public: {setup: {configured: true}}})
				.execute();
			await expect(repository.isAdminBootstrapped()).resolves.toBe(true);
			await expect(repository.verifySetupToken(generated)).resolves.toBe(false);
		});
	});
});
