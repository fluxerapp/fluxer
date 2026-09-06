// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';
import {createUserID} from '../../BrandedTypes';
import {getConfig} from '../../Config';
import {getInstanceConfigRepository, getUserRepository} from '../../middleware/ServiceSingletons';
import {torExitListCache} from '../../middleware/TorExitListCache';
import type {ApiTestHarness} from '../../test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '../../test/TestRequestBuilder';
import {
	createAuthHarness,
	createUniqueEmail,
	createUniqueUsername,
	fetchMe,
	type LoginSuccessResponse,
	registerUser,
	titleCaseEmail,
	type UserMeResponse,
} from './AuthTestUtils';

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
	});
	it('grants wildcard admin ACL to first accepted local dev registration', async () => {
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
	it('allows setup-open sessions to fetch instance config when bootstrap marker is stale', async () => {
		await withBootstrapAdminConfig({selfHosted: false, testModeEnabled: true}, async () => {
			const instanceConfigRepository = getInstanceConfigRepository();
			await instanceConfigRepository.setAppPublicConfig({setup: {configured: false}});
			await instanceConfigRepository.markAdminBootstrapped();
			const account = await registerUser(harness, bootstrapRegistrationBody('stalesetupmarker'));
			await expectUserACLs(account.user_id, []);
			await createBuilder(harness, account.token).get('/admin/instance/config').execute();
		});
	});
	it('repairs setup completer admin ACL when bootstrap marker is stale', async () => {
		await withBootstrapAdminConfig({selfHosted: false, testModeEnabled: true}, async () => {
			const instanceConfigRepository = getInstanceConfigRepository();
			await instanceConfigRepository.setAppPublicConfig({setup: {configured: false}});
			await instanceConfigRepository.markAdminBootstrapped();
			const account = await registerUser(harness, bootstrapRegistrationBody('stalesetupcomplete'));
			await expectUserACLs(account.user_id, []);

			await createBuilder(harness, account.token)
				.patch('/admin/instance/config')
				.body({app_public: {setup: {configured: true}}})
				.execute();

			await expectUserACLs(account.user_id, [AdminACLs.WILDCARD]);
			await createBuilder(harness, account.token).get('/admin/instance/config').execute();
		});
	});
	it('allows emoji global name', async () => {
		const globalName = '🌻 Sunflower';
		const reg = await registerUser(harness, {
			email: createUniqueEmail('global-name-emoji'),
			username: createUniqueUsername('globalnameemoji'),
			global_name: globalName,
			password: 'a-strong-password',
			date_of_birth: '2000-01-01',
			consent: true,
		});
		const me = (await fetchMe(harness, reg.token)).json as UserMeResponse;
		expect(me.global_name).toBe(globalName);
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
	it('rejects invalid registration payloads', async () => {
		await createBuilderWithoutAuth(harness)
			.post('/auth/register')
			.body({
				email: 'not-an-email',
				username: 'itest',
				global_name: 'Test User',
				password: 'a-strong-password',
				date_of_birth: '2000-01-01',
				consent: true,
			})
			.expect(400)
			.execute();
		await createBuilderWithoutAuth(harness)
			.post('/auth/register')
			.body({
				email: createUniqueEmail('weak-password'),
				username: 'itest',
				global_name: 'Test User',
				password: 'weak',
				date_of_birth: '2000-01-01',
				consent: true,
			})
			.expect(400)
			.execute();
		await registerUser(harness, {
			email: 'integration-duplicate-email@example.com',
			username: createUniqueUsername('firstuser'),
			global_name: 'Test User',
			password: 'a-strong-password',
			date_of_birth: '2000-01-01',
			consent: true,
		});
		const duplicateJson = await createBuilderWithoutAuth<{
			code: string;
			errors: Array<{
				path: string;
				message: string;
			}>;
		}>(harness)
			.post('/auth/register')
			.body({
				email: 'integration-duplicate-email@example.com',
				username: createUniqueUsername('seconduser'),
				global_name: 'Test User',
				password: 'a-strong-password',
				date_of_birth: '2000-01-01',
				consent: true,
			})
			.expect(400)
			.execute();
		expect(duplicateJson.code).toBe('INVALID_FORM_BODY');
		const emailError = duplicateJson.errors.find((e) => e.path === 'email');
		expect(emailError?.message).toBe('Email is already in use.');
		const missingFieldsCases: Array<{
			name: string;
			body: Record<string, unknown>;
		}> = [
			{
				name: 'missing email',
				body: {
					email: '',
					username: 'itest',
					global_name: 'Test User',
					password: 'a-strong-password',
					date_of_birth: '2000-01-01',
					consent: true,
				},
			},
			{
				name: 'missing username',
				body: {
					email: 'integration-missing-username@example.com',
					username: '',
					global_name: 'Test User',
					password: 'a-strong-password',
					date_of_birth: '2000-01-01',
					consent: true,
				},
			},
			{
				name: 'missing password',
				body: {
					email: 'integration-missing-password@example.com',
					username: 'itest',
					global_name: 'Test User',
					password: '',
					date_of_birth: '2000-01-01',
					consent: true,
				},
			},
			{
				name: 'missing date of birth',
				body: {
					email: 'integration-missing-dob@example.com',
					username: 'itest',
					global_name: 'Test User',
					password: 'a-strong-password',
					date_of_birth: '',
					consent: true,
				},
			},
		];
		for (const testCase of missingFieldsCases) {
			await createBuilderWithoutAuth(harness).post('/auth/register').body(testCase.body).expect(400).execute();
		}
	});
	it('allows login after registration', async () => {
		const email = createUniqueEmail('login');
		const password = 'a-strong-password';
		const reg = await registerUser(harness, {
			email,
			username: createUniqueUsername('loginuser'),
			global_name: 'Login User',
			password,
			date_of_birth: '2000-01-01',
			consent: true,
		});
		const login = await createBuilderWithoutAuth<LoginSuccessResponse>(harness)
			.post('/auth/login')
			.body({email, password})
			.execute();
		expect('mfa' in login).toBe(false);
		expect(login.token.length).toBeGreaterThan(0);
		expect(login.user_id).toBe(reg.user_id);
	});
	it('blocks any request from a Tor exit at the edge', async () => {
		torExitListCache.seedForTesting(['127.0.0.1']);
		try {
			await createBuilderWithoutAuth(harness)
				.post('/auth/register')
				.body({
					email: createUniqueEmail('tor-register'),
					username: createUniqueUsername('torregister'),
					global_name: 'Tor Register',
					password: 'a-strong-password',
					date_of_birth: '2000-01-01',
					consent: true,
				})
				.expect(403, 'GLOBAL_IP_BANNED')
				.execute();
		} finally {
			torExitListCache.clearForTesting();
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
