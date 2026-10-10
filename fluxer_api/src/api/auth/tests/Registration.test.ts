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
import {getInstanceConfigRepository, getUserRepository} from '@app/api/middleware/ServiceSingletons';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

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
		expect(Object.keys(reg).sort()).toEqual(['token', 'user', 'user_id']);
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
