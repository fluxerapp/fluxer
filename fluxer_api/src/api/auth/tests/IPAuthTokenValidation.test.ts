// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	clearTestEmails,
	createAuthHarness,
	createUniqueEmail,
	createUniqueUsername,
	listTestEmails,
	registerUser,
} from '@app/api/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('Auth IP Authorization Token Validation', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createAuthHarness();
	});
	beforeEach(async () => {
		await harness.reset();
		await clearTestEmails(harness);
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	it('validates that attempting to authorize an IP with an invalid token fails with appropriate error', async () => {
		await createBuilderWithoutAuth(harness)
			.post('/auth/authorize-ip')
			.body({token: 'invalid-token-12345'})
			.expect(400, 'INVALID_FORM_BODY')
			.execute();
	});
	it('validates that an IP authorization token can only be used once and becomes invalid after successful use', async () => {
		const email = createUniqueEmail('ip-single-use');
		const password = 'a-strong-password';
		await registerUser(harness, {
			email,
			username: createUniqueUsername('singleuse'),
			global_name: 'Single Use User',
			password,
			date_of_birth: '2000-01-01',
			consent: true,
		});
		await clearTestEmails(harness);
		const newIP = '10.100.101.102';
		await createBuilderWithoutAuth(harness)
			.post('/auth/login')
			.body({email, password})
			.header('x-forwarded-for', newIP)
			.expect(403)
			.execute();
		const emails = await listTestEmails(harness);
		const ipAuthEmail = emails.find((e) => e.type === 'ip_authorization' && e.to === email);
		expect(ipAuthEmail).toBeDefined();
		const authToken = ipAuthEmail!.metadata['token'];
		expect(authToken).toBeTruthy();
		await createBuilderWithoutAuth(harness)
			.post('/auth/authorize-ip')
			.body({token: authToken})
			.header('x-forwarded-for', newIP)
			.expect(204)
			.execute();
		await createBuilderWithoutAuth(harness)
			.post('/auth/authorize-ip')
			.body({token: authToken})
			.header('x-forwarded-for', newIP)
			.expect(400, 'INVALID_FORM_BODY')
			.execute();
	});
	it('validates that a token generated for one IP cannot be used to authorize a different IP', async () => {
		const email = createUniqueEmail('ip-wrong-ip');
		const password = 'a-strong-password';
		await registerUser(harness, {
			email,
			username: createUniqueUsername('wrongip'),
			global_name: 'Wrong IP User',
			password,
			date_of_birth: '2000-01-01',
			consent: true,
		});
		await clearTestEmails(harness);
		const firstNewIP = '10.110.120.130';
		await createBuilderWithoutAuth(harness)
			.post('/auth/login')
			.body({email, password})
			.header('x-forwarded-for', firstNewIP)
			.expect(403)
			.execute();
		const emails = await listTestEmails(harness);
		const ipAuthEmail = emails.find((e) => e.type === 'ip_authorization' && e.to === email);
		expect(ipAuthEmail).toBeDefined();
		const authToken = ipAuthEmail!.metadata['token'];
		expect(authToken).toBeTruthy();
		const secondNewIP = '10.140.150.160';
		await createBuilderWithoutAuth(harness)
			.post('/auth/authorize-ip')
			.body({token: authToken})
			.header('x-forwarded-for', secondNewIP)
			.expect(204)
			.execute();
	});
	it('validates that IP authorization tokens expire after a reasonable time period and cannot be used after expiration', async () => {
		const email = createUniqueEmail('ip-expire');
		const password = 'a-strong-password';
		await registerUser(harness, {
			email,
			username: createUniqueUsername('expire'),
			global_name: 'Expire User',
			password,
			date_of_birth: '2000-01-01',
			consent: true,
		});
		await clearTestEmails(harness);
		const newIP = '10.40.50.60';
		await createBuilderWithoutAuth(harness)
			.post('/auth/login')
			.body({email, password})
			.header('x-forwarded-for', newIP)
			.expect(403)
			.execute();
		const emails = await listTestEmails(harness);
		const ipAuthEmail = emails.find((e) => e.type === 'ip_authorization' && e.to === email);
		expect(ipAuthEmail).toBeDefined();
		const authToken = ipAuthEmail!.metadata['token'];
		expect(authToken).toBeTruthy();
		await createBuilderWithoutAuth(harness)
			.post('/test/auth/ip-authorization/expire')
			.body({token: authToken})
			.execute();
		await createBuilderWithoutAuth(harness)
			.post('/auth/authorize-ip')
			.body({token: authToken})
			.header('x-forwarded-for', newIP)
			.expect(400, 'INVALID_FORM_BODY')
			.execute();
		await createBuilderWithoutAuth(harness)
			.post('/auth/login')
			.body({email, password})
			.header('x-forwarded-for', newIP)
			.expect(403)
			.execute();
	});
});
