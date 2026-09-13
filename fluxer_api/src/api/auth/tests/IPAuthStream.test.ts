// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createAuthHarness,
	createUniqueEmail,
	createUniqueTestId,
	createUniqueUsername,
	registerUser,
} from '@app/api/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('Auth IP Authorization Poll', () => {
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
	it('returns not completed when authorization is pending', async () => {
		const email = createUniqueEmail('ip-poll');
		const password = 'a-strong-password';
		const reg = await registerUser(harness, {
			email,
			username: createUniqueUsername('poll'),
			global_name: 'Poll User',
			password,
			date_of_birth: '2000-01-01',
			consent: true,
		});
		const ticket = createUniqueTestId('poll');
		const token = createUniqueTestId('token');
		await createBuilderWithoutAuth(harness)
			.post('/test/auth/ip-authorization')
			.body({
				ticket,
				token,
				user_id: reg.user_id,
				email,
				username: 'poll-user',
				client_ip: '192.0.2.10',
				user_agent: 'IntegrationTest/1.0',
				client_location: 'Testland',
				created_at: Date.now() - 60 * 1000,
				ttl_seconds: 900,
			})
			.expect(200)
			.execute();
		const pollBefore = await createBuilderWithoutAuth<{
			completed: boolean;
		}>(harness)
			.get(`/auth/ip-authorization/poll?ticket=${ticket}`)
			.execute();
		expect(pollBefore).toMatchObject({completed: false});
	});
	it('returns completed with credentials after authorization', async () => {
		const email = createUniqueEmail('ip-poll-complete');
		const password = 'a-strong-password';
		const reg = await registerUser(harness, {
			email,
			username: createUniqueUsername('pollcomplete'),
			global_name: 'Poll Complete User',
			password,
			date_of_birth: '2000-01-01',
			consent: true,
		});
		const ticket = createUniqueTestId('poll-complete');
		const token = createUniqueTestId('token');
		await createBuilderWithoutAuth(harness)
			.post('/test/auth/ip-authorization')
			.body({
				ticket,
				token,
				user_id: reg.user_id,
				email,
				username: 'poll-complete-user',
				client_ip: '192.0.2.10',
				user_agent: 'IntegrationTest/1.0',
				client_location: 'Testland',
				created_at: Date.now() - 60 * 1000,
				ttl_seconds: 900,
			})
			.expect(200)
			.execute();
		await createBuilderWithoutAuth(harness)
			.post('/test/auth/ip-authorization/publish')
			.body({
				ticket,
				token,
				user_id: reg.user_id,
			})
			.expect(200)
			.execute();
		const pollAfter = await createBuilderWithoutAuth<{
			completed: boolean;
			token: string;
			user_id: string;
		}>(harness)
			.get(`/auth/ip-authorization/poll?ticket=${ticket}`)
			.execute();
		expect(pollAfter).toMatchObject({
			completed: true,
			token,
			user_id: reg.user_id,
		});
	});
	it('rejects poll with invalid ticket', async () => {
		await createBuilderWithoutAuth(harness)
			.get('/auth/ip-authorization/poll?ticket=does-not-exist')
			.expect(400, 'INVALID_FORM_BODY')
			.execute();
	});
});
