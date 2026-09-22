// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	createOAuth2Application,
	createUniqueApplicationName,
	listOAuth2Applications,
} from '@app/api/oauth/tests/OAuth2TestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {beforeEach, describe, expect, test} from 'vitest';

describe('OAuth2 Application List', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	test('returns empty list when no applications exist', async () => {
		const account = await createTestAccount(harness);
		const applications = await listOAuth2Applications(harness, account.token);
		expect(applications).toEqual([]);
	});
	test('returns list response shape', async () => {
		const account = await createTestAccount(harness);
		const appName = createUniqueApplicationName();
		await createOAuth2Application(harness, account.token, {
			name: appName,
			redirect_uris: ['https://example.com/callback'],
		});
		const applications = await listOAuth2Applications(harness, account.token);
		expect(applications.length).toBeGreaterThan(0);
		const app = applications[0]!;
		expect(app.id).toBeTruthy();
		expect(app.name).toBeTruthy();
		expect(app.redirect_uris).toBeDefined();
		expect(app.bot?.id).toBeTruthy();
		expect(app.bot?.username).toBeTruthy();
		expect(app.bot?.discriminator).toBeTruthy();
		expect(app.bot?.token).toBeUndefined();
		expect(app.client_secret).toBeUndefined();
	});
	test('returns only applications owned by user', async () => {
		const owner1 = await createTestAccount(harness);
		const owner2 = await createTestAccount(harness);
		const app1 = await createOAuth2Application(harness, owner1.token, {
			name: createUniqueApplicationName(),
		});
		const app2 = await createOAuth2Application(harness, owner2.token, {
			name: createUniqueApplicationName(),
		});
		const owner1Apps = await listOAuth2Applications(harness, owner1.token);
		const owner2Apps = await listOAuth2Applications(harness, owner2.token);
		expect(owner1Apps.length).toBe(1);
		expect(owner1Apps[0]?.id).toBe(app1.application.id);
		expect(owner2Apps.length).toBe(1);
		expect(owner2Apps[0]?.id).toBe(app2.application.id);
	});
	test('supports alternative endpoint /users/@me/applications', async () => {
		const account = await createTestAccount(harness);
		await createOAuth2Application(harness, account.token, {
			name: createUniqueApplicationName(),
		});
		const applications = await createBuilder<
			Array<{
				id: string;
			}>
		>(harness, account.token)
			.get('/users/@me/applications')
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(applications.length).toBeGreaterThan(0);
	});
	test('requires authentication', async () => {
		await createBuilderWithoutAuth(harness).get('/oauth2/applications/@me').expect(HTTP_STATUS.UNAUTHORIZED).execute();
	});
});
