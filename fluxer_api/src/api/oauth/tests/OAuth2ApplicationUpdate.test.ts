// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createOAuth2Application, createUniqueApplicationName} from '@app/api/oauth/tests/OAuth2TestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import type {ApplicationResponse} from '@fluxer/schema/src/domains/oauth/OAuthSchemas';
import {beforeEach, describe, expect, test} from 'vitest';

describe('OAuth2 Application Update', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	test('supports partial updates', async () => {
		const account = await createTestAccount(harness);
		const originalName = createUniqueApplicationName();
		const originalURIs = ['https://example.com/old'];
		const createResult = await createOAuth2Application(harness, account.token, {
			name: originalName,
			redirect_uris: originalURIs,
		});
		const newName = createUniqueApplicationName();
		const updated = await createBuilder<ApplicationResponse>(harness, account.token)
			.patch(`/oauth2/applications/${createResult.application.id}`)
			.body({name: newName})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(updated.name).toBe(newName);
		expect(updated.redirect_uris).toEqual(originalURIs);
	});
	test('enforces access control', async () => {
		const owner = await createTestAccount(harness);
		const otherUser = await createTestAccount(harness);
		const createResult = await createOAuth2Application(harness, owner.token, {
			name: createUniqueApplicationName(),
		});
		await createBuilder(harness, otherUser.token)
			.patch(`/oauth2/applications/${createResult.application.id}`)
			.body({name: createUniqueApplicationName()})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
});
