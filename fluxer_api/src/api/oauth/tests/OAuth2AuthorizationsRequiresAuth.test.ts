// SPDX-License-Identifier: AGPL-3.0-or-later

import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, it} from 'vitest';

describe('OAuth2 authorizations requires auth', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	it('verifies that the authorizations endpoint requires authentication', async () => {
		await createBuilderWithoutAuth(harness)
			.get('/oauth2/@me/authorizations')
			.expect(HTTP_STATUS.UNAUTHORIZED)
			.execute();
	});
});
