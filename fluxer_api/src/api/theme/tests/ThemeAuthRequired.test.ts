// SPDX-License-Identifier: AGPL-3.0-or-later

import {createFakeAuthToken} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, it} from 'vitest';

describe('Theme authentication required', () => {
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
	it('rejects request without authentication', async () => {
		await createBuilderWithoutAuth(harness)
			.post('/users/@me/themes')
			.body({css: '.test { color: red; }'})
			.expect(HTTP_STATUS.UNAUTHORIZED, 'UNAUTHORIZED')
			.execute();
	});
	it('rejects request with invalid token', async () => {
		const fakeToken = createFakeAuthToken();
		await createBuilder(harness, fakeToken)
			.post('/users/@me/themes')
			.body({css: '.test { color: red; }'})
			.expect(HTTP_STATUS.UNAUTHORIZED, 'UNAUTHORIZED')
			.execute();
	});
	it('rejects request with empty authorization header', async () => {
		await createBuilder(harness, '')
			.post('/users/@me/themes')
			.body({css: '.test { color: red; }'})
			.expect(HTTP_STATUS.UNAUTHORIZED, 'UNAUTHORIZED')
			.execute();
	});
	it('rejects request with malformed token', async () => {
		await createBuilder(harness, 'not-a-valid-token')
			.post('/users/@me/themes')
			.body({css: '.test { color: red; }'})
			.expect(HTTP_STATUS.UNAUTHORIZED, 'UNAUTHORIZED')
			.execute();
	});
});
