// SPDX-License-Identifier: AGPL-3.0-or-later

import {setInjectedUnfurlerService} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {afterEach, beforeEach, describe, it} from 'vitest';

describe('UnfurlController', () => {
	let harness: ApiTestHarness;

	beforeEach(async () => {
		harness = await createApiTestHarness();
	});

	afterEach(async () => {
		setInjectedUnfurlerService(undefined);
		await harness.shutdown();
	});

	it('requires authentication', async () => {
		await createBuilderWithoutAuth(harness).post('/unfurl').body({url: 'https://example.com'}).expect(401).execute();
	});
});
