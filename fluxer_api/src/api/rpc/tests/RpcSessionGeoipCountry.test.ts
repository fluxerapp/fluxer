// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

interface RpcSessionResponse {
	type: 'session';
	data: {
		country_code: string;
		geoip_country_code: string | null;
	};
}

describe('RpcService session geoip country', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('reports an unknown country as null without the country_code default', async () => {
		const account = await createTestAccount(harness);
		const response = await createBuilder<RpcSessionResponse>(harness, '')
			.post('/test/rpc-session-init')
			.body({type: 'session', token: account.token, version: 1, ip: '127.0.0.1'})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(response.data.geoip_country_code).toBeNull();
		expect(response.data.country_code).toBe('US');
	});
});
