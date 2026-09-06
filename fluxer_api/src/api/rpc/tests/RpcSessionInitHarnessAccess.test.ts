// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {Config} from '../../Config';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';

const HARNESS_TOKEN = 'rpc-session-init-harness-token';
const TEST_AUTH_HEADER = 'x-test-token';

interface RpcSessionResponse {
	type: string;
	data: {
		user: {
			id: string;
		};
	};
}

describe('POST /test/rpc-session-init harness access', () => {
	let harness: ApiTestHarness;
	let previousTestHarnessToken: string | undefined;
	beforeEach(async () => {
		harness = await createApiTestHarness();
		previousTestHarnessToken = Config.dev.testHarnessToken;
	});
	afterEach(async () => {
		Config.dev.testHarnessToken = previousTestHarnessToken;
		await harness?.shutdown();
	});

	test('rejects a session init that omits the harness token', async () => {
		const account = await createTestAccount(harness);
		Config.dev.testHarnessToken = HARNESS_TOKEN;
		await createBuilder(harness, '')
			.post('/test/rpc-session-init')
			.body({type: 'session', token: account.token, version: 1, ip: '127.0.0.1'})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.TEST_HARNESS_FORBIDDEN)
			.execute();
	});

	test('rejects before the rpc request body is parsed or dispatched', async () => {
		Config.dev.testHarnessToken = HARNESS_TOKEN;
		await createBuilder(harness, '')
			.post('/test/rpc-session-init')
			.body({type: 'definitely_not_an_rpc_request'})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.TEST_HARNESS_FORBIDDEN)
			.execute();
	});

	test('rejects a session init carrying the wrong harness token', async () => {
		const account = await createTestAccount(harness);
		Config.dev.testHarnessToken = HARNESS_TOKEN;
		await createBuilder(harness, '')
			.post('/test/rpc-session-init')
			.header(TEST_AUTH_HEADER, `${HARNESS_TOKEN}-wrong`)
			.body({type: 'session', token: account.token, version: 1, ip: '127.0.0.1'})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.TEST_HARNESS_FORBIDDEN)
			.execute();
	});

	test('accepts a session init carrying the harness token', async () => {
		const account = await createTestAccount(harness);
		Config.dev.testHarnessToken = HARNESS_TOKEN;
		const response = await createBuilder<RpcSessionResponse>(harness, '')
			.post('/test/rpc-session-init')
			.header(TEST_AUTH_HEADER, HARNESS_TOKEN)
			.body({type: 'session', token: account.token, version: 1, ip: '127.0.0.1'})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(response.type).toBe('session');
		expect(response.data.user.id).toBe(account.userId);
	});

	test('accepts a session init when no harness token is configured', async () => {
		const account = await createTestAccount(harness);
		Config.dev.testHarnessToken = undefined;
		const response = await createBuilder<RpcSessionResponse>(harness, '')
			.post('/test/rpc-session-init')
			.body({type: 'session', token: account.token, version: 1, ip: '127.0.0.1'})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(response.data.user.id).toBe(account.userId);
	});
});
