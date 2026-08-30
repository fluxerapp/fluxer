// SPDX-License-Identifier: AGPL-3.0-or-later

import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';

interface RpcSessionResponse {
	type: 'session';
	data: {
		_timings?: {
			pod_name?: string;
		};
		user: {
			id: string;
			is_staff: boolean;
		};
	};
}

async function setUserFlags(harness: ApiTestHarness, userId: string, flags: bigint): Promise<void> {
	await createBuilder(harness, '')
		.patch(`/test/users/${userId}/flags`)
		.body({flags: flags.toString()})
		.expect(HTTP_STATUS.OK)
		.execute();
}

async function initSession(harness: ApiTestHarness, token: string): Promise<RpcSessionResponse> {
	return await createBuilder<RpcSessionResponse>(harness, '')
		.post('/test/rpc-session-init')
		.body({type: 'session', token, version: 1, ip: '127.0.0.1'})
		.expect(HTTP_STATUS.OK)
		.execute();
}

describe('RpcService session timings visibility', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('omits _timings for a non-staff session', async () => {
		const account = await createTestAccount(harness);
		await setUserFlags(harness, account.userId, 0n);
		const response = await initSession(harness, account.token);
		expect(response.data.user.is_staff).toBe(false);
		expect(Object.hasOwn(response.data, '_timings')).toBe(false);
	});
	test('includes _timings with pod metadata for a staff session', async () => {
		const account = await createTestAccount(harness);
		await setUserFlags(harness, account.userId, UserFlags.STAFF);
		const response = await initSession(harness, account.token);
		expect(response.data.user.is_staff).toBe(true);
		expect(typeof response.data._timings?.pod_name).toBe('string');
	});
});
