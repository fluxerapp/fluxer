// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, loginAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {getGatewayService} from '@app/api/middleware/ServiceRegistry';
import {getUserRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

type FailingStep = 'deleteAllPushSubscriptions' | 'deleteAllAuthSessions' | 'terminateSession' | 'updateFlags';

const FAILING_STEPS: ReadonlyArray<FailingStep> = [
	'deleteAllPushSubscriptions',
	'deleteAllAuthSessions',
	'terminateSession',
	'updateFlags',
];

describe('Self disable', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		vi.restoreAllMocks();
		await harness?.shutdown();
	});

	test('disabling signs out every session', async () => {
		const account = await createTestAccount(harness);
		await loginAccount(harness, account);
		const userId = createUserID(BigInt(account.userId));
		await createBuilder(harness, account.token)
			.post('/users/@me/disable')
			.body({password: account.password})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const user = await getUserRepository().findUniqueAssert(userId);
		expect(user.flags & UserFlags.DISABLED).toBe(UserFlags.DISABLED);
		expect(await getUserRepository().listAuthSessions(userId)).toHaveLength(0);
	});

	test.each(FAILING_STEPS)('a failed %s never leaves a disabled account with a live session', async (step) => {
		const account = await createTestAccount(harness);
		await loginAccount(harness, account);
		const userId = createUserID(BigInt(account.userId));
		const users = getUserRepository();
		const failure = new Error('Cassandra timeout');
		if (step === 'terminateSession') {
			vi.spyOn(getGatewayService(), 'terminateSession').mockRejectedValueOnce(failure);
		} else {
			vi.spyOn(users, step).mockRejectedValueOnce(failure);
		}
		const response = await harness.requestJson({
			path: '/users/@me/disable',
			method: 'POST',
			body: {password: account.password},
			headers: {Authorization: account.token},
		});
		expect(response.status).toBe(HTTP_STATUS.INTERNAL_SERVER_ERROR);
		vi.restoreAllMocks();
		const user = await users.findUniqueAssert(userId);
		const disabled = (user.flags & UserFlags.DISABLED) !== 0n;
		const sessions = await users.listAuthSessions(userId);
		expect({disabled, liveSessions: disabled ? sessions.length : 0}).toEqual({disabled, liveSessions: 0});
	});
});
