// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestUserWithPremium} from '@app/api/stripe/tests/StripeWebhookTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopWorkerService} from '@app/api/test/NoopWorkerService';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

describe('RpcService session payment reconciliation', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('queues payment reconciliation without a job record', async () => {
		const account = await createTestUserWithPremium(harness, UserPremiumTypes.SUBSCRIPTION, {
			stripeCustomerId: 'cus_rpc_session_reconcile',
		});
		const addJob = vi.spyOn(NoopWorkerService.prototype, 'addJob');
		try {
			await createBuilder(harness, '')
				.post('/test/rpc-session-init')
				.body({type: 'session', token: account.token, version: 1, ip: '127.0.0.1'})
				.expect(HTTP_STATUS.OK)
				.execute();
			await vi.waitFor(() =>
				expect(addJob).toHaveBeenCalledWith('reconcileUserPayments', {userId: account.userId}, {skipLedger: true}),
			);
		} finally {
			addJob.mockRestore();
		}
	});
});
