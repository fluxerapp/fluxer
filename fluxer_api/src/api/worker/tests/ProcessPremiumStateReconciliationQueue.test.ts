// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID} from '@app/api/BrandedTypes';
import {PremiumStateReconciliationQueueService} from '@app/api/infrastructure/PremiumStateReconciliationQueueService';
import {createTestWorkerHelpers} from '@app/api/test/SyncTaskWorkerService';
import {UserRepository} from '@app/api/user/repositories/UserRepository';
import processPremiumStateReconciliationQueue from '@app/api/worker/tasks/ProcessPremiumStateReconciliationQueue';
import {clearWorkerDependencies, setWorkerDependenciesForTest} from '@app/api/worker/WorkerContext';
import {MockKVProvider} from '@pkgs/kv_client/src/testing/MockKVProvider';
import Stripe from 'stripe';
import {afterEach, describe, expect, test} from 'vitest';

const USER_ID = createUserID(834271905123471361n);
const ONE_HOUR_MS = 60 * 60 * 1000;

describe('processPremiumStateReconciliationQueue', () => {
	afterEach(() => {
		clearWorkerDependencies();
	});

	test('removes the user from the queue once reconciliation commits', async () => {
		const kvClient = new MockKVProvider();
		const queueService = new PremiumStateReconciliationQueueService(kvClient);
		await queueService.enqueueUser(USER_ID, new Date(Date.now() - 1000));

		setWorkerDependenciesForTest({
			premiumStateReconciliationQueueService: queueService,
			stripe: new Stripe('sk_test_reconcile'),
			userRepository: new UserRepository(kvClient),
		});

		await processPremiumStateReconciliationQueue({}, createTestWorkerHelpers());

		expect(await queueService.getQueueSize()).toBe(0);
		expect(await queueService.getReadyUserIds(Date.now() + ONE_HOUR_MS, 10)).toEqual([]);
	});

	test('rejects a second claim until the lease expires', async () => {
		const queueService = new PremiumStateReconciliationQueueService(new MockKVProvider());
		await queueService.enqueueUser(USER_ID, new Date(Date.now() - 1000));
		const now = Date.now();

		expect(await queueService.claimUser(USER_ID, now, now + ONE_HOUR_MS)).toBe(true);
		expect(await queueService.claimUser(USER_ID, now, now + ONE_HOUR_MS)).toBe(false);
		expect(await queueService.claimUser(USER_ID, now + ONE_HOUR_MS, now + 2 * ONE_HOUR_MS)).toBe(true);
	});
});
