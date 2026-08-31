// SPDX-License-Identifier: AGPL-3.0-or-later

import type {WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';
import type Stripe from 'stripe';
import {afterEach, describe, expect, test} from 'vitest';
import {createUserID} from '../../BrandedTypes';
import {PremiumStateReconciliationQueueService} from '../../infrastructure/PremiumStateReconciliationQueueService';
import {MockKVProvider} from '../../test/mocks/MockKVProvider';
import {NoopLogger} from '../../test/mocks/NoopLogger';
import type {UserRepository} from '../../user/repositories/UserRepository';
import processPremiumStateReconciliationQueue from '../tasks/ProcessPremiumStateReconciliationQueue';
import {clearWorkerDependencies, setWorkerDependenciesForTest} from '../WorkerContext';

const USER_ID = createUserID(834271905123471361n);
const ONE_HOUR_MS = 60 * 60 * 1000;

function createHelpers(): WorkerTaskHelpers {
	return {
		logger: new NoopLogger(),
		jobId: 1n,
		addJob: async () => 0n,
		reportProgress: async () => {},
		shouldCancel: async () => false,
		setContextLink: async () => {},
	};
}

function createQueueService(): PremiumStateReconciliationQueueService {
	return new PremiumStateReconciliationQueueService(new MockKVProvider());
}

describe('processPremiumStateReconciliationQueue', () => {
	afterEach(() => {
		clearWorkerDependencies();
	});

	test('keeps the user in the queue when the worker dies mid-reconciliation', async () => {
		const queueService = createQueueService();
		await queueService.enqueueUser(USER_ID, new Date(Date.now() - 1000));

		let signalReconcileStarted: () => void = () => {};
		const reconcileStarted = new Promise<void>((resolve) => {
			signalReconcileStarted = resolve;
		});
		const userRepository = {
			findUnique: async () => {
				signalReconcileStarted();
				return await new Promise<never>(() => {});
			},
		} as unknown as UserRepository;

		setWorkerDependenciesForTest({
			premiumStateReconciliationQueueService: queueService,
			stripe: {} as Stripe,
			userRepository,
		});

		void processPremiumStateReconciliationQueue({}, createHelpers());
		await reconcileStarted;

		expect(await queueService.getQueueSize()).toBe(1);
		expect(await queueService.getReadyUserIds(Date.now(), 10)).toEqual([]);
		expect(await queueService.getReadyUserIds(Date.now() + ONE_HOUR_MS, 10)).toEqual([USER_ID]);
	});

	test('removes the user from the queue once reconciliation commits', async () => {
		const queueService = createQueueService();
		await queueService.enqueueUser(USER_ID, new Date(Date.now() - 1000));

		const userRepository = {
			findUnique: async () => null,
		} as unknown as UserRepository;

		setWorkerDependenciesForTest({
			premiumStateReconciliationQueueService: queueService,
			stripe: {} as Stripe,
			userRepository,
		});

		await processPremiumStateReconciliationQueue({}, createHelpers());

		expect(await queueService.getQueueSize()).toBe(0);
		expect(await queueService.getReadyUserIds(Date.now() + ONE_HOUR_MS, 10)).toEqual([]);
	});

	test('rejects a second claim until the lease expires', async () => {
		const queueService = createQueueService();
		await queueService.enqueueUser(USER_ID, new Date(Date.now() - 1000));
		const now = Date.now();

		expect(await queueService.claimUser(USER_ID, now, now + ONE_HOUR_MS)).toBe(true);
		expect(await queueService.claimUser(USER_ID, now, now + ONE_HOUR_MS)).toBe(false);
		expect(await queueService.claimUser(USER_ID, now + ONE_HOUR_MS, now + 2 * ONE_HOUR_MS)).toBe(true);
	});

	test('reconciles the user once when two runs claim the same entry', async () => {
		const queueService = createQueueService();
		await queueService.enqueueUser(USER_ID, new Date(Date.now() - 1000));

		let signalFirstClaimEntered: () => void = () => {};
		const firstClaimEntered = new Promise<void>((resolve) => {
			signalFirstClaimEntered = resolve;
		});
		let releaseFirstClaim: () => void = () => {};
		const firstClaimReleased = new Promise<void>((resolve) => {
			releaseFirstClaim = resolve;
		});
		const claimUser = queueService.claimUser.bind(queueService);
		let firstClaim = true;
		queueService.claimUser = async (userId, nowMs, leaseUntilMs) => {
			if (firstClaim) {
				firstClaim = false;
				signalFirstClaimEntered();
				await firstClaimReleased;
			}
			return await claimUser(userId, nowMs, leaseUntilMs);
		};

		let signalReconcileStarted: () => void = () => {};
		const reconcileStarted = new Promise<void>((resolve) => {
			signalReconcileStarted = resolve;
		});
		let releaseReconcile: () => void = () => {};
		const reconcileReleased = new Promise<void>((resolve) => {
			releaseReconcile = resolve;
		});
		let findUniqueCalls = 0;
		const userRepository = {
			findUnique: async () => {
				findUniqueCalls += 1;
				signalReconcileStarted();
				await reconcileReleased;
				return null;
			},
		} as unknown as UserRepository;

		setWorkerDependenciesForTest({
			premiumStateReconciliationQueueService: queueService,
			stripe: {} as Stripe,
			userRepository,
		});

		const firstRun = processPremiumStateReconciliationQueue({}, createHelpers());
		await firstClaimEntered;
		const secondRun = processPremiumStateReconciliationQueue({}, createHelpers());
		await reconcileStarted;
		releaseFirstClaim();
		releaseReconcile();
		await Promise.all([firstRun, secondRun]);

		expect(findUniqueCalls).toBe(1);
		expect(await queueService.getQueueSize()).toBe(0);
	});
});
