// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID} from '@app/api/BrandedTypes';
import {getConfig} from '@app/api/Config';
import type {StorePurchaseRow} from '@app/api/database/types/StoreBillingTypes';
import type {UserRow} from '@app/api/database/types/UserTypes';
import {EMPTY_USER_ROW} from '@app/api/database/types/UserTypes';
import {getCachedInstancePremiumMode, setCachedInstancePremiumMode} from '@app/api/limits/InstancePremiumModeCache';
import {User} from '@app/api/models/User';
import type {StoreEntitlementService} from '@app/api/store_billing/StoreEntitlementService';
import {buildStorePurchaseRow} from '@app/api/store_billing/tests/StoreBillingTestUtils';
import {NoopLogger} from '@app/api/test/mocks/NoopLogger';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import processExpiredPremiumSweep from '@app/api/worker/tasks/ProcessExpiredPremiumSweep';
import {clearWorkerDependencies, setWorkerDependenciesForTest} from '@app/api/worker/WorkerContext';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';
import type {WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';
import {ms} from 'itty-time';
import {afterEach, describe, expect, test} from 'vitest';

function createHarness() {
	const scanLimits: Array<number> = [];
	const userRepository = {
		async scanAllUsersPage(limit: number): Promise<{users: []; pageState: null}> {
			scanLimits.push(limit);
			return {users: [], pageState: null};
		},
	} as unknown as UserRepository;
	setWorkerDependenciesForTest({userRepository});
	return {scanLimits};
}

const EXPIRED_USER_ID = createUserID(834271905123471362n);

function createExpiredStoreUserHarness(storeRows: Array<StorePurchaseRow>) {
	const user = new User({
		...EMPTY_USER_ROW,
		user_id: EXPIRED_USER_ID,
		username: 'storelapse',
		discriminator: 1,
		premium_type: UserPremiumTypes.SUBSCRIPTION,
		premium_since: new Date(Date.now() - ms('60 days')),
		premium_until: new Date(Date.now() - ms('10 days')),
		premium_grace_ends_at: new Date(Date.now() - ms('10 days')),
	});
	const patches: Array<Partial<UserRow>> = [];
	const jobs: Array<{task: string; payload: unknown; jobKey: string | undefined}> = [];
	const userRepository = {
		async scanAllUsersPage(): Promise<{users: Array<User>; pageState: null}> {
			return {users: [user], pageState: null};
		},
		async patchUpsert(_userId: unknown, patch: Partial<UserRow>): Promise<User> {
			patches.push(patch);
			return new User({...user.toRow(), ...patch});
		},
		async getUserGuildIds(): Promise<Array<never>> {
			return [];
		},
	} as unknown as UserRepository;
	const storeEntitlementService = {
		async listStorePurchases() {
			return storeRows;
		},
	} as unknown as StoreEntitlementService;
	const workerService = {
		async addJob(task: string, payload: unknown, options?: {jobKey?: string}) {
			jobs.push({task, payload, jobKey: options?.jobKey});
			return 0n;
		},
	} as unknown as IWorkerService<never>;
	setWorkerDependenciesForTest({
		userRepository,
		storeEntitlementService,
		workerService,
		userCacheService: {setUserPartialResponseFromUserInBackground: () => {}} as never,
		gatewayService: {dispatchPresence: async () => {}} as never,
	});
	return {patches, jobs};
}

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

async function withSelfHosted(
	selfHosted: boolean,
	callback: () => Promise<void>,
	premiumMode: 'mirror' | 'everyone' = 'everyone',
): Promise<void> {
	const config = getConfig();
	const originalSelfHosted = config.instance.selfHosted;
	const originalPremiumMode = getCachedInstancePremiumMode();
	try {
		config.instance.selfHosted = selfHosted;
		setCachedInstancePremiumMode(premiumMode);
		await callback();
	} finally {
		config.instance.selfHosted = originalSelfHosted;
		setCachedInstancePremiumMode(originalPremiumMode);
	}
}

describe('processExpiredPremiumSweep', () => {
	afterEach(() => {
		clearWorkerDependencies();
	});

	test('scans no users on a self-hosted instance where everyone is premium', async () => {
		const harness = createHarness();

		await withSelfHosted(true, async () => {
			await processExpiredPremiumSweep({}, createHelpers());
		});

		expect(harness.scanLimits).toEqual([]);
	});

	test('scans users on a self-hosted instance in mirror mode', async () => {
		const harness = createHarness();

		await withSelfHosted(
			true,
			async () => {
				await processExpiredPremiumSweep({}, createHelpers());
			},
			'mirror',
		);

		expect(harness.scanLimits).toEqual([100]);
	});

	test('refreshes a stale store subscription instead of stripping premium', async () => {
		const row = buildStorePurchaseRow({
			user_id: EXPIRED_USER_ID,
			expires_at: new Date(Date.now() - ms('2 hours')),
			synced_at: new Date(Date.now() - ms('3 hours')),
		});
		const harness = createExpiredStoreUserHarness([row]);

		await withSelfHosted(false, async () => {
			await processExpiredPremiumSweep({}, createHelpers());
		});

		expect(harness.patches).toEqual([]);
		expect(harness.jobs).toEqual([
			{
				task: 'refreshStorePurchase',
				payload: {storeKey: row.store_key},
				jobKey: `refreshStorePurchase:${row.store_key}`,
			},
		]);
	});

	test('strips premium when the store subscription was synced recently', async () => {
		const harness = createExpiredStoreUserHarness([
			buildStorePurchaseRow({
				user_id: EXPIRED_USER_ID,
				expires_at: new Date(Date.now() - ms('10 days')),
				synced_at: new Date(Date.now() - ms('5 minutes')),
			}),
			buildStorePurchaseRow({
				user_id: EXPIRED_USER_ID,
				state: 'expired',
				entitled: false,
				synced_at: new Date(Date.now() - ms('2 days')),
			}),
		]);

		await withSelfHosted(false, async () => {
			await processExpiredPremiumSweep({}, createHelpers());
		});

		expect(harness.jobs).toEqual([]);
		expect(harness.patches[0]).toMatchObject({premium_type: null, premium_until: null});
	});

	test('strips premium once a sync after the access end confirmed the store state', async () => {
		const harness = createExpiredStoreUserHarness([
			buildStorePurchaseRow({
				user_id: EXPIRED_USER_ID,
				state: 'billing_retry',
				entitled: false,
				expires_at: new Date(Date.now() - ms('3 hours')),
				synced_at: new Date(Date.now() - ms('1 hour')),
			}),
		]);

		await withSelfHosted(false, async () => {
			await processExpiredPremiumSweep({}, createHelpers());
		});

		expect(harness.jobs).toEqual([]);
		expect(harness.patches[0]).toMatchObject({premium_type: null, premium_until: null});
	});

	test('strips premium when the store access ended more than a day ago', async () => {
		const harness = createExpiredStoreUserHarness([
			buildStorePurchaseRow({
				user_id: EXPIRED_USER_ID,
				expires_at: new Date(Date.now() - ms('2 days')),
				synced_at: new Date(Date.now() - ms('3 days')),
			}),
		]);

		await withSelfHosted(false, async () => {
			await processExpiredPremiumSweep({}, createHelpers());
		});

		expect(harness.jobs).toEqual([]);
		expect(harness.patches[0]).toMatchObject({premium_type: null, premium_until: null});
	});

	test('strips premium when the store provider is not configured', async () => {
		const harness = createExpiredStoreUserHarness([
			buildStorePurchaseRow({
				user_id: EXPIRED_USER_ID,
				expires_at: new Date(Date.now() - ms('2 hours')),
				synced_at: new Date(Date.now() - ms('3 hours')),
			}),
		]);
		const config = getConfig();
		const originalEnabled = config.appStore.enabled;
		try {
			config.appStore.enabled = false;
			await withSelfHosted(false, async () => {
				await processExpiredPremiumSweep({}, createHelpers());
			});
		} finally {
			config.appStore.enabled = originalEnabled;
		}

		expect(harness.jobs).toEqual([]);
		expect(harness.patches[0]).toMatchObject({premium_type: null, premium_until: null});
	});

	test('scans users on a hosted instance', async () => {
		const harness = createHarness();

		await withSelfHosted(false, async () => {
			await processExpiredPremiumSweep({}, createHelpers());
		});

		expect(harness.scanLimits).toEqual([100]);
	});
});
