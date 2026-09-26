// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID} from '@app/api/BrandedTypes';
import type {UserRow} from '@app/api/database/types/UserTypes';
import {EMPTY_USER_ROW} from '@app/api/database/types/UserTypes';
import {PremiumStateReconciliationQueueService} from '@app/api/infrastructure/PremiumStateReconciliationQueueService';
import {User} from '@app/api/models/User';
import {MockKVProvider} from '@app/api/test/mocks/MockKVProvider';
import {NoopLogger} from '@app/api/test/mocks/NoopLogger';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import processPremiumStateReconciliationQueue from '@app/api/worker/tasks/ProcessPremiumStateReconciliationQueue';
import {clearWorkerDependencies, setWorkerDependenciesForTest} from '@app/api/worker/WorkerContext';
import {PremiumFlags} from '@fluxer/constants/src/UserConstants';
import type {WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';
import type Stripe from 'stripe';
import {afterEach, describe, expect, test} from 'vitest';

const USER_ID = createUserID(834271905123471361n);
const ONE_HOUR_MS = 60 * 60 * 1000;
const ONE_DAY_MS = 24 * ONE_HOUR_MS;

function createPremiumUser(overrides: Partial<UserRow> = {}): User {
	return new User({
		...EMPTY_USER_ROW,
		user_id: USER_ID,
		username: 'paidlate',
		discriminator: 1,
		premium_type: 1,
		premium_since: new Date(Date.now() - 200 * ONE_DAY_MS),
		premium_until: new Date(Date.now() + 20 * ONE_DAY_MS),
		stripe_customer_id: 'cus_test',
		stripe_subscription_id: 'sub_test',
		...overrides,
	});
}

function createCancelledSubscription(endedAtMs: number): Stripe.Subscription {
	return {
		id: 'sub_test',
		status: 'canceled',
		customer: 'cus_test',
		ended_at: Math.floor(endedAtMs / 1000),
		canceled_at: Math.floor(endedAtMs / 1000),
		cancel_at: null,
		cancel_at_period_end: false,
		trial_end: null,
		start_date: Math.floor((Date.now() - 200 * ONE_DAY_MS) / 1000),
		items: {data: []},
	} as unknown as Stripe.Subscription;
}

function createActiveSubscription(periodEndMs: number): Stripe.Subscription {
	return {
		id: 'sub_test',
		status: 'active',
		customer: 'cus_test',
		ended_at: null,
		canceled_at: null,
		cancel_at: null,
		cancel_at_period_end: false,
		trial_end: null,
		start_date: Math.floor((Date.now() - 200 * ONE_DAY_MS) / 1000),
		items: {
			data: [
				{
					current_period_end: Math.floor(periodEndMs / 1000),
					price: {recurring: {interval: 'month'}},
				},
			],
		},
	} as unknown as Stripe.Subscription;
}

function createPaidInvoice(periodEndMs: number): Stripe.Invoice {
	return {
		id: 'in_test',
		status: 'paid',
		lines: {data: [{period: {start: 0, end: Math.floor(periodEndMs / 1000)}}]},
	} as unknown as Stripe.Invoice;
}

function createStripeStub(subscription: Stripe.Subscription, invoices: Array<Stripe.Invoice>): Stripe {
	return {
		subscriptions: {
			retrieve: async () => subscription,
			list: async () => ({data: [subscription]}),
		},
		invoices: {
			list: async () => ({data: invoices}),
		},
	} as unknown as Stripe;
}

function createCapturingDeps(user: User): {
	userRepository: UserRepository;
	patches: Array<Partial<UserRow>>;
	extras: Record<string, unknown>;
} {
	const patches: Array<Partial<UserRow>> = [];
	const userRepository = {
		findUnique: async () => user,
		patchUpsert: async (_id: unknown, patch: Partial<UserRow>) => {
			patches.push(patch);
			return new User({...user.toRow(), ...patch});
		},
	} as unknown as UserRepository;
	const extras = {
		userCacheService: {setUserPartialResponseFromUserInBackground: () => {}},
		gatewayService: {dispatchPresence: async () => {}},
	};
	return {userRepository, patches, extras};
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

	test('keeps premium when a paid invoice covers a period beyond the cancellation', async () => {
		const queueService = createQueueService();
		await queueService.enqueueUser(USER_ID, new Date(Date.now() - 1000));

		const endedAtMs = Date.now() - 3 * ONE_DAY_MS;
		const paidThroughMs = Math.floor((Date.now() + 20 * ONE_DAY_MS) / 1000) * 1000;
		const user = createPremiumUser({premium_until: new Date(paidThroughMs)});
		const {userRepository, patches, extras} = createCapturingDeps(user);

		setWorkerDependenciesForTest({
			premiumStateReconciliationQueueService: queueService,
			stripe: createStripeStub(createCancelledSubscription(endedAtMs), [createPaidInvoice(paidThroughMs)]),
			userRepository,
			...extras,
		});

		await processPremiumStateReconciliationQueue({}, createHelpers());

		expect(patches).toHaveLength(1);
		const patch = patches[0];
		expect(patch.premium_type).toBeUndefined();
		expect(patch.premium_since).toBeUndefined();
		expect(patch.premium_until).toBeUndefined();
		expect(patch.premium_will_cancel).toBe(true);
	});

	test('extends premium to the paid period instead of the cancellation date', async () => {
		const queueService = createQueueService();
		await queueService.enqueueUser(USER_ID, new Date(Date.now() - 1000));

		const endedAtMs = Date.now() - 3 * ONE_DAY_MS;
		const paidThroughMs = Date.now() + 20 * ONE_DAY_MS;
		const user = createPremiumUser({premium_until: new Date(Date.now() + 5 * ONE_DAY_MS)});
		const {userRepository, patches, extras} = createCapturingDeps(user);

		setWorkerDependenciesForTest({
			premiumStateReconciliationQueueService: queueService,
			stripe: createStripeStub(createCancelledSubscription(endedAtMs), [createPaidInvoice(paidThroughMs)]),
			userRepository,
			...extras,
		});

		await processPremiumStateReconciliationQueue({}, createHelpers());

		expect(patches).toHaveLength(1);
		expect(patches[0].premium_until).toEqual(new Date(Math.floor(paidThroughMs / 1000) * 1000));
		expect(patches[0].premium_will_cancel).toBe(true);
		expect(patches[0].premium_type).toBeUndefined();
	});

	test('still strips premium when no paid invoice covers a future period', async () => {
		const queueService = createQueueService();
		await queueService.enqueueUser(USER_ID, new Date(Date.now() - 1000));

		const endedAtMs = Date.now() - 3 * ONE_DAY_MS;
		const paidThroughMs = Date.now() - 3 * ONE_DAY_MS;
		const user = createPremiumUser();
		const {userRepository, patches, extras} = createCapturingDeps(user);

		setWorkerDependenciesForTest({
			premiumStateReconciliationQueueService: queueService,
			stripe: createStripeStub(createCancelledSubscription(endedAtMs), [createPaidInvoice(paidThroughMs)]),
			userRepository,
			...extras,
		});

		await processPremiumStateReconciliationQueue({}, createHelpers());

		expect(patches).toHaveLength(1);
		expect(patches[0].premium_type).toBeNull();
		expect(patches[0].premium_until).toBeNull();
		expect(patches[0].premium_since).toBeNull();
	});
	test('clears the perks-sanitized latch once the subscription is active again', async () => {
		const queueService = createQueueService();
		await queueService.enqueueUser(USER_ID, new Date(Date.now() - 1000));

		const periodEndMs = Math.floor((Date.now() + 20 * ONE_DAY_MS) / 1000) * 1000;
		const user = createPremiumUser({
			premium_until: new Date(periodEndMs),
			premium_flags: PremiumFlags.PERKS_SANITIZED,
		});
		const {userRepository, patches, extras} = createCapturingDeps(user);

		setWorkerDependenciesForTest({
			premiumStateReconciliationQueueService: queueService,
			stripe: createStripeStub(createActiveSubscription(periodEndMs), []),
			userRepository,
			...extras,
		});

		await processPremiumStateReconciliationQueue({}, createHelpers());

		expect(patches).toHaveLength(1);
		expect(patches[0].premium_flags).toBe(0);
	});
});
