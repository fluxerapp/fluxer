// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID, type UserID} from '@app/api/BrandedTypes';
import type {PaymentDisputeReviewRow} from '@app/api/database/types/PaymentDisputeTypes';
import type {StorePurchaseRow} from '@app/api/database/types/StoreBillingTypes';
import {
	getGatewayService,
	getKVClient,
	getSnowflakeService,
	setInjectedWorkerService,
} from '@app/api/middleware/ServiceRegistry';
import {
	getPaymentDisputeRepository,
	getPremiumStateReconciliationQueueService,
	getStoreBillingRepository,
	getUserCacheService,
	getUserRepository,
} from '@app/api/middleware/ServiceSingletons';
import type {User} from '@app/api/models/User';
import {buildAppStoreStoreKey} from '@app/api/store_billing/StoreBillingTypes';
import type {StoreEntitlementService} from '@app/api/store_billing/StoreEntitlementService';
import {createStoreEntitlementService} from '@app/api/store_billing/StoreEntitlementServiceFactory';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopWorkerService} from '@app/api/test/NoopWorkerService';
import {SyncTaskWorkerService} from '@app/api/test/SyncTaskWorkerService';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import processAppStoreNotification from '@app/api/worker/tasks/ProcessAppStoreNotification';
import processGooglePlayNotification from '@app/api/worker/tasks/ProcessGooglePlayNotification';
import refreshStorePurchase from '@app/api/worker/tasks/RefreshStorePurchase';
import {clearWorkerDependencies, setWorkerDependenciesForTest} from '@app/api/worker/WorkerContext';
import type {StoreBillingContextResponse} from '@fluxer/schema/src/domains/premium/StoreBillingSchemas';
import {ms} from 'itty-time';

export function createTestStoreEntitlementService(): StoreEntitlementService {
	return createStoreEntitlementService({
		userRepository: getUserRepository(),
		userCacheService: getUserCacheService(),
		gatewayService: getGatewayService(),
		kvClient: getKVClient(),
		snowflakeService: getSnowflakeService(),
		premiumStateReconciliationQueueService: getPremiumStateReconciliationQueueService(),
	});
}

export function installStoreBillingWorker(): StoreEntitlementService {
	const storeEntitlementService = createTestStoreEntitlementService();
	const workerService = new SyncTaskWorkerService({
		processAppStoreNotification,
		processGooglePlayNotification,
		refreshStorePurchase,
	});
	setWorkerDependenciesForTest({kvClient: getKVClient(), storeEntitlementService, workerService});
	setInjectedWorkerService(workerService);
	return storeEntitlementService;
}

export function uninstallStoreBillingWorker(): void {
	clearWorkerDependencies();
	setInjectedWorkerService(new NoopWorkerService());
}

let seededPurchaseCounter = 0;

export function buildStorePurchaseRow(overrides: Partial<StorePurchaseRow> = {}): StorePurchaseRow {
	seededPurchaseCounter += 1;
	const now = new Date();
	const reference = `3000000${Date.now().toString().slice(-6)}${seededPurchaseCounter.toString().padStart(4, '0')}`;
	return {
		store_key: buildAppStoreStoreKey('production', reference),
		id: BigInt(`1${reference}`),
		provider: 'app_store',
		kind: 'subscription',
		slot: 'monthly',
		environment: 'production',
		app_id: 'com.fluxer',
		product_id: 'com.fluxer.plutonium.monthly',
		base_plan_id: null,
		store_reference: reference,
		latest_transaction_id: reference,
		app_transaction_id: null,
		user_id: null,
		bound_at: null,
		released_at: null,
		account_token: null,
		ownership_type: 'PURCHASED',
		state: 'active',
		store_state: '1',
		entitled: true,
		expires_at: new Date(now.getTime() + ms('20 days')),
		grace_ends_at: null,
		auto_renew: true,
		auto_renew_product_id: 'com.fluxer.plutonium.monthly',
		started_at: new Date(now.getTime() - ms('10 days')),
		purchased_at: new Date(now.getTime() - ms('10 days')),
		revoked_at: null,
		revocation_reason: null,
		linked_store_key: null,
		superseded_by_store_key: null,
		acknowledged: true,
		gift_code: null,
		region: null,
		last_event_at: now,
		synced_at: now,
		created_at: now,
		updated_at: now,
		version: 1,
		...overrides,
	};
}

export async function seedStorePurchase(
	userId: UserID,
	overrides: Partial<StorePurchaseRow> = {},
): Promise<StorePurchaseRow> {
	const row = buildStorePurchaseRow({user_id: userId, bound_at: new Date(), ...overrides});
	if (!(await getStoreBillingRepository().insertPurchase(row))) {
		throw new Error(`Store purchase ${row.store_key} already exists`);
	}
	return row;
}

export async function seedOpenPaymentDisputeReview(
	purchaserId: UserID,
	disputeId: string,
): Promise<PaymentDisputeReviewRow> {
	const now = new Date();
	const review: PaymentDisputeReviewRow = {
		dispute_id: disputeId,
		product: 'plutonium',
		order_id: `cs_${disputeId}`,
		invoice_number: null,
		purchaser_id: purchaserId,
		is_gift: false,
		reason: 'general',
		payment_intent_id: `pi_${disputeId}`,
		charge_id: `ch_${disputeId}`,
		amount_minor: 499,
		currency: 'USD',
		evidence_submitted: false,
		evidence_due_by: new Date(now.getTime() + ms('10 days')),
		disputed_at: now,
		closed_at: null,
		outcome: null,
		purchased_at: new Date(now.getTime() - ms('30 days')),
		perks_started_at: new Date(now.getTime() - ms('30 days')),
		withdrawal_ends_at: new Date(now.getTime() - ms('16 days')),
		withdrawn_at: null,
		refunded_at: null,
		perks_removed_at: null,
		status: 'open',
		created_at: now,
		decided_at: null,
		decided_by: null,
		decision_note: null,
		warning_deadline: null,
		warned_at: null,
		warning_email_sent_at: null,
		warning_dm_sent_at: null,
		enforcement_decision_id: null,
		reversed_at: null,
		reversed_by: null,
		reversal_note: null,
		dispute_purchase_at: null,
		version: 1,
	};
	if (!(await getPaymentDisputeRepository().createReview(review))) {
		throw new Error(`Payment dispute review ${disputeId} already exists`);
	}
	return review;
}

export async function findUser(userId: string): Promise<User> {
	const user = await getUserRepository().findUnique(createUserID(BigInt(userId)));
	if (!user) {
		throw new Error(`Test user ${userId} not found`);
	}
	return user;
}

export async function getStoreContext(harness: ApiTestHarness, token: string): Promise<StoreBillingContextResponse> {
	return createBuilder<StoreBillingContextResponse>(harness, token).get('/premium/store').execute();
}

export async function setTestPremium(
	harness: ApiTestHarness,
	token: string,
	userId: string,
	body: Record<string, unknown>,
): Promise<void> {
	await createBuilder(harness, token).post(`/test/users/${userId}/premium`).body(body).execute();
}

export function expectCloseTo(actual: Date | null | undefined, expected: number, toleranceMs = 5000): void {
	if (!actual) {
		throw new Error('Expected a date, got null');
	}
	const delta = Math.abs(actual.getTime() - expected);
	if (delta > toleranceMs) {
		throw new Error(`Expected ${new Date(expected).toISOString()}, got ${actual.toISOString()}`);
	}
}
