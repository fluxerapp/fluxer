// SPDX-License-Identifier: AGPL-3.0-or-later

import Stripe from 'stripe';
import {createTestAccount, type TestAccount} from '../../auth/tests/AuthTestUtils';
import type {UserID} from '../../BrandedTypes';
import {Config} from '../../Config';
import {
	getBillingRepository,
	getGatewayService,
	getSnowflakeService,
	setInjectedWorkerService,
} from '../../middleware/ServiceRegistry';
import {
	createUserCacheService,
	getAdminRepository,
	getCacheService,
	getDonationRepository,
	getEmailService,
	getGuildRepository,
	getKVAccountDeletionQueue,
	getPremiumStateReconciliationQueueService,
	getUserRepository,
} from '../../middleware/ServiceSingletons';
import type {ApiTestHarness} from '../../test/ApiTestHarness';
import {SyncTaskWorkerService} from '../../test/SyncTaskWorkerService';
import {createBuilder} from '../../test/TestRequestBuilder';
import {PaymentRepository} from '../../user/repositories/PaymentRepository';
import processStripeWebhook from '../../worker/tasks/ProcessStripeWebhook';
import {setWorkerDependenciesForTest} from '../../worker/WorkerContext';
import {STRIPE_API_VERSION} from '../StripeApiVersion';

export async function createTestUserWithPremium(
	harness: ApiTestHarness,
	premiumType: number,
	options?: {
		email?: string;
		username?: string;
		premiumUntil?: Date;
		stripeCustomerId?: string;
		stripeSubscriptionId?: string;
	},
): Promise<TestAccount> {
	const account = await createTestAccount(harness, {
		email: options?.email,
		username: options?.username,
	});
	await createBuilder(harness, account.token)
		.post(`/test/users/${account.userId}/premium`)
		.body({
			premium_type: premiumType,
			premium_until: options?.premiumUntil?.toISOString() || null,
			stripe_customer_id: options?.stripeCustomerId || null,
			stripe_subscription_id: options?.stripeSubscriptionId || null,
		})
		.execute();
	return account;
}

export async function createTestPayment(
	_harness: ApiTestHarness,
	userId: UserID,
	sessionId: string,
	options?: {
		priceId?: string;
		productType?: string;
		status?: string;
		isGift?: boolean;
		giftCode?: string;
		stripeCustomerId?: string;
		paymentIntentId?: string;
		subscriptionId?: string;
	},
): Promise<void> {
	const paymentRepository = new PaymentRepository();
	await paymentRepository.createPayment({
		checkout_session_id: sessionId,
		user_id: userId,
		price_id: options?.priceId || 'price_test_monthly',
		product_type: options?.productType || 'monthly_subscription',
		status: options?.status || 'pending',
		is_gift: options?.isGift || false,
		created_at: new Date(),
	});
	await paymentRepository.updatePayment({
		checkout_session_id: sessionId,
		gift_code: options?.giftCode || null,
		payment_intent_id: options?.paymentIntentId || null,
		stripe_customer_id: options?.stripeCustomerId || null,
		subscription_id: options?.subscriptionId || null,
	});
}

export function setupSyncStripeWebhookWorker(): void {
	const stripe = new Stripe(Config.stripe.secretKey || 'sk_test_fake', {
		apiVersion: STRIPE_API_VERSION,
		httpClient: Stripe.createFetchHttpClient(),
	});
	setWorkerDependenciesForTest({
		stripe,
		userRepository: getUserRepository(),
		userCacheService: createUserCacheService(),
		emailService: getEmailService(),
		gatewayService: getGatewayService(),
		cacheService: getCacheService(),
		guildRepository: getGuildRepository(),
		donationRepository: getDonationRepository(),
		deletionQueueService: getKVAccountDeletionQueue(),
		premiumStateReconciliationQueueService: getPremiumStateReconciliationQueueService(),
		adminRepository: getAdminRepository(),
		snowflakeService: getSnowflakeService(),
		billingRepository: getBillingRepository(),
	});
	setInjectedWorkerService(new SyncTaskWorkerService({processStripeWebhook}));
}

let originalWebhookSecretDescriptor: PropertyDescriptor | undefined;

export function mockStripeWebhookSecret(secret = 'whsec_test'): void {
	originalWebhookSecretDescriptor ??= Object.getOwnPropertyDescriptor(Config.stripe, 'webhookSecret');
	Object.defineProperty(Config.stripe, 'webhookSecret', {
		get: () => secret,
		configurable: true,
	});
}

export function restoreStripeWebhookSecret(): void {
	if (originalWebhookSecretDescriptor) {
		Object.defineProperty(Config.stripe, 'webhookSecret', originalWebhookSecretDescriptor);
		originalWebhookSecretDescriptor = undefined;
		return;
	}
	delete (
		Config.stripe as {
			webhookSecret?: string;
		}
	).webhookSecret;
}

export {} from '../../test/msw/handlers/StripeApiHandlers';
