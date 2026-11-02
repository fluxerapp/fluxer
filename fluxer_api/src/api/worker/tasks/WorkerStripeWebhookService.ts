// SPDX-License-Identifier: AGPL-3.0-or-later

import {getProductRegistry} from '@app/api/stripe/ProductRegistry';
import {StripeService} from '@app/api/stripe/StripeService';
import {StripeWebhookService} from '@app/api/stripe/services/StripeWebhookService';
import {createWorkerEnforcementServices} from '@app/api/worker/tasks/admin_bulk/AdminBulkServices';
import type {WorkerDependencies} from '@app/api/worker/WorkerDependencies';
import type Stripe from 'stripe';

export function createWorkerStripeWebhookService(deps: WorkerDependencies, stripe: Stripe): StripeWebhookService {
	const productRegistry = getProductRegistry();
	const stripeService = new StripeService(
		deps.userRepository,
		deps.gatewayService,
		deps.guildRepository,
		deps.guildService,
		deps.cacheService,
		deps.billingRepository,
		null,
		deps.storeEntitlementService,
		deps.operatorPassServices,
	);
	return new StripeWebhookService(
		stripe,
		deps.userRepository,
		deps.userCacheService,
		stripeService.subscriptions,
		deps.emailService,
		deps.gatewayService,
		productRegistry,
		deps.cacheService,
		stripeService.gifts,
		stripeService.premium,
		deps.donationRepository,
		deps.deletionQueueService,
		deps.premiumStateReconciliationQueueService,
		deps.adminRepository,
		deps.snowflakeService,
		deps.billingRepository,
		stripeService.refunds,
		createWorkerEnforcementServices(deps).enforcementService,
		deps.storeEntitlementService,
		stripeService.operatorPassFulfilment,
	);
}
