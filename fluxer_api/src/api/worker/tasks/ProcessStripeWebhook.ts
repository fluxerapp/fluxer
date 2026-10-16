// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {getProductRegistry} from '@app/api/stripe/ProductRegistry';
import {StripeService} from '@app/api/stripe/StripeService';
import {AgeVerificationService} from '@app/api/stripe/services/AgeVerificationService';
import {StripeWebhookService} from '@app/api/stripe/services/StripeWebhookService';
import {createWorkerEnforcementServices} from '@app/api/worker/tasks/admin_bulk/AdminBulkServices';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {z} from 'zod';

const PayloadSchema = z.object({
	body: z.string(),
	signature: z.string(),
});

const processStripeWebhook: WorkerTaskHandler = async (payload, helpers) => {
	const {body, signature} = PayloadSchema.parse(payload);
	const deps = getWorkerDependencies();
	const stripe = deps.stripe;
	if (!stripe) {
		if (Config.instance.selfHosted) {
			throw new Error('Stripe is not configured on this worker yet; retrying webhook event');
		}
		helpers.logger.warn('Stripe is not configured; discarding webhook event');
		return;
	}
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
	);
	const ageVerificationService = new AgeVerificationService(
		stripe,
		deps.userRepository,
		deps.gatewayService,
		deps.cacheService,
	);
	const webhookService = new StripeWebhookService(
		stripe,
		deps.userRepository,
		deps.userCacheService,
		deps.emailService,
		deps.gatewayService,
		productRegistry,
		deps.cacheService,
		stripeService.gifts,
		stripeService.premium,
		deps.donationRepository,
		deps.deletionQueueService,
		deps.premiumStateReconciliationQueueService,
		ageVerificationService,
		deps.adminRepository,
		deps.snowflakeService,
		deps.billingRepository,
		stripeService.refunds,
		createWorkerEnforcementServices(deps).enforcementService,
		deps.storeEntitlementService,
	);
	await webhookService.handleWebhook({body, signature});
};

export default processStripeWebhook;
