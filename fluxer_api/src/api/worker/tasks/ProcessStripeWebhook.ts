// SPDX-License-Identifier: AGPL-3.0-or-later

import {revokeAllAuthSessions} from '@app/api/auth/AuthSessionRevocation';
import type {ISessionTerminator} from '@app/api/auth/ISessionTerminator';
import {Config} from '@app/api/Config';
import {getProductRegistry} from '@app/api/stripe/ProductRegistry';
import {AgeVerificationService} from '@app/api/stripe/services/AgeVerificationService';
import {StripeCheckoutService} from '@app/api/stripe/services/StripeCheckoutService';
import {StripeGiftService} from '@app/api/stripe/services/StripeGiftService';
import {StripePremiumService} from '@app/api/stripe/services/StripePremiumService';
import {StripeRefundService} from '@app/api/stripe/services/StripeRefundService';
import {StripeSubscriptionService} from '@app/api/stripe/services/StripeSubscriptionService';
import {StripeWebhookService} from '@app/api/stripe/services/StripeWebhookService';
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
	const sessionTerminator: ISessionTerminator = {
		async terminateAllUserSessions(userId) {
			await revokeAllAuthSessions({users: deps.userRepository, gateway: deps.gatewayService}, userId);
		},
	};
	const premiumService = new StripePremiumService(
		deps.userRepository,
		deps.gatewayService,
		deps.guildRepository,
		deps.guildService,
	);
	const checkoutService = new StripeCheckoutService(stripe, deps.userRepository, productRegistry, deps.cacheService);
	const subscriptionService = new StripeSubscriptionService(
		stripe,
		deps.userRepository,
		productRegistry,
		deps.cacheService,
		deps.gatewayService,
	);
	const giftService = new StripeGiftService(
		stripe,
		deps.userRepository,
		deps.cacheService,
		deps.gatewayService,
		checkoutService,
		premiumService,
		subscriptionService,
	);
	const ageVerificationService = new AgeVerificationService(
		stripe,
		deps.userRepository,
		deps.gatewayService,
		deps.cacheService,
	);
	const refundService = new StripeRefundService(stripe, deps.userRepository, subscriptionService);
	const webhookService = new StripeWebhookService(
		stripe,
		checkoutService,
		deps.userRepository,
		deps.userCacheService,
		sessionTerminator,
		deps.emailService,
		deps.gatewayService,
		productRegistry,
		deps.cacheService,
		giftService,
		premiumService,
		deps.donationRepository,
		deps.deletionQueueService,
		deps.premiumStateReconciliationQueueService,
		ageVerificationService,
		deps.adminRepository,
		deps.snowflakeService,
		deps.billingRepository,
		refundService,
	);
	await webhookService.handleWebhook({body, signature});
};

export default processStripeWebhook;
