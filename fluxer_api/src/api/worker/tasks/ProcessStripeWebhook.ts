// SPDX-License-Identifier: AGPL-3.0-or-later

import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {z} from 'zod';
import type {ISessionTerminator} from '../../auth/ISessionTerminator';
import type {UserID} from '../../BrandedTypes';
import type {IGatewayService} from '../../infrastructure/IGatewayService';
import {ProductRegistry} from '../../stripe/ProductRegistry';
import {AgeVerificationService} from '../../stripe/services/AgeVerificationService';
import {StripeCheckoutService} from '../../stripe/services/StripeCheckoutService';
import {StripeGiftService} from '../../stripe/services/StripeGiftService';
import {StripePremiumService} from '../../stripe/services/StripePremiumService';
import {StripeRefundService} from '../../stripe/services/StripeRefundService';
import {StripeSubscriptionService} from '../../stripe/services/StripeSubscriptionService';
import {StripeWebhookService} from '../../stripe/services/StripeWebhookService';
import type {IUserRepositoryAggregate} from '../../user/repositories/IUserRepositoryAggregate';
import {getWorkerDependencies} from '../WorkerContext';

const PayloadSchema = z.object({
	body: z.string(),
	signature: z.string(),
});

class WorkerSessionTerminator implements ISessionTerminator {
	constructor(
		private repository: IUserRepositoryAggregate,
		private gatewayService: IGatewayService,
	) {}

	async terminateAllUserSessions(userId: UserID): Promise<void> {
		const authSessions = await this.repository.listAuthSessions(userId);
		await this.repository.deleteAllPushSubscriptions(userId);
		await this.gatewayService.invalidatePushSubscriptions({userId});
		if (authSessions.length === 0) return;
		const hashes = authSessions.map((s) => s.sessionIdHash);
		await this.repository.deleteAuthSessions(userId, hashes);
		await this.gatewayService.terminateSession({
			userId,
			sessionIdHashes: authSessions.map((s) => Buffer.from(s.sessionIdHash).toString('base64url')),
		});
	}
}

const processStripeWebhook: WorkerTaskHandler = async (payload, helpers) => {
	const {body, signature} = PayloadSchema.parse(payload);
	const deps = getWorkerDependencies();
	if (!deps.stripe) {
		helpers.logger.warn('Stripe is not configured; discarding webhook event');
		return;
	}
	const productRegistry = new ProductRegistry();
	const sessionTerminator = new WorkerSessionTerminator(deps.userRepository, deps.gatewayService);
	const premiumService = new StripePremiumService(
		deps.userRepository,
		deps.gatewayService,
		deps.guildRepository,
		deps.guildService,
	);
	const checkoutService = new StripeCheckoutService(
		deps.stripe,
		deps.userRepository,
		productRegistry,
		deps.cacheService,
	);
	const subscriptionService = new StripeSubscriptionService(
		deps.stripe,
		deps.userRepository,
		productRegistry,
		deps.cacheService,
		deps.gatewayService,
	);
	const giftService = new StripeGiftService(
		deps.stripe,
		deps.userRepository,
		deps.cacheService,
		deps.gatewayService,
		checkoutService,
		premiumService,
		subscriptionService,
	);
	const ageVerificationService = deps.stripe
		? new AgeVerificationService(deps.stripe, deps.userRepository, deps.gatewayService, deps.cacheService)
		: null;
	const refundService = new StripeRefundService(deps.stripe, deps.userRepository, subscriptionService);
	const webhookService = new StripeWebhookService(
		deps.stripe,
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
