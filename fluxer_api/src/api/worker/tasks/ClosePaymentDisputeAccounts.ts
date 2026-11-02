// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID} from '@app/api/BrandedTypes';
import {OAuth2TokenRepository} from '@app/api/oauth/repositories/OAuth2TokenRepository';
import {PaymentDisputeEnforcementService} from '@app/api/payment_disputes/PaymentDisputeEnforcement';
import {PaymentDisputeRepository} from '@app/api/payment_disputes/PaymentDisputeRepository';
import {createWorkerEnforcementServices} from '@app/api/worker/tasks/admin_bulk/AdminBulkServices';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {ms} from 'itty-time';

const SWEEP_MIN_CLOSED_AGE_MS = ms('10 minutes');

const closePaymentDisputeAccounts: WorkerTaskHandler = async (payload, helpers) => {
	const deps = getWorkerDependencies();
	const repository = new PaymentDisputeRepository();
	const purchaserId = typeof payload.purchaser_id === 'string' ? createUserID(BigInt(payload.purchaser_id)) : null;
	const sweepBefore = Date.now() - SWEEP_MIN_CLOSED_AGE_MS;
	const reviews = purchaserId
		? await repository.listReviewsByPurchaser(purchaserId)
		: (await repository.listReviews()).filter((review) => (review.closed_at?.getTime() ?? Infinity) <= sweepBefore);
	const {auditService, enforcementService} = createWorkerEnforcementServices(deps);
	const service = new PaymentDisputeEnforcementService({
		repository,
		userRepository: deps.userRepository,
		userCacheService: deps.userCacheService,
		emailService: deps.emailService,
		cacheService: deps.cacheService,
		gatewayService: deps.gatewayService,
		auditService,
		kvDeletionQueue: deps.deletionQueueService,
		oauth2Tokens: new OAuth2TokenRepository(),
		systemDm: null,
		enforcement: enforcementService,
		operatorPass: deps.operatorPassServices,
	});
	const failed = await service.closeWaitingAccounts(reviews);
	if (failed > 0 && purchaserId) {
		throw new Error(`Failed to close the account after ${failed} payment disputes, retrying`);
	}
	if (failed > 0) {
		helpers.logger.warn({failed}, 'Some lost unjustified payment disputes could not close their account yet');
	}
};

export default closePaymentDisputeAccounts;
