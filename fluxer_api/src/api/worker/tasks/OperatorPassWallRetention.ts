// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {getPaymentDisputeRepository} from '@app/api/middleware/ServiceSingletons';
import {hasOpenPaymentDispute} from '@app/api/payment_disputes/PaymentDisputeReviews';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

const operatorPassWallRetention: WorkerTaskHandler = async (_payload, helpers) => {
	if (Config.instance.selfHosted) {
		return;
	}
	const deps = getWorkerDependencies();
	const service = deps.operatorPassServices.wallService;
	const disputes = getPaymentDisputeRepository();
	const result = await service.retention.purgeExpired({isOnHold: (userId) => hasOpenPaymentDispute(disputes, userId)});
	if (result.redacted > 0 || result.purged > 0) {
		helpers.logger.info(result, "Deleted expired Operators' wall names and avatars");
	}
};

export default operatorPassWallRetention;
