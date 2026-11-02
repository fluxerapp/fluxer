// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {syncOperatorPassDeliveryRecord} from '@app/api/operator_pass/OperatorPassFulfilmentService';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

const recordOperatorPassDelivery: WorkerTaskHandler = async (payload, helpers) => {
	if (Config.instance.selfHosted) return;
	const deps = getWorkerDependencies();
	const purchase = await deps.operatorPassRepository.findPurchase(String(payload.purchase_id));
	if (!purchase?.delivered_at || !deps.stripe) return;
	try {
		await syncOperatorPassDeliveryRecord(deps.stripe, deps.operatorPassRepository, purchase);
	} catch (error) {
		if (helpers.attempt?.isLastAttempt) {
			helpers.logger.error(
				{error, sessionId: purchase.checkout_session_id},
				'Gave up recording the Operator Pass certificate delivery time on the payment',
			);
		}
		throw error;
	}
};

export default recordOperatorPassDelivery;
