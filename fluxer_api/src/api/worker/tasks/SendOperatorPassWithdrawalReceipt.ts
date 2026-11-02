// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createOperatorPassFulfilmentService,
	decodeOperatorPassWithdrawalReceipt,
} from '@app/api/operator_pass/OperatorPassFulfilmentService';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

const sendOperatorPassWithdrawalReceipt: WorkerTaskHandler = async (payload, helpers) => {
	const deps = getWorkerDependencies();
	const fulfilment = createOperatorPassFulfilmentService({
		stripe: deps.stripe,
		userRepository: deps.userRepository,
		emailService: deps.emailService,
		cacheService: deps.cacheService,
		services: deps.operatorPassServices,
	});
	if (!fulfilment) {
		return;
	}
	const receipt = decodeOperatorPassWithdrawalReceipt(payload);
	try {
		await fulfilment.sendWithdrawalReceipt(receipt, !helpers.attempt?.isLastAttempt);
	} catch (error) {
		if (helpers.attempt?.isLastAttempt) {
			helpers.logger.error(
				{error, sessionId: receipt.purchaseId, passId: receipt.passId},
				'Gave up sending the Operator Pass withdrawal receipt',
			);
		}
		throw error;
	}
};

export default sendOperatorPassWithdrawalReceipt;
