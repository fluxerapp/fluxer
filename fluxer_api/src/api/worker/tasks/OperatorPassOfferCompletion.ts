// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {createOperatorPassFulfilmentService} from '@app/api/operator_pass/OperatorPassFulfilmentService';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

const operatorPassOfferCompletion: WorkerTaskHandler = async (_payload, helpers) => {
	if (Config.instance.selfHosted) {
		return;
	}
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
	const completed = await fulfilment.holdingService.completeDueOffers();
	if (completed > 0) {
		helpers.logger.info({completed}, 'Moved Operator Passes to the recipients of accepted offers');
	}
};

export default operatorPassOfferCompletion;
