// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

const operatorPassRemovalExpiry: WorkerTaskHandler = async (_payload, helpers) => {
	if (Config.instance.selfHosted) {
		return;
	}
	const deps = getWorkerDependencies();
	const service = deps.operatorPassServices.removalService;
	const restored = await service.restoreExpired();
	if (restored > 0) {
		helpers.logger.info({restored}, 'Restored Operator Pass perks after limited removals ended');
	}
};

export default operatorPassRemovalExpiry;
