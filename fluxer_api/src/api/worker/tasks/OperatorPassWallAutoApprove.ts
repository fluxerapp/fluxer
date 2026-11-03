// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

const operatorPassWallAutoApprove: WorkerTaskHandler = async (_payload, helpers) => {
	if (Config.instance.selfHosted || !Config.operatorPass.wallAutoApprove) {
		return;
	}
	const approved = await getWorkerDependencies().operatorPassServices.wallService.approvePendingRevisions();
	if (approved > 0) {
		helpers.logger.info({approved}, "Approved pending Operators' wall names and avatars");
	}
};

export default operatorPassWallAutoApprove;
