// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID} from '@app/api/BrandedTypes';
import {Logger} from '@app/api/Logger';
import {createOperatorPassFulfilmentService} from '@app/api/operator_pass/OperatorPassFulfilmentService';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

const PAGE_SIZE = 500;

const operatorPassLabsVisionaryRoleSync: WorkerTaskHandler = async (payload) => {
	const deps = getWorkerDependencies();
	const {userRepository} = deps;
	const roles = deps.operatorPassServices.roleService;
	const visionary = createOperatorPassFulfilmentService({
		stripe: deps.stripe,
		userRepository,
		emailService: deps.emailService,
		cacheService: deps.cacheService,
		services: deps.operatorPassServices,
	})?.visionary;
	if (typeof payload.user_id === 'string') {
		const userId = createUserID(BigInt(payload.user_id));
		await visionary?.endIfVisionaryEnded(userId);
		await roles.retryLabsRoleSync(userId, typeof payload.retry === 'number' ? payload.retry + 1 : 0);
		return;
	}
	const ended = visionary ? await visionary.endAllEndedVisionaries(PAGE_SIZE) : null;
	if (ended && (ended.ended > 0 || ended.failed > 0)) {
		Logger.info(ended, 'Ended the Operator Pass of accounts whose Visionary ended');
	}
	const summary = await roles.syncLabsRolesForMembers(PAGE_SIZE);
	if (!summary) {
		Logger.warn('Skipped the Fluxer Labs role sync, Fluxer Labs is not configured');
	} else if (summary.granted > 0 || summary.removed > 0 || summary.failed > 0) {
		Logger.info(summary, 'Matched the Fluxer Labs roles to who has a pass or Visionary');
	}
	const failed = (summary?.failed ?? 0) + (ended?.failed ?? 0);
	if (failed > 0) throw new Error(`Labs Visionary role sync failed for ${failed} accounts`);
};

export default operatorPassLabsVisionaryRoleSync;
