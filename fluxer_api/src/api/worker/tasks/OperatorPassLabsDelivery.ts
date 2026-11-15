// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID} from '@app/api/BrandedTypes';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {z} from 'zod';

const PAGE_SIZE = 500;

const DeliveryJobSchema = z.object({
	user_id: z.string().regex(/^\d+$/),
	greeting: z.boolean().nullable(),
	origin: z.string().min(1),
	retry: z.number().int().nonnegative(),
});

const operatorPassLabsDelivery: WorkerTaskHandler = async (payload, helpers) => {
	const deps = getWorkerDependencies();
	const roles = deps.operatorPassServices.roleService;
	if (payload.user_id !== undefined) {
		const job = DeliveryJobSchema.parse(payload);
		await roles.retryDelivery(
			{userId: createUserID(BigInt(job.user_id)), greeting: job.greeting, origin: job.origin, retry: job.retry},
			deps.userRepository,
		);
		return;
	}
	const summary = await roles.queueMissingLabsMembers(deps.userRepository, PAGE_SIZE);
	if (summary && summary.queued > 0) {
		helpers.logger.info(summary, 'Queued Operators who should be in Fluxer Labs but are not');
	}
};

export default operatorPassLabsDelivery;
