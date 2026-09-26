// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID} from '@app/api/BrandedTypes';
import {Logger} from '@app/api/Logger';
import {processUserDeletion} from '@app/api/user/services/UserDeletionService';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {z} from 'zod';

const PayloadSchema = z.object({
	userId: z.string(),
	deletionReasonCode: z.number(),
	pendingDeletionAt: z.iso.datetime().optional(),
});
const userProcessPendingDeletion: WorkerTaskHandler = async (payload, helpers) => {
	const validated = PayloadSchema.parse(payload);
	helpers.logger.debug({payload: validated}, 'Processing userProcessPendingDeletion task');
	const userId = createUserID(BigInt(validated.userId));
	try {
		const deps = getWorkerDependencies();
		const scheduledAt = validated.pendingDeletionAt
			? new Date(validated.pendingDeletionAt)
			: ((await deps.userRepository.findUnique(userId))?.pendingDeletionAt ?? null);
		if (scheduledAt === null) {
			Logger.info({userId}, 'Account deletion schedule is no longer eligible');
			return;
		}
		await processUserDeletion(userId, scheduledAt, validated.deletionReasonCode, deps);
	} catch (error) {
		Logger.error({error, userId}, 'Failed to delete user account');
		throw error;
	}
};

export default userProcessPendingDeletion;
