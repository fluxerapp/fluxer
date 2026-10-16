// SPDX-License-Identifier: AGPL-3.0-or-later

import {createGuildID} from '@app/api/BrandedTypes';
import {Logger} from '@app/api/Logger';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {z} from 'zod';

const PayloadSchema = z.object({
	guildId: z.string(),
});
const BATCH_LIMIT = 250;
const batchGuildAuditLogMessageDeletes: WorkerTaskHandler = async (payload, helpers) => {
	const validated = PayloadSchema.parse(payload);
	helpers.logger.debug({payload: validated}, 'Processing batchGuildAuditLogMessageDeletes task');
	const guildId = createGuildID(BigInt(validated.guildId));
	const {guildAuditLogService} = getWorkerDependencies();
	try {
		const result = await guildAuditLogService.batchRecentMessageDeleteLogs(guildId, BATCH_LIMIT);
		if (result.deletedLogIds.length > 0) {
			Logger.info(
				{
					guildId: guildId.toString(),
					deletedCount: result.deletedLogIds.length,
					createdCount: result.createdLogs.length,
				},
				'Batched consecutive message delete audit logs',
			);
		} else {
			Logger.debug({guildId: guildId.toString()}, 'No consecutive message delete audit logs found to batch');
		}
	} catch (error) {
		Logger.error({error, guildId: guildId.toString()}, 'Failed to batch guild audit log message deletes');
		throw error;
	}
};

export default batchGuildAuditLogMessageDeletes;
