/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {Task} from 'graphile-worker';
import {createGuildID} from '~/BrandedTypes';
import {Logger} from '~/Logger';
import {CommonFields, validatePayload} from '../utils/TaskPayloadValidator';
import {getWorkerDependencies} from '../WorkerContext';

interface BatchGuildAuditLogMessageDeletesPayload {
	guildId: string;
}

const payloadSchema = {
	guildId: CommonFields.guildId(),
};

const BATCH_LIMIT = 250;

const batchGuildAuditLogMessageDeletes: Task = async (payload, helpers) => {
	const validated = validatePayload<BatchGuildAuditLogMessageDeletesPayload>(payload, payloadSchema);
	helpers.logger.debug('Processing batchGuildAuditLogMessageDeletes task', {payload: validated});

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
