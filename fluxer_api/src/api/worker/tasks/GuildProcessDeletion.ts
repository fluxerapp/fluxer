// SPDX-License-Identifier: AGPL-3.0-or-later

import {createGuildID} from '@app/api/BrandedTypes';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {z} from 'zod';

const PayloadSchema = z.object({
	guildId: z.string(),
	copyMode: z.enum(['source_deleted', 'purge']),
});

const guildProcessDeletion: WorkerTaskHandler = async (payload, helpers) => {
	const validated = PayloadSchema.parse(payload);
	helpers.logger.debug({payload: validated}, 'Processing guildProcessDeletion task');
	const {guildService} = getWorkerDependencies();
	await guildService.data.operations.purgeGuild(createGuildID(BigInt(validated.guildId)), validated.copyMode);
};

export default guildProcessDeletion;
