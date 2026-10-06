// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAttachmentID} from '@app/api/BrandedTypes';
import {Logger} from '@app/api/Logger';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {z} from 'zod';

const PayloadSchema = z.object({
	attachmentId: z.string(),
	requeueCount: z.number().int().min(0).optional(),
});
const finalizeNcmecAttachmentReport: WorkerTaskHandler = async (payload, helpers) => {
	const validated = PayloadSchema.parse(payload);
	helpers.logger.debug({payload: validated}, 'Processing finalizeNcmecAttachmentReport task');
	const attachmentId = createAttachmentID(BigInt(validated.attachmentId));
	try {
		const deps = getWorkerDependencies();
		await deps.ncmecSubmissionService.finalizeAttachmentReport(attachmentId, validated.requeueCount ?? 0);
	} catch (error) {
		Logger.error({error, attachmentId: attachmentId.toString()}, 'Failed to finalize NCMEC attachment report');
		throw error;
	}
};

export default finalizeNcmecAttachmentReport;
