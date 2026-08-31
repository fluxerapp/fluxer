// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ILogger} from '../ILogger';
import type {KVAccountDeletionQueueService} from '../infrastructure/KVAccountDeletionQueueService';

export async function ensureDeletionQueueState(
	deletionQueue: KVAccountDeletionQueueService,
	logger: ILogger,
): Promise<void> {
	try {
		if (!(await deletionQueue.needsRebuild())) {
			logger.info('KV deletion queue state is healthy');
			return;
		}
		const lockToken = await deletionQueue.acquireRebuildLock();
		if (!lockToken) {
			logger.info('Another instance is rebuilding the KV deletion queue, skipping');
			return;
		}
		logger.info('KV deletion queue needs rebuild, rebuilding...');
		try {
			await deletionQueue.rebuildState(lockToken);
		} finally {
			await deletionQueue.releaseRebuildLock(lockToken);
		}
	} catch (error) {
		logger.error({error}, 'KV deletion queue rebuild failed, continuing startup');
	}
}
