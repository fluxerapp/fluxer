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
	} catch (error) {
		logger.error({error}, 'Failed to read KV deletion queue state, aborting startup');
		throw error;
	}
	let lockToken: string | null;
	try {
		lockToken = await deletionQueue.acquireRebuildLock();
	} catch (error) {
		logger.error({error}, 'Failed to acquire the KV deletion queue rebuild lock, aborting startup');
		throw error;
	}
	if (!lockToken) {
		logger.info('Another instance is rebuilding the KV deletion queue, skipping');
		return;
	}
	logger.info('KV deletion queue needs rebuild, rebuilding...');
	try {
		await deletionQueue.rebuildState(lockToken);
	} catch (error) {
		logger.error({error}, 'KV deletion queue rebuild failed, leaving the rebuild to the deletion worker');
	} finally {
		try {
			await deletionQueue.releaseRebuildLock(lockToken);
		} catch (error) {
			logger.error({error}, 'Failed to release the KV deletion queue rebuild lock');
		}
	}
}
