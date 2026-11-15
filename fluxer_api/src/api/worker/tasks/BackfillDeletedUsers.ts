// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/api/Logger';
import {getUserSearchService} from '@app/api/SearchFactory';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {isDeletionComplete} from '@app/api/user/UserHelpers';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import {WorkerQueueOverflowError} from '@app/api/worker/WorkerQueueOverflowError';
import type {WorkerService} from '@app/api/worker/WorkerService';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {ms} from 'itty-time';

const DELETED_USERS_BACKFILL_DONE_KEY = 'backfill:deleted_users:done';
const CLAIM_KEY = 'backfill:deleted_users:queued';
const CLAIM_TTL_SECONDS = ms('6 hours') / 1000;
const DEFAULT_PAGE_SIZE = 500;

interface DeletedUsersBackfillSummary {
	deleted: number;
	scrubbed: number;
}

export async function backfillDeletedUsers(
	userRepository: Pick<UserRepository, 'scanAllUsersPage' | 'scrubDeletedUser'>,
	pageSize = DEFAULT_PAGE_SIZE,
): Promise<DeletedUsersBackfillSummary> {
	const summary: DeletedUsersBackfillSummary = {deleted: 0, scrubbed: 0};
	const searchService = getUserSearchService();
	let pageState: string | null = null;
	do {
		const page = await userRepository.scanAllUsersPage(pageSize, pageState);
		pageState = page.pageState;
		const deletedUsers = page.users.filter(isDeletionComplete);
		for (const user of deletedUsers) {
			if (await userRepository.scrubDeletedUser(user)) summary.scrubbed++;
		}
		if (searchService && deletedUsers.length > 0) {
			await searchService.deleteDocuments(deletedUsers.map((user) => user.id.toString()));
		}
		summary.deleted += deletedUsers.length;
	} while (pageState !== null);
	Logger.info(summary, 'Backfilled deleted user rows');
	return summary;
}

export async function queueDeletedUsersBackfill(
	kvClient: Pick<IKVProvider, 'get' | 'setnx' | 'del'>,
	workerService: Pick<WorkerService, 'addJob'>,
): Promise<void> {
	if ((await kvClient.get(DELETED_USERS_BACKFILL_DONE_KEY)) !== null) return;
	if (!(await kvClient.setnx(CLAIM_KEY, '1', CLAIM_TTL_SECONDS))) return;
	try {
		await workerService.addJob('backfillDeletedUsers', {});
	} catch (error) {
		await kvClient.del(CLAIM_KEY);
		if (!(error instanceof WorkerQueueOverflowError)) throw error;
		Logger.warn('Dropped the deleted users backfill job, jobs stream is at its limit');
	}
}

const backfillDeletedUsersTask: WorkerTaskHandler = async () => {
	const {userRepository, kvClient} = getWorkerDependencies();
	await backfillDeletedUsers(userRepository);
	await kvClient.set(DELETED_USERS_BACKFILL_DONE_KEY, new Date().toISOString());
};

export default backfillDeletedUsersTask;
