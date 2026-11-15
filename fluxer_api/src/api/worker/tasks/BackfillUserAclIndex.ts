// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {Logger} from '@app/api/Logger';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import {WorkerQueueOverflowError} from '@app/api/worker/WorkerQueueOverflowError';
import type {WorkerService} from '@app/api/worker/WorkerService';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {ms} from 'itty-time';

const USER_ACL_INDEX_BACKFILL_DONE_KEY = 'backfill:user_acl_index:done';
const CLAIM_KEY = 'backfill:user_acl_index:queued';
const CLAIM_TTL_SECONDS = ms('6 hours') / 1000;
const DEFAULT_PAGE_SIZE = 500;

async function backfillUserAclIndex(
	userRepository: Pick<UserRepository, 'scanAllUsersPage' | 'indexUserAcls'>,
	pageSize = DEFAULT_PAGE_SIZE,
): Promise<number> {
	let indexed = 0;
	let pageState: string | null = null;
	do {
		const page = await userRepository.scanAllUsersPage(pageSize, pageState);
		for (const user of page.users) {
			if (user.acls.size === 0) continue;
			await userRepository.indexUserAcls(user);
			indexed++;
		}
		pageState = page.pageState;
	} while (pageState !== null);
	Logger.info({indexed}, 'Backfilled the user ACL index');
	return indexed;
}

export async function queueUserAclIndexBackfill(
	kvClient: Pick<IKVProvider, 'get' | 'setnx' | 'del'>,
	workerService: Pick<WorkerService, 'addJob'>,
): Promise<void> {
	if (!Config.instance.selfHosted) return;
	if ((await kvClient.get(USER_ACL_INDEX_BACKFILL_DONE_KEY)) !== null) return;
	if (!(await kvClient.setnx(CLAIM_KEY, '1', CLAIM_TTL_SECONDS))) return;
	try {
		await workerService.addJob('backfillUserAclIndex', {});
	} catch (error) {
		await kvClient.del(CLAIM_KEY);
		if (!(error instanceof WorkerQueueOverflowError)) throw error;
		Logger.warn('Dropped the user ACL index backfill job, jobs stream is at its limit');
	}
}

const backfillUserAclIndexTask: WorkerTaskHandler = async () => {
	const {userRepository, kvClient} = getWorkerDependencies();
	await backfillUserAclIndex(userRepository);
	await kvClient.set(USER_ACL_INDEX_BACKFILL_DONE_KEY, new Date().toISOString());
};

export default backfillUserAclIndexTask;
