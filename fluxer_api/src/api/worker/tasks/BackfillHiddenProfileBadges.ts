// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/api/Logger';
import type {User} from '@app/api/models/User';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import {WorkerQueueOverflowError} from '@app/api/worker/WorkerQueueOverflowError';
import type {WorkerService} from '@app/api/worker/WorkerService';
import {legacyHiddenProfileBadges} from '@fluxer/constants/src/UserConstants';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {ms} from 'itty-time';

const HIDDEN_PROFILE_BADGES_BACKFILL_DONE_KEY = 'backfill:hidden_profile_badges:done';
const CLAIM_KEY = 'backfill:hidden_profile_badges:queued';
const CLAIM_TTL_SECONDS = ms('6 hours') / 1000;
const DEFAULT_PAGE_SIZE = 500;

interface HiddenProfileBadgesBackfillSummary {
	migrated: number;
	failed: number;
}

function hasLegacyBadgeHideBits(user: User): boolean {
	return legacyHiddenProfileBadges(user.flags, user.premiumFlags).length > 0;
}

export async function backfillHiddenProfileBadges(
	userRepository: Pick<UserRepository, 'scanAllUsersPage' | 'moveLegacyBadgeHideBits'>,
	pageSize = DEFAULT_PAGE_SIZE,
): Promise<HiddenProfileBadgesBackfillSummary> {
	const summary: HiddenProfileBadgesBackfillSummary = {migrated: 0, failed: 0};
	let pageState: string | null = null;
	do {
		const page = await userRepository.scanAllUsersPage(pageSize, pageState);
		pageState = page.pageState;
		for (const scanned of page.users.filter(hasLegacyBadgeHideBits)) {
			try {
				const migrated = await userRepository.moveLegacyBadgeHideBits(scanned.id, {mergeIntoHiddenList: true});
				if (migrated) summary.migrated++;
			} catch (error) {
				summary.failed++;
				Logger.error({error, userId: scanned.id.toString()}, 'Failed to move the hidden badge bits of a user');
			}
		}
	} while (pageState !== null);
	Logger.info(summary, 'Backfilled hidden profile badges');
	return summary;
}

export async function queueHiddenProfileBadgesBackfill(
	kvClient: Pick<IKVProvider, 'get' | 'setnx' | 'del'>,
	workerService: Pick<WorkerService, 'addJob'>,
): Promise<void> {
	if ((await kvClient.get(HIDDEN_PROFILE_BADGES_BACKFILL_DONE_KEY)) !== null) return;
	if (!(await kvClient.setnx(CLAIM_KEY, '1', CLAIM_TTL_SECONDS))) return;
	try {
		await workerService.addJob('backfillHiddenProfileBadges', {});
	} catch (error) {
		await kvClient.del(CLAIM_KEY);
		if (!(error instanceof WorkerQueueOverflowError)) throw error;
		Logger.warn('Dropped the hidden profile badges backfill job, jobs stream is at its limit');
	}
}

const backfillHiddenProfileBadgesTask: WorkerTaskHandler = async () => {
	const {userRepository, kvClient} = getWorkerDependencies();
	const summary = await backfillHiddenProfileBadges(userRepository);
	if (summary.failed > 0) throw new Error(`Hidden profile badges backfill failed for ${summary.failed} users`);
	await kvClient.set(HIDDEN_PROFILE_BADGES_BACKFILL_DONE_KEY, new Date().toISOString());
};

export default backfillHiddenProfileBadgesTask;
