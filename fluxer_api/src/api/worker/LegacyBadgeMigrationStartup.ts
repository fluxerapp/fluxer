// SPDX-License-Identifier: AGPL-3.0-or-later

import type {InstanceConfigRepository} from '@app/api/instance/InstanceConfigRepository';
import type {WorkerService} from '@app/api/worker/WorkerService';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import {ms} from 'itty-time';

const CLAIM_KEY = 'migration:legacy_badges:queued';
const CLAIM_TTL_SECONDS = ms('6 hours') / 1000;

export async function queueLegacyBadgeMigrationJob(
	kvClient: Pick<IKVProvider, 'setnx'>,
	workerService: Pick<WorkerService, 'addJob'>,
	instanceConfigRepository: Pick<InstanceConfigRepository, 'areLegacyBadgesMigrated'>,
): Promise<void> {
	if (await instanceConfigRepository.areLegacyBadgesMigrated()) return;
	if (!(await kvClient.setnx(CLAIM_KEY, '1', CLAIM_TTL_SECONDS))) return;
	await workerService.addJob('migrateLegacyBadges', {});
}
