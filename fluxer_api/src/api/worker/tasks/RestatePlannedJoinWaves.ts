// SPDX-License-Identifier: AGPL-3.0-or-later

import {restatePlannedJoinWaves} from '@app/api/guild/services/GuildInvitePauseApplier';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import {guildInvitePauseDeps} from '@app/api/worker/WorkerDependencies';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

const restatePlannedJoinWavesTask: WorkerTaskHandler = async (_payload, helpers) => {
	const restated = await restatePlannedJoinWaves(guildInvitePauseDeps(getWorkerDependencies()));
	if (restated > 0) helpers.logger.info({restated}, 'Restated planned join waves');
};

export default restatePlannedJoinWavesTask;
