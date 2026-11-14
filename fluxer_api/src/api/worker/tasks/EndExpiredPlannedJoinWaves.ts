// SPDX-License-Identifier: AGPL-3.0-or-later

import {endExpiredPlannedJoinWaves} from '@app/api/guild/services/GuildInvitePauseApplier';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import {guildInvitePauseDeps} from '@app/api/worker/WorkerDependencies';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

const BATCH_SIZE = 100;

const endExpiredPlannedJoinWavesTask: WorkerTaskHandler = async (_payload, helpers) => {
	const ended = await endExpiredPlannedJoinWaves(guildInvitePauseDeps(getWorkerDependencies()), BATCH_SIZE);
	if (ended > 0) helpers.logger.info({ended}, 'Ended expired planned join waves');
};

export default endExpiredPlannedJoinWavesTask;
