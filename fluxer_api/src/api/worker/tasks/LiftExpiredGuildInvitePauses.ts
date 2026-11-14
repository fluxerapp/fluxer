// SPDX-License-Identifier: AGPL-3.0-or-later

import {liftExpiredGuildInvitePauses} from '@app/api/guild/services/GuildInvitePauseApplier';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import {guildInvitePauseDeps} from '@app/api/worker/WorkerDependencies';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

const BATCH_SIZE = 100;

const liftExpiredGuildInvitePausesTask: WorkerTaskHandler = async (_payload, helpers) => {
	const lifted = await liftExpiredGuildInvitePauses(guildInvitePauseDeps(getWorkerDependencies()), BATCH_SIZE);
	if (lifted > 0) helpers.logger.info({lifted}, 'Lifted expired guild invite pauses');
};

export default liftExpiredGuildInvitePausesTask;
