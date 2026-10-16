// SPDX-License-Identifier: AGPL-3.0-or-later

import {initializeConfig} from '@app/api/Config';
import {initializeLogger} from '@app/api/Logger';
import {startWorkerMain} from '@app/api/worker/WorkerMain';
import {Config} from '@app/Config';
import {Logger} from '@app/Logger';

initializeConfig(Config);

initializeLogger(Logger);

startWorkerMain().catch((error) => {
	Logger.fatal({error}, 'Failed to start Fluxer API worker');
	process.exit(1);
});
