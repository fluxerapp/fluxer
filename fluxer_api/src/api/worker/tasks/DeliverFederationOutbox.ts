// SPDX-License-Identifier: AGPL-3.0-or-later

import {getFederationOutboxService} from '@app/api/middleware/ServiceSingletons';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

const deliverFederationOutbox: WorkerTaskHandler = async () => {
	await getFederationOutboxService().drainAllPeers();
};

export default deliverFederationOutbox;
