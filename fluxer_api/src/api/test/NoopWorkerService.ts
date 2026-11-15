// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';

export class NoopWorkerService implements IWorkerService {
	async addJob(): Promise<bigint> {
		return 0n;
	}

	async cancelJob(_jobId: bigint): Promise<boolean> {
		return false;
	}
}
