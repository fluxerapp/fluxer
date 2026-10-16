// SPDX-License-Identifier: AGPL-3.0-or-later

import {createGuildID} from '@app/api/BrandedTypes';
import {resetApiServicesForTesting} from '@app/api/CreateApiContext';
import type {ChannelFollowerRemovalCopyMode} from '@app/api/channel/services/ChannelFollowers';
import {createTestGuildStack} from '@app/api/channel/tests/CrosspostTestUtils';
import {setInjectedWorkerService} from '@app/api/middleware/ServiceRegistry';
import {RecordingWorkerService} from '@app/api/test/RecordingWorkerService';

export function recordGuildDeletions(): RecordingWorkerService {
	const worker = new RecordingWorkerService();
	setInjectedWorkerService(worker);
	resetApiServicesForTesting();
	return worker;
}

export async function processRecordedGuildDeletions(worker: RecordingWorkerService): Promise<void> {
	const operations = createTestGuildStack().guildService.data.operations;
	for (const job of worker.byTask('guildProcessDeletion')) {
		await operations.purgeGuild(
			createGuildID(BigInt(String(job.payload.guildId))),
			job.payload.copyMode as ChannelFollowerRemovalCopyMode,
		);
	}
}
