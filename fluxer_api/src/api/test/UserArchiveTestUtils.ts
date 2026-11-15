// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import type {UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createTestWorkerHelpers} from '@app/api/test/SyncTaskWorkerService';
import {UserHarvest} from '@app/api/user/UserHarvestModel';
import harvestUserData from '@app/api/worker/tasks/HarvestUserData';
import type {WorkerDependencies} from '@app/api/worker/WorkerDependencies';
import {strFromU8, unzipSync} from 'fflate';
import {z} from 'zod';

export async function harvestUserArchive(harness: ApiTestHarness, deps: WorkerDependencies, userId: UserID) {
	const harvestId = await deps.snowflakeService.generate();
	await deps.userHarvestRepository.create(
		new UserHarvest({
			user_id: userId,
			harvest_id: harvestId,
			requested_at: new Date(),
			started_at: null,
			completed_at: null,
			failed_at: null,
			storage_key: null,
			file_size: null,
			progress_percent: 0,
			progress_step: null,
			error_message: null,
			download_url_expires_at: null,
		}),
	);
	await harvestUserData(
		{userId: userId.toString(), harvestId: harvestId.toString()},
		createTestWorkerHelpers({attempt: {isLastAttempt: true}}),
	);
	const completed = await deps.userHarvestRepository.findByUserAndHarvestId(userId, harvestId);
	assert.equal(completed?.getStatus(), 'completed');
	assert(completed?.storageKey);
	return readStoredArchive(harness, completed.storageKey);
}

export async function readStoredArchive(harness: ApiTestHarness, storageKey: string) {
	const zip = await harness.storageService.readObject(Config.s3.buckets.harvests, storageKey);
	const entries = unzipSync(zip);
	return {
		entries,
		readJson(name: string): unknown {
			const bytes = entries[name];
			assert(bytes, `Archive is missing ${name}`);
			return JSON.parse(strFromU8(bytes));
		},
	};
}

export const HarvestedMessagesSchema = z.array(z.object({id: z.string(), content: z.string()}));
