// SPDX-License-Identifier: AGPL-3.0-or-later

import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import type {WorkerJobPayload} from '@pkgs/worker/src/contracts/WorkerTypes';
import type {ParkedScheduledJob} from '../../infrastructure/KVScheduledJobQueueService';
import {Logger} from '../../Logger';
import {getWorkerDependencies} from '../WorkerContext';
import {findLaneForTask, type WorkerTaskName} from '../WorkerLaneConfig';

const MAX_JOBS_PER_RUN = 500;

function buildReleasePayload(job: ParkedScheduledJob): WorkerJobPayload {
	if (job.ledgerJobId === null) {
		return job.payload;
	}
	return {...job.payload, __jobId: job.ledgerJobId};
}

const processScheduledJobQueue: WorkerTaskHandler = async (_payload, helpers) => {
	const {scheduledJobQueueService, workerService} = getWorkerDependencies();
	const nowMs = Date.now();
	const readyJobs = await scheduledJobQueueService.getReadyJobs(nowMs, MAX_JOBS_PER_RUN);
	if (readyJobs.length === 0) {
		helpers.logger.debug('No parked scheduled jobs are due');
		return;
	}
	let releasedCount = 0;
	let claimedElsewhereCount = 0;
	let droppedCount = 0;
	let failedCount = 0;
	for (const job of readyJobs) {
		try {
			const claimed = await scheduledJobQueueService.claimJob(job.jobIdentity);
			if (!claimed) {
				claimedElsewhereCount += 1;
				continue;
			}
			if (findLaneForTask(job.taskType) === null) {
				droppedCount += 1;
				Logger.error(
					{jobIdentity: job.jobIdentity, taskType: job.taskType},
					'Dropping parked scheduled job with an unknown task type',
				);
				continue;
			}
			await workerService.addJob(job.taskType as WorkerTaskName, buildReleasePayload(job), {
				runAt: new Date(job.runAtMs),
				jobKey: job.jobIdentity,
				skipLedger: true,
			});
			releasedCount += 1;
		} catch (error) {
			failedCount += 1;
			Logger.error(
				{error, jobIdentity: job.jobIdentity, taskType: job.taskType},
				'Failed to release parked scheduled job, re-parking for the next drain',
			);
			try {
				await scheduledJobQueueService.parkJob(job, new Date(nowMs));
			} catch (parkError) {
				Logger.error(
					{error: parkError, jobIdentity: job.jobIdentity, taskType: job.taskType},
					'Failed to re-park scheduled job after a release failure',
				);
			}
		}
	}
	helpers.logger.info(
		{
			total: readyJobs.length,
			releasedCount,
			claimedElsewhereCount,
			droppedCount,
			failedCount,
		},
		'Released parked scheduled jobs',
	);
};

export default processScheduledJobQueue;
