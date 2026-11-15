// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import {Config} from '@app/api/Config';
import type {WorkerTaskName} from '@app/api/worker/WorkerLaneConfig';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';
import {seconds} from 'itty-time';

const RUN_KEY = 'attachment_decay:reconcile_run';
const RUN_TTL_SECONDS = seconds('1 day');

export function attachmentDecayReconcileJobKey(runId: string, chunk: number): string {
	return `${runId}:${chunk}`;
}

export async function startAttachmentDecayReconcile(
	kvClient: IKVProvider,
	workerService: IWorkerService<WorkerTaskName>,
): Promise<void> {
	if (!Config.instance.selfHosted) return;
	const runId = randomUUID();
	await kvClient.setex(RUN_KEY, RUN_TTL_SECONDS, runId);
	await workerService.addJob(
		'reconcileAttachmentDecay',
		{runId, chunk: 0},
		{jobKey: attachmentDecayReconcileJobKey(runId, 0)},
	);
}

export async function holdAttachmentDecayReconcileRun(kvClient: IKVProvider, runId: string): Promise<boolean> {
	return (
		(await kvClient.extendLock(RUN_KEY, runId, RUN_TTL_SECONDS)) ||
		(await kvClient.acquireLock(RUN_KEY, runId, RUN_TTL_SECONDS))
	);
}

export async function finishAttachmentDecayReconcileRun(kvClient: IKVProvider, runId: string): Promise<void> {
	await kvClient.releaseLock(RUN_KEY, runId);
}

export async function isAttachmentDecayReconcileRunning(kvClient: IKVProvider): Promise<boolean> {
	if (!Config.instance.selfHosted) return false;
	return (await kvClient.exists(RUN_KEY)) > 0;
}
