import type {AdminArchive} from '@app/api/admin/models/AdminArchiveModel';
import {ArchiveAttemptSupersededError} from '@app/api/archive/ArchiveAttemptSupersededError';
import type {UserHarvest} from '@app/api/user/UserHarvestModel';
import type {WorkerTaskAttempt, WorkerTaskHandler, WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';

export class ArchiveTaskDeferredError extends Error {
	constructor(cause: unknown) {
		super('Archive execution could not confirm a terminal result', {cause});
		this.name = 'ArchiveTaskDeferredError';
	}
}

export class ArchiveTerminalFailureError extends Error {
	constructor(message: string) {
		super(message);
		this.name = 'ArchiveTerminalFailureError';
	}
}

interface ArchiveTask extends WorkerTaskHandler {
	readonly failurePolicy: 'archive';
}

export type ArchiveTaskHandler = (
	payload: Record<string, unknown>,
	helpers: WorkerTaskHelpers,
	attempt: WorkerTaskAttempt,
) => Promise<void>;

export function isArchiveTask(task: WorkerTaskHandler | undefined): task is ArchiveTask {
	return task !== undefined && 'failurePolicy' in task && task.failurePolicy === 'archive';
}

export function throwIfArchiveTerminallyFailed(archive: UserHarvest | AdminArchive): void {
	if (archive.terminalFailedAt === null) return;
	if (archive.errorMessage === null) throw new Error('Terminal archive failure has no recorded reason');
	throw new ArchiveTerminalFailureError(archive.errorMessage);
}

export function createArchiveTask(handler: ArchiveTaskHandler): ArchiveTask {
	const task: WorkerTaskHandler = async (payload, helpers) => {
		try {
			const attempt = helpers.attempt;
			if (attempt === undefined || typeof attempt.isLastAttempt !== 'boolean') {
				throw new Error('Archive execution requires queue attempt metadata');
			}
			await handler(payload, helpers, attempt);
		} catch (error) {
			if (error instanceof ArchiveAttemptSupersededError || error instanceof ArchiveTerminalFailureError) {
				throw error;
			}
			throw new ArchiveTaskDeferredError(error);
		}
	};
	return Object.assign(task, {failurePolicy: 'archive' as const});
}
