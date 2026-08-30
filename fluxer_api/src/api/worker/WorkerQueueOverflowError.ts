// SPDX-License-Identifier: AGPL-3.0-or-later

export class WorkerQueueOverflowError extends Error {
	readonly taskType: string;

	constructor(taskType: string, reason: string) {
		super(`Jobs stream rejected task "${taskType}": ${reason}`);
		this.name = 'WorkerQueueOverflowError';
		this.taskType = taskType;
	}
}
