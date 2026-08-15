// SPDX-License-Identifier: AGPL-3.0-or-later

import type {LoggerInterface} from '@fluxer/logger/src/LoggerInterface';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import type {WorkerJobPayload} from '@pkgs/worker/src/contracts/WorkerTypes';
import type {WorkerTaskName} from './WorkerLaneConfig';
import type {WorkerService} from './WorkerService';

interface CronDefinition {
	id: string;
	taskType: WorkerTaskName;
	payload: WorkerJobPayload;
	cronExpression: string;
	ledger: boolean;
	lastFired: number;
}

function parseCronField(field: string, min: number, max: number): Array<number> {
	if (field === '*') {
		return [];
	}
	const values: Array<number> = [];
	for (const part of field.split(',')) {
		const stepMatch = part.match(/^(.+)\/(\d+)$/);
		if (stepMatch) {
			const [, range, stepStr] = stepMatch;
			const step = Number.parseInt(stepStr!, 10);
			let start = min;
			let end = max;
			if (range !== '*') {
				const rangeParts = range!.split('-');
				start = Number.parseInt(rangeParts[0]!, 10);
				if (rangeParts.length > 1) {
					end = Number.parseInt(rangeParts[1]!, 10);
				}
			}
			for (let i = start; i <= end; i += step) {
				values.push(i);
			}
		} else if (part.includes('-')) {
			const [startStr, endStr] = part.split('-');
			const start = Number.parseInt(startStr!, 10);
			const end = Number.parseInt(endStr!, 10);
			for (let i = start; i <= end; i++) {
				values.push(i);
			}
		} else {
			values.push(Number.parseInt(part, 10));
		}
	}
	return values;
}

function matchesCronExpression(expression: string, date: Date): boolean {
	const parts = expression.trim().split(/\s+/);
	if (parts.length !== 6) {
		return false;
	}
	const [secField, minField, hourField, domField, monField, dowField] = parts;
	const second = date.getSeconds();
	const minute = date.getMinutes();
	const hour = date.getHours();
	const dayOfMonth = date.getDate();
	const month = date.getMonth() + 1;
	const dayOfWeek = date.getDay();
	function matches(field: string, value: number, min: number, max: number): boolean {
		const allowed = parseCronField(field, min, max);
		return allowed.length === 0 || allowed.includes(value);
	}
	return (
		matches(secField!, second, 0, 59) &&
		matches(minField!, minute, 0, 59) &&
		matches(hourField!, hour, 0, 23) &&
		matches(domField!, dayOfMonth, 1, 31) &&
		matches(monField!, month, 1, 12) &&
		matches(dowField!, dayOfWeek, 0, 6)
	);
}

export class CronScheduler {
	private readonly workerService: WorkerService;
	private readonly logger: LoggerInterface;
	private readonly kvClient: IKVProvider | null;
	private readonly definitions: Map<string, CronDefinition> = new Map();
	private intervalId: NodeJS.Timeout | null = null;

	constructor(workerService: WorkerService, logger: LoggerInterface, kvClient: IKVProvider | null = null) {
		this.workerService = workerService;
		this.logger = logger;
		this.kvClient = kvClient;
	}

	upsert(
		id: string,
		taskType: WorkerTaskName,
		payload: WorkerJobPayload,
		cronExpression: string,
		options: {ledger: boolean},
	): void {
		this.definitions.set(id, {
			id,
			taskType,
			payload,
			cronExpression,
			ledger: options.ledger,
			lastFired: 0,
		});
	}

	start(): void {
		if (this.intervalId !== null) {
			return;
		}
		this.intervalId = setInterval(() => {
			this.tick().catch((error) => {
				this.logger.error({err: error}, 'Cron scheduler tick failed');
			});
		}, 1000);
		this.logger.info(`Cron scheduler started with ${this.definitions.size} definitions`);
	}

	stop(): void {
		if (this.intervalId !== null) {
			clearInterval(this.intervalId);
			this.intervalId = null;
		}
	}

	private async tick(): Promise<void> {
		const now = new Date();
		const nowSeconds = Math.floor(now.getTime() / 1000);
		for (const def of this.definitions.values()) {
			if (def.lastFired === nowSeconds) {
				continue;
			}
			if (matchesCronExpression(def.cronExpression, now)) {
				def.lastFired = nowSeconds;
				try {
					const jobKey = `cron:${def.id}:${nowSeconds}`;
					const acquired = await this.acquireEnqueueLease(jobKey);
					if (!acquired) {
						continue;
					}
					await this.workerService.addJob(def.taskType, def.payload, {jobKey, skipLedger: !def.ledger});
					this.logger.debug({cronId: def.id, taskType: def.taskType}, 'Cron job fired');
				} catch (error) {
					this.logger.error({err: error, cronId: def.id, taskType: def.taskType}, 'Failed to enqueue cron job');
				}
			}
		}
	}

	private async acquireEnqueueLease(jobKey: string): Promise<boolean> {
		if (this.kvClient === null) {
			return true;
		}
		try {
			return await this.kvClient.setnx(`worker:cron:${jobKey}`, '1', 120);
		} catch (error) {
			this.logger.error({err: error, jobKey}, 'Cron enqueue lease failed');
			return false;
		}
	}
}
