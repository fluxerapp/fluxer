// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	WORKER_CRON_STALE_AFTER_MS,
	type WorkerHeartbeat,
	type WorkerHeartbeatSignal,
} from '@app/api/worker/WorkerHeartbeat';
import type {WorkerTaskName} from '@app/api/worker/WorkerLaneConfig';
import type {WorkerService} from '@app/api/worker/WorkerService';
import type {LoggerInterface} from '@fluxer/logger/src/LoggerInterface';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import type {WorkerJobPayload} from '@pkgs/worker/src/contracts/WorkerTypes';

const MAX_CATCHUP_SECONDS = 60;

interface CronSchedule {
	seconds: ReadonlySet<number>;
	minutes: ReadonlySet<number>;
	hours: ReadonlySet<number>;
	daysOfMonth: ReadonlySet<number>;
	months: ReadonlySet<number>;
	daysOfWeek: ReadonlySet<number>;
}

interface CronDefinition {
	id: string;
	taskType: WorkerTaskName;
	payload: WorkerJobPayload;
	schedule: CronSchedule;
	ledger: boolean;
	lastFired: number;
}

function parseCronField(field: string, name: string, min: number, max: number): ReadonlySet<number> {
	const values = new Set<number>();
	for (const part of field.split(',')) {
		const match = /^(\*|\d+)(?:-(\d+))?(?:\/(\d+))?$/.exec(part);
		if (!match) {
			throw new Error(`Invalid cron ${name} field ${JSON.stringify(field)}: malformed term ${JSON.stringify(part)}`);
		}
		const [, startText, endText, stepText] = match;
		const wildcard = startText === '*';
		if (wildcard && endText !== undefined) {
			throw new Error(`Invalid cron ${name} field ${JSON.stringify(field)}: a range must start with a number`);
		}
		const start = wildcard ? min : Number(startText);
		const defaultEnd = wildcard || stepText !== undefined ? max : start;
		const end = endText === undefined ? defaultEnd : Number(endText);
		const step = stepText === undefined ? 1 : Number(stepText);
		if (start < min || start > max || end < min || end > max) {
			throw new Error(`Invalid cron ${name} field ${JSON.stringify(field)}: values must be between ${min} and ${max}`);
		}
		if (start > end) {
			throw new Error(`Invalid cron ${name} field ${JSON.stringify(field)}: range start exceeds its end`);
		}
		if (!Number.isSafeInteger(step) || step < 1) {
			throw new Error(`Invalid cron ${name} field ${JSON.stringify(field)}: step must be a positive safe integer`);
		}
		for (let value = start; value <= end; value += step) {
			values.add(value);
		}
	}
	return values;
}

function compileCronSchedule(expression: string): CronSchedule {
	const fields = expression.trim().split(/\s+/);
	if (fields.length !== 6) {
		throw new Error(`Cron expression must contain 6 fields, received ${fields.length}: ${JSON.stringify(expression)}`);
	}
	return {
		seconds: parseCronField(fields[0]!, 'seconds', 0, 59),
		minutes: parseCronField(fields[1]!, 'minutes', 0, 59),
		hours: parseCronField(fields[2]!, 'hours', 0, 23),
		daysOfMonth: parseCronField(fields[3]!, 'day of month', 1, 31),
		months: parseCronField(fields[4]!, 'month', 1, 12),
		daysOfWeek: parseCronField(fields[5]!, 'day of week', 0, 6),
	};
}

function matchesCronSchedule(schedule: CronSchedule, date: Date): boolean {
	return (
		schedule.seconds.has(date.getSeconds()) &&
		schedule.minutes.has(date.getMinutes()) &&
		schedule.hours.has(date.getHours()) &&
		schedule.daysOfMonth.has(date.getDate()) &&
		schedule.months.has(date.getMonth() + 1) &&
		schedule.daysOfWeek.has(date.getDay())
	);
}

function findLatestDueSecond(schedule: CronSchedule, fromSeconds: number, toSeconds: number): number | null {
	for (let second = toSeconds; second >= fromSeconds; second--) {
		if (matchesCronSchedule(schedule, new Date(second * 1000))) {
			return second;
		}
	}
	return null;
}

export class CronScheduler {
	private readonly workerService: WorkerService;
	private readonly logger: LoggerInterface;
	private readonly kvClient: IKVProvider | null;
	private readonly heartbeat: WorkerHeartbeat | null;
	private readonly definitions: Map<string, CronDefinition> = new Map();
	private intervalId: NodeJS.Timeout | null = null;
	private tickPromise: Promise<void> | null = null;
	private stopPromise: Promise<void> | null = null;
	private lastTickSecond: number | null = null;
	private heartbeatSignal: WorkerHeartbeatSignal | null = null;

	constructor(
		workerService: WorkerService,
		logger: LoggerInterface,
		kvClient: IKVProvider | null = null,
		heartbeat: WorkerHeartbeat | null = null,
	) {
		this.workerService = workerService;
		this.logger = logger;
		this.kvClient = kvClient;
		this.heartbeat = heartbeat;
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
			schedule: compileCronSchedule(cronExpression),
			ledger: options.ledger,
			lastFired: 0,
		});
	}

	start(): void {
		if (this.stopPromise) throw new Error('Cron scheduler is stopping');
		if (this.intervalId !== null) {
			return;
		}
		this.heartbeatSignal = this.heartbeat?.register('cron', WORKER_CRON_STALE_AFTER_MS) ?? null;
		this.intervalId = setInterval(() => {
			if (this.intervalId === null || this.tickPromise !== null) return;
			this.tickPromise = this.tick()
				.catch((error) => {
					this.logger.error({err: error}, 'Cron scheduler tick failed');
				})
				.finally(() => {
					this.tickPromise = null;
				});
		}, 1000);
		this.logger.info(`Cron scheduler started with ${this.definitions.size} definitions`);
	}

	stop(): Promise<void> {
		if (this.stopPromise) return this.stopPromise;
		if (this.intervalId !== null) {
			clearInterval(this.intervalId);
			this.intervalId = null;
		}
		this.heartbeatSignal?.release();
		this.heartbeatSignal = null;
		this.stopPromise = (this.tickPromise ?? Promise.resolve()).finally(() => {
			this.lastTickSecond = null;
			this.stopPromise = null;
		});
		return this.stopPromise;
	}

	private async tick(): Promise<void> {
		try {
			await this.runDueDefinitions();
		} finally {
			this.heartbeatSignal?.report();
		}
	}

	private async runDueDefinitions(): Promise<void> {
		const nowSeconds = Math.floor(Date.now() / 1000);
		const previousTickSecond = this.lastTickSecond;
		this.lastTickSecond = nowSeconds;
		const fromSeconds =
			previousTickSecond === null || previousTickSecond >= nowSeconds
				? nowSeconds
				: Math.max(previousTickSecond + 1, nowSeconds - MAX_CATCHUP_SECONDS);
		for (const def of this.definitions.values()) {
			if (this.intervalId === null) return;
			const dueSecond = findLatestDueSecond(def.schedule, fromSeconds, nowSeconds);
			if (dueSecond === null || def.lastFired === dueSecond) {
				continue;
			}
			def.lastFired = dueSecond;
			try {
				const jobKey = `cron:${def.id}:${dueSecond}`;
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
