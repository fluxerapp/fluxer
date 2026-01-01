/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import CronExpressionParser from 'cron-parser';
import type {Redis} from 'ioredis';
import type {Pool} from 'pg';
import type {AssetDeletionQueue} from '~/infrastructure/AssetDeletionQueue';
import type {ICloudflarePurgeQueue} from '~/infrastructure/CloudflarePurgeQueue';
import type {IMetricsService} from '~/infrastructure/IMetricsService';
import type {RedisAccountDeletionQueueService} from '~/infrastructure/RedisAccountDeletionQueueService';
import type {RedisBulkMessageDeletionQueueService} from '~/infrastructure/RedisBulkMessageDeletionQueueService';
import {Logger} from '~/Logger';

const DEFAULT_REPORT_INTERVAL_MS = 30000;

const CRON_PATTERNS: Record<string, string> = {
	processCloudfarePurgeQueue: '* * * * *',
	processAssetDeletionQueue: '* * * * *',
	processPendingBulkMessageDeletions: '* * * * *',
	expireAttachments: '15 * * * *',
	userProcessPendingDeletions: '0 3 * * *',
	processInactivityDeletions: '0 4 * * 0',
};

interface JobStats {
	task: string;
	pending: number;
	running: number;
	failed: number;
}

interface TaskAggregateStats {
	task: string;
	totalRetries: number;
}

interface CronJobStatus {
	task: string;
	lastExecution: Date | null;
	nextExpectedRun: Date | null;
	isOverdue: boolean;
}

interface WorkerMetricsCollectorOptions {
	pgPool: Pool;
	redis: Redis;
	metricsService: IMetricsService;
	assetDeletionQueue: AssetDeletionQueue;
	cloudflarePurgeQueue: ICloudflarePurgeQueue;
	bulkMessageDeletionQueue: RedisBulkMessageDeletionQueueService;
	accountDeletionQueue: RedisAccountDeletionQueueService;
	reportIntervalMs?: number;
	workerConcurrency?: number;
}

export class WorkerMetricsCollector {
	private readonly pgPool: Pool;
	private readonly redis: Redis;
	private readonly metricsService: IMetricsService;
	private readonly assetDeletionQueue: AssetDeletionQueue;
	private readonly cloudflarePurgeQueue: ICloudflarePurgeQueue;
	private readonly bulkMessageDeletionQueue: RedisBulkMessageDeletionQueueService;
	private readonly accountDeletionQueue: RedisAccountDeletionQueueService;
	private readonly reportIntervalMs: number;
	private readonly workerConcurrency: number;
	private intervalHandle: ReturnType<typeof setInterval> | null = null;
	private dbQueryErrorCount = 0;
	private redisCommandErrorCount = 0;
	private redisErrorHandler: (() => void) | null = null;

	constructor(options: WorkerMetricsCollectorOptions) {
		this.pgPool = options.pgPool;
		this.redis = options.redis;
		this.metricsService = options.metricsService;
		this.assetDeletionQueue = options.assetDeletionQueue;
		this.cloudflarePurgeQueue = options.cloudflarePurgeQueue;
		this.bulkMessageDeletionQueue = options.bulkMessageDeletionQueue;
		this.accountDeletionQueue = options.accountDeletionQueue;
		this.reportIntervalMs = options.reportIntervalMs ?? DEFAULT_REPORT_INTERVAL_MS;
		this.workerConcurrency = options.workerConcurrency ?? 5;
		this.setupRedisErrorTracking();
	}

	private setupRedisErrorTracking(): void {
		this.redisErrorHandler = () => {
			this.redisCommandErrorCount++;
		};
		this.redis.on('error', this.redisErrorHandler);
	}

	start(): void {
		if (this.intervalHandle) {
			return;
		}

		Logger.info({intervalMs: this.reportIntervalMs}, 'Starting WorkerMetricsCollector');

		this.collectAndReport().catch((err) => {
			Logger.error({err}, 'Initial metrics collection failed');
		});

		this.intervalHandle = setInterval(() => {
			this.collectAndReport().catch((err) => {
				Logger.error({err}, 'Metrics collection failed');
			});
		}, this.reportIntervalMs);
	}

	stop(): void {
		if (this.intervalHandle) {
			clearInterval(this.intervalHandle);
			this.intervalHandle = null;
			Logger.info('Stopped WorkerMetricsCollector');
		}
		if (this.redisErrorHandler) {
			this.redis.off('error', this.redisErrorHandler);
			this.redisErrorHandler = null;
		}
	}

	private async collectAndReport(): Promise<void> {
		const [jobStats, redisQueueSizes, taskAggregates, cronStatus, avgWaitTime, redisConnectionStatus] =
			await Promise.all([
				this.collectGraphileJobStats(),
				this.collectRedisQueueSizes(),
				this.collectTaskAggregateStats(),
				this.collectCronJobStatus(),
				this.collectAverageWaitTime(),
				this.collectRedisConnectionStatus(),
			]);

		this.reportJobStats(jobStats);
		this.reportRedisQueueSizes(redisQueueSizes);
		this.reportTaskAggregateStats(taskAggregates);
		this.reportCronJobStatus(cronStatus);
		this.reportWorkerConcurrencyUtilization(jobStats);
		this.reportAverageWaitTime(avgWaitTime);
		this.reportDbPoolStats();
		this.reportRedisHealthMetrics(redisConnectionStatus);
	}

	private async collectGraphileJobStats(): Promise<Array<JobStats>> {
		try {
			const result = await this.pgPool.query<{task_identifier: string; status: string; count: string}>(`
				SELECT
					task_identifier,
					CASE
						WHEN locked_at IS NOT NULL THEN 'running'
						WHEN attempts >= max_attempts THEN 'failed'
						ELSE 'pending'
					END as status,
					COUNT(*) as count
				FROM graphile_worker.jobs
				GROUP BY task_identifier, status
			`);

			const statsMap = new Map<string, JobStats>();

			for (const row of result.rows) {
				const task = row.task_identifier;
				if (!statsMap.has(task)) {
					statsMap.set(task, {task, pending: 0, running: 0, failed: 0});
				}
				const stats = statsMap.get(task)!;
				const count = parseInt(row.count, 10);

				switch (row.status) {
					case 'pending':
						stats.pending = count;
						break;
					case 'running':
						stats.running = count;
						break;
					case 'failed':
						stats.failed = count;
						break;
				}
			}

			return Array.from(statsMap.values());
		} catch (err) {
			Logger.error({err}, 'Failed to collect Graphile job stats');
			return [];
		}
	}

	private async collectRedisQueueSizes(): Promise<{
		assetDeletion: number;
		cloudflarePurge: number;
		bulkMessageDeletion: number;
		accountDeletion: number;
	}> {
		try {
			const [assetDeletion, cloudflarePurge, bulkMessageDeletion, accountDeletion] = await Promise.all([
				this.assetDeletionQueue.getQueueSize(),
				this.cloudflarePurgeQueue.getQueueSize(),
				this.bulkMessageDeletionQueue.getQueueSize(),
				this.accountDeletionQueue.getQueueSize(),
			]);

			return {assetDeletion, cloudflarePurge, bulkMessageDeletion, accountDeletion};
		} catch (err) {
			Logger.error({err}, 'Failed to collect Redis queue sizes');
			return {assetDeletion: 0, cloudflarePurge: 0, bulkMessageDeletion: 0, accountDeletion: 0};
		}
	}

	private reportJobStats(stats: Array<JobStats>): void {
		let totalPending = 0;
		let totalRunning = 0;
		let totalFailed = 0;

		for (const stat of stats) {
			totalPending += stat.pending;
			totalRunning += stat.running;
			totalFailed += stat.failed;

			this.metricsService.gauge({
				name: 'worker.queue.pending',
				dimensions: {task: stat.task},
				value: stat.pending,
			});
			this.metricsService.gauge({
				name: 'worker.queue.running',
				dimensions: {task: stat.task},
				value: stat.running,
			});
			this.metricsService.gauge({
				name: 'worker.queue.failed',
				dimensions: {task: stat.task},
				value: stat.failed,
			});
		}

		this.metricsService.gauge({
			name: 'worker.queue.total_pending',
			value: totalPending,
		});
		this.metricsService.gauge({
			name: 'worker.queue.total_running',
			value: totalRunning,
		});
		this.metricsService.gauge({
			name: 'worker.queue.total_failed',
			value: totalFailed,
		});
	}

	private reportRedisQueueSizes(sizes: {
		assetDeletion: number;
		cloudflarePurge: number;
		bulkMessageDeletion: number;
		accountDeletion: number;
	}): void {
		this.metricsService.gauge({
			name: 'worker.redis_queue.asset_deletion',
			value: sizes.assetDeletion,
		});
		this.metricsService.gauge({
			name: 'worker.redis_queue.cloudflare_purge',
			value: sizes.cloudflarePurge,
		});
		this.metricsService.gauge({
			name: 'worker.redis_queue.bulk_message_deletion',
			value: sizes.bulkMessageDeletion,
		});
		this.metricsService.gauge({
			name: 'worker.redis_queue.account_deletion',
			value: sizes.accountDeletion,
		});
	}

	private async collectTaskAggregateStats(): Promise<Array<TaskAggregateStats>> {
		try {
			const result = await this.pgPool.query<{
				task_identifier: string;
				total_retries: string;
			}>(`
				SELECT
					task_identifier,
					SUM(GREATEST(attempts - 1, 0)) as total_retries
				FROM graphile_worker.jobs
				WHERE attempts > 1
				GROUP BY task_identifier
			`);

			return result.rows.map((row) => ({
				task: row.task_identifier,
				totalRetries: parseInt(row.total_retries, 10),
			}));
		} catch (err) {
			Logger.error({err}, 'Failed to collect task aggregate stats');
			return [];
		}
	}

	private async collectCronJobStatus(): Promise<Array<CronJobStatus>> {
		try {
			const result = await this.pgPool.query<{
				identifier: string;
				known_since: Date;
				last_execution: Date | null;
			}>(`
				SELECT identifier, known_since, last_execution
				FROM graphile_worker.known_crontabs
			`);

			const now = new Date();
			return result.rows.map((row) => {
				const nextExpectedRun = this.getNextCronRun(row.identifier, row.last_execution);
				const isOverdue = nextExpectedRun ? now > new Date(nextExpectedRun.getTime() + 60000) : false;

				return {
					task: row.identifier,
					lastExecution: row.last_execution,
					nextExpectedRun,
					isOverdue,
				};
			});
		} catch (err) {
			Logger.error({err}, 'Failed to collect cron job status');
			return [];
		}
	}

	private getNextCronRun(identifier: string, lastExecution: Date | null): Date | null {
		if (!lastExecution) {
			return null;
		}

		const pattern = CRON_PATTERNS[identifier];
		if (!pattern) {
			return null;
		}

		try {
			const expression = CronExpressionParser.parse(pattern, {
				currentDate: lastExecution,
			});
			const nextDate = expression.next();
			return nextDate.toDate();
		} catch {
			return null;
		}
	}

	private async collectAverageWaitTime(): Promise<Map<string, number>> {
		try {
			const result = await this.pgPool.query<{
				task_identifier: string;
				avg_wait_ms: string;
			}>(`
				SELECT
					task_identifier,
					EXTRACT(EPOCH FROM AVG(locked_at - run_at)) * 1000 as avg_wait_ms
				FROM graphile_worker.jobs
				WHERE locked_at IS NOT NULL AND run_at IS NOT NULL
				GROUP BY task_identifier
			`);

			const waitTimes = new Map<string, number>();
			for (const row of result.rows) {
				const avgWait = parseFloat(row.avg_wait_ms);
				if (!Number.isNaN(avgWait)) {
					waitTimes.set(row.task_identifier, avgWait);
				}
			}
			return waitTimes;
		} catch (err) {
			Logger.error({err}, 'Failed to collect average wait time');
			return new Map();
		}
	}

	private reportTaskAggregateStats(stats: Array<TaskAggregateStats>): void {
		for (const stat of stats) {
			if (stat.totalRetries > 0) {
				this.metricsService.gauge({
					name: 'worker.job.retries',
					dimensions: {task: stat.task},
					value: stat.totalRetries,
				});
			}
		}
	}

	private reportCronJobStatus(cronJobs: Array<CronJobStatus>): void {
		for (const cron of cronJobs) {
			this.metricsService.gauge({
				name: 'worker.cron.overdue',
				dimensions: {task: cron.task},
				value: cron.isOverdue ? 1 : 0,
			});

			if (cron.lastExecution) {
				const msSinceLastRun = Date.now() - cron.lastExecution.getTime();
				this.metricsService.gauge({
					name: 'worker.cron.last_run_age_ms',
					dimensions: {task: cron.task},
					value: msSinceLastRun,
				});
			}
		}
	}

	private reportWorkerConcurrencyUtilization(jobStats: Array<JobStats>): void {
		let totalRunning = 0;
		for (const stat of jobStats) {
			totalRunning += stat.running;
		}

		const utilizationPercent = (totalRunning / this.workerConcurrency) * 100;
		this.metricsService.gauge({
			name: 'worker.concurrency.utilization_percent',
			value: Math.min(utilizationPercent, 100),
		});
		this.metricsService.gauge({
			name: 'worker.concurrency.available_slots',
			value: Math.max(this.workerConcurrency - totalRunning, 0),
		});
	}

	private reportAverageWaitTime(waitTimes: Map<string, number>): void {
		for (const [task, avgWaitMs] of waitTimes) {
			this.metricsService.gauge({
				name: 'worker.job.avg_wait_time_ms',
				dimensions: {task},
				value: avgWaitMs,
			});
		}

		if (waitTimes.size > 0) {
			const totalWait = Array.from(waitTimes.values()).reduce((sum, v) => sum + v, 0);
			this.metricsService.gauge({
				name: 'worker.job.avg_wait_time_ms_total',
				value: totalWait / waitTimes.size,
			});
		}
	}

	private async collectRedisConnectionStatus(): Promise<boolean> {
		try {
			const result = await this.redis.ping();
			return result === 'PONG';
		} catch (err) {
			Logger.error({err}, 'Redis ping failed');
			return false;
		}
	}

	private reportDbPoolStats(): void {
		this.metricsService.gauge({
			name: 'db.pool.total',
			value: this.pgPool.totalCount,
		});
		this.metricsService.gauge({
			name: 'db.pool.idle',
			value: this.pgPool.idleCount,
		});
		this.metricsService.gauge({
			name: 'db.pool.waiting',
			value: this.pgPool.waitingCount,
		});

		if (this.dbQueryErrorCount > 0) {
			this.metricsService.counter({
				name: 'db.query.error',
				value: this.dbQueryErrorCount,
			});
			this.dbQueryErrorCount = 0;
		}
	}

	private reportRedisHealthMetrics(isConnected: boolean): void {
		this.metricsService.gauge({
			name: 'redis.connection.status',
			value: isConnected ? 1 : 0,
		});

		if (this.redisCommandErrorCount > 0) {
			this.metricsService.counter({
				name: 'redis.command.error',
				value: this.redisCommandErrorCount,
			});
			this.redisCommandErrorCount = 0;
		}
	}

	incrementDbQueryError(): void {
		this.dbQueryErrorCount++;
	}
}
