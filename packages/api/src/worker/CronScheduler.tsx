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

import type {JetStreamWorkerQueue} from '@fluxer/api/src/worker/JetStreamWorkerQueue';
import type {LoggerInterface} from '@fluxer/logger/src/LoggerInterface';
import type {WorkerJobPayload} from '@fluxer/worker/src/contracts/WorkerTypes';

interface CronDefinition {
	id: string;
	taskType: string;
	payload: WorkerJobPayload;
	cronExpression: string;
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
	private readonly queue: JetStreamWorkerQueue;
	private readonly logger: LoggerInterface;
	private readonly definitions: Map<string, CronDefinition> = new Map();
	private intervalId: ReturnType<typeof setInterval> | null = null;

	constructor(queue: JetStreamWorkerQueue, logger: LoggerInterface) {
		this.queue = queue;
		this.logger = logger;
	}

	upsert(id: string, taskType: string, payload: WorkerJobPayload, cronExpression: string): void {
		this.definitions.set(id, {
			id,
			taskType,
			payload,
			cronExpression,
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
					await this.queue.enqueue(def.taskType, def.payload);
					this.logger.debug({cronId: def.id, taskType: def.taskType}, 'Cron job fired');
				} catch (error) {
					this.logger.error({err: error, cronId: def.id, taskType: def.taskType}, 'Failed to enqueue cron job');
				}
			}
		}
	}
}
