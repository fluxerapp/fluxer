// SPDX-License-Identifier: AGPL-3.0-or-later

import {writeFileSync} from 'node:fs';
import type {ILogger} from '../ILogger';

const WORKER_HEARTBEAT_PATH = '/tmp/fluxer-worker-heartbeat';
export const WORKER_HEARTBEAT_WRITE_INTERVAL_MS = 5000;
export const WORKER_LANE_HEARTBEAT_INTERVAL_MS = 5000;
export const WORKER_LANE_STALE_AFTER_MS = 30000;
export const WORKER_CRON_STALE_AFTER_MS = 90000;

export interface WorkerHeartbeatSignal {
	report(): void;
	release(): void;
}

interface WorkerHeartbeatOptions {
	logger: Pick<ILogger, 'info' | 'error'>;
	path?: string;
	intervalMs?: number;
	now?: () => number;
	write?: (path: string, contents: string) => void;
}

interface WorkerHeartbeatComponent {
	staleAfterMs: number;
	lastReportedAt: number;
}

export class WorkerHeartbeat {
	private readonly logger: Pick<ILogger, 'info' | 'error'>;
	private readonly path: string;
	private readonly intervalMs: number;
	private readonly now: () => number;
	private readonly write: (path: string, contents: string) => void;
	private readonly components = new Map<string, WorkerHeartbeatComponent>();
	private intervalId: NodeJS.Timeout | null = null;
	private stalled = false;

	constructor(options: WorkerHeartbeatOptions) {
		this.logger = options.logger;
		this.path = options.path ?? WORKER_HEARTBEAT_PATH;
		this.intervalMs = options.intervalMs ?? WORKER_HEARTBEAT_WRITE_INTERVAL_MS;
		this.now = options.now ?? Date.now;
		this.write = options.write ?? ((path, contents) => writeFileSync(path, contents));
	}

	getPath(): string {
		return this.path;
	}

	register(name: string, staleAfterMs: number): WorkerHeartbeatSignal {
		const component: WorkerHeartbeatComponent = {staleAfterMs, lastReportedAt: this.now()};
		this.components.set(name, component);
		return {
			report: () => {
				component.lastReportedAt = this.now();
			},
			release: () => {
				if (this.components.get(name) === component) {
					this.components.delete(name);
				}
			},
		};
	}

	stalledComponents(): Array<string> {
		const at = this.now();
		const stalled: Array<string> = [];
		for (const [name, component] of this.components) {
			if (at - component.lastReportedAt > component.staleAfterMs) {
				stalled.push(name);
			}
		}
		return stalled;
	}

	writeOnce(): boolean {
		const stalled = this.stalledComponents();
		if (stalled.length > 0) {
			if (!this.stalled) {
				this.stalled = true;
				this.logger.error(
					{components: stalled, path: this.path},
					'Worker heartbeat stalled, the container will report unhealthy',
				);
			}
			return false;
		}
		try {
			this.write(this.path, this.snapshot());
		} catch (error) {
			this.logger.error({err: error, path: this.path}, 'Failed to write the worker heartbeat file');
			return false;
		}
		if (this.stalled) {
			this.stalled = false;
			this.logger.info({path: this.path}, 'Worker heartbeat recovered');
		}
		return true;
	}

	start(): void {
		if (this.intervalId !== null) {
			return;
		}
		this.writeOnce();
		this.intervalId = setInterval(() => {
			this.writeOnce();
		}, this.intervalMs);
		this.logger.info(
			{path: this.path, intervalMs: this.intervalMs, components: [...this.components.keys()]},
			'Worker heartbeat started',
		);
	}

	stop(): void {
		if (this.intervalId !== null) {
			clearInterval(this.intervalId);
			this.intervalId = null;
		}
	}

	private snapshot(): string {
		const at = this.now();
		return JSON.stringify({
			at: new Date(at).toISOString(),
			components: [...this.components].map(([name, component]) => ({
				name,
				ageMs: at - component.lastReportedAt,
			})),
		});
	}
}
