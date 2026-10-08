// SPDX-License-Identifier: AGPL-3.0-or-later

import {ModuleUpdaterStatus} from '@electron/main/ModuleUpdater';

export const UPDATE_SERVER_RETRY_BASE_MS = 15_000;
export const UPDATE_SERVER_RETRY_CAP_MS = 300_000;

const QUIET_STATUSES: ReadonlySet<ModuleUpdaterStatus> = new Set([
	ModuleUpdaterStatus.CHECKING,
	ModuleUpdaterStatus.RETRY_WAIT,
	ModuleUpdaterStatus.BLOCKED_UPDATE_REQUIRED,
]);

interface UpdateServerRetryDependencies {
	readonly random?: () => number;
	readonly schedule?: (callback: () => void, delayMs: number) => NodeJS.Timeout;
	readonly cancel?: (timer: NodeJS.Timeout) => void;
}

export function nextUpdateServerRetryDelay(attempt: number, random: () => number = Math.random): number {
	const exponential = Math.min(UPDATE_SERVER_RETRY_BASE_MS * 2 ** Math.max(0, attempt), UPDATE_SERVER_RETRY_CAP_MS);
	return Math.round(exponential * (0.5 + random() * 0.5));
}

export class UpdateServerRetry {
	private readonly random: () => number;
	private readonly schedule: (callback: () => void, delayMs: number) => NodeJS.Timeout;
	private readonly cancel: (timer: NodeJS.Timeout) => void;
	private readonly wakers = new Set<() => void>();
	private attempt = 0;
	private held = false;

	public constructor(dependencies: UpdateServerRetryDependencies = {}) {
		this.random = dependencies.random ?? Math.random;
		this.schedule = dependencies.schedule ?? ((callback, delayMs) => setTimeout(callback, delayMs));
		this.cancel = dependencies.cancel ?? ((timer) => clearTimeout(timer));
	}

	public get holding(): boolean {
		return this.held;
	}

	public readonly sleep = (delayMs: number): Promise<void> => {
		return new Promise((resolve) => {
			const wake = (): void => {
				this.cancel(timer);
				this.wakers.delete(wake);
				resolve();
			};
			const timer = this.schedule(wake, delayMs);
			this.wakers.add(wake);
		});
	};

	public async holdUntilNextAttempt(): Promise<void> {
		this.held = true;
		const delayMs = nextUpdateServerRetryDelay(this.attempt, this.random);
		this.attempt += 1;
		await this.sleep(delayMs);
	}

	public networkReturned(): void {
		this.attempt = 0;
		for (const wake of Array.from(this.wakers)) {
			wake();
		}
	}

	public endsHold(status: ModuleUpdaterStatus): boolean {
		if (!this.held || QUIET_STATUSES.has(status)) {
			return false;
		}
		this.held = false;
		this.attempt = 0;
		return true;
	}
}
