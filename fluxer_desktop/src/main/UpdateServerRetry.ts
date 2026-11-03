// SPDX-License-Identifier: AGPL-3.0-or-later

import {jitteredExponentialDelay} from '@electron/common/ExponentialBackoff';
import {ModuleUpdaterStatus} from '@electron/main/ModuleUpdater';

const UPDATE_SERVER_RETRY_BASE_MS = 15_000;
const UPDATE_SERVER_RETRY_CAP_MS = 300_000;

const QUIET_STATUSES: ReadonlySet<ModuleUpdaterStatus> = new Set([
	ModuleUpdaterStatus.CHECKING,
	ModuleUpdaterStatus.RETRY_WAIT,
	ModuleUpdaterStatus.BLOCKED_UPDATE_REQUIRED,
]);

function nextUpdateServerRetryDelay(attempt: number): number {
	return jitteredExponentialDelay(
		UPDATE_SERVER_RETRY_BASE_MS,
		UPDATE_SERVER_RETRY_CAP_MS,
		Math.max(0, attempt),
		Math.random,
	);
}

export class UpdateServerRetry {
	private readonly wakers = new Set<() => void>();
	private attempt = 0;
	private held = false;

	public get holding(): boolean {
		return this.held;
	}

	public readonly sleep = (delayMs: number): Promise<void> => {
		return new Promise((resolve) => {
			const wake = (): void => {
				clearTimeout(timer);
				this.wakers.delete(wake);
				resolve();
			};
			const timer = setTimeout(wake, delayMs);
			this.wakers.add(wake);
		});
	};

	public async holdUntilNextAttempt(): Promise<void> {
		this.held = true;
		const delayMs = nextUpdateServerRetryDelay(this.attempt);
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
