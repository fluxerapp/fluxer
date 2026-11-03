// SPDX-License-Identifier: AGPL-3.0-or-later

import {jitteredExponentialDelay} from '@electron/common/ExponentialBackoff';
import {type ModulePollPlan, nextModulePollDelay} from '@electron/common/ModulePollPlan';
import type {DesktopUpdateState} from '@fluxer/desktop_ipc/src/ModuleContract';

const DESKTOP_UPDATE_POLL_BACKOFF_CAP_MS = 15 * 60 * 1000;

interface DesktopUpdatePollLogger {
	readonly info: (message: string, details?: unknown) => void;
	readonly warn: (message: string, details?: unknown) => void;
}

interface DesktopUpdatePollOptions {
	readonly plan: ModulePollPlan;
	readonly check: () => Promise<DesktopUpdateState>;
	readonly logger: DesktopUpdatePollLogger;
}

function setUnrefTimer(callback: () => void, delayMs: number): NodeJS.Timeout {
	const timer = setTimeout(callback, delayMs);
	timer.unref();
	return timer;
}

class DesktopUpdatePoll {
	private readonly options: DesktopUpdatePollOptions;
	private timer: NodeJS.Timeout | null = null;
	private inFlight = false;
	private failures = 0;
	private available = false;

	public constructor(options: DesktopUpdatePollOptions) {
		this.options = options;
	}

	public start(immediately: boolean): void {
		this.options.logger.info('Polling the desktop update feeds', {
			intervalMs: this.options.plan.intervalMs,
			jitterRatio: this.options.plan.jitterRatio,
		});
		if (immediately) {
			this.tick();
		} else {
			this.schedule();
		}
	}

	public wake(): void {
		this.tick();
	}

	private nextDelay(): number {
		const delay = nextModulePollDelay(this.options.plan);
		if (this.failures === 0) {
			return delay;
		}
		return Math.max(
			delay,
			jitteredExponentialDelay(
				this.options.plan.intervalMs,
				DESKTOP_UPDATE_POLL_BACKOFF_CAP_MS,
				this.failures,
				Math.random,
			),
		);
	}

	private schedule(): void {
		this.timer = setUnrefTimer(() => {
			this.timer = null;
			this.tick();
		}, this.nextDelay());
	}

	private tick(): void {
		if (this.inFlight) {
			return;
		}
		if (this.timer != null) {
			clearTimeout(this.timer);
			this.timer = null;
		}
		this.inFlight = true;
		const {logger} = this.options;
		void this.options
			.check()
			.then((state) => {
				if (this.failures > 0) {
					logger.info('The desktop update poll recovered', {failures: this.failures});
					this.failures = 0;
				}
				if (state.available && !this.available) {
					logger.info('A desktop update is available, waiting for the user to start it');
				}
				this.available = state.available;
			})
			.catch((error: unknown) => {
				this.failures += 1;
				if (this.failures === 1) {
					logger.warn('The desktop update poll failed, backing off until it recovers', error);
				}
			})
			.finally(() => {
				this.inFlight = false;
				this.schedule();
			});
	}
}

let armedPoll: DesktopUpdatePoll | null = null;

export function armDesktopUpdatePoll(options: DesktopUpdatePollOptions, immediately: boolean): void {
	if (armedPoll != null) {
		return;
	}
	armedPoll = new DesktopUpdatePoll(options);
	armedPoll.start(immediately);
}

export function wakeDesktopUpdatePoll(): void {
	armedPoll?.wake();
}
