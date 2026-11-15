// SPDX-License-Identifier: AGPL-3.0-or-later

import {action, makeAutoObservable} from 'mobx';

const INTERRUPTION_GRACE_MS = 4000;

export class GatewayConnectionInterruption {
	interrupted = false;
	private timer: number | null = null;

	constructor(private readonly isConnected: () => boolean) {
		makeAutoObservable<this, 'isConnected' | 'timer'>(
			this,
			{
				isConnected: false,
				timer: false,
			},
			{autoBind: true},
		);
	}

	beginGracePeriod(): void {
		if (this.timer !== null || this.interrupted) {
			return;
		}
		this.timer = window.setTimeout(
			action(() => {
				this.timer = null;
				if (!this.isConnected()) {
					this.interrupted = true;
				}
			}),
			INTERRUPTION_GRACE_MS,
		);
	}

	restore(): void {
		this.clearTimer();
		this.interrupted = false;
	}

	disconnect(): void {
		this.clearTimer();
		this.interrupted = true;
	}

	private clearTimer(): void {
		if (this.timer !== null) {
			clearTimeout(this.timer);
			this.timer = null;
		}
	}
}
