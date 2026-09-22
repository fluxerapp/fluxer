// SPDX-License-Identifier: AGPL-3.0-or-later

import {IS_DEV} from '@app/features/platform/types/Env';
import LocalPresence from '@app/features/presence/state/LocalPresence';
import {makeAutoObservable} from 'mobx';

const IDLE_DURATION_MS = 1000 * (IS_DEV ? 10 : 60 * 10);
const IDLE_CHECK_INTERVAL_MS = Math.min(Math.floor(IDLE_DURATION_MS * 0.25), 30_000);
const SYSTEM_IDLE_RETRY_DELAY_MS = 60_000;

interface DesktopIdleApi {
	getSystemIdleTimeMs?: () => Promise<number>;
}

function getDesktopIdleApi(): Required<DesktopIdleApi> | null {
	const electronApi = (
		window as {
			electron?: DesktopIdleApi | null;
		}
	).electron;
	return typeof electronApi?.getSystemIdleTimeMs === 'function'
		? {getSystemIdleTimeMs: electronApi.getSystemIdleTimeMs.bind(electronApi)}
		: null;
}

function normalizeIdleTimeMs(value: number): number | null {
	if (!Number.isFinite(value) || value < 0) return null;
	return Math.floor(value);
}

class Idle {
	idle = false;
	private lastLocalActivityTime = Date.now();
	private lastSystemActivityTime = 0;
	private checkInterval: NodeJS.Timeout | null = null;
	private systemIdleCheckInFlight = false;
	private lastSystemIdleFailureAt = 0;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		this.startIdleCheck();
	}

	private startIdleCheck(): void {
		if (typeof setInterval !== 'function') return;
		this.checkInterval = setInterval(() => {
			this.updateIdleState();
		}, IDLE_CHECK_INTERVAL_MS);
	}

	destroy(): void {
		if (this.checkInterval !== null) {
			clearInterval(this.checkInterval);
			this.checkInterval = null;
		}
	}

	recordActivity(): void {
		this.lastLocalActivityTime = Date.now();
		if (this.idle) {
			this.applyIdleState(false);
		}
	}

	markBackground(): void {
		this.lastLocalActivityTime = 0;
		this.lastSystemActivityTime = 0;
		this.applyIdleState(true);
	}

	isIdle(): boolean {
		return this.idle;
	}

	getIdleSince(): number {
		return this.idle ? this.getLastActivityTime() : 0;
	}

	getInactiveDurationMs(now = Date.now()): number {
		return Math.max(0, now - this.getLastActivityTime());
	}

	private getLastActivityTime(): number {
		return Math.max(this.lastLocalActivityTime, this.lastSystemActivityTime);
	}

	private updateIdleState(): void {
		const desktopIdleApi = getDesktopIdleApi();
		if (
			desktopIdleApi &&
			!this.systemIdleCheckInFlight &&
			Date.now() - this.lastSystemIdleFailureAt >= SYSTEM_IDLE_RETRY_DELAY_MS
		) {
			void this.updateIdleStateFromSystem(desktopIdleApi);
			return;
		}
		this.updateIdleStateFromLocalActivity();
	}

	private updateIdleStateFromLocalActivity(): void {
		const now = Date.now();
		this.applyIdleState(this.getInactiveDurationMs(now) >= IDLE_DURATION_MS);
	}

	private async updateIdleStateFromSystem(desktopIdleApi: Required<DesktopIdleApi>): Promise<void> {
		this.systemIdleCheckInFlight = true;
		try {
			const idleTimeMs = normalizeIdleTimeMs(await desktopIdleApi.getSystemIdleTimeMs());
			if (idleTimeMs === null) {
				this.lastSystemIdleFailureAt = Date.now();
			} else {
				this.lastSystemActivityTime = Math.max(this.lastSystemActivityTime, Date.now() - idleTimeMs);
			}
		} catch {
			this.lastSystemIdleFailureAt = Date.now();
		} finally {
			this.systemIdleCheckInFlight = false;
		}
		this.updateIdleStateFromLocalActivity();
	}

	private applyIdleState(idle: boolean): void {
		if (idle !== this.idle) {
			this.idle = idle;
			LocalPresence.updatePresence();
		}
	}
}

export default new Idle();
