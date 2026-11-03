// SPDX-License-Identifier: AGPL-3.0-or-later

import type {EngineClock, EngineTimer} from '@electron/engine/EngineClock';

export const SCHEDULER_LIMITS = Object.freeze({
	perOriginHandshakes: 2,
	totalHandshakes: 6,
	originStaggerMs: 250,
	resumeStaggerMs: 50,
	slotTimeoutMs: 15_000,
	backoffBaseMs: 1_000,
	backoffCapMs: 60_000,
	rateLimitFloorMs: 30_000,
	onlineResumeStaggerMs: 50,
});

export interface SchedulerClient {
	readonly accountKey: string;
	readonly origin: string;
	readonly priority: number;
	readonly resuming?: boolean;
	onSlotGranted(): void;
}

interface QueueEntry {
	readonly client: SchedulerClient;
	readonly order: number;
}

interface HeldSlot {
	readonly origin: string;
	readonly timer: EngineTimer;
}

interface RetryEntry {
	readonly run: () => void;
	readonly dueAt: number;
	timer: EngineTimer | null;
}

export class SessionScheduler {
	private readonly clock: EngineClock;
	private readonly queue: Array<QueueEntry> = [];
	private readonly held = new Map<string, HeldSlot>();
	private readonly inFlightByOrigin = new Map<string, number>();
	private readonly nextStartByOrigin = new Map<string, number>();
	private readonly retries = new Map<string, RetryEntry>();
	private pumpTimer: EngineTimer | null = null;
	private order = 0;
	private online = true;
	private pumping = false;
	private pumpAgain = false;

	constructor(clock: EngineClock) {
		this.clock = clock;
	}

	get isOnline(): boolean {
		return this.online;
	}

	requestSlot(client: SchedulerClient): void {
		this.cancel(client.accountKey);
		this.queue.push({client, order: this.order++});
		this.pump();
	}

	releaseSlot(accountKey: string): void {
		const slot = this.held.get(accountKey);
		if (slot === undefined) {
			return;
		}
		this.held.delete(accountKey);
		this.clock.clearTimeout(slot.timer);
		const remaining = (this.inFlightByOrigin.get(slot.origin) ?? 1) - 1;
		if (remaining <= 0) {
			this.inFlightByOrigin.delete(slot.origin);
		} else {
			this.inFlightByOrigin.set(slot.origin, remaining);
		}
		this.pump();
	}

	cancel(accountKey: string): void {
		const index = this.queue.findIndex((entry) => entry.client.accountKey === accountKey);
		if (index >= 0) {
			this.queue.splice(index, 1);
		}
		this.cancelRetry(accountKey);
		this.releaseSlot(accountKey);
	}

	backoffDelay(attempt: number, floorMs = 0): number {
		const exponent = Math.min(Math.max(0, attempt), 16);
		const ceiling = Math.min(SCHEDULER_LIMITS.backoffCapMs, SCHEDULER_LIMITS.backoffBaseMs * 2 ** exponent);
		const jittered = Math.floor(this.clock.random() * ceiling);
		return Math.max(floorMs, jittered);
	}

	scheduleRetry(accountKey: string, delayMs: number, run: () => void): number {
		this.cancelRetry(accountKey);
		const entry: RetryEntry = {run, dueAt: this.clock.now() + delayMs, timer: null};
		this.retries.set(accountKey, entry);
		if (this.online) {
			this.armRetry(accountKey, entry, delayMs);
		}
		return entry.dueAt;
	}

	cancelRetry(accountKey: string): void {
		const entry = this.retries.get(accountKey);
		if (entry === undefined) {
			return;
		}
		this.retries.delete(accountKey);
		if (entry.timer !== null) {
			this.clock.clearTimeout(entry.timer);
		}
	}

	setOnline(online: boolean): void {
		if (this.online === online) {
			return;
		}
		this.online = online;
		if (!online) {
			for (const entry of this.retries.values()) {
				if (entry.timer !== null) {
					this.clock.clearTimeout(entry.timer);
					entry.timer = null;
				}
			}
			return;
		}
		let index = 0;
		for (const [accountKey, entry] of this.retries) {
			this.armRetry(accountKey, entry, index * SCHEDULER_LIMITS.onlineResumeStaggerMs);
			index += 1;
		}
		this.pump();
	}

	dispose(): void {
		for (const accountKey of Array.from(this.retries.keys())) {
			this.cancelRetry(accountKey);
		}
		for (const slot of this.held.values()) {
			this.clock.clearTimeout(slot.timer);
		}
		this.held.clear();
		this.inFlightByOrigin.clear();
		this.queue.length = 0;
		if (this.pumpTimer !== null) {
			this.clock.clearTimeout(this.pumpTimer);
			this.pumpTimer = null;
		}
	}

	private armRetry(accountKey: string, entry: RetryEntry, delayMs: number): void {
		entry.timer = this.clock.setTimeout(() => {
			if (this.retries.get(accountKey) !== entry) {
				return;
			}
			this.retries.delete(accountKey);
			entry.run();
		}, delayMs);
	}

	private pump(): void {
		if (this.pumping) {
			this.pumpAgain = true;
			return;
		}
		this.pumping = true;
		try {
			do {
				this.pumpAgain = false;
				this.pumpOnce();
			} while (this.pumpAgain);
		} finally {
			this.pumping = false;
		}
	}

	private pumpOnce(): void {
		if (!this.online || this.queue.length === 0) {
			return;
		}
		this.queue.sort((left, right) => right.client.priority - left.client.priority || left.order - right.order);
		const now = this.clock.now();
		let earliestWait = Number.POSITIVE_INFINITY;
		for (let index = 0; index < this.queue.length && this.held.size < SCHEDULER_LIMITS.totalHandshakes; ) {
			const {client} = this.queue[index];
			const origin = client.origin;
			if ((this.inFlightByOrigin.get(origin) ?? 0) >= SCHEDULER_LIMITS.perOriginHandshakes) {
				index += 1;
				continue;
			}
			const nextStart = this.nextStartByOrigin.get(origin) ?? 0;
			if (nextStart > now) {
				earliestWait = Math.min(earliestWait, nextStart - now);
				index += 1;
				continue;
			}
			this.queue.splice(index, 1);
			this.grant(client, now);
		}
		if (Number.isFinite(earliestWait) && this.pumpTimer === null) {
			this.pumpTimer = this.clock.setTimeout(() => {
				this.pumpTimer = null;
				this.pump();
			}, earliestWait);
		}
	}

	private grant(client: SchedulerClient, now: number): void {
		const origin = client.origin;
		this.inFlightByOrigin.set(origin, (this.inFlightByOrigin.get(origin) ?? 0) + 1);
		this.nextStartByOrigin.set(
			origin,
			now + (client.resuming === true ? SCHEDULER_LIMITS.resumeStaggerMs : SCHEDULER_LIMITS.originStaggerMs),
		);
		const timer = this.clock.setTimeout(() => this.releaseSlot(client.accountKey), SCHEDULER_LIMITS.slotTimeoutMs);
		this.held.set(client.accountKey, {origin, timer});
		client.onSlotGranted();
	}
}
