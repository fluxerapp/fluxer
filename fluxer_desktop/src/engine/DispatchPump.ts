// SPDX-License-Identifier: AGPL-3.0-or-later

const DISPATCH_SLICE_MS = 8;

export interface DispatchConsumer {
	hasPending(): boolean;
	processNext(): void;
}

export class DispatchPump {
	private readonly pending = new Set<DispatchConsumer>();
	private scheduled = false;
	private readonly sliceMs: number;
	private readonly now: () => number;

	constructor(sliceMs = DISPATCH_SLICE_MS, now: () => number = () => performance.now()) {
		this.sliceMs = sliceMs;
		this.now = now;
	}

	schedule(consumer: DispatchConsumer): void {
		this.pending.add(consumer);
		if (!this.scheduled) {
			this.scheduled = true;
			setImmediate(() => this.run());
		}
	}

	forget(consumer: DispatchConsumer): void {
		this.pending.delete(consumer);
	}

	private run(): void {
		this.scheduled = false;
		this.runSlice();
		if (this.pending.size > 0 && !this.scheduled) {
			this.scheduled = true;
			setImmediate(() => this.run());
		}
	}

	private runSlice(): void {
		const startedAt = this.now();
		while (this.pending.size > 0 && this.now() - startedAt < this.sliceMs) {
			for (const consumer of Array.from(this.pending)) {
				if (!consumer.hasPending()) {
					this.pending.delete(consumer);
					continue;
				}
				consumer.processNext();
				if (!consumer.hasPending()) {
					this.pending.delete(consumer);
				}
				if (this.now() - startedAt >= this.sliceMs) {
					return;
				}
			}
		}
	}
}
