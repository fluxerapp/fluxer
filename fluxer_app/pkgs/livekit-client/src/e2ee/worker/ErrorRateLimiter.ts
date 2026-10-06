// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
export class ErrorRateLimiter {
	private lastAt: Map<string, number> = new Map();

	private counts: Map<string, number> = new Map();

	constructor(
		private readonly throttleMs: number = 1000,
		private readonly windowMs: number = 60_000,
		private readonly maxPerWindow: number = 5,
	) {}

	reset() {
		this.lastAt.clear();
		this.counts.clear();
	}

	countFor(key: string): number {
		return this.counts.get(key) ?? 0;
	}

	shouldEmit(key: string, onSuppress?: () => void): boolean {
		const now = Date.now();
		const last = this.lastAt.get(key) ?? 0;
		const count = this.counts.get(key) ?? 0;

		if (now - last > this.windowMs) {
			this.counts.set(key, 0);
			this.lastAt.set(key, now);
			return true;
		}
		if (now - last < this.throttleMs) return false;
		if (count >= this.maxPerWindow) {
			if (count === this.maxPerWindow) {
				onSuppress?.();
				this.counts.set(key, count + 1);
			}
			return false;
		}
		this.lastAt.set(key, now);
		this.counts.set(key, count + 1);
		return true;
	}
}
