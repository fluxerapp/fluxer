// SPDX-License-Identifier: AGPL-3.0-or-later

const RATE_SMOOTHING_WINDOW_MS = 3000;
const RATE_MIN_SAMPLE_MS = 200;

export class ModuleDownloadRate {
	private lastBytes: number | null = null;
	private lastAt = 0;
	private rate: number | null = null;

	public reset(): void {
		this.lastBytes = null;
		this.lastAt = 0;
		this.rate = null;
	}

	public sample(bytes: number, at: number): number | null {
		if (this.lastBytes == null || bytes < this.lastBytes) {
			this.lastBytes = bytes;
			this.lastAt = at;
			this.rate = null;
			return null;
		}
		const elapsed = at - this.lastAt;
		if (elapsed < RATE_MIN_SAMPLE_MS) {
			return this.rate;
		}
		const instant = ((bytes - this.lastBytes) * 1000) / elapsed;
		const weight = 1 - Math.exp(-elapsed / RATE_SMOOTHING_WINDOW_MS);
		this.rate = this.rate == null ? instant : this.rate + (instant - this.rate) * weight;
		this.lastBytes = bytes;
		this.lastAt = at;
		return this.rate;
	}
}
