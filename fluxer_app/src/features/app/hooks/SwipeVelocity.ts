// SPDX-License-Identifier: AGPL-3.0-or-later

export const SWIPE_VELOCITY_THRESHOLD = 0.4;

export interface VelocitySample {
	x: number;
	y: number;
	timestamp: number;
}

export function pushVelocitySample(samples: Array<VelocitySample>, x: number, y: number): void {
	samples.push({x, y, timestamp: performance.now()});
	if (samples.length > 10) samples.splice(0, samples.length - 10);
}

export function calculateSwipeVelocity(samples: ReadonlyArray<VelocitySample>, now: number): number {
	let firstRecentIndex = 0;
	while (firstRecentIndex < samples.length && now - samples[firstRecentIndex].timestamp >= 100) firstRecentIndex++;
	if (samples.length - firstRecentIndex < 2) return 0;
	const first = samples[firstRecentIndex];
	const last = samples[samples.length - 1];
	const dt = last.timestamp - first.timestamp;
	return dt === 0 ? 0 : Math.sqrt((last.x - first.x) ** 2 + (last.y - first.y) ** 2) / dt;
}
