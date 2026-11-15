// SPDX-License-Identifier: AGPL-3.0-or-later

import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';

interface UploadEntry {
	percent: number;
	totalBytes: number;
}

const inflight = new Map<string, UploadEntry>();

let scheduled = false;
let lastReportedFraction = -1;

function flushSoon(): void {
	if (scheduled) return;
	scheduled = true;
	queueMicrotask(() => {
		scheduled = false;
		flush();
	});
}

function computeAggregate(): number {
	if (inflight.size === 0) return -1;
	let weightedSum = 0;
	let weightTotal = 0;
	let plainSum = 0;
	let count = 0;
	for (const entry of inflight.values()) {
		const p = Math.max(0, Math.min(1, entry.percent / 100));
		count += 1;
		plainSum += p;
		if (entry.totalBytes > 0) {
			weightedSum += p * entry.totalBytes;
			weightTotal += entry.totalBytes;
		}
	}
	if (weightTotal > 0) return weightedSum / weightTotal;
	if (count > 0) return plainSum / count;
	return -1;
}

function flush(): void {
	const electronApi = getElectronAPI();
	if (!electronApi || typeof electronApi.setTaskbarProgress !== 'function') return;
	const fraction = computeAggregate();
	const rounded = fraction < 0 ? -1 : Math.round(fraction * 200) / 200;
	if (rounded === lastReportedFraction) return;
	lastReportedFraction = rounded;
	if (rounded < 0) {
		electronApi.setTaskbarProgress(-1, 'none');
		return;
	}
	const value = rounded >= 0.995 ? 1 : rounded;
	electronApi.setTaskbarProgress(value, 'normal');
}

export function trackUpload(nonce: string, percent: number, totalBytes: number): void {
	if (!nonce) return;
	const sanitizedPercent = Number.isFinite(percent) ? Math.max(0, Math.min(100, percent)) : 0;
	const sanitizedBytes = Number.isFinite(totalBytes) && totalBytes > 0 ? totalBytes : 0;
	inflight.set(nonce, {percent: sanitizedPercent, totalBytes: sanitizedBytes});
	flushSoon();
}

export function completeUpload(nonce: string): void {
	if (!nonce) return;
	if (!inflight.delete(nonce)) return;
	flushSoon();
}
