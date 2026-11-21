// SPDX-License-Identifier: AGPL-3.0-or-later

const WATERMARK_MARGIN_MS = 5 * 60_000;

export interface ReplayProgress {
	pending: number;
	ackPending: number;
	oldestUnackedMs: number | null;
	latestStreamMs: number | null;
}

export interface ReplayStatus {
	drained: boolean;
	broken: string | null;
	watermark: number | null;
	lag_seconds: number;
	pending_records: number;
	awaiting_outcome: number;
}

interface ReplayStatusInput {
	progress: ReplayProgress;
	nowMs: number;
	awaitingOutcome: number;
	producersFinished: boolean;
	broken: string | null;
}

export function replayStatus({
	progress,
	nowMs,
	awaitingOutcome,
	producersFinished,
	broken,
}: ReplayStatusInput): ReplayStatus {
	const {pending, ackPending, oldestUnackedMs, latestStreamMs} = progress;
	const basis = Math.min(latestStreamMs ?? Number.POSITIVE_INFINITY, oldestUnackedMs ?? Number.POSITIVE_INFINITY);
	return {
		drained: broken === null && producersFinished && pending === 0 && ackPending === 0 && awaitingOutcome === 0,
		broken,
		watermark: Number.isFinite(basis) ? (basis - WATERMARK_MARGIN_MS) * 1000 : null,
		lag_seconds: oldestUnackedMs === null ? 0 : Math.max(0, (nowMs - oldestUnackedMs) / 1000),
		pending_records: pending + ackPending,
		awaiting_outcome: awaitingOutcome,
	};
}
