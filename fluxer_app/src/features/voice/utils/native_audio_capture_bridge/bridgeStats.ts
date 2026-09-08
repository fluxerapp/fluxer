// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {
	initialBridgeStats,
	type NativeAudioBridgeEndedCapture,
	type NativeAudioBridgeFrameMetrics,
	type NativeAudioBridgeStats,
} from '@app/features/voice/utils/native_audio_capture_bridge/shared';

const logger = new Logger('NativeAudioBridgeStats');

const MAX_ENDED_BRIDGE_CAPTURES = 8;
const SUPERSEDED_END_REASON = 'superseded';
const SUSTAINED_SILENCE_WARN_MS = 30_000;

let bridgeStats: NativeAudioBridgeStats = {...initialBridgeStats};
let endedBridgeCaptures: Array<NativeAudioBridgeEndedCapture> = [];

export function getBridgeStats(): NativeAudioBridgeStats {
	return {...bridgeStats};
}

export function getEndedBridgeCaptures(): Array<NativeAudioBridgeEndedCapture> {
	return endedBridgeCaptures.map((capture) => ({...capture}));
}

function retireBridgeStats(): void {
	if (bridgeStats.captureId === null) return;
	endedBridgeCaptures.push({
		captureId: bridgeStats.captureId,
		bridgeMode: bridgeStats.bridgeMode,
		startedAt: bridgeStats.startedAt,
		endedAt: bridgeStats.endedAt ?? Date.now(),
		endReason: bridgeStats.endReason ?? SUPERSEDED_END_REASON,
		endDetail: bridgeStats.endDetail,
		framesReceived: bridgeStats.framesReceived,
		nonSilentFrameCount: bridgeStats.nonSilentFrameCount,
		lastFramePeak: bridgeStats.lastFramePeak,
	});
	if (endedBridgeCaptures.length > MAX_ENDED_BRIDGE_CAPTURES) {
		endedBridgeCaptures = endedBridgeCaptures.slice(-MAX_ENDED_BRIDGE_CAPTURES);
	}
}

export function startBridgeStats(
	mode: 'generator' | 'script-processor',
	captureId: string,
	options: {prebufferTargetUs?: number; frameDurationUs?: number} = {},
): void {
	retireBridgeStats();
	bridgeStats = {
		...initialBridgeStats,
		active: true,
		bridgeMode: mode,
		captureId,
		startedAt: Date.now(),
		prebufferTargetMs: options.prebufferTargetUs == null ? null : Math.round(options.prebufferTargetUs / 100) / 10,
		frameDurationMs: options.frameDurationUs == null ? null : Math.round(options.frameDurationUs / 100) / 10,
	};
}

export function recordBridgeFrame(captureId: string, metrics: NativeAudioBridgeFrameMetrics = {}): void {
	if (!bridgeStats.active || bridgeStats.captureId !== captureId) return;
	const now = Date.now();
	if (bridgeStats.lastFrameAt != null) {
		const arrivalGapMs = Math.max(0, now - bridgeStats.lastFrameAt);
		bridgeStats.maxFrameArrivalGapMs = Math.max(bridgeStats.maxFrameArrivalGapMs, arrivalGapMs);
		const expectedMs = metrics.durationUs == null ? 20 : metrics.durationUs / 1_000;
		if (arrivalGapMs > Math.max(80, expectedMs * 3)) {
			bridgeStats.lateFrameCount += 1;
		}
	}
	if (metrics.timestampUs != null && bridgeStats.lastFrameTimestampUs != null) {
		const timestampGapMs = Math.max(0, (metrics.timestampUs - bridgeStats.lastFrameTimestampUs) / 1_000);
		bridgeStats.maxFrameTimestampGapMs = Math.max(bridgeStats.maxFrameTimestampGapMs, Math.round(timestampGapMs));
	}
	bridgeStats.framesReceived += 1;
	bridgeStats.lastFrameAt = now;
	if (metrics.timestampUs != null) {
		bridgeStats.lastFrameTimestampUs = Math.max(0, Math.round(metrics.timestampUs));
	}
	if (metrics.peak != null || metrics.rms != null) {
		const peak = clampAudioLevel(metrics.peak ?? 0);
		const rms = clampAudioLevel(metrics.rms ?? 0);
		bridgeStats.lastFramePeak = roundAudioLevel(peak);
		bridgeStats.lastFrameRms = roundAudioLevel(rms);
		bridgeStats.maxFramePeak = Math.max(bridgeStats.maxFramePeak, bridgeStats.lastFramePeak);
		bridgeStats.maxFrameRms = Math.max(bridgeStats.maxFrameRms, bridgeStats.lastFrameRms);
		if (peak >= 0.0005 || rms >= 0.0001) {
			bridgeStats.nonSilentFrameCount += 1;
			bridgeStats.lastNonSilentFrameAt = now;
			bridgeStats.silentFrameStreak = 0;
		} else {
			bridgeStats.silentFrameStreak += 1;
			const silentSinceMs = bridgeStats.lastNonSilentFrameAt ?? bridgeStats.startedAt ?? now;
			const silentRunMs = Math.max(0, now - silentSinceMs);
			bridgeStats.maxSilentRunMs = Math.max(bridgeStats.maxSilentRunMs, silentRunMs);
			if (!bridgeStats.sustainedSilenceWarned && silentRunMs >= SUSTAINED_SILENCE_WARN_MS) {
				bridgeStats.sustainedSilenceWarned = true;
				logger.warn('Native audio capture is delivering frames that contain only silence', {
					captureId: bridgeStats.captureId,
					bridgeMode: bridgeStats.bridgeMode,
					framesReceived: bridgeStats.framesReceived,
					nonSilentFrameCount: bridgeStats.nonSilentFrameCount,
					silentFrameStreak: bridgeStats.silentFrameStreak,
					silentRunMs,
				});
			}
		}
	}
}

function clampAudioLevel(value: number): number {
	if (!Number.isFinite(value)) return 0;
	return Math.min(1, Math.max(0, value));
}

function roundAudioLevel(value: number): number {
	return Math.round(value * 1_000_000) / 1_000_000;
}

export function recordBridgeFrameDrops(captureId: string, count: number): void {
	if (!bridgeStats.active || bridgeStats.captureId !== captureId || count <= 0) return;
	bridgeStats.framesDropped += count;
}

export function recordBridgeRebuffer(captureId: string): void {
	if (!bridgeStats.active || bridgeStats.captureId !== captureId) return;
	bridgeStats.rebufferCount += 1;
}

export function recordBridgePrebufferTarget(captureId: string, targetUs: number): void {
	if (!bridgeStats.active || bridgeStats.captureId !== captureId) return;
	bridgeStats.prebufferTargetMs = Math.round(targetUs / 100) / 10;
}

export function recordBridgeQueue(captureId: string, pendingFrames: number, bufferedDurationUs: number): void {
	if (!bridgeStats.active || bridgeStats.captureId !== captureId) return;
	bridgeStats.maxPendingFrames = Math.max(bridgeStats.maxPendingFrames, pendingFrames);
	bridgeStats.maxBufferedDurationMs = Math.max(
		bridgeStats.maxBufferedDurationMs,
		Math.round(bufferedDurationUs / 100) / 10,
	);
}

export function endBridgeStats(captureId: string, reason: string | null, detail: string | null): void {
	if (bridgeStats.active && bridgeStats.captureId === captureId) {
		bridgeStats.active = false;
		bridgeStats.endReason = reason;
		bridgeStats.endDetail = detail;
		bridgeStats.endedAt = Date.now();
		return;
	}
	const ended = endedBridgeCaptures.find((capture) => capture.captureId === captureId);
	if (!ended || ended.endReason !== SUPERSEDED_END_REASON) return;
	ended.endReason = reason;
	ended.endDetail = detail;
	ended.endedAt = Date.now();
}
