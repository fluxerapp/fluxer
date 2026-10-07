// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import type {VoiceEngineV2EventLogEntry, VoiceEngineV2EventLogSpillSink} from '@fluxer/voice_engine_v2';

const VOICE_ENGINE_V2_APP_EVENT_LOG_SPILL_WARN_INTERVAL_MS = 60_000;
const VOICE_ENGINE_V2_APP_EVENT_LOG_SPILL_WARN_INTERVAL_MAX_MS = 3_600_000;

const RESOLVED_SPILL_WRITE = Promise.resolve();

interface VoiceEngineV2AppEventLogSpillLogger {
	warn(payload: Record<string, unknown>, message?: string): void;
}

export interface VoiceEngineV2AppEventLogSpillLoggerSinkOptions {
	logger: VoiceEngineV2AppEventLogSpillLogger;
	warnIntervalMs?: number;
	nowMs?: () => number;
	onLoggerError?: (error: unknown) => void;
}

export interface VoiceEngineV2AppEventLogSpillLoggerSink extends VoiceEngineV2EventLogSpillSink {
	readonly swallowedErrorCount: number;
}

function assertTimestampMs(value: number, label: string): void {
	assert.equal(typeof value, 'number', `${label} must be a number`);
	assert.ok(Number.isFinite(value), `${label} must be finite`);
	assert.ok(value >= 0, `${label} must be non-negative`);
}

function shouldWarn(nowMs: number, lastWarnAtMs: number | null, warnIntervalMs: number): boolean {
	assertTimestampMs(nowMs, 'event log spill warning timestamp');
	assert.ok(warnIntervalMs >= 1, 'event log spill warning interval must be positive');
	if (lastWarnAtMs === null) return true;
	assertTimestampMs(lastWarnAtMs, 'last event log spill warning timestamp');
	return nowMs - lastWarnAtMs >= warnIntervalMs;
}

export function createVoiceEngineV2AppEventLogSpillLoggerSink(
	options: VoiceEngineV2AppEventLogSpillLoggerSinkOptions,
): VoiceEngineV2AppEventLogSpillLoggerSink {
	assert.ok(options !== null && typeof options === 'object', 'event log spill logger sink requires options');
	assert.ok(
		options.logger !== null && typeof options.logger === 'object',
		'event log spill logger sink requires a logger',
	);
	assert.equal(
		typeof options.logger.warn,
		'function',
		'event log spill logger sink requires logger.warn to be callable',
	);
	const warnIntervalMs = options.warnIntervalMs ?? VOICE_ENGINE_V2_APP_EVENT_LOG_SPILL_WARN_INTERVAL_MS;
	assert.ok(Number.isInteger(warnIntervalMs), 'event log spill warning interval must be an integer');
	assert.ok(warnIntervalMs >= 1, 'event log spill warning interval must be >= 1');
	assert.ok(
		warnIntervalMs <= VOICE_ENGINE_V2_APP_EVENT_LOG_SPILL_WARN_INTERVAL_MAX_MS,
		'event log spill warning interval exceeds the maximum',
	);
	if (options.nowMs !== undefined) {
		assert.equal(typeof options.nowMs, 'function', 'event log spill logger sink nowMs must be callable');
	}
	const logger = options.logger;
	const nowMs = options.nowMs ?? Date.now;
	const onLoggerError = options.onLoggerError;
	let swallowedErrorCount = 0;
	let evictedEntryCount = 0;
	let suppressedEvictedEntryCount = 0;
	let lastWarnAtMs: number | null = null;
	return {
		get swallowedErrorCount(): number {
			assert.ok(swallowedErrorCount >= 0, 'swallowed error count must stay non-negative');
			return swallowedErrorCount;
		},
		write(entry: VoiceEngineV2EventLogEntry): Promise<void> {
			assert.ok(entry !== null && typeof entry === 'object', 'event log spill entry must be an object');
			assert.ok(Number.isInteger(entry.sequence), 'event log spill entry sequence must be an integer');
			assert.ok(entry.sequence >= 1, 'event log spill entry sequence must be >= 1');
			const currentMs = nowMs();
			assertTimestampMs(currentMs, 'event log spill logger timestamp');
			evictedEntryCount += 1;
			assert.ok(evictedEntryCount >= 1, 'evicted entry count must increment');
			if (!shouldWarn(currentMs, lastWarnAtMs, warnIntervalMs)) {
				suppressedEvictedEntryCount += 1;
				assert.ok(suppressedEvictedEntryCount >= 1, 'suppressed evicted entry count must increment');
				return RESOLVED_SPILL_WRITE;
			}
			lastWarnAtMs = currentMs;
			try {
				logger.warn({
					code: 'eventLogEvicted',
					entry,
					evictedEntryCount,
					suppressedEvictedEntryCount,
				});
				suppressedEvictedEntryCount = 0;
			} catch (error) {
				swallowedErrorCount += 1;
				suppressedEvictedEntryCount += 1;
				assert.ok(swallowedErrorCount > 0, 'swallowed error count must increment on logger failure');
				if (onLoggerError) onLoggerError(error);
			}
			return RESOLVED_SPILL_WRITE;
		},
	};
}
