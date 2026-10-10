// SPDX-License-Identifier: AGPL-3.0-or-later

export interface VoiceEngineV2ClockPort {
	now(): number;
}

export interface VoiceEngineV2RandomPort {
	next(): number;
}

export interface VoiceEngineV2WallClockSource {
	read(): number;
}

export interface VoiceEngineV2SystemClockPort extends VoiceEngineV2ClockPort {
	readonly sourceFailureCount: number;
}

const PLATFORM_CLOCK_MAX_MS = Number.MAX_SAFE_INTEGER;

interface PlatformSourceFailureLatch {
	count: number;
	logged: boolean;
	message: string;
}

function createPlatformSourceFailureLatch(message: string): PlatformSourceFailureLatch {
	return {count: 0, logged: false, message};
}

function recordPlatformSourceFailure(latch: PlatformSourceFailureLatch, error: unknown): void {
	latch.count += 1;
	if (latch.logged) return;
	latch.logged = true;
	globalThis.console.error(latch.message, error);
}

export function createVoiceEngineV2SystemClockPort(
	source?: VoiceEngineV2WallClockSource,
): VoiceEngineV2SystemClockPort {
	const reader = source ?? defaultSystemWallClockSource();
	const failureLatch = createPlatformSourceFailureLatch(
		'voice engine v2 wall clock source threw (programmer error); pinning clock readings to the last safe value',
	);
	let lastValue = -1;
	return {
		get sourceFailureCount(): number {
			return failureLatch.count;
		},
		now(): number {
			const raw = readClockSourceSafely(reader, failureLatch);
			const safe = clampPlatformClockReading(raw, lastValue);
			lastValue = safe;
			return safe;
		},
	};
}

export function createVoiceEngineV2DeterministicClockPort(start = 0, stepMs = 1): VoiceEngineV2ClockPort {
	let value = start;
	return {
		now(): number {
			const current = value;
			value += stepMs;
			return current;
		},
	};
}

function defaultSystemWallClockSource(): VoiceEngineV2WallClockSource {
	return {
		read(): number {
			return globalThis.Date.now();
		},
	};
}

function readClockSourceSafely(reader: VoiceEngineV2WallClockSource, latch: PlatformSourceFailureLatch): number {
	try {
		return reader.read();
	} catch (error) {
		recordPlatformSourceFailure(latch, error);
		return 0;
	}
}

function clampPlatformClockReading(raw: number, lastValue: number): number {
	if (!Number.isFinite(raw)) return Math.max(lastValue, 0);
	if (raw < 0) return Math.max(lastValue, 0);
	if (raw > PLATFORM_CLOCK_MAX_MS) return PLATFORM_CLOCK_MAX_MS;
	if (raw < lastValue) return lastValue;
	return Math.trunc(raw);
}
