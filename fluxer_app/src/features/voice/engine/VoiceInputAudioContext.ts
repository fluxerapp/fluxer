// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {createVoiceAudioContext} from '@app/features/voice/engine/VoiceSharedAudioContext';
import NoiseSuppressionAvailability from '@app/features/voice/utils/noise_suppression/NoiseSuppressionAvailability';

const logger = new Logger('VoiceInputAudioContext');
const VOICE_INPUT_SAMPLE_RATE = 48000;
const RESUME_GESTURE_EVENTS = ['pointerdown', 'keydown'] as const;
const IDLE_SUSPEND_DELAY_MS = 500;
const IDLE_CLOSE_DELAY_MS = 60_000;
const RATE_KEYED_CANDIDATE_RATES = [48000, 44100, 32000, 24000, 16000];

export interface VoiceInputContextLease {
	readonly context: AudioContext;
	release(): void;
}

export interface VoiceInputSourceLease {
	readonly lease: VoiceInputContextLease;
	readonly source: MediaStreamAudioSourceNode;
}

interface VoiceInputContextEntry {
	readonly context: AudioContext;
	readonly resumeOnGesture: () => void;
	holders: number;
	sourceHolders: number;
	suspendTimer: ReturnType<typeof setTimeout> | null;
	closeTimer: ReturnType<typeof setTimeout> | null;
	suspending: boolean;
}

let rateKeyedMode = false;
const entries = new Set<VoiceInputContextEntry>();

function needsResume(entry: VoiceInputContextEntry): boolean {
	return entry.holders > 0 && entry.context.state !== 'running' && entry.context.state !== 'closed';
}

function resumeEntry(entry: VoiceInputContextEntry): void {
	if (!needsResume(entry) && !(entry.holders > 0 && entry.suspending)) return;
	void entry.context.resume().catch((error) => {
		logger.debug('Voice input AudioContext resume rejected', {error});
	});
}

function syncEntryResume(entry: VoiceInputContextEntry): void {
	const armGesture = needsResume(entry);
	for (const type of RESUME_GESTURE_EVENTS) {
		if (armGesture) window.addEventListener(type, entry.resumeOnGesture, {capture: true});
		else window.removeEventListener(type, entry.resumeOnGesture, {capture: true});
	}
	resumeEntry(entry);
}

function createEntry(options: AudioContextOptions, shared: boolean): VoiceInputContextEntry | null {
	const context = createVoiceAudioContext({latencyHint: 'interactive', ...options});
	if (!context) return null;
	const entry: VoiceInputContextEntry = {
		context,
		holders: 0,
		sourceHolders: 0,
		suspendTimer: null,
		closeTimer: null,
		suspending: false,
		resumeOnGesture: () => resumeEntry(entry),
	};
	context.addEventListener('statechange', () => {
		if (context.state === 'closed') entries.delete(entry);
		syncEntryResume(entry);
	});
	if (shared) entries.add(entry);
	return entry;
}

function closeEntry(entry: VoiceInputContextEntry): void {
	if (entry.suspendTimer !== null) clearTimeout(entry.suspendTimer);
	if (entry.closeTimer !== null) clearTimeout(entry.closeTimer);
	entries.delete(entry);
	if (entry.context.state === 'closed') return;
	void entry.context.close().catch((error) => {
		logger.debug('Failed to close a voice input AudioContext', {error});
	});
}

function readTrackSampleRate(track: MediaStreamTrack | null): number | null {
	const sampleRate = track?.getSettings().sampleRate;
	return typeof sampleRate === 'number' && sampleRate > 0 ? sampleRate : null;
}

function resolveEntry(): VoiceInputContextEntry | null {
	for (const entry of entries) return entry;
	return createEntry(rateKeyedMode ? {} : {sampleRate: VOICE_INPUT_SAMPLE_RATE}, true);
}

export function isVoiceInputSourceLive(): boolean {
	for (const entry of entries) {
		if (entry.sourceHolders > 0) return true;
	}
	return false;
}

function isUnsupportedRateError(error: unknown): boolean {
	return error instanceof DOMException && error.name === 'NotSupportedError';
}

function suspendIdleEntry(entry: VoiceInputContextEntry): void {
	entry.suspendTimer = null;
	if (entry.holders > 0 || entry.context.state === 'closed') return;
	entry.suspending = true;
	void entry.context
		.suspend()
		.catch((error) => {
			logger.debug('Voice input AudioContext suspend rejected', {error});
		})
		.finally(() => {
			entry.suspending = false;
			if (entry.holders === 0 && entry.context.state !== 'closed') {
				if (entry.closeTimer !== null) clearTimeout(entry.closeTimer);
				entry.closeTimer = setTimeout(() => {
					entry.closeTimer = null;
					if (entry.holders === 0) closeEntry(entry);
				}, IDLE_CLOSE_DELAY_MS);
			}
		});
}

function leaseEntry(entry: VoiceInputContextEntry, holdsSource: boolean): VoiceInputContextLease {
	if (entry.closeTimer !== null) {
		clearTimeout(entry.closeTimer);
		entry.closeTimer = null;
	}
	if (entry.suspendTimer !== null) {
		clearTimeout(entry.suspendTimer);
		entry.suspendTimer = null;
	}
	entry.holders++;
	if (holdsSource) entry.sourceHolders++;
	if (entries.has(entry)) NoiseSuppressionAvailability.setInputContextRate(entry.context.sampleRate);
	if (entry.holders === 1) syncEntryResume(entry);
	let released = false;
	return {
		context: entry.context,
		release: () => {
			if (released) return;
			released = true;
			entry.holders--;
			if (holdsSource) entry.sourceHolders--;
			if (entry.holders > 0) return;
			syncEntryResume(entry);
			if (!entries.has(entry)) {
				closeEntry(entry);
				return;
			}
			entry.suspendTimer = setTimeout(() => suspendIdleEntry(entry), IDLE_SUSPEND_DELAY_MS);
		},
	};
}

export function acquireVoiceInputContext(): VoiceInputContextLease | null {
	const entry = resolveEntry();
	return entry ? leaseEntry(entry, false) : null;
}

export function acquireIdleVoiceInputContext(): VoiceInputContextLease | null {
	return isVoiceInputSourceLive() ? null : acquireVoiceInputContext();
}

function createSource(entry: VoiceInputContextEntry, track: MediaStreamTrack): MediaStreamAudioSourceNode | null {
	try {
		return entry.context.createMediaStreamSource(new MediaStream([track]));
	} catch (error) {
		if (!isUnsupportedRateError(error)) throw error;
		return null;
	}
}

function acquireRateKeyedSource(track: MediaStreamTrack, shared: boolean): VoiceInputSourceLease | null {
	const trackRate = readTrackSampleRate(track);
	for (const rate of trackRate === null ? RATE_KEYED_CANDIDATE_RATES : [trackRate]) {
		const existing = shared ? [...entries].find((entry) => entry.context.sampleRate === rate) : undefined;
		const entry = existing ?? createEntry({sampleRate: rate}, shared);
		if (!entry) continue;
		const source = createSource(entry, track);
		if (source) return {lease: leaseEntry(entry, true), source};
		if (!existing) closeEntry(entry);
	}
	return null;
}

function acquireSource(track: MediaStreamTrack, shared: boolean): VoiceInputSourceLease | null {
	if (rateKeyedMode) return acquireRateKeyedSource(track, shared);
	const entry = shared ? resolveEntry() : createEntry({sampleRate: VOICE_INPUT_SAMPLE_RATE}, false);
	if (!entry) return null;
	const source = createSource(entry, track);
	if (source) return {lease: leaseEntry(entry, true), source};
	logger.info('Voice input cannot read the microphone at 48 kHz; using a context at the capture rate');
	rateKeyedMode = true;
	entries.delete(entry);
	if (entry.holders === 0) closeEntry(entry);
	return acquireRateKeyedSource(track, shared);
}

export function acquireVoiceInputSource(track: MediaStreamTrack): VoiceInputSourceLease | null {
	return acquireSource(track, true);
}

export function acquireIdleVoiceInputSource(track: MediaStreamTrack): VoiceInputSourceLease | null {
	return acquireSource(track, !isVoiceInputSourceLive());
}

export function discardVoiceInputContext(context: AudioContext): void {
	for (const entry of entries) {
		if (entry.context !== context) continue;
		entries.delete(entry);
		if (entry.holders === 0) closeEntry(entry);
		return;
	}
}
