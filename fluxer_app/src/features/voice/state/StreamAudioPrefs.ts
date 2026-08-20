// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {makePersistent} from '@app/features/platform/utils/MobXPersistence';
import {clampVoiceVolumePercent, recalibrateStoredVoiceVolumePercent} from '@app/features/voice/utils/VoiceVolumeUtils';
import {
	STREAM_AUDIO_PREFS_PRUNE_INTERVAL_MS,
	STREAM_AUDIO_PREFS_TOUCH_INTERVAL_MS,
	STREAM_AUDIO_PREFS_TTL_MS,
} from '@fluxer/constants/src/StreamConstants';
import {makeAutoObservable} from 'mobx';

const logger = new Logger('StreamAudioPrefs');

interface StreamAudioPrefsEntry {
	volume: number;
	muted: boolean;
	lastAccessed: number;
}

class StreamAudioPrefs {
	entries: Record<string, StreamAudioPrefsEntry> = {};
	outputVolumeRecalibratedV1 = false;
	audioPrefsRevision = 0;
	private pruneIntervalId: number | null = null;
	private listeners = new Set<() => void>();

	constructor() {
		makeAutoObservable<this, 'pruneIntervalId' | 'listeners' | 'notifyListeners' | 'hasAnyEntries'>(
			this,
			{
				pruneIntervalId: false,
				listeners: false,
				hasEntry: false,
				getVolume: false,
				isMuted: false,
				notifyListeners: false,
				hasAnyEntries: false,
			},
			{autoBind: true},
		);
		void this.initPersistence();
	}

	private async initPersistence(): Promise<void> {
		await makePersistent(this, 'StreamAudioPrefs', ['entries', 'outputVolumeRecalibratedV1']);
		this.applyOutputVolumeRecalibration();
		if (this.hasAnyEntries()) {
			this.startPruneInterval();
		}
	}

	private applyOutputVolumeRecalibration(): void {
		if (this.outputVolumeRecalibratedV1) {
			return;
		}
		this.entries = Object.fromEntries(
			Object.entries(this.entries).map(([streamKey, entry]) => [
				streamKey,
				{...entry, volume: recalibrateStoredVoiceVolumePercent(entry.volume)},
			]),
		);
		this.outputVolumeRecalibratedV1 = true;
	}

	hasEntry(streamKey: string): boolean {
		return streamKey in this.entries;
	}

	getVolume(streamKey: string): number {
		return clampVoiceVolumePercent(this.entries[streamKey]?.volume ?? 100);
	}

	isMuted(streamKey: string): boolean {
		return this.entries[streamKey]?.muted ?? false;
	}

	setVolume(streamKey: string, volume: number): void {
		const clamped = clampVoiceVolumePercent(volume);
		const existing = this.entries[streamKey];
		if (existing && existing.volume === clamped) {
			this.touchStream(streamKey);
			return;
		}
		this.entries = {
			...this.entries,
			[streamKey]: {
				volume: clamped,
				muted: existing?.muted ?? false,
				lastAccessed: Date.now(),
			},
		};
		this.audioPrefsRevision += 1;
		this.startPruneInterval();
		this.notifyListeners();
		logger.debug('Set stream volume', {streamKey, volume: clamped});
	}

	setMuted(streamKey: string, muted: boolean): void {
		const existing = this.entries[streamKey];
		if (existing && existing.muted === muted) {
			this.touchStream(streamKey);
			return;
		}
		this.entries = {
			...this.entries,
			[streamKey]: {
				volume: existing?.volume ?? 100,
				muted,
				lastAccessed: Date.now(),
			},
		};
		this.audioPrefsRevision += 1;
		this.startPruneInterval();
		this.notifyListeners();
		logger.debug('Set stream mute', {streamKey, muted});
	}

	touchStream(streamKey: string): void {
		const existing = this.entries[streamKey];
		if (!existing) return;
		const now = Date.now();
		if (now - existing.lastAccessed < STREAM_AUDIO_PREFS_TOUCH_INTERVAL_MS) return;
		this.entries = {
			...this.entries,
			[streamKey]: {
				...existing,
				lastAccessed: now,
			},
		};
		this.startPruneInterval();
	}

	private startPruneInterval(): void {
		if (this.pruneIntervalId != null) {
			return;
		}
		this.pruneIntervalId = window.setInterval(() => {
			this.pruneExpiredEntries();
		}, STREAM_AUDIO_PREFS_PRUNE_INTERVAL_MS);
	}

	private pruneExpiredEntries(): void {
		const cutoff = Date.now() - STREAM_AUDIO_PREFS_TTL_MS;
		const nextEntries = {...this.entries};
		let removedCount = 0;
		for (const [streamKey, entry] of Object.entries(this.entries)) {
			if (entry.lastAccessed < cutoff) {
				delete nextEntries[streamKey];
				removedCount += 1;
			}
		}
		if (removedCount > 0) {
			this.entries = nextEntries;
			this.audioPrefsRevision += 1;
			this.notifyListeners();
			logger.debug('Pruned stream audio prefs', {removedCount});
		}
		this.stopPruneIntervalIfIdle();
	}

	subscribe(listener: () => void): () => void {
		this.listeners.add(listener);
		return () => {
			this.listeners.delete(listener);
		};
	}

	private notifyListeners(): void {
		for (const listener of [...this.listeners]) {
			listener();
		}
	}

	private hasAnyEntries(): boolean {
		for (const _streamKey in this.entries) {
			return true;
		}
		return false;
	}

	private stopPruneIntervalIfIdle(): void {
		if (this.hasAnyEntries()) return;
		if (this.pruneIntervalId == null) return;
		window.clearInterval(this.pruneIntervalId);
		this.pruneIntervalId = null;
	}
}

export default new StreamAudioPrefs();
