// SPDX-License-Identifier: AGPL-3.0-or-later

import AppStorage from '@app/features/platform/state/PersistentStorage';
import {
	DEFAULT_VOLUME,
	MUTE_STORAGE_KEY,
	VOLUME_STORAGE_KEY,
} from '@app/features/voice/components/media_player/utils/MediaConstants';
import {useCallback, useEffect, useRef, useState, useSyncExternalStore} from 'react';

const VOLUME_CURVE_EXPONENT = 2.8;
const VOLUME_SETTING_EPSILON = 1e-6;

export function mediaGainFromVolumeSetting(setting: number): number {
	if (!Number.isFinite(setting) || setting <= 0) return 0;
	if (setting >= 1) return 1;
	return setting ** VOLUME_CURVE_EXPONENT;
}

export function volumeSettingFromMediaGain(gain: number): number {
	if (!Number.isFinite(gain) || gain <= 0) return 0;
	if (gain >= 1) return 1;
	return gain ** (1 / VOLUME_CURVE_EXPONENT);
}

interface UseMediaVolumeOptions {
	mediaRef: React.RefObject<HTMLMediaElement | null>;
	initialVolume?: number;
	initialMuted?: boolean;
	persist?: boolean;
	onVolumeChange?: (volume: number) => void;
	onMuteChange?: (muted: boolean) => void;
}

export interface UseMediaVolumeReturn {
	volume: number;
	isMuted: boolean;
	previousVolume: number;
	setVolume: (volume: number) => void;
	toggleMute: () => void;
	setMuted: (muted: boolean) => void;
	increaseVolume: (step?: number) => void;
	decreaseVolume: (step?: number) => void;
}

function getStoredVolume(): number {
	try {
		const stored = AppStorage.getItem(VOLUME_STORAGE_KEY);
		if (stored !== null) {
			const value = parseFloat(stored);
			if (Number.isFinite(value) && value >= 0 && value <= 1) {
				return volumeSettingFromMediaGain(value);
			}
		}
	} catch {}
	return DEFAULT_VOLUME;
}

function getStoredMuted(): boolean {
	try {
		return AppStorage.getItem(MUTE_STORAGE_KEY) === 'true';
	} catch {
		return false;
	}
}

function storeVolume(setting: number): void {
	try {
		AppStorage.setItem(VOLUME_STORAGE_KEY, mediaGainFromVolumeSetting(setting).toString());
	} catch {}
}

function storeMuted(muted: boolean): void {
	try {
		AppStorage.setItem(MUTE_STORAGE_KEY, muted.toString());
	} catch {}
}

interface StoredVolumeSetting {
	volume: number;
	muted: boolean;
}

let storedVolumeSetting: StoredVolumeSetting | null = null;
const storedVolumeListeners = new Set<() => void>();

function readStoredVolumeSetting(): StoredVolumeSetting {
	storedVolumeSetting ??= {volume: getStoredVolume(), muted: getStoredMuted()};
	return storedVolumeSetting;
}

function subscribeStoredVolumeSetting(listener: () => void): () => void {
	storedVolumeListeners.add(listener);
	return () => {
		storedVolumeListeners.delete(listener);
	};
}

function publishStoredVolumeSetting(next: StoredVolumeSetting): void {
	const current = readStoredVolumeSetting();
	if (Math.abs(current.volume - next.volume) < VOLUME_SETTING_EPSILON && current.muted === next.muted) {
		return;
	}
	storedVolumeSetting = next;
	storeVolume(next.volume);
	storeMuted(next.muted);
	for (const listener of Array.from(storedVolumeListeners)) {
		listener();
	}
}

export function resetStoredVolumeSettingForTests(): void {
	storedVolumeSetting = null;
	storedVolumeListeners.clear();
}

export function useMediaVolume(options: UseMediaVolumeOptions): UseMediaVolumeReturn {
	const {mediaRef, initialVolume, initialMuted, persist = true, onVolumeChange, onMuteChange} = options;
	const tracksSharedSetting = persist && initialVolume === undefined && initialMuted === undefined;
	const sharedSetting = useSyncExternalStore(
		subscribeStoredVolumeSetting,
		readStoredVolumeSetting,
		readStoredVolumeSetting,
	);
	const [ownVolume, setOwnVolume] = useState(() => initialVolume ?? (persist ? getStoredVolume() : DEFAULT_VOLUME));
	const [ownMuted, setOwnMuted] = useState(() => initialMuted ?? (persist ? getStoredMuted() : false));
	const volume = tracksSharedSetting ? sharedSetting.volume : ownVolume;
	const isMuted = tracksSharedSetting ? sharedSetting.muted : ownMuted;
	const previousVolumeRef = useRef(volume > 0 ? volume : DEFAULT_VOLUME);
	const settingRef = useRef({volume, isMuted});
	settingRef.current = {volume, isMuted};
	const commitSetting = useCallback(
		(nextVolume: number, nextMuted: boolean) => {
			if (tracksSharedSetting) {
				publishStoredVolumeSetting({volume: nextVolume, muted: nextMuted});
				return;
			}
			setOwnVolume(nextVolume);
			setOwnMuted(nextMuted);
			if (persist) {
				storeVolume(nextVolume);
				storeMuted(nextMuted);
			}
		},
		[persist, tracksSharedSetting],
	);
	useEffect(() => {
		const media = mediaRef.current;
		if (!media) return;
		media.volume = mediaGainFromVolumeSetting(volume);
		media.muted = isMuted;
		const handleVolumeChange = () => {
			const setting = volumeSettingFromMediaGain(media.volume);
			if (setting > 0) {
				previousVolumeRef.current = setting;
			}
			const current = settingRef.current;
			if (Math.abs(current.volume - setting) < VOLUME_SETTING_EPSILON && current.isMuted === media.muted) {
				return;
			}
			commitSetting(setting, media.muted);
		};
		media.addEventListener('volumechange', handleVolumeChange);
		return () => {
			media.removeEventListener('volumechange', handleVolumeChange);
		};
	}, [mediaRef, volume, isMuted, commitSetting]);
	const setMuted = useCallback(
		(muted: boolean) => {
			const media = mediaRef.current;
			let nextVolume = settingRef.current.volume;
			if (!muted && nextVolume === 0) {
				nextVolume = previousVolumeRef.current;
			}
			if (media) {
				media.muted = muted;
				media.volume = mediaGainFromVolumeSetting(nextVolume);
			}
			commitSetting(nextVolume, muted);
			onMuteChange?.(muted);
		},
		[mediaRef, commitSetting, onMuteChange],
	);
	const setVolume = useCallback(
		(newVolume: number) => {
			const media = mediaRef.current;
			const clampedVolume = Math.max(0, Math.min(1, newVolume));
			if (media) {
				media.volume = mediaGainFromVolumeSetting(clampedVolume);
			}
			if (clampedVolume > 0) {
				previousVolumeRef.current = clampedVolume;
			}
			const nextMuted = clampedVolume === 0;
			if (media) {
				media.muted = nextMuted;
			}
			commitSetting(clampedVolume, nextMuted);
			onVolumeChange?.(clampedVolume);
		},
		[mediaRef, commitSetting, onVolumeChange],
	);
	const toggleMute = useCallback(() => {
		setMuted(!isMuted);
	}, [isMuted, setMuted]);
	const increaseVolume = useCallback(
		(step = 0.1) => {
			setVolume(Math.min(1, volume + step));
		},
		[volume, setVolume],
	);
	const decreaseVolume = useCallback(
		(step = 0.1) => {
			setVolume(Math.max(0, volume - step));
		},
		[volume, setVolume],
	);
	return {
		volume,
		isMuted,
		previousVolume: previousVolumeRef.current,
		setVolume,
		toggleMute,
		setMuted,
		increaseVolume,
		decreaseVolume,
	};
}
