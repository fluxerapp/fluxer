/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {useCallback, useEffect, useRef, useState} from 'react';
import {DEFAULT_VOLUME, MUTE_STORAGE_KEY, PLAYBACK_RATE_STORAGE_KEY, VOLUME_STORAGE_KEY} from '../utils/mediaConstants';

export interface MediaPlayerState {
	isPlaying: boolean;
	isPaused: boolean;
	isEnded: boolean;
	isBuffering: boolean;
	isSeeking: boolean;
	currentTime: number;
	duration: number;
	bufferedRanges: TimeRanges | null;
	volume: number;
	isMuted: boolean;
	playbackRate: number;
	error: Error | null;
}

export interface UseMediaPlayerOptions {
	autoPlay?: boolean;
	loop?: boolean;
	muted?: boolean;
	initialVolume?: number;
	initialPlaybackRate?: number;
	persistVolume?: boolean;
	persistPlaybackRate?: boolean;
	onEnded?: () => void;
	onError?: (error: Error) => void;
	onPlay?: () => void;
	onPause?: () => void;
	onTimeUpdate?: (currentTime: number) => void;
	onLoadedMetadata?: (duration: number) => void;
}

export interface UseMediaPlayerReturn {
	mediaRef: React.RefObject<HTMLMediaElement | null>;
	state: MediaPlayerState;
	play: () => Promise<void>;
	pause: () => void;
	toggle: () => Promise<void>;
	seek: (time: number) => void;
	seekRelative: (delta: number) => void;
	seekPercentage: (percentage: number) => void;
	setVolume: (volume: number) => void;
	toggleMute: () => void;
	setPlaybackRate: (rate: number) => void;
}

function getStoredVolume(): number {
	try {
		const stored = localStorage.getItem(VOLUME_STORAGE_KEY);
		if (stored !== null) {
			const value = parseFloat(stored);
			if (Number.isFinite(value) && value >= 0 && value <= 1) {
				return value;
			}
		}
	} catch {}
	return DEFAULT_VOLUME;
}

function getStoredMuted(): boolean {
	try {
		return localStorage.getItem(MUTE_STORAGE_KEY) === 'true';
	} catch {
		return false;
	}
}

function getStoredPlaybackRate(): number {
	try {
		const stored = localStorage.getItem(PLAYBACK_RATE_STORAGE_KEY);
		if (stored !== null) {
			const value = parseFloat(stored);
			if (Number.isFinite(value) && value >= 0.25 && value <= 4) {
				return value;
			}
		}
	} catch {}
	return 1;
}

function storeVolume(volume: number): void {
	try {
		localStorage.setItem(VOLUME_STORAGE_KEY, volume.toString());
	} catch {}
}

function storeMuted(muted: boolean): void {
	try {
		localStorage.setItem(MUTE_STORAGE_KEY, muted.toString());
	} catch {}
}

function storePlaybackRate(rate: number): void {
	try {
		localStorage.setItem(PLAYBACK_RATE_STORAGE_KEY, rate.toString());
	} catch {}
}

const isAbortError = (error: unknown): boolean => {
	if (!error || typeof error !== 'object') return false;
	const name = (error as {name?: unknown}).name;
	if (name === 'AbortError') return true;
	const message = (error as {message?: unknown}).message;
	return typeof message === 'string' && message.toLowerCase().includes('interrupted');
};

const normalizeError = (error: unknown): Error => {
	if (error instanceof Error) return error;
	return new Error(typeof error === 'string' ? error : 'Unknown media error');
};

export function useMediaPlayer(options: UseMediaPlayerOptions = {}): UseMediaPlayerReturn {
	const {
		autoPlay = false,
		loop = false,
		muted: initialMuted,
		initialVolume,
		initialPlaybackRate,
		persistVolume = true,
		persistPlaybackRate = true,
		onEnded,
		onError,
		onPlay,
		onPause,
		onTimeUpdate,
		onLoadedMetadata,
	} = options;

	const mediaRef = useRef<HTMLMediaElement | null>(null);
	const previousVolumeRef = useRef<number>(DEFAULT_VOLUME);

	const callbacksRef = useRef({onEnded, onError, onPlay, onPause, onTimeUpdate, onLoadedMetadata});
	callbacksRef.current = {onEnded, onError, onPlay, onPause, onTimeUpdate, onLoadedMetadata};

	const [state, setState] = useState<MediaPlayerState>(() => ({
		isPlaying: false,
		isPaused: true,
		isEnded: false,
		isBuffering: false,
		isSeeking: false,
		currentTime: 0,
		duration: 0,
		bufferedRanges: null,
		volume: initialVolume ?? (persistVolume ? getStoredVolume() : DEFAULT_VOLUME),
		isMuted: initialMuted ?? (persistVolume ? getStoredMuted() : false),
		playbackRate: initialPlaybackRate ?? (persistPlaybackRate ? getStoredPlaybackRate() : 1),
		error: null,
	}));

	useEffect(() => {
		const media = mediaRef.current;
		if (!media) return;

		media.volume = state.volume;
		media.muted = state.isMuted;
		media.playbackRate = state.playbackRate;
		media.loop = loop;
	}, []);

	useEffect(() => {
		const media = mediaRef.current;
		if (!media) return;

		const handlePlay = () => {
			setState((prev) => ({
				...prev,
				isPlaying: true,
				isPaused: false,
				isEnded: false,
			}));
			callbacksRef.current.onPlay?.();
		};

		const handlePause = () => {
			setState((prev) => ({
				...prev,
				isPlaying: false,
				isPaused: true,
			}));
			callbacksRef.current.onPause?.();
		};

		const handleEnded = () => {
			setState((prev) => ({
				...prev,
				isPlaying: false,
				isPaused: true,
				isEnded: true,
			}));
			callbacksRef.current.onEnded?.();
		};

		const handleTimeUpdate = () => {
			const currentTime = media.currentTime;
			setState((prev) => ({
				...prev,
				currentTime,
				bufferedRanges: media.buffered,
			}));
			callbacksRef.current.onTimeUpdate?.(currentTime);
		};

		const handleLoadedMetadata = () => {
			const duration = media.duration;
			setState((prev) => ({
				...prev,
				duration: Number.isFinite(duration) ? duration : 0,
			}));
			callbacksRef.current.onLoadedMetadata?.(duration);
		};

		const handleDurationChange = () => {
			const duration = media.duration;
			setState((prev) => ({
				...prev,
				duration: Number.isFinite(duration) ? duration : 0,
			}));
		};

		const handleWaiting = () => {
			setState((prev) => ({...prev, isBuffering: true}));
		};

		const handleCanPlay = () => {
			setState((prev) => ({...prev, isBuffering: false}));
		};

		const handleSeeking = () => {
			setState((prev) => ({...prev, isSeeking: true}));
		};

		const handleSeeked = () => {
			setState((prev) => ({...prev, isSeeking: false}));
		};

		const handleVolumeChange = () => {
			setState((prev) => ({
				...prev,
				volume: media.volume,
				isMuted: media.muted,
			}));
		};

		const handleRateChange = () => {
			setState((prev) => ({
				...prev,
				playbackRate: media.playbackRate,
			}));
		};

		const handleError = () => {
			const error = media.error;
			const errorMessage = error
				? new Error(error.message || 'Media playback error')
				: new Error('Unknown media error');
			setState((prev) => ({...prev, error: errorMessage}));
			callbacksRef.current.onError?.(errorMessage);
		};

		const handleProgress = () => {
			setState((prev) => ({
				...prev,
				bufferedRanges: media.buffered,
			}));
		};

		media.addEventListener('play', handlePlay);
		media.addEventListener('pause', handlePause);
		media.addEventListener('ended', handleEnded);
		media.addEventListener('timeupdate', handleTimeUpdate);
		media.addEventListener('loadedmetadata', handleLoadedMetadata);
		media.addEventListener('durationchange', handleDurationChange);
		media.addEventListener('waiting', handleWaiting);
		media.addEventListener('canplay', handleCanPlay);
		media.addEventListener('seeking', handleSeeking);
		media.addEventListener('seeked', handleSeeked);
		media.addEventListener('volumechange', handleVolumeChange);
		media.addEventListener('ratechange', handleRateChange);
		media.addEventListener('error', handleError);
		media.addEventListener('progress', handleProgress);

		if (media.readyState >= 1) {
			handleLoadedMetadata();
		}

		return () => {
			media.removeEventListener('play', handlePlay);
			media.removeEventListener('pause', handlePause);
			media.removeEventListener('ended', handleEnded);
			media.removeEventListener('timeupdate', handleTimeUpdate);
			media.removeEventListener('loadedmetadata', handleLoadedMetadata);
			media.removeEventListener('durationchange', handleDurationChange);
			media.removeEventListener('waiting', handleWaiting);
			media.removeEventListener('canplay', handleCanPlay);
			media.removeEventListener('seeking', handleSeeking);
			media.removeEventListener('seeked', handleSeeked);
			media.removeEventListener('volumechange', handleVolumeChange);
			media.removeEventListener('ratechange', handleRateChange);
			media.removeEventListener('error', handleError);
			media.removeEventListener('progress', handleProgress);
		};
	}, []);

	useEffect(() => {
		const media = mediaRef.current;
		if (!media || !autoPlay) return;

		media.play().catch((error) => {
			console.debug('Autoplay prevented:', error);
		});
	}, [autoPlay]);

	const play = useCallback(async () => {
		const media = mediaRef.current;
		if (!media) return;

		try {
			await media.play();
			setState((prev) => ({...prev, error: null}));
		} catch (error) {
			if (isAbortError(error)) {
				console.debug('Play interrupted before it could start:', error);
				return;
			}

			const normalizedError = normalizeError(error);
			console.error('Play failed:', normalizedError);
			setState((prev) => ({...prev, error: normalizedError}));
		}
	}, []);

	const pause = useCallback(() => {
		const media = mediaRef.current;
		if (!media) return;
		media.pause();
	}, []);

	const toggle = useCallback(async () => {
		const media = mediaRef.current;
		if (!media) return;

		if (media.paused) {
			await play();
		} else {
			pause();
		}
	}, [play, pause]);

	const seek = useCallback((time: number) => {
		const media = mediaRef.current;
		if (!media) return;

		const clampedTime = Math.max(0, Math.min(time, media.duration || 0));
		media.currentTime = clampedTime;
	}, []);

	const seekRelative = useCallback(
		(delta: number) => {
			const media = mediaRef.current;
			if (!media) return;

			seek(media.currentTime + delta);
		},
		[seek],
	);

	const seekPercentage = useCallback((percentage: number) => {
		const media = mediaRef.current;
		if (!media || !Number.isFinite(media.duration)) return;

		const clampedPercentage = Math.max(0, Math.min(100, percentage));
		const time = (clampedPercentage / 100) * media.duration;
		media.currentTime = time;
	}, []);

	const setVolume = useCallback(
		(volume: number) => {
			const media = mediaRef.current;
			if (!media) return;

			const clampedVolume = Math.max(0, Math.min(1, volume));
			media.volume = clampedVolume;

			if (clampedVolume > 0) {
				previousVolumeRef.current = clampedVolume;
			}

			if (persistVolume) {
				storeVolume(clampedVolume);
			}
		},
		[persistVolume],
	);

	const toggleMute = useCallback(() => {
		const media = mediaRef.current;
		if (!media) return;

		const newMuted = !media.muted;
		media.muted = newMuted;

		if (!newMuted && media.volume === 0) {
			media.volume = previousVolumeRef.current || DEFAULT_VOLUME;
		}

		if (persistVolume) {
			storeMuted(newMuted);
		}
	}, [persistVolume]);

	const setPlaybackRate = useCallback(
		(rate: number) => {
			const media = mediaRef.current;
			if (!media) return;

			const clampedRate = Math.max(0.25, Math.min(4, rate));
			media.playbackRate = clampedRate;

			if (persistPlaybackRate) {
				storePlaybackRate(clampedRate);
			}
		},
		[persistPlaybackRate],
	);

	return {
		mediaRef,
		state,
		play,
		pause,
		toggle,
		seek,
		seekRelative,
		seekPercentage,
		setVolume,
		toggleMute,
		setPlaybackRate,
	};
}
