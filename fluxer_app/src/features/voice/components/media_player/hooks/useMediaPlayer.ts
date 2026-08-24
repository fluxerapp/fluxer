// SPDX-License-Identifier: AGPL-3.0-or-later

import AppStorage from '@app/features/platform/state/PersistentStorage';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {
	armPendingSeekTarget,
	clampMediaTime,
	clearPendingSeekTarget,
	detachMediaElementSource,
	quantiseMediaTimeToSecond,
	readPendingSeekTarget,
} from '@app/features/voice/components/media_player/utils/MediaSeekUtils';
import {useInAppMediaSoundCapture} from '@app/features/voice/hooks/useInAppMediaSoundCapture';
import {useCallback, useEffect, useLayoutEffect, useRef, useState} from 'react';

const PLAYBACK_RATE_STORAGE_KEY = 'fluxer:media:playbackRates';
const logger = new Logger('useMediaPlayer');

export type PlaybackHoldReason = 'user' | 'hidden-document' | 'seek';

export type MediaPlaybackKind = 'video' | 'audio' | 'voice-message';

export type MediaFailureCode = 'error' | 'abort' | 'emptied' | 'stalled';

export interface MediaPlayerState {
	isPlaying: boolean;
	isPaused: boolean;
	isEnded: boolean;
	isBuffering: boolean;
	isSeeking: boolean;
	currentTime: number;
	duration: number;
	playbackRate: number;
	error: Error | null;
	failureCode: MediaFailureCode | null;
}

export interface UseMediaPlayerOptions {
	mediaKind?: MediaPlaybackKind;
	autoPlay?: boolean;
	loop?: boolean;
	initialPlaybackRate?: number;
	persistPlaybackRate?: boolean;
	onEnded?: () => void;
	onError?: (error: Error, code: MediaFailureCode) => void;
	onPlay?: () => void;
	onPause?: () => void;
	onTimeUpdate?: (currentTime: number) => void;
	onLoadedMetadata?: (duration: number, media: HTMLMediaElement) => void;
}

export interface UseMediaPlayerReturn {
	mediaRef: React.RefObject<HTMLMediaElement | null>;
	state: MediaPlayerState;
	play: () => Promise<void>;
	pause: (reason?: PlaybackHoldReason) => void;
	resumeHold: (reason: PlaybackHoldReason) => boolean;
	releaseHold: (reason: PlaybackHoldReason) => boolean;
	toggle: () => Promise<void>;
	seek: (time: number) => void;
	seekRelative: (delta: number) => void;
	seekPercentage: (percentage: number) => void;
	setPlaybackRate: (rate: number) => void;
}

function readStoredPlaybackRates(): Record<string, unknown> {
	try {
		const stored = AppStorage.getItem(PLAYBACK_RATE_STORAGE_KEY);
		if (stored === null) return {};
		const parsed: unknown = JSON.parse(stored);
		if (parsed === null || typeof parsed !== 'object' || Array.isArray(parsed)) return {};
		return parsed as Record<string, unknown>;
	} catch {}
	return {};
}

function getStoredPlaybackRate(kind: MediaPlaybackKind): number {
	const value = readStoredPlaybackRates()[kind];
	if (typeof value === 'number' && Number.isFinite(value) && value >= 0.25 && value <= 4) {
		return value;
	}
	return 1;
}

function storePlaybackRate(kind: MediaPlaybackKind, rate: number): void {
	try {
		AppStorage.setItem(PLAYBACK_RATE_STORAGE_KEY, JSON.stringify({...readStoredPlaybackRates(), [kind]: rate}));
	} catch {}
}

const isAbortError = (error: unknown): boolean => {
	if (!(error instanceof Error)) return false;
	if (error.name === 'AbortError') return true;
	return error.message.toLowerCase().includes('interrupted');
};

export const isAutoplayBlockedError = (error: unknown): boolean =>
	error instanceof Error && error.name === 'NotAllowedError';

const normalizeError = (error: unknown): Error => {
	if (error instanceof Error) return error;
	return new Error(typeof error === 'string' ? error : 'Unknown media error');
};

export function useMediaPlayer(options: UseMediaPlayerOptions = {}): UseMediaPlayerReturn {
	const {
		mediaKind = 'video',
		autoPlay = false,
		loop = false,
		initialPlaybackRate,
		persistPlaybackRate = true,
		onEnded,
		onError,
		onPlay,
		onPause,
		onTimeUpdate,
		onLoadedMetadata,
	} = options;
	const mediaRef = useRef<HTMLMediaElement | null>(null);
	useInAppMediaSoundCapture(mediaRef);
	const callbacksRef = useRef({onEnded, onError, onPlay, onPause, onTimeUpdate, onLoadedMetadata});
	callbacksRef.current = {onEnded, onError, onPlay, onPause, onTimeUpdate, onLoadedMetadata};
	const initialElementSettingsRef = useRef({loop});
	const [state, setState] = useState<MediaPlayerState>(() => ({
		isPlaying: false,
		isPaused: true,
		isEnded: false,
		isBuffering: false,
		isSeeking: false,
		currentTime: 0,
		duration: 0,
		playbackRate: initialPlaybackRate ?? (persistPlaybackRate ? getStoredPlaybackRate(mediaKind) : 1),
		error: null,
		failureCode: null,
	}));
	const holdReasonRef = useRef<PlaybackHoldReason | null>(null);
	const recoverPlaybackWhenReadyRef = useRef(false);
	const initialPlaybackRateRef = useRef(state.playbackRate);
	useEffect(() => {
		const media = mediaRef.current;
		if (!media) return;
		const settings = initialElementSettingsRef.current;
		media.defaultPlaybackRate = initialPlaybackRateRef.current;
		media.playbackRate = initialPlaybackRateRef.current;
		media.loop = settings.loop;
	}, []);
	useEffect(() => {
		const media = mediaRef.current;
		if (!media) return;
		const handlePlay = () => {
			holdReasonRef.current = null;
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
			clearPendingSeekTarget(media);
			holdReasonRef.current = null;
			setState((prev) => ({
				...prev,
				isPlaying: false,
				isPaused: true,
				isEnded: true,
				isBuffering: false,
			}));
			callbacksRef.current.onEnded?.();
		};
		const handleTimeUpdate = () => {
			const currentTime = quantiseMediaTimeToSecond(media.currentTime);
			setState((prev) => (prev.currentTime === currentTime ? prev : {...prev, currentTime}));
			callbacksRef.current.onTimeUpdate?.(media.currentTime);
		};
		const publishDuration = () => {
			const duration = Number.isFinite(media.duration) ? media.duration : 0;
			setState((prev) => (prev.duration === duration ? prev : {...prev, duration}));
		};
		const handleLoadedMetadata = () => {
			publishDuration();
			callbacksRef.current.onLoadedMetadata?.(media.duration, media);
		};
		const handleDurationChange = () => {
			publishDuration();
		};
		const handleSeeking = () => {
			setState((prev) => (prev.isSeeking ? prev : {...prev, isSeeking: true}));
		};
		const handleWaiting = () => {
			if (!media.paused) {
				recoverPlaybackWhenReadyRef.current = true;
			}
			setState((prev) => (prev.isBuffering ? prev : {...prev, isBuffering: true}));
		};
		const handleReadyToPlay = () => {
			setState((prev) =>
				prev.isBuffering || prev.failureCode === 'stalled'
					? {...prev, isBuffering: false, failureCode: prev.failureCode === 'stalled' ? null : prev.failureCode}
					: prev,
			);
			if (!recoverPlaybackWhenReadyRef.current) return;
			recoverPlaybackWhenReadyRef.current = false;
			if (holdReasonRef.current !== null) return;
			if (!media.paused) return;
			void play();
		};
		const reportFailure = (code: MediaFailureCode) => {
			recoverPlaybackWhenReadyRef.current = false;
			const failure = new Error(`Media playback ${code}`);
			setState((prev) => ({...prev, isBuffering: false, failureCode: code}));
			callbacksRef.current.onError?.(failure, code);
		};
		const handleStalled = () => {
			const failure = new Error('Media playback stalled');
			setState((prev) => ({...prev, isBuffering: true, failureCode: 'stalled'}));
			callbacksRef.current.onError?.(failure, 'stalled');
		};
		const handleAbort = () => reportFailure('abort');
		const handleEmptied = () => reportFailure('emptied');
		const handleSeeked = () => {
			clearPendingSeekTarget(media);
			setState((prev) => (prev.isSeeking ? {...prev, isSeeking: false} : prev));
		};
		const handleRateChange = () => {
			setState((prev) =>
				prev.playbackRate === media.playbackRate ? prev : {...prev, playbackRate: media.playbackRate},
			);
		};
		const handleResourceLoadStart = () => {
			clearPendingSeekTarget(media);
			holdReasonRef.current = null;
			recoverPlaybackWhenReadyRef.current = false;
			setState((prev) => ({
				...prev,
				isPlaying: false,
				isPaused: true,
				isEnded: false,
				isBuffering: false,
				isSeeking: false,
				currentTime: 0,
				duration: 0,
				error: null,
				failureCode: null,
			}));
		};
		const handleError = () => {
			const error = media.error;
			const errorMessage = error
				? new Error(error.message || 'Media playback error')
				: new Error('Unknown media error');
			recoverPlaybackWhenReadyRef.current = false;
			setState((prev) => ({...prev, isBuffering: false, error: errorMessage, failureCode: 'error'}));
			callbacksRef.current.onError?.(errorMessage, 'error');
		};
		media.addEventListener('play', handlePlay);
		media.addEventListener('pause', handlePause);
		media.addEventListener('ended', handleEnded);
		media.addEventListener('timeupdate', handleTimeUpdate);
		media.addEventListener('loadedmetadata', handleLoadedMetadata);
		media.addEventListener('durationchange', handleDurationChange);
		media.addEventListener('waiting', handleWaiting);
		media.addEventListener('canplay', handleReadyToPlay);
		media.addEventListener('canplaythrough', handleReadyToPlay);
		media.addEventListener('stalled', handleStalled);
		media.addEventListener('abort', handleAbort);
		media.addEventListener('emptied', handleEmptied);
		media.addEventListener('seeking', handleSeeking);
		media.addEventListener('seeked', handleSeeked);
		media.addEventListener('ratechange', handleRateChange);
		media.addEventListener('error', handleError);
		media.addEventListener('loadstart', handleResourceLoadStart);
		if (media.readyState >= 1) {
			handleLoadedMetadata();
		}
		if (!media.paused) {
			setState((prev) => (prev.isPlaying ? prev : {...prev, isPlaying: true, isPaused: false, isEnded: false}));
		}
		return () => {
			media.removeEventListener('play', handlePlay);
			media.removeEventListener('pause', handlePause);
			media.removeEventListener('ended', handleEnded);
			media.removeEventListener('timeupdate', handleTimeUpdate);
			media.removeEventListener('loadedmetadata', handleLoadedMetadata);
			media.removeEventListener('durationchange', handleDurationChange);
			media.removeEventListener('waiting', handleWaiting);
			media.removeEventListener('canplay', handleReadyToPlay);
			media.removeEventListener('canplaythrough', handleReadyToPlay);
			media.removeEventListener('stalled', handleStalled);
			media.removeEventListener('abort', handleAbort);
			media.removeEventListener('emptied', handleEmptied);
			media.removeEventListener('seeking', handleSeeking);
			media.removeEventListener('seeked', handleSeeked);
			media.removeEventListener('ratechange', handleRateChange);
			media.removeEventListener('error', handleError);
			media.removeEventListener('loadstart', handleResourceLoadStart);
		};
	}, []);
	useEffect(() => {
		const media = mediaRef.current;
		if (!media || !autoPlay) return;
		let attempted = false;
		const attemptAutoplay = () => {
			if (attempted) return;
			attempted = true;
			const started = media.play();
			if (started == null) return;
			started.catch((error) => {
				if (isAutoplayBlockedError(error) || isAbortError(error)) {
					logger.debug('Autoplay was refused by the browser; leaving the video paused:', error);
					return;
				}
				logger.error('Autoplay failed:', normalizeError(error));
			});
		};
		const attemptAutoplayOnSource = () => {
			attempted = false;
			attemptAutoplay();
		};
		attemptAutoplay();
		media.addEventListener('loadstart', attemptAutoplayOnSource);
		return () => {
			media.removeEventListener('loadstart', attemptAutoplayOnSource);
		};
	}, [autoPlay]);
	useLayoutEffect(() => {
		const media = mediaRef.current;
		return () => {
			detachMediaElementSource(media ?? mediaRef.current);
		};
	}, []);
	const play = useCallback(async () => {
		const media = mediaRef.current;
		if (!media) return;
		try {
			await media.play();
			setState((prev) => (prev.error === null ? prev : {...prev, error: null}));
		} catch (error) {
			if (isAbortError(error)) {
				logger.debug('Play interrupted before it could start:', error);
				return;
			}
			if (isAutoplayBlockedError(error)) {
				logger.debug('Play was refused by the browser:', error);
				return;
			}
			const normalizedError = normalizeError(error);
			logger.error('Play failed:', normalizedError);
			setState((prev) => ({...prev, error: normalizedError}));
		}
	}, []);
	const pause = useCallback((reason: PlaybackHoldReason = 'user') => {
		const media = mediaRef.current;
		if (!media) return;
		holdReasonRef.current = reason;
		media.pause();
	}, []);
	const resumeHold = useCallback(
		(reason: PlaybackHoldReason) => {
			if (holdReasonRef.current !== reason) return false;
			holdReasonRef.current = null;
			void play();
			return true;
		},
		[play],
	);
	const releaseHold = useCallback((reason: PlaybackHoldReason) => {
		if (holdReasonRef.current !== reason) return false;
		holdReasonRef.current = null;
		return true;
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
		const clampedTime = clampMediaTime(time, media.duration);
		armPendingSeekTarget(media, clampedTime);
		media.currentTime = clampedTime;
	}, []);
	const seekRelative = useCallback(
		(delta: number) => {
			const media = mediaRef.current;
			if (!media) return;
			seek((readPendingSeekTarget(media) ?? media.currentTime) + delta);
		},
		[seek],
	);
	const seekPercentage = useCallback((percentage: number) => {
		const media = mediaRef.current;
		if (!media || !Number.isFinite(media.duration)) return;
		const clampedPercentage = Math.max(0, Math.min(100, percentage));
		const time = (clampedPercentage / 100) * media.duration;
		armPendingSeekTarget(media, time);
		media.currentTime = time;
	}, []);
	const setPlaybackRate = useCallback(
		(rate: number) => {
			const media = mediaRef.current;
			if (!media) return;
			const clampedRate = Math.max(0.25, Math.min(4, rate));
			media.defaultPlaybackRate = clampedRate;
			media.playbackRate = clampedRate;
			if (persistPlaybackRate) {
				storePlaybackRate(mediaKind, clampedRate);
			}
		},
		[mediaKind, persistPlaybackRate],
	);
	return {
		mediaRef,
		state,
		play,
		pause,
		resumeHold,
		releaseHold,
		toggle,
		seek,
		seekRelative,
		seekPercentage,
		setPlaybackRate,
	};
}
