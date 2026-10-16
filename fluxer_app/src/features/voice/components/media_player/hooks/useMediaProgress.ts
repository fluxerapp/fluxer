// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	areBufferedSpanFractionsEqual,
	armPendingSeekTarget,
	type BufferedSpanFraction,
	clampMediaTime,
	clampPercentage,
	clearPendingSeekTarget,
	collectBufferedSpanFractions,
	EMPTY_BUFFERED_SPANS,
	getEffectiveMediaDuration,
	readPendingSeekTarget,
} from '@app/features/voice/components/media_player/utils/MediaSeekUtils';
import {useCallback, useEffect, useRef, useState} from 'react';

const SEEK_SETTLED_TOLERANCE_SECONDS = 0.5;

interface UseMediaProgressOptions {
	mediaRef: React.RefObject<HTMLMediaElement | null>;
	initialDuration?: number;
}

export interface UseMediaProgressReturn {
	currentTime: number;
	duration: number;
	progress: number;
	buffered: ReadonlyArray<BufferedSpanFraction>;
	isSeeking: boolean;
	previewSeekToPercentage: (percentage: number) => void;
	seekToPercentage: (percentage: number) => void;
	seekToTime: (time: number) => void;
	startSeeking: () => void;
	endSeeking: () => void;
}

export function useMediaProgress(options: UseMediaProgressOptions): UseMediaProgressReturn {
	const {mediaRef, initialDuration} = options;
	const [currentTime, setCurrentTime] = useState(0);
	const [duration, setDuration] = useState(initialDuration ?? 0);
	const [buffered, setBuffered] = useState<ReadonlyArray<BufferedSpanFraction>>(EMPTY_BUFFERED_SPANS);
	const [isSeeking, setIsSeeking] = useState(false);
	const [pendingProgress, setPendingProgress] = useState<number | null>(null);
	const isDraggingRef = useRef(false);
	const deferredSeekPercentageRef = useRef<number | null>(null);
	const fallbackDurationRef = useRef(initialDuration ?? 0);
	useEffect(() => {
		fallbackDurationRef.current = initialDuration ?? 0;
	}, [initialDuration]);
	const setCurrentTimeIfChanged = useCallback((nextCurrentTime: number) => {
		setCurrentTime((previousCurrentTime) =>
			previousCurrentTime === nextCurrentTime ? previousCurrentTime : nextCurrentTime,
		);
	}, []);
	const setBufferedIfChanged = useCallback((nextBuffered: ReadonlyArray<BufferedSpanFraction>) => {
		setBuffered((previousBuffered) =>
			areBufferedSpanFractionsEqual(previousBuffered, nextBuffered) ? previousBuffered : nextBuffered,
		);
	}, []);
	const setPendingProgressIfChanged = useCallback((nextProgress: number | null) => {
		setPendingProgress((previousProgress) => (previousProgress === nextProgress ? previousProgress : nextProgress));
	}, []);
	const setDurationFromMedia = useCallback((rawDuration: number) => {
		const hasRealDuration = Number.isFinite(rawDuration) && rawDuration > 0;
		setDuration((previousDuration) => {
			const nextDuration = hasRealDuration
				? rawDuration
				: previousDuration > 0
					? previousDuration
					: fallbackDurationRef.current;
			return previousDuration === nextDuration ? previousDuration : nextDuration;
		});
	}, []);
	useEffect(() => {
		const media = mediaRef.current;
		if (!media) return;
		const applyDeferredSeek = () => {
			const deferredPercentage = deferredSeekPercentageRef.current;
			if (deferredPercentage === null) return;
			const effectiveDuration = getEffectiveMediaDuration(media, 0);
			if (effectiveDuration <= 0) return;
			deferredSeekPercentageRef.current = null;
			const time = (deferredPercentage / 100) * effectiveDuration;
			armPendingSeekTarget(media, time);
			media.currentTime = time;
			setCurrentTimeIfChanged(time);
		};
		const handleLoadedMetadata = () => {
			setDurationFromMedia(media.duration);
			applyDeferredSeek();
		};
		const handleDurationChange = () => {
			setDurationFromMedia(media.duration);
		};
		const handleTimeUpdate = () => {
			const seekTarget = readPendingSeekTarget(media);
			if (seekTarget !== null && Math.abs(media.currentTime - seekTarget) <= SEEK_SETTLED_TOLERANCE_SECONDS) {
				clearPendingSeekTarget(media);
			}
			if (
				isDraggingRef.current ||
				readPendingSeekTarget(media) !== null ||
				deferredSeekPercentageRef.current !== null
			) {
				return;
			}
			setCurrentTimeIfChanged(media.currentTime);
			setPendingProgressIfChanged(null);
		};
		const handleProgress = () => {
			setBufferedIfChanged(collectBufferedSpanFractions(media));
		};
		const handleSeeking = () => {
			if (!isDraggingRef.current) {
				setIsSeeking(true);
			}
		};
		const handleSeeked = () => {
			clearPendingSeekTarget(media);
			setCurrentTimeIfChanged(media.currentTime);
			if (isDraggingRef.current) return;
			setIsSeeking(false);
			setPendingProgressIfChanged(null);
		};
		const handleEnded = () => {
			clearPendingSeekTarget(media);
			setCurrentTimeIfChanged(media.currentTime);
		};
		const handleEmptied = () => {
			clearPendingSeekTarget(media);
			deferredSeekPercentageRef.current = null;
			setPendingProgressIfChanged(null);
			setBufferedIfChanged(EMPTY_BUFFERED_SPANS);
		};
		media.addEventListener('loadedmetadata', handleLoadedMetadata);
		media.addEventListener('durationchange', handleDurationChange);
		media.addEventListener('timeupdate', handleTimeUpdate);
		media.addEventListener('progress', handleProgress);
		media.addEventListener('seeking', handleSeeking);
		media.addEventListener('seeked', handleSeeked);
		media.addEventListener('ended', handleEnded);
		media.addEventListener('emptied', handleEmptied);
		setDurationFromMedia(media.duration);
		if (media.readyState >= 1) {
			setCurrentTimeIfChanged(media.currentTime);
			setBufferedIfChanged(collectBufferedSpanFractions(media));
		}
		return () => {
			media.removeEventListener('loadedmetadata', handleLoadedMetadata);
			media.removeEventListener('durationchange', handleDurationChange);
			media.removeEventListener('timeupdate', handleTimeUpdate);
			media.removeEventListener('progress', handleProgress);
			media.removeEventListener('seeking', handleSeeking);
			media.removeEventListener('seeked', handleSeeked);
			media.removeEventListener('ended', handleEnded);
			media.removeEventListener('emptied', handleEmptied);
		};
	}, [mediaRef, setBufferedIfChanged, setCurrentTimeIfChanged, setDurationFromMedia, setPendingProgressIfChanged]);
	const previewSeekToPercentage = useCallback(
		(percentage: number) => {
			const clampedPercentage = clampPercentage(percentage);
			setPendingProgressIfChanged(clampedPercentage);
			const effectiveDuration = getEffectiveMediaDuration(mediaRef.current, fallbackDurationRef.current);
			if (effectiveDuration > 0) {
				setCurrentTimeIfChanged((clampedPercentage / 100) * effectiveDuration);
			}
		},
		[mediaRef, setCurrentTimeIfChanged, setPendingProgressIfChanged],
	);
	const seekToPercentage = useCallback(
		(percentage: number) => {
			const media = mediaRef.current;
			const clampedPercentage = clampPercentage(percentage);
			const effectiveDuration = getEffectiveMediaDuration(media, 0);
			if (media && effectiveDuration > 0) {
				const time = (clampedPercentage / 100) * effectiveDuration;
				deferredSeekPercentageRef.current = null;
				armPendingSeekTarget(media, time);
				media.currentTime = time;
				setCurrentTimeIfChanged(time);
				setPendingProgressIfChanged(clampedPercentage);
				return;
			}
			deferredSeekPercentageRef.current = clampedPercentage;
			setPendingProgressIfChanged(clampedPercentage);
			const fallbackDuration = fallbackDurationRef.current;
			if (fallbackDuration > 0) {
				setCurrentTimeIfChanged((clampedPercentage / 100) * fallbackDuration);
			}
		},
		[mediaRef, setCurrentTimeIfChanged, setPendingProgressIfChanged],
	);
	const seekToTime = useCallback(
		(time: number) => {
			const media = mediaRef.current;
			if (!media) return;
			const clampedTime = clampMediaTime(time, media.duration);
			armPendingSeekTarget(media, clampedTime);
			media.currentTime = clampedTime;
			setCurrentTimeIfChanged(clampedTime);
			const effectiveDuration = getEffectiveMediaDuration(media, fallbackDurationRef.current);
			if (effectiveDuration > 0) {
				setPendingProgressIfChanged(clampPercentage((clampedTime / effectiveDuration) * 100));
			}
		},
		[mediaRef, setCurrentTimeIfChanged, setPendingProgressIfChanged],
	);
	const startSeeking = useCallback(() => {
		isDraggingRef.current = true;
		setIsSeeking(true);
	}, []);
	const endSeeking = useCallback(() => {
		isDraggingRef.current = false;
		setIsSeeking(false);
		if (readPendingSeekTarget(mediaRef.current) === null) {
			setPendingProgressIfChanged(null);
		}
	}, [mediaRef, setPendingProgressIfChanged]);
	const progress = pendingProgress !== null ? pendingProgress : duration > 0 ? (currentTime / duration) * 100 : 0;
	return {
		currentTime,
		duration,
		progress,
		buffered,
		isSeeking,
		previewSeekToPercentage,
		seekToPercentage,
		seekToTime,
		startSeeking,
		endSeeking,
	};
}
