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

export interface UseMediaProgressOptions {
	mediaRef: React.RefObject<HTMLMediaElement | null>;
	initialDuration?: number;
	updateInterval?: number;
	useRAF?: boolean;
}

export interface UseMediaProgressReturn {
	currentTime: number;
	duration: number;
	progress: number;
	buffered: number;
	isSeeking: boolean;
	seekToPercentage: (percentage: number) => void;
	seekToTime: (time: number) => void;
	startSeeking: () => void;
	endSeeking: () => void;
}

function getBufferedPercentage(media: HTMLMediaElement): number {
	if (!media.buffered.length || !Number.isFinite(media.duration) || media.duration === 0) {
		return 0;
	}

	const currentTime = media.currentTime;
	let bufferedEnd = 0;

	for (let i = 0; i < media.buffered.length; i++) {
		const start = media.buffered.start(i);
		const end = media.buffered.end(i);

		if (currentTime >= start && currentTime <= end) {
			bufferedEnd = end;
			break;
		}

		if (end > bufferedEnd) {
			bufferedEnd = end;
		}
	}

	return (bufferedEnd / media.duration) * 100;
}

export function useMediaProgress(options: UseMediaProgressOptions): UseMediaProgressReturn {
	const {mediaRef, initialDuration, updateInterval = 100, useRAF = true} = options;

	const [currentTime, setCurrentTime] = useState(0);
	const [duration, setDuration] = useState(initialDuration ?? 0);
	const [buffered, setBuffered] = useState(0);
	const [isSeeking, setIsSeeking] = useState(false);

	const rafRef = useRef<number | null>(null);
	const intervalRef = useRef<number | null>(null);
	const isSeekingRef = useRef(false);

	const updateProgress = useCallback(() => {
		const media = mediaRef.current;
		if (!media || isSeekingRef.current) return;

		const newCurrentTime = media.currentTime;
		const newDuration = Number.isFinite(media.duration) ? media.duration : 0;
		const newBuffered = getBufferedPercentage(media);

		setCurrentTime(newCurrentTime);
		setDuration(newDuration);
		setBuffered(newBuffered);
	}, [mediaRef]);

	useEffect(() => {
		const media = mediaRef.current;
		if (!media) return;

		updateProgress();

		const handleLoadedMetadata = () => {
			setDuration(Number.isFinite(media.duration) ? media.duration : 0);
		};

		const handleProgress = () => {
			setBuffered(getBufferedPercentage(media));
		};

		const handleTimeUpdate = () => {
			if (!isSeekingRef.current) {
				setCurrentTime(media.currentTime);
			}
		};

		const handleSeeking = () => {
			if (!isSeekingRef.current) {
				setIsSeeking(true);
			}
		};

		const handleSeeked = () => {
			if (!isSeekingRef.current) {
				setIsSeeking(false);
			}
			setCurrentTime(media.currentTime);
		};

		media.addEventListener('loadedmetadata', handleLoadedMetadata);
		media.addEventListener('progress', handleProgress);
		media.addEventListener('timeupdate', handleTimeUpdate);
		media.addEventListener('seeking', handleSeeking);
		media.addEventListener('seeked', handleSeeked);

		if (useRAF) {
			const tick = () => {
				updateProgress();
				rafRef.current = requestAnimationFrame(tick);
			};
			rafRef.current = requestAnimationFrame(tick);
		} else {
			intervalRef.current = window.setInterval(updateProgress, updateInterval);
		}

		return () => {
			media.removeEventListener('loadedmetadata', handleLoadedMetadata);
			media.removeEventListener('progress', handleProgress);
			media.removeEventListener('timeupdate', handleTimeUpdate);
			media.removeEventListener('seeking', handleSeeking);
			media.removeEventListener('seeked', handleSeeked);

			if (rafRef.current !== null) {
				cancelAnimationFrame(rafRef.current);
				rafRef.current = null;
			}

			if (intervalRef.current !== null) {
				clearInterval(intervalRef.current);
				intervalRef.current = null;
			}
		};
	}, [mediaRef, updateProgress, updateInterval, useRAF]);

	const seekToPercentage = useCallback(
		(percentage: number) => {
			const media = mediaRef.current;
			if (!media || !Number.isFinite(media.duration)) return;

			const clampedPercentage = Math.max(0, Math.min(100, percentage));
			const time = (clampedPercentage / 100) * media.duration;
			media.currentTime = time;
			setCurrentTime(time);
		},
		[mediaRef],
	);

	const seekToTime = useCallback(
		(time: number) => {
			const media = mediaRef.current;
			if (!media) return;

			const clampedTime = Math.max(0, Math.min(time, media.duration || Infinity));
			media.currentTime = clampedTime;
			setCurrentTime(clampedTime);
		},
		[mediaRef],
	);

	const startSeeking = useCallback(() => {
		isSeekingRef.current = true;
		setIsSeeking(true);
	}, []);

	const endSeeking = useCallback(() => {
		isSeekingRef.current = false;
		setIsSeeking(false);
	}, []);

	const progress = duration > 0 ? (currentTime / duration) * 100 : 0;

	return {
		currentTime,
		duration,
		progress,
		buffered,
		isSeeking,
		seekToPercentage,
		seekToTime,
		startSeeking,
		endSeeking,
	};
}
