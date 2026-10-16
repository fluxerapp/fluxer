// SPDX-License-Identifier: AGPL-3.0-or-later

import {safePause, safePlay} from '@app/features/channel/components/GifVideoPool';
import {useEffect, useRef} from 'react';

const HAVE_METADATA_READY_STATE = 1;

interface GifVideoPoolLike {
	takeElement: () => HTMLVideoElement;
	registerActive: (video: HTMLVideoElement) => void;
	unregisterActive: (video: HTMLVideoElement) => void;
	returnElement: (video: HTMLVideoElement) => void;
	isGloballyPaused?: () => boolean;
}

export function usePooledVideo({
	src,
	containerRef,
	videoPool,
	autoPlay,
	enabled = true,
	preload,
	playbackStartTime = null,
}: {
	src: string | null | undefined;
	containerRef: React.RefObject<HTMLDivElement | null>;
	videoPool: GifVideoPoolLike;
	autoPlay: boolean;
	enabled?: boolean;
	preload?: HTMLVideoElement['preload'];
	playbackStartTime?: number | null;
}) {
	const videoRef = useRef<HTMLVideoElement | null>(null);
	const effectivePreload = preload ?? (autoPlay ? 'auto' : 'metadata');
	useEffect(() => {
		if (!enabled) return;
		if (!src) return;
		const container = containerRef.current;
		if (!container) return;
		const video = videoPool.takeElement();
		videoRef.current = video;
		video.autoplay = false;
		video.preload = effectivePreload;
		video.src = src;
		container.appendChild(video);
		videoPool.registerActive(video);
		return () => {
			videoPool.unregisterActive(video);
			videoPool.returnElement(video);
			if (videoRef.current === video) {
				videoRef.current = null;
			}
		};
	}, [src, enabled, containerRef, videoPool, effectivePreload]);
	useEffect(() => {
		const video = videoRef.current;
		if (!video) return;
		const globallyPaused = videoPool.isGloballyPaused?.() ?? false;
		video.autoplay = autoPlay && playbackStartTime === null && !globallyPaused;
		if (globallyPaused) return;
		if (!autoPlay) {
			safePause(video);
			return;
		}
		const seekTarget = playbackStartTime !== null && playbackStartTime > 0 ? playbackStartTime : null;
		const playFromStartTime = () => {
			if (seekTarget !== null) {
				const duration = Number.isFinite(video.duration) && video.duration > 0 ? video.duration : null;
				const target = duration === null ? seekTarget : Math.min(seekTarget, Math.max(0, duration - 0.05));
				try {
					video.currentTime = target;
				} catch {}
			}
			void safePlay(video);
		};
		if (seekTarget !== null && video.readyState < HAVE_METADATA_READY_STATE) {
			video.addEventListener('loadedmetadata', playFromStartTime, {once: true});
			return () => {
				video.removeEventListener('loadedmetadata', playFromStartTime);
			};
		}
		playFromStartTime();
		return;
	}, [src, enabled, containerRef, videoPool, effectivePreload, autoPlay, playbackStartTime]);
	return videoRef;
}
