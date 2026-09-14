// SPDX-License-Identifier: AGPL-3.0-or-later

import type React from 'react';
import {useEffect, useState} from 'react';

const VIDEO_FRAME_POLL_INTERVAL_MS = 125;
const MAX_SHARED_VIDEO_FRAME_WATCHERS = 512;
const HTML_MEDIA_HAVE_CURRENT_DATA = 2;

type VideoFrameCallbackHandle = number;
type VideoElementRef = React.RefObject<HTMLVideoElement | null>;

interface SharedVideoFramePollTarget {
	videoRef: VideoElementRef;
	onFrame: () => void;
}

export interface VideoElementRenderedFrameWatchOptions {
	videoRef: VideoElementRef;
	onFrame: () => void;
}

const sharedVideoFramePollTargets = new Map<number, SharedVideoFramePollTarget>();
let nextSharedVideoFramePollTargetId = 1;
let sharedVideoFramePollIntervalId: number | null = null;

export interface UseVideoRenderedFrameOptions {
	enabled: boolean;
	resetKey: string;
	videoRef: React.RefObject<HTMLVideoElement | null>;
}

export function videoElementHasRenderedFrame(video: HTMLVideoElement | null): boolean {
	if (!video) return false;
	return video.readyState >= HTML_MEDIA_HAVE_CURRENT_DATA && video.videoWidth > 0 && video.videoHeight > 0;
}

function stopSharedVideoFramePollIntervalIfIdle(): void {
	if (sharedVideoFramePollTargets.size > 0) return;
	if (sharedVideoFramePollIntervalId === null) return;
	window.clearInterval(sharedVideoFramePollIntervalId);
	sharedVideoFramePollIntervalId = null;
}

function removeSharedVideoFramePollTarget(id: number): void {
	sharedVideoFramePollTargets.delete(id);
	stopSharedVideoFramePollIntervalIfIdle();
}

function sweepSharedVideoFramePollTargets(): void {
	for (const [id, target] of sharedVideoFramePollTargets) {
		if (!videoElementHasRenderedFrame(target.videoRef.current)) continue;
		removeSharedVideoFramePollTarget(id);
		target.onFrame();
	}
}

function ensureSharedVideoFramePollInterval(): void {
	if (sharedVideoFramePollIntervalId !== null) return;
	sharedVideoFramePollIntervalId = window.setInterval(sweepSharedVideoFramePollTargets, VIDEO_FRAME_POLL_INTERVAL_MS);
}

function addSharedVideoFramePollTarget(target: SharedVideoFramePollTarget): () => void {
	if (sharedVideoFramePollTargets.size >= MAX_SHARED_VIDEO_FRAME_WATCHERS) {
		throw new Error('Shared video frame watcher limit exceeded');
	}
	const id = nextSharedVideoFramePollTargetId;
	nextSharedVideoFramePollTargetId += 1;
	sharedVideoFramePollTargets.set(id, target);
	ensureSharedVideoFramePollInterval();
	return () => removeSharedVideoFramePollTarget(id);
}

export function getActiveVideoFrameWatcherCountForTests(): number {
	return sharedVideoFramePollTargets.size;
}

export function clearVideoFrameWatchersForTests(): void {
	sharedVideoFramePollTargets.clear();
	stopSharedVideoFramePollIntervalIfIdle();
}

export function watchVideoElementRenderedFrame({videoRef, onFrame}: VideoElementRenderedFrameWatchOptions): () => void {
	let disposed = false;
	let frameCallbackHandle: VideoFrameCallbackHandle | null = null;
	let removeSharedPollTarget: (() => void) | null = null;
	const video = videoRef.current;

	function clearWatchers(): void {
		if (removeSharedPollTarget !== null) {
			removeSharedPollTarget();
			removeSharedPollTarget = null;
		}
		if (frameCallbackHandle !== null) {
			video?.cancelVideoFrameCallback?.(frameCallbackHandle);
			frameCallbackHandle = null;
		}
		video?.removeEventListener('loadeddata', markReadyIfPossible);
		video?.removeEventListener('playing', markReadyIfPossible);
		video?.removeEventListener('resize', markReadyIfPossible);
	}

	function markPresentedFrame(): void {
		if (disposed) return;
		disposed = true;
		clearWatchers();
		onFrame();
	}

	function markReadyIfPossible(): void {
		if (disposed) return;
		if (!videoElementHasRenderedFrame(videoRef.current)) return;
		markPresentedFrame();
	}

	if (videoElementHasRenderedFrame(videoRef.current)) {
		onFrame();
		return () => {};
	}
	removeSharedPollTarget = addSharedVideoFramePollTarget({videoRef, onFrame: markReadyIfPossible});
	video?.addEventListener('loadeddata', markReadyIfPossible);
	video?.addEventListener('playing', markReadyIfPossible);
	video?.addEventListener('resize', markReadyIfPossible);
	if (video?.requestVideoFrameCallback) {
		frameCallbackHandle = video.requestVideoFrameCallback(() => {
			frameCallbackHandle = null;
			markPresentedFrame();
		});
	}
	markReadyIfPossible();
	return () => {
		disposed = true;
		clearWatchers();
	};
}

export function useVideoRenderedFrame({enabled, resetKey, videoRef}: UseVideoRenderedFrameOptions): boolean {
	const [hasRenderedFrame, setHasRenderedFrame] = useState(false);

	useEffect(() => {
		setHasRenderedFrame(false);
	}, [resetKey]);

	useEffect(() => {
		if (!enabled) {
			setHasRenderedFrame(false);
			return;
		}

		return watchVideoElementRenderedFrame({
			videoRef,
			onFrame: () => {
				setHasRenderedFrame(true);
			},
		});
	}, [enabled, resetKey, videoRef]);

	return enabled && hasRenderedFrame;
}
