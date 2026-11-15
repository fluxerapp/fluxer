// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import Window from '@app/features/window/state/Window';
import {STREAM_PREVIEW_MAX_DIMENSION_PX} from '@fluxer/constants/src/StreamConstants';
import {autorun} from 'mobx';
import type React from 'react';
import {useEffect, useLayoutEffect, useRef, useState} from 'react';

const logger = new Logger('OwnStreamPreviewState');
const WINDOW_FOCUS_LOSS_DEBOUNCE_MS = 800;

interface OwnCameraFeedHiddenOptions {
	isOwnContent: boolean;
	isScreenShare: boolean;
	showMyOwnCamera: boolean;
}

export function isOwnCameraFeedHidden({
	isOwnContent,
	isScreenShare,
	showMyOwnCamera,
}: OwnCameraFeedHiddenOptions): boolean {
	return isOwnContent && !isScreenShare && !showMyOwnCamera;
}

interface OwnScreenSharePreviewStateOptions {
	isOwnScreenShare: boolean;
	pausePreviewOnUnfocus: boolean;
	isWindowFocused: boolean;
	videoRef: React.RefObject<HTMLVideoElement | null>;
}

export interface OwnScreenSharePreviewState {
	frozenFrameUrl: string | null;
	isPreviewPaused: boolean;
	isOwnStreamPreviewPaused: boolean;
	shouldHideOwnScreenShareVideo: boolean;
}

function getScaledFrameSize(width: number, height: number): {width: number; height: number} {
	if (width <= STREAM_PREVIEW_MAX_DIMENSION_PX && height <= STREAM_PREVIEW_MAX_DIMENSION_PX) {
		return {width, height};
	}
	const scale = STREAM_PREVIEW_MAX_DIMENSION_PX / Math.max(width, height);
	return {
		width: Math.max(1, Math.round(width * scale)),
		height: Math.max(1, Math.round(height * scale)),
	};
}

function canvasToBlob(canvas: HTMLCanvasElement, type: string, quality: number): Promise<Blob | null> {
	return new Promise((resolve) => {
		canvas.toBlob(resolve, type, quality);
	});
}

async function captureFrozenFrameUrl(videoEl: HTMLVideoElement): Promise<string | null> {
	if (videoEl.videoWidth === 0 || videoEl.videoHeight === 0) return null;
	const {width, height} = getScaledFrameSize(videoEl.videoWidth, videoEl.videoHeight);
	const canvas = document.createElement('canvas');
	canvas.width = width;
	canvas.height = height;
	const ctx = canvas.getContext('2d');
	if (!ctx) return null;
	ctx.drawImage(videoEl, 0, 0, width, height);
	const blob = await canvasToBlob(canvas, 'image/jpeg', 0.72);
	canvas.width = 0;
	canvas.height = 0;
	return blob ? URL.createObjectURL(blob) : null;
}

export function useOwnScreenSharePreviewState({
	isOwnScreenShare,
	pausePreviewOnUnfocus,
	isWindowFocused,
	videoRef,
}: OwnScreenSharePreviewStateOptions): OwnScreenSharePreviewState {
	const debouncedFocused = useDebouncedFocusLoss(isWindowFocused, WINDOW_FOCUS_LOSS_DEBOUNCE_MS);
	const [frozenFrameUrl, setFrozenFrameUrl] = useState<string | null>(null);
	const [isPreviewPaused, setPreviewPaused] = useState(false);
	const prevWindowFocusedRef = useRef(debouncedFocused);
	const frozenFrameUrlRef = useRef<string | null>(null);
	const setManagedFrozenFrameUrl = (url: string | null): void => {
		const previous = frozenFrameUrlRef.current;
		if (previous && previous !== url) {
			URL.revokeObjectURL(previous);
		}
		frozenFrameUrlRef.current = url;
		setFrozenFrameUrl(url);
	};
	useEffect(() => {
		return () => {
			if (frozenFrameUrlRef.current) {
				URL.revokeObjectURL(frozenFrameUrlRef.current);
				frozenFrameUrlRef.current = null;
			}
		};
	}, []);
	useLayoutEffect(() => {
		let cancelled = false;
		if (!isOwnScreenShare || !pausePreviewOnUnfocus) {
			setPreviewPaused(false);
			setManagedFrozenFrameUrl(null);
			prevWindowFocusedRef.current = debouncedFocused;
			return;
		}
		const wasFocused = prevWindowFocusedRef.current;
		prevWindowFocusedRef.current = debouncedFocused;
		if (wasFocused && !debouncedFocused) {
			const videoEl = videoRef.current;
			if (videoEl && videoEl.videoWidth > 0 && videoEl.videoHeight > 0) {
				void captureFrozenFrameUrl(videoEl)
					.then((url) => {
						if (!url) {
							logger.warn('Screen share preview frame not ready for capture');
							return;
						}
						if (cancelled) {
							URL.revokeObjectURL(url);
							return;
						}
						setManagedFrozenFrameUrl(url);
					})
					.catch((err) => {
						logger.error('Failed to capture frozen frame', err);
					});
			} else {
				logger.warn('Screen share preview frame not ready for capture');
			}
			setPreviewPaused(true);
		} else if (!wasFocused && debouncedFocused) {
			setPreviewPaused(false);
			setManagedFrozenFrameUrl(null);
		}
		return () => {
			cancelled = true;
		};
	}, [isOwnScreenShare, pausePreviewOnUnfocus, debouncedFocused, videoRef]);
	const shouldHideOwnScreenShareVideo = isOwnScreenShare && isPreviewPaused;
	const isOwnStreamPreviewPaused = shouldHideOwnScreenShareVideo;
	return {frozenFrameUrl, isPreviewPaused, isOwnStreamPreviewPaused, shouldHideOwnScreenShareVideo};
}

function useDebouncedFocusLoss(value: boolean, delayMs: number): boolean {
	const [debounced, setDebounced] = useState(value);
	useEffect(() => {
		if (value) {
			setDebounced(true);
			return;
		}
		const timeoutId = window.setTimeout(() => {
			setDebounced(false);
		}, delayMs);
		return () => {
			window.clearTimeout(timeoutId);
		};
	}, [value, delayMs]);
	return debounced;
}

export function useWindowFocus(): boolean {
	const [isFocused, setIsFocused] = useState(() => Window.isFocused());
	useEffect(() => {
		const disposer = autorun(() => {
			setIsFocused(Window.isFocused());
		});
		return () => disposer();
	}, []);
	return isFocused;
}
