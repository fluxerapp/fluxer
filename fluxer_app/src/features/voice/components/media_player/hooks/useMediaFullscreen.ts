// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {
	exitFullscreenAPI,
	getFullscreenElement,
	requestFullscreen,
	supportsFullscreenAPI,
} from '@app/features/platform/utils/FullscreenMediaUtils';
import type {ExtendedHTMLVideoElement} from '@app/types/browser.d';
import {useCallback, useEffect, useRef, useState} from 'react';

const logger = new Logger('useMediaFullscreen');

export interface UseMediaFullscreenOptions {
	containerRef: React.RefObject<HTMLElement | null> | React.RefObject<HTMLElement>;
	videoRef?: React.RefObject<HTMLVideoElement | null>;
}

export interface UseMediaFullscreenReturn {
	isFullscreen: boolean;
	supportsFullscreen: boolean;
	enterFullscreen: () => Promise<void>;
	exitFullscreen: () => Promise<void>;
	toggleFullscreen: () => Promise<void>;
}

function supportsIOSVideoFullscreen(videoElement: HTMLVideoElement | null): boolean {
	if (!videoElement) return false;
	const extendedVideo = videoElement as ExtendedHTMLVideoElement;
	return !!(extendedVideo.webkitSupportsFullscreen || extendedVideo.webkitEnterFullscreen);
}

export function useMediaFullscreen(options: UseMediaFullscreenOptions): UseMediaFullscreenReturn {
	const {containerRef, videoRef} = options;
	const [isFullscreen, setIsFullscreen] = useState(false);
	const [supportsFullscreen, setSupportsFullscreen] = useState(() => supportsFullscreenAPI());
	const [useIOSFullscreen, setUseIOSFullscreen] = useState(false);
	const claimedFullscreenHostRef = useRef<Element | null>(null);
	useEffect(() => {
		const hasContainerSupport = supportsFullscreenAPI();
		const hasIOSSupport = supportsIOSVideoFullscreen(videoRef?.current ?? null);
		setSupportsFullscreen(hasContainerSupport || hasIOSSupport);
		setUseIOSFullscreen(!hasContainerSupport && hasIOSSupport);
	}, [videoRef]);
	useEffect(() => {
		const handleFullscreenChange = () => {
			const fullscreenElement = getFullscreenElement();
			const isNowFullscreen = fullscreenElement === containerRef.current;
			claimedFullscreenHostRef.current = isNowFullscreen ? fullscreenElement : null;
			setIsFullscreen(isNowFullscreen);
		};
		const handleIOSFullscreenChange = () => {
			const video = videoRef?.current as ExtendedHTMLVideoElement | null;
			if (video) {
				setIsFullscreen(video.webkitDisplayingFullscreen ?? false);
			}
		};
		document.addEventListener('fullscreenchange', handleFullscreenChange);
		document.addEventListener('webkitfullscreenchange', handleFullscreenChange);
		document.addEventListener('mozfullscreenchange', handleFullscreenChange);
		document.addEventListener('MSFullscreenChange', handleFullscreenChange);
		const video = videoRef?.current;
		if (video) {
			video.addEventListener('webkitbeginfullscreen', handleIOSFullscreenChange);
			video.addEventListener('webkitendfullscreen', handleIOSFullscreenChange);
		}
		return () => {
			document.removeEventListener('fullscreenchange', handleFullscreenChange);
			document.removeEventListener('webkitfullscreenchange', handleFullscreenChange);
			document.removeEventListener('mozfullscreenchange', handleFullscreenChange);
			document.removeEventListener('MSFullscreenChange', handleFullscreenChange);
			if (video) {
				video.removeEventListener('webkitbeginfullscreen', handleIOSFullscreenChange);
				video.removeEventListener('webkitendfullscreen', handleIOSFullscreenChange);
			}
		};
	}, [containerRef, videoRef]);
	useEffect(() => {
		return () => {
			const claimedHost = claimedFullscreenHostRef.current;
			claimedFullscreenHostRef.current = null;
			if (claimedHost === null || getFullscreenElement() !== claimedHost) return;
			exitFullscreenAPI().catch((error: unknown) => {
				logger.error('Failed to release fullscreen on teardown:', error);
			});
		};
	}, []);
	const enterFullscreen = useCallback(async () => {
		if (useIOSFullscreen && videoRef?.current) {
			try {
				const video = videoRef.current as ExtendedHTMLVideoElement;
				if (video.webkitEnterFullscreen) {
					await video.webkitEnterFullscreen();
					return;
				}
			} catch (error) {
				logger.error('Failed to enter iOS fullscreen:', error);
			}
		}
		const container = containerRef.current;
		if (!container || !supportsFullscreenAPI()) return;
		try {
			await requestFullscreen(container);
		} catch (error) {
			logger.error('Failed to enter fullscreen:', error);
		}
	}, [containerRef, videoRef, useIOSFullscreen]);
	const exitFullscreen = useCallback(async () => {
		if (useIOSFullscreen && videoRef?.current) {
			try {
				const video = videoRef.current as ExtendedHTMLVideoElement;
				if (video.webkitExitFullscreen && video.webkitDisplayingFullscreen) {
					await video.webkitExitFullscreen();
					return;
				}
			} catch (error) {
				logger.error('Failed to exit iOS fullscreen:', error);
			}
		}
		if (!getFullscreenElement()) return;
		try {
			await exitFullscreenAPI();
		} catch (error) {
			logger.error('Failed to exit fullscreen:', error);
		}
	}, [videoRef, useIOSFullscreen]);
	const toggleFullscreen = useCallback(async () => {
		if (isFullscreen) {
			await exitFullscreen();
		} else {
			await enterFullscreen();
		}
	}, [isFullscreen, enterFullscreen, exitFullscreen]);
	return {
		isFullscreen,
		supportsFullscreen,
		enterFullscreen,
		exitFullscreen,
		toggleFullscreen,
	};
}
