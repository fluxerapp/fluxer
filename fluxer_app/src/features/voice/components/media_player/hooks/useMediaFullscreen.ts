// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	getExtendedDocument,
	supportsMozRequestFullScreen,
	supportsMsRequestFullscreen,
	supportsWebkitRequestFullscreen,
} from '@app/features/platform/types/Browser';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {getFullscreenElement} from '@app/features/platform/utils/FullscreenMediaUtils';
import type {ExtendedHTMLElement, ExtendedHTMLVideoElement} from '@app/types/browser.d';
import {useCallback, useEffect, useRef, useState} from 'react';

const logger = new Logger('useMediaFullscreen');

export interface UseMediaFullscreenOptions {
	containerRef: React.RefObject<HTMLElement | null> | React.RefObject<HTMLElement>;
	videoRef?: React.RefObject<HTMLVideoElement | null>;
	onFullscreenChange?: (isFullscreen: boolean) => void;
}

export interface UseMediaFullscreenReturn {
	isFullscreen: boolean;
	supportsFullscreen: boolean;
	enterFullscreen: () => Promise<void>;
	exitFullscreen: () => Promise<void>;
	toggleFullscreen: () => Promise<void>;
}

function supportsContainerFullscreenAPI(): boolean {
	const doc = getExtendedDocument();
	return !!(
		document.fullscreenEnabled ||
		doc.webkitFullscreenEnabled ||
		doc.mozFullScreenEnabled ||
		doc.msFullscreenEnabled
	);
}

function supportsIOSVideoFullscreen(videoElement: HTMLVideoElement | null): boolean {
	if (!videoElement) return false;
	const extendedVideo = videoElement as ExtendedHTMLVideoElement;
	return !!(extendedVideo.webkitSupportsFullscreen || extendedVideo.webkitEnterFullscreen);
}

async function requestFullscreen(element: HTMLElement): Promise<void> {
	if (element.requestFullscreen) {
		await element.requestFullscreen();
	} else if (supportsWebkitRequestFullscreen(element)) {
		const extendedElement = element as ExtendedHTMLElement;
		await extendedElement.webkitRequestFullscreen!();
	} else if (supportsMozRequestFullScreen(element)) {
		const extendedElement = element as ExtendedHTMLElement;
		await extendedElement.mozRequestFullScreen!();
	} else if (supportsMsRequestFullscreen(element)) {
		const extendedElement = element as ExtendedHTMLElement;
		await extendedElement.msRequestFullscreen!();
	}
}

async function exitFullscreenAPI(): Promise<void> {
	const doc = getExtendedDocument();
	if (document.exitFullscreen) {
		await document.exitFullscreen();
	} else if (doc.webkitExitFullscreen) {
		await doc.webkitExitFullscreen();
	} else if (doc.mozCancelFullScreen) {
		await doc.mozCancelFullScreen();
	} else if (doc.msExitFullscreen) {
		await doc.msExitFullscreen();
	}
}

export function useMediaFullscreen(options: UseMediaFullscreenOptions): UseMediaFullscreenReturn {
	const {containerRef, videoRef, onFullscreenChange} = options;
	const [isFullscreen, setIsFullscreen] = useState(false);
	const [supportsFullscreen, setSupportsFullscreen] = useState(() => supportsContainerFullscreenAPI());
	const [useIOSFullscreen, setUseIOSFullscreen] = useState(false);
	const claimedFullscreenHostRef = useRef<Element | null>(null);
	useEffect(() => {
		const hasContainerSupport = supportsContainerFullscreenAPI();
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
			onFullscreenChange?.(isNowFullscreen);
		};
		const handleIOSFullscreenChange = () => {
			const video = videoRef?.current as ExtendedHTMLVideoElement | null;
			if (video) {
				const isNowFullscreen = video.webkitDisplayingFullscreen ?? false;
				setIsFullscreen(isNowFullscreen);
				onFullscreenChange?.(isNowFullscreen);
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
	}, [containerRef, videoRef, onFullscreenChange]);
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
		if (!container || !supportsContainerFullscreenAPI()) return;
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
