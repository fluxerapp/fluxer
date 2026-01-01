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

import {useCallback, useEffect, useState} from 'react';
import type {ExtendedHTMLElement, ExtendedHTMLVideoElement} from '~/types/browser';
import {
	getExtendedDocument,
	supportsMozRequestFullScreen,
	supportsMsRequestFullscreen,
	supportsWebkitRequestFullscreen,
} from '~/types/browser';

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

function getFullscreenElement(): Element | null {
	const doc = getExtendedDocument();
	return (
		document.fullscreenElement ||
		doc.webkitFullscreenElement ||
		doc.mozFullScreenElement ||
		doc.msFullscreenElement ||
		null
	);
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

	const enterFullscreen = useCallback(async () => {
		if (useIOSFullscreen && videoRef?.current) {
			try {
				const video = videoRef.current as ExtendedHTMLVideoElement;
				if (video.webkitEnterFullscreen) {
					await video.webkitEnterFullscreen();
					return;
				}
			} catch (error) {
				console.error('Failed to enter iOS fullscreen:', error);
			}
		}

		const container = containerRef.current;
		if (!container || !supportsContainerFullscreenAPI()) return;

		try {
			await requestFullscreen(container);
		} catch (error) {
			console.error('Failed to enter fullscreen:', error);
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
				console.error('Failed to exit iOS fullscreen:', error);
			}
		}

		if (!getFullscreenElement()) return;

		try {
			await exitFullscreenAPI();
		} catch (error) {
			console.error('Failed to exit fullscreen:', error);
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
