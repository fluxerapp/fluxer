// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	getExtendedDocument,
	supportsMozRequestFullScreen,
	supportsMsRequestFullscreen,
	supportsWebkitRequestFullscreen,
} from '@app/features/platform/types/Browser';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {setActivePortalHost} from '@app/features/ui/overlay/PortalHostContext';
import Modal from '@app/features/ui/state/Modal';
import {VOICE_CALL_FULLSCREEN_ENABLED} from '@app/features/voice/components/VoiceCallFullscreenFeatureFlag';
import VoiceCallFullscreen from '@app/features/voice/state/VoiceCallFullscreen';
import type {ExtendedHTMLElement} from '@app/types/browser.d';
import type {RefObject} from 'react';
import {useCallback, useEffect, useRef, useState} from 'react';

const logger = new Logger('useVoiceCallAppFullscreen');

interface UseVoiceCallAppFullscreenOptions {
	containerRef: RefObject<HTMLElement | null> | RefObject<HTMLElement>;
}

interface UseVoiceCallAppFullscreenReturn {
	isFullscreen: boolean;
	supportsFullscreen: boolean;
	enterFullscreen: () => Promise<void>;
	exitFullscreen: () => Promise<void>;
	toggleFullscreen: () => Promise<void>;
}

export function getVoiceCallFullscreenElement(): Element | null {
	const doc = getExtendedDocument();
	return (
		document.fullscreenElement ||
		doc.webkitFullscreenElement ||
		doc.mozFullScreenElement ||
		doc.msFullscreenElement ||
		null
	);
}

function supportsFullscreenAPI(): boolean {
	const doc = getExtendedDocument();
	return !!(
		document.fullscreenEnabled ||
		doc.webkitFullscreenEnabled ||
		doc.mozFullScreenEnabled ||
		doc.msFullscreenEnabled
	);
}

async function requestFullscreen(element: HTMLElement): Promise<void> {
	if (element.requestFullscreen) {
		await element.requestFullscreen();
		return;
	}
	if (supportsWebkitRequestFullscreen(element)) {
		const extendedElement = element as ExtendedHTMLElement;
		await extendedElement.webkitRequestFullscreen!();
		return;
	}
	if (supportsMozRequestFullScreen(element)) {
		const extendedElement = element as ExtendedHTMLElement;
		await extendedElement.mozRequestFullScreen!();
		return;
	}
	if (supportsMsRequestFullscreen(element)) {
		const extendedElement = element as ExtendedHTMLElement;
		await extendedElement.msRequestFullscreen!();
	}
}

async function exitFullscreenAPI(): Promise<void> {
	const doc = getExtendedDocument();
	if (document.exitFullscreen) {
		await document.exitFullscreen();
		return;
	}
	if (doc.webkitExitFullscreen) {
		await doc.webkitExitFullscreen();
		return;
	}
	if (doc.mozCancelFullScreen) {
		await doc.mozCancelFullScreen();
		return;
	}
	if (doc.msExitFullscreen) {
		await doc.msExitFullscreen();
	}
}

const VOICE_CALL_FULLSCREEN_ATTR = 'data-voice-call-fullscreen';

export function useVoiceCallAppFullscreen(options: UseVoiceCallAppFullscreenOptions): UseVoiceCallAppFullscreenReturn {
	const {containerRef} = options;
	const [isFullscreen, setIsFullscreen] = useState(false);
	const [supportsFullscreen] = useState(() => VOICE_CALL_FULLSCREEN_ENABLED && supportsFullscreenAPI());
	const initiatedFullscreenRef = useRef(false);
	const handleFullscreenChange = useCallback(() => {
		if (!VOICE_CALL_FULLSCREEN_ENABLED) {
			setIsFullscreen(false);
			initiatedFullscreenRef.current = false;
			document.documentElement.removeAttribute(VOICE_CALL_FULLSCREEN_ATTR);
			return;
		}
		const fullscreenElement = getVoiceCallFullscreenElement();
		if (!fullscreenElement) {
			setIsFullscreen(false);
			initiatedFullscreenRef.current = false;
			document.documentElement.removeAttribute(VOICE_CALL_FULLSCREEN_ATTR);
			return;
		}
		const container = containerRef.current;
		const active = initiatedFullscreenRef.current && container != null && fullscreenElement === container;
		setIsFullscreen(active);
		if (!active) {
			document.documentElement.removeAttribute(VOICE_CALL_FULLSCREEN_ATTR);
		}
	}, [containerRef]);
	useEffect(() => {
		if (!VOICE_CALL_FULLSCREEN_ENABLED) return;
		document.addEventListener('fullscreenchange', handleFullscreenChange);
		document.addEventListener('webkitfullscreenchange', handleFullscreenChange);
		document.addEventListener('mozfullscreenchange', handleFullscreenChange);
		document.addEventListener('MSFullscreenChange', handleFullscreenChange);
		handleFullscreenChange();
		return () => {
			document.removeEventListener('fullscreenchange', handleFullscreenChange);
			document.removeEventListener('webkitfullscreenchange', handleFullscreenChange);
			document.removeEventListener('mozfullscreenchange', handleFullscreenChange);
			document.removeEventListener('MSFullscreenChange', handleFullscreenChange);
		};
	}, [handleFullscreenChange]);
	const enterFullscreen = useCallback(async () => {
		if (!VOICE_CALL_FULLSCREEN_ENABLED) return;
		if (!supportsFullscreenAPI()) return;
		const container = containerRef.current;
		if (!container) return;
		const fullscreenElement = getVoiceCallFullscreenElement();
		if (fullscreenElement === container && initiatedFullscreenRef.current) {
			setIsFullscreen(true);
			return;
		}
		try {
			initiatedFullscreenRef.current = true;
			await requestFullscreen(container);
			document.documentElement.setAttribute(VOICE_CALL_FULLSCREEN_ATTR, 'true');
			setIsFullscreen(true);
		} catch (error) {
			initiatedFullscreenRef.current = false;
			document.documentElement.removeAttribute(VOICE_CALL_FULLSCREEN_ATTR);
			logger.error('Failed to enter voice call view fullscreen:', error);
		}
	}, [containerRef]);
	const exitFullscreen = useCallback(async () => {
		if (!VOICE_CALL_FULLSCREEN_ENABLED) return;
		const fullscreenElement = getVoiceCallFullscreenElement();
		if (!fullscreenElement) {
			setIsFullscreen(false);
			return;
		}
		try {
			await exitFullscreenAPI();
		} catch (error) {
			logger.error('Failed to exit voice call view fullscreen:', error);
		} finally {
			setIsFullscreen(false);
		}
	}, []);
	const toggleFullscreen = useCallback(async () => {
		if (!VOICE_CALL_FULLSCREEN_ENABLED) return;
		if (isFullscreen) {
			await exitFullscreen();
			return;
		}
		await enterFullscreen();
	}, [enterFullscreen, exitFullscreen, isFullscreen]);
	useEffect(() => {
		if (!VOICE_CALL_FULLSCREEN_ENABLED) return;
		return () => {
			if (!initiatedFullscreenRef.current || getVoiceCallFullscreenElement() !== containerRef.current) {
				return;
			}
			void exitFullscreen();
		};
	}, [containerRef, exitFullscreen]);
	return {
		isFullscreen,
		supportsFullscreen,
		enterFullscreen,
		exitFullscreen,
		toggleFullscreen,
	};
}

export function useVoiceCallFullscreenPortalHost(active: boolean, host: HTMLElement | null): void {
	useEffect(() => {
		if (!active) return;
		if (!host) return;
		setActivePortalHost(host);
		return () => {
			setActivePortalHost(null);
			Modal.releasePortalHost(host);
		};
	}, [active, host]);
}

interface UseVoiceCallFullscreenViewStateOptions {
	active: boolean;
	scopeKey: string;
}

interface UseVoiceCallFullscreenViewStateReturn {
	showFullscreenView: boolean;
	fullscreenRequestNonce: number;
	openFullscreenView: () => void;
	closeFullscreenView: () => void;
}

export function useVoiceCallFullscreenViewState({
	active,
	scopeKey,
}: UseVoiceCallFullscreenViewStateOptions): UseVoiceCallFullscreenViewStateReturn {
	const showFullscreenView = VoiceCallFullscreen.isScopeActive(scopeKey);
	const fullscreenRequestNonce = showFullscreenView ? VoiceCallFullscreen.fullscreenRequestNonce : 0;
	const openFullscreenView = useCallback(() => {
		if (!VOICE_CALL_FULLSCREEN_ENABLED) return;
		if (!active) return;
		VoiceCallFullscreen.open(scopeKey);
	}, [active, scopeKey]);
	const closeFullscreenView = useCallback(() => {
		const wasActive = VoiceCallFullscreen.isScopeActive(scopeKey);
		VoiceCallFullscreen.close(scopeKey);
		if (!wasActive || !getVoiceCallFullscreenElement()) return;
		void exitFullscreenAPI().catch((error) => {
			logger.error('Failed to exit voice call fullscreen view:', error);
		});
	}, [scopeKey]);
	useEffect(() => {
		VoiceCallFullscreen.mountScope(scopeKey);
		return () => {
			VoiceCallFullscreen.unmountScope(scopeKey);
			window.setTimeout(() => {
				if (!VoiceCallFullscreen.hasMountedScope(scopeKey)) {
					VoiceCallFullscreen.close(scopeKey);
				}
			}, 0);
		};
	}, [scopeKey]);
	useEffect(() => {
		if (active) return;
		VoiceCallFullscreen.close(scopeKey);
	}, [active, scopeKey]);
	useEffect(() => {
		if (!showFullscreenView) return;
		const handleFullscreenChange = () => {
			if (getVoiceCallFullscreenElement()) return;
			VoiceCallFullscreen.close(scopeKey);
		};
		document.addEventListener('fullscreenchange', handleFullscreenChange);
		document.addEventListener('webkitfullscreenchange', handleFullscreenChange);
		document.addEventListener('mozfullscreenchange', handleFullscreenChange);
		document.addEventListener('MSFullscreenChange', handleFullscreenChange);
		return () => {
			document.removeEventListener('fullscreenchange', handleFullscreenChange);
			document.removeEventListener('webkitfullscreenchange', handleFullscreenChange);
			document.removeEventListener('mozfullscreenchange', handleFullscreenChange);
			document.removeEventListener('MSFullscreenChange', handleFullscreenChange);
		};
	}, [showFullscreenView, scopeKey]);
	return {
		showFullscreenView,
		fullscreenRequestNonce,
		openFullscreenView,
		closeFullscreenView,
	};
}
