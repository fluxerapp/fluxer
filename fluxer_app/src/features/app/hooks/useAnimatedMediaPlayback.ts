// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	isUnfocusedFullyInteractive,
	WINDOW_HOVER_CONTROLS_CHANGE_EVENT,
} from '@app/features/ui/utils/WindowFocusInteractionGuard';
import {type RefObject, useEffect, useSyncExternalStore} from 'react';

type AnimatedMediaPlaybackListener = () => void;

interface UseAnimatedMediaPlaybackAllowedOptions {
	enabled?: boolean;
	isAnimated?: boolean;
}

const listeners = new Set<AnimatedMediaPlaybackListener>();

let teardownGlobalListeners: (() => void) | null = null;
let lastAllowedSnapshot = true;

const subscribeConstant = (): (() => void) => () => {};
const getAlwaysAllowed = (): boolean => true;
const getNeverAllowed = (): boolean => false;

export function getAnimatedMediaPlaybackAllowed(root?: HTMLElement): boolean {
	if (typeof document === 'undefined') return true;
	if (document.hidden) return false;
	if (typeof document.hasFocus === 'function' && document.hasFocus()) return true;
	return isUnfocusedFullyInteractive(root ?? document.documentElement);
}

function notifyIfPlaybackChanged(): void {
	const nextAllowed = getAnimatedMediaPlaybackAllowed();
	if (nextAllowed === lastAllowedSnapshot) return;
	lastAllowedSnapshot = nextAllowed;
	listeners.forEach((listener) => listener());
}

function installGlobalListeners(): void {
	if (teardownGlobalListeners || typeof window === 'undefined' || typeof document === 'undefined') return;
	lastAllowedSnapshot = getAnimatedMediaPlaybackAllowed();
	window.addEventListener('focus', notifyIfPlaybackChanged);
	window.addEventListener('blur', notifyIfPlaybackChanged);
	window.addEventListener(WINDOW_HOVER_CONTROLS_CHANGE_EVENT, notifyIfPlaybackChanged);
	document.addEventListener('visibilitychange', notifyIfPlaybackChanged);
	const classObserver = typeof MutationObserver !== 'undefined' ? new MutationObserver(notifyIfPlaybackChanged) : null;
	classObserver?.observe(document.documentElement, {attributes: true, attributeFilter: ['class']});
	teardownGlobalListeners = () => {
		window.removeEventListener('focus', notifyIfPlaybackChanged);
		window.removeEventListener('blur', notifyIfPlaybackChanged);
		window.removeEventListener(WINDOW_HOVER_CONTROLS_CHANGE_EVENT, notifyIfPlaybackChanged);
		document.removeEventListener('visibilitychange', notifyIfPlaybackChanged);
		classObserver?.disconnect();
		teardownGlobalListeners = null;
	};
}

export function subscribeAnimatedMediaPlaybackChange(listener: AnimatedMediaPlaybackListener): () => void {
	installGlobalListeners();
	listeners.add(listener);
	return () => {
		listeners.delete(listener);
		if (listeners.size === 0) {
			teardownGlobalListeners?.();
		}
	};
}

export function useAnimatedMediaPlaybackAllowed({
	enabled = true,
	isAnimated = true,
}: UseAnimatedMediaPlaybackAllowedOptions = {}): boolean {
	const getConstant = isAnimated ? getAlwaysAllowed : getNeverAllowed;
	const live = enabled && isAnimated;
	return useSyncExternalStore(
		live ? subscribeAnimatedMediaPlaybackChange : subscribeConstant,
		live ? getAnimatedMediaPlaybackAllowed : getConstant,
		getConstant,
	);
}

export function useAnimatedMediaVideoPlayback(
	videoRef: RefObject<HTMLVideoElement | null>,
	{
		enabled = true,
		shouldPlay = true,
		isAnimated = true,
	}: {enabled?: boolean; shouldPlay?: boolean; isAnimated?: boolean} = {},
): boolean {
	const playbackAllowed = useAnimatedMediaPlaybackAllowed({isAnimated});
	useEffect(() => {
		const video = videoRef.current;
		if (!video) return;
		const playAllowed = enabled && shouldPlay && playbackAllowed;
		video.autoplay = playAllowed;
		if (playAllowed) {
			void video.play().catch(() => {});
		} else {
			video.pause();
		}
	}, [enabled, playbackAllowed, shouldPlay, videoRef]);
	return playbackAllowed;
}
