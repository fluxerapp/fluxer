// SPDX-License-Identifier: AGPL-3.0-or-later

import {getLiveResizeSnapshot, subscribeToLiveResize} from '@app/features/window/state/LiveResize';
import {useCallback, useEffect, useLayoutEffect, useRef, useSyncExternalStore} from 'react';

const SETTLE_DURATION_MS = 200;
const SETTLE_EASING = 'cubic-bezier(0.2, 0, 0, 1)';
const SETTLE_EPSILON_PX = 0.5;

export function useWindowLiveResize(): boolean {
	return useSyncExternalStore(subscribeToLiveResize, getLiveResizeSnapshot, getLiveResizeSnapshot);
}

export function useModalResizePin(
	isResizing: boolean,
	prefersReducedMotion: boolean,
): (node: HTMLElement | null) => void {
	const nodeRef = useRef<HTMLElement | null>(null);
	const pinnedRef = useRef<{left: number; top: number} | null>(null);

	const setNode = useCallback((node: HTMLElement | null) => {
		nodeRef.current = node;
	}, []);

	useLayoutEffect(() => {
		const node = nodeRef.current;
		if (node == null) return;

		if (isResizing) {
			const rect = node.getBoundingClientRect();
			pinnedRef.current = {left: rect.left, top: rect.top};
			node.style.transition = '';
			node.style.transform = '';
			node.style.position = 'absolute';
			node.style.left = `clamp(0px, ${rect.left}px, max(0px, 100% - ${rect.width}px))`;
			node.style.top = `clamp(0px, ${rect.top}px, max(0px, 100% - ${rect.height}px))`;
			return;
		}

		const pinned = pinnedRef.current;
		if (pinned == null) return;
		pinnedRef.current = null;

		const first = node.getBoundingClientRect();
		node.style.position = '';
		node.style.left = '';
		node.style.top = '';
		const last = node.getBoundingClientRect();
		const deltaX = first.left - last.left;
		const deltaY = first.top - last.top;

		if (prefersReducedMotion || (Math.abs(deltaX) < SETTLE_EPSILON_PX && Math.abs(deltaY) < SETTLE_EPSILON_PX)) {
			node.style.transition = '';
			node.style.transform = '';
			return;
		}

		node.style.transition = 'none';
		node.style.transform = `translate(${deltaX}px, ${deltaY}px)`;
		const settle = requestAnimationFrame(() => {
			node.style.transition = `transform ${SETTLE_DURATION_MS}ms ${SETTLE_EASING}`;
			node.style.transform = 'translate(0px, 0px)';
		});
		const clear = (event: TransitionEvent) => {
			if (event.propertyName !== 'transform') return;
			node.style.transition = '';
			node.style.transform = '';
			node.removeEventListener('transitionend', clear);
		};
		node.addEventListener('transitionend', clear);
		return () => {
			cancelAnimationFrame(settle);
			node.removeEventListener('transitionend', clear);
		};
	}, [isResizing, prefersReducedMotion]);

	useEffect(() => {
		return () => {
			const node = nodeRef.current;
			if (node == null) return;
			node.style.position = '';
			node.style.left = '';
			node.style.top = '';
			node.style.transition = '';
			node.style.transform = '';
		};
	}, []);

	return setNode;
}
