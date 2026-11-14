// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	canUseWindowFocusedHoverControls,
	subscribeWindowHoverControlsChange,
} from '@app/features/ui/utils/WindowFocusInteractionGuard';
import type React from 'react';
import {useCallback, useEffect, useRef, useState} from 'react';

type HoverHook = [React.RefCallback<HTMLElement>, boolean];

const hoverWindowFocusListeners = new Set<() => void>();
let disposeHoverWindowFocusBridge: (() => void) | null = null;

function notifyHoverWindowFocusListeners(): void {
	for (const listener of Array.from(hoverWindowFocusListeners)) {
		listener();
	}
}

function subscribeHoverWindowFocus(listener: () => void): () => void {
	hoverWindowFocusListeners.add(listener);
	if (disposeHoverWindowFocusBridge == null) {
		const unsubscribeHoverControls = subscribeWindowHoverControlsChange(notifyHoverWindowFocusListeners);
		window.addEventListener('blur', notifyHoverWindowFocusListeners);
		disposeHoverWindowFocusBridge = () => {
			unsubscribeHoverControls();
			window.removeEventListener('blur', notifyHoverWindowFocusListeners);
		};
	}
	return () => {
		hoverWindowFocusListeners.delete(listener);
		if (hoverWindowFocusListeners.size > 0 || disposeHoverWindowFocusBridge == null) return;
		disposeHoverWindowFocusBridge();
		disposeHoverWindowFocusBridge = null;
	};
}

export const useHover = (delay = 0): HoverHook => {
	const [isHovering, setHovering] = useState(false);
	const hoveringRef = useRef(false);
	const previousNode = useRef<HTMLElement | null>(null);
	const timeoutId = useRef<NodeJS.Timeout | null>(null);
	const clearHoverTimeout = useCallback(() => {
		if (!timeoutId.current) return;
		clearTimeout(timeoutId.current);
		timeoutId.current = null;
	}, []);
	const sendHoverEvent = useCallback((next: boolean) => {
		if (hoveringRef.current === next) return;
		hoveringRef.current = next;
		setHovering(next);
	}, []);
	const handleMouseEnter = useCallback(() => {
		clearHoverTimeout();
		if (!canUseWindowFocusedHoverControls()) {
			sendHoverEvent(false);
			return;
		}
		timeoutId.current = setTimeout(() => {
			if (canUseWindowFocusedHoverControls() && previousNode.current?.matches(':hover')) {
				sendHoverEvent(true);
			}
		}, delay);
	}, [clearHoverTimeout, delay, sendHoverEvent]);
	const handleMouseLeave = useCallback(() => {
		clearHoverTimeout();
		sendHoverEvent(false);
	}, [clearHoverTimeout, sendHoverEvent]);
	const syncHoverWithWindowFocus = useCallback(() => {
		clearHoverTimeout();
		if (!canUseWindowFocusedHoverControls() || !previousNode.current?.matches(':hover')) {
			sendHoverEvent(false);
			return;
		}
		timeoutId.current = setTimeout(() => {
			if (canUseWindowFocusedHoverControls() && previousNode.current?.matches(':hover')) {
				sendHoverEvent(true);
			}
		}, delay);
	}, [clearHoverTimeout, delay, sendHoverEvent]);
	useEffect(() => {
		const unsubscribe = subscribeHoverWindowFocus(syncHoverWithWindowFocus);
		return () => {
			unsubscribe();
			clearHoverTimeout();
		};
	}, [clearHoverTimeout, syncHoverWithWindowFocus]);
	const customRef = useCallback(
		(node: HTMLElement | null) => {
			if (previousNode.current === node) return;
			if (previousNode.current) {
				previousNode.current.removeEventListener('mouseenter', handleMouseEnter);
				previousNode.current.removeEventListener('mouseleave', handleMouseLeave);
			}
			clearHoverTimeout();
			sendHoverEvent(false);
			previousNode.current = node;
			if (node) {
				node.addEventListener('mouseenter', handleMouseEnter);
				node.addEventListener('mouseleave', handleMouseLeave);
				if (node.matches(':hover')) handleMouseEnter();
			}
		},
		[clearHoverTimeout, handleMouseEnter, handleMouseLeave, sendHoverEvent],
	);
	return [customRef, isHovering];
};
