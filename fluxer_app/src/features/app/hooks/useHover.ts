// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	canUseWindowFocusedHoverControls,
	subscribeWindowHoverControlsChange,
} from '@app/features/ui/utils/WindowFocusInteractionGuard';
import type React from 'react';
import {useCallback, useEffect, useRef, useState} from 'react';
import {
	createHoverStateSnapshot,
	type HoverStateEvent,
	type HoverStateSnapshot,
	selectIsHovering,
	transitionHoverStateSnapshot,
} from './HoverStateMachine';

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
	const [snapshot, setSnapshot] = useState<HoverStateSnapshot>(createHoverStateSnapshot);
	const snapshotRef = useRef(snapshot);
	const previousNode = useRef<HTMLElement | null>(null);
	const timeoutId = useRef<NodeJS.Timeout | null>(null);
	const clearHoverTimeout = useCallback(() => {
		if (!timeoutId.current) return;
		clearTimeout(timeoutId.current);
		timeoutId.current = null;
	}, []);
	const sendHoverEvent = useCallback((event: HoverStateEvent) => {
		const nextSnapshot = transitionHoverStateSnapshot(snapshotRef.current, event);
		if (selectIsHovering(nextSnapshot) === selectIsHovering(snapshotRef.current)) return;
		snapshotRef.current = nextSnapshot;
		setSnapshot(nextSnapshot);
	}, []);
	const handleMouseEnter = useCallback(() => {
		clearHoverTimeout();
		if (!canUseWindowFocusedHoverControls()) {
			sendHoverEvent({type: 'hover.leave'});
			return;
		}
		timeoutId.current = setTimeout(() => {
			if (canUseWindowFocusedHoverControls() && previousNode.current?.matches(':hover')) {
				sendHoverEvent({type: 'hover.enter'});
			}
		}, delay);
	}, [clearHoverTimeout, delay, sendHoverEvent]);
	const handleMouseLeave = useCallback(() => {
		clearHoverTimeout();
		sendHoverEvent({type: 'hover.leave'});
	}, [clearHoverTimeout, sendHoverEvent]);
	const syncHoverWithWindowFocus = useCallback(() => {
		clearHoverTimeout();
		if (!canUseWindowFocusedHoverControls() || !previousNode.current?.matches(':hover')) {
			sendHoverEvent({type: 'hover.leave'});
			return;
		}
		timeoutId.current = setTimeout(() => {
			if (canUseWindowFocusedHoverControls() && previousNode.current?.matches(':hover')) {
				sendHoverEvent({type: 'hover.enter'});
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
			sendHoverEvent({type: 'hover.leave'});
			previousNode.current = node;
			if (node) {
				node.addEventListener('mouseenter', handleMouseEnter);
				node.addEventListener('mouseleave', handleMouseLeave);
				if (node.matches(':hover')) handleMouseEnter();
			}
		},
		[clearHoverTimeout, handleMouseEnter, handleMouseLeave, sendHoverEvent],
	);
	return [customRef, selectIsHovering(snapshot)];
};
