// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	registerMessageHoverTarget,
	resolveMessageHoverTargetsNow,
} from '@app/features/channel/components/MessageHoverTracking';
import MessageHoverTrackingRollout from '@app/features/channel/state/MessageHoverTrackingRollout';
import {subscribeWindowFocus} from '@app/features/platform/utils/WindowFocusBroadcast';
import type React from 'react';
import {useCallback, useEffect, useLayoutEffect, useRef, useState} from 'react';

const isPointInsideMessageTree = (messageElement: HTMLElement, point: {x: number; y: number}): boolean => {
	const target = messageElement.ownerDocument.elementFromPoint(point.x, point.y);
	return Boolean(target && messageElement.contains(target));
};
const HOVER_SCROLL_IDLE_MS = 150;
let lastPointerPosition: {x: number; y: number} | null = null;
let pointerPositionNotificationFrame: number | null = null;
let pointerPositionSubscriptionCount = 0;
let hoverInvalidationSubscriptionCount = 0;
let pointerHoverSuspendedByScroll = false;
let pointerHoverScrollIdleTimer: ReturnType<typeof setTimeout> | null = null;
const pointerPositionListeners = new Set<() => void>();
const hoverInvalidationListeners = new Set<() => void>();
const notifyPointerPositionListeners = (): void => {
	for (const listener of Array.from(pointerPositionListeners)) {
		listener();
	}
};
const notifyHoverInvalidationListeners = (): void => {
	for (const listener of Array.from(hoverInvalidationListeners)) {
		listener();
	}
};
const clearPointerHoverScrollIdleTimer = (): void => {
	if (pointerHoverScrollIdleTimer == null) {
		return;
	}
	clearTimeout(pointerHoverScrollIdleTimer);
	pointerHoverScrollIdleTimer = null;
};
const resumePointerHoverAfterScroll = (): void => {
	clearPointerHoverScrollIdleTimer();
	if (!pointerHoverSuspendedByScroll) {
		return;
	}
	pointerHoverSuspendedByScroll = false;
	notifyHoverInvalidationListeners();
};
const suspendPointerHoverForScroll = (): void => {
	clearPointerHoverScrollIdleTimer();
	pointerHoverScrollIdleTimer = setTimeout(resumePointerHoverAfterScroll, HOVER_SCROLL_IDLE_MS);
	if (pointerHoverSuspendedByScroll) {
		return;
	}
	pointerHoverSuspendedByScroll = true;
	if (pointerPositionNotificationFrame != null) {
		cancelAnimationFrame(pointerPositionNotificationFrame);
		pointerPositionNotificationFrame = null;
	}
	notifyHoverInvalidationListeners();
};
const schedulePointerPositionNotification = (): void => {
	if (pointerPositionNotificationFrame != null) {
		return;
	}
	pointerPositionNotificationFrame = requestAnimationFrame(() => {
		pointerPositionNotificationFrame = null;
		notifyPointerPositionListeners();
	});
};
const updateLastPointerPosition = (event: PointerEvent | MouseEvent): void => {
	if (lastPointerPosition?.x === event.clientX && lastPointerPosition.y === event.clientY) {
		return;
	}
	lastPointerPosition = {x: event.clientX, y: event.clientY};
	resumePointerHoverAfterScroll();
	schedulePointerPositionNotification();
};
const clearLastPointerPosition = (): void => {
	if (!lastPointerPosition) {
		return;
	}
	lastPointerPosition = null;
	schedulePointerPositionNotification();
};
const clearLastPointerPositionOnWindowBlur = (): void => {
	clearLastPointerPosition();
	notifyHoverInvalidationListeners();
};
const clearLastPointerPositionOnWindowExit = (event: PointerEvent | MouseEvent): void => {
	if (event.relatedTarget == null) {
		clearLastPointerPosition();
	}
};
const supportsPointerPositionEvents = (): boolean => 'PointerEvent' in window;
const subscribePointerPosition = (listener: () => void): (() => void) => {
	if (pointerPositionSubscriptionCount === 0) {
		if (supportsPointerPositionEvents()) {
			window.addEventListener('pointermove', updateLastPointerPosition, true);
			window.addEventListener('pointerdown', updateLastPointerPosition, true);
			window.addEventListener('pointerout', clearLastPointerPositionOnWindowExit, true);
		} else {
			window.addEventListener('mousemove', updateLastPointerPosition, true);
			window.addEventListener('mousedown', updateLastPointerPosition, true);
			window.addEventListener('mouseout', clearLastPointerPositionOnWindowExit, true);
		}
	}
	pointerPositionSubscriptionCount += 1;
	pointerPositionListeners.add(listener);
	return () => {
		pointerPositionListeners.delete(listener);
		pointerPositionSubscriptionCount = Math.max(0, pointerPositionSubscriptionCount - 1);
		if (pointerPositionSubscriptionCount !== 0) {
			return;
		}
		lastPointerPosition = null;
		if (pointerPositionNotificationFrame != null) {
			cancelAnimationFrame(pointerPositionNotificationFrame);
			pointerPositionNotificationFrame = null;
		}
		if (supportsPointerPositionEvents()) {
			window.removeEventListener('pointermove', updateLastPointerPosition, true);
			window.removeEventListener('pointerdown', updateLastPointerPosition, true);
			window.removeEventListener('pointerout', clearLastPointerPositionOnWindowExit, true);
		} else {
			window.removeEventListener('mousemove', updateLastPointerPosition, true);
			window.removeEventListener('mousedown', updateLastPointerPosition, true);
			window.removeEventListener('mouseout', clearLastPointerPositionOnWindowExit, true);
		}
	};
};
const subscribeMessageHoverInvalidation = (listener: () => void): (() => void) => {
	if (hoverInvalidationSubscriptionCount === 0) {
		window.addEventListener('scroll', suspendPointerHoverForScroll, true);
		window.addEventListener('resize', notifyHoverInvalidationListeners);
		window.addEventListener('blur', clearLastPointerPositionOnWindowBlur);
	}
	hoverInvalidationSubscriptionCount += 1;
	hoverInvalidationListeners.add(listener);
	return () => {
		hoverInvalidationListeners.delete(listener);
		hoverInvalidationSubscriptionCount = Math.max(0, hoverInvalidationSubscriptionCount - 1);
		if (hoverInvalidationSubscriptionCount !== 0) {
			return;
		}
		clearPointerHoverScrollIdleTimer();
		pointerHoverSuspendedByScroll = false;
		window.removeEventListener('scroll', suspendPointerHoverForScroll, true);
		window.removeEventListener('resize', notifyHoverInvalidationListeners);
		window.removeEventListener('blur', clearLastPointerPositionOnWindowBlur);
	};
};

interface UseMessageHoverStateParams {
	messageRef: React.RefObject<HTMLDivElement | null>;
	mobileLayoutEnabled: boolean;
	keyboardModeEnabled: boolean;
	contextMenuOpen: boolean;
}

export interface MessageHoverState {
	isHovering: boolean;
	isPopoutOpen: boolean;
	handlePopoutToggle: (isOpen: boolean) => void;
	trackingEnabled: boolean;
}

export function useMessageHoverState({
	messageRef,
	mobileLayoutEnabled,
	keyboardModeEnabled,
	contextMenuOpen,
}: UseMessageHoverStateParams): MessageHoverState {
	const trackingEnabled = MessageHoverTrackingRollout.enabled;
	const [isHoveringDesktop, setIsHoveringDesktop] = useState(false);
	const [isPopoutOpen, setIsPopoutOpen] = useState(false);
	const isHoveringDesktopRef = useRef(false);
	const popoutCloseRafRef = useRef<number | null>(null);
	const setDesktopHoverState = useCallback((isHovered: boolean) => {
		if (isHoveringDesktopRef.current === isHovered) {
			return;
		}
		isHoveringDesktopRef.current = isHovered;
		setIsHoveringDesktop(isHovered);
	}, []);
	const isPointerInsideMessage = useCallback((): boolean => {
		if (mobileLayoutEnabled) {
			return false;
		}
		if (pointerHoverSuspendedByScroll) {
			return false;
		}
		const element = messageRef.current;
		if (!element) {
			return false;
		}
		if (!lastPointerPosition) {
			return element.matches(':hover');
		}
		return isPointInsideMessageTree(element, lastPointerPosition);
	}, [mobileLayoutEnabled, messageRef]);
	const syncPointerHoverState = useCallback((): boolean => {
		const isHovered = isPointerInsideMessage();
		setDesktopHoverState(isHovered);
		return isHovered;
	}, [isPointerInsideMessage, setDesktopHoverState]);
	const cancelScheduledPopoutClose = useCallback(() => {
		if (popoutCloseRafRef.current == null) {
			return;
		}
		cancelAnimationFrame(popoutCloseRafRef.current);
		popoutCloseRafRef.current = null;
	}, []);
	const handlePopoutToggle = useCallback(
		(isOpen: boolean) => {
			if (trackingEnabled) {
				setIsPopoutOpen(isOpen);
				return;
			}
			if (isOpen) {
				cancelScheduledPopoutClose();
				setIsPopoutOpen(true);
				return;
			}
			cancelScheduledPopoutClose();
			popoutCloseRafRef.current = requestAnimationFrame(() => {
				popoutCloseRafRef.current = null;
				syncPointerHoverState();
				setIsPopoutOpen(false);
			});
		},
		[cancelScheduledPopoutClose, syncPointerHoverState, trackingEnabled],
	);
	useEffect(() => {
		if (!trackingEnabled || mobileLayoutEnabled) {
			return;
		}
		const element = messageRef.current;
		if (element == null) {
			return;
		}
		return registerMessageHoverTarget(element, setDesktopHoverState);
	}, [trackingEnabled, mobileLayoutEnabled, messageRef, setDesktopHoverState]);
	useEffect(() => {
		if (trackingEnabled || mobileLayoutEnabled || !messageRef.current) return;
		const element = messageRef.current;
		const handleMouseEnter = (event: MouseEvent) => {
			updateLastPointerPosition(event);
			if (pointerHoverSuspendedByScroll) {
				return;
			}
			setDesktopHoverState(true);
		};
		const handleMouseLeave = (event: MouseEvent) => {
			updateLastPointerPosition(event);
			setDesktopHoverState(false);
		};
		element.addEventListener('mouseenter', handleMouseEnter);
		element.addEventListener('mouseleave', handleMouseLeave);
		const unsubscribeFocus = subscribeWindowFocus(syncPointerHoverState);
		const unsubscribeHoverInvalidation = subscribeMessageHoverInvalidation(syncPointerHoverState);
		const rafId = requestAnimationFrame(syncPointerHoverState);
		return () => {
			cancelAnimationFrame(rafId);
			element.removeEventListener('mouseenter', handleMouseEnter);
			element.removeEventListener('mouseleave', handleMouseLeave);
			unsubscribeFocus();
			unsubscribeHoverInvalidation();
		};
	}, [
		trackingEnabled,
		mobileLayoutEnabled,
		keyboardModeEnabled,
		messageRef,
		setDesktopHoverState,
		syncPointerHoverState,
	]);
	const shouldTrackActivePointer =
		!trackingEnabled && !mobileLayoutEnabled && (isHoveringDesktop || isPopoutOpen || contextMenuOpen);
	useEffect(() => {
		if (!shouldTrackActivePointer) {
			return;
		}
		const rafId = requestAnimationFrame(syncPointerHoverState);
		const unsubscribePointerPosition = subscribePointerPosition(syncPointerHoverState);
		return () => {
			cancelAnimationFrame(rafId);
			unsubscribePointerPosition();
		};
	}, [shouldTrackActivePointer, syncPointerHoverState]);
	const isOverlayOpen = trackingEnabled ? contextMenuOpen || isPopoutOpen : contextMenuOpen;
	const wasOverlayOpenRef = useRef(false);
	useLayoutEffect(() => {
		const wasOpen = wasOverlayOpenRef.current;
		wasOverlayOpenRef.current = isOverlayOpen;
		if (!wasOpen || isOverlayOpen) {
			return;
		}
		if (trackingEnabled) {
			resolveMessageHoverTargetsNow();
			return;
		}
		syncPointerHoverState();
	}, [isOverlayOpen, syncPointerHoverState, trackingEnabled]);
	useEffect(() => {
		return () => {
			cancelScheduledPopoutClose();
		};
	}, [cancelScheduledPopoutClose]);
	return {
		isHovering: mobileLayoutEnabled ? false : isHoveringDesktop,
		isPopoutOpen,
		handlePopoutToggle,
		trackingEnabled,
	};
}
