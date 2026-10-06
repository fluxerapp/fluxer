// SPDX-License-Identifier: AGPL-3.0-or-later

type MessageHoverListener = (isHovered: boolean) => void;

const SCROLL_IDLE_MS = 150;

const hoverTargets = new Map<HTMLElement, MessageHoverListener>();
let pointerPosition: {x: number; y: number} | null = null;
let hoveredTarget: HTMLElement | null = null;
let resolveFrame: number | null = null;
let globalListenersAttached = false;
let scrollIdleTimer: ReturnType<typeof setTimeout> | null = null;
let suspendedByScroll = false;

const resolveHoveredTarget = (): HTMLElement | null => {
	if (pointerPosition == null) {
		return null;
	}
	let node: Element | null = document.elementFromPoint(pointerPosition.x, pointerPosition.y);
	while (node != null) {
		if (node instanceof HTMLElement && hoverTargets.has(node)) {
			return node;
		}
		node = node.parentElement;
	}
	return null;
};

const setHoveredTarget = (nextHoveredTarget: HTMLElement | null): void => {
	if (hoveredTarget === nextHoveredTarget) {
		return;
	}
	const previousHoveredTarget = hoveredTarget;
	hoveredTarget = nextHoveredTarget;
	if (previousHoveredTarget != null) {
		hoverTargets.get(previousHoveredTarget)?.(false);
	}
	if (nextHoveredTarget != null) {
		hoverTargets.get(nextHoveredTarget)?.(true);
	}
};

const cancelScheduledResolve = (): void => {
	if (resolveFrame == null) {
		return;
	}
	cancelAnimationFrame(resolveFrame);
	resolveFrame = null;
};

const scheduleResolve = (): void => {
	if (suspendedByScroll || resolveFrame != null) {
		return;
	}
	resolveFrame = requestAnimationFrame(() => {
		resolveFrame = null;
		setHoveredTarget(resolveHoveredTarget());
	});
};

const clearScrollIdleTimer = (): void => {
	if (scrollIdleTimer == null) {
		return;
	}
	clearTimeout(scrollIdleTimer);
	scrollIdleTimer = null;
};

const resumeAfterScroll = (): void => {
	clearScrollIdleTimer();
	if (!suspendedByScroll) {
		return;
	}
	suspendedByScroll = false;
	scheduleResolve();
};

const forgetPointerPosition = (): void => {
	pointerPosition = null;
	cancelScheduledResolve();
	clearScrollIdleTimer();
	suspendedByScroll = false;
	setHoveredTarget(null);
};

const isTouchPointerEvent = (event: PointerEvent | MouseEvent): boolean =>
	'pointerType' in event && event.pointerType === 'touch';

const handlePointerActivity = (event: PointerEvent | MouseEvent): void => {
	if (isTouchPointerEvent(event)) {
		return;
	}
	pointerPosition = {x: event.clientX, y: event.clientY};
	scheduleResolve();
};

const handlePointerWindowExit = (event: PointerEvent | MouseEvent): void => {
	if (isTouchPointerEvent(event) || event.relatedTarget != null) {
		return;
	}
	forgetPointerPosition();
};

const handlePointerMotion = (event: PointerEvent | MouseEvent): void => {
	if (isTouchPointerEvent(event)) {
		return;
	}
	pointerPosition = {x: event.clientX, y: event.clientY};
	if (suspendedByScroll) {
		resumeAfterScroll();
		return;
	}
	scheduleResolve();
};

const handleScroll = (): void => {
	suspendedByScroll = true;
	cancelScheduledResolve();
	setHoveredTarget(null);
	clearScrollIdleTimer();
	scrollIdleTimer = setTimeout(resumeAfterScroll, SCROLL_IDLE_MS);
};

const handleLayoutChange = (): void => {
	scheduleResolve();
};

const supportsPointerEvents = (): boolean => 'PointerEvent' in window;

const attachGlobalListeners = (): void => {
	if (globalListenersAttached) {
		return;
	}
	globalListenersAttached = true;
	if (supportsPointerEvents()) {
		window.addEventListener('pointermove', handlePointerMotion, true);
		window.addEventListener('pointerdown', handlePointerMotion, true);
		window.addEventListener('pointerover', handlePointerActivity, true);
		window.addEventListener('pointerout', handlePointerWindowExit, true);
	} else {
		window.addEventListener('mousemove', handlePointerMotion, true);
		window.addEventListener('mousedown', handlePointerMotion, true);
		window.addEventListener('mouseover', handlePointerActivity, true);
		window.addEventListener('mouseout', handlePointerWindowExit, true);
	}
	window.addEventListener('scroll', handleScroll, true);
	window.addEventListener('resize', handleLayoutChange);
	window.addEventListener('blur', forgetPointerPosition);
};

const detachGlobalListeners = (): void => {
	if (!globalListenersAttached) {
		return;
	}
	globalListenersAttached = false;
	clearScrollIdleTimer();
	suspendedByScroll = false;
	if (supportsPointerEvents()) {
		window.removeEventListener('pointermove', handlePointerMotion, true);
		window.removeEventListener('pointerdown', handlePointerMotion, true);
		window.removeEventListener('pointerover', handlePointerActivity, true);
		window.removeEventListener('pointerout', handlePointerWindowExit, true);
	} else {
		window.removeEventListener('mousemove', handlePointerMotion, true);
		window.removeEventListener('mousedown', handlePointerMotion, true);
		window.removeEventListener('mouseover', handlePointerActivity, true);
		window.removeEventListener('mouseout', handlePointerWindowExit, true);
	}
	window.removeEventListener('scroll', handleScroll, true);
	window.removeEventListener('resize', handleLayoutChange);
	window.removeEventListener('blur', forgetPointerPosition);
};

export function registerMessageHoverTarget(element: HTMLElement, listener: MessageHoverListener): () => void {
	hoverTargets.set(element, listener);
	attachGlobalListeners();
	scheduleResolve();
	return () => {
		if (hoverTargets.get(element) !== listener) {
			return;
		}
		hoverTargets.delete(element);
		if (hoveredTarget === element) {
			hoveredTarget = null;
			listener(false);
		}
		if (hoverTargets.size > 0) {
			scheduleResolve();
			return;
		}
		detachGlobalListeners();
		cancelScheduledResolve();
		hoveredTarget = null;
	};
}

export function refreshMessageHoverTargets(): void {
	scheduleResolve();
}

export function resolveMessageHoverTargetsNow(): void {
	if (suspendedByScroll) {
		return;
	}
	cancelScheduledResolve();
	setHoveredTarget(resolveHoveredTarget());
}

export function resetMessageHoverTrackingForTests(): void {
	detachGlobalListeners();
	cancelScheduledResolve();
	clearScrollIdleTimer();
	suspendedByScroll = false;
	hoverTargets.clear();
	pointerPosition = null;
	hoveredTarget = null;
}
