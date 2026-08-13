// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	measureVisibleScrollViewportRect,
	type VisibleScrollViewportRect,
} from '@app/features/ui/utils/ScrollViewportGeometry';
import {useEffect} from 'react';
import {useDragDropManager} from 'react-dnd';

type DragAutoScrollAxis = 'horizontal' | 'vertical';

interface DragAutoScrollOptions {
	active: boolean;
	axis?: DragAutoScrollAxis;
	getScrollElement: () => HTMLElement | null;
}

const EDGE_SIZE_PX = 72;
const OUTSIDE_TOLERANCE_PX = 24;
const MIN_SPEED_PX_PER_SECOND = 120;
const MAX_SPEED_PX_PER_SECOND = 960;
const MAX_FRAME_DELTA_MS = 32;
function resolveOwnerWindow(element: HTMLElement): Window & typeof globalThis {
	const ownerWindow = element.ownerDocument.defaultView;
	if (ownerWindow == null) {
		throw new Error('Drag auto-scroll element has no owner window');
	}
	return ownerWindow;
}

function getScrollVelocity(pointer: number, start: number, end: number): number {
	if (pointer < start - OUTSIDE_TOLERANCE_PX || pointer > end + OUTSIDE_TOLERANCE_PX) return 0;
	const edgeSize = Math.min(EDGE_SIZE_PX, (end - start) / 3);
	if (edgeSize <= 0) return 0;
	if (pointer < start + edgeSize) {
		const intensity = Math.min(1, (start + edgeSize - pointer) / edgeSize);
		return -Math.max(MIN_SPEED_PX_PER_SECOND, MAX_SPEED_PX_PER_SECOND * intensity * intensity);
	}
	if (pointer > end - edgeSize) {
		const intensity = Math.min(1, (pointer - (end - edgeSize)) / edgeSize);
		return Math.max(MIN_SPEED_PX_PER_SECOND, MAX_SPEED_PX_PER_SECOND * intensity * intensity);
	}
	return 0;
}

export function useDragAutoScroll({active, axis = 'vertical', getScrollElement}: DragAutoScrollOptions): void {
	const dragDropManager = useDragDropManager();

	useEffect(() => {
		if (!active) return;
		const monitor = dragDropManager.getMonitor();
		let animationFrame: number | null = null;
		let animationWindow: Window | null = null;
		let resizeWindow: Window | null = null;
		let layoutMutationObserver: MutationObserver | null = null;
		let lastFrameTime: number | null = null;
		let scrollElement: HTMLElement | null = null;
		let bounds: VisibleScrollViewportRect | null = null;
		let pointerX = 0;
		let pointerY = 0;

		const stopAnimation = () => {
			if (animationFrame != null && animationWindow != null) {
				animationWindow.cancelAnimationFrame(animationFrame);
			}
			animationFrame = null;
			animationWindow = null;
			lastFrameTime = null;
		};

		const handleResize = () => {
			bounds = null;
			synchronizePointer();
		};

		const updateScrollElement = (element: HTMLElement) => {
			if (element === scrollElement && bounds != null) return;
			const elementChanged = element !== scrollElement;
			scrollElement = element;
			bounds = measureVisibleScrollViewportRect(element);
			const ownerWindow = resolveOwnerWindow(element);
			if (ownerWindow !== resizeWindow) {
				if (resizeWindow != null) {
					resizeWindow.removeEventListener('resize', handleResize);
				}
				resizeWindow = ownerWindow;
				resizeWindow.addEventListener('resize', handleResize);
			}
			if (!elementChanged) return;
			if (layoutMutationObserver != null) {
				layoutMutationObserver.disconnect();
			}
			layoutMutationObserver =
				ownerWindow.MutationObserver == null ? null : new ownerWindow.MutationObserver(handleResize);
			for (let ancestor: HTMLElement | null = element; ancestor != null; ancestor = ancestor.parentElement) {
				if (layoutMutationObserver != null) {
					layoutMutationObserver.observe(ancestor, {attributes: true, attributeFilter: ['class', 'style']});
				}
			}
		};

		const getVelocity = (): number => {
			if (bounds == null) return 0;
			if (axis === 'vertical') {
				if (pointerX < bounds.left || pointerX > bounds.right) return 0;
				return getScrollVelocity(pointerY, bounds.top, bounds.bottom);
			}
			if (pointerY < bounds.top || pointerY > bounds.bottom) return 0;
			return getScrollVelocity(pointerX, bounds.left, bounds.right);
		};

		const runAnimationFrame = (timestamp: number) => {
			animationFrame = null;
			const element = getScrollElement();
			if (element == null || !monitor.isDragging()) {
				stopAnimation();
				return;
			}
			updateScrollElement(element);
			const velocity = getVelocity();
			if (velocity === 0) {
				stopAnimation();
				return;
			}
			const previousFrameTime = lastFrameTime == null ? timestamp - 16 : lastFrameTime;
			const elapsedMs = Math.min(MAX_FRAME_DELTA_MS, Math.max(0, timestamp - previousFrameTime));
			lastFrameTime = timestamp;
			const currentPosition = axis === 'vertical' ? element.scrollTop : element.scrollLeft;
			const scrollSize = axis === 'vertical' ? element.scrollHeight : element.scrollWidth;
			const viewportSize = axis === 'vertical' ? element.clientHeight : element.clientWidth;
			const maximumPosition = Math.max(0, scrollSize - viewportSize);
			const nextPosition = Math.min(maximumPosition, Math.max(0, currentPosition + velocity * (elapsedMs / 1000)));
			if (nextPosition === currentPosition) {
				stopAnimation();
				return;
			}
			if (axis === 'vertical') {
				element.scrollTop = nextPosition;
			} else {
				element.scrollLeft = nextPosition;
			}
			const ownerWindow = resolveOwnerWindow(element);
			animationWindow = ownerWindow;
			animationFrame = ownerWindow.requestAnimationFrame(runAnimationFrame);
		};

		function synchronizePointer(): void {
			if (!monitor.isDragging()) {
				stopAnimation();
				return;
			}
			const clientOffset = monitor.getClientOffset();
			const element = getScrollElement();
			if (clientOffset == null || element == null) {
				stopAnimation();
				return;
			}
			pointerX = clientOffset.x;
			pointerY = clientOffset.y;
			updateScrollElement(element);
			if (getVelocity() === 0) {
				stopAnimation();
				return;
			}
			if (animationFrame == null) {
				const ownerWindow = resolveOwnerWindow(element);
				animationWindow = ownerWindow;
				animationFrame = ownerWindow.requestAnimationFrame(runAnimationFrame);
			}
		}
		const unsubscribeOffset = monitor.subscribeToOffsetChange(synchronizePointer);
		const unsubscribeState = monitor.subscribeToStateChange(synchronizePointer);
		synchronizePointer();
		return () => {
			unsubscribeOffset();
			unsubscribeState();
			if (resizeWindow != null) {
				resizeWindow.removeEventListener('resize', handleResize);
			}
			if (layoutMutationObserver != null) {
				layoutMutationObserver.disconnect();
			}
			stopAnimation();
		};
	}, [active, axis, dragDropManager, getScrollElement]);
}
