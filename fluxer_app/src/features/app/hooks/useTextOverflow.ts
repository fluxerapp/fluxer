// SPDX-License-Identifier: AGPL-3.0-or-later

import {observeResize} from '@app/features/platform/utils/SharedResizeObserver';
import {type RefObject, useEffect, useState} from 'react';

type OverflowAxis = 'horizontal' | 'vertical' | 'both';

function getElementOverflowState(el: HTMLElement, axis: OverflowAxis): boolean {
	return (
		(axis !== 'vertical' && el.scrollWidth - el.clientWidth > 1) ||
		(axis !== 'horizontal' && el.scrollHeight - el.clientHeight > 1)
	);
}

function useOverflow(target: HTMLElement | RefObject<HTMLElement | null> | null, axis: OverflowAxis): boolean {
	const [isOverflowing, setIsOverflowing] = useState(false);
	useEffect(() => {
		const element = target && 'current' in target ? target.current : target;
		const ownerWindow = element?.ownerDocument.defaultView;
		if (!element || !ownerWindow) {
			setIsOverflowing(false);
			return;
		}
		let rafId: number | null = null;
		let disposed = false;
		const checkOverflow = () => {
			rafId = null;
			const overflowing = getElementOverflowState(element, axis);
			setIsOverflowing((prev) => (prev === overflowing ? prev : overflowing));
		};
		const scheduleCheckOverflow = () => {
			if (disposed || rafId != null) return;
			rafId = ownerWindow.requestAnimationFrame(checkOverflow);
		};
		scheduleCheckOverflow();
		const unobserve = typeof ResizeObserver === 'undefined' ? undefined : observeResize(element, scheduleCheckOverflow);
		return () => {
			disposed = true;
			if (rafId != null) {
				ownerWindow.cancelAnimationFrame(rafId);
			}
			unobserve?.();
		};
	}, [target, axis]);
	return isOverflowing;
}

export function useElementOverflow(element: HTMLElement | null, axis: OverflowAxis = 'horizontal'): boolean {
	return useOverflow(element, axis);
}

export function useTextOverflow(ref: RefObject<HTMLElement | null>, axis: OverflowAxis = 'horizontal'): boolean {
	return useOverflow(ref, axis);
}
