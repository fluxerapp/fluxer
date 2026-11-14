// SPDX-License-Identifier: AGPL-3.0-or-later

import {observeResize} from '@app/features/platform/utils/SharedResizeObserver';
import {useEffect, useState} from 'react';

type OverflowAxis = 'horizontal' | 'vertical' | 'both';

function getElementOverflowState(el: HTMLElement, axis: OverflowAxis): boolean {
	return (
		(axis !== 'vertical' && el.scrollWidth - el.clientWidth > 1) ||
		(axis !== 'horizontal' && el.scrollHeight - el.clientHeight > 1)
	);
}

export function useElementOverflow(element: HTMLElement | null, axis: OverflowAxis = 'horizontal'): boolean {
	const [isOverflowing, setIsOverflowing] = useState(false);
	useEffect(() => {
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
	}, [element, axis]);
	return isOverflowing;
}
