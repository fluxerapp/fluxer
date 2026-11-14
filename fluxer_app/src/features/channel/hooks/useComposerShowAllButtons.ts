// SPDX-License-Identifier: AGPL-3.0-or-later

import {type RefObject, useLayoutEffect, useState} from 'react';

const SHOW_ALL_BUTTONS_MIN_WIDTH_PX = 500;

function contentBoxWidth(element: HTMLElement): number {
	const style = getComputedStyle(element);
	const paddingLeft = Number.parseFloat(style.paddingLeft) || 0;
	const paddingRight = Number.parseFloat(style.paddingRight) || 0;
	return Math.round(element.clientWidth - paddingLeft - paddingRight);
}

export function useComposerShowAllButtons(containerRef: RefObject<HTMLElement | null>, isMobile: boolean): boolean {
	const [showAllButtons, setShowAllButtons] = useState(true);
	useLayoutEffect(() => {
		if (isMobile) {
			setShowAllButtons(true);
			return;
		}
		const container = containerRef.current;
		if (container === null) return;
		let lastWidth = -1;
		let rafId: number | null = null;
		let pendingWidth: number | null = null;
		const applyWidth = (width: number) => {
			if (width === lastWidth) return;
			lastWidth = width;
			setShowAllButtons(width > SHOW_ALL_BUTTONS_MIN_WIDTH_PX);
		};
		if (container.clientWidth > 0) {
			applyWidth(contentBoxWidth(container));
		}
		if (typeof ResizeObserver === 'undefined') return;
		const applyPendingWidth = () => {
			rafId = null;
			if (pendingWidth === null) return;
			const width = pendingWidth;
			pendingWidth = null;
			applyWidth(width);
		};
		const resizeObserver = new ResizeObserver((entries) => {
			const entry = entries[0];
			if (entry === undefined) return;
			pendingWidth = Math.round(entry.contentRect.width);
			if (rafId === null) {
				rafId = requestAnimationFrame(applyPendingWidth);
			}
		});
		resizeObserver.observe(container);
		return () => {
			if (rafId !== null) {
				cancelAnimationFrame(rafId);
			}
			resizeObserver.disconnect();
		};
	}, [containerRef, isMobile]);
	return showAllButtons;
}
