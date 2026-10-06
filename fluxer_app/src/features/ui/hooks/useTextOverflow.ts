// SPDX-License-Identifier: AGPL-3.0-or-later

import {observeResize} from '@app/features/platform/utils/SharedResizeObserver';
import {type RefObject, useEffect, useState} from 'react';

interface UseTextOverflowOptions {
	content: string | null | undefined;
	checkVertical?: boolean;
	measureTextRange?: boolean;
}

const OVERFLOW_EPSILON_PX = 1;

function isMeaningfullyGreater(measuredSize: number, availableSize: number): boolean {
	return measuredSize - availableSize > OVERFLOW_EPSILON_PX;
}

function getAvailableInlineSize(element: HTMLElement): number {
	const clientWidth = element.clientWidth;
	if (clientWidth > 0) {
		return clientWidth;
	}
	return element.getBoundingClientRect().width;
}

function measureRangeInlineSize(element: HTMLElement): number {
	const range = element.ownerDocument.createRange();
	try {
		range.selectNodeContents(element);
		return range.getBoundingClientRect().width;
	} finally {
		range.detach?.();
	}
}

function shouldMeasureNaturalInlineSize(element: HTMLElement): boolean {
	const computedStyle = element.ownerDocument.defaultView?.getComputedStyle(element);
	if (!computedStyle) return false;
	return (
		computedStyle.textOverflow !== 'clip' || computedStyle.whiteSpace === 'nowrap' || computedStyle.whiteSpace === 'pre'
	);
}

function measureNaturalInlineSize(element: HTMLElement): number | null {
	const ownerDocument = element.ownerDocument;
	if (!ownerDocument.body || !shouldMeasureNaturalInlineSize(element)) return null;
	const clone = element.cloneNode(true) as HTMLElement;
	const computedStyle = ownerDocument.defaultView?.getComputedStyle(element);
	if (computedStyle) {
		clone.style.font = computedStyle.font;
		clone.style.fontKerning = computedStyle.fontKerning;
		clone.style.letterSpacing = computedStyle.letterSpacing;
		clone.style.lineHeight = computedStyle.lineHeight;
		clone.style.textTransform = computedStyle.textTransform;
		clone.style.wordSpacing = computedStyle.wordSpacing;
	}
	Object.assign(clone.style, {
		position: 'absolute',
		left: '-100000px',
		top: '-100000px',
		display: 'inline-block',
		width: 'auto',
		minWidth: '0',
		maxWidth: 'none',
		height: 'auto',
		maxHeight: 'none',
		overflow: 'visible',
		overflowX: 'visible',
		overflowY: 'visible',
		whiteSpace: 'nowrap',
		textOverflow: 'clip',
		visibility: 'hidden',
		pointerEvents: 'none',
		zIndex: '-1',
	} satisfies Partial<CSSStyleDeclaration>);
	ownerDocument.body.appendChild(clone);
	try {
		return Math.max(clone.scrollWidth, clone.getBoundingClientRect().width);
	} finally {
		clone.remove();
	}
}

export function useTextOverflow(
	elementRef: RefObject<HTMLElement | null>,
	{content, checkVertical = false, measureTextRange = false}: UseTextOverflowOptions,
): boolean {
	const [isOverflowing, setIsOverflowing] = useState(false);
	useEffect(() => {
		const element = elementRef.current;
		const ownerWindow = element?.ownerDocument.defaultView;
		if (!element || !ownerWindow || !content) {
			setIsOverflowing(false);
			return;
		}
		let frameId: number | null = null;
		let disposed = false;
		const updateOverflowing = (next: boolean) => {
			setIsOverflowing((previous) => (previous === next ? previous : next));
		};
		const checkOverflow = () => {
			frameId = null;
			if (
				isMeaningfullyGreater(element.scrollWidth, element.clientWidth) ||
				(checkVertical && isMeaningfullyGreater(element.scrollHeight, element.clientHeight))
			) {
				updateOverflowing(true);
				return;
			}
			if (measureTextRange) {
				const availableWidth = getAvailableInlineSize(element);
				if (isMeaningfullyGreater(measureRangeInlineSize(element), availableWidth)) {
					updateOverflowing(true);
					return;
				}
				const naturalInlineSize = measureNaturalInlineSize(element);
				updateOverflowing(naturalInlineSize != null && isMeaningfullyGreater(naturalInlineSize, availableWidth));
				return;
			}
			updateOverflowing(false);
		};
		const scheduleOverflowCheck = () => {
			if (disposed || frameId != null) {
				return;
			}
			frameId = ownerWindow.requestAnimationFrame(checkOverflow);
		};
		scheduleOverflowCheck();
		const unobserveResize =
			typeof ResizeObserver !== 'undefined' ? observeResize(element, scheduleOverflowCheck) : undefined;
		let mutationObserver: MutationObserver | null = null;
		if (typeof ownerWindow.MutationObserver !== 'undefined') {
			mutationObserver = new ownerWindow.MutationObserver(scheduleOverflowCheck);
			mutationObserver.observe(element, {
				attributes: true,
				attributeFilter: ['alt', 'class', 'src', 'style'],
				childList: true,
				characterData: true,
				subtree: true,
			});
		}
		element.addEventListener('load', scheduleOverflowCheck, true);
		const fontSet = element.ownerDocument.fonts;
		fontSet?.ready?.then(scheduleOverflowCheck);
		fontSet?.addEventListener?.('loadingdone', scheduleOverflowCheck);
		return () => {
			disposed = true;
			if (frameId != null) {
				ownerWindow.cancelAnimationFrame(frameId);
			}
			unobserveResize?.();
			mutationObserver?.disconnect();
			element.removeEventListener('load', scheduleOverflowCheck, true);
			fontSet?.removeEventListener?.('loadingdone', scheduleOverflowCheck);
		};
	}, [checkVertical, content, elementRef, measureTextRange]);
	return isOverflowing;
}
