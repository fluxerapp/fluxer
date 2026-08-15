// SPDX-License-Identifier: AGPL-3.0-or-later

const SCROLLER_TRACK_BLOCK_END_INSET_VARIABLE = '--scroller-track-block-end-inset';
const RESOLVED_LENGTH_VARIABLE = '--resolved-scroll-viewport-length';
const CSS_PIXEL_VALUE_PATTERN = /^(-?(?:\d+(?:\.\d+)?|\.\d+))px$/;

export interface VisibleScrollViewportRect {
	top: number;
	right: number;
	bottom: number;
	left: number;
	bottomInset: number;
}

function resolveOwnerWindow(element: HTMLElement): Window & typeof globalThis {
	const ownerWindow = element.ownerDocument.defaultView;
	if (ownerWindow == null) {
		throw new Error('Scroll viewport element has no owner window');
	}
	return ownerWindow;
}

function resolveCSSPixelLength(element: HTMLElement, propertyName: string): number {
	const ownerWindow = resolveOwnerWindow(element);
	const computedStyle = ownerWindow.getComputedStyle(element);
	const source = computedStyle.getPropertyValue(propertyName).trim();
	const pixelMatch = CSS_PIXEL_VALUE_PATTERN.exec(source);
	if (pixelMatch != null) {
		const value = Number(pixelMatch[1]);
		return Number.isFinite(value) ? value : 0;
	}
	if (source === '' || source === '0') return 0;
	const probe = element.ownerDocument.createElement('div');
	probe.style.position = 'fixed';
	probe.style.visibility = 'hidden';
	probe.style.pointerEvents = 'none';
	probe.style.boxSizing = 'border-box';
	probe.style.width = '0';
	probe.style.height = `var(${RESOLVED_LENGTH_VARIABLE})`;
	probe.style.fontSize = computedStyle.fontSize;
	probe.style.setProperty(RESOLVED_LENGTH_VARIABLE, source);
	element.ownerDocument.documentElement.appendChild(probe);
	try {
		const value = probe.getBoundingClientRect().height;
		return Number.isFinite(value) ? value : 0;
	} finally {
		probe.remove();
	}
}

export function resolveScrollViewportBottomInset(element: HTMLElement): number {
	return Math.min(
		element.clientHeight,
		Math.max(0, resolveCSSPixelLength(element, SCROLLER_TRACK_BLOCK_END_INSET_VARIABLE)),
	);
}

export function resolveVisibleScrollViewportHeight(element: HTMLElement): number {
	return Math.max(0, element.clientHeight - resolveScrollViewportBottomInset(element));
}

export function measureVisibleScrollViewportRect(element: HTMLElement): VisibleScrollViewportRect {
	const rect = element.getBoundingClientRect();
	const bottomInset = resolveScrollViewportBottomInset(element);
	return {
		top: rect.top,
		right: rect.right,
		bottom: Math.max(rect.top, rect.bottom - bottomInset),
		left: rect.left,
		bottomInset,
	};
}
