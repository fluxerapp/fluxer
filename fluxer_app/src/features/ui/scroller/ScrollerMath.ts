// SPDX-License-Identifier: AGPL-3.0-or-later

export type ScrollAxis = 'vertical' | 'horizontal';

export interface AxisScrollMetrics {
	viewportSize: number;
	contentSize: number;
	scrollPosition: number;
	maxScrollPosition: number;
}

export interface ScrollbarThumbState {
	hasTrack: boolean;
	thumbSize: number;
	thumbOffset: number;
	trackSize: number;
	maxScrollPosition: number;
}

export function clamp(value: number, minValue: number, maxValue: number): number {
	return Math.max(minValue, Math.min(value, maxValue));
}

export function getAxisScrollMetrics(element: HTMLElement, orientation: ScrollAxis): AxisScrollMetrics {
	if (orientation === 'vertical') {
		const viewportSize = element.clientHeight;
		const contentSize = element.scrollHeight;
		const scrollPosition = element.scrollTop;
		const maxScrollPosition = Math.max(0, contentSize - viewportSize);
		return {
			viewportSize,
			contentSize,
			scrollPosition,
			maxScrollPosition,
		};
	}
	const viewportSize = element.clientWidth;
	const contentSize = element.scrollWidth;
	const scrollPosition = element.scrollLeft;
	const maxScrollPosition = Math.max(0, contentSize - viewportSize);
	return {
		viewportSize,
		contentSize,
		scrollPosition,
		maxScrollPosition,
	};
}

export function calculateThumbState(
	metrics: AxisScrollMetrics,
	minThumbSize: number,
	forceTrack: boolean,
	trackSizeOverride?: number,
): ScrollbarThumbState {
	const hasOverflow = metrics.maxScrollPosition > 0;
	const hasTrack = forceTrack || hasOverflow;
	const trackSize =
		trackSizeOverride != null && Number.isFinite(trackSizeOverride)
			? Math.max(0, trackSizeOverride)
			: metrics.viewportSize;
	if (!hasTrack || trackSize <= 0) {
		return {
			hasTrack: false,
			thumbSize: 0,
			thumbOffset: 0,
			trackSize,
			maxScrollPosition: metrics.maxScrollPosition,
		};
	}
	if (!hasOverflow) {
		return {
			hasTrack: true,
			thumbSize: trackSize,
			thumbOffset: 0,
			trackSize,
			maxScrollPosition: metrics.maxScrollPosition,
		};
	}
	const proportionalThumbSize = (metrics.viewportSize / metrics.contentSize) * trackSize;
	const thumbSize = clamp(proportionalThumbSize, minThumbSize, trackSize);
	const travelRange = Math.max(0, trackSize - thumbSize);
	const scrollRatio = metrics.maxScrollPosition === 0 ? 0 : metrics.scrollPosition / metrics.maxScrollPosition;
	const thumbOffset = travelRange * clamp(scrollRatio, 0, 1);
	return {
		hasTrack: true,
		thumbSize,
		thumbOffset,
		trackSize,
		maxScrollPosition: metrics.maxScrollPosition,
	};
}

export function getAxisPointerPosition(clientX: number, clientY: number, orientation: ScrollAxis): number {
	if (orientation === 'vertical') {
		return clientY;
	}
	return clientX;
}

export function setAxisScrollPosition(element: HTMLElement, orientation: ScrollAxis, value: number): void {
	if (orientation === 'vertical') {
		element.scrollTop = value;
		return;
	}
	element.scrollLeft = value;
}
