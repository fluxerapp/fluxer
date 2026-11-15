// SPDX-License-Identifier: AGPL-3.0-or-later

export interface ScrollMetrics {
	scrollTop: number;
	scrollHeight: number;
	offsetHeight: number;
}

export interface ScrollPinOptions {
	tolerance?: number;
	hasMoreAfter?: boolean;
	wasPinned?: boolean;
}

export interface ScrollPinResult {
	distanceFromBottom: number;
	isAtBottom: boolean;
	isPinned: boolean;
}

const DEFAULT_TOLERANCE = 8;
const DEFAULT_STICKY_TOLERANCE = 64;

export function evaluateScrollPinning(metrics: ScrollMetrics, options: ScrollPinOptions = {}): ScrollPinResult {
	const tolerance = options.tolerance ?? DEFAULT_TOLERANCE;
	const hasMoreAfter = options.hasMoreAfter ?? false;
	const wasPinned = options.wasPinned ?? false;
	const distanceFromBottom = Math.max(metrics.scrollHeight - metrics.offsetHeight - metrics.scrollTop, 0);
	const isWithinTolerance = distanceFromBottom <= tolerance;
	const isWithinStickyRange = distanceFromBottom <= DEFAULT_STICKY_TOLERANCE;
	const shouldPin = (isWithinTolerance || (wasPinned && isWithinStickyRange)) && !hasMoreAfter;
	return {
		distanceFromBottom,
		isAtBottom: isWithinTolerance,
		isPinned: shouldPin,
	};
}
