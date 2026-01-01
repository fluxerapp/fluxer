/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

export interface ScrollMetrics {
	scrollTop: number;
	scrollHeight: number;
	offsetHeight: number;
}

export interface ScrollPinOptions {
	tolerance?: number;
	stickyTolerance?: number;
	hasMoreAfter?: boolean;
	wasPinned?: boolean;
	allowPinWhenHasMoreAfter?: boolean;
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
	const stickyTolerance = options.stickyTolerance ?? DEFAULT_STICKY_TOLERANCE;
	const hasMoreAfter = options.hasMoreAfter ?? false;
	const allowPinWhenHasMoreAfter = options.allowPinWhenHasMoreAfter ?? true;
	const wasPinned = options.wasPinned ?? false;

	const distanceFromBottom = Math.max(metrics.scrollHeight - metrics.offsetHeight - metrics.scrollTop, 0);
	const isWithinTolerance = distanceFromBottom <= tolerance;
	const isWithinStickyRange = distanceFromBottom <= stickyTolerance;

	const shouldPin =
		(isWithinTolerance || (wasPinned && isWithinStickyRange)) && (allowPinWhenHasMoreAfter || !hasMoreAfter);

	return {
		distanceFromBottom,
		isAtBottom: isWithinTolerance,
		isPinned: shouldPin,
	};
}
