// SPDX-License-Identifier: AGPL-3.0-or-later

import {RelativePosition} from '@app/features/ui/RelativePosition';

export interface VerticalDropPoint {
	y: number;
}

export interface VerticalDropRect {
	top: number;
	bottom: number;
}

export const VerticalDropZone = Object.freeze({
	CENTER: 'center',
} as const);

export type VerticalDropZone = (typeof VerticalDropZone)[keyof typeof VerticalDropZone];
export type ResolvedVerticalDropZone = VerticalDropZone | RelativePosition;

export function getVerticalDropTargetHeight(rect: VerticalDropRect): number {
	return rect.bottom - rect.top;
}

function getVerticalDropOffset(point: VerticalDropPoint, rect: VerticalDropRect): number {
	const height = getVerticalDropTargetHeight(rect);
	return Math.min(height, Math.max(0, point.y - rect.top));
}

export function resolveVerticalDropEdge(point: VerticalDropPoint, rect: VerticalDropRect): RelativePosition {
	if (getVerticalDropOffset(point, rect) < getVerticalDropTargetHeight(rect) / 2) {
		return RelativePosition.BEFORE;
	}
	return RelativePosition.AFTER;
}

export function resolveVerticalDropZone(
	point: VerticalDropPoint,
	rect: VerticalDropRect,
	edgeThreshold: number,
): ResolvedVerticalDropZone {
	if (edgeThreshold >= 0.5) {
		return resolveVerticalDropEdge(point, rect);
	}
	const height = getVerticalDropTargetHeight(rect);
	const offset = getVerticalDropOffset(point, rect);
	const threshold = height * edgeThreshold;
	if (offset < threshold) {
		return RelativePosition.BEFORE;
	}
	if (offset > height - threshold) {
		return RelativePosition.AFTER;
	}
	return VerticalDropZone.CENTER;
}
