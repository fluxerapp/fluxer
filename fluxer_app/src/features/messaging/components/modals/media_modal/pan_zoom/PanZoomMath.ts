// SPDX-License-Identifier: AGPL-3.0-or-later

export const MIN_ZOOM_SCALE = 1;
export const DEFAULT_ZOOM_SCALE = 2.5;
export const MAX_ZOOM_SCALE = 5;
export const ZOOM_STEP = 1.25;
export const ZOOM_STATE_EPSILON = 0.015;
export const TAP_MOVE_THRESHOLD = 20;

export interface Point {
	x: number;
	y: number;
}

export interface PanZoomMetrics {
	viewportWidth: number;
	viewportHeight: number;
	contentWidth: number;
	contentHeight: number;
}

function clamp(value: number, min: number, max: number): number {
	const clamped = Math.min(max, Math.max(min, value));
	return Object.is(clamped, -0) ? 0 : clamped;
}

export function clampScale(value: number, minScale = MIN_ZOOM_SCALE, maxScale = MAX_ZOOM_SCALE): number {
	return clamp(value, minScale, maxScale);
}

export function isDefaultTransform({
	scale,
	x,
	y,
	minScale = MIN_ZOOM_SCALE,
	epsilon = ZOOM_STATE_EPSILON,
}: {
	scale: number;
	x: number;
	y: number;
	minScale?: number;
	epsilon?: number;
}): boolean {
	return Math.abs(scale - minScale) <= epsilon && Math.abs(x) <= epsilon && Math.abs(y) <= epsilon;
}

export function getDistance(a: Point, b: Point): number {
	return Math.hypot(a.x - b.x, a.y - b.y);
}

export function getCentroid(a: Point, b: Point): Point {
	return {
		x: (a.x + b.x) / 2,
		y: (a.y + b.y) / 2,
	};
}

export function getViewportPoint(clientX: number, clientY: number, viewport: DOMRect): Point {
	return {
		x: clientX - viewport.left - viewport.width / 2,
		y: clientY - viewport.top - viewport.height / 2,
	};
}

export function clampPanForScale(point: Point, scale: number, metrics: PanZoomMetrics): Point {
	if (scale <= MIN_ZOOM_SCALE + ZOOM_STATE_EPSILON) {
		return {x: 0, y: 0};
	}
	const maxX = Math.max(0, (metrics.contentWidth * scale - metrics.viewportWidth) / 2);
	const maxY = Math.max(0, (metrics.contentHeight * scale - metrics.viewportHeight) / 2);
	return {
		x: clamp(point.x, -maxX, maxX),
		y: clamp(point.y, -maxY, maxY),
	};
}

export function getAnchoredZoomPoint({
	origin,
	current,
	currentScale,
	nextScale,
	metrics,
}: {
	origin: Point;
	current: Point;
	currentScale: number;
	nextScale: number;
	metrics: PanZoomMetrics;
}): Point {
	if (nextScale <= MIN_ZOOM_SCALE + ZOOM_STATE_EPSILON) {
		return {x: 0, y: 0};
	}
	const scaleRatio = nextScale / Math.max(currentScale, MIN_ZOOM_SCALE);
	return clampPanForScale(
		{
			x: origin.x - (origin.x - current.x) * scaleRatio,
			y: origin.y - (origin.y - current.y) * scaleRatio,
		},
		nextScale,
		metrics,
	);
}
