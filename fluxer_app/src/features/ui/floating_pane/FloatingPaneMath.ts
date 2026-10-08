// SPDX-License-Identifier: AGPL-3.0-or-later

export type Corner = 'top-left' | 'top-right' | 'bottom-right' | 'bottom-left';
export type ResizeEdge =
	| 'top'
	| 'bottom'
	| 'left'
	| 'right'
	| 'top-left'
	| 'top-right'
	| 'bottom-left'
	| 'bottom-right';
export const ALL_RESIZE_EDGES: ReadonlyArray<ResizeEdge> = [
	'top',
	'bottom',
	'left',
	'right',
	'top-left',
	'top-right',
	'bottom-left',
	'bottom-right',
];

interface Size {
	width: number;
	height: number;
}

export interface Point {
	x: number;
	y: number;
}

export interface DragBounds {
	minX: number;
	maxX: number;
	minY: number;
	maxY: number;
}

export interface FloatingPaneGeometry {
	viewport: Size;
	edgePadding: number;
	topInset: number;
	aspectRatio: number;
	minWidth: number;
	maxWidth: number;
}

export interface ResizeStart {
	edge: ResizeEdge;
	startWidth: number;
	pointerStartX: number;
	pointerStartY: number;
	paneStartX: number;
	paneStartY: number;
}

export interface FlingOptions {
	lookaheadSeconds: number;
	strongVelocity: number;
	axisVelocityFloor: number;
}

const DEFAULT_FLING_OPTIONS: FlingOptions = {
	lookaheadSeconds: 0.2,
	strongVelocity: 550,
	axisVelocityFloor: 180,
};

function isFiniteNumber(value: number): boolean {
	return typeof value === 'number' && Number.isFinite(value);
}

function clamp(value: number, min: number, max: number): number {
	if (!isFiniteNumber(value)) return min;
	if (max < min) return min;
	return Math.max(min, Math.min(max, value));
}

export function getPaneHeight(width: number, aspectRatio: number): number {
	if (!isFiniteNumber(width) || !isFiniteNumber(aspectRatio) || aspectRatio <= 0) return 0;
	return Math.round(width / aspectRatio);
}

interface WidthRange {
	min: number;
	max: number;
}

function getEffectiveWidthRange(geometry: FloatingPaneGeometry): WidthRange {
	const {viewport, edgePadding, topInset, aspectRatio, minWidth, maxWidth} = geometry;
	const availableWidth = Math.max(0, viewport.width - edgePadding * 2);
	const availableHeight = Math.max(0, viewport.height - edgePadding * 2 - topInset);
	const maxByWidth = availableWidth;
	const maxByHeight = availableHeight * aspectRatio;
	const ceiling = Math.max(0, Math.min(maxWidth, maxByWidth, maxByHeight));
	const floor = Math.max(0, Math.min(minWidth, ceiling));
	return {min: floor, max: ceiling};
}

export function clampWidth(width: number, geometry: FloatingPaneGeometry): number {
	const {min, max} = getEffectiveWidthRange(geometry);
	if (!isFiniteNumber(width)) return min;
	return clamp(Math.round(width), min, max);
}

export function getDragBounds(geometry: FloatingPaneGeometry, width: number): DragBounds {
	const {viewport, edgePadding, topInset, aspectRatio} = geometry;
	const height = getPaneHeight(width, aspectRatio);
	const minX = edgePadding;
	const minY = topInset + edgePadding;
	const maxX = Math.max(minX, viewport.width - width - edgePadding);
	const maxY = Math.max(minY, viewport.height - height - edgePadding);
	return {minX, maxX, minY, maxY};
}

function clampPoint(point: Point, bounds: DragBounds): Point {
	return {
		x: clamp(point.x, bounds.minX, bounds.maxX),
		y: clamp(point.y, bounds.minY, bounds.maxY),
	};
}

export function getCornerPoint(corner: Corner, bounds: DragBounds): Point {
	switch (corner) {
		case 'top-left':
			return {x: bounds.minX, y: bounds.minY};
		case 'top-right':
			return {x: bounds.maxX, y: bounds.minY};
		case 'bottom-right':
			return {x: bounds.maxX, y: bounds.maxY};
		case 'bottom-left':
			return {x: bounds.minX, y: bounds.maxY};
	}
}

function snapPointToCorner(point: Point, bounds: DragBounds): Corner {
	const midX = (bounds.minX + bounds.maxX) / 2;
	const midY = (bounds.minY + bounds.maxY) / 2;
	const isRight = point.x >= midX;
	const isBottom = point.y >= midY;
	return `${isBottom ? 'bottom' : 'top'}-${isRight ? 'right' : 'left'}` as Corner;
}

export function pickCornerForFling(
	point: Point,
	velocity: Point,
	bounds: DragBounds,
	options: FlingOptions = DEFAULT_FLING_OPTIONS,
): Corner {
	const projectedX = clamp(point.x + velocity.x * options.lookaheadSeconds, bounds.minX, bounds.maxX);
	const projectedY = clamp(point.y + velocity.y * options.lookaheadSeconds, bounds.minY, bounds.maxY);
	const speed = Math.hypot(velocity.x, velocity.y);
	const midX = (bounds.minX + bounds.maxX) / 2;
	const midY = (bounds.minY + bounds.maxY) / 2;
	if (speed >= options.strongVelocity) {
		const horizontal =
			Math.abs(velocity.x) >= options.axisVelocityFloor
				? velocity.x >= 0
					? 'right'
					: 'left'
				: projectedX >= midX
					? 'right'
					: 'left';
		const vertical =
			Math.abs(velocity.y) >= options.axisVelocityFloor
				? velocity.y >= 0
					? 'bottom'
					: 'top'
				: projectedY >= midY
					? 'bottom'
					: 'top';
		return `${vertical}-${horizontal}` as Corner;
	}
	return snapPointToCorner({x: projectedX, y: projectedY}, bounds);
}

export interface ResizeResult {
	width: number;
	offset: Point;
}

function computeRawWidthDelta(edge: ResizeEdge, deltaX: number, deltaY: number, aspectRatio: number): number {
	const fromHorizontal = edge.includes('right') ? deltaX : edge.includes('left') ? -deltaX : 0;
	const fromVertical = edge.includes('bottom')
		? deltaY * aspectRatio
		: edge.includes('top')
			? -deltaY * aspectRatio
			: 0;
	if (edge === 'top' || edge === 'bottom') return fromVertical;
	if (edge === 'left' || edge === 'right') return fromHorizontal;
	return fromHorizontal + fromVertical;
}

export function computeResize(
	start: ResizeStart,
	pointerX: number,
	pointerY: number,
	geometry: FloatingPaneGeometry,
): ResizeResult {
	const {edge, startWidth, pointerStartX, pointerStartY, paneStartX, paneStartY} = start;
	const {aspectRatio} = geometry;
	const deltaX = pointerX - pointerStartX;
	const deltaY = pointerY - pointerStartY;
	const rawWidthDelta = computeRawWidthDelta(edge, deltaX, deltaY, aspectRatio);
	const nextWidth = clampWidth(startWidth + rawWidthDelta, geometry);
	const appliedWidthDelta = nextWidth - startWidth;
	const appliedHeightDelta = appliedWidthDelta / aspectRatio;
	const offsetXDelta = edge.includes('left') ? -appliedWidthDelta : 0;
	const offsetYDelta = edge.includes('top') ? -appliedHeightDelta : 0;
	const bounds = getDragBounds(geometry, nextWidth);
	const offset = clampPoint({x: paneStartX + offsetXDelta, y: paneStartY + offsetYDelta}, bounds);
	return {width: nextWidth, offset};
}
