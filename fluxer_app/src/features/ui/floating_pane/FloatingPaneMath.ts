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

interface FlingOptions {
	lookaheadSeconds: number;
	strongVelocity: number;
	axisVelocityFloor: number;
}

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
	return `${isBottom ? 'bottom' : 'top'}-${isRight ? 'right' : 'left'}`;
}

export function pickCornerForFling(point: Point, velocity: Point, bounds: DragBounds, options: FlingOptions): Corner {
	const projectedX = clamp(point.x + velocity.x * options.lookaheadSeconds, bounds.minX, bounds.maxX);
	const projectedY = clamp(point.y + velocity.y * options.lookaheadSeconds, bounds.minY, bounds.maxY);
	const speed = Math.hypot(velocity.x, velocity.y);
	const midX = (bounds.minX + bounds.maxX) / 2;
	const midY = (bounds.minY + bounds.maxY) / 2;
	if (speed >= options.strongVelocity) {
		let horizontal: 'left' | 'right';
		if (Math.abs(velocity.x) >= options.axisVelocityFloor) {
			horizontal = velocity.x >= 0 ? 'right' : 'left';
		} else {
			horizontal = projectedX >= midX ? 'right' : 'left';
		}
		let vertical: 'top' | 'bottom';
		if (Math.abs(velocity.y) >= options.axisVelocityFloor) {
			vertical = velocity.y >= 0 ? 'bottom' : 'top';
		} else {
			vertical = projectedY >= midY ? 'bottom' : 'top';
		}
		return `${vertical}-${horizontal}`;
	}
	return snapPointToCorner({x: projectedX, y: projectedY}, bounds);
}

interface ResizeResult {
	width: number;
	offset: Point;
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
	let fromHorizontal = 0;
	if (edge.includes('right')) {
		fromHorizontal = deltaX;
	} else if (edge.includes('left')) {
		fromHorizontal = -deltaX;
	}
	let fromVertical = 0;
	if (edge.includes('bottom')) {
		fromVertical = deltaY * aspectRatio;
	} else if (edge.includes('top')) {
		fromVertical = -deltaY * aspectRatio;
	}
	let rawWidthDelta: number;
	if (edge === 'top' || edge === 'bottom') {
		rawWidthDelta = fromVertical;
	} else if (edge === 'left' || edge === 'right') {
		rawWidthDelta = fromHorizontal;
	} else {
		rawWidthDelta = fromHorizontal + fromVertical;
	}
	const nextWidth = clampWidth(startWidth + rawWidthDelta, geometry);
	const appliedWidthDelta = nextWidth - startWidth;
	const appliedHeightDelta = appliedWidthDelta / aspectRatio;
	const offsetXDelta = edge.includes('left') ? -appliedWidthDelta : 0;
	const offsetYDelta = edge.includes('top') ? -appliedHeightDelta : 0;
	const bounds = getDragBounds(geometry, nextWidth);
	const offset = clampPoint({x: paneStartX + offsetXDelta, y: paneStartY + offsetYDelta}, bounds);
	return {width: nextWidth, offset};
}

interface FreeResizeRect {
	x: number;
	y: number;
	width: number;
	height: number;
}

interface FreeResizeBounds {
	left: number;
	top: number;
	right: number;
	bottom: number;
}

interface FreeResizeStart {
	edge: ResizeEdge;
	pointerStartX: number;
	pointerStartY: number;
	rect: FreeResizeRect;
}

function resizeAxis(
	start: number,
	size: number,
	delta: number,
	growsAtStart: boolean,
	boundsMin: number,
	boundsMax: number,
	minSize: number,
): {start: number; size: number} {
	const end = start + size;
	if (growsAtStart) {
		const nextStart = clamp(start + delta, boundsMin, Math.max(boundsMin, end - minSize));
		return {start: nextStart, size: end - nextStart};
	}
	const nextEnd = clamp(end + delta, Math.min(boundsMax, start + minSize), boundsMax);
	return {start, size: nextEnd - start};
}

export function computeFreeResize(
	start: FreeResizeStart,
	pointerX: number,
	pointerY: number,
	bounds: FreeResizeBounds,
	minSize: Size,
): FreeResizeRect {
	const {edge, rect} = start;
	let horizontal = {start: rect.x, size: rect.width};
	if (edge.includes('left') || edge.includes('right')) {
		horizontal = resizeAxis(
			rect.x,
			rect.width,
			pointerX - start.pointerStartX,
			edge.includes('left'),
			bounds.left,
			bounds.right,
			minSize.width,
		);
	}
	let vertical = {start: rect.y, size: rect.height};
	if (edge.includes('top') || edge.includes('bottom')) {
		vertical = resizeAxis(
			rect.y,
			rect.height,
			pointerY - start.pointerStartY,
			edge.includes('top'),
			bounds.top,
			bounds.bottom,
			minSize.height,
		);
	}
	return {x: horizontal.start, y: vertical.start, width: horizontal.size, height: vertical.size};
}
