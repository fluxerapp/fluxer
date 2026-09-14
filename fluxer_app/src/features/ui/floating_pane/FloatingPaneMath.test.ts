// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	ALL_CORNERS,
	ALL_RESIZE_EDGES,
	type Corner,
	clampPoint,
	clampWidth,
	computeResize,
	DEFAULT_FLING_OPTIONS,
	type FloatingPaneGeometry,
	getCornerPoint,
	getDragBounds,
	getEffectiveWidthRange,
	getPaneHeight,
	pickCornerForFling,
	type ResizeEdge,
	reconcileToGeometry,
	snapPointToCorner,
} from '@app/features/ui/floating_pane/FloatingPaneMath';
import {describe, expect, it} from 'vitest';

const BASE_GEOMETRY: FloatingPaneGeometry = {
	viewport: {width: 1000, height: 600},
	edgePadding: 12,
	topInset: 0,
	aspectRatio: 16 / 9,
	minWidth: 180,
	maxWidth: 420,
};

function withViewport(
	width: number,
	height: number,
	overrides: Partial<FloatingPaneGeometry> = {},
): FloatingPaneGeometry {
	return {...BASE_GEOMETRY, viewport: {width, height}, ...overrides};
}

describe('getPaneHeight', () => {
	it('rounds width / aspect ratio', () => {
		expect(getPaneHeight(320, 16 / 9)).toBe(180);
		expect(getPaneHeight(200, 16 / 9)).toBe(113);
	});
	it('returns 0 for invalid input', () => {
		expect(getPaneHeight(Number.NaN, 16 / 9)).toBe(0);
		expect(getPaneHeight(320, 0)).toBe(0);
		expect(getPaneHeight(320, -1)).toBe(0);
	});
});

describe('getEffectiveWidthRange', () => {
	it('caps by maxWidth when viewport is generous', () => {
		const range = getEffectiveWidthRange(withViewport(2000, 1200));
		expect(range.max).toBe(420);
		expect(range.min).toBe(180);
	});
	it('caps by viewport width when viewport is narrow', () => {
		const range = getEffectiveWidthRange(withViewport(300, 600));
		expect(range.max).toBe(300 - 12 * 2);
	});
	it('caps by viewport height converted through aspect ratio when short', () => {
		const range = getEffectiveWidthRange(withViewport(1000, 120));
		const expected = (120 - 12 * 2) * (16 / 9);
		expect(range.max).toBeCloseTo(expected, 5);
	});
	it('pulls min down to max when room is tighter than min', () => {
		const range = getEffectiveWidthRange(withViewport(120, 120));
		expect(range.min).toBeLessThanOrEqual(range.max);
		expect(range.min).toBe(Math.max(0, Math.min(180, range.max)));
	});
	it('returns zero range when viewport is empty', () => {
		const range = getEffectiveWidthRange(withViewport(0, 0));
		expect(range.min).toBe(0);
		expect(range.max).toBe(0);
	});
	it('respects topInset by subtracting from available height', () => {
		const withInset = getEffectiveWidthRange(withViewport(1000, 200, {topInset: 100}));
		const sameHeightNoInset = getEffectiveWidthRange(withViewport(1000, 100));
		expect(withInset.max).toBeCloseTo(sameHeightNoInset.max, 5);
	});
});

describe('clampWidth', () => {
	it('rounds within range', () => {
		expect(clampWidth(220.4, BASE_GEOMETRY)).toBe(220);
		expect(clampWidth(220.6, BASE_GEOMETRY)).toBe(221);
	});
	it('clamps below min', () => {
		expect(clampWidth(50, BASE_GEOMETRY)).toBe(180);
	});
	it('clamps above max', () => {
		expect(clampWidth(9999, BASE_GEOMETRY)).toBe(420);
	});
	it('returns min for non-finite input', () => {
		expect(clampWidth(Number.NaN, BASE_GEOMETRY)).toBe(180);
		expect(clampWidth(Number.POSITIVE_INFINITY, BASE_GEOMETRY)).toBe(180);
		expect(clampWidth(Number.NEGATIVE_INFINITY, BASE_GEOMETRY)).toBe(180);
	});
});

describe('getDragBounds', () => {
	it('produces minX/minY at edgePadding (+topInset)', () => {
		const bounds = getDragBounds(withViewport(1000, 600, {topInset: 24}), 320);
		expect(bounds.minX).toBe(12);
		expect(bounds.minY).toBe(36);
	});
	it('produces maxX/maxY so that pane is inset by edgePadding on opposite side', () => {
		const bounds = getDragBounds(BASE_GEOMETRY, 320);
		const expectedHeight = getPaneHeight(320, 16 / 9);
		expect(bounds.maxX).toBe(1000 - 320 - 12);
		expect(bounds.maxY).toBe(600 - expectedHeight - 12);
	});
	it('collapses to minX = maxX when pane fills width', () => {
		const bounds = getDragBounds(withViewport(300, 600), 320);
		expect(bounds.maxX).toBe(bounds.minX);
	});
});

describe('clampPoint', () => {
	it('clamps each axis independently', () => {
		const bounds = {minX: 10, maxX: 100, minY: 5, maxY: 50};
		expect(clampPoint({x: -50, y: 999}, bounds)).toEqual({x: 10, y: 50});
		expect(clampPoint({x: 50, y: 25}, bounds)).toEqual({x: 50, y: 25});
	});
});

describe('getCornerPoint / snapPointToCorner', () => {
	const bounds = {minX: 10, maxX: 90, minY: 20, maxY: 80};
	it('returns the matching anchor point for each corner', () => {
		expect(getCornerPoint('top-left', bounds)).toEqual({x: 10, y: 20});
		expect(getCornerPoint('top-right', bounds)).toEqual({x: 90, y: 20});
		expect(getCornerPoint('bottom-right', bounds)).toEqual({x: 90, y: 80});
		expect(getCornerPoint('bottom-left', bounds)).toEqual({x: 10, y: 80});
	});
	it('snaps a point to its nearest corner by quadrant', () => {
		expect(snapPointToCorner({x: 15, y: 25}, bounds)).toBe('top-left');
		expect(snapPointToCorner({x: 85, y: 25}, bounds)).toBe('top-right');
		expect(snapPointToCorner({x: 85, y: 75}, bounds)).toBe('bottom-right');
		expect(snapPointToCorner({x: 15, y: 75}, bounds)).toBe('bottom-left');
	});
	it('snaps exactly on the midline towards the right/bottom side', () => {
		const midX = (bounds.minX + bounds.maxX) / 2;
		const midY = (bounds.minY + bounds.maxY) / 2;
		expect(snapPointToCorner({x: midX, y: midY}, bounds)).toBe('bottom-right');
	});
	it('round-trips corner -> point -> corner', () => {
		for (const corner of ALL_CORNERS) {
			const point = getCornerPoint(corner, bounds);
			expect(snapPointToCorner(point, bounds)).toBe(corner);
		}
	});
});

describe('pickCornerForFling', () => {
	const bounds = {minX: 0, maxX: 100, minY: 0, maxY: 100};
	it('falls back to nearest-corner snap below strong threshold', () => {
		const corner = pickCornerForFling({x: 20, y: 30}, {x: 0, y: 0}, bounds);
		expect(corner).toBe('top-left');
	});
	it('uses fling direction with strong horizontal velocity', () => {
		const corner = pickCornerForFling({x: 40, y: 40}, {x: 1200, y: 0}, bounds);
		expect(corner.endsWith('right')).toBe(true);
	});
	it('uses fling direction with strong vertical velocity', () => {
		const corner = pickCornerForFling({x: 40, y: 40}, {x: 0, y: -1200}, bounds);
		expect(corner.startsWith('top')).toBe(true);
	});
	it('respects axis velocity floor when fling is diagonal but one axis weak', () => {
		const corner = pickCornerForFling({x: 90, y: 50}, {x: -50, y: -1200}, bounds, {
			...DEFAULT_FLING_OPTIONS,
			axisVelocityFloor: 200,
		});
		expect(corner).toBe('top-right');
	});
});

describe('computeResize', () => {
	const geometry = withViewport(1000, 600);
	function startState(edge: ResizeEdge, opts: {startWidth?: number; paneStartX?: number; paneStartY?: number} = {}) {
		const startWidth = opts.startWidth ?? 240;
		return {
			edge,
			startWidth,
			pointerStartX: 500,
			pointerStartY: 300,
			paneStartX: opts.paneStartX ?? 12,
			paneStartY: opts.paneStartY ?? 12,
		};
	}
	it('grows from right edge without moving anchor', () => {
		const start = startState('right');
		const result = computeResize(start, 540, 300, geometry);
		expect(result.width).toBeGreaterThan(start.startWidth);
		expect(result.offset).toEqual({x: 12, y: 12});
	});
	it('grows from bottom edge without moving anchor', () => {
		const start = startState('bottom');
		const result = computeResize(start, 500, 340, geometry);
		expect(result.width).toBeGreaterThan(start.startWidth);
		expect(result.offset.x).toBe(12);
		expect(result.offset.y).toBe(12);
	});
	it('keeps right edge anchored when resizing from left edge', () => {
		const start = startState('left', {paneStartX: 600, startWidth: 240});
		const result = computeResize(start, 460, 300, geometry);
		const appliedWidthDelta = result.width - start.startWidth;
		expect(result.offset.x).toBeCloseTo(start.paneStartX - appliedWidthDelta, 5);
	});
	it('keeps bottom edge anchored when resizing from top edge', () => {
		const start = startState('top', {paneStartY: 300, startWidth: 240});
		const result = computeResize(start, 500, 260, geometry);
		const appliedWidthDelta = result.width - start.startWidth;
		const appliedHeightDelta = appliedWidthDelta / (16 / 9);
		expect(result.offset.y).toBeCloseTo(start.paneStartY - appliedHeightDelta, 5);
	});
	it('keeps bottom-right anchor stationary when resizing from top-left', () => {
		const start = startState('top-left', {paneStartX: 600, paneStartY: 300, startWidth: 240});
		const initialRight = start.paneStartX + start.startWidth;
		const initialBottom = start.paneStartY + getPaneHeight(start.startWidth, 16 / 9);
		const result = computeResize(start, 460, 260, geometry);
		const resultRight = result.offset.x + result.width;
		const resultBottom = result.offset.y + getPaneHeight(result.width, 16 / 9);
		expect(resultRight).toBeCloseTo(initialRight, 0);
		expect(resultBottom).toBeCloseTo(initialBottom, 0);
	});
	it('shrinks when resizing from bottom-right inward', () => {
		const start = startState('bottom-right', {paneStartX: 12, paneStartY: 12, startWidth: 320});
		const result = computeResize(start, 460, 270, geometry);
		expect(result.width).toBeLessThan(start.startWidth);
		expect(result.offset).toEqual({x: 12, y: 12});
	});
	it('clamps width at the geometry min', () => {
		const start = startState('right', {startWidth: 220});
		const result = computeResize(start, 0, 300, geometry);
		expect(result.width).toBe(getEffectiveWidthRange(geometry).min);
	});
	it('clamps width at the geometry max', () => {
		const start = startState('right', {startWidth: 220});
		const result = computeResize(start, 9000, 300, geometry);
		expect(result.width).toBe(getEffectiveWidthRange(geometry).max);
	});
	it('never produces an offset outside drag bounds', () => {
		for (const edge of ALL_RESIZE_EDGES) {
			const start = startState(edge, {paneStartX: 12, paneStartY: 12, startWidth: 240});
			for (const dx of [-500, -50, 0, 50, 500]) {
				for (const dy of [-500, -50, 0, 50, 500]) {
					const result = computeResize(start, start.pointerStartX + dx, start.pointerStartY + dy, geometry);
					const bounds = getDragBounds(geometry, result.width);
					expect(result.offset.x).toBeGreaterThanOrEqual(bounds.minX);
					expect(result.offset.x).toBeLessThanOrEqual(bounds.maxX);
					expect(result.offset.y).toBeGreaterThanOrEqual(bounds.minY);
					expect(result.offset.y).toBeLessThanOrEqual(bounds.maxY);
				}
			}
		}
	});
});

describe('reconcileToGeometry', () => {
	it('returns the same corner regardless of viewport shrink/grow', () => {
		const before = reconcileToGeometry({
			corner: 'bottom-right',
			width: 320,
			geometry: withViewport(1000, 600),
		});
		const after = reconcileToGeometry({
			corner: 'bottom-right',
			width: 320,
			geometry: withViewport(500, 400),
		});
		expect(before.corner).toBe('bottom-right');
		expect(after.corner).toBe('bottom-right');
	});
	it('places offset at the chosen corner of the new bounds', () => {
		for (const corner of ALL_CORNERS) {
			const result = reconcileToGeometry({corner, width: 240, geometry: BASE_GEOMETRY});
			const expected = getCornerPoint(corner, getDragBounds(BASE_GEOMETRY, result.width));
			expect(result.offset).toEqual(expected);
		}
	});
	it('clamps width when the new viewport cannot fit it', () => {
		const result = reconcileToGeometry({
			corner: 'top-left' as Corner,
			width: 9999,
			geometry: withViewport(400, 300),
		});
		expect(result.width).toBeLessThanOrEqual(getEffectiveWidthRange(withViewport(400, 300)).max);
	});
});
