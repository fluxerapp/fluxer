// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {
	clampPanForScale,
	clampScale,
	DEFAULT_ZOOM_SCALE,
	getAnchoredZoomPoint,
	getCentroid,
	getDistance,
	isDefaultTransform,
	MIN_ZOOM_SCALE,
	type PanZoomMetrics,
	TAP_MOVE_THRESHOLD,
} from './PanZoomMath';

const METRICS: PanZoomMetrics = {
	viewportWidth: 800,
	viewportHeight: 600,
	contentWidth: 400,
	contentHeight: 300,
};
const UNCAPPED_METRICS: PanZoomMetrics = {
	viewportWidth: 800,
	viewportHeight: 600,
	contentWidth: 800,
	contentHeight: 600,
};

describe('PanZoomMath', () => {
	it('measures pinch distance and centroid without rounding drift', () => {
		const first = {x: 10, y: 20};
		const second = {x: 70, y: 100};
		expect(getDistance(first, second)).toBe(100);
		expect(getCentroid(first, second)).toEqual({x: 40, y: 60});
	});
	it('recenters pan when scale returns to the fitted view', () => {
		expect(clampPanForScale({x: 300, y: -180}, MIN_ZOOM_SCALE, METRICS)).toEqual({x: 0, y: 0});
	});
	it('clamps stepped zoom scales to configured limits', () => {
		expect(clampScale(0.6, 1, 5)).toBe(1);
		expect(clampScale(2.5, 1, 5)).toBe(2.5);
		expect(clampScale(8, 1, 5)).toBe(5);
	});
	it('treats a drag under twenty pixels as a tap', () => {
		expect(TAP_MOVE_THRESHOLD).toBe(20);
	});
	it('pins an axis whose zoomed content still fits inside the viewport', () => {
		const portrait: PanZoomMetrics = {viewportWidth: 800, viewportHeight: 600, contentWidth: 300, contentHeight: 600};
		expect(clampPanForScale({x: 400, y: 600}, 2.5, portrait)).toEqual({x: 0, y: 450});
	});
	it('detects the centered default transform with a small epsilon', () => {
		expect(isDefaultTransform({scale: 1.005, x: 0.004, y: -0.003})).toBe(true);
		expect(isDefaultTransform({scale: 1.05, x: 0, y: 0})).toBe(false);
		expect(isDefaultTransform({scale: 1, x: 3, y: 0})).toBe(false);
	});
	it('clamps pan to the transformed content bounds', () => {
		expect(clampPanForScale({x: 500, y: -500}, 4, METRICS)).toEqual({x: 400, y: -300});
		expect(clampPanForScale({x: -240, y: 120}, 2, METRICS)).toEqual({x: 0, y: 0});
	});
	it('keeps the zoom origin visually anchored while zooming in', () => {
		const nextPoint = getAnchoredZoomPoint({
			origin: {x: 120, y: -80},
			current: {x: 0, y: 0},
			currentScale: MIN_ZOOM_SCALE,
			nextScale: DEFAULT_ZOOM_SCALE,
			metrics: UNCAPPED_METRICS,
		});
		expect(nextPoint).toEqual({x: -180, y: 120});
	});
	it('clamps anchored zoom to the available pan range for edge origins', () => {
		const nextPoint = getAnchoredZoomPoint({
			origin: {x: -390, y: 290},
			current: {x: 0, y: 0},
			currentScale: MIN_ZOOM_SCALE,
			nextScale: 5,
			metrics: METRICS,
		});
		expect(nextPoint).toEqual({x: 600, y: -450});
	});
	it('resets pan when an anchored zoom resolves back to fit', () => {
		const nextPoint = getAnchoredZoomPoint({
			origin: {x: 200, y: 100},
			current: {x: -80, y: 60},
			currentScale: DEFAULT_ZOOM_SCALE,
			nextScale: MIN_ZOOM_SCALE,
			metrics: METRICS,
		});
		expect(nextPoint).toEqual({x: 0, y: 0});
	});
});
