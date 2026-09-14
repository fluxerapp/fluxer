// SPDX-License-Identifier: AGPL-3.0-or-later

import {readFileSync} from 'node:fs';
import {
	getVoiceGridColumnCount,
	getVoiceGridGap,
	getVoiceGridMinTileSize,
	getVoiceGridPadding,
	getVoiceGridRowStyle,
	getVoiceGridRowsByColumnCount,
	getVoiceGridVisibleTileCapacity,
	resolveVoiceGridLayoutMetrics,
	resolveVoiceGridPackedLayoutMetrics,
	VOICE_GRID_COLUMN_RULES,
	VOICE_GRID_COMPACT_MIN_TILE_WIDTH_PX,
	VOICE_GRID_MIN_TILE_WIDTH_PX,
	VOICE_GRID_TILE_ASPECT_RATIO,
} from '@app/features/voice/components/VoiceGridLayoutMetrics';
import {describe, expect, it} from 'vitest';

const EPSILON_PX = 0.001;

function sourceFile(name: string): string {
	return readFileSync(new URL(name, import.meta.url), 'utf8');
}

describe('VoiceGridLayoutMetrics', () => {
	it('emits stable row variables for every CSS-selected column count', () => {
		expect(getVoiceGridRowsByColumnCount(0)).toEqual({1: 1, 2: 1, 3: 1, 4: 1});
		expect(getVoiceGridRowsByColumnCount(1)).toEqual({1: 1, 2: 1, 3: 1, 4: 1});
		expect(getVoiceGridRowsByColumnCount(5)).toEqual({1: 5, 2: 3, 3: 2, 4: 2});
		expect(getVoiceGridRowsByColumnCount(10)).toEqual({1: 10, 2: 5, 3: 4, 4: 3});
		expect(getVoiceGridRowStyle(10)).toEqual({
			'--voice-grid-rows-1': '10',
			'--voice-grid-rows-2': '5',
			'--voice-grid-rows-3': '4',
			'--voice-grid-rows-4': '3',
		});
	});
	it('matches the intended column breakpoints exactly', () => {
		expect(getVoiceGridColumnCount({tileCount: 1, containerWidth: 1920, containerHeight: 1080})).toBe(1);
		expect(getVoiceGridColumnCount({tileCount: 2, containerWidth: 519, containerHeight: 800})).toBe(1);
		expect(getVoiceGridColumnCount({tileCount: 2, containerWidth: 520, containerHeight: 260})).toBe(2);
		expect(getVoiceGridColumnCount({tileCount: 5, containerWidth: 859, containerHeight: 800})).toBe(2);
		expect(getVoiceGridColumnCount({tileCount: 5, containerWidth: 860, containerHeight: 360})).toBe(3);
		expect(getVoiceGridColumnCount({tileCount: 10, containerWidth: 1179, containerHeight: 800})).toBe(3);
		expect(getVoiceGridColumnCount({tileCount: 10, containerWidth: 1180, containerHeight: 460})).toBe(4);
	});
	it('keeps all feasible grid layouts inside their viewport without post-paint measurement', () => {
		const widths = [240, 320, 419, 420, 519, 520, 759, 760, 859, 860, 1179, 1180, 1440, 1920];
		const heights = [180, 240, 259, 260, 359, 360, 459, 460, 519, 520, 720, 1080];
		const tileCounts = [1, 2, 3, 4, 5, 6, 8, 10, 12, 16, 24, 32, 40, 64];
		for (const tileCount of tileCounts) {
			for (const containerWidth of widths) {
				for (const containerHeight of heights) {
					for (const compact of [false, true]) {
						for (const edgeToEdge of [false, true]) {
							const metrics = resolveVoiceGridLayoutMetrics({
								tileCount,
								containerWidth,
								containerHeight,
								compact,
								edgeToEdge,
							});
							const padding = getVoiceGridPadding({containerWidth, containerHeight, compact, edgeToEdge});
							const gap = getVoiceGridGap({tileCount, compact, containerHeight});
							const gapBudgetFits =
								padding.verticalPadding * 2 + gap * Math.max(0, metrics.rows - 1) <= containerHeight + EPSILON_PX;
							expect(
								metrics.tileWidth,
								`tile width for ${tileCount} tiles in ${containerWidth}x${containerHeight}`,
							).toBeGreaterThanOrEqual(0);
							expect(
								metrics.tileHeight,
								`tile height for ${tileCount} tiles in ${containerWidth}x${containerHeight}`,
							).toBeGreaterThanOrEqual(0);
							if (metrics.tileWidth > 0) {
								expect(metrics.tileWidth / metrics.tileHeight).toBeCloseTo(VOICE_GRID_TILE_ASPECT_RATIO, 5);
							}
							expect(metrics.contentWidth).toBeLessThanOrEqual(containerWidth + EPSILON_PX);
							if (gapBudgetFits) {
								expect(metrics.contentHeight).toBeLessThanOrEqual(containerHeight + EPSILON_PX);
							} else {
								expect(metrics.tileWidth).toBe(0);
								expect(metrics.tileHeight).toBe(0);
							}
						}
					}
				}
			}
		}
	});
	it('keeps compact single-tile layouts contained at 16:9 inside the safe grid frame', () => {
		const metrics = resolveVoiceGridLayoutMetrics({
			tileCount: 1,
			containerWidth: 1600,
			containerHeight: 500,
			compact: true,
		});
		expect(metrics.tileWidth).toBeLessThan(metrics.availableWidth);
		expect(metrics.tileWidth).toBeCloseTo(metrics.availableHeight * VOICE_GRID_TILE_ASPECT_RATIO, 5);
		expect(metrics.tileHeight).toBeCloseTo(metrics.availableHeight, 5);
		expect(metrics.tileWidth / metrics.tileHeight).toBeCloseTo(VOICE_GRID_TILE_ASPECT_RATIO, 5);
		expect(metrics.contentWidth).toBeLessThan(1600);
		expect(metrics.contentHeight).toBeCloseTo(500, 5);
	});
	it('limits visible tiles before they fall below the minimum readable size', () => {
		const capacity = getVoiceGridVisibleTileCapacity({
			tileCount: 64,
			containerWidth: 800,
			containerHeight: 450,
		});
		const minSize = getVoiceGridMinTileSize();
		const metrics = resolveVoiceGridPackedLayoutMetrics({
			tileCount: 64,
			containerWidth: 800,
			containerHeight: 450,
		});
		expect(capacity).toBeGreaterThan(0);
		expect(capacity).toBeLessThan(64);
		expect(metrics.visibleTileCount).toBe(capacity);
		expect(metrics.tileWidth).toBeGreaterThanOrEqual(minSize.minTileWidth - EPSILON_PX);
		expect(metrics.tileHeight).toBeGreaterThanOrEqual(minSize.minTileHeight - EPSILON_PX);
		const wideMetrics = resolveVoiceGridPackedLayoutMetrics({
			tileCount: 24,
			containerWidth: 1920,
			containerHeight: 1080,
		});
		expect(wideMetrics.visibleTileCount).toBe(24);
		expect(wideMetrics.columns).toBeGreaterThan(VOICE_GRID_COLUMN_RULES[0].columns);
		expect(
			getVoiceGridVisibleTileCapacity({
				tileCount: 10,
				containerWidth: VOICE_GRID_MIN_TILE_WIDTH_PX / 2,
				containerHeight: 90,
			}),
		).toBe(0);
	});
	it('does not force an oversized fallback tile when the compact grid cannot fit one readable tile', () => {
		const metrics = resolveVoiceGridPackedLayoutMetrics({
			tileCount: 8,
			containerWidth: 640,
			containerHeight: 0,
			compact: true,
		});
		expect(metrics.visibleTileCount).toBe(0);
		expect(metrics.tileWidth).toBe(0);
		expect(metrics.tileHeight).toBe(0);
	});
	it('packs compact tiles across a wide short viewport before hiding overflow', () => {
		const metrics = resolveVoiceGridPackedLayoutMetrics({
			tileCount: 64,
			containerWidth: 1920,
			containerHeight: 120,
			compact: true,
		});
		expect(metrics.visibleTileCount).toBeGreaterThan(8);
		expect(metrics.columns).toBe(metrics.visibleTileCount);
		expect(metrics.rows).toBe(1);
		expect(metrics.tileWidth).toBeGreaterThanOrEqual(VOICE_GRID_COMPACT_MIN_TILE_WIDTH_PX - EPSILON_PX);
		expect(metrics.tileHeight).toBeGreaterThanOrEqual(
			VOICE_GRID_COMPACT_MIN_TILE_WIDTH_PX / VOICE_GRID_TILE_ASPECT_RATIO - EPSILON_PX,
		);
		expect(metrics.contentWidth).toBeLessThanOrEqual(1920 + EPSILON_PX);
		expect(metrics.contentHeight).toBeLessThanOrEqual(120 + EPSILON_PX);
	});
	it('packs compact tiles down a tall narrow viewport before hiding overflow', () => {
		const metrics = resolveVoiceGridPackedLayoutMetrics({
			tileCount: 64,
			containerWidth: 180,
			containerHeight: 1200,
			compact: true,
		});
		expect(metrics.visibleTileCount).toBeGreaterThan(8);
		expect(metrics.columns).toBe(1);
		expect(metrics.rows).toBe(metrics.visibleTileCount);
		expect(metrics.tileWidth).toBeGreaterThanOrEqual(VOICE_GRID_COMPACT_MIN_TILE_WIDTH_PX - EPSILON_PX);
		expect(metrics.tileHeight).toBeGreaterThanOrEqual(
			VOICE_GRID_COMPACT_MIN_TILE_WIDTH_PX / VOICE_GRID_TILE_ASPECT_RATIO - EPSILON_PX,
		);
		expect(metrics.contentWidth).toBeLessThanOrEqual(180 + EPSILON_PX);
		expect(metrics.contentHeight).toBeLessThanOrEqual(1200 + EPSILON_PX);
	});
	it('keeps the helper constants synchronized with the CSS module breakpoints', () => {
		const css = sourceFile('VoiceGridLayout.module.css');
		for (const rule of VOICE_GRID_COLUMN_RULES) {
			expect(css).toContain(
				`@container voice-grid (min-width: ${rule.minWidth}px) and (min-height: ${rule.minHeight}px)`,
			);
			expect(css).toContain(`--voice-grid-columns: ${rule.columns};`);
		}
		expect(css).toContain('--voice-grid-gap: 10px;');
		expect(css).toContain('--voice-grid-gap: 8px;');
		expect(css).toContain('--voice-grid-gap: 6px;');
		expect(css).toContain('--voice-grid-gap: 5px;');
		expect(css).toContain('--voice-grid-gap: 4px;');
		expect(css).toContain('--voice-grid-tile-width: var(--voice-grid-multi-tile-width);');
		expect(css).toContain('calc(var(--voice-grid-row-height) * 16 / 9)');
		expect(css).not.toContain('--voice-grid-tile-width: var(--voice-grid-available-width);');
		expect(css).not.toMatch(/\n\s*height: var\(--voice-grid-available-height\);/);
		expect(css).not.toContain('aspect-ratio: auto;');
		expect(css).toContain(`--voice-grid-min-tile-width: ${VOICE_GRID_MIN_TILE_WIDTH_PX}px;`);
		expect(css).toContain(`--voice-grid-min-tile-width: ${VOICE_GRID_COMPACT_MIN_TILE_WIDTH_PX}px;`);
	});
	it('centers non-full final rows through wrapping layout', () => {
		const css = sourceFile('VoiceGridLayout.module.css');
		expect(css).toContain('display: flex;');
		expect(css).toContain('flex-wrap: wrap;');
		expect(css).toContain('justify-content: center;');
		expect(css).toContain('align-content: center;');
	});
	it('keeps the grid surface non-scrolling while fit is handled by capacity', () => {
		const gridSource = sourceFile('VoiceGridLayout.tsx');
		const layoutSource = sourceFile('VoiceCallLayoutContent.tsx');
		expect(gridSource).toContain('resolveVoiceGridPackedLayoutMetrics');
		expect(layoutSource).toContain('overflow="hidden"');
	});
});
