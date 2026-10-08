// SPDX-License-Identifier: AGPL-3.0-or-later

const VOICE_GRID_TILE_ASPECT_RATIO = 16 / 9;
const VOICE_GRID_DEFAULT_GAP_PX = 12;
const VOICE_GRID_DEFAULT_SIDE_PADDING_PX = 12;
const VOICE_GRID_DEFAULT_VERTICAL_PADDING_PX = 14;
const VOICE_GRID_MIN_TILE_WIDTH_PX = 220;
const VOICE_GRID_MIN_TILE_HEIGHT_PX = VOICE_GRID_MIN_TILE_WIDTH_PX / VOICE_GRID_TILE_ASPECT_RATIO;
const VOICE_GRID_COMPACT_MIN_TILE_WIDTH_PX = 148;
const VOICE_GRID_COMPACT_MIN_TILE_HEIGHT_PX = VOICE_GRID_COMPACT_MIN_TILE_WIDTH_PX / VOICE_GRID_TILE_ASPECT_RATIO;

type VoiceGridRowVariable =
	| '--voice-grid-rows-1'
	| '--voice-grid-rows-2'
	| '--voice-grid-rows-3'
	| '--voice-grid-rows-4';
export type VoiceGridRowStyle = Record<VoiceGridRowVariable, string>;

interface VoiceGridLayoutMetricsInput {
	tileCount: number;
	containerWidth: number;
	containerHeight: number;
	compact?: boolean;
	edgeToEdge?: boolean;
}

export interface VoiceGridVisibleTileCapacityInput extends VoiceGridLayoutMetricsInput {
	minTileWidth?: number;
	minTileHeight?: number;
}

interface VoiceGridLayoutMetricsForColumnsInput extends VoiceGridLayoutMetricsInput {
	columns: number;
}

interface VoiceGridLayoutMetrics {
	columns: number;
	rows: number;
	gap: number;
	sidePadding: number;
	verticalPadding: number;
	availableWidth: number;
	availableHeight: number;
	tileWidth: number;
	tileHeight: number;
	contentWidth: number;
	contentHeight: number;
}

export interface VoiceGridPackedLayoutMetrics extends VoiceGridLayoutMetrics {
	visibleTileCount: number;
}

function resolveVoiceGridEmptyLayoutMetrics({
	tileCount,
	containerWidth,
	containerHeight,
	compact = false,
	edgeToEdge = false,
}: VoiceGridLayoutMetricsInput): VoiceGridLayoutMetrics {
	const count = sanitizeCount(tileCount);
	const width = sanitizeDimension(containerWidth);
	const height = sanitizeDimension(containerHeight);
	const gap = getVoiceGridGap({tileCount: count, compact, containerHeight: height});
	const {sidePadding, verticalPadding} = getVoiceGridPadding({
		containerWidth: width,
		containerHeight: height,
		compact,
		edgeToEdge,
	});
	const availableWidth = Math.max(0, width - sidePadding * 2);
	const availableHeight = Math.max(0, height - verticalPadding * 2);
	return {
		columns: 1,
		rows: 1,
		gap,
		sidePadding,
		verticalPadding,
		availableWidth,
		availableHeight,
		tileWidth: 0,
		tileHeight: 0,
		contentWidth: sidePadding * 2,
		contentHeight: verticalPadding * 2,
	};
}

function sanitizeCount(tileCount: number): number {
	if (!Number.isFinite(tileCount)) return 0;
	return Math.max(0, Math.floor(tileCount));
}

function sanitizeDimension(value: number): number {
	if (!Number.isFinite(value)) return 0;
	return Math.max(0, value);
}

function clamp(value: number, min: number, max: number): number {
	return Math.max(min, Math.min(max, value));
}

function getVoiceGridRowCount(tileCount: number, columnCount: number): number {
	return Math.max(1, Math.ceil(sanitizeCount(tileCount) / Math.max(1, Math.floor(columnCount))));
}

function getVoiceGridRowsByColumnCount(tileCount: number): {1: number; 2: number; 3: number; 4: number} {
	return {
		1: getVoiceGridRowCount(tileCount, 1),
		2: getVoiceGridRowCount(tileCount, 2),
		3: getVoiceGridRowCount(tileCount, 3),
		4: getVoiceGridRowCount(tileCount, 4),
	};
}

export function getVoiceGridRowStyle(tileCount: number): VoiceGridRowStyle {
	const rows = getVoiceGridRowsByColumnCount(tileCount);
	return {
		'--voice-grid-rows-1': `${rows[1]}`,
		'--voice-grid-rows-2': `${rows[2]}`,
		'--voice-grid-rows-3': `${rows[3]}`,
		'--voice-grid-rows-4': `${rows[4]}`,
	};
}

function getVoiceGridGap({
	tileCount,
	compact = false,
	containerHeight,
}: Pick<VoiceGridLayoutMetricsInput, 'tileCount' | 'compact' | 'containerHeight'>): number {
	const count = sanitizeCount(tileCount);
	if (count >= 40) return 4;
	if (compact && count >= 24) return 5;
	if (count >= 24) return 6;
	if (compact && count >= 12) return 6;
	if (count >= 12) return 8;
	if (count >= 6) return 10;
	if (compact) return clamp(sanitizeDimension(containerHeight) * 0.024, 6, 10);
	return VOICE_GRID_DEFAULT_GAP_PX;
}

function getVoiceGridPadding({
	containerWidth,
	containerHeight,
	compact = false,
	edgeToEdge = false,
}: Pick<VoiceGridLayoutMetricsInput, 'containerWidth' | 'containerHeight' | 'compact' | 'edgeToEdge'>): {
	sidePadding: number;
	verticalPadding: number;
} {
	if (edgeToEdge) {
		return {sidePadding: 0, verticalPadding: 0};
	}
	const width = sanitizeDimension(containerWidth);
	const height = sanitizeDimension(containerHeight);
	const sidePadding = compact
		? clamp(height * 0.02, 6, 12)
		: width <= 419
			? 6
			: width <= 759
				? 8
				: VOICE_GRID_DEFAULT_SIDE_PADDING_PX;
	const verticalPadding = compact
		? clamp(height * 0.018, 5, 10)
		: height <= 359
			? 8
			: height <= 519
				? 10
				: VOICE_GRID_DEFAULT_VERTICAL_PADDING_PX;
	return {sidePadding, verticalPadding};
}

function getVoiceGridMinTileSize(compact = false): {minTileWidth: number; minTileHeight: number} {
	return compact
		? {minTileWidth: VOICE_GRID_COMPACT_MIN_TILE_WIDTH_PX, minTileHeight: VOICE_GRID_COMPACT_MIN_TILE_HEIGHT_PX}
		: {minTileWidth: VOICE_GRID_MIN_TILE_WIDTH_PX, minTileHeight: VOICE_GRID_MIN_TILE_HEIGHT_PX};
}

function resolveVoiceGridLayoutMetricsForColumns({
	tileCount,
	containerWidth,
	containerHeight,
	compact = false,
	edgeToEdge = false,
	columns,
}: VoiceGridLayoutMetricsForColumnsInput): VoiceGridLayoutMetrics {
	const count = sanitizeCount(tileCount);
	if (count <= 0) {
		return resolveVoiceGridEmptyLayoutMetrics({
			tileCount: count,
			containerWidth,
			containerHeight,
			compact,
			edgeToEdge,
		});
	}
	const width = sanitizeDimension(containerWidth);
	const height = sanitizeDimension(containerHeight);
	const resolvedColumns = Math.max(1, Math.min(Math.max(1, count), Math.floor(columns)));
	const rows = getVoiceGridRowCount(count, resolvedColumns);
	const gap = getVoiceGridGap({tileCount: count, compact, containerHeight: height});
	const {sidePadding, verticalPadding} = getVoiceGridPadding({
		containerWidth: width,
		containerHeight: height,
		compact,
		edgeToEdge,
	});
	const availableWidth = Math.max(0, width - sidePadding * 2);
	const availableHeight = Math.max(0, height - verticalPadding * 2);
	const columnWidth = Math.max(0, (availableWidth - gap * Math.max(0, resolvedColumns - 1)) / resolvedColumns);
	const rowHeight = Math.max(0, (availableHeight - gap * Math.max(0, rows - 1)) / rows);
	const tileWidth = Math.min(columnWidth, rowHeight * VOICE_GRID_TILE_ASPECT_RATIO);
	const tileHeight = tileWidth / VOICE_GRID_TILE_ASPECT_RATIO;
	const contentWidth =
		sidePadding * 2 +
		tileWidth * Math.min(count, resolvedColumns) +
		gap * Math.max(0, Math.min(count, resolvedColumns) - 1);
	const contentHeight = verticalPadding * 2 + tileHeight * rows + gap * Math.max(0, rows - 1);
	return {
		columns: resolvedColumns,
		rows,
		gap,
		sidePadding,
		verticalPadding,
		availableWidth,
		availableHeight,
		tileWidth,
		tileHeight,
		contentWidth,
		contentHeight,
	};
}

export function resolveVoiceGridPackedLayoutMetrics({
	tileCount,
	containerWidth,
	containerHeight,
	compact = false,
	edgeToEdge = false,
	minTileWidth,
	minTileHeight,
}: VoiceGridVisibleTileCapacityInput): VoiceGridPackedLayoutMetrics {
	const count = sanitizeCount(tileCount);
	const width = sanitizeDimension(containerWidth);
	const height = sanitizeDimension(containerHeight);
	if (count <= 0) {
		return {
			...resolveVoiceGridEmptyLayoutMetrics({
				tileCount: 0,
				containerWidth: width,
				containerHeight: height,
				compact,
				edgeToEdge,
			}),
			visibleTileCount: 0,
		};
	}
	const minSize = getVoiceGridMinTileSize(compact);
	const effectiveMinTileWidth = sanitizeDimension(minTileWidth ?? minSize.minTileWidth);
	const effectiveMinTileHeight = sanitizeDimension(minTileHeight ?? minSize.minTileHeight);
	for (let visibleCount = count; visibleCount >= 1; visibleCount--) {
		let bestMetrics: VoiceGridLayoutMetrics | null = null;
		for (let columns = 1; columns <= visibleCount; columns++) {
			const metrics = resolveVoiceGridLayoutMetricsForColumns({
				tileCount: visibleCount,
				containerWidth: width,
				containerHeight: height,
				compact,
				edgeToEdge,
				columns,
			});
			if (metrics.tileWidth < effectiveMinTileWidth || metrics.tileHeight < effectiveMinTileHeight) continue;
			if (!bestMetrics || metrics.tileWidth * metrics.tileHeight > bestMetrics.tileWidth * bestMetrics.tileHeight) {
				bestMetrics = metrics;
			}
		}
		if (bestMetrics) {
			return {...bestMetrics, visibleTileCount: visibleCount};
		}
	}
	return {
		...resolveVoiceGridEmptyLayoutMetrics({
			tileCount: count,
			containerWidth: width,
			containerHeight: height,
			compact,
			edgeToEdge,
		}),
		visibleTileCount: 0,
	};
}
