// SPDX-License-Identifier: AGPL-3.0-or-later

import {useMasonryGridNavigation} from '@app/features/app/hooks/useMasonryGridNavigation';
import {
	MASONRY_OVERSCAN_PX,
	MASONRY_PADDING_PX,
	MASONRY_SCROLL_CHUNK_PX,
} from '@app/features/channel/components/pickers/shared/PickerConstants';
import {TileFlowSolver} from '@app/features/channel/components/TileFlowSolver';
import type * as React from 'react';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';

type VisibleItemTuple = [itemKey: string, sectionIndex: number, itemIndex: number];

interface TileBox {
	position: 'absolute' | 'sticky';
	left?: number;
	right?: number;
	top?: number;
	width: number;
	height: number;
}

type TileBoxMap = Record<string, TileBox>;
type WindowedTiles = Record<string, Array<VisibleItemTuple>>;

interface GridCell {
	section: number;
	row: number;
	column: number;
}

interface NavGrid {
	rowBase: Array<number>;
	tileCells: Record<string, GridCell>;
}

export interface MasonryExtraSection {
	sectionIndex: number;
	height: number;
	render: () => React.ReactNode;
}

const EMPTY_EXTRA_SECTIONS: ReadonlyArray<MasonryExtraSection> = [];

export function MasonryVirtualGrid<T>({
	data,
	itemKeys,
	columns,
	tileSpacing,
	viewportWidth,
	viewportHeight,
	scrollTop,
	tileKeyOf,
	tileHeightOf,
	onSelectItemKey,
	checkSuspension,
	renderItem,
	onContentSizeChange,
	extraSections,
	overscanPx = MASONRY_OVERSCAN_PX,
	paddingPx = MASONRY_PADDING_PX,
	bottomPaddingPx = MASONRY_PADDING_PX * 2,
}: {
	data: ReadonlyArray<T>;
	itemKeys: ReadonlyArray<string>;
	columns: number;
	tileSpacing: number;
	viewportWidth: number;
	viewportHeight: number;
	scrollTop: number;
	tileKeyOf: (item: T, index: number) => string;
	tileHeightOf: (item: T, index: number, laneWidth: number) => number;
	onSelectItemKey: (itemKey: string) => void;
	checkSuspension: () => boolean;
	renderItem: (args: {item: T; itemKey: string; coords: TileBox; isFocused: boolean; index: number}) => React.ReactNode;
	onContentSizeChange?: (contentSize: number) => void;
	extraSections?: ReadonlyArray<MasonryExtraSection>;
	overscanPx?: number;
	paddingPx?: number;
	bottomPaddingPx?: number;
}) {
	const stableExtraSections = extraSections ?? EMPTY_EXTRA_SECTIONS;
	const [masonrySolver] = useState(() => new TileFlowSolver());
	const containerRef = useRef<HTMLDivElement>(null);
	const [version, setVersion] = useState(0);
	useEffect(() => {
		setVersion((v) => v + 1);
	}, [data, columns, tileSpacing, viewportWidth, viewportHeight, itemKeys, stableExtraSections]);
	const sectionCount = 1 + stableExtraSections.length;
	const tileKeyForSolver = useCallback(
		(sectionIndex: number, itemIndex: number): string | null => {
			if (sectionIndex !== 0) return null;
			const item = data[itemIndex];
			return item != null ? tileKeyOf(item, itemIndex) : null;
		},
		[data, tileKeyOf],
	);
	const tileHeightForSolver = useCallback(
		(sectionIndex: number, itemIndex: number, laneWidth: number): number => {
			if (sectionIndex !== 0) return 0;
			const item = data[itemIndex];
			if (item == null) return 0;
			return tileHeightOf(item, itemIndex, laneWidth);
		},
		[data, tileHeightOf],
	);
	const sectionHeightForSolver = useCallback(
		(sectionIndex: number): number => {
			if (sectionIndex === 0) return 0;
			const extra = stableExtraSections.find((s) => s.sectionIndex === sectionIndex);
			return extra?.height ?? 0;
		},
		[stableExtraSections],
	);
	const scrollChunkPx = Math.max(1, Math.min(MASONRY_SCROLL_CHUNK_PX, overscanPx));
	const windowStartChunk = Math.floor(scrollTop / scrollChunkPx);
	const windowEndChunk = Math.ceil((scrollTop + viewportHeight) / scrollChunkPx);
	const masonryState = useMemo(() => {
		if (viewportWidth <= 0 || viewportHeight <= 0) {
			return {
				tileBoxes: {} as TileBoxMap,
				windowedTiles: {} as WindowedTiles,
				contentHeight: 0,
				navGrid: null,
			};
		}
		masonrySolver.applySettings({
			sectionSizes: [data.length, ...Array.from({length: sectionCount - 1}, () => 0)],
			laneCount: columns,
			tileSpacing,
			tileKeyOf: tileKeyForSolver,
			tileHeightOf: tileHeightForSolver,
			sectionHeightOf: sectionHeightForSolver,
			viewportSpan: viewportWidth,
			padBox: {left: paddingPx, right: paddingPx, top: 0, bottom: 0},
			layoutStamp: version,
		});
		const start = Math.max(0, windowStartChunk * scrollChunkPx - overscanPx);
		const end = windowEndChunk * scrollChunkPx + overscanPx;
		masonrySolver.selectWindow(start, end);
		const state = masonrySolver.readLayout() as {
			tileBoxes: TileBoxMap;
			windowedTiles: WindowedTiles;
			contentHeight: number;
			navGrid: NavGrid;
		};
		return state;
	}, [
		masonrySolver,
		data.length,
		sectionCount,
		columns,
		tileSpacing,
		tileKeyForSolver,
		tileHeightForSolver,
		sectionHeightForSolver,
		viewportWidth,
		viewportHeight,
		windowStartChunk,
		windowEndChunk,
		scrollChunkPx,
		overscanPx,
		paddingPx,
		version,
	]);
	const {focusedItemKey} = useMasonryGridNavigation({
		navGrid: masonryState.navGrid,
		itemKeys,
		columns,
		onSelect: onSelectItemKey,
		containerRef: containerRef as React.RefObject<HTMLElement>,
		checkSuspension,
	});
	const topPadding = paddingPx;
	const contentSize = masonryState.contentHeight + topPadding + bottomPaddingPx;
	useEffect(() => {
		onContentSizeChange?.(contentSize);
	}, [contentSize, onContentSizeChange]);
	const visibleEntries = Object.entries(masonryState.windowedTiles) as Array<[string, WindowedTiles[string]]>;
	const parseSectionIndex = (sectionKey: string): number | null => {
		const prefix = 'section-';
		if (!sectionKey.startsWith(prefix)) return null;
		const rest = sectionKey.slice(prefix.length);
		if (rest.includes('-')) return null;
		const n = Number(rest);
		return Number.isFinite(n) ? n : null;
	};
	return (
		<div
			ref={containerRef}
			style={{
				position: 'relative',
				width: '100%',
				height: contentSize,
				pointerEvents: 'none',
			}}
			data-flx="channel.pickers.masonry-virtual-grid.div"
		>
			{visibleEntries.flatMap(([sectionKey, items]): Array<React.ReactNode> => {
				const sectionCoords = masonryState.tileBoxes[sectionKey];
				if (!sectionCoords) return [];
				const sectionTop = (sectionCoords.top ?? 0) + topPadding;
				const sectionIndex = parseSectionIndex(sectionKey);
				if (sectionIndex != null && sectionIndex > 0) {
					const extra = stableExtraSections.find((s) => s.sectionIndex === sectionIndex);
					if (!extra || extra.height <= 0) return [];
					return [
						<div
							key={sectionKey}
							style={{
								position: 'absolute',
								left: sectionCoords.left,
								right: sectionCoords.right,
								width: sectionCoords.width,
								top: sectionTop,
								height: sectionCoords.height,
								pointerEvents: 'none',
							}}
							data-flx="channel.pickers.masonry-virtual-grid.div--2"
						>
							{extra.render()}
						</div>,
					];
				}
				return items.flatMap(([itemKey, itemSectionIndex, itemIndex]): Array<React.ReactNode> => {
					if (itemSectionIndex !== 0) return [];
					const itemCoords = masonryState.tileBoxes[itemKey];
					if (!itemCoords) return [];
					const item = data[itemIndex];
					if (item == null) return [];
					const absoluteItemCoords: TileBox = {
						...itemCoords,
						top: (itemCoords.top ?? 0) + sectionTop,
					};
					const node = renderItem({
						item,
						itemKey,
						coords: absoluteItemCoords,
						isFocused: focusedItemKey === itemKey,
						index: itemIndex,
					});
					return node == null ? [] : [node];
				});
			})}
		</div>
	);
}
