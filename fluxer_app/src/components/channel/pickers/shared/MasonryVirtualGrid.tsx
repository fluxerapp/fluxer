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

import * as React from 'react';
import {MasonryListComputer} from '~/components/channel/MasonryListComputer';
import {useMasonryGridNavigation} from '~/hooks/useMasonryGridNavigation';
import {MASONRY_OVERSCAN_PX, MASONRY_PADDING_PX} from './constants';

type VisibleItemTuple = [itemKey: string, sectionIndex: number, itemIndex: number];

type Coords = {
	position: 'absolute' | 'sticky';
	left?: number;
	right?: number;
	top?: number;
	width: number;
	height: number;
};

type CoordsMap = Record<string, Coords>;
type VisibleSections = Record<string, Array<VisibleItemTuple>>;

interface GridCoordinates {
	section: number;
	row: number;
	column: number;
}

interface GridData {
	boundaries: Array<number>;
	coordinates: Record<string, GridCoordinates>;
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
	itemGutter,
	viewportWidth,
	viewportHeight,
	scrollTop,
	getItemKey,
	getItemHeight,
	onSelectItemKey,
	checkSuspension,
	renderItem,
	extraSections,
	overscanPx = MASONRY_OVERSCAN_PX,
	paddingPx = MASONRY_PADDING_PX,
}: {
	data: ReadonlyArray<T>;
	itemKeys: ReadonlyArray<string>;
	columns: number;
	itemGutter: number;
	viewportWidth: number;
	viewportHeight: number;
	scrollTop: number;

	getItemKey: (item: T, index: number) => string;
	getItemHeight: (item: T, index: number, columnWidth: number) => number;

	onSelectItemKey: (itemKey: string) => void;
	checkSuspension: () => boolean;

	renderItem: (args: {item: T; itemKey: string; coords: Coords; isFocused: boolean; index: number}) => React.ReactNode;

	extraSections?: ReadonlyArray<MasonryExtraSection>;
	overscanPx?: number;
	paddingPx?: number;
}) {
	const stableExtraSections = extraSections ?? EMPTY_EXTRA_SECTIONS;
	const [masonryComputer] = React.useState(() => new MasonryListComputer());
	const containerRef = React.useRef<HTMLDivElement>(null);

	const [version, setVersion] = React.useState(0);
	React.useEffect(() => {
		setVersion((v) => v + 1);
	}, [data, columns, itemGutter, viewportWidth, viewportHeight, itemKeys, stableExtraSections]);

	const sectionCount = 1 + stableExtraSections.length;

	const getItemKeyForComputer = React.useCallback(
		(sectionIndex: number, itemIndex: number): string | null => {
			if (sectionIndex !== 0) return null;
			const item = data[itemIndex];
			return item != null ? getItemKey(item, itemIndex) : null;
		},
		[data, getItemKey],
	);

	const getItemHeightForComputer = React.useCallback(
		(sectionIndex: number, itemIndex: number, columnWidth: number): number => {
			if (sectionIndex !== 0) return 0;
			const item = data[itemIndex];
			if (item == null) return 0;
			return getItemHeight(item, itemIndex, columnWidth);
		},
		[data, getItemHeight],
	);

	const getSectionHeightForComputer = React.useCallback(
		(sectionIndex: number): number => {
			if (sectionIndex === 0) return 0;
			const extra = stableExtraSections.find((s) => s.sectionIndex === sectionIndex);
			return extra?.height ?? 0;
		},
		[stableExtraSections],
	);

	const masonryState = React.useMemo(() => {
		if (viewportWidth <= 0 || viewportHeight <= 0) {
			return {
				coordsMap: {} as CoordsMap,
				visibleSections: {} as VisibleSections,
				totalHeight: 0,
				gridData: null as any,
			};
		}

		masonryComputer.mergeProps({
			sections: [data.length, ...Array.from({length: sectionCount - 1}, () => 0)],
			columns,
			itemGutter,
			getItemKey: getItemKeyForComputer,
			getItemHeight: getItemHeightForComputer,
			getSectionHeight: getSectionHeightForComputer,
			bufferWidth: viewportWidth,
			padding: {left: paddingPx, right: paddingPx, top: 0, bottom: 0},
			version,
		});

		const start = Math.max(0, scrollTop - overscanPx);
		const end = scrollTop + viewportHeight + overscanPx;
		masonryComputer.computeVisibleSections(start, end);

		const state = masonryComputer.getState() as {
			coordsMap: CoordsMap;
			visibleSections: VisibleSections;
			totalHeight: number;
			gridData: GridData;
		};

		return state;
	}, [
		masonryComputer,
		data.length,
		sectionCount,
		columns,
		itemGutter,
		getItemKeyForComputer,
		getItemHeightForComputer,
		getSectionHeightForComputer,
		viewportWidth,
		viewportHeight,
		scrollTop,
		overscanPx,
		paddingPx,
		version,
	]);

	const {focusedItemKey} = useMasonryGridNavigation({
		gridData: masonryState.gridData,
		itemKeys,
		columns,
		onSelect: onSelectItemKey,
		containerRef: containerRef as React.RefObject<HTMLElement>,
		checkSuspension,
	});

	const padding = paddingPx;

	const visibleEntries = Object.entries(masonryState.visibleSections) as Array<[string, VisibleSections[string]]>;

	const parseSectionIndex = (sectionKey: string): number | null => {
		const prefix = '__section__';
		if (!sectionKey.startsWith(prefix)) return null;
		const raw = sectionKey.slice(prefix.length);
		const n = Number(raw);
		return Number.isFinite(n) ? n : null;
	};

	return (
		<div
			ref={containerRef}
			style={{position: 'relative', width: '100%', height: masonryState.totalHeight + padding * 2}}
		>
			{visibleEntries.map(([sectionKey, items]) => {
				const sectionCoords = masonryState.coordsMap[sectionKey];
				if (!sectionCoords) return null;

				const adjustedSectionCoords: Coords = {
					...sectionCoords,
					top: (sectionCoords.top ?? 0) + padding,
				};

				const sectionIndex = parseSectionIndex(sectionKey);

				if (sectionIndex != null && sectionIndex > 0) {
					const extra = stableExtraSections.find((s) => s.sectionIndex === sectionIndex);
					if (!extra || extra.height <= 0) return null;

					return (
						<div key={sectionKey} style={adjustedSectionCoords}>
							{extra.render()}
						</div>
					);
				}

				return (
					<div key={sectionKey} style={adjustedSectionCoords}>
						{items.map(([itemKey, itemSectionIndex, itemIndex]) => {
							if (itemSectionIndex !== 0) return null;

							const itemCoords = masonryState.coordsMap[itemKey];
							if (!itemCoords) return null;

							const item = data[itemIndex];
							if (item == null) return null;

							return renderItem({
								item,
								itemKey,
								coords: itemCoords,
								isFocused: focusedItemKey === itemKey,
								index: itemIndex,
							});
						})}
					</div>
				);
			})}

			<div
				style={{position: 'absolute', left: 0, right: 0, top: masonryState.totalHeight + padding, height: padding}}
			/>
		</div>
	);
}
