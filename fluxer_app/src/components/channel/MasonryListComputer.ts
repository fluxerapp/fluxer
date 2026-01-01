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

interface CoordsStyle {
	position: 'absolute' | 'sticky';
	left?: number;
	right?: number;
	width: number;
	top: number;
	height: number;
}

interface GridCoordinates {
	section: number;
	row: number;
	column: number;
}

interface GridData {
	boundaries: Array<number>;
	coordinates: Record<string, GridCoordinates>;
}

type VisibleSection = Array<[string, number, number]>;

type Padding = number | {top?: number; bottom?: number; left?: number; right?: number};

function findMinColumnIndex(columnHeights: Array<number>): [number, number] {
	return columnHeights.reduce((acc, height, index) => (height < acc[0] ? [height, index] : acc), [columnHeights[0], 0]);
}

const defaultGetSectionHeight = () => 0;

const getSectionKey = (sectionIndex: number) => `__section__${sectionIndex}`;
const getSectionHeaderKey = (sectionIndex: number) => `__section_header__${sectionIndex}`;

export class MasonryListComputer {
	public visibleSections: Record<string, VisibleSection> = {};
	public gridData: GridData = {coordinates: {}, boundaries: []};
	public coordsMap: Record<string, CoordsStyle> = {};
	public itemGrid: Array<Array<string>> = [];
	public totalHeight: number = 0;

	private columnHeights: Array<number> = [];
	private columnWidth: number = 0;
	private currentRow: number = 0;
	private lastColumnIndex: number = 0;
	private needsFullCompute: boolean = true;

	private bufferWidth: number = 0;
	private sections: Array<number> = [];
	private columns: number = 0;
	private itemGutter: number = 0;
	private removeEdgeItemGutters: boolean = false;
	private sectionGutter: number | null = null;
	private padding: Padding | null = null;
	private paddingVertical: number | null = null;
	private paddingHorizontal: number | null = null;
	private marginLeft: number | null = null;
	private dir: 'ltr' | 'rtl' = 'ltr';
	private version: number | string | null = null;
	private getItemKey: (section: number, item: number) => string | null = () => {
		throw new Error('MasonryListComputer: getItemKey has not been implemented');
	};
	private getItemHeight: (section: number, item: number, width: number) => number = () => {
		throw new Error('MasonryListComputer: getItemHeight has not been implemented');
	};
	private getSectionHeight: (section: number) => number = defaultGetSectionHeight;

	private getPadding(key: 'top' | 'bottom' | 'left' | 'right'): number {
		if (this.padding == null) {
			return this.itemGutter;
		}
		if (typeof this.padding === 'number') {
			return this.padding;
		}
		return this.padding[key] ?? this.itemGutter;
	}

	private getPaddingLeft(): number {
		return this.paddingHorizontal != null ? this.paddingHorizontal : this.getPadding('left');
	}

	private getPaddingRight(): number {
		return this.paddingHorizontal != null ? this.paddingHorizontal : this.getPadding('right');
	}

	private getPaddingTop(): number {
		return this.paddingVertical != null ? this.paddingVertical : this.getPadding('top');
	}

	private getPaddingBottom(): number {
		return this.paddingVertical != null ? this.paddingVertical : this.getPadding('bottom');
	}

	private getSectionGutter(): number {
		return this.sectionGutter != null ? this.sectionGutter : this.itemGutter;
	}

	mergeProps(props: {
		sections?: Array<number>;
		columns?: number;
		itemGutter?: number;
		removeEdgeItemGutters?: boolean;
		getItemKey?: (section: number, item: number) => string | null;
		getItemHeight?: (section: number, item: number, width: number) => number;
		getSectionHeight?: (section: number) => number;
		bufferWidth?: number;
		padding?: Padding;
		paddingVertical?: number;
		paddingHorizontal?: number;
		marginLeft?: number;
		sectionGutter?: number;
		dir?: 'ltr' | 'rtl';
		version?: number | string | null;
	}): void {
		const {
			sections = this.sections,
			columns = this.columns,
			itemGutter = this.itemGutter,
			removeEdgeItemGutters = this.removeEdgeItemGutters,
			getItemKey = this.getItemKey,
			getItemHeight = this.getItemHeight,
			getSectionHeight = this.getSectionHeight,
			bufferWidth = this.bufferWidth,
			padding = this.padding,
			paddingVertical = this.paddingVertical,
			paddingHorizontal = this.paddingHorizontal,
			marginLeft = this.marginLeft,
			sectionGutter = this.sectionGutter,
			dir = this.dir,
			version = this.version,
		} = props;

		if (
			this.sections !== sections ||
			this.columns !== columns ||
			this.itemGutter !== itemGutter ||
			this.removeEdgeItemGutters !== removeEdgeItemGutters ||
			this.getItemKey !== getItemKey ||
			this.getSectionHeight !== getSectionHeight ||
			this.getItemHeight !== getItemHeight ||
			this.bufferWidth !== bufferWidth ||
			this.padding !== padding ||
			this.paddingVertical !== paddingVertical ||
			this.paddingHorizontal !== paddingHorizontal ||
			this.marginLeft !== marginLeft ||
			this.sectionGutter !== sectionGutter ||
			this.dir !== dir ||
			this.version !== version
		) {
			this.needsFullCompute = true;
			this.sections = sections;
			this.columns = columns;
			this.itemGutter = itemGutter;
			this.removeEdgeItemGutters = removeEdgeItemGutters;
			this.getItemKey = getItemKey;
			this.getSectionHeight = getSectionHeight;
			this.getItemHeight = getItemHeight;
			this.bufferWidth = bufferWidth;
			this.padding = padding;
			this.paddingVertical = paddingVertical;
			this.paddingHorizontal = paddingHorizontal;
			this.marginLeft = marginLeft;
			this.sectionGutter = sectionGutter;
			this.dir = dir;
			this.version = version;
		}
	}

	private computeFullCoords(): void {
		if (!this.needsFullCompute) return;

		const {columns, getItemKey, getItemHeight, itemGutter, getSectionHeight, bufferWidth, removeEdgeItemGutters} = this;
		const horizontalKey = this.dir === 'rtl' ? 'right' : 'left';

		this.coordsMap = {};
		this.gridData = {boundaries: [], coordinates: {}};
		this.currentRow = 0;
		this.lastColumnIndex = 0;

		const paddingTop = this.getPaddingTop();
		const paddingBottom = this.getPaddingBottom();
		const paddingLeft = this.getPaddingLeft();
		const paddingRight = this.getPaddingRight();
		const marginLeft = this.marginLeft ?? 0;

		this.columnHeights = Array(columns).fill(paddingTop);
		this.columnWidth =
			(bufferWidth -
				paddingRight -
				paddingLeft -
				itemGutter * (columns - 1) -
				(removeEdgeItemGutters ? itemGutter : 0)) /
			columns;
		this.itemGrid = [];

		let sectionIndex = 0;
		while (sectionIndex < this.sections.length) {
			this.gridData.boundaries[sectionIndex] = this.currentRow;
			this.currentRow = 0;
			this.lastColumnIndex = 0;

			const sectionLength = this.sections[sectionIndex];
			let itemIndex = 0;
			let minItemTop = Number.POSITIVE_INFINITY;
			let maxItemBottom = Number.NEGATIVE_INFINITY;

			const sectionHeight = getSectionHeight(sectionIndex);
			let maxColumnHeight = this.getMaxColumnHeight(this.columnHeights);

			if (sectionIndex > 0) {
				maxColumnHeight = maxColumnHeight - itemGutter + this.getSectionGutter();
			}

			const sectionHeaderHeight = sectionHeight > 0 ? sectionHeight + itemGutter : 0;
			for (let col = 0; col < this.columnHeights.length; col++) {
				this.columnHeights[col] = maxColumnHeight + sectionHeaderHeight;
			}

			while (itemIndex < sectionLength) {
				const itemKey = getItemKey(sectionIndex, itemIndex);

				if (itemKey == null) {
					itemIndex++;
					continue;
				}

				const [minHeight, minColumnIndex] = findMinColumnIndex(this.columnHeights);

				if (minColumnIndex < this.lastColumnIndex) {
					this.currentRow++;
				}
				this.lastColumnIndex = minColumnIndex;

				const itemHeight = getItemHeight(sectionIndex, itemIndex, this.columnWidth);

				const coords: CoordsStyle = {
					position: 'absolute',
					[horizontalKey]:
						this.columnWidth * minColumnIndex + itemGutter * (minColumnIndex + 1) - itemGutter + paddingLeft,
					width: this.columnWidth,
					top: minHeight - maxColumnHeight,
					height: itemHeight,
				};

				minItemTop = Math.min(minItemTop, coords.top);
				maxItemBottom = Math.max(maxItemBottom, coords.top + coords.height);

				const gridCoords: GridCoordinates = {
					section: sectionIndex,
					row: this.currentRow,
					column: minColumnIndex,
				};

				this.coordsMap[itemKey] = coords;
				this.gridData.coordinates[itemKey] = gridCoords;
				this.columnHeights[minColumnIndex] = minHeight + itemHeight + itemGutter;

				this.itemGrid[minColumnIndex] = this.itemGrid[minColumnIndex] ?? [];
				this.itemGrid[minColumnIndex].push(itemKey);

				itemIndex++;
			}

			if (sectionHeight > 0) {
				this.coordsMap[getSectionHeaderKey(sectionIndex)] = {
					position: 'sticky',
					[horizontalKey]: paddingLeft,
					width: this.columnWidth * columns + itemGutter * columns,
					top: 0,
					height: sectionHeight,
				};

				this.coordsMap[getSectionKey(sectionIndex)] = {
					position: 'absolute',
					[horizontalKey]: marginLeft,
					width: this.columnWidth * columns + itemGutter * (columns - 1) + paddingLeft + paddingRight,
					top: maxColumnHeight,
					height: this.getMaxColumnHeight(this.columnHeights) - maxColumnHeight,
				};
			} else if (Number.isFinite(minItemTop) && Number.isFinite(maxItemBottom)) {
				this.coordsMap[getSectionKey(sectionIndex)] = {
					position: 'absolute',
					[horizontalKey]: marginLeft,
					width: this.columnWidth * columns + itemGutter * (columns - 1) + paddingLeft + paddingRight,
					top: minItemTop,
					height: maxItemBottom - minItemTop,
				};
			}

			sectionIndex++;
		}

		this.columnHeights = this.columnHeights.map((height) => height - itemGutter + paddingBottom);
		this.totalHeight = this.getMaxColumnHeight();
		this.visibleSections = {};
		this.needsFullCompute = false;
	}

	computeVisibleSections(start: number, end: number): void {
		this.computeFullCoords();

		const {getItemKey, coordsMap} = this;
		this.visibleSections = {};

		let sectionIndex = 0;
		while (sectionIndex < this.sections.length) {
			const sectionLength = this.sections[sectionIndex];
			const sectionKey = getSectionKey(sectionIndex);
			const sectionCoords = coordsMap[sectionKey];

			if (sectionCoords == null) {
				sectionIndex++;
				continue;
			}

			const {top} = sectionCoords;
			const bottom = top + sectionCoords.height;

			if (top > end) break;
			if (bottom < start) {
				sectionIndex++;
				continue;
			}

			let itemIndex = 0;
			let direction = 1;

			if (bottom < end && bottom > start) {
				itemIndex = sectionLength - 1;
				direction = -1;
			}

			this.visibleSections[sectionKey] = [];

			while (itemIndex >= 0 && itemIndex < sectionLength) {
				const itemKey = getItemKey(sectionIndex, itemIndex);
				const itemCoords = itemKey != null ? coordsMap[itemKey] : null;

				if (itemKey == null || itemCoords == null) {
					itemIndex += direction;
					continue;
				}

				const {top: itemTop, height: itemHeight} = itemCoords;

				const itemAbsoluteTop = itemTop + top;

				if (itemAbsoluteTop > start - itemHeight && itemAbsoluteTop < end) {
					if (direction === -1) {
						this.visibleSections[sectionKey].unshift([itemKey, sectionIndex, itemIndex]);
					} else {
						this.visibleSections[sectionKey].push([itemKey, sectionIndex, itemIndex]);
					}
				}

				itemIndex += direction;
			}

			if (top < start && bottom > end) break;
			sectionIndex++;
		}
	}

	private getMaxColumnHeight(columnHeights: Array<number> = this.columnHeights): number {
		return columnHeights.reduce((max, height) => Math.max(max, height), 0);
	}

	getState(): {
		coordsMap: Record<string, CoordsStyle>;
		gridData: GridData;
		visibleSections: Record<string, VisibleSection>;
		totalHeight: number;
	} {
		return {
			coordsMap: this.coordsMap,
			gridData: this.gridData,
			visibleSections: this.visibleSections,
			totalHeight: this.totalHeight,
		};
	}
}
