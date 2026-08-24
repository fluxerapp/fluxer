// SPDX-License-Identifier: AGPL-3.0-or-later

type SectionWindow = Array<[string, number, number]>;

interface GridCell {
	section: number;
	row: number;
	column: number;
}

interface NavGrid {
	tileCells: Record<string, GridCell>;
	rowBase: Array<number>;
}

interface TileBox {
	position: 'absolute' | 'sticky';
	left?: number;
	right?: number;
	width: number;
	top: number;
	height: number;
}

type LayoutPadding =
	| number
	| {
			top?: number;
			bottom?: number;
			left?: number;
			right?: number;
	  };

function noSectionHeight(_section: number): number {
	return 0;
}

function shortestLane(laneFill: Array<number>): [number, number] {
	let minHeight = laneFill[0];
	let minIndex = 0;
	for (let i = 1; i < laneFill.length; i++) {
		if (laneFill[i] < minHeight) {
			minHeight = laneFill[i];
			minIndex = i;
		}
	}
	return [minHeight, minIndex];
}

function getSectionHeaderKey(sectionIndex: number): string {
	return `section-header-${sectionIndex}`;
}

function getSectionKey(sectionIndex: number): string {
	return `section-${sectionIndex}`;
}

export class TileFlowSolver {
	public windowedTiles: Record<string, SectionWindow> = {};
	public navGrid: NavGrid = {tileCells: {}, rowBase: []};
	public tileBoxes: Record<string, TileBox> = {};
	public laneOrder: Array<Array<string>> = [];
	public contentHeight: number = 0;
	private laneFill: Array<number> = [];
	private laneWidth: number = 0;
	private rowCursor: number = 0;
	private prevLaneIndex: number = 0;
	private layoutDirty: boolean = true;
	private viewportSpan: number = 0;
	private sectionSizes: Array<number> = [];
	private laneCount: number = 0;
	private tileSpacing: number = 0;
	private trimOuterSpacing: boolean = false;
	private sectionSpacing: number | null = null;
	private padBox: LayoutPadding | null = null;
	private padAxisY: number | null = null;
	private padAxisX: number | null = null;
	private originShift: number | null = null;
	private flowDirection: 'ltr' | 'rtl' = 'ltr';
	private layoutStamp: number | string | null = null;
	private tileKeyOf: (section: number, item: number) => string | null = () => {
		throw new Error('TileFlowSolver requires a tile key resolver before layout can run');
	};
	private tileHeightOf: (section: number, item: number, width: number) => number = () => {
		throw new Error('TileFlowSolver requires a tile height measurer before layout can run');
	};
	private sectionHeightOf: (section: number) => number = noSectionHeight;

	private padFor(key: 'top' | 'bottom' | 'left' | 'right'): number {
		if (this.padBox == null) {
			return this.tileSpacing;
		}
		if (typeof this.padBox === 'number') {
			return this.padBox;
		}
		return this.padBox[key] ?? this.tileSpacing;
	}

	private padLeading(): number {
		return this.padAxisX != null ? this.padAxisX : this.padFor('left');
	}

	private padTrailing(): number {
		return this.padAxisX != null ? this.padAxisX : this.padFor('right');
	}

	private padTop(): number {
		return this.padAxisY != null ? this.padAxisY : this.padFor('top');
	}

	private padBottom(): number {
		return this.padAxisY != null ? this.padAxisY : this.padFor('bottom');
	}

	private spacingBetweenSections(): number {
		return this.sectionSpacing != null ? this.sectionSpacing : this.tileSpacing;
	}

	applySettings(settings: {
		sectionSizes?: Array<number>;
		laneCount?: number;
		tileSpacing?: number;
		trimOuterSpacing?: boolean;
		tileKeyOf?: (section: number, item: number) => string | null;
		tileHeightOf?: (section: number, item: number, width: number) => number;
		sectionHeightOf?: (section: number) => number;
		viewportSpan?: number;
		padBox?: LayoutPadding;
		padAxisY?: number;
		padAxisX?: number;
		originShift?: number;
		sectionSpacing?: number;
		flowDirection?: 'ltr' | 'rtl';
		layoutStamp?: number | string | null;
	}): void {
		const {
			sectionSizes = this.sectionSizes,
			laneCount = this.laneCount,
			tileSpacing = this.tileSpacing,
			trimOuterSpacing = this.trimOuterSpacing,
			tileKeyOf = this.tileKeyOf,
			tileHeightOf = this.tileHeightOf,
			sectionHeightOf = this.sectionHeightOf,
			viewportSpan = this.viewportSpan,
			padBox = this.padBox,
			padAxisY = this.padAxisY,
			padAxisX = this.padAxisX,
			originShift = this.originShift,
			sectionSpacing = this.sectionSpacing,
			flowDirection = this.flowDirection,
			layoutStamp = this.layoutStamp,
		} = settings;
		if (
			this.sectionSizes !== sectionSizes ||
			this.laneCount !== laneCount ||
			this.tileSpacing !== tileSpacing ||
			this.trimOuterSpacing !== trimOuterSpacing ||
			this.tileKeyOf !== tileKeyOf ||
			this.sectionHeightOf !== sectionHeightOf ||
			this.tileHeightOf !== tileHeightOf ||
			this.viewportSpan !== viewportSpan ||
			this.padBox !== padBox ||
			this.padAxisY !== padAxisY ||
			this.padAxisX !== padAxisX ||
			this.originShift !== originShift ||
			this.sectionSpacing !== sectionSpacing ||
			this.flowDirection !== flowDirection ||
			this.layoutStamp !== layoutStamp
		) {
			this.layoutDirty = true;
			this.sectionSizes = sectionSizes;
			this.laneCount = laneCount;
			this.tileSpacing = tileSpacing;
			this.trimOuterSpacing = trimOuterSpacing;
			this.tileKeyOf = tileKeyOf;
			this.sectionHeightOf = sectionHeightOf;
			this.tileHeightOf = tileHeightOf;
			this.viewportSpan = viewportSpan;
			this.padBox = padBox;
			this.padAxisY = padAxisY;
			this.padAxisX = padAxisX;
			this.originShift = originShift;
			this.sectionSpacing = sectionSpacing;
			this.flowDirection = flowDirection;
			this.layoutStamp = layoutStamp;
		}
	}

	private rebuildLayout(): void {
		if (!this.layoutDirty) return;
		const {laneCount, tileKeyOf, tileHeightOf, tileSpacing, sectionHeightOf, viewportSpan, trimOuterSpacing} = this;
		const horizontalKey = this.flowDirection === 'rtl' ? 'right' : 'left';
		this.tileBoxes = {};
		this.navGrid = {rowBase: [], tileCells: {}};
		this.rowCursor = 0;
		this.prevLaneIndex = 0;
		const padTop = this.padTop();
		const padBottom = this.padBottom();
		const padLeading = this.padLeading();
		const padTrailing = this.padTrailing();
		const originShift = this.originShift ?? 0;
		this.laneFill = Array(laneCount).fill(padTop);
		this.laneWidth =
			(viewportSpan - padTrailing - padLeading - tileSpacing * (laneCount - 1) - (trimOuterSpacing ? tileSpacing : 0)) /
			laneCount;
		this.laneOrder = [];
		let sectionIndex = 0;
		while (sectionIndex < this.sectionSizes.length) {
			this.navGrid.rowBase[sectionIndex] = this.rowCursor;
			this.rowCursor = 0;
			this.prevLaneIndex = 0;
			const sectionLength = this.sectionSizes[sectionIndex];
			let itemIndex = 0;
			let minItemTop = Number.POSITIVE_INFINITY;
			let maxItemBottom = Number.NEGATIVE_INFINITY;
			const sectionHeight = sectionHeightOf(sectionIndex);
			let laneCeiling = this.tallestLane(this.laneFill);
			if (sectionIndex > 0) {
				laneCeiling = laneCeiling - tileSpacing + this.spacingBetweenSections();
			}
			const headerBand = sectionHeight > 0 ? sectionHeight + tileSpacing : 0;
			for (let lane = 0; lane < this.laneFill.length; lane++) {
				this.laneFill[lane] = laneCeiling + headerBand;
			}
			while (itemIndex < sectionLength) {
				const tileKey = tileKeyOf(sectionIndex, itemIndex);
				if (tileKey == null) {
					itemIndex++;
					continue;
				}
				const [minHeight, laneIndex] = shortestLane(this.laneFill);
				if (laneIndex < this.prevLaneIndex) {
					this.rowCursor++;
				}
				this.prevLaneIndex = laneIndex;
				const tileHeight = tileHeightOf(sectionIndex, itemIndex, this.laneWidth);
				const box: TileBox = {
					position: 'absolute',
					[horizontalKey]: this.laneWidth * laneIndex + tileSpacing * (laneIndex + 1) - tileSpacing + padLeading,
					width: this.laneWidth,
					top: minHeight - laneCeiling,
					height: tileHeight,
				};
				minItemTop = Math.min(minItemTop, box.top);
				maxItemBottom = Math.max(maxItemBottom, box.top + box.height);
				const cell: GridCell = {
					section: sectionIndex,
					row: this.rowCursor,
					column: laneIndex,
				};
				this.tileBoxes[tileKey] = box;
				this.navGrid.tileCells[tileKey] = cell;
				this.laneFill[laneIndex] = minHeight + tileHeight + tileSpacing;
				this.laneOrder[laneIndex] = this.laneOrder[laneIndex] ?? [];
				this.laneOrder[laneIndex].push(tileKey);
				itemIndex++;
			}
			if (sectionHeight > 0) {
				this.tileBoxes[getSectionHeaderKey(sectionIndex)] = {
					position: 'sticky',
					[horizontalKey]: padLeading,
					width: this.laneWidth * laneCount + tileSpacing * laneCount,
					top: 0,
					height: sectionHeight,
				};
				this.tileBoxes[getSectionKey(sectionIndex)] = {
					position: 'absolute',
					[horizontalKey]: originShift,
					width: this.laneWidth * laneCount + tileSpacing * (laneCount - 1) + padLeading + padTrailing,
					top: laneCeiling,
					height: this.tallestLane(this.laneFill) - laneCeiling,
				};
			} else if (Number.isFinite(minItemTop) && Number.isFinite(maxItemBottom)) {
				this.tileBoxes[getSectionKey(sectionIndex)] = {
					position: 'absolute',
					[horizontalKey]: originShift,
					width: this.laneWidth * laneCount + tileSpacing * (laneCount - 1) + padLeading + padTrailing,
					top: minItemTop,
					height: maxItemBottom - minItemTop,
				};
			}
			sectionIndex++;
		}
		this.laneFill = this.laneFill.map((height) => height - tileSpacing + padBottom);
		this.contentHeight = this.tallestLane();
		this.windowedTiles = {};
		this.layoutDirty = false;
	}

	selectWindow(start: number, end: number): void {
		this.rebuildLayout();
		const {tileKeyOf, tileBoxes} = this;
		this.windowedTiles = {};
		let sectionIndex = 0;
		while (sectionIndex < this.sectionSizes.length) {
			const sectionLength = this.sectionSizes[sectionIndex];
			const sectionKey = getSectionKey(sectionIndex);
			const sectionExtent = tileBoxes[sectionKey];
			if (sectionExtent == null) {
				sectionIndex++;
				continue;
			}
			const {top} = sectionExtent;
			const bottom = top + sectionExtent.height;
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
			this.windowedTiles[sectionKey] = [];
			while (itemIndex >= 0 && itemIndex < sectionLength) {
				const tileKey = tileKeyOf(sectionIndex, itemIndex);
				const tileBox = tileKey != null ? tileBoxes[tileKey] : null;
				if (tileKey == null || tileBox == null) {
					itemIndex += direction;
					continue;
				}
				const {top: tileTop, height: tileHeight} = tileBox;
				const tileAbsoluteTop = tileTop + top;
				if (tileAbsoluteTop > start - tileHeight && tileAbsoluteTop < end) {
					if (direction === -1) {
						this.windowedTiles[sectionKey].unshift([tileKey, sectionIndex, itemIndex]);
					} else {
						this.windowedTiles[sectionKey].push([tileKey, sectionIndex, itemIndex]);
					}
				}
				itemIndex += direction;
			}
			if (top < start && bottom > end) break;
			sectionIndex++;
		}
	}

	private tallestLane(laneFill: Array<number> = this.laneFill): number {
		return laneFill.reduce((max, height) => Math.max(max, height), 0);
	}

	readLayout(): {
		tileBoxes: Record<string, TileBox>;
		navGrid: NavGrid;
		windowedTiles: Record<string, SectionWindow>;
		contentHeight: number;
	} {
		return {
			tileBoxes: this.tileBoxes,
			navGrid: this.navGrid,
			windowedTiles: this.windowedTiles,
			contentHeight: this.contentHeight,
		};
	}
}
