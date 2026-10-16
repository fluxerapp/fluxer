// SPDX-License-Identifier: AGPL-3.0-or-later
import {getRowWindow} from '@app/features/channel/components/pickers/shared/PickerRowLayout';
import type {ScrollerHandle} from '@app/features/ui/components/Scroller';
import type {RefObject} from 'react';
import {useCallback, useEffect, useLayoutEffect, useMemo, useState} from 'react';

interface PickerListMetrics {
	origin: number;
	viewportHeight: number;
	gridWidth: number;
}
export function usePickerListMetrics(
	scrollerRef: RefObject<ScrollerHandle | null>,
	rowListRef: RefObject<HTMLDivElement | null>,
): PickerListMetrics {
	const [metrics, setMetrics] = useState({origin: 0, viewportHeight: 0, gridWidth: 0});
	useLayoutEffect(() => {
		const list = rowListRef.current,
			scroller = scrollerRef.current?.getViewportElement();
		if (!list || !scroller) return;
		const origin = Math.round(
			list.getBoundingClientRect().top - scroller.getBoundingClientRect().top + scroller.scrollTop,
		);
		const viewportHeight = scroller.clientHeight,
			gridWidth = list.clientWidth;
		setMetrics((current) =>
			current.origin === origin && current.viewportHeight === viewportHeight && current.gridWidth === gridWidth
				? current
				: {origin, viewportHeight, gridWidth},
		);
	});
	return metrics;
}
export function useVirtualPickerGrid<T, R extends {type: string; index: number; category?: string}>({
	rows,
	getItems,
	rowOffsets,
	scrollerRef,
	scrollTop,
	rowHeightPx,
	listMetrics,
	leadRows,
	zoomLevel,
	renderedCount,
}: {
	rows: ReadonlyArray<R>;
	getItems: (row: R) => ReadonlyArray<T> | null;
	rowOffsets: ReadonlyArray<number>;
	scrollerRef: RefObject<ScrollerHandle | null>;
	scrollTop: number;
	rowHeightPx: number;
	listMetrics: PickerListMetrics;
	leadRows: number;
	zoomLevel: number;
	renderedCount: number;
}) {
	const [hovered, setHovered] = useState<T | null>(null),
		[selectedRow, setSelectedRow] = useState(-1),
		[selectedColumn, setSelectedColumn] = useState(-1),
		[shouldScrollOnSelection, setShouldScrollOnSelection] = useState(false);
	const {itemRows, sections, rowIndexes, rowStarts, categoryRowIndexes} = useMemo(() => {
		const itemRows: Array<ReadonlyArray<T>> = [],
			sections: Array<number> = [],
			rowIndexes: Array<number> = [],
			rowStarts: Array<number> = [],
			categoryRowIndexes = new Map<string, number>();
		for (let index = 0; index < rows.length; index++) {
			const row = rows[index]!;
			const items = getItems(row);
			rowIndexes.push(itemRows.length);
			if (items) {
				rowStarts.push(index);
				itemRows.push(items);
				sections.push(items.length);
			} else if (row.category != null) categoryRowIndexes.set(row.category, index);
		}
		return {itemRows, sections, rowIndexes, rowStarts, categoryRowIndexes};
	}, [rows, getItems, zoomLevel]);
	const rowWindow = useMemo(
		() => getRowWindow(rowOffsets, scrollTop - listMetrics.origin, listMetrics.viewportHeight, leadRows),
		[rowOffsets, scrollTop, listMetrics, leadRows, zoomLevel],
	);
	const handleCategoryClick = useCallback(
		(category: string) => {
			const index = categoryRowIndexes.get(category);
			if (index != null) scrollerRef.current?.scrollTo({to: listMetrics.origin + rowOffsets[index]!});
		},
		[categoryRowIndexes, scrollerRef, listMetrics.origin, rowOffsets],
	);
	const selectAt = useCallback(
		(row: number | null, column: number | null): T | undefined =>
			row == null || column == null ? undefined : itemRows[row]?.[column],
		[itemRows],
	);
	const handleSelectionChange = useCallback(
		(row: number, column: number, shouldScroll = false) => {
			if (row < 0 || column < 0) return;
			setSelectedRow(row);
			setSelectedColumn(column);
			setShouldScrollOnSelection(shouldScroll);
			const item = selectAt(row, column);
			if (item !== undefined) setHovered(item);
		},
		[selectAt],
	);
	const handleHover = useCallback(
		(item: T | null, row?: number, column?: number) => {
			setHovered(item);
			if (item && row !== undefined && column !== undefined) handleSelectionChange(row, column, false);
		},
		[handleSelectionChange],
	);
	useEffect(() => {
		if (renderedCount > 0 && selectedRow === 0 && selectedColumn === 0 && !hovered) handleSelectionChange(0, 0, false);
	}, [renderedCount, selectedRow, selectedColumn, hovered, handleSelectionChange]);
	useEffect(() => {
		if (!shouldScrollOnSelection || selectedRow < 0) return;
		const index = rowStarts[selectedRow];
		if (index == null) return;
		const start = listMetrics.origin + rowOffsets[index]!;
		scrollerRef.current?.revealRange({start, end: start + rowHeightPx});
	}, [shouldScrollOnSelection, selectedRow, rowStarts, rowOffsets, listMetrics.origin, rowHeightPx, scrollerRef]);
	return {
		sections,
		rowIndexes,
		rowStarts,
		categoryRowIndexes,
		listMetrics,
		rowWindow,
		contentHeight: rowOffsets[rowOffsets.length - 1]!,
		selectedRow,
		selectedColumn,
		hovered,
		shouldScrollOnSelection,
		setSelectedRow,
		setSelectedColumn,
		setHovered,
		setShouldScrollOnSelection,
		handleSelectionChange,
		handleHover,
		handleCategoryClick,
		selectAt,
	};
}
