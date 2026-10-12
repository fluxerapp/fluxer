// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {useVirtualPickerGrid} from '@app/features/channel/components/pickers/shared/useVirtualPickerGrid';
import type {ScrollerHandle} from '@app/features/ui/components/Scroller';
import {act} from 'react';
import {createRoot, type Root} from 'react-dom/client';
import {afterEach, beforeEach, expect, test, vi} from 'vitest';

type Row = {type: 'header'; category: string; index: number} | {type: 'items'; items: Array<string>; index: number};
const rows: Array<Row> = [
	{type: 'header', category: 'guild', index: 0},
	{type: 'items', items: ['first', 'second'], index: 1},
];
const getItems = (row: Row) => (row.type === 'header' ? null : row.items);
const scrollTo = vi.fn();
const revealRange = vi.fn();
const scrollerRef = {current: {scrollTo, revealRange} as unknown as ScrollerHandle};
const listMetrics = {origin: 10, viewportHeight: 100, gridWidth: 200};
let controller: ReturnType<typeof useVirtualPickerGrid<string, Row>>;
let root: Root;
let container: HTMLDivElement;

function Harness({rowHeightPx = 128, zoomLevel = 1}: {rowHeightPx?: number; zoomLevel?: number}) {
	controller = useVirtualPickerGrid({
		rows,
		getItems,
		rowOffsets: [0, 40, 40 + rowHeightPx],
		scrollerRef,
		scrollTop: 0,
		rowHeightPx,
		listMetrics,
		leadRows: 2,
		zoomLevel,
		renderedCount: 2,
	});
	return null;
}

beforeEach(() => {
	(globalThis as {IS_REACT_ACT_ENVIRONMENT?: boolean}).IS_REACT_ACT_ENVIRONMENT = true;
	scrollTo.mockClear();
	revealRange.mockClear();
	container = document.createElement('div');
	document.body.appendChild(container);
	root = createRoot(container);
	act(() => root.render(<Harness data-flx="channel.pickers.shared.use-virtual-picker-grid-test.harness" />));
});
afterEach(() => {
	act(() => root.unmount());
	container.remove();
});

test('keyboard selection skips headers and reveals the item using the measured list origin', () => {
	act(() => controller.handleSelectionChange(0, 1, true));
	expect(controller.hovered).toBe('second');
	expect(controller.selectedRow).toBe(0);
	expect(controller.selectAt(0, 0)).toBe('first');
	expect(revealRange).toHaveBeenLastCalledWith({start: 50, end: 178});
	controller.handleCategoryClick('guild');
	expect(scrollTo).toHaveBeenCalledWith({to: 10});
});

test('zoom changes preserve selection and update its reveal height', () => {
	act(() => controller.handleSelectionChange(0, 0, true));
	act(() =>
		root.render(
			<Harness
				rowHeightPx={256}
				zoomLevel={2}
				data-flx="channel.pickers.shared.use-virtual-picker-grid-test.harness--2"
			/>,
		),
	);
	expect(controller.hovered).toBe('first');
	expect(revealRange).toHaveBeenLastCalledWith({start: 50, end: 306});
});

test('pointer hover updates selection without forcing a scroll', () => {
	act(() => controller.handleHover('second', 0, 1));
	expect(controller.selectedColumn).toBe(1);
	expect(controller.shouldScrollOnSelection).toBe(false);
	expect(revealRange).not.toHaveBeenCalled();
	act(() => controller.handleHover(null));
	expect(controller.hovered).toBeNull();
});
