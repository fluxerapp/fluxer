// SPDX-License-Identifier: AGPL-3.0-or-later

import {isKeyboardDragActive} from '@app/features/app/components/layout/dnd/KeyboardDragMode';
import {type AccountTileDragItem, DragItemType} from '@app/features/app/components/layout/types/DndTypes';
import {
	type AccountDropTarget,
	type AccountGridLayout,
	buildAccountGridLayout,
	initialAccountDropTarget,
	resolveAccountDropTarget,
} from '@app/features/auth/components/accounts/account_switcher/AccountTileDropIndex';
import {useCallback, useRef} from 'react';
import {useDrag, useDrop} from 'react-dnd';

export interface AccountTileReorderHandlers {
	readonly onMoveTile: (draggedKey: string, index: number) => void;
	readonly onDropTile: () => void;
	readonly onDragEnd: () => void;
}

interface UseAccountTileReorderOptions extends AccountTileReorderHandlers {
	readonly accountKey: string;
	readonly enabled: boolean;
	readonly order: ReadonlyArray<string>;
}

export interface AccountTileReorder {
	readonly isDragging: boolean;
	readonly dragRef: (node: HTMLElement | null) => void;
	readonly dropRef: (node: HTMLElement | null) => void;
}

export function useAccountTileReorder({
	accountKey,
	enabled,
	order,
	onMoveTile,
	onDropTile,
	onDragEnd,
}: UseAccountTileReorderOptions): AccountTileReorder {
	const [{isDragging}, connectDrag] = useDrag(
		() => ({
			type: DragItemType.ACCOUNT_TILE,
			item: (): AccountTileDragItem => ({type: DragItemType.ACCOUNT_TILE, accountKey}),
			canDrag: enabled,
			collect: (monitor) => ({isDragging: monitor.isDragging()}),
			end: () => onDragEnd(),
		}),
		[accountKey, enabled, onDragEnd],
	);
	const [, connectDrop] = useDrop(
		() => ({
			accept: DragItemType.ACCOUNT_TILE,
			hover: (dragged: AccountTileDragItem) => {
				const target = order.indexOf(accountKey);
				if (isKeyboardDragActive() && dragged.accountKey !== accountKey && target >= 0) {
					onMoveTile(dragged.accountKey, target);
				}
			},
			drop: () => {
				onDropTile();
			},
		}),
		[accountKey, onDropTile, onMoveTile, order],
	);
	const dragRef = useCallback((node: HTMLElement | null) => connectDrag(node), [connectDrag]);
	const dropRef = useCallback((node: HTMLElement | null) => connectDrop(node), [connectDrop]);
	return {isDragging, dragRef, dropRef};
}

interface AccountGridDragSession {
	readonly item: AccountTileDragItem;
	layout: AccountGridLayout;
	width: number;
	target: AccountDropTarget;
}

function measureAccountGrid(grid: HTMLElement): AccountGridLayout {
	const slots = Array.from(grid.querySelectorAll<HTMLElement>(':scope > [data-account-slot]'), (node) => {
		const gridIsParent = node.offsetParent === grid;
		return {
			left: node.offsetLeft + (gridIsParent ? grid.clientLeft : -grid.offsetLeft),
			top: node.offsetTop + (gridIsParent ? grid.clientTop : -grid.offsetTop),
			width: node.offsetWidth,
			height: node.offsetHeight,
		};
	});
	return buildAccountGridLayout(slots, getComputedStyle(grid).direction === 'rtl');
}

interface UseAccountGridReorderOptions {
	readonly order: ReadonlyArray<string>;
	readonly onMoveTile: (draggedKey: string, index: number) => void;
	readonly onDropTile: () => void;
}

export function useAccountGridReorder({
	order,
	onMoveTile,
	onDropTile,
}: UseAccountGridReorderOptions): (node: HTMLElement | null) => void {
	const gridNodeRef = useRef<HTMLElement | null>(null);
	const sessionRef = useRef<AccountGridDragSession | null>(null);
	const [, connectDrop] = useDrop(
		() => ({
			accept: DragItemType.ACCOUNT_TILE,
			hover: (dragged: AccountTileDragItem, monitor) => {
				const grid = gridNodeRef.current;
				const pointer = monitor.getClientOffset();
				if (grid == null || pointer == null || isKeyboardDragActive()) {
					return;
				}
				let session = sessionRef.current;
				if (session == null || session.item !== dragged) {
					const index = order.indexOf(dragged.accountKey);
					if (index < 0) {
						return;
					}
					const layout = measureAccountGrid(grid);
					session = {item: dragged, layout, width: grid.clientWidth, target: initialAccountDropTarget(layout, index)};
					sessionRef.current = session;
				} else if (session.width !== grid.clientWidth) {
					session.layout = measureAccountGrid(grid);
					session.width = grid.clientWidth;
					session.target = initialAccountDropTarget(session.layout, session.target.index);
				}
				const origin = grid.getBoundingClientRect();
				const previous = session.target;
				session.target = resolveAccountDropTarget(
					session.layout,
					{x: pointer.x - origin.left, y: pointer.y - origin.top},
					previous,
				);
				if (session.target.index !== previous.index) {
					onMoveTile(dragged.accountKey, session.target.index);
				}
			},
			drop: (_item, monitor) => {
				if (!monitor.didDrop()) {
					onDropTile();
				}
			},
		}),
		[onDropTile, onMoveTile, order],
	);
	return useCallback(
		(node: HTMLElement | null) => {
			gridNodeRef.current = node;
			connectDrop(node);
		},
		[connectDrop],
	);
}
