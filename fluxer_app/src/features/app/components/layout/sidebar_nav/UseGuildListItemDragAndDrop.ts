// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type GuildItemReorderIndicator,
	GuildReorderStateMachine,
	type GuildReorderTarget,
	GuildReorderTargetKind,
} from '@app/features/app/components/layout/dnd/GuildReorderStateMachine';
import {useDragTargetRect} from '@app/features/app/components/layout/dnd/useDragTargetRect';
import {
	DragItemType,
	DropPlacement,
	type GuildDragItem,
	type GuildDropResult,
} from '@app/features/app/components/layout/types/DndTypes';
import {useCallback, useEffect, useMemo, useState} from 'react';
import {useDrag, useDrop} from 'react-dnd';
import {getEmptyImage} from 'react-dnd-html5-backend';

interface GuildDropState {
	readonly combineSourceGuildId: string | null;
	readonly indicator: GuildItemReorderIndicator | null;
}

interface UseGuildListItemDragAndDropOptions {
	readonly guildId: string;
	readonly insideFolderId: number | null;
	readonly isLastInsideFolder: boolean;
	readonly disableDrag: boolean;
	readonly itemRef: React.RefObject<HTMLElement | null>;
	readonly onGuildDrop: ((item: GuildDragItem, result: GuildDropResult) => void) | null;
	readonly onDragStateChange: ((item: GuildDragItem | null) => void) | null;
}

export interface GuildListItemDragAndDrop {
	readonly combineSourceGuildId: string | null;
	readonly dropIndicator: GuildItemReorderIndicator | null;
	readonly isDragging: boolean;
	readonly dragConnectorRef: (node: HTMLElement | null) => void;
	readonly dropConnectorRef: (node: HTMLElement | null) => void;
}

export function useGuildListItemDragAndDrop({
	guildId,
	insideFolderId,
	isLastInsideFolder,
	disableDrag,
	itemRef,
	onGuildDrop,
	onDragStateChange,
}: UseGuildListItemDragAndDropOptions): GuildListItemDragAndDrop {
	const [dropIndicator, setDropIndicator] = useState<GuildItemReorderIndicator | null>(null);
	const [combineSourceGuildId, setCombineSourceGuildId] = useState<string | null>(null);
	const getDropTargetRect = useDragTargetRect(itemRef);
	const setGuildDropState = useCallback(({combineSourceGuildId, indicator}: GuildDropState) => {
		setDropIndicator(indicator);
		setCombineSourceGuildId(combineSourceGuildId);
	}, []);
	const resetGuildDropState = useCallback(() => {
		setGuildDropState({combineSourceGuildId: null, indicator: null});
	}, [setGuildDropState]);
	const dragItemData = useMemo<GuildDragItem>(
		() => ({type: DragItemType.GUILD_ITEM, id: guildId, isFolder: false, folderId: insideFolderId}),
		[guildId, insideFolderId],
	);
	const dropTargetData = useMemo<GuildReorderTarget>(() => {
		if (insideFolderId == null) {
			return {id: guildId, kind: GuildReorderTargetKind.TOP_LEVEL_GUILD};
		}
		return {
			id: guildId,
			kind: GuildReorderTargetKind.FOLDER_GUILD,
			folderId: insideFolderId,
			isTerminal: isLastInsideFolder,
		};
	}, [guildId, insideFolderId, isLastInsideFolder]);
	const [{isDragging}, dragRef, preview] = useDrag(
		() => ({
			type: DragItemType.GUILD_ITEM,
			item: () => {
				if (onDragStateChange != null) {
					onDragStateChange(dragItemData);
				}
				return dragItemData;
			},
			canDrag: !disableDrag,
			collect: (monitor) => ({isDragging: monitor.isDragging()}),
			end: () => {
				if (onDragStateChange != null) {
					onDragStateChange(null);
				}
				resetGuildDropState();
			},
		}),
		[disableDrag, dragItemData, onDragStateChange, resetGuildDropState],
	);
	const [{isOver}, dropRef] = useDrop(
		() => ({
			accept: [DragItemType.GUILD_ITEM, DragItemType.GUILD_FOLDER],
			canDrop: (item: GuildDragItem) => GuildReorderStateMachine.canDrop({item, target: dropTargetData}),
			hover: (item: GuildDragItem, monitor) => {
				if (!GuildReorderStateMachine.canDrop({item, target: dropTargetData})) {
					resetGuildDropState();
					return;
				}
				const clientOffset = monitor.getClientOffset();
				if (clientOffset == null) return;
				const targetRect = getDropTargetRect();
				if (targetRect == null) return;
				const intent = GuildReorderStateMachine.selectIntent({item, target: dropTargetData, clientOffset, targetRect});
				if (intent == null || intent.indicator === DropPlacement.INSIDE) {
					resetGuildDropState();
					return;
				}
				setGuildDropState({combineSourceGuildId: intent.combineSourceGuildId, indicator: intent.indicator});
			},
			drop: (item: GuildDragItem, monitor): GuildDropResult | undefined => {
				if (!monitor.canDrop()) {
					resetGuildDropState();
					return;
				}
				const node = itemRef.current;
				const clientOffset = monitor.getClientOffset();
				if (node == null || clientOffset == null) return;
				const intent = GuildReorderStateMachine.selectIntent({
					item,
					target: dropTargetData,
					clientOffset,
					targetRect: node.getBoundingClientRect(),
				});
				if (intent == null) {
					resetGuildDropState();
					return;
				}
				if (onGuildDrop != null) {
					onGuildDrop(item, intent.result);
				}
				resetGuildDropState();
				return intent.result;
			},
			collect: (monitor) => ({isOver: monitor.isOver({shallow: true})}),
		}),
		[dropTargetData, getDropTargetRect, itemRef, onGuildDrop, resetGuildDropState, setGuildDropState],
	);
	useEffect(() => {
		if (!isOver) resetGuildDropState();
	}, [isOver, resetGuildDropState]);
	useEffect(() => {
		preview(getEmptyImage(), {captureDraggingState: true});
	}, [preview]);
	const dragConnectorRef = useCallback((node: HTMLElement | null) => dragRef(node), [dragRef]);
	const dropConnectorRef = useCallback((node: HTMLElement | null) => dropRef(node), [dropRef]);
	return {combineSourceGuildId, dropIndicator, isDragging, dragConnectorRef, dropConnectorRef};
}
