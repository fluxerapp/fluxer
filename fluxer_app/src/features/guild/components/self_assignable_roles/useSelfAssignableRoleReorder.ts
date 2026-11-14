// SPDX-License-Identifier: AGPL-3.0-or-later

import {computeVerticalDropPosition} from '@app/features/app/components/layout/dnd/DndDropPosition';
import {isKeyboardDragActive} from '@app/features/app/components/layout/dnd/KeyboardDragMode';
import {DragItemType, type SelfAssignableRoleDragItem} from '@app/features/app/components/layout/types/DndTypes';
import {useCallback, useRef} from 'react';
import {useDrag, useDrop, type XYCoord} from 'react-dnd';

export interface SelfAssignableRoleReorderHandlers {
	readonly onDragStart: (sectionKey: string) => void;
	readonly onMoveRole: (sectionKey: string, roleId: string, index: number) => void;
	readonly onDragEnd: (didDrop: boolean) => void;
}

interface UseSelfAssignableRoleReorderOptions extends SelfAssignableRoleReorderHandlers {
	readonly sectionKey: string;
	readonly roleId: string;
	readonly order: ReadonlyArray<string>;
	readonly enabled: boolean;
}

function pointerPastMiddle(node: HTMLElement, pointer: XYCoord): boolean {
	return computeVerticalDropPosition(pointer, node.getBoundingClientRect()) === 'after';
}

export function useSelfAssignableRoleReorder({
	sectionKey,
	roleId,
	order,
	enabled,
	onDragStart,
	onMoveRole,
	onDragEnd,
}: UseSelfAssignableRoleReorderOptions): {
	readonly isDragging: boolean;
	readonly rowRef: (node: HTMLElement | null) => void;
	readonly handleRef: (node: HTMLElement | null) => void;
} {
	const nodeRef = useRef<HTMLElement | null>(null);
	const [{isDragging}, connectDrag, connectDragPreview] = useDrag(
		() => ({
			type: DragItemType.SELF_ASSIGNABLE_ROLE,
			item: (): SelfAssignableRoleDragItem => {
				onDragStart(sectionKey);
				return {type: DragItemType.SELF_ASSIGNABLE_ROLE, sectionKey, roleId};
			},
			canDrag: enabled,
			collect: (monitor) => ({isDragging: monitor.isDragging()}),
			end: (_item, monitor) => onDragEnd(monitor.didDrop()),
		}),
		[enabled, onDragEnd, onDragStart, roleId, sectionKey],
	);
	const [, connectDrop] = useDrop(
		() => ({
			accept: DragItemType.SELF_ASSIGNABLE_ROLE,
			canDrop: (dragged: SelfAssignableRoleDragItem) => dragged.sectionKey === sectionKey,
			hover: (dragged: SelfAssignableRoleDragItem, monitor) => {
				if (isKeyboardDragActive() || dragged.sectionKey !== sectionKey || dragged.roleId === roleId) return;
				const from = order.indexOf(dragged.roleId);
				const to = order.indexOf(roleId);
				const node = nodeRef.current;
				const pointer = monitor.getClientOffset();
				if (from < 0 || to < 0 || node == null || pointer == null) return;
				if (pointerPastMiddle(node, pointer) === to > from) {
					onMoveRole(sectionKey, dragged.roleId, to);
				}
			},
			drop: (dragged: SelfAssignableRoleDragItem) => {
				const to = order.indexOf(roleId);
				if (isKeyboardDragActive() && to >= 0) {
					onMoveRole(sectionKey, dragged.roleId, to);
				}
			},
		}),
		[onMoveRole, order, roleId, sectionKey],
	);
	const rowRef = useCallback(
		(node: HTMLElement | null) => {
			nodeRef.current = node;
			connectDragPreview(node);
			connectDrop(node);
		},
		[connectDragPreview, connectDrop],
	);
	const handleRef = useCallback(
		(node: HTMLElement | null) => {
			connectDrag(node);
		},
		[connectDrag],
	);
	return {isDragging, rowRef, handleRef};
}

export function useSelfAssignableRoleListDrop(sectionKey: string): (node: HTMLElement | null) => void {
	const [, connectDrop] = useDrop(
		() => ({
			accept: DragItemType.SELF_ASSIGNABLE_ROLE,
			canDrop: (dragged: SelfAssignableRoleDragItem) => dragged.sectionKey === sectionKey && !isKeyboardDragActive(),
		}),
		[sectionKey],
	);
	return useCallback(
		(node: HTMLElement | null) => {
			connectDrop(node);
		},
		[connectDrop],
	);
}
