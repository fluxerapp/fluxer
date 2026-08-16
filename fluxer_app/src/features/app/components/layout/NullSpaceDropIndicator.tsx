// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/layout/NullSpaceDropIndicator.module.css';
import {type DragItem, DragItemType, type DropResult} from '@app/features/app/components/layout/types/DndTypes';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {useCallback} from 'react';
import type {ConnectableElement} from 'react-dnd';
import {useDrop} from 'react-dnd';

interface NullSpaceDropIndicatorProps {
	isDraggingAnything: boolean;
	onChannelDrop?: (item: DragItem, result: DropResult) => void;
	variant?: 'top' | 'bottom';
}

export const NullSpaceDropIndicator = observer(
	({isDraggingAnything, onChannelDrop, variant = 'top'}: NullSpaceDropIndicatorProps) => {
		const [{isOver, canDrop}, dropRef] = useDrop(
			() => ({
				accept: [DragItemType.CHANNEL, DragItemType.CATEGORY],
				drop: (item: DragItem): DropResult => {
					const result: DropResult =
						variant === 'top'
							? {targetId: 'null-space', position: 'before', targetParentId: null}
							: {targetId: 'trailing-space', position: 'after', targetParentId: null};
					onChannelDrop?.(item, result);
					return result;
				},
				collect: (monitor) => ({
					isOver: monitor.isOver({shallow: true}),
					canDrop: monitor.canDrop(),
				}),
			}),
			[onChannelDrop, variant],
		);
		const dropConnectorRef = useCallback(
			(node: ConnectableElement | null) => {
				dropRef(node);
			},
			[dropRef],
		);
		return (
			<div
				ref={dropConnectorRef}
				className={clsx(styles.container, isDraggingAnything ? styles.containerDragging : styles.containerNotDragging)}
				data-flx="app.null-space-drop-indicator.container"
			>
				<div
					className={clsx(
						styles.indicator,
						isOver && canDrop && isDraggingAnything ? styles.indicatorVisible : styles.indicatorHidden,
					)}
					data-flx="app.null-space-drop-indicator.indicator"
				/>
			</div>
		);
	},
);
