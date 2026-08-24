// SPDX-License-Identifier: AGPL-3.0-or-later

import {ALL_RESIZE_EDGES, type ResizeEdge} from '@app/features/ui/floating_pane/FloatingPaneMath';
import type {ResizablePaneHandleProps} from '@app/features/ui/hooks/useResizablePane';
import type {PopoutPosition} from '@app/features/ui/popover';
import {usePopoutResizePlacement} from '@app/features/ui/popover/PopoutResizePositionContext';
import styles from '@app/features/ui/resizable_pane/ResizablePaneHandles.module.css';
import type {MessageDescriptor} from '@lingui/core';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';

const EDGES_AWAY_FROM_ANCHOR: Readonly<Record<PopoutPosition, ReadonlyArray<ResizeEdge>>> = {
	top: ['top'],
	bottom: ['bottom'],
	left: ['left'],
	right: ['right'],
	'top-start': ['top', 'right', 'top-right'],
	'top-end': ['top', 'left', 'top-left'],
	'bottom-start': ['bottom', 'right', 'bottom-right'],
	'bottom-end': ['bottom', 'left', 'bottom-left'],
	'left-start': ['left', 'bottom', 'bottom-left'],
	'left-end': ['left', 'top', 'top-left'],
	'right-start': ['right', 'bottom', 'bottom-right'],
	'right-end': ['right', 'top', 'top-right'],
};

export function getResizablePaneHandleEdges(placement: PopoutPosition | null): ReadonlyArray<ResizeEdge> {
	if (placement == null) return ALL_RESIZE_EDGES;
	return EDGES_AWAY_FROM_ANCHOR[placement];
}

const RESIZABLE_PANE_HANDLE_KEY_SHORTCUTS = 'ArrowLeft ArrowRight ArrowUp ArrowDown Home End Enter Backspace Delete';

export type ResizablePaneHandleLabels = Readonly<Record<ResizeEdge, MessageDescriptor>>;

const EDGE_CLASS_NAMES: Readonly<Record<ResizeEdge, string>> = {
	top: styles.edgeTop,
	bottom: styles.edgeBottom,
	left: styles.edgeLeft,
	right: styles.edgeRight,
	'top-left': styles.cornerTopLeft,
	'top-right': styles.cornerTopRight,
	'bottom-left': styles.cornerBottomLeft,
	'bottom-right': styles.cornerBottomRight,
};

export interface ResizablePaneHandlesProps {
	getHandleProps: (edge: ResizeEdge) => ResizablePaneHandleProps;
	labels: ResizablePaneHandleLabels;
}

export function ResizablePaneHandles({getHandleProps, labels}: ResizablePaneHandlesProps) {
	const {i18n} = useLingui();
	const placement = usePopoutResizePlacement();
	return (
		<>
			{getResizablePaneHandleEdges(placement).map((edge) => (
				<button
					key={edge}
					type="button"
					aria-label={i18n._(labels[edge])}
					aria-keyshortcuts={RESIZABLE_PANE_HANDLE_KEY_SHORTCUTS}
					className={clsx(styles.handle, EDGE_CLASS_NAMES[edge])}
					data-resizable-pane-edge={edge}
					data-flx="ui.resizable-pane.resizable-pane-handles.handle.button"
					{...getHandleProps(edge)}
				/>
			))}
		</>
	);
}
