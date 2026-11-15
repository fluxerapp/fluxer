// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Corner, ResizeEdge} from '@app/features/ui/floating_pane/FloatingPaneMath';
import styles from '@app/features/ui/floating_pane/FloatingPaneResizeHandles.module.css';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import FocusRingContext from '@app/features/ui/focus_ring/FocusRingContext';
import {clsx} from 'clsx';
import type React from 'react';
import {useCallback, useContext, useMemo} from 'react';

type ResizePointerHandler = (event: React.PointerEvent<HTMLElement>) => void;

const HANDLE_CLASS_BY_EDGE: Record<ResizeEdge, string> = {
	top: styles.edgeTop,
	bottom: styles.edgeBottom,
	left: styles.edgeLeft,
	right: styles.edgeRight,
	'top-left': styles.cornerTopLeft,
	'top-right': styles.cornerTopRight,
	'bottom-left': styles.cornerBottomLeft,
	'bottom-right': styles.cornerBottomRight,
};

const EDGES = Object.keys(HANDLE_CLASS_BY_EDGE) as ReadonlyArray<ResizeEdge>;

export interface FloatingPaneResizeHandlesProps {
	createResizeHandler: (edge: ResizeEdge) => ResizePointerHandler;
	label: string;
	gripCorner: Corner;
	onGripKeyDown: (event: React.KeyboardEvent<HTMLButtonElement>) => void;
	onGripDoubleClick: (event: React.MouseEvent<HTMLButtonElement>) => void;
}

function stopDoubleClick(event: React.MouseEvent) {
	event.stopPropagation();
}

export function FloatingPaneResizeHandles({
	createResizeHandler,
	label,
	gripCorner,
	onGripKeyDown,
	onGripDoubleClick,
}: FloatingPaneResizeHandlesProps) {
	const ringContext = useContext(FocusRingContext);
	const handleGripKeyDown = useCallback(
		(event: React.KeyboardEvent<HTMLButtonElement>) => {
			onGripKeyDown(event);
			requestAnimationFrame(() => ringContext.invalidate());
		},
		[onGripKeyDown, ringContext],
	);
	const handlerByEdge = useMemo(() => {
		const entries = {} as Record<ResizeEdge, ResizePointerHandler>;
		for (const edge of EDGES) {
			entries[edge] = createResizeHandler(edge);
		}
		return entries;
	}, [createResizeHandler]);
	return (
		<>
			{EDGES.filter((edge) => edge !== gripCorner).map((edge) => (
				<div
					key={edge}
					aria-hidden
					className={clsx(styles.handle, HANDLE_CLASS_BY_EDGE[edge])}
					onPointerDown={handlerByEdge[edge]}
					onDoubleClick={stopDoubleClick}
					data-floating-pane-resize-edge={edge}
					data-flx="ui.floating-pane.floating-pane-resize-handles.handle"
				/>
			))}
			<FocusRing offset={4} data-flx="ui.floating-pane.floating-pane-resize-handles.focus-ring">
				<button
					type="button"
					aria-label={label}
					className={clsx(styles.handle, styles.grip, HANDLE_CLASS_BY_EDGE[gripCorner])}
					onPointerDown={handlerByEdge[gripCorner]}
					onKeyDown={handleGripKeyDown}
					onDoubleClick={onGripDoubleClick}
					data-floating-pane-resize-edge={gripCorner}
					data-flx="ui.floating-pane.floating-pane-resize-handles.grip"
				>
					<span className={styles.gripMark} data-flx="ui.floating-pane.floating-pane-resize-handles.grip-mark" />
				</button>
			</FocusRing>
		</>
	);
}
