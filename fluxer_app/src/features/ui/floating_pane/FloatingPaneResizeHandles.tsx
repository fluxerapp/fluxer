// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ResizeEdge} from '@app/features/ui/floating_pane/FloatingPaneMath';
import styles from '@app/features/ui/floating_pane/FloatingPaneResizeHandles.module.css';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import type React from 'react';
import {useMemo} from 'react';

const RESIZE_HANDLE_LABEL = msg({
	message: 'Resize floating panel',
	comment: 'Aria label on a resize handle of a floating draggable panel (the floating call tile or PiP overlay).',
});

interface HandleSpec {
	edge: ResizeEdge;
	className: string;
}

const HANDLES: ReadonlyArray<HandleSpec> = [
	{edge: 'top', className: styles.edgeTop},
	{edge: 'bottom', className: styles.edgeBottom},
	{edge: 'left', className: styles.edgeLeft},
	{edge: 'right', className: styles.edgeRight},
	{edge: 'top-left', className: `${styles.handle} ${styles.cornerTopLeft}`},
	{edge: 'top-right', className: `${styles.handle} ${styles.cornerTopRight}`},
	{edge: 'bottom-left', className: `${styles.handle} ${styles.cornerBottomLeft}`},
	{edge: 'bottom-right', className: `${styles.handle} ${styles.cornerBottomRight}`},
];

export interface FloatingPaneResizeHandlesProps {
	createResizeHandler: (edge: ResizeEdge) => (event: React.PointerEvent<HTMLButtonElement>) => void;
	label: string | null;
}

export function FloatingPaneResizeHandles({createResizeHandler, label}: FloatingPaneResizeHandlesProps) {
	const {i18n} = useLingui();
	let resolvedLabel = i18n._(RESIZE_HANDLE_LABEL);
	if (label != null) resolvedLabel = label;
	const handlerByEdge = useMemo(() => {
		const entries = {} as Record<ResizeEdge, (event: React.PointerEvent<HTMLButtonElement>) => void>;
		for (const {edge} of HANDLES) {
			entries[edge] = createResizeHandler(edge);
		}
		return entries;
	}, [createResizeHandler]);
	return (
		<>
			{HANDLES.map(({edge, className}) => (
				<button
					key={edge}
					type="button"
					aria-label={resolvedLabel}
					className={className.includes(styles.handle) ? className : `${styles.handle} ${className}`}
					onPointerDown={handlerByEdge[edge]}
					onDoubleClick={(event) => event.stopPropagation()}
					data-floating-pane-resize-edge={edge}
					data-flx="ui.floating-pane.floating-pane-resize-handles.handle.edge.button"
				/>
			))}
		</>
	);
}
