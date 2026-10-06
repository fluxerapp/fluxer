// SPDX-License-Identifier: AGPL-3.0-or-later

export type {
	Corner,
	DragBounds,
	FlingOptions,
	FloatingPaneGeometry,
	Point,
	ResizeEdge,
	ResizeResult,
	ResizeStart,
	Size,
	WidthRange,
} from '@app/features/ui/floating_pane/FloatingPaneMath';
export {
	ALL_CORNERS,
	ALL_RESIZE_EDGES,
	clampPoint,
	clampWidth,
	computeResize,
	DEFAULT_FLING_OPTIONS,
	getCornerPoint,
	getDragBounds,
	getEffectiveWidthRange,
	getPaneHeight,
	pickCornerForFling,
	reconcileToGeometry,
	snapPointToCorner,
} from '@app/features/ui/floating_pane/FloatingPaneMath';
export type {FloatingPaneResizeHandlesProps} from '@app/features/ui/floating_pane/FloatingPaneResizeHandles';
export {FloatingPaneResizeHandles} from '@app/features/ui/floating_pane/FloatingPaneResizeHandles';
export type {
	FloatingPanePointerHandlers,
	FloatingPaneResult,
	UseFloatingPaneOptions,
} from '@app/features/ui/floating_pane/useFloatingPane';
export {useFloatingPane} from '@app/features/ui/floating_pane/useFloatingPane';
