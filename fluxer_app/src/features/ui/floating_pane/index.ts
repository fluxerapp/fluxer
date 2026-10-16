// SPDX-License-Identifier: AGPL-3.0-or-later

export type {
	Corner,
	DragBounds,
	FloatingPaneGeometry,
	Point,
	ResizeEdge,
	ResizeStart,
} from '@app/features/ui/floating_pane/FloatingPaneMath';
export {
	clampWidth,
	computeResize,
	getCornerPoint,
	getDragBounds,
	getPaneHeight,
	pickCornerForFling,
} from '@app/features/ui/floating_pane/FloatingPaneMath';
export {FloatingPaneResizeHandles} from '@app/features/ui/floating_pane/FloatingPaneResizeHandles';
