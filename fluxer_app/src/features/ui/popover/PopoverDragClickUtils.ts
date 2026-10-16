// SPDX-License-Identifier: AGPL-3.0-or-later

const CLICK_DRAG_TOLERANCE_PX = 3;

export interface PopoutClickStart {
	x: number;
	y: number;
	button: number;
}

interface PopoutClickGeometry {
	detail: number;
	button: number;
	clientX: number;
	clientY: number;
}

export function isPopoutDragClick(clickStart: PopoutClickStart | null, event: PopoutClickGeometry): boolean {
	if (event.detail === 0) {
		return false;
	}
	if (!clickStart || clickStart.button !== event.button) {
		return false;
	}
	return (
		Math.abs(event.clientX - clickStart.x) > CLICK_DRAG_TOLERANCE_PX ||
		Math.abs(event.clientY - clickStart.y) > CLICK_DRAG_TOLERANCE_PX
	);
}
