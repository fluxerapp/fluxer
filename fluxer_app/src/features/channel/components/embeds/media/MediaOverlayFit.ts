// SPDX-License-Identifier: AGPL-3.0-or-later

const OVERLAY_ACTION_ITEM_SIZE = 28;
const OVERLAY_ACTION_GAP = 2;
const OVERLAY_ACTION_PADDING = 3;
const OVERLAY_EDGE_INSET = 8;
const MAX_OVERLAY_ACTIONS = 3;

const OVERLAY_TOOLBAR_WIDTH =
	OVERLAY_ACTION_PADDING * 2 +
	MAX_OVERLAY_ACTIONS * OVERLAY_ACTION_ITEM_SIZE +
	(MAX_OVERLAY_ACTIONS - 1) * OVERLAY_ACTION_GAP;
const OVERLAY_TOOLBAR_HEIGHT = OVERLAY_ACTION_PADDING * 2 + OVERLAY_ACTION_ITEM_SIZE;

export const MIN_WIDTH_FOR_OVERLAYS = OVERLAY_TOOLBAR_WIDTH + OVERLAY_EDGE_INSET * 2;
export const MIN_HEIGHT_FOR_OVERLAYS = OVERLAY_TOOLBAR_HEIGHT + OVERLAY_EDGE_INSET * 2;

export function shouldShowOverlays(renderedWidth?: number, renderedHeight?: number): boolean {
	if (renderedWidth === undefined || renderedHeight === undefined) {
		return true;
	}
	return renderedWidth >= MIN_WIDTH_FOR_OVERLAYS && renderedHeight >= MIN_HEIGHT_FOR_OVERLAYS;
}
