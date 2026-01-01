/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

const SCROLLER_DRAG_ATTR = 'data-scroller-dragging';

let activeDrags = 0;
let pendingClear: number | null = null;

const getRoot = () => document.documentElement;

const updateAttribute = (isDragging: boolean) => {
	const root = getRoot();

	if (isDragging) {
		root.setAttribute(SCROLLER_DRAG_ATTR, 'true');
	} else {
		root.removeAttribute(SCROLLER_DRAG_ATTR);
	}
};

const clearPendingTimeout = () => {
	if (pendingClear !== null) {
		window.clearTimeout(pendingClear);
		pendingClear = null;
	}
};

const decrementDragCount = () => {
	activeDrags = Math.max(0, activeDrags - 1);
	if (activeDrags === 0) {
		updateAttribute(false);
	}
};

export const beginScrollbarDrag = () => {
	clearPendingTimeout();
	activeDrags += 1;
	updateAttribute(true);
};

export const endScrollbarDrag = () => {
	clearPendingTimeout();
	decrementDragCount();
};

export const endScrollbarDragDeferred = () => {
	clearPendingTimeout();

	pendingClear = window.setTimeout(() => {
		pendingClear = null;
		decrementDragCount();
	}, 0);
};

export const isScrollbarDragActive = () => activeDrags > 0;
