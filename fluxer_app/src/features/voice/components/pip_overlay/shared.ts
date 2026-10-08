// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {
	type Corner,
	clampWidth as clampPaneWidth,
	computeResize,
	type DragBounds,
	type FloatingPaneGeometry,
	getCornerPoint as getCornerOffset,
	getDragBounds as getPaneDragBounds,
	getPaneHeight,
	type Point,
	pickCornerForFling,
	type ResizeEdge,
	type ResizeStart,
} from '@app/features/ui/floating_pane';
import type {PiPContent} from '@app/features/ui/state/PiP';
import type {SpringOptions} from 'framer-motion';
import type {Room} from 'livekit-client';

const PIP_ASPECT_RATIO = 16 / 9;
const PIP_MAX_WIDTH = 720;
const PIP_MIN_WIDTH = 240;
const EDGE_PADDING = 20;

export type {Corner, ResizeEdge};

export interface CornerPosition {
	x: number;
	y: number;
}

export interface ResizeListeners {
	move: (event: PointerEvent) => void;
	up: (event: PointerEvent) => void;
}

export interface ResizeState {
	pointerId: number;
	edge: ResizeEdge;
	startX: number;
	startY: number;
	startWidth: number;
	startPosX: number;
	startPosY: number;
}

export const SNAP_SPRING: SpringOptions = {
	stiffness: 520,
	damping: 42,
	mass: 0.9,
	bounce: 0.35,
};
export const INTERACTION_SPRING: SpringOptions = {
	stiffness: 520,
	damping: 38,
	mass: 0.8,
};
export const pipOverlayLogger = new Logger('PiPOverlay');

function buildGeometry(viewportWidth: number, viewportHeight: number, titlebarHeight: number): FloatingPaneGeometry {
	return {
		viewport: {width: viewportWidth, height: viewportHeight},
		edgePadding: EDGE_PADDING,
		topInset: titlebarHeight,
		aspectRatio: PIP_ASPECT_RATIO,
		minWidth: PIP_MIN_WIDTH,
		maxWidth: PIP_MAX_WIDTH,
	};
}

export function clamp(value: number, min: number, max: number): number {
	return Math.min(max, Math.max(min, value));
}

export function getPiPHeight(width: number): number {
	return getPaneHeight(width, PIP_ASPECT_RATIO);
}

export function clampPiPWidth(value: number, viewportWidth: number, viewportHeight: number): number {
	return clampPaneWidth(value, buildGeometry(viewportWidth, viewportHeight, getTitlebarHeight()));
}

export function getTitlebarHeight(): number {
	if (typeof window === 'undefined' || typeof document === 'undefined') return 0;
	const raw = getComputedStyle(document.documentElement).getPropertyValue('--native-titlebar-height').trim();
	const parsed = Number.parseFloat(raw);
	return Number.isFinite(parsed) ? parsed : 0;
}

export function getDragBounds(
	viewportWidth: number,
	viewportHeight: number,
	pipWidth: number,
	_pipHeight: number,
	titlebarHeight = getTitlebarHeight(),
): DragBounds {
	return getPaneDragBounds(buildGeometry(viewportWidth, viewportHeight, titlebarHeight), pipWidth);
}

export function getCornerPositions(
	viewportWidth: number,
	viewportHeight: number,
	pipWidth: number,
	_pipHeight: number,
	titlebarHeight = getTitlebarHeight(),
): Record<Corner, CornerPosition> {
	const bounds = getPaneDragBounds(buildGeometry(viewportWidth, viewportHeight, titlebarHeight), pipWidth);
	return {
		'top-left': getCornerOffset('top-left', bounds),
		'top-right': getCornerOffset('top-right', bounds),
		'bottom-right': getCornerOffset('bottom-right', bounds),
		'bottom-left': getCornerOffset('bottom-left', bounds),
	};
}

export function pickCornerOnRelease(
	currentX: number,
	currentY: number,
	velocityX: number,
	velocityY: number,
	_corners: Record<Corner, CornerPosition>,
	bounds: DragBounds,
): Corner {
	return pickCornerForFling({x: currentX, y: currentY}, {x: velocityX, y: velocityY}, bounds, {
		lookaheadSeconds: 0.25,
		strongVelocity: 550,
		axisVelocityFloor: 180,
	});
}

export function computePiPResize(
	state: ResizeState,
	pointerX: number,
	pointerY: number,
	viewportWidth: number,
	viewportHeight: number,
	titlebarHeight = getTitlebarHeight(),
): {width: number; offset: Point} {
	const start: ResizeStart = {
		edge: state.edge,
		startWidth: state.startWidth,
		pointerStartX: state.startX,
		pointerStartY: state.startY,
		paneStartX: state.startPosX,
		paneStartY: state.startPosY,
	};
	return computeResize(start, pointerX, pointerY, buildGeometry(viewportWidth, viewportHeight, titlebarHeight));
}

export interface PiPOverlayInnerProps {
	content: PiPContent;
	room: Room | null;
}
