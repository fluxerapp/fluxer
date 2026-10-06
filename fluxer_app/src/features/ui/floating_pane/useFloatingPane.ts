// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type Corner,
	clampPoint,
	clampWidth,
	computeResize,
	type FloatingPaneGeometry,
	getCornerPoint,
	getDragBounds,
	getPaneHeight,
	pickCornerForFling,
	type ResizeEdge,
	type ResizeStart,
	reconcileToGeometry,
} from '@app/features/ui/floating_pane/FloatingPaneMath';
import {appZoomLayoutPx} from '@app/features/ui/utils/AppZoomUtils';
import {canUseWindowFocusedHoverControls} from '@app/features/ui/utils/WindowFocusInteractionGuard';
import type React from 'react';
import {useCallback, useEffect, useLayoutEffect, useMemo, useRef, useState} from 'react';

const DRAG_ACTIVATION_DISTANCE_SQ = 9;

export interface UseFloatingPaneOptions {
	geometry: FloatingPaneGeometry;
	initialCorner: Corner;
	initialWidth: number;
	onCornerChange?: (corner: Corner) => void;
	onWidthChange?: (width: number) => void;
	requireFocusedWindow?: boolean;
	useFlingForCornerPick?: boolean;
}

export interface FloatingPanePointerHandlers {
	onPointerDown: (event: React.PointerEvent<HTMLElement>) => void;
}

export interface FloatingPaneResult {
	width: number;
	height: number;
	offset: {x: number; y: number};
	corner: Corner;
	isDragging: boolean;
	isResizing: boolean;
	hasPositioned: boolean;
	dragHandlers: FloatingPanePointerHandlers;
	createResizeHandler: (edge: ResizeEdge) => (event: React.PointerEvent<HTMLElement>) => void;
	suppressNextClick: boolean;
	consumeClickSuppression: () => boolean;
}

interface DragInternalState {
	pointerId: number;
	startClientX: number;
	startClientY: number;
	startOffsetX: number;
	startOffsetY: number;
	startTimestamp: number;
	lastClientX: number;
	lastClientY: number;
	lastTimestamp: number;
	velocityX: number;
	velocityY: number;
	dragActivated: boolean;
}

interface ResizeInternalState extends ResizeStart {
	pointerId: number;
}

export function useFloatingPane(options: UseFloatingPaneOptions): FloatingPaneResult {
	const {
		geometry,
		initialCorner,
		initialWidth,
		onCornerChange,
		onWidthChange,
		requireFocusedWindow = true,
		useFlingForCornerPick = false,
	} = options;
	const geometryRef = useRef(geometry);
	useEffect(() => {
		geometryRef.current = geometry;
	}, [geometry]);
	const [corner, setCorner] = useState<Corner>(initialCorner);
	const hasMeasuredViewport = geometry.viewport.width > 0 && geometry.viewport.height > 0;
	const [width, setWidth] = useState(() => (hasMeasuredViewport ? clampWidth(initialWidth, geometry) : initialWidth));
	const [offset, setOffset] = useState(() =>
		hasMeasuredViewport
			? getCornerPoint(initialCorner, getDragBounds(geometry, clampWidth(initialWidth, geometry)))
			: {x: 0, y: 0},
	);
	const [isDragging, setIsDragging] = useState(false);
	const [isResizing, setIsResizing] = useState(false);
	const [hasPositioned, setHasPositioned] = useState(hasMeasuredViewport);
	const cornerRef = useRef(corner);
	const widthRef = useRef(width);
	const offsetRef = useRef(offset);
	const dragStateRef = useRef<DragInternalState | null>(null);
	const resizeStateRef = useRef<ResizeInternalState | null>(null);
	const dragListenersRef = useRef<{move: (event: PointerEvent) => void; up: (event: PointerEvent) => void} | null>(
		null,
	);
	const resizeListenersRef = useRef<{move: (event: PointerEvent) => void; up: (event: PointerEvent) => void} | null>(
		null,
	);
	const resizeFrameRef = useRef<number | null>(null);
	const pendingResizeMoveRef = useRef<{pointerId: number; clientX: number; clientY: number} | null>(null);
	const suppressClickRef = useRef(false);
	useEffect(() => {
		cornerRef.current = corner;
	}, [corner]);
	useEffect(() => {
		widthRef.current = width;
	}, [width]);
	useEffect(() => {
		offsetRef.current = offset;
	}, [offset]);
	const commitCorner = useCallback(
		(nextCorner: Corner) => {
			cornerRef.current = nextCorner;
			setCorner((previous) => (previous === nextCorner ? previous : nextCorner));
			onCornerChange?.(nextCorner);
		},
		[onCornerChange],
	);
	const updateWidthSilently = useCallback((nextWidth: number) => {
		widthRef.current = nextWidth;
		setWidth((previous) => (Math.abs(previous - nextWidth) < 0.5 ? previous : nextWidth));
	}, []);
	const commitWidth = useCallback(
		(nextWidth: number) => {
			updateWidthSilently(nextWidth);
			onWidthChange?.(nextWidth);
		},
		[onWidthChange, updateWidthSilently],
	);
	const updateOffsetSilently = useCallback((next: {x: number; y: number}) => {
		offsetRef.current = next;
		setOffset((previous) =>
			Math.abs(previous.x - next.x) < 0.5 && Math.abs(previous.y - next.y) < 0.5 ? previous : next,
		);
	}, []);
	useLayoutEffect(() => {
		if (dragStateRef.current || resizeStateRef.current) return;
		if (geometry.viewport.width <= 0 || geometry.viewport.height <= 0) return;
		const reconciled = reconcileToGeometry({
			corner: cornerRef.current,
			width: widthRef.current,
			geometry,
		});
		if (Math.abs(reconciled.width - widthRef.current) >= 0.5) {
			commitWidth(reconciled.width);
		}
		updateOffsetSilently(reconciled.offset);
		setHasPositioned(true);
	}, [
		commitWidth,
		geometry,
		geometry.aspectRatio,
		geometry.edgePadding,
		geometry.maxWidth,
		geometry.minWidth,
		geometry.topInset,
		geometry.viewport.height,
		geometry.viewport.width,
		updateOffsetSilently,
	]);
	const cancelPendingResizeFrame = useCallback(() => {
		if (resizeFrameRef.current !== null) {
			cancelAnimationFrame(resizeFrameRef.current);
			resizeFrameRef.current = null;
		}
	}, []);
	const flushResizeFrame = useCallback(() => {
		resizeFrameRef.current = null;
		const pendingMove = pendingResizeMoveRef.current;
		pendingResizeMoveRef.current = null;
		const state = resizeStateRef.current;
		if (!state || !pendingMove || state.pointerId !== pendingMove.pointerId) return;
		const pointerX = appZoomLayoutPx(pendingMove.clientX);
		const pointerY = appZoomLayoutPx(pendingMove.clientY);
		const result = computeResize(state, pointerX, pointerY, geometryRef.current);
		updateWidthSilently(result.width);
		updateOffsetSilently(result.offset);
	}, [updateOffsetSilently, updateWidthSilently]);
	const cleanupResizeListeners = useCallback(() => {
		const listeners = resizeListenersRef.current;
		if (!listeners) return;
		window.removeEventListener('pointermove', listeners.move);
		window.removeEventListener('pointerup', listeners.up);
		window.removeEventListener('pointercancel', listeners.up);
		resizeListenersRef.current = null;
		pendingResizeMoveRef.current = null;
		cancelPendingResizeFrame();
	}, [cancelPendingResizeFrame]);
	const handleResizeMove = useCallback(
		(event: PointerEvent) => {
			const state = resizeStateRef.current;
			if (!state || state.pointerId !== event.pointerId) return;
			event.preventDefault();
			pendingResizeMoveRef.current = {pointerId: event.pointerId, clientX: event.clientX, clientY: event.clientY};
			if (resizeFrameRef.current !== null) return;
			resizeFrameRef.current = requestAnimationFrame(flushResizeFrame);
		},
		[flushResizeFrame],
	);
	const handleResizeUp = useCallback(
		(event: PointerEvent) => {
			const state = resizeStateRef.current;
			if (!state || state.pointerId !== event.pointerId) return;
			event.preventDefault();
			if (resizeFrameRef.current !== null) {
				cancelPendingResizeFrame();
				flushResizeFrame();
			}
			cleanupResizeListeners();
			resizeStateRef.current = null;
			setIsResizing(false);
			const reconcileGeometry = geometryRef.current;
			const bounds = getDragBounds(reconcileGeometry, widthRef.current);
			const snappedOffset = clampPoint(offsetRef.current, bounds);
			const nextCorner: Corner = (() => {
				const midX = (bounds.minX + bounds.maxX) / 2;
				const midY = (bounds.minY + bounds.maxY) / 2;
				const isRight = snappedOffset.x >= midX;
				const isBottom = snappedOffset.y >= midY;
				return `${isBottom ? 'bottom' : 'top'}-${isRight ? 'right' : 'left'}` as Corner;
			})();
			commitCorner(nextCorner);
			commitWidth(widthRef.current);
			updateOffsetSilently(getCornerPoint(nextCorner, bounds));
		},
		[
			cancelPendingResizeFrame,
			cleanupResizeListeners,
			commitCorner,
			commitWidth,
			flushResizeFrame,
			updateOffsetSilently,
		],
	);
	const createResizeHandler = useCallback(
		(edge: ResizeEdge) => (event: React.PointerEvent<HTMLElement>) => {
			if (event.button !== 0) return;
			if (requireFocusedWindow && !canUseWindowFocusedHoverControls()) return;
			if (dragStateRef.current) return;
			event.preventDefault();
			event.stopPropagation();
			const startWidth = clampWidth(widthRef.current, geometryRef.current);
			const start: ResizeInternalState = {
				pointerId: event.pointerId,
				edge,
				startWidth,
				pointerStartX: appZoomLayoutPx(event.clientX),
				pointerStartY: appZoomLayoutPx(event.clientY),
				paneStartX: offsetRef.current.x,
				paneStartY: offsetRef.current.y,
			};
			resizeStateRef.current = start;
			const listeners = {move: handleResizeMove, up: handleResizeUp};
			resizeListenersRef.current = listeners;
			setIsResizing(true);
			window.addEventListener('pointermove', listeners.move);
			window.addEventListener('pointerup', listeners.up);
			window.addEventListener('pointercancel', listeners.up);
		},
		[handleResizeMove, handleResizeUp, requireFocusedWindow],
	);
	const cleanupDragListeners = useCallback(() => {
		const listeners = dragListenersRef.current;
		if (!listeners) return;
		window.removeEventListener('pointermove', listeners.move);
		window.removeEventListener('pointerup', listeners.up);
		window.removeEventListener('pointercancel', listeners.up);
		dragListenersRef.current = null;
	}, []);
	const handleDragMove = useCallback(
		(event: PointerEvent) => {
			const state = dragStateRef.current;
			if (!state || state.pointerId !== event.pointerId) return;
			const deltaX = appZoomLayoutPx(event.clientX - state.startClientX);
			const deltaY = appZoomLayoutPx(event.clientY - state.startClientY);
			if (!state.dragActivated) {
				if (deltaX * deltaX + deltaY * deltaY < DRAG_ACTIVATION_DISTANCE_SQ) return;
				state.dragActivated = true;
				suppressClickRef.current = true;
				setIsDragging(true);
			}
			const now = event.timeStamp;
			const dt = now - state.lastTimestamp;
			if (dt > 0) {
				const dx = event.clientX - state.lastClientX;
				const dy = event.clientY - state.lastClientY;
				state.velocityX = (dx / dt) * 1000;
				state.velocityY = (dy / dt) * 1000;
			}
			state.lastClientX = event.clientX;
			state.lastClientY = event.clientY;
			state.lastTimestamp = now;
			const nextOffset = clampPoint(
				{x: state.startOffsetX + deltaX, y: state.startOffsetY + deltaY},
				getDragBounds(geometryRef.current, widthRef.current),
			);
			updateOffsetSilently(nextOffset);
		},
		[updateOffsetSilently],
	);
	const handleDragUp = useCallback(
		(event: PointerEvent) => {
			const state = dragStateRef.current;
			if (!state || state.pointerId !== event.pointerId) return;
			dragStateRef.current = null;
			cleanupDragListeners();
			if (!state.dragActivated) {
				setIsDragging(false);
				return;
			}
			const bounds = getDragBounds(geometryRef.current, widthRef.current);
			const nextCorner = useFlingForCornerPick
				? pickCornerForFling(offsetRef.current, {x: state.velocityX, y: state.velocityY}, bounds)
				: (() => {
						const midX = (bounds.minX + bounds.maxX) / 2;
						const midY = (bounds.minY + bounds.maxY) / 2;
						const isRight = offsetRef.current.x >= midX;
						const isBottom = offsetRef.current.y >= midY;
						return `${isBottom ? 'bottom' : 'top'}-${isRight ? 'right' : 'left'}` as Corner;
					})();
			commitCorner(nextCorner);
			updateOffsetSilently(getCornerPoint(nextCorner, bounds));
			setIsDragging(false);
		},
		[cleanupDragListeners, commitCorner, updateOffsetSilently, useFlingForCornerPick],
	);
	const beginDrag = useCallback(
		(event: React.PointerEvent<HTMLElement>) => {
			if (event.button !== 0) return;
			if (requireFocusedWindow && !canUseWindowFocusedHoverControls()) return;
			if (resizeStateRef.current) return;
			event.preventDefault();
			event.stopPropagation();
			try {
				event.currentTarget.setPointerCapture(event.pointerId);
			} catch {}
			const now = event.timeStamp;
			dragStateRef.current = {
				pointerId: event.pointerId,
				startClientX: event.clientX,
				startClientY: event.clientY,
				startOffsetX: offsetRef.current.x,
				startOffsetY: offsetRef.current.y,
				startTimestamp: now,
				lastClientX: event.clientX,
				lastClientY: event.clientY,
				lastTimestamp: now,
				velocityX: 0,
				velocityY: 0,
				dragActivated: false,
			};
			const listeners = {move: handleDragMove, up: handleDragUp};
			dragListenersRef.current = listeners;
			window.addEventListener('pointermove', listeners.move);
			window.addEventListener('pointerup', listeners.up);
			window.addEventListener('pointercancel', listeners.up);
		},
		[handleDragMove, handleDragUp, requireFocusedWindow],
	);
	useEffect(() => {
		return () => {
			cleanupDragListeners();
			cleanupResizeListeners();
		};
	}, [cleanupDragListeners, cleanupResizeListeners]);
	const consumeClickSuppression = useCallback(() => {
		if (!suppressClickRef.current) return false;
		suppressClickRef.current = false;
		return true;
	}, []);
	const height = useMemo(() => getPaneHeight(width, geometry.aspectRatio), [width, geometry.aspectRatio]);
	const dragHandlers = useMemo<FloatingPanePointerHandlers>(() => ({onPointerDown: beginDrag}), [beginDrag]);
	return {
		width,
		height,
		offset,
		corner,
		isDragging,
		isResizing,
		hasPositioned,
		dragHandlers,
		createResizeHandler,
		suppressNextClick: suppressClickRef.current,
		consumeClickSuppression,
	};
}
