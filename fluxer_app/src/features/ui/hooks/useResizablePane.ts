// SPDX-License-Identifier: AGPL-3.0-or-later

import AppStorage from '@app/features/platform/state/PersistentStorage';
import {Logger} from '@app/features/platform/utils/AppLogger';
import type {ResizeEdge} from '@app/features/ui/floating_pane/FloatingPaneMath';
import {
	type PopoutResizePositionOffset,
	type PopoutResizePositionSession,
	usePopoutResizePositionController,
} from '@app/features/ui/popover/PopoutResizePositionContext';
import {getAppZoomFactor} from '@app/features/ui/utils/AppZoomUtils';
import type React from 'react';
import {useCallback, useEffect, useRef, useState} from 'react';

export interface ResizablePaneSize {
	width: number;
	height: number;
}

export interface UseResizablePaneOptions {
	readonly storageKey: string;
	readonly defaultSize: ResizablePaneSize;
	readonly minSize: ResizablePaneSize;
	readonly viewportPadding: number;
	readonly resizingClassName: string;
	readonly cursorProperty: string;
}

export interface ResizablePaneHandleProps {
	readonly onPointerDown: (event: React.PointerEvent<HTMLButtonElement>) => void;
	readonly onPointerMove: (event: React.PointerEvent<HTMLButtonElement>) => void;
	readonly onPointerUp: (event: React.PointerEvent<HTMLButtonElement>) => void;
	readonly onPointerCancel: (event: React.PointerEvent<HTMLButtonElement>) => void;
	readonly onLostPointerCapture: (event: React.PointerEvent<HTMLButtonElement>) => void;
	readonly onDoubleClick: (event: React.MouseEvent<HTMLButtonElement>) => void;
	readonly onKeyDown: (event: React.KeyboardEvent<HTMLButtonElement>) => void;
}

export interface ResizablePane {
	size: ResizablePaneSize;
	resizeEdge: ResizeEdge | null;
	getHandleProps: (edge: ResizeEdge) => ResizablePaneHandleProps;
	resetSize: () => void;
}

interface ResizeAnchorRect {
	left: number;
	right: number;
	top: number;
	bottom: number;
}

type ResizeViewportBounds = ResizeAnchorRect;

interface ResizeInternalState {
	edge: ResizeEdge;
	handle: HTMLButtonElement;
	ownerDocument: Document;
	ownerWindow: Window;
	pointerId: number;
	positionSession: PopoutResizePositionSession;
	startX: number;
	startY: number;
	startWidth: number;
	startHeight: number;
	startOffset: PopoutResizePositionOffset;
	anchorRect: ResizeAnchorRect;
	zoom: number;
	latest: ResizablePaneSize;
	latestOffset: PopoutResizePositionOffset;
	handleWindowBlur: () => void;
}

export const RESIZABLE_PANE_DEFAULT_VIEWPORT_PADDING = 16;
const RESIZABLE_PANE_KEYBOARD_STEP = 8;
const RESIZABLE_PANE_KEYBOARD_LARGE_STEP = 32;
const NON_NEGATIVE_FINITE_NUMBER_JSON_MAX_LENGTH = JSON.stringify(Number.MAX_VALUE).length;
const RESIZABLE_PANE_SIZE_JSON_MAX_LENGTH =
	'{"width":,"height":}'.length + NON_NEGATIVE_FINITE_NUMBER_JSON_MAX_LENGTH * 2;
const TITLEBAR_SELECTOR = '[data-native-titlebar]';
const logger = new Logger('useResizablePane');

function clampDimension(value: number, configuredMin: number, availableMax: number): number {
	const max = Math.max(1, Math.floor(availableMax));
	const min = Math.min(configuredMin, max);
	if (!Number.isFinite(value)) return min;
	return Math.max(min, Math.min(Math.round(value), max));
}

function getUsableAppZoomFactor(ownerDocument: Document): number {
	const zoom = getAppZoomFactor(ownerDocument);
	if (!Number.isFinite(zoom) || zoom <= 0) return 1;
	return zoom;
}

function getNativeTitlebarBottom(ownerDocument: Document): number {
	const titlebar = ownerDocument.querySelector<HTMLElement>(TITLEBAR_SELECTOR);
	if (titlebar == null) return 0;
	const rect = titlebar.getBoundingClientRect();
	if (rect.height <= 0 || rect.bottom <= 0) return 0;
	return rect.bottom;
}

function getResizeViewportBounds(
	ownerDocument: Document,
	viewportPadding: number,
	zoom: number,
): ResizeViewportBounds | null {
	const ownerWindow = ownerDocument.defaultView;
	if (ownerWindow == null) return null;
	const visualViewport = ownerWindow.visualViewport;
	let viewportLeft = 0;
	let viewportTop = 0;
	let viewportWidth = ownerWindow.innerWidth;
	let viewportHeight = ownerWindow.innerHeight;
	if (visualViewport != null) {
		viewportLeft = visualViewport.offsetLeft;
		viewportTop = visualViewport.offsetTop;
		viewportWidth = visualViewport.width;
		viewportHeight = visualViewport.height;
	}
	const padding = viewportPadding * zoom;
	const left = viewportLeft + padding;
	const top = Math.max(viewportTop, getNativeTitlebarBottom(ownerDocument)) + padding;
	return {
		left,
		right: Math.max(left, viewportLeft + viewportWidth - padding),
		top,
		bottom: Math.max(top, viewportTop + viewportHeight - padding),
	};
}

function getResizeMaximumSize(
	ownerDocument: Document,
	viewportPadding: number,
	zoom: number,
	edge: ResizeEdge | null,
	anchorRect: ResizeAnchorRect | null,
): ResizablePaneSize | null {
	const bounds = getResizeViewportBounds(ownerDocument, viewportPadding, zoom);
	if (bounds == null) return null;
	let availableWidth = bounds.right - bounds.left;
	let availableHeight = bounds.bottom - bounds.top;
	if (edge != null && anchorRect != null) {
		if (edge.includes('left')) {
			availableWidth = anchorRect.right - bounds.left;
		} else if (edge.includes('right')) {
			availableWidth = bounds.right - anchorRect.left;
		}
		if (edge.includes('top')) {
			availableHeight = anchorRect.bottom - bounds.top;
		} else if (edge.includes('bottom')) {
			availableHeight = bounds.bottom - anchorRect.top;
		}
	}
	return {
		width: Math.max(1, availableWidth / zoom),
		height: Math.max(1, availableHeight / zoom),
	};
}

function getResizeCursor(edge: ResizeEdge): string {
	switch (edge) {
		case 'top-left':
		case 'bottom-right':
			return 'nwse-resize';
		case 'top-right':
		case 'bottom-left':
			return 'nesw-resize';
		case 'top':
			return 'n-resize';
		case 'right':
			return 'e-resize';
		case 'bottom':
			return 's-resize';
		case 'left':
			return 'w-resize';
	}
}

function resolveHorizontalResizeDelta(edge: ResizeEdge, deltaX: number): number {
	if (edge.includes('right')) return deltaX;
	if (edge.includes('left')) return -deltaX;
	return 0;
}

function resolveVerticalResizeDelta(edge: ResizeEdge, deltaY: number): number {
	if (edge.includes('bottom')) return deltaY;
	if (edge.includes('top')) return -deltaY;
	return 0;
}

function getPositionOffset(
	edge: ResizeEdge,
	startSize: ResizablePaneSize,
	nextSize: ResizablePaneSize,
	zoom: number,
): PopoutResizePositionOffset {
	return {
		x: edge.includes('left') ? (startSize.width - nextSize.width) * zoom : 0,
		y: edge.includes('top') ? (startSize.height - nextSize.height) * zoom : 0,
	};
}

function parseStoredResizablePaneSize(raw: string): ResizablePaneSize | null {
	if (raw.length > RESIZABLE_PANE_SIZE_JSON_MAX_LENGTH) return null;
	let stored: unknown;
	try {
		stored = JSON.parse(raw);
	} catch {
		return null;
	}
	if (stored == null || typeof stored !== 'object' || Array.isArray(stored)) return null;
	const candidate = stored as Record<string, unknown>;
	const keys = Object.keys(candidate);
	if (keys.length !== 2 || !keys.includes('width') || !keys.includes('height')) return null;
	if (typeof candidate.width !== 'number' || !Number.isFinite(candidate.width) || candidate.width < 0) return null;
	if (typeof candidate.height !== 'number' || !Number.isFinite(candidate.height) || candidate.height < 0) return null;
	const size = {width: candidate.width, height: candidate.height};
	if (JSON.stringify(size) !== raw) return null;
	return size;
}

function readStoredResizablePaneSize(storageKey: string): ResizablePaneSize | null {
	const raw = AppStorage.getItem(storageKey);
	if (raw == null) return null;
	const stored = parseStoredResizablePaneSize(raw);
	if (stored != null) return stored;
	AppStorage.removeItem(storageKey);
	logger.warn({storageKey}, 'ui.resizable_pane.invalid_persistence.removed');
	return null;
}

function serializeResizablePaneSize(size: ResizablePaneSize): string {
	const serialized = JSON.stringify(size);
	if (serialized.length > RESIZABLE_PANE_SIZE_JSON_MAX_LENGTH) {
		throw new Error('Resizable pane size exceeds its storage contract');
	}
	return serialized;
}

function areSizesEqual(left: ResizablePaneSize, right: ResizablePaneSize): boolean {
	return left.width === right.width && left.height === right.height;
}

function getKeyboardResizeDelta(edge: ResizeEdge, key: string, step: number): ResizablePaneSize | null {
	if (key === 'ArrowLeft' && (edge.includes('left') || edge.includes('right'))) {
		return {width: edge.includes('left') ? step : -step, height: 0};
	}
	if (key === 'ArrowRight' && (edge.includes('left') || edge.includes('right'))) {
		return {width: edge.includes('right') ? step : -step, height: 0};
	}
	if (key === 'ArrowUp' && (edge.includes('top') || edge.includes('bottom'))) {
		return {width: 0, height: edge.includes('top') ? step : -step};
	}
	if (key === 'ArrowDown' && (edge.includes('top') || edge.includes('bottom'))) {
		return {width: 0, height: edge.includes('bottom') ? step : -step};
	}
	return null;
}

export function useResizablePane(
	containerRef: React.RefObject<HTMLElement | null>,
	options: UseResizablePaneOptions,
): ResizablePane {
	const positionController = usePopoutResizePositionController();
	const optionsRef = useRef(options);
	optionsRef.current = options;

	const resolveOwnerDocument = useCallback((): Document | null => {
		const container = containerRef.current;
		if (container != null) return container.ownerDocument;
		if (typeof document === 'undefined') return null;
		return document;
	}, [containerRef]);

	const clampSize = useCallback(
		(
			size: ResizablePaneSize,
			ownerDocument: Document | null,
			edge: ResizeEdge | null,
			anchorRect: ResizeAnchorRect | null,
		): ResizablePaneSize => {
			const {minSize, viewportPadding} = optionsRef.current;
			if (ownerDocument == null) {
				return {
					width: Math.max(minSize.width, Math.round(size.width)),
					height: Math.max(minSize.height, Math.round(size.height)),
				};
			}
			const zoom = getUsableAppZoomFactor(ownerDocument);
			const maximumSize = getResizeMaximumSize(ownerDocument, viewportPadding, zoom, edge, anchorRect);
			if (maximumSize == null) {
				return {
					width: Math.max(minSize.width, Math.round(size.width)),
					height: Math.max(minSize.height, Math.round(size.height)),
				};
			}
			return {
				width: clampDimension(size.width, minSize.width, maximumSize.width),
				height: clampDimension(size.height, minSize.height, maximumSize.height),
			};
		},
		[],
	);

	const loadSize = useCallback((): ResizablePaneSize => {
		const {storageKey, defaultSize} = optionsRef.current;
		const storedSize = readStoredResizablePaneSize(storageKey);
		const requestedSize = storedSize == null ? defaultSize : storedSize;
		return clampSize(requestedSize, resolveOwnerDocument(), null, null);
	}, [clampSize, resolveOwnerDocument]);

	const [size, setSize] = useState<ResizablePaneSize>(() => loadSize());
	const [resizeEdge, setResizeEdge] = useState<ResizeEdge | null>(null);
	const resizeStateRef = useRef<ResizeInternalState | null>(null);
	const sizeRef = useRef(size);
	const mountedRef = useRef(true);

	const publishSize = useCallback((nextSize: ResizablePaneSize) => {
		sizeRef.current = nextSize;
		if (mountedRef.current) setSize(nextSize);
	}, []);

	const finalizeResize = useCallback((publishInactiveState: boolean, persistSize: boolean) => {
		const state = resizeStateRef.current;
		if (state == null) {
			if (publishInactiveState && mountedRef.current) setResizeEdge(null);
			return;
		}
		resizeStateRef.current = null;
		state.ownerWindow.removeEventListener('blur', state.handleWindowBlur);
		const {resizingClassName, cursorProperty, storageKey} = optionsRef.current;
		state.ownerDocument.documentElement.classList.remove(resizingClassName);
		state.ownerDocument.documentElement.style.removeProperty(cursorProperty);
		state.positionSession.finish(state.latestOffset);
		if (state.handle.hasPointerCapture(state.pointerId)) {
			state.handle.releasePointerCapture(state.pointerId);
		}
		if (publishInactiveState && mountedRef.current) setResizeEdge(null);
		if (persistSize) AppStorage.setItem(storageKey, serializeResizablePaneSize(state.latest));
	}, []);

	useEffect(() => {
		mountedRef.current = true;
		return () => {
			mountedRef.current = false;
			finalizeResize(false, false);
		};
	}, [finalizeResize]);

	useEffect(() => {
		const ownerDocument = resolveOwnerDocument();
		if (ownerDocument == null) return;
		const ownerWindow = ownerDocument.defaultView;
		if (ownerWindow == null) return;
		const clampCurrentSize = () => {
			if (resizeStateRef.current != null) return;
			const currentSize = sizeRef.current;
			const nextSize = clampSize(currentSize, ownerDocument, null, null);
			if (!areSizesEqual(currentSize, nextSize)) publishSize(nextSize);
		};
		clampCurrentSize();
		ownerWindow.addEventListener('resize', clampCurrentSize);
		const visualViewport = ownerWindow.visualViewport;
		if (visualViewport != null) {
			visualViewport.addEventListener('resize', clampCurrentSize);
			visualViewport.addEventListener('scroll', clampCurrentSize);
		}
		return () => {
			ownerWindow.removeEventListener('resize', clampCurrentSize);
			if (visualViewport != null) {
				visualViewport.removeEventListener('resize', clampCurrentSize);
				visualViewport.removeEventListener('scroll', clampCurrentSize);
			}
		};
	}, [clampSize, publishSize, resolveOwnerDocument]);

	const handlePointerDown = useCallback(
		(edge: ResizeEdge, event: React.PointerEvent<HTMLButtonElement>) => {
			if (resizeStateRef.current != null) {
				event.preventDefault();
				event.stopPropagation();
				return;
			}
			if (!event.isPrimary || event.button !== 0) return;
			const handle = event.currentTarget;
			const ownerDocument = handle.ownerDocument;
			const ownerWindow = ownerDocument.defaultView;
			const container = containerRef.current;
			if (ownerWindow == null || container == null || container.ownerDocument !== ownerDocument) return;
			const rect = container.getBoundingClientRect();
			const zoom = getUsableAppZoomFactor(ownerDocument);
			const measuredSize = {
				width: rect.width > 0 ? rect.width / zoom : sizeRef.current.width,
				height: rect.height > 0 ? rect.height / zoom : sizeRef.current.height,
			};
			const anchorRect = {left: rect.left, right: rect.right, top: rect.top, bottom: rect.bottom};
			const startSize = clampSize(measuredSize, ownerDocument, edge, anchorRect);
			event.preventDefault();
			event.stopPropagation();
			const positionSession = positionController.begin();
			const initialOffset = getPositionOffset(edge, measuredSize, startSize, zoom);
			const handleWindowBlur = () => finalizeResize(true, true);
			const state: ResizeInternalState = {
				edge,
				handle,
				ownerDocument,
				ownerWindow,
				pointerId: event.pointerId,
				positionSession,
				startX: event.clientX,
				startY: event.clientY,
				startWidth: startSize.width,
				startHeight: startSize.height,
				startOffset: initialOffset,
				anchorRect,
				zoom,
				latest: startSize,
				latestOffset: initialOffset,
				handleWindowBlur,
			};
			resizeStateRef.current = state;
			ownerWindow.addEventListener('blur', handleWindowBlur);
			const {resizingClassName, cursorProperty} = optionsRef.current;
			ownerDocument.documentElement.classList.add(resizingClassName);
			ownerDocument.documentElement.style.setProperty(cursorProperty, getResizeCursor(edge));
			try {
				handle.setPointerCapture(event.pointerId);
			} catch {
				finalizeResize(true, false);
				return;
			}
			positionSession.updateOffset(initialOffset);
			publishSize(startSize);
			setResizeEdge(edge);
		},
		[clampSize, containerRef, finalizeResize, positionController, publishSize],
	);

	const handlePointerMove = useCallback(
		(event: React.PointerEvent<HTMLButtonElement>) => {
			const state = resizeStateRef.current;
			if (state == null || state.pointerId !== event.pointerId || !event.isPrimary) return;
			event.preventDefault();
			event.stopPropagation();
			const deltaX = (event.clientX - state.startX) / state.zoom;
			const deltaY = (event.clientY - state.startY) / state.zoom;
			const nextSize = clampSize(
				{
					width: state.startWidth + resolveHorizontalResizeDelta(state.edge, deltaX),
					height: state.startHeight + resolveVerticalResizeDelta(state.edge, deltaY),
				},
				state.ownerDocument,
				state.edge,
				state.anchorRect,
			);
			const resizeOffset = getPositionOffset(
				state.edge,
				{width: state.startWidth, height: state.startHeight},
				nextSize,
				state.zoom,
			);
			const offset = {
				x: state.startOffset.x + resizeOffset.x,
				y: state.startOffset.y + resizeOffset.y,
			};
			state.latest = nextSize;
			state.latestOffset = offset;
			state.positionSession.updateOffset(offset);
			publishSize(nextSize);
		},
		[clampSize, publishSize],
	);

	const handlePointerEnd = useCallback(
		(event: React.PointerEvent<HTMLButtonElement>) => {
			const state = resizeStateRef.current;
			if (state == null || state.pointerId !== event.pointerId) return;
			event.preventDefault();
			event.stopPropagation();
			finalizeResize(true, true);
		},
		[finalizeResize],
	);

	const resetSizeFromEdge = useCallback(
		(edge: ResizeEdge | null) => {
			finalizeResize(true, false);
			const ownerDocument = resolveOwnerDocument();
			const container = containerRef.current;
			let anchorRect: ResizeAnchorRect | null = null;
			let measuredSize = sizeRef.current;
			let zoom = 1;
			if (ownerDocument != null) zoom = getUsableAppZoomFactor(ownerDocument);
			if (container != null && (ownerDocument == null || container.ownerDocument === ownerDocument)) {
				const rect = container.getBoundingClientRect();
				anchorRect = {left: rect.left, right: rect.right, top: rect.top, bottom: rect.bottom};
				if (rect.width > 0 && rect.height > 0) {
					measuredSize = {width: rect.width / zoom, height: rect.height / zoom};
				}
			}
			const nextSize = clampSize(optionsRef.current.defaultSize, ownerDocument, edge, anchorRect);
			if (edge != null) {
				const positionSession = positionController.begin();
				const offset = getPositionOffset(edge, measuredSize, nextSize, zoom);
				positionSession.updateOffset(offset);
				positionSession.finish(offset);
			}
			publishSize(nextSize);
			AppStorage.removeItem(optionsRef.current.storageKey);
		},
		[clampSize, containerRef, finalizeResize, positionController, publishSize, resolveOwnerDocument],
	);

	const resetSize = useCallback(() => resetSizeFromEdge(null), [resetSizeFromEdge]);

	const handleDoubleClick = useCallback(
		(edge: ResizeEdge, event: React.MouseEvent<HTMLButtonElement>) => {
			event.preventDefault();
			event.stopPropagation();
			resetSizeFromEdge(edge);
		},
		[resetSizeFromEdge],
	);

	const handleKeyDown = useCallback(
		(edge: ResizeEdge, event: React.KeyboardEvent<HTMLButtonElement>) => {
			if (event.key === 'Enter' || event.key === 'Backspace' || event.key === 'Delete') {
				event.preventDefault();
				event.stopPropagation();
				resetSizeFromEdge(edge);
				return;
			}
			if (resizeStateRef.current != null) return;
			const ownerDocument = resolveOwnerDocument();
			const container = containerRef.current;
			if (ownerDocument == null || container == null || container.ownerDocument !== ownerDocument) return;
			const zoom = getUsableAppZoomFactor(ownerDocument);
			const rect = container.getBoundingClientRect();
			const measuredSize = {
				width: rect.width > 0 ? rect.width / zoom : sizeRef.current.width,
				height: rect.height > 0 ? rect.height / zoom : sizeRef.current.height,
			};
			const anchorRect = {left: rect.left, right: rect.right, top: rect.top, bottom: rect.bottom};
			let requestedSize = measuredSize;
			if (event.key === 'Home' || event.key === 'End') {
				const setMaximum = event.key === 'End';
				requestedSize = {
					width:
						edge.includes('left') || edge.includes('right')
							? setMaximum
								? Number.MAX_SAFE_INTEGER
								: optionsRef.current.minSize.width
							: measuredSize.width,
					height:
						edge.includes('top') || edge.includes('bottom')
							? setMaximum
								? Number.MAX_SAFE_INTEGER
								: optionsRef.current.minSize.height
							: measuredSize.height,
				};
			} else {
				const step = event.shiftKey ? RESIZABLE_PANE_KEYBOARD_LARGE_STEP : RESIZABLE_PANE_KEYBOARD_STEP;
				const delta = getKeyboardResizeDelta(edge, event.key, step);
				if (delta == null) return;
				requestedSize = {width: measuredSize.width + delta.width, height: measuredSize.height + delta.height};
			}
			const nextSize = clampSize(requestedSize, ownerDocument, edge, anchorRect);
			if (areSizesEqual(measuredSize, nextSize)) return;
			event.preventDefault();
			event.stopPropagation();
			const positionSession = positionController.begin();
			const offset = getPositionOffset(edge, measuredSize, nextSize, zoom);
			positionSession.updateOffset(offset);
			publishSize(nextSize);
			AppStorage.setItem(optionsRef.current.storageKey, serializeResizablePaneSize(nextSize));
			positionSession.finish(offset);
		},
		[clampSize, containerRef, positionController, publishSize, resetSizeFromEdge, resolveOwnerDocument],
	);

	const getHandleProps = useCallback(
		(edge: ResizeEdge): ResizablePaneHandleProps => ({
			onPointerDown: (event) => handlePointerDown(edge, event),
			onPointerMove: handlePointerMove,
			onPointerUp: handlePointerEnd,
			onPointerCancel: handlePointerEnd,
			onLostPointerCapture: handlePointerEnd,
			onDoubleClick: (event) => handleDoubleClick(edge, event),
			onKeyDown: (event) => handleKeyDown(edge, event),
		}),
		[handleDoubleClick, handleKeyDown, handlePointerDown, handlePointerEnd, handlePointerMove],
	);

	return {size, resizeEdge, getHandleProps, resetSize};
}
