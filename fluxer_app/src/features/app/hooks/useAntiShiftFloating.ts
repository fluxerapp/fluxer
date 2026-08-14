// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import type {PopoutResizePositionSession} from '@app/features/ui/popover/PopoutResizePositionContext';
import {appZoomCssPx, appZoomLayoutPx} from '@app/features/ui/utils/AppZoomUtils';
import {getAdaptivePadding} from '@app/features/ui/utils/Positioning';
import {
	autoUpdate,
	computePosition,
	flip,
	type Middleware,
	offset,
	type Padding,
	type Placement,
	type ReferenceElement,
	shift,
	size,
	type VirtualElement,
} from '@floating-ui/react';
import {useCallback, useLayoutEffect, useRef, useState} from 'react';

const logger = new Logger('useAntiShiftFloating');
const DEFAULT_MIDDLEWARE: Array<Middleware> = [];
const TITLEBAR_SELECTOR = '[data-native-titlebar]';
const ANTI_SHIFT_AUTO_UPDATE_OPTIONS = {
	ancestorScroll: false,
	ancestorResize: false,
	elementResize: true,
	layoutShift: true,
} as const;

type FloatingUpdateCallback = () => void | Promise<void>;

interface FloatingUpdateRegistry {
	ownerDocument: Document;
	ownerWindow: Window;
	scrollListenersAttached: boolean;
	viewportListenersAttached: boolean;
	updateRaf: number | null;
	scrollCallbacks: Set<FloatingUpdateCallback>;
	viewportCallbacks: Set<FloatingUpdateCallback>;
	scheduledCallbacks: Set<FloatingUpdateCallback>;
	handleScroll: () => void;
	handleViewport: () => void;
}

const floatingUpdateRegistries = new WeakMap<Document, FloatingUpdateRegistry>();

interface FloatingState {
	x: number;
	y: number;
	isReady: boolean;
	offsetX: number;
	offsetY: number;
}

interface UseAntiShiftFloatingOptions {
	placement: Placement;
	offsetMainAxis?: number;
	offsetCrossAxis?: number;
	middleware?: Array<Middleware>;
	shouldAutoUpdate?: boolean;
	shouldObserveFloatingResize?: boolean;
	enableSmartBoundary?: boolean;
	constrainHeight?: boolean;
}

function clamp(value: number, min: number, max: number): number {
	return Math.min(Math.max(value, min), max);
}

function normalizePadding(padding: Padding): {top: number; right: number; bottom: number; left: number} {
	if (typeof padding === 'number') {
		return {top: padding, right: padding, bottom: padding, left: padding};
	}
	return {
		top: padding.top == null ? 0 : padding.top,
		right: padding.right == null ? 0 : padding.right,
		bottom: padding.bottom == null ? 0 : padding.bottom,
		left: padding.left == null ? 0 : padding.left,
	};
}

function clampToViewportBoundary(
	x: number,
	y: number,
	floating: HTMLElement,
	padding: Padding,
): {x: number; y: number} {
	const normalizedPadding = normalizePadding(padding);
	const rect = floating.getBoundingClientRect();
	const ownerWindow = floating.ownerDocument.defaultView;
	if (ownerWindow == null) return {x, y};
	const visualViewport = ownerWindow.visualViewport;
	const viewportLeft = visualViewport == null ? 0 : visualViewport.offsetLeft;
	const viewportTop = visualViewport == null ? 0 : visualViewport.offsetTop;
	const viewportWidth = visualViewport == null ? ownerWindow.innerWidth : visualViewport.width;
	const viewportHeight = visualViewport == null ? ownerWindow.innerHeight : visualViewport.height;
	const minX = viewportLeft + normalizedPadding.left;
	const minY = viewportTop + normalizedPadding.top;
	const maxX = Math.max(minX, viewportLeft + viewportWidth - normalizedPadding.right - rect.width);
	const maxY = Math.max(minY, viewportTop + viewportHeight - normalizedPadding.bottom - rect.height);
	return {
		x: clamp(x, minX, maxX),
		y: clamp(y, minY, maxY),
	};
}

function scheduleFloatingUpdates(registry: FloatingUpdateRegistry, callbacks: Iterable<FloatingUpdateCallback>): void {
	for (const callback of callbacks) {
		registry.scheduledCallbacks.add(callback);
	}
	if (registry.scheduledCallbacks.size === 0 || registry.updateRaf != null) return;
	registry.updateRaf = registry.ownerWindow.requestAnimationFrame(() => {
		registry.updateRaf = null;
		const callbacksToRun = Array.from(registry.scheduledCallbacks);
		registry.scheduledCallbacks.clear();
		for (const callback of callbacksToRun) {
			void callback();
		}
	});
}

function getFloatingUpdateRegistry(ownerDocument: Document): FloatingUpdateRegistry | null {
	const current = floatingUpdateRegistries.get(ownerDocument);
	if (current != null) return current;
	const ownerWindow = ownerDocument.defaultView;
	if (ownerWindow == null) return null;
	let registry: FloatingUpdateRegistry;
	registry = {
		ownerDocument,
		ownerWindow,
		scrollListenersAttached: false,
		viewportListenersAttached: false,
		updateRaf: null,
		scrollCallbacks: new Set(),
		viewportCallbacks: new Set(),
		scheduledCallbacks: new Set(),
		handleScroll: () => scheduleFloatingUpdates(registry, registry.scrollCallbacks),
		handleViewport: () => {
			scheduleFloatingUpdates(registry, registry.scrollCallbacks);
			scheduleFloatingUpdates(registry, registry.viewportCallbacks);
		},
	};
	floatingUpdateRegistries.set(ownerDocument, registry);
	return registry;
}

function ensureSharedScrollListeners(registry: FloatingUpdateRegistry): void {
	if (registry.scrollListenersAttached) return;
	registry.ownerDocument.addEventListener('scroll', registry.handleScroll, {capture: true, passive: true});
	registry.scrollListenersAttached = true;
}

function ensureSharedViewportListeners(registry: FloatingUpdateRegistry): void {
	if (registry.viewportListenersAttached) return;
	registry.ownerWindow.addEventListener('resize', registry.handleViewport);
	const visualViewport = registry.ownerWindow.visualViewport;
	if (visualViewport != null) {
		visualViewport.addEventListener('resize', registry.handleViewport);
		visualViewport.addEventListener('scroll', registry.handleViewport);
	}
	registry.viewportListenersAttached = true;
}

function releaseSharedScrollListeners(registry: FloatingUpdateRegistry): void {
	if (!registry.scrollListenersAttached || registry.scrollCallbacks.size > 0) return;
	registry.ownerDocument.removeEventListener('scroll', registry.handleScroll, true);
	registry.scrollListenersAttached = false;
}

function releaseSharedViewportListeners(registry: FloatingUpdateRegistry): void {
	if (!registry.viewportListenersAttached || registry.scrollCallbacks.size > 0 || registry.viewportCallbacks.size > 0) {
		return;
	}
	registry.ownerWindow.removeEventListener('resize', registry.handleViewport);
	const visualViewport = registry.ownerWindow.visualViewport;
	if (visualViewport != null) {
		visualViewport.removeEventListener('resize', registry.handleViewport);
		visualViewport.removeEventListener('scroll', registry.handleViewport);
	}
	registry.viewportListenersAttached = false;
	if (registry.updateRaf != null && registry.scheduledCallbacks.size === 0) {
		registry.ownerWindow.cancelAnimationFrame(registry.updateRaf);
		registry.updateRaf = null;
	}
	if (!registry.scrollListenersAttached) floatingUpdateRegistries.delete(registry.ownerDocument);
}

function removeScheduledFloatingUpdate(registry: FloatingUpdateRegistry, callback: FloatingUpdateCallback): void {
	registry.scheduledCallbacks.delete(callback);
	if (registry.updateRaf != null && registry.scheduledCallbacks.size === 0) {
		registry.ownerWindow.cancelAnimationFrame(registry.updateRaf);
		registry.updateRaf = null;
	}
}

function subscribeSharedScrollUpdate(ownerDocument: Document, callback: FloatingUpdateCallback): () => void {
	const registry = getFloatingUpdateRegistry(ownerDocument);
	if (registry == null) return () => {};
	registry.scrollCallbacks.add(callback);
	ensureSharedScrollListeners(registry);
	ensureSharedViewportListeners(registry);
	return () => {
		registry.scrollCallbacks.delete(callback);
		removeScheduledFloatingUpdate(registry, callback);
		releaseSharedScrollListeners(registry);
		releaseSharedViewportListeners(registry);
	};
}

function subscribeSharedViewportUpdate(ownerDocument: Document, callback: FloatingUpdateCallback): () => void {
	const registry = getFloatingUpdateRegistry(ownerDocument);
	if (registry == null) return () => {};
	registry.viewportCallbacks.add(callback);
	ensureSharedViewportListeners(registry);
	return () => {
		registry.viewportCallbacks.delete(callback);
		removeScheduledFloatingUpdate(registry, callback);
		releaseSharedViewportListeners(registry);
	};
}

function observeFloatingResize(floating: HTMLElement, updatePosition: () => void): () => void {
	const cleanupCallbacks: Array<() => void> = [];
	const ownerWindow = floating.ownerDocument.defaultView;
	const ResizeObserverConstructor = ownerWindow == null ? null : ownerWindow.ResizeObserver;
	if (ResizeObserverConstructor != null) {
		const resizeObserver = new ResizeObserverConstructor(updatePosition);
		resizeObserver.observe(floating);
		cleanupCallbacks.push(() => resizeObserver.disconnect());
	}
	cleanupCallbacks.push(subscribeSharedViewportUpdate(floating.ownerDocument, updatePosition));
	return () => {
		for (const cleanup of cleanupCallbacks) {
			cleanup();
		}
	};
}

function getNativeTitlebarInset(ownerDocument: Document): number {
	const titlebar = ownerDocument.querySelector<HTMLElement>(TITLEBAR_SELECTOR);
	if (titlebar == null) return 0;
	const rect = titlebar.getBoundingClientRect();
	if (rect.height <= 0 || rect.bottom <= 0) return 0;
	const ownerWindow = ownerDocument.defaultView;
	const visualViewport = ownerWindow == null ? null : ownerWindow.visualViewport;
	const viewportTop = visualViewport == null ? 0 : visualViewport.offsetTop;
	return Math.max(0, rect.bottom - viewportTop);
}

function getBoundaryPadding(ownerDocument: Document, basePadding: number): Padding {
	const titlebarInset = getNativeTitlebarInset(ownerDocument);
	if (titlebarInset <= 0) {
		return basePadding;
	}
	return {
		top: titlebarInset + basePadding,
		right: basePadding,
		bottom: basePadding,
		left: basePadding,
	};
}

export function useAntiShiftFloating(
	target: ReferenceElement | null,
	enabled: boolean,
	options: UseAntiShiftFloatingOptions,
) {
	const {
		placement,
		offsetMainAxis = 8,
		offsetCrossAxis = 0,
		middleware: extraMiddleware = DEFAULT_MIDDLEWARE,
		shouldAutoUpdate = true,
		shouldObserveFloatingResize = true,
		enableSmartBoundary = false,
		constrainHeight = false,
	} = options;
	const floatingRef = useRef<HTMLElement>(null);
	const [state, setState] = useState<FloatingState>(() => {
		const {x, y} = target ? getInitialGuess(target, placement, offsetMainAxis, offsetCrossAxis) : {x: -9999, y: -9999};
		return {x, y, isReady: false, offsetX: 0, offsetY: 0};
	});
	const cleanupRef = useRef<(() => void) | null>(null);
	const isCalculatingRef = useRef(false);
	const rafIdRef = useRef<number | null>(null);
	const rafOwnerWindowRef = useRef<Window | null>(null);
	const pendingUpdateRef = useRef(false);
	const manualSessionIdRef = useRef<number | null>(null);
	const positionRevisionRef = useRef(0);
	const basePositionRef = useRef({x: state.x, y: state.y});
	const manualOffsetRef = useRef({x: 0, y: 0});
	const manualAbsolutePositionRef = useRef<{x: number; y: number} | null>(null);
	const requestPositionUpdateRef = useRef<() => void>(() => {});
	const updatePositionNow = useCallback(async () => {
		if (!enabled || target == null || floatingRef.current == null) return;
		if (manualSessionIdRef.current != null || isCalculatingRef.current) {
			pendingUpdateRef.current = true;
			return;
		}
		pendingUpdateRef.current = false;
		isCalculatingRef.current = true;
		const calculationRevision = positionRevisionRef.current;
		try {
			const floating = floatingRef.current;
			if (floating == null) return;
			const ownerDocument = floating.ownerDocument;
			const ownerWindow = ownerDocument.defaultView;
			if (ownerWindow == null) return;
			const adaptivePadding = enableSmartBoundary ? getAdaptivePadding(ownerWindow) : 8;
			const shiftPadding = Math.max(6, adaptivePadding);
			const boundaryPadding = getBoundaryPadding(ownerDocument, shiftPadding);
			const middleware: Array<Middleware> = [
				offset({mainAxis: offsetMainAxis, crossAxis: offsetCrossAxis}),
				flip({padding: boundaryPadding}),
				shift({padding: boundaryPadding, crossAxis: true}),
				...extraMiddleware,
			];
			if (constrainHeight) {
				middleware.push(
					size({
						apply({
							availableWidth,
							availableHeight,
							elements,
						}: {
							availableWidth: number;
							availableHeight: number;
							elements: {
								floating: HTMLElement;
							};
						}) {
							const maxWidth = Math.max(0, availableWidth);
							const maxHeight = Math.max(0, availableHeight);
							Object.assign(elements.floating.style, {
								maxWidth: appZoomCssPx(maxWidth),
								maxHeight: appZoomCssPx(maxHeight),
								overflowX: 'hidden',
								overflowY: 'auto',
								overscrollBehavior: 'contain',
							});
						},
						padding: boundaryPadding,
					}),
				);
			}
			const {x, y} = await computePosition(target, floating, {
				placement,
				middleware,
			});
			if (
				calculationRevision !== positionRevisionRef.current ||
				manualSessionIdRef.current != null ||
				floatingRef.current !== floating
			) {
				pendingUpdateRef.current = true;
				return;
			}
			const safeBasePosition = clampToViewportBoundary(x, y, floating, boundaryPadding);
			const manualAbsolutePosition = manualAbsolutePositionRef.current;
			let desiredPosition: {x: number; y: number};
			if (manualAbsolutePosition == null) {
				desiredPosition = {
					x: safeBasePosition.x + manualOffsetRef.current.x,
					y: safeBasePosition.y + manualOffsetRef.current.y,
				};
			} else {
				desiredPosition = manualAbsolutePosition;
			}
			const safePosition = clampToViewportBoundary(desiredPosition.x, desiredPosition.y, floating, boundaryPadding);
			const nextOffset = {
				x: safePosition.x - safeBasePosition.x,
				y: safePosition.y - safeBasePosition.y,
			};
			basePositionRef.current = safeBasePosition;
			manualOffsetRef.current = nextOffset;
			manualAbsolutePositionRef.current = null;
			Object.assign(floating.style, {
				left: appZoomCssPx(safePosition.x),
				top: appZoomCssPx(safePosition.y),
				visibility: 'visible',
			});
			setState((prev) => {
				if (
					prev.x === safeBasePosition.x &&
					prev.y === safeBasePosition.y &&
					prev.isReady &&
					prev.offsetX === nextOffset.x &&
					prev.offsetY === nextOffset.y
				) {
					return prev;
				}
				return {...safeBasePosition, isReady: true, offsetX: nextOffset.x, offsetY: nextOffset.y};
			});
		} catch (error) {
			logger.error('Error positioning floating element', error);
			if (floatingRef.current != null) {
				floatingRef.current.style.visibility = 'visible';
			}
		} finally {
			isCalculatingRef.current = false;
			if (pendingUpdateRef.current && manualSessionIdRef.current == null) {
				requestPositionUpdateRef.current();
			}
		}
	}, [
		enabled,
		target,
		placement,
		offsetMainAxis,
		offsetCrossAxis,
		extraMiddleware,
		enableSmartBoundary,
		constrainHeight,
	]);
	const updatePosition = useCallback(() => {
		pendingUpdateRef.current = true;
		if (manualSessionIdRef.current != null || rafIdRef.current != null) return;
		const floating = floatingRef.current;
		if (floating == null) return;
		const ownerWindow = floating.ownerDocument.defaultView;
		if (ownerWindow == null) return;
		rafOwnerWindowRef.current = ownerWindow;
		rafIdRef.current = ownerWindow.requestAnimationFrame(() => {
			rafIdRef.current = null;
			rafOwnerWindowRef.current = null;
			pendingUpdateRef.current = false;
			void updatePositionNow();
		});
	}, [updatePositionNow]);
	requestPositionUpdateRef.current = updatePosition;
	const beginManualPositioning = useCallback((): PopoutResizePositionSession => {
		positionRevisionRef.current += 1;
		const sessionId = positionRevisionRef.current;
		manualSessionIdRef.current = sessionId;
		pendingUpdateRef.current = false;
		if (rafIdRef.current != null && rafOwnerWindowRef.current != null) {
			rafOwnerWindowRef.current.cancelAnimationFrame(rafIdRef.current);
			rafIdRef.current = null;
			rafOwnerWindowRef.current = null;
		}
		const baseOffset = manualOffsetRef.current;
		const sessionBasePosition = basePositionRef.current;
		let finished = false;
		const publishOffset = (offset: {x: number; y: number}) => {
			if (finished || manualSessionIdRef.current !== sessionId) return;
			const nextOffset = {x: baseOffset.x + offset.x, y: baseOffset.y + offset.y};
			manualOffsetRef.current = nextOffset;
			manualAbsolutePositionRef.current = {
				x: sessionBasePosition.x + nextOffset.x,
				y: sessionBasePosition.y + nextOffset.y,
			};
			setState((prev) => {
				if (prev.offsetX === nextOffset.x && prev.offsetY === nextOffset.y) return prev;
				return {...prev, offsetX: nextOffset.x, offsetY: nextOffset.y};
			});
		};
		return {
			updateOffset: publishOffset,
			finish: (offset) => {
				if (finished || manualSessionIdRef.current !== sessionId) return;
				publishOffset(offset);
				finished = true;
				manualSessionIdRef.current = null;
				pendingUpdateRef.current = true;
				requestPositionUpdateRef.current();
			},
		};
	}, []);
	useLayoutEffect(() => {
		if (!enabled || target == null || floatingRef.current == null) {
			setState((prev) => ({...prev, isReady: false, offsetX: 0, offsetY: 0}));
			return;
		}
		if (!isReferenceConnected(target)) {
			setState((prev) => ({...prev, isReady: false, offsetX: 0, offsetY: 0}));
			return;
		}
		const floating = floatingRef.current;
		updatePosition();
		if (shouldAutoUpdate) {
			const cleanupCallbacks = [
				autoUpdate(target, floating, updatePosition, ANTI_SHIFT_AUTO_UPDATE_OPTIONS),
				subscribeSharedScrollUpdate(floating.ownerDocument, updatePositionNow),
			];
			cleanupRef.current = () => {
				for (const cleanup of cleanupCallbacks) {
					cleanup();
				}
			};
		} else if (shouldObserveFloatingResize) {
			cleanupRef.current = observeFloatingResize(floatingRef.current, updatePosition);
		}
		return () => {
			positionRevisionRef.current += 1;
			manualSessionIdRef.current = null;
			manualOffsetRef.current = {x: 0, y: 0};
			manualAbsolutePositionRef.current = null;
			pendingUpdateRef.current = false;
			if (cleanupRef.current != null) cleanupRef.current();
			cleanupRef.current = null;
			if (rafIdRef.current != null && rafOwnerWindowRef.current != null) {
				rafOwnerWindowRef.current.cancelAnimationFrame(rafIdRef.current);
				rafIdRef.current = null;
				rafOwnerWindowRef.current = null;
			}
			setState((prev) => ({...prev, isReady: false, offsetX: 0, offsetY: 0}));
		};
	}, [enabled, target, shouldAutoUpdate, shouldObserveFloatingResize, updatePosition, updatePositionNow]);
	return {
		ref: floatingRef,
		state,
		style: {
			position: 'fixed' as const,
			left: appZoomLayoutPx(state.x + state.offsetX),
			top: appZoomLayoutPx(state.y + state.offsetY),
		},
		updatePosition,
		beginManualPositioning,
	};
}

function getReferenceContextElement(target: ReferenceElement): Element | null {
	if ('ownerDocument' in target) return target as Element;
	const virtualTarget = target as VirtualElement;
	const contextElement = virtualTarget.contextElement;
	if (contextElement == null) return null;
	return contextElement;
}

function isReferenceConnected(target: ReferenceElement): boolean {
	const contextElement = getReferenceContextElement(target);
	if (contextElement == null) return true;
	const ownerDocument = contextElement.ownerDocument;
	const ownerWindow = ownerDocument.defaultView;
	if (ownerWindow == null || !(contextElement instanceof ownerWindow.Element)) return true;
	return ownerDocument.contains(contextElement);
}

function getInitialGuess(
	target: ReferenceElement,
	placement: Placement,
	offsetMainAxis: number,
	offsetCrossAxis: number,
) {
	const rect = target.getBoundingClientRect();
	const [side, align = 'center'] = placement.split('-') as [string, string];
	let x = rect.left;
	let y = rect.top;
	switch (side) {
		case 'right':
			x = rect.right + offsetMainAxis;
			break;
		case 'left':
			x = rect.left - offsetMainAxis;
			break;
		case 'bottom':
			y = rect.bottom + offsetMainAxis;
			break;
		case 'top':
			y = rect.top - offsetMainAxis;
			break;
		default:
			break;
	}
	if (side === 'top' || side === 'bottom') {
		if (align === 'end') {
			x = rect.right;
		} else if (align === 'start') {
			x = rect.left;
		} else {
			x = rect.left + rect.width / 2;
		}
		x += offsetCrossAxis;
	} else {
		if (align === 'end') {
			y = rect.bottom;
		} else if (align === 'start') {
			y = rect.top;
		} else {
			y = rect.top + rect.height / 2;
		}
		y += offsetCrossAxis;
	}
	return {x, y};
}
