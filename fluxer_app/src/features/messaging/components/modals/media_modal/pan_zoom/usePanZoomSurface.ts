// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ZoomState} from '@app/features/messaging/components/modals/media_modal/shared';
import {wasPointerDownInside} from '@app/lib/overlay/DismissGuard';
import type {AnimationPlaybackControls, MotionValue} from 'framer-motion';
import {animate, useMotionValue, useReducedMotion} from 'framer-motion';
import type React from 'react';
import {useCallback, useEffect, useLayoutEffect, useMemo, useRef, useState} from 'react';
import {
	clampPanForScale,
	clampScale,
	DEFAULT_ZOOM_SCALE,
	getAnchoredZoomPoint,
	getCentroid,
	getDistance,
	getViewportPoint,
	isDefaultTransform,
	MAX_ZOOM_SCALE,
	MIN_ZOOM_SCALE,
	type PanZoomMetrics,
	type Point,
	TAP_MOVE_THRESHOLD,
	ZOOM_STATE_EPSILON,
	ZOOM_STEP,
} from './PanZoomMath';
import {useLatestRef} from './useLatestRef';

interface PointerRecord extends Point {
	pointerId: number;
}

interface PanGesture {
	mode: 'pan';
	pointerId: number;
	startPointer: Point;
	startX: number;
	startY: number;
	moved: boolean;
	startedOnContent: boolean;
	startedOnBackdrop: boolean;
	followsPinch: boolean;
}

interface PinchGesture {
	mode: 'pinch';
	startCentroid: Point;
	startDistance: number;
	startScale: number;
	startX: number;
	startY: number;
	moved: boolean;
}

type GestureState = PanGesture | PinchGesture | {mode: 'idle'};

export interface PanZoomTransformSnapshot {
	scale: number;
	x: number;
	y: number;
	zoomState: ZoomState;
	isDragging: boolean;
	isDefault: boolean;
	canZoomIn: boolean;
	canZoomOut: boolean;
}

export interface UsePanZoomSurfaceOptions {
	zoomState?: ZoomState;
	minScale?: number;
	maxScale?: number;
	zoomedScale?: number;
	disabled?: boolean;
	panDisabled?: boolean;
	wheelEnabled?: boolean;
	pinchEnabled?: boolean;
	doubleClickEnabled?: boolean;
	tapToToggleZoom?: boolean;
	resetKey?: unknown;
	onZoomStateChange?: (state: ZoomState) => void;
	onTransformChange?: (snapshot: PanZoomTransformSnapshot) => void;
	onTap?: () => void;
	onBackdropTap?: () => void;
}

interface PanZoomSurfaceBindings {
	ref: React.RefObject<HTMLDivElement | null>;
	onPointerDown: (event: React.PointerEvent<HTMLDivElement>) => void;
	onPointerMove: (event: React.PointerEvent<HTMLDivElement>) => void;
	onPointerUp: (event: React.PointerEvent<HTMLDivElement>) => void;
	onPointerCancel: (event: React.PointerEvent<HTMLDivElement>) => void;
	onDoubleClick: (event: React.MouseEvent<HTMLDivElement>) => void;
}

interface PanZoomContentBindings {
	ref: React.RefObject<HTMLDivElement | null>;
	onPointerEnter: () => void;
	onPointerLeave: () => void;
}

export interface PanZoomSurfaceController {
	x: MotionValue<number>;
	y: MotionValue<number>;
	scale: MotionValue<number>;
	zoomState: ZoomState;
	isDragging: boolean;
	isHoveringContent: boolean;
	cursor: string;
	viewportBindings: PanZoomSurfaceBindings;
	contentBindings: PanZoomContentBindings;
	zoomTo: (state: ZoomState, origin?: Point) => void;
	zoomIn: (origin?: Point) => void;
	zoomOut: (origin?: Point) => void;
	reset: () => void;
	getSnapshot: () => PanZoomTransformSnapshot;
}

const ZOOM_TRANSITION = {
	type: 'spring' as const,
	stiffness: 300,
	damping: 30,
	mass: 1,
};
const NON_PASSIVE_EVENT_LISTENER_OPTIONS: AddEventListenerOptions = {passive: false};
const MEASURED_BOX_SELECTOR = '[data-pan-zoom-measured-box]';
const MEASURED_ELEMENT_SELECTOR = 'img, video, canvas, svg';

function toZoomState(scale: number, minScale: number): ZoomState {
	return scale <= minScale + ZOOM_STATE_EPSILON ? 'fit' : 'zoomed';
}

function mix(start: number, end: number, progress: number): number {
	return start + (end - start) * progress;
}

function getPointerRecords(records: Map<number, PointerRecord>): Array<PointerRecord> {
	return Array.from(records.values());
}

function getMediaCandidates(content: HTMLElement): Array<Element> {
	const measuredBox = content.querySelector<HTMLElement>(MEASURED_BOX_SELECTOR) ?? content;
	return Array.from(measuredBox.querySelectorAll(MEASURED_ELEMENT_SELECTOR));
}

function findMediaElement(content: HTMLElement): Element | null {
	let widest: Element | null = null;
	let widestArea = 0;
	for (const candidate of getMediaCandidates(content)) {
		const rect = candidate.getBoundingClientRect();
		const area = rect.width * rect.height;
		if (area > widestArea) {
			widestArea = area;
			widest = candidate;
		}
	}
	return widest;
}

export function usePanZoomSurface(options: UsePanZoomSurfaceOptions): PanZoomSurfaceController {
	const {
		zoomState: controlledZoomState,
		minScale = MIN_ZOOM_SCALE,
		maxScale = MAX_ZOOM_SCALE,
		zoomedScale = DEFAULT_ZOOM_SCALE,
		disabled = false,
		panDisabled = false,
		wheelEnabled = true,
		pinchEnabled = true,
		doubleClickEnabled = true,
		tapToToggleZoom = false,
		resetKey,
		onZoomStateChange,
		onTransformChange,
		onTap,
		onBackdropTap,
	} = options;
	const viewportRef = useRef<HTMLDivElement>(null);
	const contentRef = useRef<HTMLDivElement>(null);
	const x = useMotionValue(0);
	const y = useMotionValue(0);
	const scale = useMotionValue(minScale);
	const requestedTransformRef = useRef({scale: minScale, x: 0, y: 0});
	const prefersReducedMotion = useReducedMotion();
	const [internalZoomState, setInternalZoomState] = useState<ZoomState>(controlledZoomState ?? 'fit');
	const [isDragging, setIsDragging] = useState(false);
	const [isHoveringContent, setIsHoveringContent] = useState(false);
	const pointerRecordsRef = useRef(new Map<number, PointerRecord>());
	const gestureRef = useRef<GestureState>({mode: 'idle'});
	const metricsRef = useRef<PanZoomMetrics | null>(null);
	const viewportRectRef = useRef<DOMRectReadOnly | null>(null);
	const animationControlsRef = useRef<Array<AnimationPlaybackControls>>([]);
	const metricsFrameRef = useRef<number | null>(null);
	const isDraggingRef = useRef(false);
	const lastCommittedZoomStateRef = useRef<ZoomState>(controlledZoomState ?? 'fit');
	const controlledZoomStateRef = useLatestRef(controlledZoomState);
	const onZoomStateChangeRef = useLatestRef(onZoomStateChange);
	const onTransformChangeRef = useLatestRef(onTransformChange);
	const onTapRef = useLatestRef(onTap);
	const onBackdropTapRef = useLatestRef(onBackdropTap);
	const activeZoomState = controlledZoomState ?? internalZoomState;
	const minScaleRef = useLatestRef(minScale);
	const maxScaleRef = useLatestRef(maxScale);
	const zoomedScaleRef = useLatestRef(zoomedScale);
	const disabledRef = useLatestRef(disabled);
	const panDisabledRef = useLatestRef(panDisabled);
	const stopAnimations = useCallback(() => {
		if (animationControlsRef.current.length === 0) return;
		for (const controls of animationControlsRef.current) {
			controls.stop();
		}
		animationControlsRef.current = [];
		requestedTransformRef.current = {scale: scale.get(), x: x.get(), y: y.get()};
	}, [scale, x, y]);
	const measureMetrics = useCallback((): PanZoomMetrics | null => {
		const viewport = viewportRef.current;
		const content = contentRef.current;
		if (!viewport || !content) return null;
		const viewportRect = viewport.getBoundingClientRect();
		viewportRectRef.current = viewportRect;
		const activeScale = scale.get();
		const inverseScale = activeScale > 0 ? 1 / activeScale : 1;
		const mediaRect = findMediaElement(content)?.getBoundingClientRect();
		const hasMediaRect = mediaRect != null && mediaRect.width > 0 && mediaRect.height > 0;
		const metrics = {
			viewportWidth: viewportRect.width,
			viewportHeight: viewportRect.height,
			contentWidth: hasMediaRect ? mediaRect.width * inverseScale : content.offsetWidth || viewportRect.width,
			contentHeight: hasMediaRect ? mediaRect.height * inverseScale : content.offsetHeight || viewportRect.height,
		};
		metricsRef.current = metrics;
		return metrics;
	}, [scale]);
	const getMetrics = useCallback((): PanZoomMetrics | null => metricsRef.current ?? measureMetrics(), [measureMetrics]);
	const scheduleMetricsRefresh = useCallback(() => {
		if (typeof window === 'undefined' || metricsFrameRef.current != null) return;
		metricsFrameRef.current = window.requestAnimationFrame(() => {
			metricsFrameRef.current = null;
			measureMetrics();
		});
	}, [measureMetrics]);
	const commitZoomState = useCallback(
		(nextZoomState: ZoomState, emit = true) => {
			if (lastCommittedZoomStateRef.current === nextZoomState) return;
			lastCommittedZoomStateRef.current = nextZoomState;
			if (controlledZoomStateRef.current === undefined) {
				setInternalZoomState(nextZoomState);
			}
			if (emit) {
				onZoomStateChangeRef.current?.(nextZoomState);
			}
		},
		[controlledZoomStateRef, onZoomStateChangeRef],
	);
	const canStepScale = useCallback(
		(fromScale: number, factor: number) =>
			clampScale(fromScale * factor, minScaleRef.current, maxScaleRef.current) !== fromScale,
		[maxScaleRef, minScaleRef],
	);
	const getSnapshot = useCallback((): PanZoomTransformSnapshot => {
		const requested = requestedTransformRef.current;
		return {
			scale: requested.scale,
			x: requested.x,
			y: requested.y,
			zoomState: lastCommittedZoomStateRef.current,
			isDragging: gestureRef.current.mode !== 'idle',
			isDefault: isDefaultTransform({
				scale: requested.scale,
				x: requested.x,
				y: requested.y,
				minScale: minScaleRef.current,
			}),
			canZoomIn: canStepScale(requested.scale, ZOOM_STEP),
			canZoomOut: canStepScale(requested.scale, 1 / ZOOM_STEP),
		};
	}, [canStepScale, minScaleRef]);
	const commitRequestedTransform = useCallback(
		(next: {scale: number; x: number; y: number}) => {
			requestedTransformRef.current = next;
			commitZoomState(toZoomState(next.scale, minScaleRef.current));
			onTransformChangeRef.current?.(getSnapshot());
		},
		[commitZoomState, getSnapshot, minScaleRef, onTransformChangeRef],
	);
	const clampPanToMetrics = useCallback(
		(metrics: PanZoomMetrics | null) => {
			if (!metrics) return;
			const currentX = x.get();
			const currentY = y.get();
			const clamped = clampPanForScale({x: currentX, y: currentY}, scale.get(), metrics);
			if (clamped.x === currentX && clamped.y === currentY) return;
			x.set(clamped.x);
			y.set(clamped.y);
			commitRequestedTransform({scale: requestedTransformRef.current.scale, x: clamped.x, y: clamped.y});
		},
		[commitRequestedTransform, scale, x, y],
	);
	const updateDragging = useCallback((nextIsDragging: boolean) => {
		if (isDraggingRef.current === nextIsDragging) return;
		isDraggingRef.current = nextIsDragging;
		setIsDragging(nextIsDragging);
	}, []);
	useLayoutEffect(() => {
		measureMetrics();
		const viewport = viewportRef.current;
		const content = contentRef.current;
		if (!viewport || !content) return;
		const ownerWindow = viewport.ownerDocument.defaultView;
		const handleResize = () => {
			clampPanToMetrics(measureMetrics());
		};
		const handleSettledTransform = (event: Event) => {
			if ((event as TransitionEvent).propertyName !== 'transform') return;
			if (event.target === content) return;
			clampPanToMetrics(measureMetrics());
		};
		ownerWindow?.addEventListener('resize', handleResize);
		content.addEventListener('transitionend', handleSettledTransform);
		content.addEventListener('transitioncancel', handleSettledTransform);
		const ResizeObserverCtor = ownerWindow?.ResizeObserver;
		if (!ResizeObserverCtor) {
			return () => {
				ownerWindow?.removeEventListener('resize', handleResize);
				content.removeEventListener('transitionend', handleSettledTransform);
				content.removeEventListener('transitioncancel', handleSettledTransform);
			};
		}
		const observer = new ResizeObserverCtor(handleResize);
		observer.observe(viewport);
		observer.observe(content);
		for (const candidate of getMediaCandidates(content)) {
			observer.observe(candidate);
		}
		return () => {
			observer.disconnect();
			ownerWindow?.removeEventListener('resize', handleResize);
			content.removeEventListener('transitionend', handleSettledTransform);
			content.removeEventListener('transitioncancel', handleSettledTransform);
		};
	}, [clampPanToMetrics, measureMetrics, resetKey]);
	const getZoomedScale = useCallback(
		() => clampScale(zoomedScaleRef.current, minScaleRef.current, maxScaleRef.current),
		[maxScaleRef, minScaleRef, zoomedScaleRef],
	);
	const setTransform = useCallback(
		(next: {scale: number; x: number; y: number}, animated: boolean) => {
			stopAnimations();
			commitRequestedTransform(next);
			if (animated && !prefersReducedMotion) {
				const start = {
					scale: scale.get(),
					x: x.get(),
					y: y.get(),
				};
				const controls = animate(0, 1, {
					...ZOOM_TRANSITION,
					onUpdate: (progress) => {
						x.set(mix(start.x, next.x, progress));
						y.set(mix(start.y, next.y, progress));
						scale.set(mix(start.scale, next.scale, progress));
					},
					onComplete: () => {
						x.set(next.x);
						y.set(next.y);
						scale.set(next.scale);
						animationControlsRef.current = [];
					},
				});
				animationControlsRef.current = [controls];
			} else {
				x.set(next.x);
				y.set(next.y);
				scale.set(next.scale);
			}
		},
		[commitRequestedTransform, prefersReducedMotion, scale, stopAnimations, x, y],
	);
	const zoomTo = useCallback(
		(nextZoomState: ZoomState, origin?: Point) => {
			const metrics = measureMetrics();
			if (!metrics) return;
			const nextScale = nextZoomState === 'fit' ? minScaleRef.current : getZoomedScale();
			const requested = requestedTransformRef.current;
			const nextPoint =
				nextZoomState === 'fit'
					? {x: 0, y: 0}
					: getAnchoredZoomPoint({
							origin: origin ?? {x: 0, y: 0},
							current: {x: requested.x, y: requested.y},
							currentScale: requested.scale,
							nextScale,
							metrics,
						});
			setTransform({scale: nextScale, x: nextPoint.x, y: nextPoint.y}, true);
		},
		[getZoomedScale, measureMetrics, minScaleRef, setTransform],
	);
	const zoomBy = useCallback(
		(factor: number, origin?: Point) => {
			const metrics = measureMetrics();
			if (!metrics) return;
			const requested = requestedTransformRef.current;
			const currentScale = requested.scale;
			const nextScale = clampScale(currentScale * factor, minScaleRef.current, maxScaleRef.current);
			if (nextScale === currentScale) return;
			const nextPoint = getAnchoredZoomPoint({
				origin: origin ?? {x: 0, y: 0},
				current: {x: requested.x, y: requested.y},
				currentScale,
				nextScale,
				metrics,
			});
			setTransform({scale: nextScale, x: nextPoint.x, y: nextPoint.y}, true);
		},
		[maxScaleRef, measureMetrics, minScaleRef, setTransform],
	);
	const zoomIn = useCallback((origin?: Point) => zoomBy(ZOOM_STEP, origin), [zoomBy]);
	const zoomOut = useCallback((origin?: Point) => zoomBy(1 / ZOOM_STEP, origin), [zoomBy]);
	const reset = useCallback(() => {
		setTransform({scale: minScaleRef.current, x: 0, y: 0}, true);
	}, [minScaleRef, setTransform]);
	useEffect(() => {
		if (controlledZoomState === undefined) return;
		if (lastCommittedZoomStateRef.current === controlledZoomState) return;
		commitZoomState(controlledZoomState, false);
		zoomTo(controlledZoomState);
	}, [commitZoomState, controlledZoomState, zoomTo]);
	useEffect(() => {
		measureMetrics();
		reset();
	}, [measureMetrics, reset, resetKey]);
	useEffect(() => {
		return () => stopAnimations();
	}, [stopAnimations]);
	useEffect(() => {
		return () => {
			if (typeof window === 'undefined') return;
			if (metricsFrameRef.current != null) {
				window.cancelAnimationFrame(metricsFrameRef.current);
			}
		};
	}, []);
	const updatePointerRecord = useCallback((event: React.PointerEvent<HTMLDivElement>) => {
		pointerRecordsRef.current.set(event.pointerId, {
			pointerId: event.pointerId,
			x: event.clientX,
			y: event.clientY,
		});
	}, []);
	const getViewportRelativePoint = useCallback(
		(clientX: number, clientY: number): Point | null => {
			let rect = viewportRectRef.current;
			if (!rect) {
				measureMetrics();
				rect = viewportRectRef.current;
			}
			if (!rect) return null;
			return getViewportPoint(clientX, clientY, rect);
		},
		[measureMetrics],
	);
	const handleWheel = useCallback(
		(event: WheelEvent) => {
			if (disabledRef.current || !wheelEnabled || event.ctrlKey || panDisabledRef.current) return;
			const currentScale = scale.get();
			if (currentScale <= minScaleRef.current + ZOOM_STATE_EPSILON) return;
			const metrics = getMetrics();
			if (!metrics) return;
			event.preventDefault();
			stopAnimations();
			const requested = requestedTransformRef.current;
			const pannedPoint = clampPanForScale(
				{x: requested.x - event.deltaX, y: requested.y - event.deltaY},
				requested.scale,
				metrics,
			);
			x.set(pannedPoint.x);
			y.set(pannedPoint.y);
			commitRequestedTransform({scale: requested.scale, x: pannedPoint.x, y: pannedPoint.y});
			scheduleMetricsRefresh();
		},
		[
			commitRequestedTransform,
			disabledRef,
			getMetrics,
			minScaleRef,
			panDisabledRef,
			scale,
			scheduleMetricsRefresh,
			stopAnimations,
			wheelEnabled,
		],
	);
	useEffect(() => {
		const viewport = viewportRef.current;
		if (!viewport) return;
		viewport.addEventListener('wheel', handleWheel, NON_PASSIVE_EVENT_LISTENER_OPTIONS);
		return () => {
			viewport.removeEventListener('wheel', handleWheel, NON_PASSIVE_EVENT_LISTENER_OPTIONS);
		};
	}, [handleWheel]);
	const handlePointerDown = useCallback(
		(event: React.PointerEvent<HTMLDivElement>) => {
			if (event.button !== 0 && event.pointerType === 'mouse') return;
			measureMetrics();
			updatePointerRecord(event);
			event.currentTarget.setPointerCapture?.(event.pointerId);
			const target = event.target as Node | null;
			const startedOnContent = Boolean(target && contentRef.current?.contains(target));
			const startedOnBackdrop = event.target === event.currentTarget;
			const pointer: Point = {x: event.clientX, y: event.clientY};
			const currentScale = scale.get();
			if (!disabledRef.current) {
				stopAnimations();
			}
			if (pointerRecordsRef.current.size === 1) {
				gestureRef.current = {
					mode: 'pan',
					pointerId: event.pointerId,
					startPointer: pointer,
					startX: x.get(),
					startY: y.get(),
					moved: false,
					startedOnContent,
					startedOnBackdrop,
					followsPinch: false,
				};
				updateDragging(
					!disabledRef.current && !panDisabledRef.current && currentScale > minScaleRef.current + ZOOM_STATE_EPSILON,
				);
				return;
			}
			if (!pinchEnabled || disabledRef.current || pointerRecordsRef.current.size !== 2) return;
			const records = getPointerRecords(pointerRecordsRef.current);
			const viewportRect = viewportRectRef.current;
			if (!viewportRect || records.length !== 2) return;
			const first = getViewportPoint(records[0].x, records[0].y, viewportRect);
			const second = getViewportPoint(records[1].x, records[1].y, viewportRect);
			gestureRef.current = {
				mode: 'pinch',
				startCentroid: getCentroid(first, second),
				startDistance: Math.max(1, getDistance(first, second)),
				startScale: currentScale,
				startX: x.get(),
				startY: y.get(),
				moved: false,
			};
			updateDragging(true);
		},
		[
			disabledRef,
			measureMetrics,
			minScaleRef,
			panDisabledRef,
			pinchEnabled,
			scale,
			stopAnimations,
			updateDragging,
			updatePointerRecord,
			x,
			y,
		],
	);
	const handlePointerMove = useCallback(
		(event: React.PointerEvent<HTMLDivElement>) => {
			if (!pointerRecordsRef.current.has(event.pointerId)) return;
			updatePointerRecord(event);
			const gesture = gestureRef.current;
			if (gesture.mode === 'idle' || disabledRef.current) return;
			const metrics = getMetrics();
			if (!metrics) return;
			if (gesture.mode === 'pan') {
				const dx = event.clientX - gesture.startPointer.x;
				const dy = event.clientY - gesture.startPointer.y;
				const moved = Math.hypot(dx, dy) >= TAP_MOVE_THRESHOLD;
				gesture.moved = gesture.moved || moved;
				if (panDisabledRef.current || scale.get() <= minScaleRef.current + ZOOM_STATE_EPSILON) return;
				const nextPoint = clampPanForScale(
					{
						x: gesture.startX + dx,
						y: gesture.startY + dy,
					},
					scale.get(),
					metrics,
				);
				x.set(nextPoint.x);
				y.set(nextPoint.y);
				commitRequestedTransform({scale: requestedTransformRef.current.scale, x: nextPoint.x, y: nextPoint.y});
				updateDragging(true);
				return;
			}
			if (!pinchEnabled) return;
			const records = getPointerRecords(pointerRecordsRef.current);
			if (records.length < 2) return;
			const viewportRect = viewportRectRef.current;
			if (!viewportRect) return;
			const first = getViewportPoint(records[0].x, records[0].y, viewportRect);
			const second = getViewportPoint(records[1].x, records[1].y, viewportRect);
			const centroid = getCentroid(first, second);
			const distance = Math.max(1, getDistance(first, second));
			const nextScale = Math.min(
				maxScaleRef.current,
				Math.max(minScaleRef.current, gesture.startScale * (distance / gesture.startDistance)),
			);
			const localPoint = {
				x: (gesture.startCentroid.x - gesture.startX) / gesture.startScale,
				y: (gesture.startCentroid.y - gesture.startY) / gesture.startScale,
			};
			const nextPoint = clampPanForScale(
				{
					x: centroid.x - localPoint.x * nextScale,
					y: centroid.y - localPoint.y * nextScale,
				},
				nextScale,
				metrics,
			);
			gesture.moved = true;
			scale.set(nextScale);
			x.set(nextPoint.x);
			y.set(nextPoint.y);
			commitRequestedTransform({scale: nextScale, x: nextPoint.x, y: nextPoint.y});
		},
		[
			commitRequestedTransform,
			disabledRef,
			getMetrics,
			maxScaleRef,
			minScaleRef,
			panDisabledRef,
			pinchEnabled,
			scale,
			updateDragging,
			updatePointerRecord,
			x,
			y,
		],
	);
	const finishPointer = useCallback(
		(event: React.PointerEvent<HTMLDivElement>) => {
			const gesture = gestureRef.current;
			pointerRecordsRef.current.delete(event.pointerId);
			event.currentTarget.releasePointerCapture?.(event.pointerId);
			if (gesture.mode === 'pan' && gesture.pointerId === event.pointerId) {
				const dx = event.clientX - gesture.startPointer.x;
				const dy = event.clientY - gesture.startPointer.y;
				const moved = gesture.moved || Math.hypot(dx, dy) >= TAP_MOVE_THRESHOLD;
				updateDragging(false);
				gestureRef.current = {mode: 'idle'};
				if (!moved && !gesture.followsPinch) {
					if (gesture.startedOnBackdrop && !wasPointerDownInside(contentRef.current)) {
						onBackdropTapRef.current?.();
						return;
					}
					if (gesture.startedOnContent) {
						onTapRef.current?.();
						if (tapToToggleZoom && !disabledRef.current) {
							const origin = getViewportRelativePoint(event.clientX, event.clientY) ?? {x: 0, y: 0};
							zoomTo(lastCommittedZoomStateRef.current === 'fit' ? 'zoomed' : 'fit', origin);
						}
					}
				}
			}
			if (pointerRecordsRef.current.size === 1) {
				const [remaining] = getPointerRecords(pointerRecordsRef.current);
				gestureRef.current = {
					mode: 'pan',
					pointerId: remaining.pointerId,
					startPointer: {x: remaining.x, y: remaining.y},
					startX: x.get(),
					startY: y.get(),
					moved: false,
					startedOnContent: true,
					startedOnBackdrop: false,
					followsPinch: true,
				};
				updateDragging(scale.get() > minScaleRef.current + ZOOM_STATE_EPSILON);
				return;
			}
			if (pointerRecordsRef.current.size === 0) {
				gestureRef.current = {mode: 'idle'};
				updateDragging(false);
			}
		},
		[
			disabledRef,
			getViewportRelativePoint,
			minScaleRef,
			onBackdropTapRef,
			onTapRef,
			scale,
			tapToToggleZoom,
			updateDragging,
			x,
			y,
			zoomTo,
		],
	);
	const handlePointerCancel = useCallback(
		(event: React.PointerEvent<HTMLDivElement>) => {
			pointerRecordsRef.current.delete(event.pointerId);
			gestureRef.current = {mode: 'idle'};
			updateDragging(false);
		},
		[updateDragging],
	);
	const handleDoubleClick = useCallback(
		(event: React.MouseEvent<HTMLDivElement>) => {
			if (disabledRef.current || !doubleClickEnabled) return;
			if (!contentRef.current?.contains(event.target as Node)) return;
			event.preventDefault();
			const origin = getViewportRelativePoint(event.clientX, event.clientY) ?? {x: 0, y: 0};
			zoomTo(lastCommittedZoomStateRef.current === 'fit' ? 'zoomed' : 'fit', origin);
		},
		[disabledRef, doubleClickEnabled, getViewportRelativePoint, zoomTo],
	);
	const cursor = useMemo(() => {
		if (disabled) return 'default';
		if (!isHoveringContent) return 'default';
		if (tapToToggleZoom) return activeZoomState === 'zoomed' ? 'zoom-out' : 'zoom-in';
		if (isDragging) return 'grabbing';
		if (activeZoomState === 'zoomed') return 'grab';
		return doubleClickEnabled ? 'zoom-in' : 'default';
	}, [activeZoomState, disabled, doubleClickEnabled, isDragging, isHoveringContent, tapToToggleZoom]);
	return {
		x,
		y,
		scale,
		zoomState: activeZoomState,
		isDragging,
		isHoveringContent,
		cursor,
		viewportBindings: {
			ref: viewportRef,
			onPointerDown: handlePointerDown,
			onPointerMove: handlePointerMove,
			onPointerUp: finishPointer,
			onPointerCancel: handlePointerCancel,
			onDoubleClick: handleDoubleClick,
		},
		contentBindings: {
			ref: contentRef,
			onPointerEnter: () => setIsHoveringContent(true),
			onPointerLeave: () => setIsHoveringContent(false),
		},
		zoomTo,
		zoomIn,
		zoomOut,
		reset,
		getSnapshot,
	};
}
