// SPDX-License-Identifier: AGPL-3.0-or-later

import {appZoomLayoutPx} from '@app/features/ui/utils/AppZoomUtils';
import {
	COMPACT_HEIGHT_MESSAGES_SLIVER,
	getCompactHeightMax,
	resolveCompactHeightMaxFromLayout,
} from '@app/features/voice/components/compact_voice_call_view/CompactVoiceCallHeightBounds';
import {
	COMPACT_HEIGHT_DRAG_THRESHOLD_SQ,
	COMPACT_HEIGHT_STEP,
	type ResizeListeners,
	type ResizeState,
	toLayoutPx,
} from '@app/features/voice/components/compact_voice_call_view/shared';
import CompactVoiceCallHeight from '@app/features/voice/state/CompactVoiceCallHeight';
import type React from 'react';
import {useCallback, useEffect, useLayoutEffect, useRef, useState} from 'react';

const CHANNEL_GRID_SELECTOR = '[data-flx="channel.channel-view.channel-view-scaffold.channel-grid"]';
const CHAT_AREA_SLOT_SELECTOR = '[data-flx="channel.channel-view.channel-view-scaffold.chat-area-slot"]';
const COMPOSER_SELECTOR = '[data-flx="channel.channel-chat-layout.textarea-area"]';
const TYPING_AREA_SELECTOR = '[data-flx="channel.channel-chat-layout.typing-area"]';

export function useCompactVoiceCallResize({
	containerRef,
	heightKey,
	isResizable,
	compactHeightMin,
}: {
	containerRef: React.RefObject<HTMLElement | null>;
	heightKey: string;
	isResizable: boolean;
	compactHeightMin: number;
}) {
	const resizeStateRef = useRef<ResizeState | null>(null);
	const resizeListenersRef = useRef<ResizeListeners | null>(null);
	const pendingResizeMoveRef = useRef<{pointerId: number; clientY: number} | null>(null);
	const resizeFrameRef = useRef<number | null>(null);
	const maxHeightResizeFrameRef = useRef<number | null>(null);
	const compactHeightKeyRef = useRef(heightKey);
	const [isResizing, setIsResizing] = useState(false);
	const [maxHeight, setMaxHeight] = useState(() => getCompactHeightMax(compactHeightMin));
	const [compactHeight, setCompactHeight] = useState<number | null>(() =>
		isResizable
			? Math.max(
					compactHeightMin,
					Math.min(Math.round(CompactVoiceCallHeight.getStartingHeight(heightKey) ?? compactHeightMin), maxHeight),
				)
			: null,
	);
	const measureMaxHeight = useCallback((): number => {
		const callViewEl = containerRef.current;
		const fallback = getCompactHeightMax(compactHeightMin);
		if (!callViewEl) return fallback;
		const grid = callViewEl.closest(CHANNEL_GRID_SELECTOR);
		const chatAreaSlot = grid?.querySelector(CHAT_AREA_SLOT_SELECTOR);
		const composer = grid?.querySelector(COMPOSER_SELECTOR);
		if (!grid || !chatAreaSlot || !composer) return fallback;
		const typingArea = grid.querySelector(TYPING_AREA_SELECTOR);
		const callViewTop = callViewEl.getBoundingClientRect().top;
		const chatAreaBottom = chatAreaSlot.getBoundingClientRect().bottom;
		const composerHeight = composer.getBoundingClientRect().height;
		const typingHeight = typingArea?.getBoundingClientRect().height ?? 0;
		const availableSpan = toLayoutPx(chatAreaBottom - callViewTop);
		if (availableSpan <= 0) return fallback;
		const chatReservation = toLayoutPx(composerHeight + typingHeight) + COMPACT_HEIGHT_MESSAGES_SLIVER;
		return resolveCompactHeightMaxFromLayout({compactHeightMin, availableSpan, chatReservation});
	}, [compactHeightMin, containerRef]);
	const clampHeight = useCallback(
		(value: number) => Math.max(compactHeightMin, Math.min(Math.round(value), maxHeight)),
		[compactHeightMin, maxHeight],
	);
	const setCompactHeightForKey = useCallback(
		(nextHeight: number, options: {persist?: boolean} = {}) => {
			const normalizedHeight = clampHeight(nextHeight);
			if (isResizable && options.persist) {
				const persistedHeight = CompactVoiceCallHeight.setHeightForKey(heightKey, normalizedHeight);
				setCompactHeight(persistedHeight);
				return;
			}
			setCompactHeight(normalizedHeight);
		},
		[clampHeight, heightKey, isResizable],
	);
	useEffect(() => {
		if (!isResizable) return;
		const updateMaxHeight = () => {
			maxHeightResizeFrameRef.current = null;
			const nextMaxHeight = measureMaxHeight();
			setMaxHeight((previousMaxHeight) => (previousMaxHeight === nextMaxHeight ? previousMaxHeight : nextMaxHeight));
		};
		const scheduleMaxHeightUpdate = () => {
			if (maxHeightResizeFrameRef.current !== null) return;
			maxHeightResizeFrameRef.current = requestAnimationFrame(updateMaxHeight);
		};
		scheduleMaxHeightUpdate();
		window.addEventListener('resize', scheduleMaxHeightUpdate);
		let composerObserver: ResizeObserver | null = null;
		const grid = containerRef.current?.closest(CHANNEL_GRID_SELECTOR);
		if (grid && typeof ResizeObserver !== 'undefined') {
			composerObserver = new ResizeObserver(scheduleMaxHeightUpdate);
			const composer = grid.querySelector(COMPOSER_SELECTOR);
			const typingArea = grid.querySelector(TYPING_AREA_SELECTOR);
			if (composer) composerObserver.observe(composer);
			if (typingArea) composerObserver.observe(typingArea);
		}
		return () => {
			window.removeEventListener('resize', scheduleMaxHeightUpdate);
			composerObserver?.disconnect();
			if (maxHeightResizeFrameRef.current !== null) {
				cancelAnimationFrame(maxHeightResizeFrameRef.current);
				maxHeightResizeFrameRef.current = null;
			}
		};
	}, [containerRef, isResizable, measureMaxHeight]);
	useLayoutEffect(() => {
		if (!isResizable) return;
		setCompactHeightForKey(CompactVoiceCallHeight.getStartingHeight(heightKey) ?? compactHeightMin);
	}, [heightKey, isResizable, setCompactHeightForKey, compactHeightMin]);
	useLayoutEffect(() => {
		if (!isResizable || compactHeight == null) return;
		compactHeightKeyRef.current = heightKey;
	}, [compactHeight, heightKey, isResizable]);
	useLayoutEffect(() => {
		if (!isResizable || compactHeight == null) return;
		if (compactHeightKeyRef.current !== heightKey) return;
		if (compactHeight > maxHeight) {
			setCompactHeightForKey(maxHeight);
			return;
		}
		if (compactHeight < compactHeightMin) {
			setCompactHeightForKey(compactHeightMin);
		}
	}, [compactHeight, compactHeightMin, heightKey, isResizable, maxHeight, setCompactHeightForKey]);
	useLayoutEffect(() => {
		if (!isResizable) return;
		if (compactHeight != null) return;
		const container = containerRef.current;
		if (!container) return;
		const measured = toLayoutPx(container.getBoundingClientRect().height);
		if (!Number.isFinite(measured) || measured <= 0) return;
		setCompactHeightForKey(measured);
	}, [compactHeight, containerRef, isResizable, setCompactHeightForKey]);
	useEffect(() => {
		if (!isResizing) return;
		const prevCursor = document.body.style.cursor;
		const prevSelect = document.body.style.userSelect;
		document.body.style.cursor = 'ns-resize';
		document.body.style.userSelect = 'none';
		return () => {
			document.body.style.cursor = prevCursor;
			document.body.style.userSelect = prevSelect;
		};
	}, [isResizing]);
	const cancelPendingResizeFrame = useCallback(() => {
		if (resizeFrameRef.current !== null) {
			cancelAnimationFrame(resizeFrameRef.current);
			resizeFrameRef.current = null;
		}
	}, []);
	const flushResizePointerMove = useCallback(() => {
		resizeFrameRef.current = null;
		const pendingMove = pendingResizeMoveRef.current;
		pendingResizeMoveRef.current = null;
		if (!pendingMove || !isResizable) return;
		const state = resizeStateRef.current;
		if (!state || state.pointerId !== pendingMove.pointerId) return;
		const deltaY = appZoomLayoutPx(pendingMove.clientY - state.startY);
		if (!state.dragging) {
			if (deltaY * deltaY <= COMPACT_HEIGHT_DRAG_THRESHOLD_SQ) return;
			state.dragging = true;
			setIsResizing(true);
		}
		const nextHeight = clampHeight(state.startHeight + deltaY);
		state.lastHeight = nextHeight;
		setCompactHeightForKey(nextHeight);
	}, [clampHeight, isResizable, setCompactHeightForKey]);
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
	const handleResizePointerMove = useCallback(
		(event: PointerEvent) => {
			if (!isResizable) return;
			const state = resizeStateRef.current;
			if (!state || state.pointerId !== event.pointerId) return;
			event.preventDefault();
			pendingResizeMoveRef.current = {pointerId: event.pointerId, clientY: event.clientY};
			if (resizeFrameRef.current !== null) return;
			resizeFrameRef.current = requestAnimationFrame(flushResizePointerMove);
		},
		[flushResizePointerMove, isResizable],
	);
	const handleResizePointerUp = useCallback(
		(event: PointerEvent) => {
			if (!isResizable) return;
			const state = resizeStateRef.current;
			if (!state || state.pointerId !== event.pointerId) return;
			if (resizeFrameRef.current !== null) {
				cancelPendingResizeFrame();
				flushResizePointerMove();
			}
			cleanupResizeListeners();
			if (state.dragging) {
				setIsResizing(false);
				if (state.lastHeight != null) {
					setCompactHeightForKey(state.lastHeight, {persist: true});
				}
			}
			resizeStateRef.current = null;
		},
		[cancelPendingResizeFrame, cleanupResizeListeners, flushResizePointerMove, isResizable, setCompactHeightForKey],
	);
	const handleResizePointerDown = useCallback(
		(event: React.PointerEvent<HTMLDivElement>) => {
			if (!isResizable || event.button !== 0) return;
			event.preventDefault();
			event.stopPropagation();
			const container = containerRef.current;
			const startHeight =
				compactHeight ??
				container?.getBoundingClientRect().height ??
				CompactVoiceCallHeight.getStartingHeight(heightKey) ??
				compactHeightMin;
			resizeStateRef.current = {
				pointerId: event.pointerId,
				startY: event.clientY,
				startHeight,
				dragging: false,
			};
			const moveListener = (moveEvent: PointerEvent) => handleResizePointerMove(moveEvent);
			const upListener = (upEvent: PointerEvent) => handleResizePointerUp(upEvent);
			resizeListenersRef.current = {
				move: moveListener,
				up: upListener,
			};
			window.addEventListener('pointermove', moveListener);
			window.addEventListener('pointerup', upListener);
			window.addEventListener('pointercancel', upListener);
		},
		[
			compactHeight,
			compactHeightMin,
			containerRef,
			handleResizePointerMove,
			handleResizePointerUp,
			heightKey,
			isResizable,
		],
	);
	const handleResizeKeyDown = useCallback(
		(event: React.KeyboardEvent<HTMLDivElement>) => {
			if (!isResizable) return;
			if (event.key !== 'ArrowUp' && event.key !== 'ArrowDown') return;
			if (event.altKey || event.ctrlKey || event.metaKey || event.shiftKey) return;
			event.preventDefault();
			const direction = event.key === 'ArrowUp' ? -1 : 1;
			const baseHeight = compactHeight ?? containerRef.current?.getBoundingClientRect().height ?? compactHeightMin;
			setCompactHeightForKey(baseHeight + COMPACT_HEIGHT_STEP * direction, {persist: true});
		},
		[compactHeight, compactHeightMin, containerRef, isResizable, setCompactHeightForKey],
	);
	useEffect(() => {
		return () => {
			cleanupResizeListeners();
		};
	}, [cleanupResizeListeners]);
	return {
		compactHeight,
		maxHeight,
		isResizing,
		handleResizePointerDown,
		handleResizeKeyDown,
	};
}
