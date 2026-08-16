// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	calculateThumbState,
	clamp,
	getAxisPointerPosition,
	getAxisScrollMetrics,
	type ScrollAxis,
	type ScrollbarThumbState,
	setAxisScrollPosition,
} from '@app/features/ui/scroller/ScrollerMath';
import {type PointerEvent as ReactPointerEvent, useCallback, useEffect, useRef, useState} from 'react';

type ScrollOverflow = 'scroll' | 'auto' | 'hidden';

interface UseScrollerThumbOptions {
	orientation: ScrollAxis;
	overflow: ScrollOverflow;
	minThumbSize: number;
	scrollRef: React.RefObject<HTMLDivElement | null>;
	showTrack: boolean;
}

interface DragState {
	pointerId: number;
	thumbElement: HTMLDivElement;
	trackElement: HTMLElement;
	gripOffset: number;
}

interface AppliedThumbStyle {
	orientation: ScrollAxis;
	hasTrack: boolean;
	thumbSize: number;
	thumbOffset: number;
}

interface UseScrollerThumbResult {
	isDragging: boolean;
	refreshThumbState: () => void;
	thumbState: ScrollbarThumbState;
	setTrackElement: (element: HTMLDivElement | null) => void;
	setThumbElement: (element: HTMLDivElement | null) => void;
	onThumbPointerDown: (event: ReactPointerEvent<HTMLDivElement>) => void;
	onTrackPointerDown: (event: ReactPointerEvent<HTMLDivElement>) => void;
}

function createEmptyThumbState(): ScrollbarThumbState {
	return {
		hasTrack: false,
		thumbSize: 0,
		thumbOffset: 0,
		trackSize: 0,
		maxScrollPosition: 0,
	};
}

export function useScrollerThumb({
	orientation,
	overflow,
	minThumbSize,
	scrollRef,
	showTrack,
}: UseScrollerThumbOptions): UseScrollerThumbResult {
	const [isDragging, setIsDragging] = useState(false);
	const [thumbState, setThumbState] = useState<ScrollbarThumbState>(createEmptyThumbState);
	const thumbStateRef = useRef<ScrollbarThumbState>(thumbState);
	const trackElementRef = useRef<HTMLDivElement | null>(null);
	const trackResizeObserverRef = useRef<ResizeObserver | null>(null);
	const thumbElementRef = useRef<HTMLDivElement | null>(null);
	const appliedThumbStyleRef = useRef<AppliedThumbStyle | null>(null);
	const dragStateRef = useRef<DragState | null>(null);
	const previousUserSelectRef = useRef<string>('');
	const previousCursorRef = useRef<string>('');
	const applyThumbStateToElement = useCallback(
		(nextThumbState: ScrollbarThumbState) => {
			const thumbElement = thumbElementRef.current;
			if (!thumbElement) {
				return;
			}
			const nextStyle: AppliedThumbStyle = {
				orientation,
				hasTrack: nextThumbState.hasTrack,
				thumbSize: nextThumbState.thumbSize,
				thumbOffset: nextThumbState.thumbOffset,
			};
			const previousStyle = appliedThumbStyleRef.current;
			if (
				previousStyle?.orientation === nextStyle.orientation &&
				previousStyle.hasTrack === nextStyle.hasTrack &&
				previousStyle.thumbSize === nextStyle.thumbSize &&
				previousStyle.thumbOffset === nextStyle.thumbOffset
			) {
				return;
			}
			appliedThumbStyleRef.current = nextStyle;
			if (!nextThumbState.hasTrack) {
				thumbElement.style.removeProperty('height');
				thumbElement.style.removeProperty('width');
				thumbElement.style.removeProperty('transform');
				return;
			}
			if (orientation === 'vertical') {
				thumbElement.style.height = `${nextThumbState.thumbSize}px`;
				thumbElement.style.removeProperty('width');
				thumbElement.style.transform = `translate3d(0, ${nextThumbState.thumbOffset}px, 0)`;
			} else {
				thumbElement.style.width = `${nextThumbState.thumbSize}px`;
				thumbElement.style.removeProperty('height');
				thumbElement.style.transform = `translate3d(${nextThumbState.thumbOffset}px, 0, 0)`;
			}
		},
		[orientation],
	);
	const setThumbStateIfChanged = useCallback(
		(nextThumbState: ScrollbarThumbState) => {
			const previousState = thumbStateRef.current;
			const hasSameValues =
				previousState.hasTrack === nextThumbState.hasTrack &&
				previousState.thumbSize === nextThumbState.thumbSize &&
				previousState.thumbOffset === nextThumbState.thumbOffset &&
				previousState.trackSize === nextThumbState.trackSize &&
				previousState.maxScrollPosition === nextThumbState.maxScrollPosition;
			if (hasSameValues) {
				return;
			}
			thumbStateRef.current = nextThumbState;
			applyThumbStateToElement(nextThumbState);
			const shouldRender =
				previousState.hasTrack !== nextThumbState.hasTrack ||
				previousState.thumbSize !== nextThumbState.thumbSize ||
				previousState.trackSize !== nextThumbState.trackSize ||
				previousState.maxScrollPosition !== nextThumbState.maxScrollPosition;
			if (shouldRender) {
				setThumbState(nextThumbState);
			}
		},
		[applyThumbStateToElement],
	);
	const setThumbElement = useCallback(
		(element: HTMLDivElement | null) => {
			thumbElementRef.current = element;
			appliedThumbStyleRef.current = null;
			if (element) {
				applyThumbStateToElement(thumbStateRef.current);
			}
		},
		[applyThumbStateToElement],
	);
	const getTrackElementSize = useCallback((): number | undefined => {
		const trackElement = trackElementRef.current;
		if (!trackElement) return undefined;
		const rect = trackElement.getBoundingClientRect();
		return orientation === 'vertical' ? rect.height : rect.width;
	}, [orientation]);
	const refreshThumbState = useCallback(() => {
		const scrollElement = scrollRef.current;
		if (!scrollElement || overflow === 'hidden' || !showTrack) {
			setThumbStateIfChanged(createEmptyThumbState());
			return;
		}
		const metrics = getAxisScrollMetrics(scrollElement, orientation);
		const forceTrack = overflow === 'scroll';
		const nextThumbState = calculateThumbState(metrics, minThumbSize, forceTrack, getTrackElementSize());
		setThumbStateIfChanged(nextThumbState);
	}, [getTrackElementSize, minThumbSize, orientation, overflow, scrollRef, setThumbStateIfChanged, showTrack]);
	const setTrackElement = useCallback(
		(element: HTMLDivElement | null) => {
			trackResizeObserverRef.current?.disconnect();
			trackResizeObserverRef.current = null;
			trackElementRef.current = element;
			if (element) {
				const ownerWindow = element.ownerDocument.defaultView ?? window;
				if (typeof ownerWindow.ResizeObserver !== 'undefined') {
					const observer = new ownerWindow.ResizeObserver(() => {
						refreshThumbState();
					});
					observer.observe(element);
					trackResizeObserverRef.current = observer;
				}
				refreshThumbState();
			}
		},
		[refreshThumbState],
	);
	useEffect(() => {
		return () => {
			trackResizeObserverRef.current?.disconnect();
			trackResizeObserverRef.current = null;
		};
	}, []);
	const stopDragging = useCallback(() => {
		const dragState = dragStateRef.current;
		if (dragState) {
			try {
				dragState.thumbElement.releasePointerCapture(dragState.pointerId);
			} catch {}
		}
		dragStateRef.current = null;
		setIsDragging(false);
		if (previousUserSelectRef.current) {
			document.body.style.userSelect = previousUserSelectRef.current;
			previousUserSelectRef.current = '';
		} else {
			document.body.style.removeProperty('user-select');
		}
		if (previousCursorRef.current) {
			document.body.style.cursor = previousCursorRef.current;
			previousCursorRef.current = '';
		} else {
			document.body.style.removeProperty('cursor');
		}
	}, []);
	const handleWindowPointerMove = useCallback(
		(event: PointerEvent) => {
			const dragState = dragStateRef.current;
			const scrollElement = scrollRef.current;
			if (!dragState || !scrollElement) {
				return;
			}
			const liveThumb = thumbStateRef.current;
			const trackTravel = Math.max(1, liveThumb.trackSize - liveThumb.thumbSize);
			const maxScroll = liveThumb.maxScrollPosition;
			if (maxScroll <= 0) {
				return;
			}
			const trackRect = dragState.trackElement.getBoundingClientRect();
			const trackOrigin = orientation === 'vertical' ? trackRect.top : trackRect.left;
			const pointerInTrack = getAxisPointerPosition(event.clientX, event.clientY, orientation) - trackOrigin;
			const desiredThumbOffset = pointerInTrack - dragState.gripOffset;
			const ratio = clamp(desiredThumbOffset / trackTravel, 0, 1);
			setAxisScrollPosition(scrollElement, orientation, ratio * maxScroll);
			setThumbStateIfChanged({
				...liveThumb,
				thumbOffset: ratio * trackTravel,
			});
		},
		[orientation, scrollRef, setThumbStateIfChanged],
	);
	const handleWindowPointerUp = useCallback(() => {
		stopDragging();
	}, [stopDragging]);
	useEffect(() => {
		if (!isDragging) {
			return;
		}
		window.addEventListener('pointermove', handleWindowPointerMove);
		window.addEventListener('pointerup', handleWindowPointerUp);
		window.addEventListener('pointercancel', handleWindowPointerUp);
		return () => {
			window.removeEventListener('pointermove', handleWindowPointerMove);
			window.removeEventListener('pointerup', handleWindowPointerUp);
			window.removeEventListener('pointercancel', handleWindowPointerUp);
		};
	}, [isDragging, handleWindowPointerMove, handleWindowPointerUp]);
	useEffect(() => {
		return () => {
			stopDragging();
		};
	}, [stopDragging]);
	useEffect(() => {
		refreshThumbState();
	}, [refreshThumbState]);
	const onThumbPointerDown = useCallback(
		(event: ReactPointerEvent<HTMLDivElement>) => {
			const liveThumb = thumbStateRef.current;
			if (event.button !== 0 || liveThumb.maxScrollPosition <= 0) {
				return;
			}
			const scrollElement = scrollRef.current;
			if (!scrollElement) {
				return;
			}
			const thumbElement = event.currentTarget;
			const trackElement = thumbElement.parentElement;
			if (!trackElement) {
				return;
			}
			const trackRect = trackElement.getBoundingClientRect();
			const pointerInTrack =
				getAxisPointerPosition(event.clientX, event.clientY, orientation) -
				(orientation === 'vertical' ? trackRect.top : trackRect.left);
			const gripOffset = pointerInTrack - liveThumb.thumbOffset;
			thumbElement.setPointerCapture(event.pointerId);
			dragStateRef.current = {
				pointerId: event.pointerId,
				thumbElement,
				trackElement,
				gripOffset,
			};
			previousUserSelectRef.current = document.body.style.userSelect;
			document.body.style.userSelect = 'none';
			previousCursorRef.current = document.body.style.cursor;
			document.body.style.cursor = 'grabbing';
			setIsDragging(true);
			event.preventDefault();
			event.stopPropagation();
		},
		[orientation, scrollRef],
	);
	const onTrackPointerDown = useCallback(
		(event: ReactPointerEvent<HTMLDivElement>) => {
			const liveThumb = thumbStateRef.current;
			if (event.button !== 0 || liveThumb.maxScrollPosition <= 0) {
				return;
			}
			const target = event.target as HTMLElement;
			if (target.dataset.scrollerThumb === 'true') {
				return;
			}
			const scrollElement = scrollRef.current;
			if (!scrollElement) {
				return;
			}
			const rect = event.currentTarget.getBoundingClientRect();
			const pointerPosition = orientation === 'vertical' ? event.clientY - rect.top : event.clientX - rect.left;
			const trackTravel = Math.max(1, liveThumb.trackSize - liveThumb.thumbSize);
			const thumbOffset = pointerPosition - liveThumb.thumbSize / 2;
			const nextRatio = clamp(thumbOffset / trackTravel, 0, 1);
			const nextScrollPosition = nextRatio * liveThumb.maxScrollPosition;
			setAxisScrollPosition(scrollElement, orientation, nextScrollPosition);
			setThumbStateIfChanged({
				...liveThumb,
				thumbOffset: nextRatio * trackTravel,
			});
			event.preventDefault();
		},
		[orientation, scrollRef, setThumbStateIfChanged],
	);
	return {
		isDragging,
		refreshThumbState,
		thumbState,
		setTrackElement,
		setThumbElement,
		onThumbPointerDown,
		onTrackPointerDown,
	};
}
