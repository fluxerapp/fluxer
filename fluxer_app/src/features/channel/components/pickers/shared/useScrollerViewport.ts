// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ScrollerHandle} from '@app/features/ui/components/Scroller';
import {useCallback, useEffect, useRef, useState} from 'react';

type ResizeType = 'container' | 'content';

export function useScrollerViewport(scrollerRef: React.RefObject<ScrollerHandle | null>) {
	const [viewportSize, setViewportSize] = useState({width: 0, height: 0});
	const [scrollTop, setScrollTop] = useState(0);
	const pendingScrollTopRef = useRef<number | null>(null);
	const scrollFrameRef = useRef<number | null>(null);
	const pendingViewportSizeRef = useRef<{width: number; height: number} | null>(null);
	const viewportFrameRef = useRef<number | null>(null);
	const flushScrollTop = useCallback(() => {
		scrollFrameRef.current = null;
		const nextScrollTop = pendingScrollTopRef.current;
		pendingScrollTopRef.current = null;
		if (nextScrollTop === null) return;
		setScrollTop((currentScrollTop) => (currentScrollTop === nextScrollTop ? currentScrollTop : nextScrollTop));
	}, []);
	const scheduleScrollTop = useCallback(
		(nextScrollTop: number) => {
			pendingScrollTopRef.current = nextScrollTop;
			if (scrollFrameRef.current != null) return;
			scrollFrameRef.current = requestAnimationFrame(flushScrollTop);
		},
		[flushScrollTop],
	);
	const flushViewportSize = useCallback(() => {
		viewportFrameRef.current = null;
		const nextViewportSize = pendingViewportSizeRef.current;
		pendingViewportSizeRef.current = null;
		if (!nextViewportSize) return;
		setViewportSize((prev) => {
			if (prev.width === nextViewportSize.width && prev.height === nextViewportSize.height) return prev;
			return nextViewportSize;
		});
	}, []);
	const scheduleViewportSize = useCallback(
		(nextViewportSize: {width: number; height: number}) => {
			pendingViewportSizeRef.current = nextViewportSize;
			if (viewportFrameRef.current != null) return;
			viewportFrameRef.current = requestAnimationFrame(flushViewportSize);
		},
		[flushViewportSize],
	);
	const handleScroll = useCallback(
		(event: React.UIEvent<HTMLDivElement>) => {
			scheduleScrollTop(event.currentTarget.scrollTop);
		},
		[scheduleScrollTop],
	);
	const handleResize = useCallback(
		(entry: ResizeObserverEntry, type: ResizeType) => {
			if (type !== 'container') return;
			const {width, height} = entry.contentRect;
			scheduleViewportSize({width, height});
		},
		[scheduleViewportSize],
	);
	useEffect(() => {
		const state = scrollerRef.current?.readViewportMetrics();
		if (!state || state.offsetWidth === 0 || state.offsetHeight === 0) return;
		scheduleViewportSize({width: state.offsetWidth, height: state.offsetHeight});
	}, [scrollerRef, scheduleViewportSize]);
	useEffect(() => {
		return () => {
			if (scrollFrameRef.current != null) {
				cancelAnimationFrame(scrollFrameRef.current);
			}
			if (viewportFrameRef.current != null) {
				cancelAnimationFrame(viewportFrameRef.current);
			}
		};
	}, []);
	const scrollToTop = useCallback(() => {
		scrollerRef.current?.scrollTo({to: 0, animate: false});
		pendingScrollTopRef.current = null;
		if (scrollFrameRef.current != null) {
			cancelAnimationFrame(scrollFrameRef.current);
			scrollFrameRef.current = null;
		}
		setScrollTop(0);
	}, [scrollerRef]);
	return {
		viewportSize,
		scrollTop,
		setScrollTop,
		handleScroll,
		handleResize,
		scrollToTop,
	};
}
