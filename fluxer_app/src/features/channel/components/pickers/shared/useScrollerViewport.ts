// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ScrollerHandle} from '@app/features/ui/components/Scroller';
import {useCallback, useEffect, useLayoutEffect, useRef, useState} from 'react';

type ResizeType = 'container' | 'content';

function readContentBoxSize(element: HTMLElement): {width: number; height: number} | null {
	const view = element.ownerDocument.defaultView;
	if (view === null) return null;
	const style = view.getComputedStyle(element);
	const width = element.clientWidth - Number.parseFloat(style.paddingLeft) - Number.parseFloat(style.paddingRight);
	const height = element.clientHeight - Number.parseFloat(style.paddingTop) - Number.parseFloat(style.paddingBottom);
	if (!(width > 0) || !(height > 0)) return null;
	return {width, height};
}

export function useScrollerViewport(scrollerRef: React.RefObject<ScrollerHandle | null>) {
	const [viewportSize, setViewportSize] = useState({width: 0, height: 0});
	const [scrollTop, setScrollTop] = useState(0);
	const pendingScrollTopRef = useRef<number | null>(null);
	const scrollFrameRef = useRef<number | null>(null);
	const pendingViewportSizeRef = useRef<{width: number; height: number} | null>(null);
	const viewportFrameRef = useRef<number | null>(null);
	const hasMeasuredViewportRef = useRef(false);
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
	const commitViewportSize = useCallback((nextViewportSize: {width: number; height: number}) => {
		pendingViewportSizeRef.current = null;
		if (viewportFrameRef.current != null) {
			cancelAnimationFrame(viewportFrameRef.current);
			viewportFrameRef.current = null;
		}
		hasMeasuredViewportRef.current = true;
		setViewportSize((prev) => {
			if (prev.width === nextViewportSize.width && prev.height === nextViewportSize.height) return prev;
			return nextViewportSize;
		});
	}, []);
	const flushViewportSize = useCallback(() => {
		viewportFrameRef.current = null;
		const nextViewportSize = pendingViewportSizeRef.current;
		pendingViewportSizeRef.current = null;
		if (!nextViewportSize) return;
		commitViewportSize(nextViewportSize);
	}, [commitViewportSize]);
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
			if (!hasMeasuredViewportRef.current && width > 0 && height > 0) {
				commitViewportSize({width, height});
				return;
			}
			scheduleViewportSize({width, height});
		},
		[commitViewportSize, scheduleViewportSize],
	);
	useLayoutEffect(() => {
		if (hasMeasuredViewportRef.current) return;
		const element = scrollerRef.current?.getViewportElement() ?? null;
		if (element === null) return;
		const contentBox = readContentBoxSize(element);
		if (contentBox === null) return;
		commitViewportSize(contentBox);
	}, [scrollerRef, commitViewportSize]);
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
	const jumpToStartEdge = useCallback(() => {
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
		jumpToStartEdge,
	};
}
