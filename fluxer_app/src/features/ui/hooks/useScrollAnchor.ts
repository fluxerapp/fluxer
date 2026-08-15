// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ScrollerHandle} from '@app/features/ui/components/Scroller';
import {type RefCallback, type RefObject, useCallback, useEffect, useLayoutEffect, useMemo, useRef} from 'react';

const LIST_SCROLL_TOP_EPSILON_PX = 0.5;
const LIST_SCROLL_BOTTOM_TOLERANCE_PX = 2;

interface ScrollAnchorOptions {
	containerSelector?: string;
	durationMs?: number;
}

interface ActiveAnchor {
	container: HTMLElement;
	targetTop: number;
	deadline: number;
	cleanupUserScrollListeners: () => void;
}

interface ListScrollAnchorPosition {
	readonly key: string;
	readonly top: number;
	readonly bottom: number;
}

interface ListScrollAnchorSnapshot {
	readonly anchors: ReadonlyArray<ListScrollAnchorPosition>;
	readonly clientHeight: number;
	readonly scrollHeight: number;
}

interface OwnedAnimationFrame {
	readonly id: number;
	readonly ownerWindow: Window;
}

interface UseListScrollAnchorOptions {
	readonly scrollerRef: RefObject<ScrollerHandle | null>;
}

export interface ListScrollAnchorController {
	getAnchorRef: (key: string) => RefCallback<HTMLElement>;
	handleResize: () => void;
	handleScroll: (scrollNode: HTMLElement) => void;
	stabilize: () => void;
}

function canScrollOnOverflowAxis(overflow: string): boolean {
	if (overflow === 'auto') return true;
	if (overflow === 'scroll') return true;
	return overflow === 'overlay';
}

function findScrollContainer(el: HTMLElement, selector: string | undefined): HTMLElement | null {
	if (selector != null) {
		const found = el.closest(selector);
		if (found instanceof HTMLElement) return found;
		return null;
	}
	let current: HTMLElement | null = el.parentElement;
	while (current != null && current !== document.body) {
		const style = window.getComputedStyle(current);
		if (canScrollOnOverflowAxis(style.overflowY) && current.scrollHeight > current.clientHeight) return current;
		current = current.parentElement;
	}
	return null;
}

function resolveScrollerNode(scrollerRef: RefObject<ScrollerHandle | null>): HTMLElement | null {
	const scroller = scrollerRef.current;
	if (scroller == null) return null;
	return scroller.getScrollerNode();
}

function compareListScrollAnchorPositions(left: ListScrollAnchorPosition, right: ListScrollAnchorPosition): number {
	const topComparison = left.top - right.top;
	if (topComparison !== 0 && !Number.isNaN(topComparison)) return topComparison;
	return left.key.localeCompare(right.key);
}

function measureListScrollAnchors(
	scrollNode: HTMLElement,
	anchorNodes: ReadonlyMap<string, HTMLElement>,
): ListScrollAnchorSnapshot {
	const scrollRect = scrollNode.getBoundingClientRect();
	const scrollTop = scrollNode.scrollTop;
	const anchors: Array<ListScrollAnchorPosition> = [];
	for (const [key, element] of anchorNodes) {
		if (!element.isConnected || !scrollNode.contains(element)) continue;
		const rect = element.getBoundingClientRect();
		if (rect.height <= LIST_SCROLL_TOP_EPSILON_PX) continue;
		anchors.push({
			key,
			top: scrollTop + rect.top - scrollRect.top,
			bottom: scrollTop + rect.bottom - scrollRect.top,
		});
	}
	anchors.sort(compareListScrollAnchorPositions);
	return {
		anchors,
		clientHeight: scrollNode.clientHeight,
		scrollHeight: scrollNode.scrollHeight,
	};
}

function getListScrollAnchorDistance(anchor: ListScrollAnchorPosition, viewportTop: number): number {
	if (anchor.top <= viewportTop && anchor.bottom > viewportTop) return viewportTop - anchor.top;
	if (anchor.top > viewportTop) return anchor.top - viewportTop;
	return viewportTop - anchor.bottom;
}

function getListScrollAnchorCandidates(
	anchors: ReadonlyArray<ListScrollAnchorPosition>,
	viewportTop: number,
	viewportHeight: number,
): Array<ListScrollAnchorPosition> {
	const viewportBottom = viewportTop + viewportHeight;
	const visibleAnchors = anchors.filter((anchor) => anchor.bottom > viewportTop && anchor.top < viewportBottom);
	const candidates = visibleAnchors.length > 0 ? visibleAnchors : [...anchors];
	candidates.sort(
		(left, right) => getListScrollAnchorDistance(left, viewportTop) - getListScrollAnchorDistance(right, viewportTop),
	);
	return candidates;
}

function indexListScrollAnchors(
	anchors: ReadonlyArray<ListScrollAnchorPosition>,
): Map<string, ListScrollAnchorPosition> {
	const anchorsByKey = new Map<string, ListScrollAnchorPosition>();
	for (const anchor of anchors) anchorsByKey.set(anchor.key, anchor);
	return anchorsByKey;
}

function clampListScrollTop(scrollNode: HTMLElement, scrollTop: number): number {
	const maxScrollTop = Math.max(0, scrollNode.scrollHeight - scrollNode.clientHeight);
	return Math.max(0, Math.min(scrollTop, maxScrollTop));
}

export function useScrollAnchor<T extends HTMLElement = HTMLElement>(options: ScrollAnchorOptions = {}) {
	const {containerSelector, durationMs = 400} = options;
	const anchorRef = useRef<T | null>(null);
	const rafRef = useRef<number | null>(null);
	const stateRef = useRef<ActiveAnchor | null>(null);
	const cancel = useCallback(() => {
		if (rafRef.current != null) {
			cancelAnimationFrame(rafRef.current);
			rafRef.current = null;
		}
		const cancelledAnchor = stateRef.current;
		if (cancelledAnchor != null) cancelledAnchor.cleanupUserScrollListeners();
		stateRef.current = null;
	}, []);
	const anchor = useCallback(() => {
		const el = anchorRef.current;
		if (el == null) return;
		const container = findScrollContainer(el, containerSelector);
		if (container == null) return;
		const anchorRect = el.getBoundingClientRect();
		const containerRect = container.getBoundingClientRect();
		const targetTop = anchorRect.top - containerRect.top;
		if (rafRef.current != null) {
			cancelAnimationFrame(rafRef.current);
			rafRef.current = null;
		}
		const supersededAnchor = stateRef.current;
		if (supersededAnchor != null) supersededAnchor.cleanupUserScrollListeners();
		const cancelForUserScroll = () => cancel();
		const passiveOptions: AddEventListenerOptions = {passive: true};
		const keyOptions: AddEventListenerOptions = {capture: true};
		container.addEventListener('wheel', cancelForUserScroll, passiveOptions);
		container.addEventListener('touchstart', cancelForUserScroll, passiveOptions);
		container.addEventListener('touchmove', cancelForUserScroll, passiveOptions);
		container.addEventListener('pointerdown', cancelForUserScroll, passiveOptions);
		window.addEventListener('keydown', cancelForUserScroll, keyOptions);
		const cleanupUserScrollListeners = () => {
			container.removeEventListener('wheel', cancelForUserScroll, passiveOptions);
			container.removeEventListener('touchstart', cancelForUserScroll, passiveOptions);
			container.removeEventListener('touchmove', cancelForUserScroll, passiveOptions);
			container.removeEventListener('pointerdown', cancelForUserScroll, passiveOptions);
			window.removeEventListener('keydown', cancelForUserScroll, keyOptions);
		};
		stateRef.current = {
			container,
			targetTop,
			deadline: performance.now() + durationMs,
			cleanupUserScrollListeners,
		};
		const tick = () => {
			const state = stateRef.current;
			const node = anchorRef.current;
			if (state == null || node == null) {
				rafRef.current = null;
				stateRef.current = null;
				return;
			}
			const anchorRect = node.getBoundingClientRect();
			const containerRect = state.container.getBoundingClientRect();
			const currentTop = anchorRect.top - containerRect.top;
			const delta = currentTop - state.targetTop;
			if (Math.abs(delta) > LIST_SCROLL_TOP_EPSILON_PX) state.container.scrollTop += delta;
			if (performance.now() < state.deadline) {
				rafRef.current = requestAnimationFrame(tick);
				return;
			}
			state.cleanupUserScrollListeners();
			rafRef.current = null;
			stateRef.current = null;
		};
		rafRef.current = requestAnimationFrame(tick);
	}, [cancel, containerSelector, durationMs]);
	useEffect(() => cancel, [cancel]);
	return {anchorRef, anchor};
}

export function useListScrollAnchor({scrollerRef}: UseListScrollAnchorOptions): ListScrollAnchorController {
	const anchorNodesRef = useRef<Map<string, HTMLElement>>(new Map());
	const anchorCallbacksRef = useRef<Map<string, RefCallback<HTMLElement>>>(new Map());
	const snapshotRef = useRef<ListScrollAnchorSnapshot | null>(null);
	const latestScrollTopRef = useRef(0);
	const resizeFrameRef = useRef<OwnedAnimationFrame | null>(null);

	const getAnchorRef = useCallback((key: string): RefCallback<HTMLElement> => {
		const existingCallback = anchorCallbacksRef.current.get(key);
		if (existingCallback != null) return existingCallback;
		const callback: RefCallback<HTMLElement> = (element) => {
			if (element == null) {
				anchorNodesRef.current.delete(key);
				anchorCallbacksRef.current.delete(key);
				return;
			}
			anchorNodesRef.current.set(key, element);
		};
		anchorCallbacksRef.current.set(key, callback);
		return callback;
	}, []);

	const stabilize = useCallback(() => {
		const scrollNode = resolveScrollerNode(scrollerRef);
		if (scrollNode == null) return;
		const currentSnapshot = measureListScrollAnchors(scrollNode, anchorNodesRef.current);
		const previousSnapshot = snapshotRef.current;
		if (previousSnapshot == null) {
			snapshotRef.current = currentSnapshot;
			latestScrollTopRef.current = scrollNode.scrollTop;
			return;
		}

		const previousScrollTop = latestScrollTopRef.current;
		let targetScrollTop = previousScrollTop;
		const previousMaxScrollTop = Math.max(0, previousSnapshot.scrollHeight - previousSnapshot.clientHeight);
		const previousDistanceFromBottom = previousMaxScrollTop - previousScrollTop;
		if (previousScrollTop > LIST_SCROLL_TOP_EPSILON_PX) {
			if (previousDistanceFromBottom <= LIST_SCROLL_BOTTOM_TOLERANCE_PX) {
				targetScrollTop = Math.max(0, currentSnapshot.scrollHeight - currentSnapshot.clientHeight);
			} else {
				const currentAnchorsByKey = indexListScrollAnchors(currentSnapshot.anchors);
				const candidates = getListScrollAnchorCandidates(
					previousSnapshot.anchors,
					previousScrollTop,
					previousSnapshot.clientHeight,
				);
				for (const previousAnchor of candidates) {
					const currentAnchor = currentAnchorsByKey.get(previousAnchor.key);
					if (currentAnchor == null) continue;
					targetScrollTop = previousScrollTop + currentAnchor.top - previousAnchor.top;
					break;
				}
			}
		}

		targetScrollTop = clampListScrollTop(scrollNode, targetScrollTop);
		if (Math.abs(scrollNode.scrollTop - targetScrollTop) > LIST_SCROLL_TOP_EPSILON_PX) {
			scrollNode.scrollTop = targetScrollTop;
		}
		latestScrollTopRef.current = targetScrollTop;
		snapshotRef.current = currentSnapshot;
	}, [scrollerRef]);

	const handleScroll = useCallback((scrollNode: HTMLElement) => {
		latestScrollTopRef.current = scrollNode.scrollTop;
	}, []);

	const handleResize = useCallback(() => {
		const scrollNode = resolveScrollerNode(scrollerRef);
		if (scrollNode == null) return;
		const ownerWindow = scrollNode.ownerDocument.defaultView;
		if (ownerWindow == null) return;
		const pendingFrame = resizeFrameRef.current;
		if (pendingFrame != null && pendingFrame.ownerWindow === ownerWindow) return;
		if (pendingFrame != null) pendingFrame.ownerWindow.cancelAnimationFrame(pendingFrame.id);
		const frame: OwnedAnimationFrame = {
			id: ownerWindow.requestAnimationFrame(() => {
				if (resizeFrameRef.current !== frame) return;
				resizeFrameRef.current = null;
				stabilize();
			}),
			ownerWindow,
		};
		resizeFrameRef.current = frame;
	}, [scrollerRef, stabilize]);

	useLayoutEffect(() => {
		stabilize();
	}, [stabilize]);

	useEffect(() => {
		return () => {
			const pendingFrame = resizeFrameRef.current;
			if (pendingFrame != null) pendingFrame.ownerWindow.cancelAnimationFrame(pendingFrame.id);
			resizeFrameRef.current = null;
			anchorNodesRef.current.clear();
			anchorCallbacksRef.current.clear();
		};
	}, []);

	return useMemo(
		() => ({getAnchorRef, handleResize, handleScroll, stabilize}),
		[getAnchorRef, handleResize, handleScroll, stabilize],
	);
}
