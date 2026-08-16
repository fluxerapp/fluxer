// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {
	FloatingUnreadEdgeLayout,
	type FloatingUnreadEdges,
	type FloatingUnreadTarget,
	type FloatingUnreadTargetBounds,
	type MeasuredFloatingUnreadTarget,
} from '@app/features/app/components/layout/FloatingUnreadEdges';
import styles from '@app/features/app/components/layout/FloatingUnreadIndicators.module.css';
import type {ScrollIndicatorSeverity} from '@app/features/app/components/layout/ScrollIndicatorStateMachine';
import {Edge, type VerticalEdge} from '@app/features/ui/AxisOrientation';
import type {ScrollerHandle} from '@app/features/ui/components/Scroller';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {AnimeButton, AnimePresence} from '@app/features/ui/motion/AnimeElement';
import {resolveVisibleScrollViewportHeight} from '@app/features/ui/utils/ScrollViewportGeometry';
import {flxElementClassName} from '@app/lib/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useLayoutEffect, useMemo, useRef, useState} from 'react';

const SCROLL_TARGET_PADDING_PX = 12;
const SETTLE_REFRESH_DELAY_MS = 280;

const FloatingUnreadRefreshScope = Object.freeze({
	GEOMETRY: 'geometry',
	SCROLL: 'scroll',
	TARGETS: 'targets',
} as const);

type FloatingUnreadRefreshScope = (typeof FloatingUnreadRefreshScope)[keyof typeof FloatingUnreadRefreshScope];

function buildFloatingUnreadTargetsSignature(targets: ReadonlyArray<FloatingUnreadTarget>): string {
	if (targets.length === 0) return '';
	const parts = new Array<string>(targets.length);
	for (let index = 0; index < targets.length; index += 1) {
		const target = targets[index];
		parts[index] = `${target.id}:${target.severity}`;
	}
	return parts.join('|');
}

export interface FloatingUnreadTargetRegistry {
	register: (id: string) => React.RefCallback<HTMLElement>;
	getTargetNode: (id: string) => HTMLElement | null;
}

export function useFloatingUnreadTargetRegistry(): FloatingUnreadTargetRegistry {
	const nodesRef = useRef<Map<string, HTMLElement>>(new Map());
	const callbacksRef = useRef<Map<string, React.RefCallback<HTMLElement>>>(new Map());
	const register = useCallback((id: string): React.RefCallback<HTMLElement> => {
		const cached = callbacksRef.current.get(id);
		if (cached != null) return cached;
		const callback = (node: HTMLElement | null) => {
			const registeredNode = nodesRef.current.get(id);
			if (registeredNode === node) return;
			if (registeredNode == null && node == null) return;
			if (node != null) {
				nodesRef.current.set(id, node);
				callbacksRef.current.set(id, callback);
			} else {
				nodesRef.current.delete(id);
				callbacksRef.current.delete(id);
			}
		};
		callbacksRef.current.set(id, callback);
		return callback;
	}, []);
	const getTargetNode = useCallback((id: string): HTMLElement | null => {
		const node = nodesRef.current.get(id);
		if (node == null) {
			return null;
		}
		return node;
	}, []);
	return useMemo(() => ({register, getTargetNode}), [register, getTargetNode]);
}

interface FloatingUnreadPillProps {
	readonly label: string;
	readonly severity: ScrollIndicatorSeverity;
	readonly direction: VerticalEdge;
	readonly onClick: () => void;
}

interface VisibleFloatingUnreadEdge {
	readonly direction: VerticalEdge;
	readonly edge: MeasuredFloatingUnreadTarget;
}

const FloatingUnreadPill = ({label, severity, direction, onClick}: FloatingUnreadPillProps) => {
	const prefersReducedMotion = Accessibility.useReducedMotion;
	let directionOffset: 10 | -10;
	if (direction === Edge.TOP) {
		directionOffset = -10;
	} else {
		directionOffset = 10;
	}
	let severityClassName = styles.indicatorBrand;
	if (severity === 'mention') {
		severityClassName = styles.indicatorMention;
	}
	if (prefersReducedMotion) {
		return (
			<FocusRing offset={-2} data-flx="app.floating-unread-indicators.floating-unread-pill.focus-ring">
				<AnimeButton
					type="button"
					className={clsx(styles.indicator, severityClassName)}
					onClick={onClick}
					from={{opacity: 1, translateY: 0, scale: 1}}
					to={{opacity: 1, translateY: 0, scale: 1}}
					leave={{opacity: 1, translateY: 0, scale: 1, tween: {duration: 0}}}
					tween={{duration: 0, ease: 'out(3)'}}
					aria-label={label}
					data-flx="app.floating-unread-indicators.floating-unread-pill.indicator.click.button"
				>
					{label}
				</AnimeButton>
			</FocusRing>
		);
	}
	return (
		<FocusRing offset={-2} data-flx="app.floating-unread-indicators.floating-unread-pill.focus-ring--2">
			<AnimeButton
				type="button"
				className={clsx(styles.indicator, severityClassName)}
				onClick={onClick}
				from={{opacity: 0, translateY: directionOffset, scale: 0.96}}
				to={{opacity: 1, translateY: 0, scale: 1}}
				leave={{
					opacity: 0,
					translateY: directionOffset,
					scale: 0.96,
					tween: {duration: 0.16, ease: 'inOut(2)'},
				}}
				tween={{duration: 0.18, ease: 'out(3)'}}
				hover={{scale: 1.05}}
				press={{translateY: 1}}
				aria-label={label}
				data-flx="app.floating-unread-indicators.floating-unread-pill.indicator.click.button--2"
			>
				{label}
			</AnimeButton>
		</FocusRing>
	);
};

export interface FloatingUnreadIndicatorsProps {
	readonly label: string;
	readonly scrollerRef: React.RefObject<ScrollerHandle | null>;
	readonly scrollerIdentity: string;
	readonly targets: ReadonlyArray<FloatingUnreadTarget>;
	readonly getTargetNode: (id: string) => HTMLElement | null;
	readonly measurementRevision: number;
	readonly getTargetBounds: (
		targets: ReadonlyArray<FloatingUnreadTarget>,
	) => ReadonlyMap<string, FloatingUnreadTargetBounds>;
	readonly scrollToTarget: (id: string, direction: VerticalEdge) => boolean;
}

function resolveScrollerNode(scroller: ScrollerHandle | null): HTMLElement | null {
	if (scroller == null) {
		return null;
	}
	return scroller.getScrollerNode();
}

function resolveOwnerWindow(container: HTMLElement): Window & typeof globalThis {
	const defaultView = container.ownerDocument.defaultView;
	if (defaultView == null) {
		throw new Error('Floating unread indicator container has no owner window');
	}
	return defaultView;
}

function observeScrollContentElements(
	resizeObserver: ResizeObserver,
	container: HTMLElement,
	ownerWindow: Window & typeof globalThis,
): void {
	const contentWrapper = container.firstElementChild;
	if (!(contentWrapper instanceof ownerWindow.HTMLElement)) return;
	const contentChildren = contentWrapper.children;
	if (contentChildren.length === 0) {
		resizeObserver.observe(contentWrapper);
		return;
	}
	for (let index = 0; index < contentChildren.length; index += 1) {
		const child = contentChildren[index];
		if (child instanceof ownerWindow.HTMLElement) resizeObserver.observe(child);
	}
}

interface OwnedAnimationFrame {
	readonly id: number;
	readonly ownerWindow: Window & typeof globalThis;
}

interface OwnedSettleTimer {
	readonly id: number;
	readonly ownerWindow: Window & typeof globalThis;
}

interface AlignMountedTargetRequest {
	readonly animate: boolean;
	readonly direction: VerticalEdge;
	readonly scroller: ScrollerHandle;
	readonly targetNode: HTMLElement;
}

function cancelOwnedAnimationFrame(frame: OwnedAnimationFrame | null): void {
	if (frame == null) return;
	frame.ownerWindow.cancelAnimationFrame(frame.id);
}

function cancelOwnedSettleTimer(timer: OwnedSettleTimer | null): void {
	if (timer == null) return;
	timer.ownerWindow.clearTimeout(timer.id);
}

function alignMountedTarget({animate, direction, scroller, targetNode}: AlignMountedTargetRequest): boolean {
	const container = scroller.getScrollerNode();
	if (container == null) return false;
	const containerRect = container.getBoundingClientRect();
	const targetRect = targetNode.getBoundingClientRect();
	const targetTop = container.scrollTop + targetRect.top - containerRect.top;
	const targetBottom = container.scrollTop + targetRect.bottom - containerRect.top;
	let to = targetBottom + SCROLL_TARGET_PADDING_PX - resolveVisibleScrollViewportHeight(container);
	if (direction === Edge.TOP) {
		to = targetTop - SCROLL_TARGET_PADDING_PX;
	}
	scroller.scrollTo({to, animate});
	return true;
}

export const FloatingUnreadIndicators = observer(
	({
		label,
		scrollerRef,
		scrollerIdentity,
		targets,
		getTargetNode,
		measurementRevision,
		getTargetBounds,
		scrollToTarget,
	}: FloatingUnreadIndicatorsProps) => {
		const [edges, setEdges] = useState<FloatingUnreadEdges>(FloatingUnreadEdgeLayout.EMPTY_EDGES);
		const targetsSignature = useMemo(() => buildFloatingUnreadTargetsSignature(targets), [targets]);
		const targetsRef = useRef(targets);
		const getTargetNodeRef = useRef(getTargetNode);
		const getTargetBoundsRef = useRef(getTargetBounds);
		const scrollToTargetRef = useRef(scrollToTarget);
		targetsRef.current = targets;
		getTargetNodeRef.current = getTargetNode;
		getTargetBoundsRef.current = getTargetBounds;
		scrollToTargetRef.current = scrollToTarget;
		const rafRef = useRef<OwnedAnimationFrame | null>(null);
		const targetAlignmentFrameRef = useRef<OwnedAnimationFrame | null>(null);
		const settleTimerRef = useRef<OwnedSettleTimer | null>(null);
		const geometrySettleTimerRef = useRef<OwnedSettleTimer | null>(null);
		const geometrySettleArmedRef = useRef(false);
		const geometrySettleSuppressedRef = useRef(false);
		const refreshNowRef = useRef<() => void>(() => {});
		const geometryInvalidatedRef = useRef(true);
		const targetIndexInvalidatedRef = useRef(true);
		const visibleViewportHeightRef = useRef(0);
		const targetBoundsCacheRef = useRef<Map<string, FloatingUnreadTargetBounds>>(new Map());
		const pendingScrollTopRef = useRef<number | null>(null);
		const targetIndexRef = useRef(FloatingUnreadEdgeLayout.EMPTY_TARGET_INDEX);
		const refreshTargetIndex = useCallback(() => {
			const currentTargets = targetsRef.current;
			const boundsCache = targetBoundsCacheRef.current;
			const unmeasuredTargets: Array<FloatingUnreadTarget> = [];
			for (const target of currentTargets) {
				if (boundsCache.has(target.id)) continue;
				unmeasuredTargets.push(target);
			}
			if (unmeasuredTargets.length > 0) {
				const measuredBounds = getTargetBoundsRef.current(unmeasuredTargets);
				for (const [id, bounds] of measuredBounds) {
					boundsCache.set(id, bounds);
				}
			}
			if (boundsCache.size > currentTargets.length) {
				const retainedBounds = new Map<string, FloatingUnreadTargetBounds>();
				for (const target of currentTargets) {
					const bounds = boundsCache.get(target.id);
					if (bounds != null) retainedBounds.set(target.id, bounds);
				}
				targetBoundsCacheRef.current = retainedBounds;
			}
			targetIndexRef.current = FloatingUnreadEdgeLayout.createTargetIndexFromBounds({
				targets: currentTargets,
				boundsByTargetId: targetBoundsCacheRef.current,
			});
		}, []);
		const refreshNow = useCallback(() => {
			const container = resolveScrollerNode(scrollerRef.current);
			if (container == null) {
				pendingScrollTopRef.current = null;
				targetIndexRef.current = FloatingUnreadEdgeLayout.EMPTY_TARGET_INDEX;
				setEdges(FloatingUnreadEdgeLayout.EMPTY_EDGES);
				return;
			}
			if (targetsRef.current.length === 0) {
				pendingScrollTopRef.current = null;
				targetBoundsCacheRef.current.clear();
				targetIndexRef.current = FloatingUnreadEdgeLayout.EMPTY_TARGET_INDEX;
				setEdges(FloatingUnreadEdgeLayout.EMPTY_EDGES);
				return;
			}
			if (geometryInvalidatedRef.current) {
				geometryInvalidatedRef.current = false;
				targetIndexInvalidatedRef.current = true;
				targetBoundsCacheRef.current.clear();
				visibleViewportHeightRef.current = resolveVisibleScrollViewportHeight(container);
				if (geometrySettleSuppressedRef.current) {
					geometrySettleSuppressedRef.current = false;
					geometrySettleArmedRef.current = false;
				} else if (!geometrySettleArmedRef.current) {
					geometrySettleArmedRef.current = true;
					const settleOwnerWindow = resolveOwnerWindow(container);
					const settleId = settleOwnerWindow.setTimeout(() => {
						geometrySettleTimerRef.current = null;
						geometrySettleArmedRef.current = false;
						geometrySettleSuppressedRef.current = true;
						geometryInvalidatedRef.current = true;
						targetIndexInvalidatedRef.current = true;
						refreshNowRef.current();
					}, SETTLE_REFRESH_DELAY_MS);
					geometrySettleTimerRef.current = {id: settleId, ownerWindow: settleOwnerWindow};
				}
			}
			if (targetIndexInvalidatedRef.current) {
				targetIndexInvalidatedRef.current = false;
				refreshTargetIndex();
			}
			const pendingScrollTop = pendingScrollTopRef.current;
			pendingScrollTopRef.current = null;
			let scrollTop: number;
			if (pendingScrollTop == null) {
				scrollTop = container.scrollTop;
			} else {
				scrollTop = pendingScrollTop;
			}
			const next = FloatingUnreadEdgeLayout.selectEdges({
				scrollTop,
				viewportHeight: visibleViewportHeightRef.current,
				targetIndex: targetIndexRef.current,
			});
			setEdges((current) => FloatingUnreadEdgeLayout.preserveStableEdges({current, next}));
		}, [refreshTargetIndex, scrollerRef]);
		refreshNowRef.current = refreshNow;
		const scheduleRefresh = useCallback(
			(scope: FloatingUnreadRefreshScope) => {
				if (scope === FloatingUnreadRefreshScope.GEOMETRY) {
					geometryInvalidatedRef.current = true;
					targetIndexInvalidatedRef.current = true;
					geometrySettleSuppressedRef.current = false;
					geometrySettleArmedRef.current = false;
					cancelOwnedSettleTimer(geometrySettleTimerRef.current);
					geometrySettleTimerRef.current = null;
				} else if (scope === FloatingUnreadRefreshScope.TARGETS) {
					targetIndexInvalidatedRef.current = true;
					if (geometrySettleArmedRef.current) geometryInvalidatedRef.current = true;
				}
				const container = resolveScrollerNode(scrollerRef.current);
				if (container == null) {
					refreshNow();
					return;
				}
				const ownerWindow = resolveOwnerWindow(container);
				const pendingFrame = rafRef.current;
				if (pendingFrame != null && pendingFrame.ownerWindow === ownerWindow) return;
				cancelOwnedAnimationFrame(pendingFrame);
				const id = ownerWindow.requestAnimationFrame(() => {
					rafRef.current = null;
					refreshNow();
				});
				rafRef.current = {id, ownerWindow};
			},
			[refreshNow, scrollerRef],
		);
		useLayoutEffect(() => {
			const container = resolveScrollerNode(scrollerRef.current);
			if (container == null) {
				targetIndexRef.current = FloatingUnreadEdgeLayout.EMPTY_TARGET_INDEX;
				setEdges(FloatingUnreadEdgeLayout.EMPTY_EDGES);
				return;
			}
			geometryInvalidatedRef.current = true;
			targetIndexInvalidatedRef.current = true;
			refreshNow();
			const ownerWindow = resolveOwnerWindow(container);
			const handleScroll = () => {
				pendingScrollTopRef.current = container.scrollTop;
				scheduleRefresh(FloatingUnreadRefreshScope.SCROLL);
			};
			const handleLayoutChange = () => scheduleRefresh(FloatingUnreadRefreshScope.GEOMETRY);
			container.addEventListener('scroll', handleScroll, {passive: true});
			ownerWindow.addEventListener('resize', handleLayoutChange);
			let resizeObserver: ResizeObserver | null;
			if (ownerWindow.ResizeObserver != null) {
				resizeObserver = new ownerWindow.ResizeObserver(handleLayoutChange);
			} else {
				resizeObserver = null;
			}
			if (resizeObserver != null) {
				resizeObserver.observe(container);
				observeScrollContentElements(resizeObserver, container, ownerWindow);
			}
			return () => {
				container.removeEventListener('scroll', handleScroll);
				ownerWindow.removeEventListener('resize', handleLayoutChange);
				if (resizeObserver != null) {
					resizeObserver.disconnect();
				}
				cancelOwnedAnimationFrame(rafRef.current);
				rafRef.current = null;
				cancelOwnedAnimationFrame(targetAlignmentFrameRef.current);
				targetAlignmentFrameRef.current = null;
				cancelOwnedSettleTimer(settleTimerRef.current);
				settleTimerRef.current = null;
				cancelOwnedSettleTimer(geometrySettleTimerRef.current);
				geometrySettleTimerRef.current = null;
				geometrySettleArmedRef.current = false;
				geometrySettleSuppressedRef.current = false;
			};
		}, [refreshNow, scheduleRefresh, scrollerIdentity, scrollerRef]);
		useLayoutEffect(() => {
			scheduleRefresh(FloatingUnreadRefreshScope.GEOMETRY);
		}, [measurementRevision, scheduleRefresh]);
		useLayoutEffect(() => {
			scheduleRefresh(FloatingUnreadRefreshScope.TARGETS);
		}, [scheduleRefresh, targetsSignature]);
		const alignTarget = useCallback(
			(id: string, direction: VerticalEdge, animate: boolean): boolean => {
				const scroller = scrollerRef.current;
				const targetNode = getTargetNodeRef.current(id);
				if (scroller == null || targetNode == null) return false;
				return alignMountedTarget({animate, direction, scroller, targetNode});
			},
			[scrollerRef],
		);
		const scrollToEdge = useCallback(
			(edge: MeasuredFloatingUnreadTarget, direction: VerticalEdge) => {
				const scroller = scrollerRef.current;
				const scrollTarget = scrollToTargetRef.current;
				if (scrollTarget(edge.id, direction)) {
					scheduleRefresh(FloatingUnreadRefreshScope.GEOMETRY);
					if (scroller == null) return;
					const scrollNode = scroller.getScrollerNode();
					if (scrollNode == null) return;
					const ownerWindow = resolveOwnerWindow(scrollNode);
					cancelOwnedAnimationFrame(targetAlignmentFrameRef.current);
					const id = ownerWindow.requestAnimationFrame(() => {
						targetAlignmentFrameRef.current = null;
						alignTarget(edge.id, direction, Accessibility.useSmoothScrolling);
						scheduleRefresh(FloatingUnreadRefreshScope.GEOMETRY);
					});
					targetAlignmentFrameRef.current = {id, ownerWindow};
				} else if (!alignTarget(edge.id, direction, Accessibility.useSmoothScrolling)) {
					scheduleRefresh(FloatingUnreadRefreshScope.GEOMETRY);
					return;
				}
				scheduleRefresh(FloatingUnreadRefreshScope.SCROLL);
				if (scroller == null) return;
				const scrollNode = scroller.getScrollerNode();
				if (scrollNode == null) return;
				const ownerWindow = resolveOwnerWindow(scrollNode);
				cancelOwnedSettleTimer(settleTimerRef.current);
				const id = ownerWindow.setTimeout(() => {
					settleTimerRef.current = null;
					alignTarget(edge.id, direction, false);
					scheduleRefresh(FloatingUnreadRefreshScope.GEOMETRY);
				}, SETTLE_REFRESH_DELAY_MS);
				settleTimerRef.current = {id, ownerWindow};
			},
			[alignTarget, scheduleRefresh, scrollerRef],
		);
		const visibleEdges: Array<VisibleFloatingUnreadEdge> = [];
		if (edges.top != null) visibleEdges.push({direction: Edge.TOP, edge: edges.top});
		if (edges.bottom != null) visibleEdges.push({direction: Edge.BOTTOM, edge: edges.bottom});
		function renderVisibleEdge({direction, edge}: VisibleFloatingUnreadEdge): React.ReactNode {
			let slotPositionClassName = styles.slotBottom;
			if (direction === Edge.TOP) {
				slotPositionClassName = styles.slotTop;
			}
			return (
				<flx-app-floating-unread-indicators-slot
					key={`${direction}:${edge.id}:${edge.severity}`}
					className={flxElementClassName(styles.slot, slotPositionClassName)}
					data-flx="app.floating-unread-indicators.render-visible-edge.slot"
				>
					<FloatingUnreadPill
						label={label}
						severity={edge.severity}
						direction={direction}
						onClick={() => scrollToEdge(edge, direction)}
						data-flx="app.floating-unread-indicators.render-visible-edge.floating-unread-pill.scroll-to-edge"
					/>
				</flx-app-floating-unread-indicators-slot>
			);
		}
		return (
			<flx-app-floating-unread-indicators-layer
				className={flxElementClassName(styles.layer)}
				data-flx="app.floating-unread-indicators.layer"
			>
				<AnimePresence enterOnMount={false} data-flx="app.floating-unread-indicators.anime-presence">
					{visibleEdges.map(renderVisibleEdge)}
				</AnimePresence>
			</flx-app-floating-unread-indicators-layer>
		);
	},
);
