// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import styles from '@app/features/app/components/layout/ScrollIndicatorOverlay.module.css';
import {
	type ActiveScrollIndicator,
	createScrollIndicatorSnapshot,
	type ScrollIndicatorMachineEvent,
	type ScrollIndicatorSeverity,
	type ScrollIndicatorTargetMeasurement,
	selectActiveScrollIndicators,
	transitionScrollIndicatorSnapshot,
} from '@app/features/app/components/layout/ScrollIndicatorStateMachine';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {resolveVisibleScrollViewportHeight} from '@app/features/ui/utils/ScrollViewportGeometry';
import {clsx} from 'clsx';
import {AnimatePresence, motion} from 'framer-motion';
import type React from 'react';
import {useCallback, useEffect, useLayoutEffect, useRef, useState} from 'react';

export type {ScrollIndicatorSeverity};

const SCROLL_TARGET_PADDING_PX = 12;

interface OwnedAnimationFrame {
	id: number;
	ownerWindow: Window & typeof globalThis;
}

function resolveOwnerWindow(container: HTMLElement): Window & typeof globalThis {
	const ownerWindow = container.ownerDocument.defaultView;
	if (ownerWindow == null) {
		throw new Error('Scroll indicator container has no owner window');
	}
	return ownerWindow;
}

function cancelOwnedAnimationFrame(frame: OwnedAnimationFrame | null): void {
	if (frame == null) return;
	frame.ownerWindow.cancelAnimationFrame(frame.id);
}

function scrollIndicatorNodeIntoVisibleViewport(
	container: HTMLElement,
	node: HTMLElement,
	direction: 'top' | 'bottom',
	behavior: ScrollBehavior,
): void {
	const containerRect = container.getBoundingClientRect();
	const nodeRect = node.getBoundingClientRect();
	const nodeTop = container.scrollTop + nodeRect.top - containerRect.top;
	const nodeBottom = container.scrollTop + nodeRect.bottom - containerRect.top;
	let top = nodeTop - SCROLL_TARGET_PADDING_PX;
	if (direction === 'bottom') {
		top = nodeBottom + SCROLL_TARGET_PADDING_PX - resolveVisibleScrollViewportHeight(container);
	}
	const maximumScrollTop = Math.max(0, container.scrollHeight - container.clientHeight);
	container.scrollTo({top: Math.min(maximumScrollTop, Math.max(0, top)), behavior});
}

interface MeasurementRect {
	top: number;
	bottom: number;
	left: number;
	right: number;
}

function hasPositiveArea(rect: MeasurementRect): boolean {
	return rect.bottom > rect.top && rect.right > rect.left;
}

function getMeasurableRectInsideScrollContent(node: HTMLElement, container: HTMLElement): MeasurementRect | null {
	const nodeStyle = resolveOwnerWindow(node).getComputedStyle(node);
	if (nodeStyle.display === 'none' || nodeStyle.visibility === 'hidden') return null;
	const nodeRect = node.getBoundingClientRect();
	if (!hasPositiveArea(nodeRect)) return null;
	for (let parent = node.parentElement; parent && parent !== container; parent = parent.parentElement) {
		const parentStyle = resolveOwnerWindow(parent).getComputedStyle(parent);
		if (parentStyle.display === 'none' || parentStyle.visibility === 'hidden') return null;
		const parentRect = parent.getBoundingClientRect();
		if (!hasPositiveArea(parentRect)) return null;
	}
	return nodeRect;
}

export function measureScrollIndicatorTargets(container: HTMLElement): Array<ScrollIndicatorTargetMeasurement> {
	const containerRect = container.getBoundingClientRect();
	const nodes = container.querySelectorAll<HTMLElement>(
		'[data-scroll-indicator="mention"],[data-scroll-indicator="unread"]',
	);
	const measurements: Array<ScrollIndicatorTargetMeasurement> = [];
	nodes.forEach((node, order) => {
		const severity = node.dataset.scrollIndicator as ScrollIndicatorSeverity | undefined;
		const id = node.dataset.scrollId;
		if (!severity || !id || !node.isConnected || node.getClientRects().length === 0) return;
		const rect = getMeasurableRectInsideScrollContent(node, container);
		if (!rect) return;
		measurements.push({
			id,
			severity,
			top: container.scrollTop + rect.top - containerRect.top,
			bottom: container.scrollTop + rect.bottom - containerRect.top,
			order,
		});
	});
	return measurements;
}

function findScrollIndicatorNode(container: HTMLElement, id: string): HTMLElement | null {
	const nodes = container.querySelectorAll<HTMLElement>('[data-scroll-id]');
	for (const node of nodes) {
		if (node.dataset.scrollId === id) return node;
	}
	return null;
}

export const useScrollEdgeIndicators = (
	getScrollContainer: () => HTMLElement | null,
	scrollContainerIdentity: string,
	dependencies: React.DependencyList = [],
) => {
	const [snapshot, setSnapshot] = useState(() => createScrollIndicatorSnapshot());
	const activeIndicators = selectActiveScrollIndicators(snapshot);
	const refreshFrameRef = useRef<OwnedAnimationFrame | null>(null);
	const visibleViewportHeightRef = useRef(0);
	const layoutInvalidatedRef = useRef(true);
	const send = useCallback((event: ScrollIndicatorMachineEvent) => {
		setSnapshot((previous) => transitionScrollIndicatorSnapshot(previous, event));
	}, []);
	const refreshNow = useCallback(() => {
		const container = getScrollContainer();
		if (!container) {
			send({type: 'scrollIndicator.reset'});
			return;
		}
		if (layoutInvalidatedRef.current) {
			layoutInvalidatedRef.current = false;
			visibleViewportHeightRef.current = resolveVisibleScrollViewportHeight(container);
		}
		send({
			type: 'scrollIndicator.measured',
			measurement: {
				scrollTop: container.scrollTop,
				viewportHeight: visibleViewportHeightRef.current,
				targets: measureScrollIndicatorTargets(container),
			},
		});
	}, [getScrollContainer, send]);
	const scheduleRefresh = useCallback(
		(invalidateLayout = false) => {
			if (invalidateLayout) layoutInvalidatedRef.current = true;
			const container = getScrollContainer();
			if (container == null) {
				refreshNow();
				return;
			}
			const ownerWindow = resolveOwnerWindow(container);
			const pendingFrame = refreshFrameRef.current;
			if (pendingFrame != null && pendingFrame.ownerWindow === ownerWindow) return;
			cancelOwnedAnimationFrame(pendingFrame);
			const id = ownerWindow.requestAnimationFrame(() => {
				refreshFrameRef.current = null;
				refreshNow();
			});
			refreshFrameRef.current = {id, ownerWindow};
		},
		[getScrollContainer, refreshNow],
	);
	useLayoutEffect(() => {
		layoutInvalidatedRef.current = true;
		refreshNow();
	}, [refreshNow, scrollContainerIdentity, ...dependencies]);
	useLayoutEffect(() => {
		const container = getScrollContainer();
		if (!container) return;
		const ownerWindow = resolveOwnerWindow(container);
		const content = container.firstElementChild instanceof ownerWindow.HTMLElement ? container.firstElementChild : null;
		const resizeObserver =
			ownerWindow.ResizeObserver != null
				? new ownerWindow.ResizeObserver((entries) => {
						scheduleRefresh(entries.some((entry) => entry.target === container));
					})
				: null;
		const contentMutationObserver =
			ownerWindow.MutationObserver != null
				? new ownerWindow.MutationObserver(() => {
						scheduleRefresh(false);
					})
				: null;
		const layoutMutationObserver =
			ownerWindow.MutationObserver != null
				? new ownerWindow.MutationObserver(() => {
						scheduleRefresh(true);
					})
				: null;
		if (resizeObserver != null) {
			resizeObserver.observe(container);
			if (content != null) resizeObserver.observe(content);
		}
		if (contentMutationObserver != null) {
			contentMutationObserver.observe(container, {
				attributes: true,
				childList: true,
				subtree: true,
				attributeFilter: ['data-scroll-indicator', 'data-scroll-id', 'class', 'style'],
			});
		}
		for (let element: HTMLElement | null = container; element != null; element = element.parentElement) {
			if (layoutMutationObserver != null) {
				layoutMutationObserver.observe(element, {attributes: true, attributeFilter: ['class', 'style']});
			}
		}
		return () => {
			if (resizeObserver != null) resizeObserver.disconnect();
			if (contentMutationObserver != null) contentMutationObserver.disconnect();
			if (layoutMutationObserver != null) layoutMutationObserver.disconnect();
			cancelOwnedAnimationFrame(refreshFrameRef.current);
			refreshFrameRef.current = null;
		};
	}, [getScrollContainer, scheduleRefresh, scrollContainerIdentity]);
	useEffect(() => {
		const container = getScrollContainer();
		if (!container) return;
		const handleScroll = () => {
			scheduleRefresh();
		};
		container.addEventListener('scroll', handleScroll, {passive: true});
		return () => {
			container.removeEventListener('scroll', handleScroll);
		};
	}, [getScrollContainer, scheduleRefresh, scrollContainerIdentity]);
	useEffect(() => {
		const container = getScrollContainer();
		if (container == null) return;
		const ownerWindow = resolveOwnerWindow(container);
		const handleResize = () => scheduleRefresh(true);
		ownerWindow.addEventListener('resize', handleResize);
		return () => {
			ownerWindow.removeEventListener('resize', handleResize);
		};
	}, [getScrollContainer, scheduleRefresh, scrollContainerIdentity]);
	return {activeIndicators, refresh: refreshNow, scheduleRefresh};
};

interface FloatingScrollIndicatorProps {
	label: React.ReactNode;
	severity: ScrollIndicatorSeverity;
	onClick: () => void;
}

const FloatingScrollIndicator = ({label, severity, onClick}: FloatingScrollIndicatorProps) => {
	const prefersReducedMotion = Accessibility.useReducedMotion;
	return (
		<FocusRing offset={-2} data-flx="app.scroll-indicator-overlay.floating-scroll-indicator.focus-ring">
			<motion.button
				type="button"
				className={clsx(styles.indicator, severity === 'mention' ? styles.indicatorMention : styles.indicatorBrand)}
				onClick={onClick}
				initial={{opacity: 1, y: 0, scale: 1}}
				animate={{opacity: 1, y: 0, scale: 1}}
				exit={
					prefersReducedMotion
						? {opacity: 1, y: 0, scale: 1, transition: {duration: 0}}
						: {opacity: 0, y: 0, scale: 1, transition: {duration: 0}}
				}
				transition={{duration: 0}}
				whileHover={prefersReducedMotion ? undefined : {scale: 1.05}}
				whileTap={prefersReducedMotion ? undefined : {y: 1}}
				aria-label={typeof label === 'string' ? label : undefined}
				data-flx="app.scroll-indicator-overlay.floating-scroll-indicator.indicator.click.button"
			>
				{label}
			</motion.button>
		</FocusRing>
	);
};

interface ScrollIndicatorOverlayProps {
	getScrollContainer: () => HTMLElement | null;
	scrollContainerIdentity: string;
	dependencies?: React.DependencyList;
	label: React.ReactNode;
}

export const ScrollIndicatorOverlay = ({
	getScrollContainer,
	scrollContainerIdentity,
	dependencies = [],
	label,
}: ScrollIndicatorOverlayProps) => {
	const {activeIndicators, refresh, scheduleRefresh} = useScrollEdgeIndicators(
		getScrollContainer,
		scrollContainerIdentity,
		dependencies,
	);
	const scrollIndicatorIntoView = (indicator: ActiveScrollIndicator) => {
		const container = getScrollContainer();
		if (!container) {
			refresh();
			return;
		}
		const node = findScrollIndicatorNode(container, indicator.indicator.id);
		if (!node) {
			refresh();
			return;
		}
		scrollIndicatorNodeIntoVisibleViewport(
			container,
			node,
			indicator.direction,
			Accessibility.useSmoothScrolling ? 'smooth' : 'auto',
		);
		scheduleRefresh();
	};
	return (
		<div className={styles.scrollIndicatorLayer} data-flx="app.scroll-indicator-overlay.scroll-indicator-layer">
			<AnimatePresence initial={false} data-flx="app.scroll-indicator-overlay.animate-presence">
				{Object.values(activeIndicators).map((activeIndicator) =>
					activeIndicator == null ? null : (
						<div
							key={`${activeIndicator.direction}:${activeIndicator.indicator.id}:${activeIndicator.indicator.severity}`}
							className={clsx(
								styles.indicatorSlot,
								activeIndicator.direction === 'top' ? styles.indicatorSlotTop : styles.indicatorSlotBottom,
							)}
							data-flx="app.scroll-indicator-overlay.indicator-slot"
						>
							<FloatingScrollIndicator
								severity={activeIndicator.indicator.severity}
								onClick={() => scrollIndicatorIntoView(activeIndicator)}
								label={label}
								data-flx="app.scroll-indicator-overlay.floating-scroll-indicator.scroll-indicator-into-view"
							/>
						</div>
					),
				)}
			</AnimatePresence>
		</div>
	);
};
