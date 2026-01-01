/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {clsx} from 'clsx';
import {AnimatePresence, motion} from 'framer-motion';
import React from 'react';
import styles from './ScrollIndicatorOverlay.module.css';

export type ScrollIndicatorSeverity = 'mention' | 'unread';

export interface ScrollEdgeIndicator {
	element: HTMLElement;
	severity: ScrollIndicatorSeverity;
	id?: string;
}

const severityOrder: Record<ScrollIndicatorSeverity, number> = {
	mention: 2,
	unread: 1,
};

export const useScrollEdgeIndicators = (
	getScrollContainer: () => HTMLElement | null,
	dependencies: React.DependencyList = [],
) => {
	const [topIndicator, setTopIndicator] = React.useState<ScrollEdgeIndicator | null>(null);
	const [bottomIndicator, setBottomIndicator] = React.useState<ScrollEdgeIndicator | null>(null);

	const refresh = React.useCallback(() => {
		const container = getScrollContainer();
		if (!container) {
			setTopIndicator(null);
			setBottomIndicator(null);
			return;
		}

		const containerRect = container.getBoundingClientRect();
		const nodes = container.querySelectorAll<HTMLElement>(
			'[data-scroll-indicator="mention"],[data-scroll-indicator="unread"]',
		);

		let nextTop: ScrollEdgeIndicator | null = null;
		let nextBottom: ScrollEdgeIndicator | null = null;
		let topDistance = Infinity;
		let bottomDistance = Infinity;

		for (const node of nodes) {
			const datasetValue = node.dataset.scrollIndicator as ScrollIndicatorSeverity | undefined;
			if (!datasetValue) continue;
			const rect = node.getBoundingClientRect();
			const nodeId = node.dataset.scrollId;

			if (rect.bottom <= containerRect.top) {
				const distance = containerRect.top - rect.bottom;
				if (
					!nextTop ||
					severityOrder[datasetValue] > severityOrder[nextTop.severity] ||
					(severityOrder[datasetValue] === severityOrder[nextTop.severity] && distance < topDistance)
				) {
					nextTop = {element: node, severity: datasetValue, id: nodeId};
					topDistance = distance;
				}
			} else if (rect.top >= containerRect.bottom) {
				const distance = rect.top - containerRect.bottom;
				if (
					!nextBottom ||
					severityOrder[datasetValue] > severityOrder[nextBottom.severity] ||
					(severityOrder[datasetValue] === severityOrder[nextBottom.severity] && distance < bottomDistance)
				) {
					nextBottom = {element: node, severity: datasetValue, id: nodeId};
					bottomDistance = distance;
				}
			}
		}

		setTopIndicator((previous) => {
			if (previous && nextTop) {
				if (previous.element === nextTop.element && previous.severity === nextTop.severity) {
					return previous;
				}
			}
			return nextTop;
		});
		setBottomIndicator((previous) => {
			if (previous && nextBottom) {
				if (previous.element === nextBottom.element && previous.severity === nextBottom.severity) {
					return previous;
				}
			}
			return nextBottom;
		});
	}, [getScrollContainer]);

	React.useLayoutEffect(() => {
		refresh();
	}, [refresh, ...dependencies]);

	React.useEffect(() => {
		const container = getScrollContainer();
		if (!container) return;

		const handleScroll = () => refresh();
		container.addEventListener('scroll', handleScroll, {passive: true});
		return () => {
			container.removeEventListener('scroll', handleScroll);
		};
	}, [getScrollContainer, refresh]);

	React.useEffect(() => {
		const handleResize = () => refresh();
		window.addEventListener('resize', handleResize);
		return () => {
			window.removeEventListener('resize', handleResize);
		};
	}, [refresh]);

	return {topIndicator, bottomIndicator, refresh};
};

interface FloatingScrollIndicatorProps {
	label: React.ReactNode;
	severity: ScrollIndicatorSeverity;
	direction: 'top' | 'bottom';
	onClick: () => void;
}

const FloatingScrollIndicator = ({label, severity, direction, onClick}: FloatingScrollIndicatorProps) => {
	const offset = direction === 'top' ? -16 : 16;

	return (
		<motion.button
			type="button"
			className={clsx(styles.indicator, severity === 'mention' ? styles.indicatorMention : styles.indicatorBrand)}
			onClick={onClick}
			initial={{opacity: 0, y: offset}}
			animate={{opacity: 1, y: 0}}
			exit={{opacity: 0, y: offset}}
			transition={{type: 'spring', stiffness: 500, damping: 20}}
			aria-label={typeof label === 'string' ? label : undefined}
		>
			{label}
		</motion.button>
	);
};

interface ScrollIndicatorOverlayProps {
	getScrollContainer: () => HTMLElement | null;
	dependencies?: React.DependencyList;
	label: React.ReactNode;
}

export const ScrollIndicatorOverlay = ({getScrollContainer, dependencies = [], label}: ScrollIndicatorOverlayProps) => {
	const {topIndicator, bottomIndicator} = useScrollEdgeIndicators(getScrollContainer, dependencies);

	const scrollIndicatorIntoView = (indicator: ScrollEdgeIndicator | null) => {
		if (!indicator) return;
		indicator.element.scrollIntoView({behavior: 'smooth', block: 'nearest'});
	};

	return (
		<div className={styles.scrollIndicatorLayer}>
			<AnimatePresence initial={false}>
				{topIndicator && (
					<div className={clsx(styles.indicatorSlot, styles.indicatorSlotTop)}>
						<FloatingScrollIndicator
							direction="top"
							severity={topIndicator.severity}
							onClick={() => scrollIndicatorIntoView(topIndicator)}
							label={label}
						/>
					</div>
				)}
				{bottomIndicator && (
					<div className={clsx(styles.indicatorSlot, styles.indicatorSlotBottom)}>
						<FloatingScrollIndicator
							direction="bottom"
							severity={bottomIndicator.severity}
							onClick={() => scrollIndicatorIntoView(bottomIndicator)}
							label={label}
						/>
					</div>
				)}
			</AnimatePresence>
		</div>
	);
};
