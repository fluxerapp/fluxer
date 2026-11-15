// SPDX-License-Identifier: AGPL-3.0-or-later

import {useCallback, useEffect, useRef, useState} from 'react';

export interface UseScrollSpyOptions {
	sectionIds: ReadonlyArray<string>;
	container: HTMLElement | null;
}

export interface UseScrollSpyReturn {
	activeSectionId: string | null;
	scrollToSection: (sectionId: string) => boolean;
}

interface MeasuredSection {
	id: string;
	top: number;
}

interface SectionThreshold {
	id: string;
	threshold: number;
}

interface PinnedSection {
	id: string;
	expectedScrollTop: number;
}

const SCROLL_EPSILON = 2;
const SCROLL_OFFSET = 24;

function getSectionElement(container: HTMLElement, id: string): HTMLElement | null {
	const element = document.getElementById(id);
	if (!(element instanceof HTMLElement) || !container.contains(element)) {
		return null;
	}
	return element;
}

function getMaxScrollTop(container: HTMLElement): number {
	return Math.max(0, container.scrollHeight - container.clientHeight);
}

function clampScrollTop(value: number, maxScrollTop: number): number {
	return Math.min(Math.max(value, 0), maxScrollTop);
}

function buildThresholds(
	sections: ReadonlyArray<MeasuredSection>,
	maxScrollTop: number,
	clientHeight: number,
	offset: number,
): Array<SectionThreshold> {
	const raw = sections.map(({id, top}) => ({id, threshold: top - offset}));
	if (raw.length === 0 || maxScrollTop <= 0) {
		return raw;
	}
	const firstUnreachable = raw.findIndex(({threshold}) => threshold > maxScrollTop - SCROLL_EPSILON);
	if (firstUnreachable === -1) {
		return raw;
	}
	const span = raw[raw.length - 1]!.threshold - maxScrollTop;
	if (span <= 0) {
		return raw.map((section, index) => (index < firstUnreachable ? section : {...section, threshold: maxScrollTop}));
	}
	const zoneFloor = firstUnreachable > 0 ? clampScrollTop(raw[firstUnreachable - 1]!.threshold, maxScrollTop) : 0;
	const zone = Math.min(span, Math.max(clientHeight - offset, 0), maxScrollTop - zoneFloor);
	return raw.map((section, index) => {
		if (index < firstUnreachable) {
			return section;
		}
		const progress = Math.min(Math.max((section.threshold - maxScrollTop) / span, 0), 1);
		return {id: section.id, threshold: maxScrollTop - zone * (1 - progress)};
	});
}

export function useScrollSpy({sectionIds, container}: UseScrollSpyOptions): UseScrollSpyReturn {
	const [activeSectionId, setActiveSectionId] = useState<string | null>(sectionIds[0] ?? null);
	const thresholdsRef = useRef<Array<SectionThreshold>>([]);
	const sectionTopsRef = useRef<Map<string, number>>(new Map());
	const pinnedRef = useRef<PinnedSection | null>(null);
	const rafIdRef = useRef<number | null>(null);
	useEffect(() => {
		pinnedRef.current = null;
		thresholdsRef.current = [];
		sectionTopsRef.current = new Map();
		if (!container || sectionIds.length === 0) {
			setActiveSectionId(sectionIds[0] ?? null);
			return;
		}
		setActiveSectionId((prev) => (prev !== null && sectionIds.includes(prev) ? prev : (sectionIds[0] ?? null)));
		let shouldMeasure = false;
		let resizeObserver: ResizeObserver | null = null;
		const observedElements = new Set<Element>();
		const observeElement = (element: Element) => {
			if (!resizeObserver || observedElements.has(element)) {
				return;
			}
			resizeObserver.observe(element);
			observedElements.add(element);
		};
		const pruneObservedElements = () => {
			if (!resizeObserver) {
				return;
			}
			for (const element of observedElements) {
				if (element === container) {
					continue;
				}
				if (!container.contains(element)) {
					resizeObserver.unobserve(element);
					observedElements.delete(element);
				}
			}
		};
		const applyActiveFromPosition = () => {
			const scrollTop = container.scrollTop;
			const pinned = pinnedRef.current;
			if (pinned) {
				if (Math.abs(scrollTop - pinned.expectedScrollTop) <= SCROLL_EPSILON) {
					setActiveSectionId(pinned.id);
					return;
				}
				pinnedRef.current = null;
			}
			let nextActive: string | null = null;
			for (const {id, threshold} of thresholdsRef.current) {
				if (threshold <= scrollTop + SCROLL_EPSILON) {
					nextActive = id;
				} else {
					break;
				}
			}
			if (nextActive === null) {
				nextActive = thresholdsRef.current[0]?.id ?? sectionIds[0] ?? null;
			}
			setActiveSectionId(nextActive);
		};
		const reanchorPinnedSection = () => {
			const pinned = pinnedRef.current;
			if (!pinned) {
				return;
			}
			const top = sectionTopsRef.current.get(pinned.id);
			if (top === undefined) {
				return;
			}
			const target = clampScrollTop(top - SCROLL_OFFSET, getMaxScrollTop(container));
			if (Math.abs(target - pinned.expectedScrollTop) > SCROLL_EPSILON) {
				pinned.expectedScrollTop = target;
				container.scrollTo({top: target, behavior: 'auto'});
			}
		};
		const measureSections = () => {
			const containerRect = container.getBoundingClientRect();
			const scrollTop = container.scrollTop;
			const measured: Array<MeasuredSection> = [];
			for (const id of sectionIds) {
				const element = getSectionElement(container, id);
				if (!element) {
					continue;
				}
				observeElement(element);
				measured.push({id, top: element.getBoundingClientRect().top - containerRect.top + scrollTop});
			}
			measured.sort((a, b) => a.top - b.top);
			sectionTopsRef.current = new Map(measured.map(({id, top}) => [id, top]));
			thresholdsRef.current = buildThresholds(
				measured,
				getMaxScrollTop(container),
				container.clientHeight,
				SCROLL_OFFSET,
			);
			pruneObservedElements();
			reanchorPinnedSection();
			applyActiveFromPosition();
		};
		const scheduleUpdate = (needsMeasure: boolean) => {
			shouldMeasure = shouldMeasure || needsMeasure;
			if (rafIdRef.current != null) {
				return;
			}
			rafIdRef.current = window.requestAnimationFrame(() => {
				rafIdRef.current = null;
				if (shouldMeasure) {
					shouldMeasure = false;
					measureSections();
					return;
				}
				applyActiveFromPosition();
			});
		};
		if (typeof ResizeObserver !== 'undefined') {
			resizeObserver = new ResizeObserver(() => scheduleUpdate(true));
			observeElement(container);
		}
		scheduleUpdate(true);
		const handleScroll = () => scheduleUpdate(false);
		const handleResize = () => scheduleUpdate(true);
		container.addEventListener('scroll', handleScroll, {passive: true});
		window.addEventListener('resize', handleResize);
		return () => {
			container.removeEventListener('scroll', handleScroll);
			window.removeEventListener('resize', handleResize);
			resizeObserver?.disconnect();
			if (rafIdRef.current != null) {
				cancelAnimationFrame(rafIdRef.current);
				rafIdRef.current = null;
			}
		};
	}, [sectionIds, container]);
	const scrollToSection = useCallback(
		(sectionId: string) => {
			if (!container) {
				return false;
			}
			const element = getSectionElement(container, sectionId);
			if (!element) {
				return false;
			}
			const containerRect = container.getBoundingClientRect();
			const elementRect = element.getBoundingClientRect();
			const top = container.scrollTop + (elementRect.top - containerRect.top);
			const target = clampScrollTop(top - SCROLL_OFFSET, getMaxScrollTop(container));
			pinnedRef.current = {id: sectionId, expectedScrollTop: target};
			container.scrollTo({top: target, behavior: 'auto'});
			setActiveSectionId(sectionId);
			return true;
		},
		[container],
	);
	return {activeSectionId, scrollToSection};
}
