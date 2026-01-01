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

import React from 'react';
import AccessibilityStore from '~/stores/AccessibilityStore';

export interface UseScrollSpyOptions {
	sectionIds: ReadonlyArray<string>;
	containerRef: React.RefObject<HTMLElement | null>;
	offset?: number;
}

export interface UseScrollSpyReturn {
	activeSectionId: string | null;
	scrollToSection: (sectionId: string) => void;
}

export function useScrollSpy({sectionIds, containerRef, offset = 68}: UseScrollSpyOptions): UseScrollSpyReturn {
	const [activeSectionId, setActiveSectionId] = React.useState<string | null>(sectionIds[0] ?? null);
	const sectionsInViewRef = React.useRef<Map<string, boolean>>(new Map());

	React.useEffect(() => {
		const container = containerRef.current;
		if (!container || sectionIds.length === 0) {
			setActiveSectionId(null);
			return;
		}

		const handleIntersection = (entries: Array<IntersectionObserverEntry>) => {
			for (const entry of entries) {
				sectionsInViewRef.current.set(entry.target.id, entry.isIntersecting);
			}

			const visibleSections = sectionIds.filter((id) => sectionsInViewRef.current.get(id) === true);

			if (visibleSections.length > 0) {
				let topSectionId = visibleSections[0];
				let topSectionTop = Number.MAX_VALUE;

				for (const id of visibleSections) {
					const element = document.getElementById(id);
					if (element) {
						const rect = element.getBoundingClientRect();
						if (rect.top < topSectionTop) {
							topSectionTop = rect.top;
							topSectionId = id;
						}
					}
				}

				setActiveSectionId(topSectionId);
			}
		};

		const observer = new IntersectionObserver(handleIntersection, {
			root: container,
			rootMargin: `-${offset}px 0px -50% 0px`,
			threshold: [0, 0.1, 0.5, 1],
		});

		for (const id of sectionIds) {
			const element = document.getElementById(id);
			if (element) {
				observer.observe(element);
			}
		}

		return () => {
			observer.disconnect();
			sectionsInViewRef.current.clear();
		};
	}, [sectionIds, containerRef, offset]);

	const scrollToSection = React.useCallback(
		(sectionId: string) => {
			const element = document.getElementById(sectionId);
			const container = containerRef.current;
			if (!element || !container) {
				return;
			}

			const containerRect = container.getBoundingClientRect();
			const elementRect = element.getBoundingClientRect();
			const scrollTop = container.scrollTop + (elementRect.top - containerRect.top) - offset;

			const reduceMotion = AccessibilityStore.useReducedMotion;
			container.scrollTo({
				top: scrollTop,
				behavior: reduceMotion ? 'auto' : 'smooth',
			});

			setActiveSectionId(sectionId);
		},
		[containerRef, offset],
	);

	return {activeSectionId, scrollToSection};
}
