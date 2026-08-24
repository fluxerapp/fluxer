// SPDX-License-Identifier: AGPL-3.0-or-later

import {observeIntersection} from '@app/features/platform/utils/SharedIntersectionObserver';
import {LRUCache} from 'lru-cache';
import {createContext, useCallback, useContext, useEffect, useState} from 'react';

const DEFAULT_ROOT_MARGIN = '300px';
const REMEMBERED_VIEWPORT_KEY_LIMIT = 1000;
const rememberedViewportKeys = new LRUCache<string, true>({
	max: REMEMBERED_VIEWPORT_KEY_LIMIT,
});

export type ScrollSurfaceResolver = () => Element | null;

const resolveNoScrollSurface: ScrollSurfaceResolver = () => null;

export const NearViewportSurfaceContext = createContext<ScrollSurfaceResolver>(resolveNoScrollSurface);

export function resolveObserverRoot(resolve: ScrollSurfaceResolver, element: Element): Element | null {
	const surface = resolve();
	if (surface == null || surface === element) return null;
	if (!surface.contains(element)) return null;
	return surface;
}

interface UseNearViewportOptions {
	disabled?: boolean;
	rememberKey?: string | null;
	rootMargin?: string;
	threshold?: number | Array<number>;
}

export function useNearViewport<T extends Element>({
	disabled = false,
	rememberKey,
	rootMargin = DEFAULT_ROOT_MARGIN,
	threshold = 0,
}: UseNearViewportOptions = {}): {ref: (node: T | null) => void; isNearViewport: boolean} {
	const resolveScrollSurface = useContext(NearViewportSurfaceContext);
	const loadImmediately = disabled || typeof IntersectionObserver === 'undefined';
	const wasRemembered = rememberKey ? rememberedViewportKeys.has(rememberKey) : false;
	const [element, setElement] = useState<T | null>(null);
	const [isNearViewport, setIsNearViewport] = useState(loadImmediately || wasRemembered);
	const ref = useCallback((node: T | null) => {
		setElement(node);
	}, []);
	useEffect(() => {
		if (disabled || typeof IntersectionObserver === 'undefined') {
			setIsNearViewport(true);
		}
	}, [disabled]);
	useEffect(() => {
		if (!isNearViewport || !rememberKey) return;
		if (disabled || typeof IntersectionObserver === 'undefined') return;
		rememberedViewportKeys.set(rememberKey, true);
	}, [disabled, isNearViewport, rememberKey]);
	useEffect(() => {
		if (disabled || isNearViewport || !element) return undefined;
		if (rememberKey && rememberedViewportKeys.has(rememberKey)) {
			setIsNearViewport(true);
			return undefined;
		}
		let release: (() => void) | null = null;
		const stopObserving = () => {
			release?.();
			release = null;
		};
		release = observeIntersection(
			element,
			(entry) => {
				if (!entry.isIntersecting && entry.intersectionRatio <= 0) return;
				stopObserving();
				if (rememberKey) {
					rememberedViewportKeys.set(rememberKey, true);
				}
				setIsNearViewport(true);
			},
			{root: resolveObserverRoot(resolveScrollSurface, element), rootMargin, threshold},
		);
		return stopObserving;
	}, [disabled, element, isNearViewport, rememberKey, resolveScrollSurface, rootMargin, threshold]);
	return {ref, isNearViewport};
}
