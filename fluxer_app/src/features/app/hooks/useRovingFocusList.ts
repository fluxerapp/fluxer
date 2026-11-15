// SPDX-License-Identifier: AGPL-3.0-or-later

import {TextEntryElementPolicy} from '@app/features/input/TextEntryElementPolicy';
import {useCallback, useEffect, useRef, useState} from 'react';

interface UseRovingFocusOptions {
	focusableSelector: string;
	restoreFocusOnWindowFocus?: boolean;
	enabled?: boolean;
}

const isDisabledElement = (element: HTMLElement): boolean => {
	if ('disabled' in element && typeof (element as HTMLButtonElement).disabled === 'boolean') {
		return Boolean((element as HTMLButtonElement).disabled);
	}
	return element.getAttribute('aria-disabled') === 'true';
};
const getFocusableElements = (container: HTMLElement, selector: string): Array<HTMLElement> =>
	Array.from(container.querySelectorAll<HTMLElement>(selector)).filter((element) => !isDisabledElement(element));
const ROVING_MANAGED_ATTR = 'data-roving-focus-managed';
const hasFocusableNode = (nodes: NodeList, selector: string): boolean => {
	for (let index = 0; index < nodes.length; index++) {
		const node = nodes.item(index);
		if (!(node instanceof HTMLElement)) continue;
		if (node.matches(selector)) return true;
		if (node.querySelector(selector) != null) return true;
	}
	return false;
};
const applyTabIndices = (focusable: Array<HTMLElement>, activeIndex: number): void => {
	if (focusable.length === 0) return;
	const clamped = activeIndex >= 0 && activeIndex < focusable.length ? activeIndex : 0;
	for (let i = 0; i < focusable.length; i++) {
		const element = focusable[i];
		const desired = i === clamped ? '0' : '-1';
		if (!element.hasAttribute(ROVING_MANAGED_ATTR)) {
			element.setAttribute(ROVING_MANAGED_ATTR, '');
		}
		if (element.getAttribute('tabindex') !== desired) {
			element.setAttribute('tabindex', desired);
		}
	}
};
const shouldHandleKey = (key: string): boolean =>
	key === 'Home' || key === 'End' || key === 'ArrowUp' || key === 'ArrowDown';
const getMovementDelta = (key: string): number | null => {
	switch (key) {
		case 'ArrowDown':
			return 1;
		case 'ArrowUp':
			return -1;
		default:
			return null;
	}
};
const isRelevantKey = (key: string): boolean => key === 'ArrowUp' || key === 'ArrowDown';
export const useRovingFocusList = <T extends HTMLElement>(options: UseRovingFocusOptions) => {
	const {focusableSelector, restoreFocusOnWindowFocus = true, enabled = true} = options;
	const [node, setNode] = useState<T | null>(null);
	const latestOptionsRef = useRef({focusableSelector, enabled});
	const lastFocusedIndexRef = useRef<number>(-1);
	const focusableCacheRef = useRef<{
		node: HTMLElement;
		selector: string;
		elements: Array<HTMLElement>;
	} | null>(null);
	const invalidateFocusableCache = useCallback(() => {
		focusableCacheRef.current = null;
	}, []);
	const getCachedFocusableElements = useCallback((container: HTMLElement, selector: string): Array<HTMLElement> => {
		const cache = focusableCacheRef.current;
		if (cache && cache.node === container && cache.selector === selector) {
			return cache.elements;
		}
		const elements = getFocusableElements(container, selector);
		focusableCacheRef.current = {node: container, selector, elements};
		return elements;
	}, []);
	useEffect(() => {
		latestOptionsRef.current = {focusableSelector, enabled};
		invalidateFocusableCache();
	}, [enabled, focusableSelector, invalidateFocusableCache]);
	useEffect(() => {
		if (!node) return;
		if (!enabled) return;
		const handleKeyDown = (event: KeyboardEvent) => {
			const currentActiveElement = document.activeElement;
			if (currentActiveElement instanceof HTMLElement && TextEntryElementPolicy.isTextEntry(currentActiveElement)) {
				return;
			}
			const {focusableSelector: selector, enabled: rovingFocusEnabled} = latestOptionsRef.current;
			if (!rovingFocusEnabled) return;
			if (!shouldHandleKey(event.key)) {
				return;
			}
			const focusable = getCachedFocusableElements(node, selector);
			if (focusable.length === 0) return;
			const activeElement = document.activeElement as HTMLElement | null;
			const activeTarget = activeElement ? (activeElement.closest(selector) as HTMLElement | null) : null;
			let currentIndex = activeTarget ? focusable.indexOf(activeTarget) : -1;
			if (currentIndex === -1 && lastFocusedIndexRef.current >= 0 && lastFocusedIndexRef.current < focusable.length) {
				currentIndex = lastFocusedIndexRef.current;
			}
			let nextIndex = currentIndex;
			if (event.key === 'Home') {
				nextIndex = 0;
			} else if (event.key === 'End') {
				nextIndex = focusable.length - 1;
			} else if (isRelevantKey(event.key)) {
				const delta = getMovementDelta(event.key);
				if (delta !== null) {
					if (currentIndex === -1) {
						nextIndex = delta > 0 ? 0 : focusable.length - 1;
					} else {
						nextIndex = currentIndex + delta;
					}
				}
			}
			if (nextIndex === currentIndex) return;
			nextIndex = (nextIndex + focusable.length) % focusable.length;
			const target = focusable[nextIndex];
			if (target) {
				event.preventDefault();
				if (event.key !== 'Home' && event.key !== 'End') {
					event.stopPropagation();
				}
				lastFocusedIndexRef.current = nextIndex;
				target.focus();
			}
		};
		const handleFocusIn = (event: FocusEvent) => {
			if (!(event.target instanceof HTMLElement)) return;
			const {focusableSelector: selector} = latestOptionsRef.current;
			const focusable = getCachedFocusableElements(node, selector);
			const target = event.target.closest(selector) as HTMLElement | null;
			const nextIndex = target ? focusable.indexOf(target) : -1;
			if (nextIndex !== -1) {
				lastFocusedIndexRef.current = nextIndex;
				applyTabIndices(focusable, nextIndex);
			}
		};
		node.addEventListener('keydown', handleKeyDown);
		node.addEventListener('focusin', handleFocusIn);
		return () => {
			node.removeEventListener('keydown', handleKeyDown);
			node.removeEventListener('focusin', handleFocusIn);
		};
	}, [node, enabled, getCachedFocusableElements]);
	useEffect(() => {
		if (!node || !restoreFocusOnWindowFocus || !enabled) return;
		const handleWindowFocused = () => {
			const {focusableSelector: selector} = latestOptionsRef.current;
			const focusable = getCachedFocusableElements(node, selector);
			if (focusable.length === 0) return;
			const activeElement = document.activeElement as HTMLElement | null;
			if (activeElement && node.contains(activeElement)) {
				return;
			}
			const lastIndex = lastFocusedIndexRef.current;
			if (lastIndex < 0 || lastIndex >= focusable.length) return;
			const target = focusable[lastIndex];
			if (target) {
				target.focus();
			}
		};
		window.addEventListener('focus', handleWindowFocused);
		return () => {
			window.removeEventListener('focus', handleWindowFocused);
		};
	}, [node, restoreFocusOnWindowFocus, enabled, getCachedFocusableElements]);
	useEffect(() => {
		if (!node) return;
		let scheduled = false;
		let frameId: number | null = null;
		const apply = () => {
			scheduled = false;
			frameId = null;
			const {focusableSelector: selector} = latestOptionsRef.current;
			const focusable = getCachedFocusableElements(node, selector);
			if (focusable.length === 0) return;
			const activeElement = document.activeElement as HTMLElement | null;
			let activeIndex = -1;
			if (activeElement && node.contains(activeElement)) {
				const target = activeElement.closest(selector) as HTMLElement | null;
				if (target) activeIndex = focusable.indexOf(target);
			}
			if (activeIndex === -1) {
				const last = lastFocusedIndexRef.current;
				if (last >= 0 && last < focusable.length) activeIndex = last;
			}
			if (activeIndex === -1) activeIndex = 0;
			applyTabIndices(focusable, activeIndex);
		};
		const schedule = () => {
			if (scheduled) return;
			scheduled = true;
			frameId = requestAnimationFrame(apply);
		};
		apply();
		const observer = new MutationObserver((mutations) => {
			const {focusableSelector: selector} = latestOptionsRef.current;
			for (const m of mutations) {
				if (m.type === 'childList') {
					if (!hasFocusableNode(m.addedNodes, selector) && !hasFocusableNode(m.removedNodes, selector)) {
						continue;
					}
					invalidateFocusableCache();
					schedule();
					return;
				}
				if (m.type === 'attributes' && m.attributeName === 'tabindex') {
					const target = m.target as HTMLElement;
					if (target.hasAttribute(ROVING_MANAGED_ATTR)) {
						invalidateFocusableCache();
						schedule();
						return;
					}
				}
				if (m.type === 'attributes' && (m.attributeName === 'disabled' || m.attributeName === 'aria-disabled')) {
					invalidateFocusableCache();
					schedule();
					return;
				}
			}
		});
		observer.observe(node, {
			childList: true,
			subtree: true,
			attributes: true,
			attributeFilter: ['tabindex', 'disabled', 'aria-disabled'],
		});
		return () => {
			observer.disconnect();
			if (frameId !== null) cancelAnimationFrame(frameId);
			invalidateFocusableCache();
		};
	}, [node, getCachedFocusableElements, invalidateFocusableCache]);
	return useCallback((instance: T | null) => {
		setNode(instance);
	}, []);
};
