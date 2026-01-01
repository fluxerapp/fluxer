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

import {useCallback, useEffect, useRef, useState} from 'react';

const DEFAULT_SELECTOR = [
	'button:not([disabled])',
	'input:not([disabled])',
	'select:not([disabled])',
	'textarea:not([disabled])',
	'[role="button"]',
	'[role="menuitem"]',
	'[tabindex]:not([tabindex="-1"])',
].join(',');

type Orientation = 'vertical' | 'horizontal' | 'both';

interface UseRovingFocusOptions {
	focusableSelector?: string;
	orientation?: Orientation;
	loop?: boolean;
	autoFocusFirst?: boolean;
	restoreFocusOnWindowFocus?: boolean;
}

const TEXT_ENTRY_INPUT_TYPES = new Set(['text', 'search', 'url', 'email', 'password', 'tel', 'number']);

const isTextEntryElement = (element: HTMLElement): boolean => {
	if (element.isContentEditable) return true;

	if (element.tagName === 'TEXTAREA') {
		return true;
	}

	if (element.tagName === 'INPUT') {
		const inputType = (element as HTMLInputElement).type.toLowerCase();
		return TEXT_ENTRY_INPUT_TYPES.has(inputType);
	}

	if (element.getAttribute('role') === 'textbox') {
		return true;
	}

	return false;
};

const isDisabledElement = (element: HTMLElement): boolean => {
	if ('disabled' in element && typeof (element as HTMLButtonElement).disabled === 'boolean') {
		return Boolean((element as HTMLButtonElement).disabled);
	}
	return element.getAttribute('aria-disabled') === 'true';
};

const getFocusableElements = (container: HTMLElement, selector: string): Array<HTMLElement> => {
	return Array.from(container.querySelectorAll<HTMLElement>(selector)).filter((element) => {
		return !isDisabledElement(element) && element.tabIndex !== -1;
	});
};

const shouldHandleKey = (key: string, orientation: Orientation): boolean => {
	if (key === 'Home' || key === 'End') return true;
	if (orientation === 'vertical') {
		return key === 'ArrowUp' || key === 'ArrowDown';
	}
	if (orientation === 'horizontal') {
		return key === 'ArrowLeft' || key === 'ArrowRight';
	}
	return key === 'ArrowUp' || key === 'ArrowDown' || key === 'ArrowLeft' || key === 'ArrowRight';
};

const getMovementDelta = (key: string): number | null => {
	switch (key) {
		case 'ArrowDown':
		case 'ArrowRight':
			return 1;
		case 'ArrowUp':
		case 'ArrowLeft':
			return -1;
		default:
			return null;
	}
};

const isRelevantKeyForOrientation = (key: string, orientation: Orientation): boolean => {
	if (orientation === 'vertical') {
		return key === 'ArrowUp' || key === 'ArrowDown';
	}
	if (orientation === 'horizontal') {
		return key === 'ArrowLeft' || key === 'ArrowRight';
	}
	return key === 'ArrowUp' || key === 'ArrowDown' || key === 'ArrowLeft' || key === 'ArrowRight';
};

export const useRovingFocusList = <T extends HTMLElement>(options: UseRovingFocusOptions = {}) => {
	const {
		focusableSelector = DEFAULT_SELECTOR,
		orientation = 'vertical',
		loop = true,
		autoFocusFirst = false,
		restoreFocusOnWindowFocus = true,
	} = options;

	const [node, setNode] = useState<T | null>(null);
	const latestOptionsRef = useRef({focusableSelector, orientation, loop});
	const lastFocusedIndexRef = useRef<number>(-1);

	useEffect(() => {
		latestOptionsRef.current = {focusableSelector, orientation, loop};
	}, [focusableSelector, orientation, loop]);

	useEffect(() => {
		if (!node) return;
		if (!autoFocusFirst) return;
		const elements = getFocusableElements(node, focusableSelector);
		const firstElement = elements[0];
		if (firstElement && document.activeElement !== firstElement) {
			firstElement.focus();
			lastFocusedIndexRef.current = 0;
		}
	}, [node, focusableSelector, autoFocusFirst]);

	useEffect(() => {
		if (!node) return;

		const handleKeyDown = (event: KeyboardEvent) => {
			const currentActiveElement = document.activeElement;
			if (currentActiveElement instanceof HTMLElement && isTextEntryElement(currentActiveElement)) {
				return;
			}

			const {orientation: currentOrientation, loop: shouldLoop, focusableSelector: selector} = latestOptionsRef.current;
			if (!shouldHandleKey(event.key, currentOrientation)) {
				return;
			}

			const focusable = getFocusableElements(node, selector);
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
			} else if (isRelevantKeyForOrientation(event.key, currentOrientation)) {
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

			if (shouldLoop) {
				nextIndex = (nextIndex + focusable.length) % focusable.length;
			} else {
				nextIndex = Math.min(Math.max(nextIndex, 0), focusable.length - 1);
			}

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
			const focusable = getFocusableElements(node, selector);
			const target = event.target.closest(selector) as HTMLElement | null;
			const nextIndex = target ? focusable.indexOf(target) : -1;
			if (nextIndex !== -1) {
				lastFocusedIndexRef.current = nextIndex;
			}
		};

		node.addEventListener('keydown', handleKeyDown);
		node.addEventListener('focusin', handleFocusIn);
		return () => {
			node.removeEventListener('keydown', handleKeyDown);
			node.removeEventListener('focusin', handleFocusIn);
		};
	}, [node]);

	useEffect(() => {
		if (!node || !restoreFocusOnWindowFocus) return;

		const handleWindowFocus = () => {
			const {focusableSelector: selector} = latestOptionsRef.current;
			const focusable = getFocusableElements(node, selector);
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

		window.addEventListener('focus', handleWindowFocus);
		return () => {
			window.removeEventListener('focus', handleWindowFocus);
		};
	}, [node, restoreFocusOnWindowFocus]);

	return useCallback((instance: T | null) => {
		setNode(instance);
	}, []);
};
