// SPDX-License-Identifier: AGPL-3.0-or-later

import {isEditableElement} from '@app/features/app/keybindings/utils/EditableElement';
import {SEEK_STEP, VOLUME_STEP} from '@app/features/voice/components/media_player/utils/MediaConstants';
import {useCallback, useEffect} from 'react';

interface UseMediaKeyboardOptions {
	containerRef: React.RefObject<HTMLElement | null>;
	enabled?: boolean;
	onTogglePlay?: () => void;
	onSeekBackward?: (amount: number) => void;
	onSeekForward?: (amount: number) => void;
	onVolumeUp?: (step: number) => void;
	onVolumeDown?: (step: number) => void;
	onToggleMute?: () => void;
	onToggleFullscreen?: () => void;
	onSeekPercentage?: (percentage: number) => void;
	captureDocumentKeys?: boolean;
}

export interface UseMediaKeyboardReturn {
	handleKeyDown: (event: React.KeyboardEvent) => void;
}

const ARROW_SEEK_STEP = 5;
const PLAY_PAUSE_KEYS = [' ', 'Space', 'Spacebar', 'k', 'K', 'MediaPlayPause'] as const;
const SEEK_BACKWARD_KEYS = ['j', 'J'] as const;
const SEEK_FORWARD_KEYS = ['l', 'L'] as const;
const VOLUME_UP_KEYS = ['ArrowUp'] as const;
const VOLUME_DOWN_KEYS = ['ArrowDown'] as const;
const MUTE_KEYS = ['m', 'M'] as const;
const FULLSCREEN_KEYS = ['f', 'F'] as const;
const SEEK_PERCENTAGE_KEYS = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'] as const;
const CONTROL_SELECTOR = [
	'a[href]',
	'button',
	'input',
	'select',
	'textarea',
	'[contenteditable="true"]',
	'[role="button"]',
	'[role="slider"]',
	'[tabindex]',
].join(',');
const FOCUSABLE_SELECTOR = [
	'a[href]',
	'button:not([disabled])',
	'input:not([disabled])',
	'select:not([disabled])',
	'textarea:not([disabled])',
	'[contenteditable="true"]',
	'[tabindex]:not([tabindex="-1"])',
].join(',');

function keyMatches(key: string, keys: ReadonlyArray<string>): boolean {
	return keys.includes(key);
}

function hasShortcutModifier(event: React.KeyboardEvent | KeyboardEvent): boolean {
	return event.ctrlKey || event.metaKey || event.altKey;
}

function shouldFocusedControlOwnKey(event: React.KeyboardEvent | KeyboardEvent, container: HTMLElement): boolean {
	const target = event.target;
	if (!(target instanceof HTMLElement) || !container.contains(target)) return false;
	const control = target.closest(CONTROL_SELECTOR);
	if (!(control instanceof HTMLElement) || control === container || !container.contains(control)) return false;
	const key = event.key;
	const role = control.getAttribute('role');
	if (role === 'slider') {
		return (
			key === 'ArrowLeft' ||
			key === 'ArrowRight' ||
			key === 'ArrowUp' ||
			key === 'ArrowDown' ||
			key === 'Home' ||
			key === 'End'
		);
	}
	if (control.tagName === 'BUTTON' || role === 'button') {
		return key === ' ' || key === 'Space' || key === 'Spacebar' || key === 'Enter';
	}
	return isEditableElement(control);
}

function stopKeyboardEvent(event: KeyboardEvent): void {
	event.preventDefault();
	event.stopPropagation();
	event.stopImmediatePropagation();
}

function shouldBlockFullscreenKey(event: KeyboardEvent): boolean {
	if (hasShortcutModifier(event)) return false;
	if (event.key === 'Shift' || event.key === 'Control' || event.key === 'Alt' || event.key === 'Meta') return false;
	return true;
}

function getFocusableElements(container: HTMLElement): Array<HTMLElement> {
	return Array.from(container.querySelectorAll<HTMLElement>(FOCUSABLE_SELECTOR)).filter((element) => {
		if (element.tabIndex < 0) return false;
		if (element.hidden || element.getAttribute('aria-hidden') === 'true') return false;
		return element.getClientRects().length > 0;
	});
}

function focusElement(element: HTMLElement): void {
	element.focus({preventScroll: true});
}

function trapFocusInContainer(event: KeyboardEvent, container: HTMLElement): void {
	const focusableElements = getFocusableElements(container);
	stopKeyboardEvent(event);
	if (focusableElements.length === 0) {
		focusElement(container);
		return;
	}
	const activeElement = document.activeElement;
	const activeIndex = activeElement instanceof HTMLElement ? focusableElements.indexOf(activeElement) : -1;
	let nextIndex: number;
	if (activeIndex === -1) {
		nextIndex = event.shiftKey ? focusableElements.length - 1 : 0;
	} else if (event.shiftKey) {
		nextIndex = (activeIndex - 1 + focusableElements.length) % focusableElements.length;
	} else {
		nextIndex = (activeIndex + 1) % focusableElements.length;
	}
	focusElement(focusableElements[nextIndex]);
}

export function useMediaKeyboard(options: UseMediaKeyboardOptions): UseMediaKeyboardReturn {
	const {
		containerRef,
		enabled = true,
		onTogglePlay,
		onSeekBackward,
		onSeekForward,
		onVolumeUp,
		onVolumeDown,
		onToggleMute,
		onToggleFullscreen,
		onSeekPercentage,
		captureDocumentKeys = false,
	} = options;
	const handleMediaKeyDown = useCallback(
		(
			event: React.KeyboardEvent | KeyboardEvent,
			options: {
				allowEditableTarget?: boolean;
			} = {},
		): boolean => {
			if (!enabled || hasShortcutModifier(event)) return false;
			const container = containerRef.current;
			if (container && shouldFocusedControlOwnKey(event, container)) return false;
			const target = event.target as HTMLElement;
			if (!options.allowEditableTarget && isEditableElement(target)) {
				return false;
			}
			const {key} = event;
			if (keyMatches(key, PLAY_PAUSE_KEYS)) {
				event.preventDefault();
				if (!event.repeat) onTogglePlay?.();
				return true;
			} else if (keyMatches(key, SEEK_BACKWARD_KEYS)) {
				event.preventDefault();
				onSeekBackward?.(SEEK_STEP);
				return true;
			} else if (key === 'ArrowLeft') {
				event.preventDefault();
				onSeekBackward?.(ARROW_SEEK_STEP);
				return true;
			} else if (keyMatches(key, SEEK_FORWARD_KEYS)) {
				event.preventDefault();
				onSeekForward?.(SEEK_STEP);
				return true;
			} else if (key === 'ArrowRight') {
				event.preventDefault();
				onSeekForward?.(ARROW_SEEK_STEP);
				return true;
			} else if (keyMatches(key, VOLUME_UP_KEYS)) {
				event.preventDefault();
				onVolumeUp?.(VOLUME_STEP);
				return true;
			} else if (keyMatches(key, VOLUME_DOWN_KEYS)) {
				event.preventDefault();
				onVolumeDown?.(VOLUME_STEP);
				return true;
			} else if (keyMatches(key, MUTE_KEYS)) {
				event.preventDefault();
				if (!event.repeat) onToggleMute?.();
				return true;
			} else if (keyMatches(key, FULLSCREEN_KEYS)) {
				event.preventDefault();
				if (!event.repeat) onToggleFullscreen?.();
				return true;
			} else if (keyMatches(key, SEEK_PERCENTAGE_KEYS)) {
				event.preventDefault();
				const percentage = parseInt(key, 10) * 10;
				onSeekPercentage?.(percentage);
				return true;
			} else if (key === 'Home') {
				event.preventDefault();
				onSeekPercentage?.(0);
				return true;
			} else if (key === 'End') {
				event.preventDefault();
				onSeekPercentage?.(100);
				return true;
			}
			return false;
		},
		[
			enabled,
			containerRef,
			onTogglePlay,
			onSeekBackward,
			onSeekForward,
			onVolumeUp,
			onVolumeDown,
			onToggleMute,
			onToggleFullscreen,
			onSeekPercentage,
		],
	);
	const handleKeyDown = useCallback(
		(event: React.KeyboardEvent | KeyboardEvent) => {
			handleMediaKeyDown(event);
		},
		[handleMediaKeyDown],
	);
	useEffect(() => {
		const container = containerRef.current;
		if (!container || !enabled) return;
		const handleContainerKeyDown = (event: KeyboardEvent) => {
			if (container.contains(document.activeElement)) {
				handleMediaKeyDown(event);
			}
		};
		container.addEventListener('keydown', handleContainerKeyDown);
		return () => {
			container.removeEventListener('keydown', handleContainerKeyDown);
		};
	}, [containerRef, enabled, handleMediaKeyDown]);
	useEffect(() => {
		if (!enabled || !captureDocumentKeys) return;
		const handleDocumentKeyDown = (event: KeyboardEvent) => {
			const container = containerRef.current;
			if (!container) return;
			if (event.key === 'Escape') return;
			if (event.key === 'Tab') {
				trapFocusInContainer(event, container);
				return;
			}
			if (shouldFocusedControlOwnKey(event, container)) return;
			if (handleMediaKeyDown(event, {allowEditableTarget: true})) {
				stopKeyboardEvent(event);
				return;
			}
			if (shouldBlockFullscreenKey(event)) {
				stopKeyboardEvent(event);
			}
		};
		document.addEventListener('keydown', handleDocumentKeyDown, true);
		return () => {
			document.removeEventListener('keydown', handleDocumentKeyDown, true);
		};
	}, [captureDocumentKeys, containerRef, enabled, handleMediaKeyDown]);
	return {
		handleKeyDown: handleKeyDown as (event: React.KeyboardEvent) => void,
	};
}
