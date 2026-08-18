// SPDX-License-Identifier: AGPL-3.0-or-later

export const WINDOW_FOCUSED_CLASS = 'window-focused';
export const WINDOW_FOCUS_ACTIVATION_GUARD_CLASS = 'window-focus-activation-guard';
export const UNFOCUSED_FULLY_INTERACTIVE_CLASS = 'unfocused-fully-interactive';
export const WINDOW_HOVER_CONTROLS_CHANGE_EVENT = 'fluxer-window-hover-controls-change';
const DEFAULT_GUARD_TIMEOUT_MS = 250;
const DEFAULT_RELEASE_CLEAR_DELAY_MS = 50;
const DEFAULT_MAX_GUARD_TIMEOUT_MS = 5000;

interface WindowFocusInteractionGuardOptions {
	root?: HTMLElement;
	windowTarget?: Window;
	initiallyFocused?: boolean;
	guardTimeoutMs?: number;
	releaseClearDelayMs?: number;
	maxGuardTimeoutMs?: number;
}

export interface WindowFocusInteractionGuard {
	setFocused: (focused: boolean) => void;
	clearActivationGuard: () => void;
	destroy: () => void;
}

export function isWindowFocusActivationGuardActive(root: HTMLElement = document.documentElement): boolean {
	return root.classList.contains(WINDOW_FOCUS_ACTIVATION_GUARD_CLASS);
}

export function isUnfocusedFullyInteractive(root: HTMLElement = document.documentElement): boolean {
	return root.classList.contains(UNFOCUSED_FULLY_INTERACTIVE_CLASS);
}

export function canUseWindowFocusedHoverControls(root: HTMLElement = document.documentElement): boolean {
	if (isUnfocusedFullyInteractive(root)) {
		return !isWindowFocusActivationGuardActive(root);
	}
	return root.classList.contains(WINDOW_FOCUSED_CLASS) && !isWindowFocusActivationGuardActive(root);
}

export function subscribeWindowHoverControlsChange(listener: () => void, windowTarget: Window = window): () => void {
	windowTarget.addEventListener(WINDOW_HOVER_CONTROLS_CHANGE_EVENT, listener);
	return () => windowTarget.removeEventListener(WINDOW_HOVER_CONTROLS_CHANGE_EVENT, listener);
}

export function createWindowFocusInteractionGuard({
	root = document.documentElement,
	windowTarget = window,
	initiallyFocused = root.classList.contains(WINDOW_FOCUSED_CLASS),
	guardTimeoutMs = DEFAULT_GUARD_TIMEOUT_MS,
	releaseClearDelayMs = DEFAULT_RELEASE_CLEAR_DELAY_MS,
	maxGuardTimeoutMs = DEFAULT_MAX_GUARD_TIMEOUT_MS,
}: WindowFocusInteractionGuardOptions = {}): WindowFocusInteractionGuard {
	let focused = initiallyFocused;
	let wasBlurred = !initiallyFocused;
	let activationPointerInProgress = false;
	let guardTimer: number | null = null;
	let releaseTimer: number | null = null;
	let maxGuardTimer: number | null = null;
	let lastHoverControlsEnabled = canUseWindowFocusedHoverControls(root);
	const notifyHoverControlsChange = () => {
		const nextHoverControlsEnabled = canUseWindowFocusedHoverControls(root);
		if (nextHoverControlsEnabled === lastHoverControlsEnabled) return;
		lastHoverControlsEnabled = nextHoverControlsEnabled;
		windowTarget.dispatchEvent(new Event(WINDOW_HOVER_CONTROLS_CHANGE_EVENT));
	};
	const clearGuardTimer = () => {
		if (guardTimer == null) return;
		windowTarget.clearTimeout(guardTimer);
		guardTimer = null;
	};
	const clearReleaseTimer = () => {
		if (releaseTimer == null) return;
		windowTarget.clearTimeout(releaseTimer);
		releaseTimer = null;
	};
	const clearMaxGuardTimer = () => {
		if (maxGuardTimer == null) return;
		windowTarget.clearTimeout(maxGuardTimer);
		maxGuardTimer = null;
	};
	const clearActivationGuard = () => {
		clearGuardTimer();
		clearReleaseTimer();
		clearMaxGuardTimer();
		activationPointerInProgress = false;
		root.classList.remove(WINDOW_FOCUS_ACTIVATION_GUARD_CLASS);
		notifyHoverControlsChange();
	};
	const scheduleGuardFallback = () => {
		clearGuardTimer();
		guardTimer = windowTarget.setTimeout(() => {
			guardTimer = null;
			if (!activationPointerInProgress) {
				clearActivationGuard();
			}
		}, guardTimeoutMs);
	};
	const beginActivationGuard = (pointerInProgress: boolean) => {
		clearReleaseTimer();
		if (pointerInProgress) {
			activationPointerInProgress = true;
		}
		root.classList.add(WINDOW_FOCUS_ACTIVATION_GUARD_CLASS);
		notifyHoverControlsChange();
		scheduleGuardFallback();
		if (maxGuardTimer == null) {
			maxGuardTimer = windowTarget.setTimeout(() => {
				maxGuardTimer = null;
				clearActivationGuard();
			}, maxGuardTimeoutMs);
		}
	};
	const scheduleClearAfterActivationClick = () => {
		if (!isWindowFocusActivationGuardActive(root)) return;
		activationPointerInProgress = false;
		clearGuardTimer();
		clearReleaseTimer();
		releaseTimer = windowTarget.setTimeout(() => {
			releaseTimer = null;
			clearActivationGuard();
		}, releaseClearDelayMs);
	};
	const handlePointerStart: EventListener = () => {
		if (wasBlurred || !focused || isWindowFocusActivationGuardActive(root)) {
			beginActivationGuard(true);
		}
	};
	const handlePointerEnd: EventListener = () => {
		scheduleClearAfterActivationClick();
	};
	windowTarget.addEventListener('pointerdown', handlePointerStart, true);
	windowTarget.addEventListener('mousedown', handlePointerStart, true);
	windowTarget.addEventListener('pointerup', handlePointerEnd, true);
	windowTarget.addEventListener('mouseup', handlePointerEnd, true);
	windowTarget.addEventListener('pointercancel', handlePointerEnd, true);
	windowTarget.addEventListener('click', handlePointerEnd, true);
	const classObserver =
		typeof MutationObserver !== 'undefined' ? new MutationObserver(notifyHoverControlsChange) : null;
	classObserver?.observe(root, {attributes: true, attributeFilter: ['class']});
	return {
		setFocused(nextFocused: boolean): void {
			focused = nextFocused;
			root.classList.toggle(WINDOW_FOCUSED_CLASS, nextFocused);
			notifyHoverControlsChange();
			if (!nextFocused) {
				wasBlurred = true;
				clearActivationGuard();
				return;
			}
			if (wasBlurred) {
				beginActivationGuard(false);
			}
			wasBlurred = false;
		},
		clearActivationGuard,
		destroy(): void {
			windowTarget.removeEventListener('pointerdown', handlePointerStart, true);
			windowTarget.removeEventListener('mousedown', handlePointerStart, true);
			windowTarget.removeEventListener('pointerup', handlePointerEnd, true);
			windowTarget.removeEventListener('mouseup', handlePointerEnd, true);
			windowTarget.removeEventListener('pointercancel', handlePointerEnd, true);
			windowTarget.removeEventListener('click', handlePointerEnd, true);
			classObserver?.disconnect();
			clearActivationGuard();
			root.classList.remove(WINDOW_FOCUSED_CLASS);
			notifyHoverControlsChange();
		},
	};
}
