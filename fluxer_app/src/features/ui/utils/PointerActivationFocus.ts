// SPDX-License-Identifier: AGPL-3.0-or-later

const ACTIVATABLE_FOCUS_TARGET_SELECTOR = 'button, [role="button"]';

let pointerActivationFocusTarget: HTMLElement | null = null;

export function getActivatableFocusTarget(target: EventTarget | null): HTMLElement | null {
	if (typeof Element === 'undefined' || typeof HTMLElement === 'undefined') return null;
	if (!(target instanceof Element)) return null;
	const control = target.closest(ACTIVATABLE_FOCUS_TARGET_SELECTOR);
	return control instanceof HTMLElement ? control : null;
}

export function recordPointerActivationFocusTarget(target: HTMLElement | null): void {
	pointerActivationFocusTarget = target;
}

function isSamePointerActivation(target: HTMLElement, activated: HTMLElement | null): boolean {
	if (activated == null) return false;
	return target === activated || target.contains(activated) || activated.contains(target);
}

export function shouldRestoreFocusToTarget(target: HTMLElement | null, keyboardModeEnabled: boolean): boolean {
	if (target == null) return false;
	if (keyboardModeEnabled) return true;
	return !isSamePointerActivation(target, pointerActivationFocusTarget);
}
