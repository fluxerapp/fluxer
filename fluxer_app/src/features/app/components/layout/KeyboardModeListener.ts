// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {CHANNEL_TEXTAREA_SELECTOR} from '@app/features/app/keybindings/utils/EditableElement';
import {useLocation} from '@app/features/platform/components/router/RouterReact';
import {ComponentBus} from '@app/features/platform/utils/ComponentBus';
import FocusRingManager from '@app/features/ui/focus_ring/FocusRingManager';
import KeyboardMode from '@app/features/ui/state/KeyboardMode';
import {
	getActivatableFocusTarget,
	recordPointerActivationFocusTarget,
} from '@app/features/ui/utils/PointerActivationFocus';
import {observer} from 'mobx-react-lite';
import {useEffect, useMemo} from 'react';

const FOCUS_TRAPPING_OVERLAY_SELECTOR = [
	'[role="dialog"]',
	'[role="alertdialog"]',
	'[role="menu"]',
	'[role="listbox"]',
	'[data-floating-ui-portal]',
	'[data-popouts-root]',
	'[data-rsbs-overlay]',
].join(',');

const isElementInert = (element: Element | null): boolean => element != null && element.closest('[inert]') != null;

const canRedirectTabToComposer = (composer: HTMLTextAreaElement | null): composer is HTMLTextAreaElement => {
	if (composer == null || composer.disabled || composer.getAttribute('aria-disabled') === 'true') return false;
	if (isElementInert(composer)) return false;
	const active = document.activeElement;
	return !(active instanceof Element && active.closest(FOCUS_TRAPPING_OVERLAY_SELECTOR) != null);
};

export const KeyboardModeListener = observer(() => {
	const keyboardModeEnabled = KeyboardMode.keyboardModeEnabled;
	const location = useLocation();
	const isAuthRoute = useMemo(() => {
		const path = location.pathname;
		return (
			path.startsWith(Routes.LOGIN) ||
			path.startsWith(Routes.REGISTER) ||
			path.startsWith(Routes.FORGOT_PASSWORD) ||
			path.startsWith(Routes.RESET_PASSWORD) ||
			path.startsWith(Routes.VERIFY_EMAIL) ||
			path.startsWith(Routes.AUTHORIZE_IP) ||
			path.startsWith(Routes.OAUTH_AUTHORIZE) ||
			path.startsWith('/invite/') ||
			path.startsWith('/gift/')
		);
	}, [location.pathname]);
	useEffect(() => {
		let lastWindowFocusTime = document.hasFocus() ? 0 : -Infinity;
		const REFOCUS_THRESHOLD_MS = 100;
		const handleWindowFocused = () => {
			lastWindowFocusTime = performance.now();
		};
		const handleKeyDown = (event: KeyboardEvent) => {
			if (event.key === 'Tab') {
				if (!KeyboardMode.keyboardModeEnabled) {
					const composer = document.querySelector<HTMLTextAreaElement>(CHANNEL_TEXTAREA_SELECTOR);
					if (canRedirectTabToComposer(composer)) {
						event.preventDefault();
						ComponentBus.dispatch('FOCUS_TEXTAREA', {enterKeyboardMode: true});
						return;
					}
				}
				KeyboardMode.enterKeyboardMode(!isAuthRoute);
			}
		};
		const handlePointer = (event: MouseEvent | PointerEvent) => {
			recordPointerActivationFocusTarget(getActivatableFocusTarget(event.target));
			const timeSinceFocus = performance.now() - lastWindowFocusTime;
			if (timeSinceFocus > REFOCUS_THRESHOLD_MS) {
				KeyboardMode.exitKeyboardMode();
			}
		};
		window.addEventListener('focus', handleWindowFocused);
		window.addEventListener('keydown', handleKeyDown, true);
		window.addEventListener('mousedown', handlePointer, true);
		window.addEventListener('pointerdown', handlePointer, true);
		return () => {
			window.removeEventListener('focus', handleWindowFocused);
			window.removeEventListener('keydown', handleKeyDown, true);
			window.removeEventListener('mousedown', handlePointer, true);
			window.removeEventListener('pointerdown', handlePointer, true);
		};
	}, [isAuthRoute]);
	useEffect(() => {
		FocusRingManager.setRingsEnabled(keyboardModeEnabled);
	}, [keyboardModeEnabled]);
	useEffect(() => {
		const pendingFrames = new Set<number>();
		const handlePointerActivation = (event: MouseEvent) => {
			if (event.detail === 0) return;
			const control = getActivatableFocusTarget(event.target);
			if (!control) return;
			recordPointerActivationFocusTarget(control);
			if (document.activeElement !== control) return;
			const frame = requestAnimationFrame(() => {
				pendingFrames.delete(frame);
				if (document.activeElement === control) {
					control.blur();
				}
			});
			pendingFrames.add(frame);
		};
		window.addEventListener('click', handlePointerActivation, true);
		return () => {
			window.removeEventListener('click', handlePointerActivation, true);
			for (const frame of pendingFrames) cancelAnimationFrame(frame);
			pendingFrames.clear();
		};
	}, []);
	return null;
});
