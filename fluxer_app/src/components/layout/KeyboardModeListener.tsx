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

import {observer} from 'mobx-react-lite';
import React from 'react';
import FocusRingManager from '~/components/uikit/FocusRing/FocusRingManager';
import {ComponentDispatch} from '~/lib/ComponentDispatch';
import {useLocation} from '~/lib/router';
import {Routes} from '~/Routes';
import KeyboardModeStore from '~/stores/KeyboardModeStore';

export const KeyboardModeListener = observer(function KeyboardModeListener() {
	const keyboardModeEnabled = KeyboardModeStore.keyboardModeEnabled;
	const location = useLocation();

	const isAuthRoute = React.useMemo(() => {
		const path = location.pathname;
		return (
			path.startsWith(Routes.LOGIN) ||
			path.startsWith(Routes.REGISTER) ||
			path.startsWith(Routes.FORGOT_PASSWORD) ||
			path.startsWith(Routes.RESET_PASSWORD) ||
			path.startsWith(Routes.VERIFY_EMAIL) ||
			path.startsWith(Routes.AUTHORIZE_IP) ||
			path.startsWith(Routes.PENDING_VERIFICATION) ||
			path.startsWith(Routes.OAUTH_AUTHORIZE) ||
			path.startsWith('/invite/') ||
			path.startsWith('/gift/')
		);
	}, [location.pathname]);

	React.useEffect(() => {
		let lastWindowFocusTime = document.hasFocus() ? 0 : -Infinity;
		const REFOCUS_THRESHOLD_MS = 100;

		const handleWindowFocus = () => {
			lastWindowFocusTime = performance.now();
		};

		const handleKeyDown = (event: KeyboardEvent) => {
			if (event.key === 'Tab') {
				if (!KeyboardModeStore.keyboardModeEnabled) {
					const textarea = document.querySelector<HTMLTextAreaElement>('[data-channel-textarea]');
					const hasEnabledTextarea =
						textarea && !textarea.disabled && textarea.getAttribute('aria-disabled') !== 'true';

					if (hasEnabledTextarea) {
						event.preventDefault();
						ComponentDispatch.dispatch('FOCUS_TEXTAREA', {enterKeyboardMode: true});
						return;
					}
				}
				KeyboardModeStore.enterKeyboardMode(!isAuthRoute);
			}
		};

		const handlePointer = () => {
			const timeSinceFocus = performance.now() - lastWindowFocusTime;
			if (timeSinceFocus > REFOCUS_THRESHOLD_MS) {
				KeyboardModeStore.exitKeyboardMode();
			}
		};

		window.addEventListener('focus', handleWindowFocus);
		window.addEventListener('keydown', handleKeyDown, true);
		window.addEventListener('mousedown', handlePointer, true);
		window.addEventListener('pointerdown', handlePointer, true);

		return () => {
			window.removeEventListener('focus', handleWindowFocus);
			window.removeEventListener('keydown', handleKeyDown, true);
			window.removeEventListener('mousedown', handlePointer, true);
			window.removeEventListener('pointerdown', handlePointer, true);
		};
	}, [isAuthRoute]);

	React.useEffect(() => {
		FocusRingManager.setRingsEnabled(keyboardModeEnabled);
	}, [keyboardModeEnabled]);

	return null;
});
