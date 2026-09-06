// SPDX-License-Identifier: AGPL-3.0-or-later

import FocusManager from '@app/features/platform/utils/FocusManager';
import {createWindowFocusInteractionGuard} from '@app/features/ui/utils/WindowFocusInteractionGuard';
import {useActivityRecorder} from '@app/features/voice/hooks/useActivityRecorder';
import * as WindowCommands from '@app/features/window/commands/WindowCommands';
import {useCallback, useEffect} from 'react';

interface WindowEventListenersOptions {
	preventDocumentScroll: boolean;
}

export function useWindowEventListeners({preventDocumentScroll}: WindowEventListenersOptions): void {
	const recordActivity = useActivityRecorder();
	const handleUserActivity = useCallback(() => recordActivity(), [recordActivity]);
	const handleImmediateActivity = useCallback(() => recordActivity(true), [recordActivity]);
	const handleResize = useCallback(() => WindowCommands.resized(), []);
	useEffect(() => {
		FocusManager.init();
		const guard = createWindowFocusInteractionGuard({initiallyFocused: document.hasFocus()});
		guard.setFocused(document.hasFocus());
		const handleFocus = () => {
			guard.setFocused(true);
			WindowCommands.focused(true);
			handleImmediateActivity();
		};
		const handleBlur = () => {
			if (document.hasFocus()) return;
			guard.setFocused(false);
			WindowCommands.focused(false);
		};
		const handleVisibilityChange = () => {
			WindowCommands.visibilityChanged(!document.hidden);
		};
		const preventPinchZoom = (event: TouchEvent) => {
			if (event.touches.length > 1) {
				event.preventDefault();
			}
		};
		const preventScroll = (event: Event) => event.preventDefault();
		window.addEventListener('focus', handleFocus);
		window.addEventListener('blur', handleBlur);
		document.addEventListener('visibilitychange', handleVisibilityChange);
		window.addEventListener('mousedown', handleImmediateActivity);
		window.addEventListener('keydown', handleUserActivity);
		window.addEventListener('input', handleUserActivity);
		window.addEventListener('resize', handleResize);
		window.addEventListener('touchstart', handleImmediateActivity);
		document.addEventListener('touchstart', preventPinchZoom, {passive: false});
		document.addEventListener('touchmove', preventPinchZoom, {passive: false});
		if (preventDocumentScroll) {
			document.addEventListener('scroll', preventScroll);
		}
		return () => {
			FocusManager.destroy();
			guard.destroy();
			window.removeEventListener('focus', handleFocus);
			window.removeEventListener('blur', handleBlur);
			document.removeEventListener('visibilitychange', handleVisibilityChange);
			window.removeEventListener('mousedown', handleImmediateActivity);
			window.removeEventListener('keydown', handleUserActivity);
			window.removeEventListener('input', handleUserActivity);
			window.removeEventListener('resize', handleResize);
			window.removeEventListener('touchstart', handleImmediateActivity);
			document.removeEventListener('touchstart', preventPinchZoom);
			document.removeEventListener('touchmove', preventPinchZoom);
			if (preventDocumentScroll) {
				document.removeEventListener('scroll', preventScroll);
			}
		};
	}, [handleImmediateActivity, handleUserActivity, handleResize, preventDocumentScroll]);
}
