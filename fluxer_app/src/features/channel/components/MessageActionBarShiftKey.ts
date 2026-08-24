// SPDX-License-Identifier: AGPL-3.0-or-later

import {useCallback, useSyncExternalStore} from 'react';

export const shiftKeyManager = (() => {
	let isShiftPressed = false;
	const listeners = new Set<() => void>();
	const notify = () => {
		listeners.forEach((listener) => listener());
	};
	const setShiftPressed = (pressed: boolean) => {
		if (isShiftPressed !== pressed) {
			isShiftPressed = pressed;
			notify();
		}
	};
	const handleKeyDown = (event: KeyboardEvent) => {
		if (event.key === 'Shift') {
			setShiftPressed(true);
		}
	};
	const handleKeyUp = (event: KeyboardEvent) => {
		if (event.key === 'Shift') {
			setShiftPressed(false);
		}
	};
	const clearShiftPressed = () => {
		setShiftPressed(false);
	};
	const handleVisibilityChange = () => {
		if (document.visibilityState !== 'visible') {
			setShiftPressed(false);
		}
	};
	const handlePointerModifierChange = (event: MouseEvent) => {
		setShiftPressed(event.getModifierState?.('Shift') ?? event.shiftKey);
	};
	const supportsPointerModifierEvents = (): boolean => 'PointerEvent' in window;
	const registerListeners = () => {
		window.addEventListener('keydown', handleKeyDown, true);
		window.addEventListener('keyup', handleKeyUp, true);
		window.addEventListener('blur', clearShiftPressed);
		document.addEventListener('visibilitychange', handleVisibilityChange);
		if (supportsPointerModifierEvents()) {
			window.addEventListener('pointermove', handlePointerModifierChange, true);
			window.addEventListener('pointerdown', handlePointerModifierChange, true);
			window.addEventListener('pointerup', handlePointerModifierChange, true);
			return;
		}
		window.addEventListener('mousemove', handlePointerModifierChange, true);
		window.addEventListener('mousedown', handlePointerModifierChange, true);
		window.addEventListener('mouseup', handlePointerModifierChange, true);
	};
	registerListeners();
	return {
		subscribe: (listener: () => void) => {
			listeners.add(listener);
			return () => {
				listeners.delete(listener);
			};
		},
		getSnapshot: () => isShiftPressed,
		getServerSnapshot: () => false,
	};
})();

export const useShiftKey = (enabled: boolean) => {
	const subscribe = useCallback(
		(listener: () => void) => {
			if (!enabled) {
				return () => undefined;
			}
			return shiftKeyManager.subscribe(listener);
		},
		[enabled],
	);
	const getSnapshot = useCallback(() => {
		return enabled ? shiftKeyManager.getSnapshot() : false;
	}, [enabled]);
	return useSyncExternalStore(subscribe, getSnapshot, shiftKeyManager.getServerSnapshot);
};
