// SPDX-License-Identifier: AGPL-3.0-or-later

interface PopoutReturnFocusInput<TReturnFocusRef> {
	restoreFocusPolicy: boolean;
	isKeyboardModeEnabled: boolean;
	returnFocusRef?: TReturnFocusRef | null;
	keyboardDismissRef: TReturnFocusRef;
	isTargetInDOM: boolean;
}

export function resolvePopoutReturnFocus<TReturnFocusRef>({
	restoreFocusPolicy,
	isKeyboardModeEnabled,
	returnFocusRef,
	keyboardDismissRef,
	isTargetInDOM,
}: PopoutReturnFocusInput<TReturnFocusRef>): boolean | TReturnFocusRef {
	if (!restoreFocusPolicy && !isKeyboardModeEnabled) {
		return keyboardDismissRef;
	}
	if (returnFocusRef != null) {
		return returnFocusRef;
	}
	return isTargetInDOM;
}

interface PopoutKeyboardDismissInput {
	ownerWindow: Window;
	keyboardDismissRef: {current: HTMLElement | null};
	resolveFocusTarget: () => HTMLElement | null;
}

export function observePopoutKeyboardDismiss({
	ownerWindow,
	keyboardDismissRef,
	resolveFocusTarget,
}: PopoutKeyboardDismissInput): () => void {
	const handleKeyDown = (event: KeyboardEvent): void => {
		if (event.key !== 'Escape') {
			return;
		}
		keyboardDismissRef.current = resolveFocusTarget();
	};
	ownerWindow.addEventListener('keydown', handleKeyDown, true);
	return () => {
		ownerWindow.removeEventListener('keydown', handleKeyDown, true);
	};
}
