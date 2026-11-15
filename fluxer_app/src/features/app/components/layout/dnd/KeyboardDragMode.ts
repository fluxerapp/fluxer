// SPDX-License-Identifier: AGPL-3.0-or-later

let keyboardDragActive = false;

export function setKeyboardDragActive(active: boolean): void {
	keyboardDragActive = active;
}

export function isKeyboardDragActive(): boolean {
	return keyboardDragActive;
}
