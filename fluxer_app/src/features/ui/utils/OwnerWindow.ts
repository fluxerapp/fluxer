// SPDX-License-Identifier: AGPL-3.0-or-later

export interface OwnedAnimationFrame {
	readonly id: number;
	readonly ownerWindow: Window;
}

export function requireOwnerWindow(element: HTMLElement, label: string): Window & typeof globalThis {
	const ownerWindow = element.ownerDocument.defaultView;
	if (ownerWindow == null) throw new Error(`${label} has no owner window`);
	return ownerWindow;
}

export function cancelOwnedAnimationFrame(frame: OwnedAnimationFrame | null): void {
	if (frame != null) frame.ownerWindow.cancelAnimationFrame(frame.id);
}
