// SPDX-License-Identifier: AGPL-3.0-or-later

export interface FullScreenHideableWindow {
	isDestroyed(): boolean;
	isFullScreen(): boolean;
	setFullScreen(flag: boolean): void;
	hide(): void;
	once(event: 'leave-full-screen', listener: () => void): unknown;
	removeListener(event: 'leave-full-screen', listener: () => void): unknown;
}

const pendingHides = new WeakMap<FullScreenHideableWindow, () => void>();

export function hideWindowLeavingFullScreen(
	window: FullScreenHideableWindow,
	platform: NodeJS.Platform = process.platform,
): void {
	if (platform !== 'darwin' || !window.isFullScreen()) {
		window.hide();
		return;
	}
	if (pendingHides.has(window)) return;
	const hide = () => {
		pendingHides.delete(window);
		if (!window.isDestroyed()) window.hide();
	};
	pendingHides.set(window, hide);
	window.once('leave-full-screen', hide);
	window.setFullScreen(false);
}

export function cancelPendingFullScreenHide(window: FullScreenHideableWindow): void {
	const hide = pendingHides.get(window);
	if (!hide) return;
	pendingHides.delete(window);
	window.removeListener('leave-full-screen', hide);
}
