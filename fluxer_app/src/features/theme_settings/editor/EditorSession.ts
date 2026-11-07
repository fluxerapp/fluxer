// SPDX-License-Identifier: AGPL-3.0-or-later

const openedCss = new Map<string, string>();
let liveEditorCapture: (() => void) | null = null;

export function readOpenedCss(key: string, current: string): string {
	const opened = openedCss.get(key);
	if (opened !== undefined) return opened;
	openedCss.set(key, current);
	return current;
}

export function forgetOpenedCss(): void {
	openedCss.clear();
}

export function registerLiveEditorCapture(capture: () => void): () => void {
	liveEditorCapture = capture;
	return () => {
		if (liveEditorCapture === capture) liveEditorCapture = null;
	};
}

export function flushEditorHistory(): void {
	liveEditorCapture?.();
}
