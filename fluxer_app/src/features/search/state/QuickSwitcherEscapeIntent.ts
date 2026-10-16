// SPDX-License-Identifier: AGPL-3.0-or-later

export interface EscapeIntent {
	current: boolean;
}

export function trackEscapeIntentUntilNextTask(intent: EscapeIntent): () => void {
	const recordEscape = (event: KeyboardEvent) => {
		if (event.key !== 'Escape') {
			return;
		}
		intent.current = true;
		window.setTimeout(() => {
			intent.current = false;
		}, 0);
	};
	window.addEventListener('keydown', recordEscape, {capture: true});
	return () => {
		window.removeEventListener('keydown', recordEscape, {capture: true});
	};
}

export function consumeEscapeIntent(intent: EscapeIntent): boolean {
	const wasEscape = intent.current;
	intent.current = false;
	return wasEscape;
}
