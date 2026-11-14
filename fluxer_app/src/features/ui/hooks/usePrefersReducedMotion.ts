// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {reaction} from 'mobx';
import {useSyncExternalStore} from 'react';

const listeners = new Set<() => void>();
let disposeReaction: (() => void) | null = null;

function getSnapshot(): boolean {
	return Accessibility.useReducedMotion;
}

function notifyListeners(): void {
	for (const listener of Array.from(listeners)) {
		listener();
	}
}

function subscribe(onStoreChange: () => void): () => void {
	listeners.add(onStoreChange);
	disposeReaction ??= reaction(getSnapshot, notifyListeners);
	return () => {
		listeners.delete(onStoreChange);
		if (listeners.size > 0) return;
		disposeReaction?.();
		disposeReaction = null;
	};
}

export function usePrefersReducedMotion(): boolean {
	return useSyncExternalStore(subscribe, getSnapshot, getSnapshot);
}
