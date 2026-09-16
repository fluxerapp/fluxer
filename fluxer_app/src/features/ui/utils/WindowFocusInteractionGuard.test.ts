// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	canUseWindowFocusedHoverControls,
	createWindowFocusInteractionGuard,
	isWindowFocusActivationGuardActive,
	subscribeWindowHoverControlsChange,
	WINDOW_FOCUSED_CLASS,
	type WindowFocusInteractionGuard,
} from '@app/features/ui/utils/WindowFocusInteractionGuard';
import {afterEach, describe, expect, it} from 'vitest';

const GUARD_TIMEOUT_MS = 10;

let activeGuard: WindowFocusInteractionGuard | null = null;
let unsubscribe: (() => void) | null = null;

function createFocusedGuard(): {guard: WindowFocusInteractionGuard; root: HTMLElement; observed: Array<boolean>} {
	const root = document.createElement('div');
	root.classList.add(WINDOW_FOCUSED_CLASS);
	document.body.append(root);
	const guard = createWindowFocusInteractionGuard({
		root,
		guardTimeoutMs: GUARD_TIMEOUT_MS,
		releaseClearDelayMs: 5,
		maxGuardTimeoutMs: 200,
	});
	activeGuard = guard;
	const observed: Array<boolean> = [];
	unsubscribe = subscribeWindowHoverControlsChange(() => observed.push(canUseWindowFocusedHoverControls(root)));
	return {guard, root, observed};
}

function wait(ms: number): Promise<void> {
	return new Promise((resolve) => {
		setTimeout(resolve, ms);
	});
}

afterEach(() => {
	unsubscribe?.();
	unsubscribe = null;
	activeGuard?.destroy();
	activeGuard = null;
	document.body.replaceChildren();
});

describe('WindowFocusInteractionGuard', () => {
	it('never publishes an enabled edge between refocus and the activation guard', async () => {
		const {guard, root, observed} = createFocusedGuard();

		guard.setFocused(false);
		guard.setFocused(true);

		expect(observed).toEqual([false]);
		expect(isWindowFocusActivationGuardActive(root)).toBe(true);
		expect(canUseWindowFocusedHoverControls(root)).toBe(false);

		await wait(GUARD_TIMEOUT_MS * 3);

		expect(observed).toEqual([false, true]);
		expect(canUseWindowFocusedHoverControls(root)).toBe(true);
	});

	it('does not publish an enabled edge while tearing down a guarded window', () => {
		const {guard, observed} = createFocusedGuard();

		guard.setFocused(false);
		guard.setFocused(true);
		observed.length = 0;

		guard.destroy();
		activeGuard = null;

		expect(observed).toEqual([]);
	});
});
