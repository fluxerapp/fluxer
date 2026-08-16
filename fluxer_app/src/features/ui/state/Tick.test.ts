// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {autorun} from 'mobx';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

async function loadTick() {
	vi.resetModules();
	const {default: Window} = await import('@app/features/window/state/Window');
	const {default: Tick} = await import('@app/features/ui/state/Tick');
	return {Tick, Window};
}

describe('Tick', () => {
	beforeEach(() => {
		vi.useFakeTimers();
	});

	afterEach(() => {
		vi.useRealTimers();
	});

	it('keeps counting while the window is visible but not focused', async () => {
		const {Tick, Window} = await loadTick();
		const dispose = autorun(() => Tick.nowSecond);
		const started = Tick.nowSecond;
		Window.setFocused(false);
		vi.advanceTimersByTime(3000);
		expect(Window.visible).toBe(true);
		expect(Tick.nowSecond).toBe(started + 3);
		dispose();
	});

	it('stops counting while the window is hidden', async () => {
		const {Tick, Window} = await loadTick();
		const dispose = autorun(() => Tick.nowSecond);
		Window.setVisible(false);
		const stopped = Tick.nowSecond;
		vi.advanceTimersByTime(5000);
		expect(Tick.nowSecond).toBe(stopped);
		dispose();
	});

	it('resynchronises to wall clock as soon as the window becomes visible again', async () => {
		const {Tick, Window} = await loadTick();
		const dispose = autorun(() => Tick.nowSecond);
		Window.setVisible(false);
		const stopped = Tick.nowSecond;
		vi.advanceTimersByTime(60_000);
		expect(Tick.nowSecond).toBe(stopped);
		Window.setVisible(true);
		expect(Tick.nowSecond).toBe(stopped + 60);
		dispose();
	});

	it('does not run an interval while nothing observes the clock', async () => {
		const {Tick} = await loadTick();
		const idle = Tick.nowSecond;
		vi.advanceTimersByTime(5000);
		expect(Tick.nowSecond).toBe(idle);
		const dispose = autorun(() => Tick.nowSecond);
		expect(Tick.nowSecond).toBe(idle + 5);
		dispose();
	});
});
