// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {autorun} from 'mobx';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

async function loadTick() {
	vi.resetModules();
	const {Logger} = await import('@app/features/platform/utils/AppLogger');
	const debug = vi.spyOn(Logger.prototype, 'debug').mockImplementation(() => undefined);
	const {default: Window} = await import('@app/features/window/state/Window');
	const {default: Tick} = await import('@app/features/ui/state/Tick');
	return {debug, Tick, Window};
}

describe('Tick', () => {
	beforeEach(() => {
		vi.useFakeTimers();
	});

	afterEach(() => {
		vi.restoreAllMocks();
		vi.useRealTimers();
	});

	it('keeps counting while the window is visible but not focused', async () => {
		const {debug, Tick, Window} = await loadTick();
		const dispose = autorun(() => Tick.nowSecond);
		const started = Tick.nowSecond;
		Window.setFocused(false);
		expect(debug).toHaveBeenCalledExactlyOnceWith('Window focus changed: false');
		vi.advanceTimersByTime(3000);
		expect(Window.visible).toBe(true);
		expect(Tick.nowSecond).toBe(started + 3);
		dispose();
	});

	it('stops counting while the window is hidden', async () => {
		const {debug, Tick, Window} = await loadTick();
		const dispose = autorun(() => Tick.nowSecond);
		Window.setVisible(false);
		expect(debug).toHaveBeenCalledExactlyOnceWith('Window visibility changed: false');
		const stopped = Tick.nowSecond;
		vi.advanceTimersByTime(5000);
		expect(Tick.nowSecond).toBe(stopped);
		dispose();
	});

	it('resynchronises to wall clock as soon as the window becomes visible again', async () => {
		const {debug, Tick, Window} = await loadTick();
		const dispose = autorun(() => Tick.nowSecond);
		Window.setVisible(false);
		const stopped = Tick.nowSecond;
		vi.advanceTimersByTime(60_000);
		expect(Tick.nowSecond).toBe(stopped);
		Window.setVisible(true);
		expect(debug).toHaveBeenNthCalledWith(1, 'Window visibility changed: false');
		expect(debug).toHaveBeenNthCalledWith(2, 'Window visibility changed: true');
		expect(debug).toHaveBeenCalledTimes(2);
		expect(Tick.nowSecond).toBe(stopped + 60);
		dispose();
	});

	it('does not run an interval while nothing observes the clock', async () => {
		const {debug, Tick} = await loadTick();
		const idle = Tick.nowSecond;
		vi.advanceTimersByTime(5000);
		expect(Tick.nowSecond).toBe(idle);
		const dispose = autorun(() => Tick.nowSecond);
		expect(Tick.nowSecond).toBe(idle + 5);
		expect(debug).not.toHaveBeenCalled();
		dispose();
	});
});
