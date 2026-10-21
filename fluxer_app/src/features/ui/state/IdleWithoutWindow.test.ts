// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterEach, expect, it, vi} from 'vitest';

afterEach(async () => {
	const {default: Idle} = await import('@app/features/ui/state/Idle');
	Idle.destroy();
	vi.useRealTimers();
});

it('checks idle state on its timer when there is no window', async () => {
	vi.useFakeTimers();
	const {default: Idle} = await import('@app/features/ui/state/Idle');
	expect(typeof window).toBe('undefined');

	await vi.advanceTimersByTimeAsync(11 * 60 * 1000);

	expect(Idle.isIdle()).toBe(true);
});
