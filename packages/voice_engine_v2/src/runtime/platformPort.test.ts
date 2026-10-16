// SPDX-License-Identifier: AGPL-3.0-or-later

import {createVoiceEngineV2SystemClockPort} from '@fluxer/voice_engine_v2/src/runtime/platformPort';
import {afterEach, describe, expect, it, vi} from 'vitest';

afterEach(() => {
	vi.restoreAllMocks();
});

describe('voice engine v2 platform port source failures', () => {
	it('keeps the clock non-throwing, counts failures, and logs exactly once', () => {
		const consoleError = vi.spyOn(console, 'error').mockImplementation(() => {});
		const clock = createVoiceEngineV2SystemClockPort({
			read(): number {
				throw new Error('clock source boom');
			},
		});

		expect(clock.now()).toBe(0);
		expect(clock.now()).toBe(0);
		expect(clock.now()).toBe(0);

		expect(clock.sourceFailureCount).toBe(3);
		expect(consoleError).toHaveBeenCalledTimes(1);
	});

	it('pins a failing clock to the last safe value instead of regressing', () => {
		vi.spyOn(console, 'error').mockImplementation(() => {});
		let healthy = true;
		const clock = createVoiceEngineV2SystemClockPort({
			read(): number {
				if (!healthy) throw new Error('clock source boom');
				return 1_000;
			},
		});

		expect(clock.now()).toBe(1_000);
		healthy = false;
		expect(clock.now()).toBe(1_000);
		expect(clock.sourceFailureCount).toBe(1);
	});

	it('reports zero failures and stays silent for healthy sources', () => {
		const consoleError = vi.spyOn(console, 'error').mockImplementation(() => {});
		const clock = createVoiceEngineV2SystemClockPort({
			read(): number {
				return 42;
			},
		});

		expect(clock.now()).toBe(42);
		expect(clock.sourceFailureCount).toBe(0);
		expect(consoleError).not.toHaveBeenCalled();
	});

	it('latches failure logging per port instance, not globally', () => {
		const consoleError = vi.spyOn(console, 'error').mockImplementation(() => {});
		const first = createVoiceEngineV2SystemClockPort({
			read(): number {
				throw new Error('first boom');
			},
		});
		const second = createVoiceEngineV2SystemClockPort({
			read(): number {
				throw new Error('second boom');
			},
		});

		first.now();
		second.now();

		expect(first.sourceFailureCount).toBe(1);
		expect(second.sourceFailureCount).toBe(1);
		expect(consoleError).toHaveBeenCalledTimes(2);
	});
});
