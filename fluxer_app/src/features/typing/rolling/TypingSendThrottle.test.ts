// SPDX-License-Identifier: AGPL-3.0-or-later

import {planTypingSend, type TypingSendSlot} from '@app/features/typing/rolling/TypingSendThrottle';
import {describe, expect, it} from 'vitest';

const PREVIOUS_SEND = 100000;
const PENDING_TIMEOUT = {} as NodeJS.Timeout;

function slot(overrides: Partial<TypingSendSlot> = {}): TypingSendSlot {
	return {channelId: 'channel', userId: 'me', timeout: null, prevSend: PREVIOUS_SEND, ...overrides};
}

describe('planTypingSend', () => {
	it('schedules the first ever send after 1500 ms', () => {
		expect(planTypingSend(null, 'channel', 'me', PREVIOUS_SEND)).toEqual({
			dropSlot: false,
			action: 'schedule',
			delayMs: 1500,
		});
	});

	it('throttles while a send is pending', () => {
		expect(planTypingSend(slot({timeout: PENDING_TIMEOUT, prevSend: 0}), 'channel', 'me', PREVIOUS_SEND)).toEqual({
			dropSlot: false,
			action: 'throttled',
		});
	});

	it('throttles when the previous schedule is less than 8000 ms old', () => {
		expect(planTypingSend(slot(), 'channel', 'me', PREVIOUS_SEND + 7999)).toEqual({
			dropSlot: false,
			action: 'throttled',
		});
	});

	it('schedules exactly 8000 ms after the previous schedule', () => {
		expect(planTypingSend(slot(), 'channel', 'me', PREVIOUS_SEND + 8000)).toEqual({
			dropSlot: false,
			action: 'schedule',
			delayMs: 1500,
		});
	});

	it('schedules after 1500 ms when the previous schedule is less than 16000 ms old', () => {
		expect(planTypingSend(slot(), 'channel', 'me', PREVIOUS_SEND + 15999)).toEqual({
			dropSlot: false,
			action: 'schedule',
			delayMs: 1500,
		});
	});

	it('schedules after 0 ms when the previous schedule is exactly 16000 ms old', () => {
		expect(planTypingSend(slot(), 'channel', 'me', PREVIOUS_SEND + 16000)).toEqual({
			dropSlot: false,
			action: 'schedule',
			delayMs: 0,
		});
	});

	it('drops a slot for another channel and then schedules after 1500 ms', () => {
		expect(
			planTypingSend(slot({channelId: 'other', timeout: PENDING_TIMEOUT}), 'channel', 'me', PREVIOUS_SEND + 1),
		).toEqual({dropSlot: true, action: 'schedule', delayMs: 1500});
	});

	it('drops a slot for another account before checking the throttle', () => {
		expect(
			planTypingSend(slot({userId: 'previous', timeout: PENDING_TIMEOUT}), 'channel', 'me', PREVIOUS_SEND + 1),
		).toEqual({dropSlot: true, action: 'schedule', delayMs: 1500});
	});
});
