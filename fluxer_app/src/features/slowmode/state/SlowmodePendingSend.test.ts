// SPDX-License-Identifier: AGPL-3.0-or-later

import {resolveRetryAfterMs} from '@app/features/messaging/utils/RetryAfterUtils';
import {HttpError} from '@app/features/platform/types/EndpointError';
import * as SlowmodeCommands from '@app/features/slowmode/commands/SlowmodeCommands';
import Slowmode from '@app/features/slowmode/state/Slowmode';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

const CHANNEL_ID = '1234567890123456789';
const RATE_LIMIT_PER_USER = 5;
const SLOWMODE_WINDOW_MS = RATE_LIMIT_PER_USER * 1000;
const T0 = Date.UTC(2026, 7, 20, 12, 0, 0);

function at(offsetMs: number): void {
	vi.setSystemTime(T0 + offsetMs);
}

function shownRemainingMs(): number {
	return Slowmode.getSlowmodeRemaining(CHANNEL_ID, RATE_LIMIT_PER_USER);
}

function shownCountdownSeconds(): number {
	return Math.ceil(shownRemainingMs() / 1000);
}

function composerBlocksSend(): boolean {
	return shownRemainingMs() > 0;
}

function serverTimestamp(offsetMs: number): string {
	return new Date(T0 + offsetMs).toISOString();
}

function slowmodeRejection(retryAfterDecimalSeconds: number, retryAfterHeader: string): HttpError {
	return new HttpError({
		method: 'POST',
		path: `/channels/${CHANNEL_ID}/messages`,
		status: 400,
		body: {code: APIErrorCodes.SLOWMODE_RATE_LIMITED, retry_after: retryAfterDecimalSeconds},
		responseHeaders: {'retry-after': retryAfterHeader},
	});
}

function applySlowmodeRejection(error: HttpError): number {
	const retryAfterMs = SlowmodeCommands.clampSlowmodeRetryAfterMs(resolveRetryAfterMs(error));
	SlowmodeCommands.updateSlowmodeRemaining(CHANNEL_ID, retryAfterMs);
	return retryAfterMs;
}

describe('slowmode while an earlier send is still pending', () => {
	beforeEach(() => {
		vi.useFakeTimers();
		at(0);
		Slowmode.clearChannel(CHANNEL_ID);
	});
	afterEach(() => {
		Slowmode.clearChannel(CHANNEL_ID);
		vi.useRealTimers();
	});

	it('blocks the second message while the first one is still pending', () => {
		at(0);
		expect(composerBlocksSend()).toBe(false);
		SlowmodeCommands.recordPendingMessageSend(CHANNEL_ID);
		at(120);
		expect(composerBlocksSend()).toBe(true);
		expect(shownRemainingMs()).toBe(SLOWMODE_WINDOW_MS - 120);
	});

	it('anchors the window to the timestamp the server assigned the message', () => {
		at(0);
		SlowmodeCommands.recordPendingMessageSend(CHANNEL_ID);
		at(450);
		SlowmodeCommands.confirmMessageSend(CHANNEL_ID, serverTimestamp(200));
		expect(Slowmode.getLastSendTimestamp(CHANNEL_ID)).toBe(T0 + 200);
		expect(shownRemainingMs()).toBe(SLOWMODE_WINDOW_MS - 250);
	});

	it('never shows more than the channel setting across the reported ordering', () => {
		const countdown: Array<{atMs: number; shownMs: number; shownSeconds: number}> = [];
		const sample = (offsetMs: number): void => {
			at(offsetMs);
			countdown.push({atMs: offsetMs, shownMs: shownRemainingMs(), shownSeconds: shownCountdownSeconds()});
		};
		at(0);
		SlowmodeCommands.recordPendingMessageSend(CHANNEL_ID);
		sample(0);
		sample(120);
		sample(449);
		at(450);
		SlowmodeCommands.confirmMessageSend(CHANNEL_ID, serverTimestamp(200));
		sample(450);
		sample(2000);
		sample(5199);
		sample(5200);
		sample(10200);
		for (const entry of countdown) {
			expect(entry.shownMs).toBeLessThanOrEqual(SLOWMODE_WINDOW_MS);
			expect(entry.shownSeconds).toBeLessThanOrEqual(RATE_LIMIT_PER_USER);
		}
		for (let index = 1; index < countdown.length; index++) {
			expect(countdown[index]!.shownSeconds).toBeLessThanOrEqual(countdown[index - 1]!.shownSeconds);
		}
		expect(countdown.at(-3)!.shownMs).toBeGreaterThan(0);
		expect(countdown.at(-2)!.shownMs).toBe(0);
		expect(countdown.at(-1)!.shownMs).toBe(0);
	});

	it('keeps a server slowmode rejection inside the channel setting', () => {
		at(0);
		SlowmodeCommands.recordPendingMessageSend(CHANNEL_ID);
		at(450);
		SlowmodeCommands.confirmMessageSend(CHANNEL_ID, serverTimestamp(200));
		at(759);
		const beforeRejection = shownRemainingMs();
		at(760);
		const storedMs = applySlowmodeRejection(slowmodeRejection(4.44, '5'));
		expect(storedMs).toBe(4440);
		expect(shownRemainingMs()).toBe(SLOWMODE_WINDOW_MS - 560);
		expect(shownRemainingMs()).toBeLessThan(beforeRejection);
		at(5200);
		expect(shownRemainingMs()).toBe(0);
		expect(composerBlocksSend()).toBe(false);
	});

	it('does not inflate the window when the rejection header carries milliseconds', () => {
		at(0);
		SlowmodeCommands.recordPendingMessageSend(CHANNEL_ID);
		at(450);
		SlowmodeCommands.confirmMessageSend(CHANNEL_ID, serverTimestamp(200));
		at(760);
		const storedMs = applySlowmodeRejection(slowmodeRejection(4.7, '4700'));
		expect(storedMs).toBe(4700);
		expect(shownRemainingMs()).toBeLessThanOrEqual(SLOWMODE_WINDOW_MS);
		at(5460);
		expect(shownRemainingMs()).toBe(0);
	});

	it('releases the window when the pending send never reaches the server', () => {
		at(0);
		const pendingSend = SlowmodeCommands.recordPendingMessageSend(CHANNEL_ID);
		at(120);
		expect(composerBlocksSend()).toBe(true);
		at(300);
		SlowmodeCommands.discardPendingMessageSend(CHANNEL_ID, pendingSend);
		expect(Slowmode.getLastSendTimestamp(CHANNEL_ID)).toBeNull();
		expect(composerBlocksSend()).toBe(false);
	});

	it('keeps a rejection window when the rejected send releases its own guess', () => {
		at(0);
		const pendingSend = SlowmodeCommands.recordPendingMessageSend(CHANNEL_ID);
		at(760);
		applySlowmodeRejection(slowmodeRejection(4.44, '5'));
		SlowmodeCommands.discardPendingMessageSend(CHANNEL_ID, pendingSend);
		expect(Slowmode.getLastSendTimestamp(CHANNEL_ID)).toBeNull();
		expect(shownRemainingMs()).toBe(4440);
	});

	it('ignores an anchor from a clock that runs behind the server', () => {
		at(0);
		SlowmodeCommands.recordPendingMessageSend(CHANNEL_ID);
		at(450);
		SlowmodeCommands.confirmMessageSend(CHANNEL_ID, serverTimestamp(30_000));
		expect(shownRemainingMs()).toBeLessThanOrEqual(SLOWMODE_WINDOW_MS);
		at(5450);
		expect(shownRemainingMs()).toBe(0);
	});
});

describe('clock skew on the send anchor', () => {
	const CHANNEL_ID = '900000000000000001';
	const RATE_LIMIT_SECONDS = 5;

	beforeEach(() => {
		vi.useFakeTimers();
		Slowmode.clearChannel(CHANNEL_ID);
	});

	afterEach(() => {
		Slowmode.clearChannel(CHANNEL_ID);
		vi.useRealTimers();
	});

	const remainingAfterAck = (clientAheadMs: number): number => {
		vi.setSystemTime(new Date(1_000_000));
		const pending = SlowmodeCommands.recordPendingMessageSend(CHANNEL_ID);
		const serverAcceptedAt = new Date(1_000_000 - clientAheadMs).toISOString();
		vi.setSystemTime(new Date(1_000_450));
		SlowmodeCommands.confirmMessageSend(CHANNEL_ID, serverAcceptedAt, pending);
		return Slowmode.getSlowmodeRemaining(CHANNEL_ID, RATE_LIMIT_SECONDS);
	};

	it('does not shorten the window when the client clock runs ahead of the server', () => {
		for (const aheadMs of [0, 250, 1_000, 2_500, 4_800, 30_000, 3_600_000]) {
			expect(remainingAfterAck(aheadMs)).toBe(4_550);
		}
	});

	it('never lets the local guard reach zero while the window is live', () => {
		for (const aheadMs of [4_800, 30_000, 3_600_000]) {
			expect(remainingAfterAck(aheadMs)).toBeGreaterThan(0);
		}
	});
});
