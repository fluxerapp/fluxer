// SPDX-License-Identifier: AGPL-3.0-or-later

export const TYPING_ROLLING_EXPIRY_MS = 10000;
export const TYPING_ROLLING_SEND_DELAY_MS = 1500;
export const TYPING_ROLLING_THROTTLE_MS = 8000;
export const TYPING_ROLLING_IMMEDIATE_SEND_IDLE_MS = 16000;
export const TYPING_ROLLING_SKIP_ABOVE_TRACKED = 5;
export const TYPING_ROLLING_MAX_NAMES = 3;
export const TYPING_ROLLING_OVERFLOW_SLACK_PX = 48;

export type TypingSendSlot = {
	channelId: string;
	userId: string;
	timeout: NodeJS.Timeout | null;
	prevSend: number;
};

type TypingSendPlan = {dropSlot: boolean} & ({action: 'throttled'} | {action: 'schedule'; delayMs: number});

export function planTypingSend(
	slot: TypingSendSlot | null,
	channelId: string,
	userId: string,
	now: number,
): TypingSendPlan {
	if (slot === null) {
		return {dropSlot: false, action: 'schedule', delayMs: TYPING_ROLLING_SEND_DELAY_MS};
	}
	if (slot.channelId !== channelId || slot.userId !== userId) {
		return {dropSlot: true, action: 'schedule', delayMs: TYPING_ROLLING_SEND_DELAY_MS};
	}
	if (slot.timeout !== null || slot.prevSend + TYPING_ROLLING_THROTTLE_MS > now) {
		return {dropSlot: false, action: 'throttled'};
	}
	const idleLongEnough = slot.prevSend <= now - TYPING_ROLLING_IMMEDIATE_SEND_IDLE_MS;
	return {dropSlot: false, action: 'schedule', delayMs: idleLongEnough ? 0 : TYPING_ROLLING_SEND_DELAY_MS};
}
