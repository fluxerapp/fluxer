// SPDX-License-Identifier: AGPL-3.0-or-later

import ChannelSticker from '@app/features/channel/state/ChannelSticker';
import Slowmode from '@app/features/slowmode/state/Slowmode';
import {CHANNEL_RATE_LIMIT_PER_USER_MAX} from '@fluxer/constants/src/LimitConstants';

const MAX_RETRY_AFTER_MS = CHANNEL_RATE_LIMIT_PER_USER_MAX * 1000;

export interface PendingMessageSend {
	readonly previousSendTimestamp: number | null;
	readonly pendingSendTimestamp: number;
}

function clearSendScopedSticker(channelId: string): void {
	ChannelSticker.clearPendingStickerOnMessageSend(channelId);
}

export function prepareMessageSend(channelId: string): void {
	clearSendScopedSticker(channelId);
}

export function recordPendingMessageSend(channelId: string): PendingMessageSend {
	const previousSendTimestamp = Slowmode.getLastSendTimestamp(channelId);
	const pendingSendTimestamp = Slowmode.recordMessageSend(channelId);
	return {previousSendTimestamp, pendingSendTimestamp};
}

export function confirmMessageSend(channelId: string, sentAt: string, pending?: PendingMessageSend): void {
	const timestamp = Date.parse(sentAt);
	if (!Number.isFinite(timestamp)) return;
	const floor = pending?.pendingSendTimestamp;
	const anchored = floor == null ? timestamp : Math.max(timestamp, floor);
	Slowmode.updateSlowmodeTimestamp(channelId, anchored);
}

export function discardPendingMessageSend(channelId: string, pending: PendingMessageSend): void {
	if (Slowmode.getLastSendTimestamp(channelId) !== pending.pendingSendTimestamp) return;
	Slowmode.updateSlowmodeTimestamp(channelId, pending.previousSendTimestamp);
}

export function updateSlowmodeRemaining(channelId: string, retryAfterMs: number): void {
	Slowmode.updateSlowmodeRemaining(channelId, retryAfterMs);
}

export function clampSlowmodeRetryAfterMs(retryAfterMs: number | null): number {
	if (retryAfterMs == null || !Number.isSafeInteger(retryAfterMs) || retryAfterMs <= 0) {
		return 0;
	}
	if (retryAfterMs > MAX_RETRY_AFTER_MS) {
		return 0;
	}
	return retryAfterMs;
}
