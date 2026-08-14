// SPDX-License-Identifier: AGPL-3.0-or-later

import ChannelSticker from '@app/features/channel/state/ChannelSticker';
import Slowmode from '@app/features/slowmode/state/Slowmode';
import {CHANNEL_RATE_LIMIT_PER_USER_MAX} from '@fluxer/constants/src/LimitConstants';

const MAX_RETRY_AFTER_MS = CHANNEL_RATE_LIMIT_PER_USER_MAX * 1000;

function clearSendScopedSticker(channelId: string): void {
	ChannelSticker.clearPendingStickerOnMessageSend(channelId);
}

function markSlowmodeSend(channelId: string): void {
	Slowmode.recordMessageSend(channelId);
}

export function prepareMessageSend(channelId: string): void {
	clearSendScopedSticker(channelId);
}

export function recordMessageSend(channelId: string): void {
	markSlowmodeSend(channelId);
}

export function updateSlowmodeRemaining(channelId: string, retryAfterMs: number): void {
	Slowmode.updateSlowmodeRemaining(channelId, retryAfterMs);
}

export function retryAfterSecondsToMs(retryAfterSeconds: number | undefined): number {
	if (retryAfterSeconds == null || !Number.isFinite(retryAfterSeconds) || retryAfterSeconds <= 0) {
		return 0;
	}
	const retryAfterMs = Math.ceil(retryAfterSeconds * 1000);
	if (!Number.isSafeInteger(retryAfterMs) || retryAfterMs > MAX_RETRY_AFTER_MS) {
		return 0;
	}
	return retryAfterMs;
}
