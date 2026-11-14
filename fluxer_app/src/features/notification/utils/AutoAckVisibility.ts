// SPDX-License-Identifier: AGPL-3.0-or-later

import {getGuildVoiceCallExpansionKey} from '@app/features/voice/state/CompactVoiceCallHeight';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';

interface TextChatVisibleForAutoAckOptions {
	channelId: string;
	channelType: number | null | undefined;
	isGuildVoiceCallExpanded: boolean;
	activeVoiceCallFullscreenScopeKey: string | null;
}

function getDirectCallFullscreenScopeKey(channelId: string): string {
	return `dm-call:${channelId}`;
}

function isVoiceCallFullscreenScopeForChannel(channelId: string, scopeKey: string | null): boolean {
	return (
		scopeKey === getGuildVoiceCallExpansionKey(channelId) || scopeKey === getDirectCallFullscreenScopeKey(channelId)
	);
}

export function isTextChatVisibleForAutoAck({
	channelId,
	channelType,
	isGuildVoiceCallExpanded,
	activeVoiceCallFullscreenScopeKey,
}: TextChatVisibleForAutoAckOptions): boolean {
	if (isVoiceCallFullscreenScopeForChannel(channelId, activeVoiceCallFullscreenScopeKey)) {
		return false;
	}
	if (channelType === ChannelTypes.GUILD_VOICE) {
		return !isGuildVoiceCallExpanded;
	}
	return true;
}
